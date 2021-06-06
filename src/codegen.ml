open Label
open Big_int
open Printf 

open Imm
open CodegenEnv
open LocationEnv
open Evm
open Syntax
open ContractId
open Contract
open Misc

module Loc  = Location 
module Eth  = Ethereum 
module BL   = BatList
module L    = List
module LI   = LayoutInfo

let err = failwith 



let copy_stor_range_to_top le ce (range : imm Loc.stor_range) =
    assert (is_const_int 1 range.Loc.stor_size) ; 
    let offset:imm  = range.Loc.stor_start in
    let ce = PUSH32 offset     >>> ce in
    let ce = SLOAD             >>> ce in
    le, ce

let copy_nth_stack_elem le ce (n:int) =
    let start_size    = stack_size ce  in
    let diff          = start_size - n in
    assert (diff >= 0) ; 
    let ce = dup_succ diff     >>> ce in
    assert (stack_size ce = start_size+1) ; 
    le,ce


let shiftR_top ce bits =
    assert (bits >= 0 ) ; 
    assert (bits < 256) ; 
    let start_size = stack_size ce in
    if bits=0 then ce else                  (*                 x >>> .. *) 
    let ce = PUSH1 (Int bits)   >>> ce in   (*        bits >>> x >>> .. *)
    let ce = PUSH1 (Int 2)      >>> ce in   (*  2 >>> bits >>> x >>> .. *)
    let ce = EXP                >>> ce in   (*     2**bits >>> x >>> .. *) 
    let ce = SWAP1              >>> ce in   (*     x >>> 2**bits >>> .. *) 
    let ce = DIV                >>> ce in   (*       x/(2**bits) >>> .. *) 
    assert (stack_size ce = start_size) ; 
    ce

let shiftL_top ce bits =
    assert (bits >= 0 ) ; 
    assert (bits < 256) ; 
    if bits=0 then ce else                  (*                 x >>> .. *)
    let ce = PUSH1 (Int bits)   >>> ce in   (*        bits >>> x >>> .. *)                   
    let ce = PUSH1 (Int 2)      >>> ce in   (*  2 >>> bits >>> x >>> .. *) 
    let ce = EXP                >>> ce in   (*     2**bits >>> x >>> .. *) 
    let ce = MUL                >>> ce in   (*       (2**bits)*x >>> .. *) 
    ce

let copy_calldata_to_top le ce (range : Loc.calldata_range) =
    assert (range.Loc.calldata_size >  0 ) ;
    assert (range.Loc.calldata_size <= 32) ;
    let ce  = PUSH4 (Int range.Loc.calldata_start) >>> ce  in
    let ce  = CALLDATALOAD      >>> ce in
    let ce  = shiftR_top ce ((32 - range.Loc.calldata_size) * 8) in
    le, ce


type alignment          = L_ 
                        | R_


let align_boolean ce align = 
    assert (align = R_) ; 
    ce


let align_address ce    = function 
    | R_        ->  ce
    | L_        ->  shiftL_top ce (12 * 8)

let align_from_right_aligned (ce:ce) align tyT =
    match align with
    | R_        ->  ce
    | L_        ->  let size   = size_of_ty tyT in
                    assert (size <= 32) ;
                    if size=32 then ce else (* no align *) 
                        let shift = (32 - size) * 8 in
                        let ce = PUSH1 (Int shift)  >>> ce in (* stack: [shift] *)
                        let ce = PUSH1 (Int 2)      >>> ce in (* stack: [shift, 2] *)
                        let ce = EXP                >>> ce in (* stack: [2 ** shift] *)
                        let ce = MUL                >>> ce in
                        ce

let copy_to_top le ce align ty (l:Loc.location) =
    let le,ce = Loc.( match l with
      | Stor range   -> copy_stor_range_to_top le ce range
      | CachedStor _ -> err "copy_to_top: CachedStor"
      | Volatile _      -> err "copy_to_top: Volatile"
      | Code _          -> err "copy_to_top: Code"
      | Calldata range  -> copy_calldata_to_top le ce range
      | Stack s         -> copy_nth_stack_elem le ce s ) in
    let ce = align_from_right_aligned ce align ty in
    (* le needs to remember the alignment *)
    le, ce


let swap_pc_with_0 ce   =
    let ce = PUSH1 (Int 0)      >>> ce in       (*                             0 >>> .. *)
    let ce = SLOAD              >>> ce in       (*                          S[0] >>> .. *)
    let ce = PUSH1 (Int 0)      >>> ce in       (*                    0 >>> S[0] >>> .. *)
    let ce = DUP1               >>> ce in       (*              0 >>> 0 >>> S[0] >>> .. *)
    let ce = SSTORE             >>> ce in       (* S'[0]=0                  S[0] >>> .. *)
    ce


(** [restore_pc]   *)  
(*                                                 
 *     BEFORE             AFTER                    
 *                                                   
 *    +--------+                                     
 *    | bkp_pc |                                     
 *  --+--------+--    --+--------+--                
 *)
let restore_pc ce       =                       (*                bkp_pc >>> .. *)
    let ce = PUSH1 (Int 0)      >>> ce in       (*          0 >>> bkp_pc >>> .. *)
    let ce = SSTORE             >>> ce in       (* S'[0]=bkp_pc              .. *)             
    ce


(** [throw_if_zero] if the topstack is zero, then throw (goto 0)  *)
let throw_if_zero ce    =                       (*                   i >>> .. *)   
    let ce = DUP1               >>> ce in       (*             i >>> i >>> .. *)
    let ce = ISZERO             >>> ce in       (*             b >>> i >>> .. *)
    let ce = PUSH1 (Int 0)      >>> ce in       (*       0 >>> b >>> i >>> .. *)
    let ce = JUMPI              >>> ce in       (* {GOTO 0 if b}     i >>> .. *)
    ce







(*************************************)
(***           MEMORY              ***)
(*************************************)

(**  [mem_alloc]                              Addr     Val              Addr     Val     
 *                                         +--------+--------+       +--------+--------+
 *                                         |   64   |   a    |       |   64   | a+size |
 *      BEFORE            AFTER            +--------+--------+       +--------+--------+
 *                                         | ...    |  ...   |       | ...    |  ...   |
 *   +----------+      +----------+        +--------+--------+       +--------+--------+
 *   |   size   |      |    a     |     -->|   a    |     0  |       |   a    |     0  |
 * --+----------+--  --+----------+--      +--------+--------+       +--------+--------+
 *                                         | ...    |     0  |       | ...    |     0  |
 *  mem_alloc :=                           +--------+--------+       +--------+--------+
 *      size := pop();                     | a+size |        |    -->| a+size |        |
 *      a    := alloc(size);               +--------+--------+       +--------+--------+
 *      push(a)                                BEFORE MEM                AFTER MEM 
 *)
let mem_alloc (ce:ce) =           
    let start_size = stack_size     ce in (*                            len <<< .. *)
    let ce = PUSH1 (Int 64)     >>> ce in (*                     64 <<< len <<< .. *)
    let ce = DUP1               >>> ce in (*              64 <<< 64 <<< len <<< .. *)
    let ce = MLOAD              >>> ce in (*           M[64] <<< 64 <<< len <<< .. *)
    let ce = DUP1               >>> ce in (* M[64] <<< M[64] <<< 64 <<< len <<< .. *)
    let ce = SWAP3              >>> ce in (* len <<< M[64] <<< 64 <<< M[64] <<< .. *)
    let ce = ADD                >>> ce in (*     M[64+len] <<< 64 <<< M[64] <<< .. *)
    let ce = SWAP1              >>> ce in (*     64 <<< M[64+len] <<< M[64] <<< .. *)
    let ce = MSTORE             >>> ce in (*                          M[64] <<< .. *) 
    assert (stack_size ce = start_size) ; 
    ce
(* M[64]  :=   the address of mem alloc     *) 
(* MSTORE :=   x=pop() ; y=pop() ; M[x]=y   *) 
(* MLOAD  :=   x=pop() ; push M[x]          *) 

let get_alloc (ce:ce) =
    let start_size = stack_size     ce in (*           .. *)
    let ce = PUSH1 (Int 64)     >>> ce in (* 64    <<< .. *) 
    let ce = MLOAD              >>> ce in (* M[64] <<< .. *) 
    assert (stack_size ce = 1+start_size) ; 
    ce

(** [Tight] just uses [size_of_ty] bytes on the memory.
 *  [ABI] always uses multiples of 32 bytes.
 *  These choices do not affect the alighments
 *)
type memoryPack = TightPack | ABIPack

(** [copy_runtime_code_to_mem ce cntrcts idx]
 * /adds opcodes to [ce] so that in the final state the memory contains the runtime code
 * for all cntrcts that are reachable from [idx] in the
 * list [cntrcts] in the
 * addresses [code_start, code_start + code_size).
 * This adds two elements to the stack, resulting in
 * [..., code_length, code_start) *)
let copy_runtime_code_to_mem ce idx =
    let start_size = stack_size ce in                 (*                                                           .. *)  
    let ce = PUSH32(RuntimeCodeSize)        >>> ce in (*                                                  size <<< .. *)
    let ce = DUP1                           >>> ce in (*                                         size <<< size <<< .. *)  
    let ce = mem_alloc                          ce in (*                                  alloc(size) <<< size <<< .. *)
    let ce = DUP2                           >>> ce in (*                         size <<< alloc(size) <<< size <<< .. *)
    let ce = PUSH32(RuntimeCodeOffset idx)  >>> ce in (*                 idx <<< size <<< alloc(size) <<< size <<< .. *)
    let ce = DUP3                           >>> ce in (* alloc(size) <<< idx <<< size <<< alloc(size) <<< size <<< .. *)
    let ce = CODECOPY                       >>> ce in (*                                  alloc(size) <<< size <<< .. *)
    assert (stack_size ce = start_size+2) ; 
    ce

let copy_code_to_mem ce =                             (*                                          idx <<< size <<< .. *)
    let ce = DUP2                           >>> ce in (*                                 size <<< idx <<< size <<< .. *)
    let ce = mem_alloc                          ce in (*                          alloc(size) <<< idx <<< size <<< .. *)
    let ce = SWAP1                          >>> ce in (*                          idx <<< alloc(size) <<< size <<< .. *)
    let ce = DUP3                           >>> ce in (*                 size <<< idx <<< alloc(size) <<< size <<< .. *)
    let ce = SWAP1                          >>> ce in (*                 idx <<< size <<< alloc(size) <<< size <<< .. *)
    let ce = DUP3                           >>> ce in (* alloc(size) <<< idx <<< size <<< alloc(size) <<< size <<< .. *) 
    let ce = CODECOPY                       >>> ce in (*                                  alloc(size) <<< size <<< .. *)  
    ce

(** [copy_whole_code_to_mem] allocates enough memory to accomodate the
 *  whole of the currently running code, and copies it there.
 *  After this, [size, offset] of the memory region is left on the stack.
 *)
let copy_whole_code_to_mem ce =
    let start_size = stack_size ce               in (*                                                             .. *)        
    let ce = CODESIZE                       >>> ce in (*                                                  size <<< .. *)
    let ce = DUP1                           >>> ce in (*                                         size <<< size <<< .. *)
    let ce = mem_alloc                          ce in (*                                  alloc(size) <<< size <<< .. *)
    let ce = DUP2                           >>> ce in (*                         size <<< alloc(size) <<< size <<< .. *)
    let ce = PUSH1(Int 0)                   >>> ce in (*                  0  <<< size <<< alloc(size) <<< size <<< .. *)
    let ce = DUP3                           >>> ce in (* alloc(size) <<<  0  <<< size <<< alloc(size) <<< size <<< .. *)
    let ce = CODECOPY                       >>> ce in (*                                  alloc(size) <<< size <<< .. *)
    assert(start_size + 2 = stack_size ce) ;        
    ce

let push_method ce (m:mthd_info)  =
    let hash  = Eth.hash_of_mthd_info_ty m         in
    let b     = Eth.hex_to_big_int hash            in 
    let ce    = PUSH4(Big b)                >>> ce in 
    ce

(** [prepare_functiohn_signature ce mthd]
 *  Allocates 4 bytes on the memory, and puts the function signature of the arg there.
 *  After that, the stack has (..., signature size, signature offset )
 *)
let prepare_mthd_sig mthd ce =
    let start_size = stack_size ce in
    let ce = PUSH1(Int 4)                   >>> ce in (*                                                  4 <<< .. *)
    let ce = DUP1                           >>> ce in (*                                           4  <<< 4 <<< .. *)
    let ce = mem_alloc                          ce in (*                                     alloc(4) <<< 4 <<< .. *)
    let ce = push_method ce mthd                   in (*                      method_sig <<< alloc(4) <<< 4 <<< .. *)
    let ce = DUP2                           >>> ce in (*         alloc(4) <<< method_sig <<< alloc(4) <<< 4 <<< .. *)
    let ce = MSTORE                         >>> ce in (* M[alloc(4)] := method_sig           alloc(4) <<< 4 <<< .. *)
    assert (stack_size ce = start_size + 2) ; 
    ce

(* take a := 256bit , b := 256bit(=32byte) , and return sha3(a++b) *)  
let keccak_cons le ce =                               
    let start_size = stack_size                 ce in (*                    a <<< b <<< .. *) 
    let ce = PUSH1 (Int 0x0)                >>> ce in (*           0x00 <<< a <<< b <<< .. *)
    let ce = MSTORE                         >>> ce in (* M[0x00]=a                b <<< .. *)
    let ce = PUSH1 (Int 0x20)               >>> ce in (*                 0x20 <<< b <<< .. *)
    let ce = MSTORE                         >>> ce in (* M[0x20]=b                      .. *) 
    let ce = PUSH1 (Int 0x40)               >>> ce in (*                       0x40 <<< .. *)
    let ce = PUSH1 (Int 0x0 )               >>> ce in (*              0x0  <<< 0x40 <<< .. *)
    let ce = SHA3                           >>> ce in (*        sha3(M[0x00..0x3F]) <<< .. *)
    assert (stack_size ce + 1 = start_size) ;
    ce

let incr_top ce (inc : int) =
    let ce = PUSH32 (Int inc)               >>> ce in
    let ce = ADD                            >>> ce in
    ce


















(* CODEGEN *) 


(* sumsize := the size of arguments allocated *)  

(** [add_cnstrctr_arg_to_mem ce arg] realizes [arg] on the memory according to the ABI.  
 *  This increases the stack top (sumsize) by the size of the new allocation. *)
let rec mstore_mthd_arg le pack ce (arg:ty expr) =
    let start_size  = stack_size ce in
    let ty          = snd arg       in 
    printf "it's appending type %s\n"(string_of_ty ty) ; 
    assert (fits_in_one_stor_slot ty) ; 
    let i,a= (match pack with 
              | ABIPack   -> 32           ,R_ 
              | TightPack -> size_of_ty ty,L_)  in   (*                                          sumsize >>> .. *)
    let ce = PUSH1 (Int i)              >>> ce  in   (*                                 size >>> sumsize >>> .. *)
    let ce = DUP1                       >>> ce  in   (*                       size  >>> size >>> sumsize >>> .. *)
    let ce = mem_alloc ce                       in   (*                 alloc(size) >>> size >>> sumsize >>> .. *)
    let ce = arg                >>>>> (a,le,ce) in   (*         arg >>> alloc(size) >>> size >>> sumsize >>> .. *)
    let ce = SWAP1                      >>> ce  in   (*         alloc(size) >>> arg >>> size >>> sumsize >>> .. *)
    let ce = MSTORE                     >>> ce  in   (* M[alloc(size)] := arg           size >>> sumsize >>> .. *)
    let ce = ADD                        >>> ce  in   (*                                     size+sumsize >>> .. *)
    assert (stack_size ce = start_size) ; 
    ce

(** [add_cnstrctr_args_to_mem args] realizes [args] on the memory according to the ABI.  
 * This leaves the amount of memory on the stack.
 *  Usually this function is called right after the cnstrctr code is set up in the memory,
 *  so the offset of the memory is not returned.
 *  (This makes it easy for the zero-arg mthd)
 *)
and mstore_mthd_args le pack ce (args:ty expr list) =
    let add_arg     = mstore_mthd_arg le pack   in 
    let start_size  = stack_size            ce  in
    let ce = PUSH1(Int 0)               >>> ce  in  (*                  0 >>> ..   *)
    let ce = L.fold_left add_arg ce args        in  (*            sumsize >>> ..   *) 
    assert (start_size+1 = stack_size ce) ; 
    ce


(* [init_code_into_mem] 
 *
 *              ADDRESS              MEMORY 
 *          +---------------------+-----------------+                         
 *          |                  0  | runtime_code    |                                   
 *          |                ...  |  ...            |                                   
 *          |               size  | contract_code   |
 *          |                ...  |  ...            |
 *          |         size+wsize  | arg1            |                                   
 *          |                     |  ...            |
 *          |                     | arg2            |
 *          |                     |  ...            |
 *          |---------------------|-----------------|
 *          | argsize+size+wsize  |                 |
 *          |                ...  |                 |                         
 *)                                                                 

and init_code_into_mem le ce new_e =
    let name =  new_e.new_head in
    let idx  =  idx_lookup ce name in 
    let ce = PUSH32(CnstrctrCodeSize idx)           >>> ce  in (*                                                             size >>> .. *) 
    let ce = PUSH32(CnstrctrInRuntimeCodeOffset idx)>>> ce  in (*                                                    cie  >>> size >>> .. *)
    let ce = copy_code_to_mem ce                            in (*                                             alloc(size) >>> size >>> .. *)
    let ce = SWAP1                                  >>> ce  in (*                                             size >>> alloc(size) >>> .. *)
    let ce = copy_whole_code_to_mem ce                      in (*                  alloc(wsize) >>> wsize >>> size >>> alloc(size) >>> .. *)
    let ce = mstore_mthd_args le ABIPack ce new_e.new_args  in (*     argssize >>> alloc(wsize) >>> wsize >>> size >>> alloc(size) >>> .. *)
    let ce = SWAP1                                  >>> ce  in (*     alloc(wsize) >>> argssize >>> wsize >>> size >>> alloc(size) >>> .. *)
    let ce = POP                                    >>> ce  in (*                      argssize >>> wsize >>> size >>> alloc(size) >>> .. *)
    let ce = ADD                                    >>> ce  in (*                          argssize+wsize >>> size >>> alloc(size) >>> .. *)
    let ce = ADD                                    >>> ce  in (*                              argssize+wsize+size >>> alloc(size) >>> .. *)
    let ce = SWAP1                                  >>> ce  in (*                                      alloc(size) >>>   totalsize >>> .. *)
    ce

and codegen_fun_call_expr le ce align (fncall:ty fn_call) (reT:ty) =
    if      fncall.call_head = "pre_ecdsarecover" then (
                assert (align = R_) ; 
                codegen_ecdsarecover le ce fncall.call_args reT )   (* XXX: need to pass alignment *)
    else if fncall.call_head = "keccak256" then (
                assert (align = R_) ; 
                codegen_keccak256    le ce fncall.call_args reT )   (* XXX: need to pass alignment *) 
    else if fncall.call_head = "iszero" then 
                codegen_iszero le ce align fncall.call_args reT
    else        err "codegen_fun_call_expr: unknown function head."

and codegen_iszero le ce align args reT = match args with
    | [arg] ->  assert (reT = TyBool) ; 
                let ce =  arg       >>>>> (align,le,ce) in
                          ISZERO    >>>         ce 
    | _     ->  err "codegen_iszero: Wrong number of args"
       
and codegen_keccak256 le ce args rettyp =
    let start_size = stack_size ce in
    let ce = get_alloc ce                               in (* stack: [..., offset] *)
    let ce = mstore_mthd_args le TightPack ce args      in (* stack: [..., offset, size] *)
    let ce = SWAP1                      >>>     ce      in (* stack: [..., size, offset] *)
    let ce = SHA3                       >>>     ce      in
    assert(stack_size ce = start_size + 1) ;
    ce

and codegen_ecdsarecover le ce args rettyp = match args with
    | [h; v; r; s] ->
        let start_size = stack_size ce                  in  (* stack: [] *)
        let ce = PUSH1 (Int 32)         >>>     ce      in  (* stack: [out size] *)
        let ce = DUP1                   >>>     ce      in  (* stack: [out size, out size] *)
        let ce = mem_alloc                      ce      in  (* stack: [out size, out address] *)
        let ce = DUP2                   >>>     ce      in  (* stack: [out size, out address, out size] *)
        let ce = DUP2                   >>>     ce      in  (* stack: [out size, out address, out size, out address] *)
        let ce = get_alloc                      ce      in
        let ce = mstore_mthd_args le ABIPack ce args    in  (* stack: [out size, out address, out size, out address, mem_offset, mem_total_size] *)
        let ce = SWAP1                  >>>     ce      in  (* stack: [out size, out address, out size, out address, in size, in offset] *)
        let ce = PUSH1 (Int 0)          >>>     ce      in  (* stack: [out size, out address, out size, out address, in size, in offset, value] *)
        assert (stack_size ce = start_size+7) ;
        let ce = PUSH1 (Int 1)          >>>     ce      in  (* stack: [out size, out address, out size, out address, in size, in offset, value, to] *)
        let ce = PUSH4 (Int 10000)      >>>     ce      in  (* stack: [out size, out address, out size, out offset, in size, in offset, value, to, gas] *)
        let ce = CALL                   >>>     ce      in  (* stack: [out size, out address, success?] *)
        let ce = throw_if_zero ce                       in
        let ce = POP                    >>>     ce      in  (* stack: [out size, out address] *)
        let ce = SWAP1                  >>>     ce      in  (* stack: [out address, out size] *)
        let ce = POP                    >>>     ce      in  (* we know it's 32 *) (* stack: [out address] *)
        let ce = MLOAD                  >>>     ce      in  (* stack: [output] *)
        assert (stack_size ce = start_size+1) ;    
        ce
    | _ -> err "pre_ecdsarecover has a wrong number of args"

and codegen_new_expr le ce new_e (cntrctname : string) =
    let start_size = stack_size ce                      in (* assert that the reentrance info is throw *)
    assert(is_throw_only new_e.new_msg_info.msg_reentrance_info) ;  (* set up the reentrance guard *)
    let ce = swap_pc_with_0 ce                          in (* stack : [pc_bkp] *)
    let ce = init_code_into_mem le ce new_e             in (* stack : [pc_bkp, size, alloc(size)] *)
    let ce = (match new_e.new_msg_info.msg_value_info with
       | None     -> PUSH1 (Int 0)  >>>     ce             (* no value info means value of zero *)
       | Some e   -> e              >>>>> (R_,le,ce)  ) in (* stack : [pc_bkp, size, alloc(size), v ] *)
    let ce =  CREATE                >>>     ce          in (* stack : [pc_bkp, create_result] *)
    let ce = throw_if_zero                  ce          in (* stack : [pc_bkp, create_result] *)
    let ce =  SWAP1                 >>>     ce          in (* stack : [create_result, pc_bkp] *)
    let ce = restore_pc ce                              in (* stack : [create_result] *)
    assert (stack_size ce = start_size + 1) ; 
    ce

and gen_array_storLoc le ce aa =
    let array = aa.array_name                           in
    let index = aa.array_index                          in
    let ce    = index               >>>>> (R_,le,ce)    in (*                    index <<< .. *)
    let ce    = array               >>>>> (R_,le,ce)    in (*      array_loc <<< index <<< .. *)
                keccak_cons le ce                          (*   sha3(array_loc++index) <<< .. *) 
      
and codegen_array le ce (aa:ty array) =
    let ce    = gen_array_storLoc le ce aa in
    SLOAD >>> ce 

(* if the stack top is zero, 
 *      then set up an array seed at aa, 
 *           replace the zero with the new seed *)
and setup_array_storLoc le ce aa =
    let label = get_new_label ()                        in       (* stack: [result, result] *)
    let ce = DUP1                                >>> ce in       (* stack: [result, result] *)
    let ce = PUSH4(Label label)                  >>> ce in       (* stack: [result, result, shortcut] *)
    let ce = JUMPI                               >>> ce in       (* stack: [result] *)
    let ce = POP                                 >>> ce in       (* stack: [] *)
    let ce = gen_array_storLoc le ce aa                   in       (* stack: [stor_index] *)
    let ce = PUSH1 (Int 1)                       >>> ce in       (* stack: [stor_index, 1] *)
    let ce = SLOAD                               >>> ce in       (* stack: [stor_index, orig_seed] *)
    let ce = DUP1                                >>> ce in       (* stack: [stor_index, orig_seed, orig_seed] *)
    let ce = incr_top ce 1                              in       (* stack: [stor_index, orig_seed, orig_seed + 1] *)
    let ce = PUSH1 (Int 1)                       >>> ce in       (* stack: [stor_index, orig_seed, orig_seed + 1, 1] *)
    let ce = SSTORE                              >>> ce in       (* stack: [stor_index, orig_seed] *)
    let ce = DUP1                                >>> ce in       (* stack: [stor_index, orig_seed, orig_seed] *)
    let ce = SWAP2                               >>> ce in       (* stack: [orig_seed, orig_seed, stor_index] *)
    let ce = SSTORE                              >>> ce in       (* stack: [orig_seed] *)
    let ce = JUMPDEST label                      >>> ce in       (* stack: [result] *)
    ce

(*   if the stack top is zero, 
 *      then set up an array seed at aa, 
 *      replace the zero with the new seed *)
and setup_array_storLoc_of_loc le ce loc =
    let stor_idx = (match loc with
      | Loc.Stor stor_range     ->  assert (stor_range.Loc.stor_size = (Int 1)) ;
                                    stor_range.Loc.stor_start
      | _                       ->  err "setup array seed at non-storage") in
    let label = get_new_label ()    in    (* stack: [result, result] *)
    let ce = DUP1                   >>> ce in    (* stack: [result, result] *)
    let ce = PUSH32(Label label)    >>> ce in    (* stack: [result, result, shortcut] *)
    let ce = JUMPI                  >>> ce in    (* stack: [result] *)
    let ce = POP                    >>> ce in    (* stack: [] *)
    let ce = PUSH32 stor_idx        >>> ce in    (* stack: [stor_index] *)
    let ce = PUSH1(Int 1)           >>> ce in    (* stack: [stor_index, 1] *)
    let ce = SLOAD                  >>> ce in    (* stack: [stor_index, orig_seed] *)
    let ce = DUP1                   >>> ce in    (* stack: [stor_index, orig_seed, orig_seed] *)
    let ce = incr_top ce 1                 in    (* stack: [stor_index, orig_seed, orig_seed + 1] *)
    let ce = PUSH1(Int 1)           >>> ce in    (* stack: [stor_index, orig_seed, orig_seed + 1, 1] *)
    let ce = SSTORE                 >>> ce in    (* stack: [stor_index, orig_seed] *)
    let ce = DUP1                   >>> ce in    (* stack: [stor_index, orig_seed, orig_seed] *)
    let ce = SWAP2                  >>> ce in    (* stack: [orig_seed, orig_seed, stor_index] *)
    let ce = SSTORE                 >>> ce in    (* stack: [orig_seed] *)
    let ce = JUMPDEST label         >>> ce in    (* stack: [result] *)
    ce








(* le is not updated here.  It can be only updated in
 * a variable initialization *)

and codegen_expr le ce align  ((e,t):ty expr) : ce =
    let ret = (match e,t with
    | EpAddr((c,TyCntrctInstance _)as inner),TyAddr ->
                                inner >>>>> (align,le,ce) 
                                (* c is a contract instance.
                                 * The concrete representation of a contact instance is already the address *)
    | EpAddr _, _          -> errc "EpAddr"

    | EpValue,TyUint256    -> CALLVALUE >>> ce 
    | EpValue,_            -> errc "EpValue"

    | EpSender,TyAddr      -> let ce = CALLER         >>> ce      in
                                let ce = align_address ce align     in
                                ce

    | EpSender,_           -> errc "EpSender"

    | EpArray a,ty         -> assert (align = R_) ; 
                                let ce = codegen_array le ce (read_array a) in
                                begin match ty with
                                | TyMap _   -> setup_array_storLoc le ce (read_array a)
                                | _         -> ce                   end 

    | EpThis,_             -> let ce = ADDRESS        >>> ce      in
                                let ce = align_address ce align     in
                                ce

    | EpIdent id,ty        -> (match lookup le id with
                                (** if things are just DUP'ed, location env should not be updated.  
                                 *  If they are SLOADED,   the location env should be updated. *)
                                | Some loc  ->  let le,ce = copy_to_top le ce align ty loc in
                                                    begin match ty with
                                                    | TyMap _   -> setup_array_storLoc_of_loc le ce loc
                                                    | _         -> ce           end
                                | None      ->  err ("codegen_expr: identifier's location not found: "^id) )

    | EpFalse,TyBool       -> let ce = PUSH1(Big zero_big_int)>>> ce  in
                                assert (align = R_) ; 
                                ce
    | EpFalse, _           -> errc "EpFalse"

    | EpTrue,TyBool        -> let ce = PUSH1(Big unit_big_int)>>> ce  in 
                                assert (align = R_) ; 
                                ce
    | EpTrue, _            -> errc "EpTrue"

    | EpDecLit256 d,TyUint256->
                                let ce = PUSH32(Big d)  >>> ce  in
                                assert (align = R_) ; 
                                ce
    | EpDecLit256 d, _     -> errc ("EpDecLit256 "^(string_of_big_int d))

    | EpDecLit8 d, TyUint8 -> let ce = PUSH1(Big d)   >>> ce  in
                                assert (align = R_) ; 
                                ce
    | EpDecLit8 d, _       -> errc ("EpDecLit8 "^(string_of_big_int d))

    | EpLand(l,r),TyBool   -> let la = get_new_label () in
                                assert (align=R_) ; 
                                let ce = l              >>>>>(R_,le,ce)    in (* stack: [..., l] *)
                                let ce = DUP1           >>>ce              in (* stack: [..., l, l] *)
                                let ce = ISZERO         >>>ce              in (* stack: [..., l, !l] *)
                                let ce = PUSH4(Label la)>>>ce              in (* stack: [..., l, !l, shortcut] *)
                                let ce = JUMPI          >>>ce              in (* stack: [..., l] *)
                                let ce = POP            >>>ce              in (* stack: [...] *)
                                let ce = r              >>>>>(R_,le,ce)    in (* stack: [..., r] *)
                                let ce = JUMPDEST la    >>>ce              in
                                let ce = ISZERO         >>>ce              in
                                let ce = ISZERO         >>>ce              in
                                ce
    | EpLand (_, _), _     -> errc "EpLand"

    | EpNot expr, TyBool   -> let ce = expr           >>>>>(align,le,ce) in
                                let ce = ISZERO         >>>ce              in 
                                align_boolean ce align   
    | EpNot sub, _         -> errc "EpNot"

    | EpNow,TyUint256      ->          TIMESTAMP      >>>ce 
    | EpNow,_              -> errc "EpNow"

    | EpNeq(l,r),TyBool    -> let ce = r              >>>>>(R_,le,ce)    in
                                let ce = l              >>>>>(R_,le,ce)    in
                                let ce = EQ             >>>ce              in
                                let ce = ISZERO         >>>ce              in
                                let ce = align_boolean ce align            in
                                ce
    | EpNeq _, _           -> errc "EpNeq"

    | EpLt(l,r),TyBool     -> let ce = r              >>>>>(R_,le,ce)    in
                                let ce = l              >>>>>(R_,le,ce)    in
                                let ce = LT             >>>ce              in
                                let ce = align_boolean ce align            in
                                ce
    | EpLt _, _            -> errc "EpLt"

    | EpPlus(l,r),TyUint256-> let ce = r              >>>>>(R_,le,ce)    in
                                let ce = l              >>>>>(R_,le,ce)    in
                                         ADD            >>>ce 
    | EpPlus(l,r),TyUint8  -> let ce = r              >>>>>(R_,le,ce)    in
                                let ce = l              >>>>>(R_,le,ce)    in
                                         ADD            >>>ce 
    | EpPlus(l,r),_        -> errc "EpPlus"

    | EpMinus(l,r),TyUint256->let ce = r              >>>>>(R_,le,ce)    in
                                let ce = l              >>>>>(R_,le,ce)    in
                                         SUB            >>>ce 
    | EpMinus(l,r),TyUint8 -> let ce = r              >>>>>(R_,le,ce)    in
                                let ce = l              >>>>>(R_,le,ce)    in
                                         SUB            >>>ce 
    | EpMinus(l,r),_       -> errc "EpMinus"

    | EpMult(l,r),TyUint256->
                                let ce = r              >>>>>(R_,le,ce)    in
                                let ce = l              >>>>>(R_,le,ce)    in
                                         MUL            >>>ce
    | EpMult(l,r),TyUint8  ->
                                let ce = r              >>>>>(R_,le,ce)    in
                                let ce = l              >>>>>(R_,le,ce)    in
                                         MUL            >>>ce
    | EpMult (l, r), _     -> errc "EpMult"

    | EpGt(l,r),TyBool     -> let ce = r              >>>>>(R_,le,ce)    in
                                let ce = l              >>>>>(R_,le,ce)    in
                                let ce = GT             >>>ce              in
                                         align_boolean ce align   
    (* XXX there should be some type system making sure this the above line exists *)
                                
    | EpGt _, _            -> errc "EpGt"

    | EpBalance e,TyUint256->
                                let ce = e              >>>>>(R_,le,ce)    in
                                         BALANCE        >>>ce
    | EpBalance inner, _   -> errc "EpBalance"

    | EpEq(l,r),TyBool     -> let ce = r              >>>>>(R_,le,ce)    in
                                let ce = l              >>>>>(R_,le,ce)    in
                                let ce = EQ             >>>ce              in
                                let ce = align_boolean ce align            in
                                ce
    | EpEq _, _            -> errc "EpEq"

    | EpSend s, _          -> assert (align = R_) ; 
                                codegen_send_expr le ce s

    | EpNew n,TyCntrctInstance c ->
                                assert (align = R_) ; 
                                codegen_new_expr le ce n c
    | EpNew n, _           -> errc "EpNew"

    | EpFnCall fcall,reT  -> codegen_fun_call_expr le ce align fcall reT

    | EpParen _, _         -> errc "EpParen"

    | EpSingleDeref(ref,tyRef),ty ->
                                let size = size_of_ty ty in
                                assert (size <= 32)         ;   (* assuming word-size *)
                                assert (tyRef=TyRef[ty])    ;
                                assert (align=R_)           ; 
                                let ce = (ref,tyRef)    >>>>>(R_,le,ce) in (* pushes the pointer *)
                                         MLOAD          >>>ce 

    | EpTupleDeref _,_     -> err "Codegen EpTupleDeref should not happen.Try to decompose it into several assignments."

    ) in

    assert (stack_size ret = stack_size ce + 1) ; 
    ret

and (>>>>>) expr (align,le,ce)  = codegen_expr le ce align expr 
and errc str                    = err ("codegen_expr: " ^ str ^ " of unexpected type")




(** [prepare_arg ce arg] places an arg in the memory, and increments the stack top position by the size of the arg. *)
and prepare_arg le ce arg =    (* stack: (..., accum) *)
    let start_size  = stack_size ce             in
    let size        = size_of_ty(snd arg)       in
    assert (size = 32) ; 
    let ce = PUSH1 (Int size)   >>>ce           in   (* stack: (..., accum, size) *)
    let ce = arg                >>>>>(R_,le,ce) in   (* stack: (..., accum, size, val) *)
    let ce = DUP2               >>>ce           in   (* stack: (..., accum, size, val, size) *)
    let ce = mem_alloc             ce           in   (* stack: (..., accum, size, val, offset) *)
    let ce = MSTORE             >>>ce           in   (* stack: (..., accum, size) *)
    let ce = ADD                >>>ce           in   (* stack: (..., new_accum) *)
    assert (stack_size ce = start_size) ; 
    ce
(** [prepare_args] prepares args in the memory.
 *  This leaves (..., args size) on the stack.
 *  Since this is called always immediately after allocating memory for the signature,
 *  the offset of the memory is not necessary.
 *  Also, when there are zero amount of memory desired, it's easy to just return zero.
 *)
and prepare_args args le ce =
    let start_size = stack_size    ce in
    let ce = PUSH1 (Int 0)      >>>ce in
    let ce = L.fold_left(prepare_arg le)ce args in
    assert(stack_size ce = start_size+1) ; 
    ce

(** [prepare_input_in_mem] prepares the input for CALL opcode in the memory.
 *  That leaves "..., in size, in offset" (top) on the stack.
 *)
and prepare_input_in_mem le ce s mthd =
    let start_size  = stack_size ce   in
    let args        = s.send_args     in
    let ce = prepare_mthd_sig mthd ce in   (* stack : [sig size, &(mthd_sig) ] *)
    let ce = prepare_args args  le ce in   (* stack : [sigsize, &(mthd_sig), argsize] *)
    let ce = SWAP1              >>>ce in   (* stack : [sigsize, argsize, &(mthd_sig)] *)
    let ce = SWAP2              >>>ce in   (* stack : [&(mthd_sig), argsize, sigsize] *)
    let ce = ADD                >>>ce in   (* stack : [&(mthd_sig), total size] *)
    let ce = SWAP1              >>>ce in   (* stack : [total size, &(mthd_sig)] *)
    assert (stack_size ce = start_size+2) ; 
    ce
(** [obtain_ret_values_from_mem] assumes stack (..., out size, out offset),
    and copies the outputs onto the stack.  The first comes top-most. *)
(* XXX currently supports one-word output only *)
and obtain_ret_values_from_mem ce =         (* stack : out size, out offset *)
    let ce = DUP2               >>>ce in   (* stack : out size, out offset, out size *)
    let ce = PUSH1 (Int 32)     >>>ce in   (* stack : out size, out offset, out size, 32 *)
    let ce = EQ                 >>>ce in   (* stack : out size, out offset, out size = 32 *)
    let ce = ISZERO             >>>ce in   (* stack : out size, out offset, out size != 32 *)
    let ce = PUSH1 (Int 0)      >>>ce in   (* stack : out size, out offset, out size != 32, 0 *)
    let ce = JUMPI              >>>ce in   (* stack : out size, out offset *)
    let ce = MLOAD              >>>ce in   (* stack : out size, out *)
    let ce = SWAP1              >>>ce in   (* stack : out, out size *)
    let ce = POP                >>>ce in   (* stack : out *)
    ce
and codegen_send_expr le ce (s:ty send_expr) =
    let start_size  = stack_size ce         in
    let head_cntrct = s.send_cntrct    in
    match snd head_cntrct with
    | TyCntrctInstance c_name -> 
        let callee_idx          = idx_lookup ce c_name                  in 
        let callee              = cntrct_lookup ce callee_idx           in
        begin match s.send_mthd with
        | None              -> err "Unknown Method Name"
        | Some mthd_name    -> (
        let m : mthd_info = lookup_mthd_info ce callee mthd_name in
        assert(is_throw_only s.send_msg_info.msg_reentrance_info) ; 
        let ret_size    = size_of_tys m.mthd_ret_ty     in (* [pc_bkp] *)
        let ce = swap_pc_with_0            ce           in (* [pc_bkp] *)
        let ce = PUSH1(Int ret_size)    >>>ce           in (* [pc_bkp, retsize] *)
        let ce = DUP1                   >>>ce           in (* [pc_bkp, retsize, retsize] *)
        let ce = mem_alloc                 ce           in (* [pc_bkp, retsize, alloc(retsize)] *)
        let ce = DUP2                   >>>ce           in (* [pc_bkp, retsize, alloc(retsize), retsize] *)
        let ce = DUP2                   >>>ce           in (* [pc_bkp, retsize, alloc(retsize), retsize, alloc(retsize)] *)
        let ce = prepare_input_in_mem le ce s m         in (* [pc_bkp, retsize, alloc(retsize), retsize, alloc(retsize), insize, alloc(insize)] *)
        assert (stack_size ce = start_size+7) ; 
        let ce = push_msg_and_gas s     le ce           in (* [alloc(retsize), retsize] *)
        let ce = call_and_restore_pc       ce           in 
        let ce = SWAP1                  >>>ce           in (* [retsize,alloc(retsize)] *)
        let ce = obtain_ret_values_from_mem ce          in (* [ret] *)
        ce ) end
    | TyAddr        -> (
        assert (is_throw_only s.send_msg_info.msg_reentrance_info) ; 
        let ret_size    = 0                             in 
        let ce = swap_pc_with_0            ce           in (* [pc_bkp] *)
        let ce = PUSH1(Int ret_size)    >>>ce           in (* [pc_bkp, 0] *)
        let ce = DUP1                   >>>ce           in (* [pc_bkp, 0, 0] *)
        let ce = DUP2                   >>>ce           in (* [pc_bkp, 0, 0, 0] *)
        let ce = DUP2                   >>>ce           in (* [pc_bkp, 0, 0, 0, 0] *)
        let ce = DUP2                   >>>ce           in (* [pc_bkp, 0, 0, 0, 0, 0] *)
        let ce = DUP2                   >>>ce           in (* [pc_bkp, 0, 0, 0, 0, 0, 0] *)
        assert (stack_size ce = start_size+7) ;   
        let ce = push_msg_and_gas s     le ce           in 
        let ce = call_and_restore_pc       ce           in 
        let ce = POP                    >>>ce           in (* [0] *)
        ce )
    | TyVoid        -> err "send expression with TyVoid?"
    | TyUint256     -> err "send expression with TyUint256?"
    | TyUint8       -> err "send expression with TyUint8?"
    | _             -> err "send expression with unknown type"





and push_msg_and_gas s le ce = 
    let ce = match s.send_msg_info.msg_value_info with 
        | None      -> PUSH1(Int 0) >>>ce 
        | Some e    -> e            >>>>>(R_,le,ce) in 
    let ce = s.send_cntrct          >>>>>(R_,le,ce) in
    let ce = PUSH4(Int 3000)        >>>ce           in
    let ce = GAS                    >>>ce           in
    let ce = SUB                    >>>ce           in
    ce 

and call_and_restore_pc ce = 
    let ce = CALL                   >>>ce           in 
    let ce = PUSH1(Int 0)           >>>ce           in 
    let ce = JUMPI                  >>>ce           in 
    let ce = SWAP2                  >>>ce           in 
    restore_pc ce 














let codegen_stmt     (orig:ce) (s:ty stmt)     : ce     = err "codegen_stmt"
let move_info_around (assumption:ce) (goal:le) : ce     = err "move_info_around"
let codegen_bytecode (src:ty cntrct) : imm Evm.program  = err "codegen_bytecode"



(****************************)
(*  Init MemAlloc           *)
(****************************)


(* [init_mem_alloc] initialize as M[64] := 96 *)
let init_mem_alloc ce =
    let ce = PUSH1 (Int 96)           >>> ce in
    let ce = PUSH1 (Int 64)           >>> ce in
    let ce = MSTORE                   >>> ce in
    ce




(****************************)
(*      CONTRACT PC         *) 
(****************************)
(**
 * [set_cntrct_pc ce id] puts the program counter for the cntrct specified by
   [id] in the storage at index [StorPCIndex]
 *)
let set_cntrct_pc ce idx =
    let start_size = stack_size ce in
    let ce = PUSH32 (CntrctOffsetInRuntimeCode idx)   >>> ce in
    let ce = PUSH32 StorPCIndex                       >>> ce in
    let ce = SSTORE                                   >>> ce in
    assert (stack_size ce = start_size) ; 
    ce
(**
 * [get_cntrct_pc ce] pushes the value at [StorPCIndex] in storage.
 *)
let get_cntrct_pc ce =
    let start_size = stack_size ce in
    let ce = PUSH32 StorPCIndex       >>> ce in
    let ce = SLOAD                    >>> ce in
    assert (stack_size ce = start_size + 1) ;
    ce



  

(****************************)
(*  mem --COPY CODE--> stor *) 
(****************************)
(**
 * [bulk_store_from_mem ce]
 * adds opcodes to ce after which some memory contents are copied to the storage.
 * Precondition:    the stack has [..., size, mem_src_start, stor_tgt_start]
 * Postcondition:   the stack has [...]
 *)
let copy_code_from_mem ce =
    let start_size  = stack_size ce     in       (* TODO: check that size is a multiple of 32 *)
    let label       = get_new_label()   in
    let exit        = get_new_label()   in
    let ce = JUMPDEST label         >>>ce   in (*                          idx <<< mem_start <<< size <<< .. *)
    let ce = DUP3                   >>>ce   in (*                 size <<< idx <<< mem_start <<< size <<< .. *)
    let ce = ISZERO                 >>>ce   in (*          size_iszero <<< idx <<< mem_start <<< size <<< .. *)
    let ce = PUSH4(Label exit)      >>>ce   in (* exit <<< size_iszero <<< idx <<< mem_start <<< size <<< .. *)
    assert (stack_size ce = start_size+2)   ;
    (* if size_iszero then GOTO exit *) 
    let ce = JUMPI                  >>>ce   in (*                          idx <<< mem_start <<< size <<< .. *)   
    let ce = DUP2                   >>>ce   in (*            mem_start <<< idx <<< mem_start <<< size <<< .. *) 
    let ce = MLOAD                  >>>ce   in (*         M[mem_start] <<< idx <<< mem_start <<< size <<< .. *)
    let ce = DUP2                   >>>ce   in (* idx <<< M[mem_start] <<< idx <<< mem_start <<< size <<< .. *)
    let ce = SSTORE                 >>>ce   in (* S[idx]=M[mem_start]      idx <<< mem_start <<< size <<< .. *)  
    (* decrease size *)
    let ce = PUSH32(Int 32)         >>>ce   in (*                   32 <<< idx <<< mem_start <<< size <<< .. *)
    let ce = SWAP1                  >>>ce   in (*                   idx <<< 32 <<< mem_start <<< size <<< .. *)
    let ce = SWAP3                  >>>ce   in (*                   size <<< 32 <<< mem_start <<< idx <<< .. *)
    let ce = SUB                    >>>ce   in (*                       size-32 <<< mem_start <<< idx <<< .. *)
    let ce = SWAP2                  >>>ce   in (*                       idx <<< mem_start <<< size-32 <<< .. *) 
    let ce = incr_top ce 1(*word*)          in (*                     idx+1 <<< mem_start <<< size-32 <<< .. *)
    let ce = SWAP1                  >>>ce   in (*                     mem_start <<< idx+1 <<< size-32 <<< .. *)
    (* increase mem_src_start *)
    let ce = incr_top ce 32                 in (*                  mem_start+32 <<< idx+1 <<< size-32 <<< .. *)
    let ce = SWAP1                  >>>ce   in (*                  idx+1 <<< mem_start+32 <<< size-32 <<< .. *)
    let ce = PUSH4(Label label)     >>>ce   in (*        label <<< idx+1 <<< mem_start+32 <<< size-32 <<< .. *) 
    let ce = JUMP                   >>>ce   in (*                  idx+1 <<< mem_start+32 <<< size-32 <<< .. *)
    (* ????? let ce = set_stack_size ce start_size   in *) 
    let ce = JUMPDEST exit          >>>ce   in (*                          idx <<< mem_start <<< size <<< .. *)
    let ce = POP                    >>>ce   in (*                                  mem_start <<< size <<< .. *) 
    let ce = POP                    >>>ce   in (*                                                size <<< .. *) 
    let ce = POP                    >>>ce   in (*                                                         .. *)
    assert (stack_size ce = start_size - 3) ;
    ce

(** [copy_args_from_mem_to_stor le ce]
 *  adds opcodes to ce s.t.  the cnstrctr args stored in the mem are copied to the storage.
 *
 *  Precondition  of the stack : [..., total, mem_start]
 *  Postcondition of the stack : [...] 
 *  Final storage has the args in [CnstrctrArgumentBegin...CnstrctrArgumentBegin + CnstrctrArgumentLength]
 *)
let copy_args_from_mem_to_stor le ce idx =
    let ce = PUSH32(InitDataSize idx)           >>>ce   in (*                                    datasize <<< mem_start <<< size <<< .. *)
    let ce = CODESIZE                           >>>ce   in (*                       codesize <<< datasize <<< mem_start <<< size <<< .. *) 
    let ce = EQ                                 >>>ce   in (* (eq := if codesize==datasize then 1 else 0) <<< mem_start <<< size <<< .. *) 
    let ce = ISZERO                             >>>ce   in (*                                      not eq <<< mem_start <<< size <<< .. *) 
    let ce = PUSH1 (Int 2)                      >>>ce   in (*                                2 <<< not eq <<< mem_start <<< size <<< .. *)
    (* IF noteq THEN GOTO 2     *) 
    let ce = JUMPI                              >>>ce   in (*                                                 mem_start <<< size <<< .. *) 
    (* ELSE                     *) 
    let ce = PUSH32(StorCnstrctrArgsBegin idx)  >>>ce   in (*                                         idx <<< mem_start <<< size <<< .. *)
    copy_code_from_mem ce


(** [copy_args_from_code_to_mem]
 *  copies cnstrctr args at the end of the bytecode into the memory.  
 *  The number of bytes is deidxed using the cntrct interface.
 *  The memory usage counter at byte [0x40] is increased accordingly.
 *  After this, the stack contains the size and the beginning of the memory piece that contains the args.
 *  Output [rest of the stack, mem_size, mem_begin].
 *)
let copy_args_from_code_to_mem le ce (cn:ty cntrct) =
    let total_size = Eth.total_size_of_args (L.map snd (Eth.cnstrctr_args cn)) in
    let start_size = stack_size ce        in (* [] *)
    let ce = PUSH32(Int total_size) >>>ce in (* [total_size] *)
    let ce = DUP1                   >>>ce in (* [total_size, total_size] *)
    let ce = mem_alloc                 ce in (* [total_size, mem_start] *)
    let ce = DUP2                   >>>ce in (* [total_size, mem_start, total_size] *)
    let ce = DUP1                   >>>ce in (* [total_size, mem_start, total_size, total_size] *)
    let ce = CODESIZE               >>>ce in (* [total_size, mem_start, total_size, total_size, code size] *)
    let ce = SUB                    >>>ce in (* [total_size, mem_start, total_size, code_begin] *)
    let ce = DUP3                   >>>ce in (* [total size, mem_start, total_size, code_begin, mem_start *)
    let ce = CODECOPY               >>>ce in (* [total size, mem_start] *)
    assert (start_size + 2 = stack_size ce) ; 
    ce







(* S[1] := ARRAY SEED COUNTER *) 
(*
 * S[loc] := old seed 
 * S[1]   := S[1] + 1 
 *)
let setup_seed (le,ce) (array_loc:LI.stor_location) =
    let label       = get_new_label()   in
    let start_size  = stack_size ce     in
    let ce = PUSH4 (Int array_loc)  >>>ce in (* loc  <<< .. *)
    let ce = PUSH4 (Label label)    >>>ce in (* label <<< loc <<< .. *)
    let ce = JUMPI                  >>>ce in (* IF loc!=0 GOTO label *) 
    let ce = PUSH1 (Int 1)          >>>ce in (*   1  <<< .. *)
    let ce = SLOAD                  >>>ce in (* S[1] <<< .. *)
    let ce = DUP1                   >>>ce in (* S[1] <<< S[1] <<< .. *)
    let ce = PUSH4 (Int array_loc)  >>>ce in (* loc  <<< S[1] <<< S[1] <<< .. *)
    let ce = SSTORE                 >>>ce in (* S[loc]:=S[1] *)
    let ce = incr_top ce 1                in (*S[1]+1<<< .. *)
    let ce = PUSH1 (Int 1)          >>>ce in (*   1  <<< S[1]+1 <<< .. *)
    let ce = SSTORE                 >>>ce in (* S[1]:=S[1]+1 *)
    let ce = JUMPDEST label         >>>ce in (* .. *)
    assert (stack_size ce = start_size) ; 
    le, ce


let reset_array_seed_counter ce = 
(* let setup_array_seed_counter_to_one_if_not_initialized ce = *)
    let start_size  = stack_size ce     in
    let label       = get_new_label()   in
    let ce = PUSH1 (Int 1)          >>>ce in (* 1 >>> .. *)
    let ce = SLOAD                  >>>ce in (* S[1] >>> .. *)
    let ce = PUSH4 (Label label)    >>>ce in (* label >>> S[1] >>> ..*)
    let ce = JUMPI                  >>>ce in (* IF S[1]=0 GOTO label ..*) 
    (* the mthd where it has to be changed *)
    let ce = PUSH1 (Int 1)          >>>ce in (* 1 >>> .. *)
    let ce = DUP1                   >>>ce in (* 1 >>> 1 .. *)
    let ce = SSTORE                 >>>ce in (* S[1]:=1    *) 
    let ce = JUMPDEST label         >>>ce in 
    assert (stack_size ce = start_size) ; 
    ce



(* array[0] := seed 
 * array[1] := seed + 1
 * array[2] := seed + 2 
 * ..
 * array[n] := seed + n *) 

let setup_array_seeds le ce (cn:ty cntrct) =
    let ce            = reset_array_seed_counter ce in
    let array_locs    = LI.array_locations cn in
    let _,ce          = L.fold_left setup_seed (le,ce) array_locs in
    ce




(* CODEGEN CONSTRUCTOR *) 

let codegen_cnstrctr_bytecode ((cns:ty cntrct idx_list), idx) : ce (* containing the program *)  =
    let cn      = choose_cntrct idx cns                         in 
    let le      = cnstrctr_initial_env idx cn                   in
    let ce      = empty_ce (idx_lookup_in_assoc cns) cns        in
    let ce      = init_mem_alloc                         ce     in
    let ce      = copy_args_from_code_to_mem le ce cn           in (* stack: [arg_mem_size, arg_mem_begin] *)
    let ce      = copy_args_from_mem_to_stor le ce idx          in (* stack: [] *)
    let ce      = setup_array_seeds le ce cn                    in
    let ce      = set_cntrct_pc ce idx                          in (* stack: [] *)
    let ce      = copy_runtime_code_to_mem ce idx               in (* stack: [code_length, code_start_on_memory] *)
    RETURN >>> ce 
























type cnstrctr_compiled      =
                            { cnstrctr_codegen_env      : ce
                            ; cnstrctr_intf             : cntrct_intf
                            ; cnstrctr_cntrct           : ty cntrct                     }

type runtime_compiled       =
                            { runtime_codegen_env       : ce
                            ; runtime_cntrct_offsets    : int idx_list                  }
                            (* what form should the cnstrctr code be encoded?
                               1. pseudo program.  easy
                               2. pseudo codegen_env.  maybe uniform
                             *)

let empty_runtime_compiled idx_lookup layouts =
    { runtime_codegen_env       = empty_ce idx_lookup layouts
    ; runtime_cntrct_offsets    = []                            }

let compile_cnstrctr (cns,idx) : cnstrctr_compiled =
    { cnstrctr_codegen_env      = codegen_cnstrctr_bytecode (cns, idx)
    ; cnstrctr_intf             = cntrct_intf_of (L.assoc idx cns)
    ; cnstrctr_cntrct           = L.assoc idx cns                           }

let compile_cnstrctrs cns : cnstrctr_compiled idx_list =
    pair_map (fun idx _ -> compile_cnstrctr (cns,idx)) cns

let initial_runtime_compiled idx_lookup layouts : runtime_compiled =
    let ce = empty_ce idx_lookup layouts in
    let ce = get_cntrct_pc     ce  in
    let ce = JUMP           >>>ce  in
    { runtime_codegen_env       = ce
    ; runtime_cntrct_offsets    = [] }

let push_mthd_dest ce idx m_hd =
    PUSH32(MthdAddrInRuntimeCode(idx,m_hd)) >>>ce 

let add_dispatcher_for_a_mthd_info le ce idx m =   
    let start_size = stack_size ce              in 
    let ce = DUP1                       >>>ce   in 
    let ce = push_method ce m                   in
    let ce = EQ                         >>>ce   in
    let ce = push_mthd_dest ce idx(Method m)    in
    let ce = JUMPI                      >>>ce   in
    assert (stack_size ce = start_size) ; 
    ce

let add_dispatcher_for_default_mthd le ce idx =
    let start_size = stack_size ce              in
    let ce = push_mthd_dest ce idx Default      in
    let ce = JUMP                       >>>ce   in
    assert (stack_size ce = start_size) ;
    ce

let push_word_of_inputdata_at_byte ce b =
    let start_size = stack_size ce in
    let ce = PUSH32 b                   >>>ce   in
    let ce = CALLDATALOAD               >>>ce   in
    assert (stack_size ce = start_size + 1) ; 
    ce


let add_throw ce =
    (* Just using the same method as solc. *)
    let ce = PUSH1 (Int 2)              >>>ce   in
    let ce = JUMP                       >>>ce   in
    ce

let add_dispatcher le ce idx cntrct =
    let start_size = stack_size ce in

    (* load the first four bytes of the input data *)
    let ce = push_word_of_inputdata_at_byte ce (Int 0) in
    let ce = shiftR_top ce Eth.(word_bits - sig_bits) in
    assert (stack_size ce = start_size + 1) ;
    let mthd_sigs   = L.map (fun x->x.mthd_head) cntrct.mthds in
    let mthd_infos  = BL.filter_map 
                         (function  | Default  -> None 
                                    | Method u -> Some u) mthd_sigs in 
    let ce = L.fold_left (fun ce -> add_dispatcher_for_a_mthd_info le ce idx) ce mthd_infos in
    let ce = POP                        >>>ce   in (* the signature in input is not necessary anymore *)
    let ce = if L.exists (function  | Default  -> true | _ -> false) mthd_sigs 
                    then add_dispatcher_for_default_mthd le ce idx
                    else add_throw ce   in
    le,ce

let add_mthd_dest ce idx (m:mthd_head) =
    let label   = get_new_label()               in
    let ce = JUMPDEST label             >>>ce   in
    Entrypoint.(register_entrypoint(Case(idx,m))label) ; 
    ce


(** [prepare_words_on_stack le ce [arg0 arg1]] evaluates
 * [arg1] and then [arg0] and puts them onto the stack.
 * [arg0] will be the topmost element of the stack.
 *)
let prepare_words_on_stack le ce args =
    let ce = L.fold_right (fun arg ce->arg >>>>>(R_,le,ce)) args ce in 
    le,ce

let store_word_into_stor_location (le,ce) (stor_loc : LI.stor_location) =
    let ce = PUSH32 (Int stor_loc)      >>>ce   in
    let ce = SSTORE                     >>>ce   in
    le,ce

(** [store_words_into_stor_locations le ce arg_locations] moves the topmost stack element to the
 *  location indicated by [arg_locations] and the next element to the next location and so on.
 *  The stack elements will disappear.
 *)
let store_words_into_stor_locations le ce arg_locations =
    L.fold_left store_word_into_stor_location (le,ce) arg_locations

let set_cntrct_args le ce offset idx args = 
    let cntrct  =   try cntrct_lookup ce idx
                    with e ->(eprintf "set_cntrct_args: looking up %d\n" idx; raise e) in 
    let arglocs = LI.arg_locations offset cntrct  in
    assert (L.length arglocs = L.length args) ; 
    let le,ce   =   prepare_words_on_stack le ce args in
    let le,ce   =   store_words_into_stor_locations le ce arglocs in
    (* TODO: In a special case where no movements are necessary, we can then skip these args. *)
    le,ce

let set_cont_to_fncall le ce layout (fncall, ty_expr) =
    let hd      =   fncall.call_head in
    let args    =   fncall.call_args in
    let idx     =   idx_lookup ce hd in 
    let ce      =   set_cntrct_pc ce idx in
    let offset  =   layout.LI.stor_cnstrctr_args_begin idx in
    let le,ce   =   try set_cntrct_args le ce offset idx args
                    with e  ->(eprintf "Cntrct: %s\n" hd;
                               eprintf "set_cont_to_fncall idx: %d\n" idx; raise e) in
    le,ce 


(* set_cont sets the storage contents.
 * So that the next message call would start from the continuation. *)
let set_cont le ce layout (cont, ty) =
    let start_size  = stack_size ce     in
    let le,ce       = match cont with
        | EpFnCall fncall    -> set_cont_to_fncall le ce layout (fncall,ty)
        | _                     -> err "strange_continuation" in
    assert (stack_size ce = start_size) ;
    le,ce




(*       mstore_word ty 
 *
 *     BEFORE           AFTER               
 *
 *                   +---------+            
 *                   |alloc(32)|            
 *   +---------+     +---------+            
 *   |  value  |     |    32   |            
 * --+---------+-- --+---------+-- 
 * *)
let mstore_word ty le ce =
    assert (size_of_ty ty <= 32)  ;             (* ..., val *)
    let ce = PUSH1(Int 32)          >>> ce  in  (* ..., val, 32 *)
    let ce = DUP1                   >>> ce  in  (* ..., val, 32, 32 *)
    let ce = mem_alloc                  ce  in  (* ..., val, 32, alloc(32) *)
    let ce = SWAP2                  >>> ce  in  (* ..., alloc(32), 32, val *)
    let ce = DUP3                   >>> ce  in  (* ..., alloc(32), 32, val, alloc(32) *)
    let ce = MSTORE                 >>> ce  in  (* ..., alloc(32), 32 *)
    let ce = SWAP1                  >>> ce  in  (* ..., 32, alloc(32) *)
    ce


(*
 * after this, the stack contains
 * ..., size, alloc(size) 
 *)
let place_expr_in_mem le ce pack ((e,ty):ty expr) =
    let start_size  = stack_size ce                 in
    let align   = match pack with
        | ABIPack       -> R_
        | TightPack     -> L_                       in
    let ce = (e,ty)             >>>>>(align,le,ce)  in
    let ce = mstore_word ty le ce                in     (* the stack layout depends on ty *)
    assert (stack_size ce = 2+start_size) ; 
    le,ce

(*
 * When called on [a, b, c], a shoud occupy the smallest address, and c should occupy the largest address.
 * after this, the stack contains
 * ..., size, offset_in_mem
 *)
let rec place_exprs_in_mem le ce pack = function 
    | []         ->
                    let ce    = PUSH1(Int 0)                >>> ce  in
                    let ce    = PUSH1(Int 0)                >>> ce  in
                    le, ce
    | expr::rest ->
                    let le,ce = place_expr_in_mem le ce pack expr   in (* stack : [size, offset] *)
                    let ce    = SWAP1                       >>> ce  in (* stack : [offset, size] *)
                    let le,ce = place_exprs_in_mem le ce pack rest  in (* stack : [offset, size, size', offset] *)
                    let ce    = POP                         >>> ce  in (* stack : [offset, size, size'] *)
                    let ce    = ADD                         >>> ce  in (* stack : [offset, size_sum] *)
                    let ce    = SWAP1                       >>> ce  in (* stack : [size_sum, offset] *)
                    le, ce


(*
 * return_mem_content assumes the stack left after place_expr_in_mem
 * ..., size, offset_in_mem
 *)
let return_mem_content le ce    = RETURN >>> ce 


let add_return le ce layout ret =
    let start_size = stack_size ce          in
    let e       = ret.ret_expr              in
    let c       = ret.ret_cont              in
    let le,ce   = set_cont le ce layout c   in
    let ce      = match e with
      | Some e  ->  let le,ce   = place_expr_in_mem le ce ABIPack e in
                    return_mem_content le ce
      | None    ->  STOP            >>> ce  in
    assert (stack_size ce = start_size) ; 
    le,ce

let put_stacktop_into_array le ce layout (aa : ty array) =
    let array = aa.array_name       in
    let index = aa.array_index       in
    let ce = index          >>>>>(R_,le,ce) in   (* stack : [value, index] *)
    let ce = array          >>>>>(R_,le,ce) in   (* stack : [value, index, array_seed] *)
    let ce = keccak_cons    le ce           in   (* stack : [value, kec(array_seed ^ index)] *)
    let ce = SSTORE         >>>ce           in
    ce

let put_stacktop_into_lexpr le ce layout = function 
    | LExprArray aa -> 
    let start_size = stack_size ce in
    let ce = put_stacktop_into_array le ce layout aa in
    assert (start_size = stack_size ce + 1) ; 
    ce

let add_assign le ce layout l r =
    let start_size = stack_size ce in
    (* produce r on the stack and then think about where to put that *)
    let ce = r              >>>>>(R_,le,ce) in
    assert (1+start_size = stack_size ce) ; 
    let ce = put_stacktop_into_lexpr le ce layout l in
    assert (start_size = stack_size ce) ; 
    le, ce

let push_event ce event =
    let hash    = Eth.event_sig_hash event in
    let ce      = PUSH4 (Big (Eth.hex_to_big_int hash)) >>> ce in
    ce

let add_varDecl le ce layout i =
    let pos     = stack_size ce                         in
    let name    = i.varDecl_id                          in
    let ce      = i.varDecl_val         >>>>>(R_,le,ce) in
    let loc     = Loc.Stack (pos + 1)                   in
    let le      = add_pair le(name,loc)                 in
    le, ce






let rec add_if_single le ce layout cond body =
    let label       = get_new_label ()              in
    let start_size  = stack_size ce                 in
    let ce      = codegen_expr le ce R_ cond        in
    let ce      = ISZERO                    >>> ce  in
    let ce      = PUSH4(Label label)        >>> ce  in 
    let ce      = JUMPI                     >>> ce  in
    let le,ce   = add_stmts le ce layout body       in
    let ce      = JUMPDEST label            >>> ce  in
    assert (start_size = stack_size ce) ; 
    le,ce

and add_if le ce layout cond bodyT bodyF =
    let label       = get_new_label()   in
    let next        = get_new_label()   in
    let start_size  = stack_size ce     in
    let ce      = codegen_expr le ce R_ cond        in
    let ce      = ISZERO                    >>> ce  in
    let ce      = PUSH4(Label label)        >>> ce  in
    let ce      = JUMPI                     >>> ce  in
    let _,ce    = add_stmts le ce layout bodyT      in (* location env needs to be discarded *)
    let ce      = PUSH4(Label next)         >>> ce  in
    let ce      = JUMP                      >>> ce  in
    let ce      = JUMPDEST label            >>> ce  in
    let _,ce    = add_stmts le ce layout bodyF      in (* location env needs to be discarded *)
    let ce      = JUMPDEST next             >>> ce  in
    assert (start_size = stack_size ce) ; 
    le,ce

and add_stmts le ce layout ss =
    L.fold_left (fun(le,ce)->add_stmt le ce layout) (le,ce) ss

and add_stmt le ce layout = function 
    | SmAbort                     -> le, add_throw ce
    | SmReturn ret                -> add_return        le ce layout ret
    | SmAssign (l,r)              -> add_assign        le ce layout l r
    | SmVarDecl i                 -> add_varDecl       le ce layout i
    | SmIfThen(cond,body)         -> add_if_single     le ce layout cond body (* this is a special case of the next *)
    | SmIfThenElse(cond,bodyT,bodyF)  -> add_if            le ce layout cond bodyT bodyF
    | SmSelfDestruct expr         -> add_self_destruct le ce layout expr
    | SmExpr expr                 -> add_expr_stmt     le ce layout expr
    | SmLog(name,args,Some event) -> add_log_stmt      le ce layout name args event
    | SmLog(name,args,None)       -> err "add_stmt: type check first"

and add_log_stmt le ce layout name args event =
    let start_size = stack_size ce in
    (* get the indexed *)
    let indexed_args,non_indexed_args = split_event_args event args in
    (* prepare indexed args on the stack *)
    let le,ce   = prepare_words_on_stack le ce indexed_args in
    (* prepare the event signature on the stack *)
    let ce      = push_event ce event       in
    (* prepare non-indexed args in the memory *)
    let le,ce   = place_exprs_in_mem le ce ABIPack non_indexed_args in (* stack : [..., size, offset] *)
    let n       = L.length indexed_args + 1 in
    let ce      = log n     >>>ce           in
    (* deidxe N in logN *)
    assert (stack_size ce = start_size) ;
    le, ce

and add_expr_stmt le ce layout expr =
    let ce = expr           >>>>>(R_,le,ce) in
    let ce = POP            >>>ce           in
    le, ce

and add_self_destruct le ce layout expr =
    let ce = expr           >>>>>(R_,le,ce) in
    let ce = SELFDESTRUCT   >>>ce           in
    le, ce













let add_mthd_arg_locations le (m : ty mthd) =
    let locEnv = Eth.args_with_locations m in
    add_pairs le locEnv

let calldatasize_of_usual_header us =
    let args = us.mthd_args in
    4 (* for signature *) +
        try     BL.sum (L.map (fun x ->size_of_ty x.ty) args) 
        with    Invalid_argument _ -> 0

let add_mthd_arg_len_chk ce = function 
    | Default       -> (* no check, the choice is arguable *) ce
    | Method us     ->
    let ce    = PUSH4(Int(calldatasize_of_usual_header us))   >>> ce in
    let ce    = CALLDATASIZE                                  >>> ce in
    let ce    = EQ                                            >>> ce in
    let ce    = ISZERO                                        >>> ce in
    let ce    = PUSH1(Int 0)                                  >>> ce in
    let ce    = JUMPI                                         >>> ce in
    ce

let add_mthd le ce layout idx m =
    let ce    = add_mthd_dest ce idx m.mthd_head in
    let ce    = add_mthd_arg_len_chk ce m.mthd_head in
    let le    = add_empty_locEnv le in
    let le    = add_mthd_arg_locations le m in
    let le,ce = L.fold_left(fun(le,ce)-> add_stmt le ce layout)(le,ce)m.mthd_body in
    le, ce

let codegen_append_cntrct_bytecode le ce layout((idx,cntrct):idx*ty cntrct) =
    (* jump destination for the cntrct *)
    let label = get_new_label () in
    let ce    = append_opcode ce (JUMPDEST label) in
    (* update the entrypoint database with (id, pc) pair *)
    Entrypoint.(register_entrypoint (Cntrct idx) label); 
    let ce    = init_mem_alloc ce in
    (* add jumps to the mthds *)
    let le,ce = add_dispatcher le ce idx cntrct in
    (* add the mthds *)
    let mthds = cntrct.mthds in
    let le,ce = L.fold_left(fun(le,ce)->add_mthd le ce layout idx)(le,ce)mthds in
    ce

let append_runtime layout(prev:runtime_compiled)(idx, (cn : ty cntrct)) : runtime_compiled =
    { runtime_codegen_env     = codegen_append_cntrct_bytecode (runtime_initial_env cn)prev.runtime_codegen_env layout(idx,cn)
    ; runtime_cntrct_offsets  = insert idx (code_length prev.runtime_codegen_env)prev.runtime_cntrct_offsets
    }

let compile_runtime layout (cns : ty cntrct idx_list) : runtime_compiled = 
    L.fold_left (append_runtime layout) (initial_runtime_compiled (idx_lookup_in_assoc cns) cns) cns

let stor_layout_from_cnstrctr_compiled (cc : cnstrctr_compiled) : LI.cntrct_stor_layout =
    LI.stor_layout_of_cntrct cc.cnstrctr_cntrct (extract_program cc.cnstrctr_codegen_env)

let sizes_of_cnstrctrs (cnstrctrs : cnstrctr_compiled idx_list) : int list =
    let lengths = map    (fun cc -> code_length cc.cnstrctr_codegen_env) cnstrctrs in
    let lengths = L.sort (fun a b-> compare(fst a)(fst b)) lengths in
    L.map snd lengths

let rec calculate_offsets_inner ret current = function
    | []        -> L.rev ret
    | hd::tl    -> calculate_offsets_inner (current::ret) (current + hd) tl

let calculate_offsets init l   = calculate_offsets_inner [] init l

let stor_layout_from_runtime_compiled (rc:runtime_compiled) (cnstrctrs:cnstrctr_compiled idx_list) : LI.runtime_stor_layout =
    let sizes_of_cnstrctrs       = sizes_of_cnstrctrs cnstrctrs in
    let offsets_of_cnstrctrs     = calculate_offsets (code_length rc.runtime_codegen_env) sizes_of_cnstrctrs in
    let sum_of_cnstrctr_sizes    = BL.sum sizes_of_cnstrctrs in
    LI.(
        { runtime_code_size             = sum_of_cnstrctr_sizes + code_length rc.runtime_codegen_env
        ; runtime_offset_of_idx         = rc.runtime_cntrct_offsets
        ; runtime_size_of_cnstrctr      = to_idx_list sizes_of_cnstrctrs
        ; runtime_offset_of_cnstrctr    = to_idx_list offsets_of_cnstrctrs })

let concat_programs_rev (programs : 'imm Evm.program list) =
    let rev_programs = L.rev programs in
    L.concat rev_programs

(** cnstrctrs_packed concatenates cnstrctr code.
 *  Since the code is stored in the reverse order, the concatenation is also reversed. *)
let cnstrctrs_packed layout (cnstrctrs : cnstrctr_compiled idx_list) =
    let programs            = map (fun cc -> extract_program cc.cnstrctr_codegen_env) cnstrctrs in
    let programs            = L.sort (fun a b -> compare (fst a) (fst b)) programs in
    let programs            = L.map snd programs in
    concat_programs_rev programs

let compose_bytecode (cnstrctrs : cnstrctr_compiled idx_list)
                     (runtime : runtime_compiled) idx : big_int Evm.program =
    let cntrcts_stor_layout : (idx * LI.cntrct_stor_layout) list =
      L.map (fun (id, const) -> (id, stor_layout_from_cnstrctr_compiled const)) cnstrctrs in
    let runtime_layout      = stor_layout_from_runtime_compiled runtime cnstrctrs in
    let layout              = LI.cnstrct_post_stor_layout cntrcts_stor_layout runtime_layout in
    let pseudo_cnstrctr     = choose_cntrct idx cnstrctrs in
    let imm_cnstrctr        = LI.realize_program layout idx (extract_program pseudo_cnstrctr.cnstrctr_codegen_env) in
    let pseudo_runtime_core = extract_program runtime.runtime_codegen_env in
    (* Sicne the code is stored in the reverse order, the concatenation is also reversed. *)
    let imm_runtime         = LI.realize_program layout idx ((cnstrctrs_packed layout cnstrctrs)@pseudo_runtime_core) in
    (* the code is stored in the reverse order *)
    imm_runtime @ imm_cnstrctr

let compose_runtime_bytecode (cnstrctrs : cnstrctr_compiled idx_list)
                     (runtime : runtime_compiled) : big_int Evm.program =
    let cntrcts_stor_layout : (idx * LI.cntrct_stor_layout) list =
      L.map (fun (id, const) -> (id, stor_layout_from_cnstrctr_compiled const)) cnstrctrs in
    let runtime_layout  = stor_layout_from_runtime_compiled runtime cnstrctrs in
    let layout          = LI.cnstrct_post_stor_layout cntrcts_stor_layout runtime_layout in
    (* TODO: 0 in the next line is a bit ugly. *)
    let imm_runtime     = LI.realize_program layout 0 ((cnstrctrs_packed layout cnstrctrs)@(extract_program runtime.runtime_codegen_env)) in
    imm_runtime
