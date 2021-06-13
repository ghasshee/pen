open Label
open Big_int
open Printf 

open Misc
open Imm
open CodegenEnv
open LocationEnv
open Evm
open Syntax
open IndexedList
open Contract

module Loc  = Location 
module Eth  = Ethereum 
module BL   = BatList
module L    = List
module SL   = StorLayout




let push_storRange le ce (range : imm Loc.stor_range) =
    assert (is_const_int 1 range.Loc.stor_size) ; 
    let offset:imm  = range.Loc.stor_start in
    let ce    = PUSH32 offset     >>> ce in
    let ce    = SLOAD             >>> ce in
    le, ce

let copy_nth_stack_elem le ce (n:int) =
    let start_size    = stack_size ce  in
    let diff          = start_size - n in
    assert (diff >= 0) ; 
    let ce    = dup_succ diff     >>> ce in
    assert (stack_size ce = start_size+1) ; 
    le,ce


let shiftR_top ce bits =
    assert (0 <= bits && bits < 256) ; 
    if bits=0 then ce else                     (*                 x >>> .. *) 
    let ce    = PUSH1 (Int bits)   >>> ce in   (*        bits >>> x >>> .. *)
    let ce    = PUSH1 (Int 2)      >>> ce in   (*  2 >>> bits >>> x >>> .. *)
    let ce    = EXP                >>> ce in   (*     2**bits >>> x >>> .. *) 
    let ce    = SWAP1              >>> ce in   (*     x >>> 2**bits >>> .. *) 
    let ce    = DIV                >>> ce in   (*       x/(2**bits) >>> .. *) 
    ce

let shiftL_top ce bits =
    assert (bits >= 0 ) ; 
    assert (bits < 256) ; 
    if bits=0 then ce else                     (*                 x >>> .. *)
    let ce    = PUSH1 (Int bits)   >>> ce in   (*        bits >>> x >>> .. *)                   
    let ce    = PUSH1 (Int 2)      >>> ce in   (*  2 >>> bits >>> x >>> .. *) 
    let ce    = EXP                >>> ce in   (*     2**bits >>> x >>> .. *) 
    let ce    = MUL                >>> ce in   (*       (2**bits)*x >>> .. *) 
    ce

let copy_calldata_to_top le ce (range : Loc.calldata_range) =
    let start= range.calldata_start         in 
    let size = range.calldata_size          in 
    assert (0 < size && size <= 32) ;
    let ce    = PUSH4 (Int start)   >>>ce   in
    let ce    = CALLDATALOAD        >>>ce   in
    let ce    = shiftR_top ce((32-size)*8)  in
    le, ce


type alignment          = L_ 
                        | R_


let align_bool ce align = 
    assert (align = R_) ; 
    ce


let align_addr ce = function 
    | R_        ->  ce
    | L_        ->  shiftL_top ce (12 * 8)

let align_to_L_ (ce:ce) align tyT =
    match align with
    | R_        ->  ce
    | L_        ->  let size   = size_of_ty tyT in
                    assert (size <= 32) ;
                    let shift = (32-size)*8 in
                    shiftL_top ce shift

let copy_to_top le ce align ty (l:Loc.location) =
    let le,ce = Loc.( match l with
    | Stor range      -> push_storRange le ce range
    | CachedStor _    -> err "copy_to_top: CachedStor"
    | Volatile _      -> err "copy_to_top: Volatile"
    | Code _          -> err "copy_to_top: Code"
    | Calldata range  -> copy_calldata_to_top le ce range
    | Stack s         -> copy_nth_stack_elem le ce s )      in
    let ce    = align_to_L_ ce align ty                        in
    (* le needs to remember the alignment *)
    le, ce


let swap_pc_with_0 ce   =
    let ce    = PUSH1 (Int 0)      >>> ce in       (*                             0 >>> .. *)
    let ce    = SLOAD              >>> ce in       (*                          S[0] >>> .. *)
    let ce    = PUSH1 (Int 0)      >>> ce in       (*                    0 >>> S[0] >>> .. *)
    let ce    = DUP1               >>> ce in       (*              0 >>> 0 >>> S[0] >>> .. *)
    let ce    = SSTORE             >>> ce in       (* S'[0]=0                  S[0] >>> .. *)
    ce


(** [restore_pc]   *)  
(*                                                 
 *     BEFORE             AFTER                    
 *                                                   
 *    +--------+                                     
 *    | bkp_pc |                                     
 *  --+--------+--    --+--------+--                
 *)
let restore_pc ce       =                           (*                bkp_pc >>> .. *)
    let ce    = PUSH1 (Int 0)      >>> ce in        (*          0 >>> bkp_pc >>> .. *)
    let ce    = SSTORE             >>> ce in        (* S'[0]=bkp_pc              .. *)             
    ce


(** [throw_if_zero] if the topstack is zero, then throw (goto 0)  *)
let throw_if_zero ce    =                           (*                   i >>> .. *)   
    let ce    = DUP1               >>> ce in        (*             i >>> i >>> .. *)
    let ce    = ISZERO             >>> ce in        (*             b >>> i >>> .. *)
    let ce    = PUSH1 (Int 0)      >>> ce in        (*       0 >>> b >>> i >>> .. *)
    let ce    = JUMPI              >>> ce in        (* {GOTO 0 if b}     i >>> .. *)
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
 *   |   size   |      |    a     |      -->   a    |     0  |       |   a    |     0  |
 * --+----------+--  --+----------+--      +--------+--------+       +--------+--------+
 *                                         | ...    |     0  |       | ...    |     0  |
 *  mem_alloc :=                           +--------+--------+       +--------+--------+
 *      size := pop();                     | a+size |        |     --> a+size |        |
 *      a    := alloc(size);               +--------+--------+       +--------+--------+
 *      push(a)                                BEFORE MEM                AFTER MEM 
 *)

let mem_alloc (ce:ce) =                         (*  STACK                     len <<< .. *)
    let ce    = PUSH1 (Int 64)     >>> ce in    (*                     64 <<< len <<< .. *)
    let ce    = DUP1               >>> ce in    (*              64 <<< 64 <<< len <<< .. *)
    let ce    = MLOAD              >>> ce in    (*           M[64] <<< 64 <<< len <<< .. *)
    let ce    = DUP1               >>> ce in    (* M[64] <<< M[64] <<< 64 <<< len <<< .. *)
    let ce    = SWAP3              >>> ce in    (* len <<< M[64] <<< 64 <<< M[64] <<< .. *)
    let ce    = ADD                >>> ce in    (*     M[64+len] <<< 64 <<< M[64] <<< .. *)
    let ce    = SWAP1              >>> ce in    (*     64 <<< M[64+len] <<< M[64] <<< .. *)
    let ce    = MSTORE             >>> ce in    (*                          M[64] <<< .. *) 
    ce

(* M[64]  :=   the address of mem alloc     *) 
(* MSTORE :=   x=pop() ; y=pop() ; M[x]=y   *) 
(* MLOAD  :=   x=pop() ; push M[x]          *) 

let get_alloc (ce:ce) =
    let ce    = PUSH1 (Int 64)     >>> ce in (* 64    <<< .. *) 
    let ce    = MLOAD              >>> ce in (* M[64] <<< .. *) 
    ce

(** [Tight] just uses [size_of_ty] bytes on the memory.
 *  [ABI] always uses multiples of 32 bytes.
 *  These choices do not affect the alighments
 *)
type memoryPack = TightPack | ABIPack

(** [copy_rntime_code_to_mem ce cntrcts idx]
 * /adds opcodes to [ce] so that in the final state the memory contains the rntime code
 * for all cntrcts that are reachable from [idx] in the
 * list [cntrcts] in the
 * addresses [code_start, code_start + code_size).
 * This adds two elements to the stack, resulting in
 * [..., code_len, code_start) *)
let mstore_rntime_code ce idx =
    let ce    = PUSH32(RntimeCodeSize)         >>> ce in (*                                                  size <<< .. *)
    let ce    = DUP1                           >>> ce in (*                                         size <<< size <<< .. *)  
    let ce    = mem_alloc                          ce in (*                                  alloc(size) <<< size <<< .. *)
    let ce    = DUP2                           >>> ce in (*                         size <<< alloc(size) <<< size <<< .. *)
    let ce    = PUSH32(RntimeCodeOffset idx)   >>> ce in (*                 idx <<< size <<< alloc(size) <<< size <<< .. *)
    let ce    = DUP3                           >>> ce in (* alloc(size) <<< idx <<< size <<< alloc(size) <<< size <<< .. *)
    let ce    = CODECOPY                       >>> ce in (*                                  alloc(size) <<< size <<< .. *)
    ce

let mstore_code ce =                                     (*                                          idx <<< size <<< .. *)
    let ce    = DUP2                           >>> ce in (*                                 size <<< idx <<< size <<< .. *)
    let ce    = mem_alloc                          ce in (*                          alloc(size) <<< idx <<< size <<< .. *)
    let ce    = SWAP1                          >>> ce in (*                          idx <<< alloc(size) <<< size <<< .. *)
    let ce    = DUP3                           >>> ce in (*                 size <<< idx <<< alloc(size) <<< size <<< .. *)
    let ce    = SWAP1                          >>> ce in (*                 idx <<< size <<< alloc(size) <<< size <<< .. *)
    let ce    = DUP3                           >>> ce in (* alloc(size) <<< idx <<< size <<< alloc(size) <<< size <<< .. *) 
    let ce    = CODECOPY                       >>> ce in (*                                  alloc(size) <<< size <<< .. *)  
    ce

(** [copy_whole_code_to_mem] allocates enough memory to accomodate the
 *  whole of the currently running code, and copies it there.
 *  After this, [size, offset] of the memory region is left on the stack.
 *)
let mstore_whole_code ce =
    let ce    = CODESIZE                       >>> ce in (*                                                  size <<< .. *)
    let ce    = DUP1                           >>> ce in (*                                         size <<< size <<< .. *)
    let ce    = mem_alloc                          ce in (*                                  alloc(size) <<< size <<< .. *)
    let ce    = DUP2                           >>> ce in (*                         size <<< alloc(size) <<< size <<< .. *)
    let ce    = PUSH1(Int 0)                   >>> ce in (*                  0  <<< size <<< alloc(size) <<< size <<< .. *)
    let ce    = DUP3                           >>> ce in (* alloc(size) <<<  0  <<< size <<< alloc(size) <<< size <<< .. *)
    let ce    = CODECOPY                       >>> ce in (*                                  alloc(size) <<< size <<< .. *)
    ce

let push_method ce (m:mthd_info)  =
    let hash  = Eth.hash_of_mthd_info_ty m             in
    let b     = Eth.hex_to_big_int hash                in 
    let ce    = PUSH4(Big b)                    >>> ce in 
    ce

(** [prepare_functiohn_signature ce mthd]
 *  Allocates 4 bytes on the memory, and puts the function signature of the arg there.
 *  After that, the stack has (..., signature size, signature offset )
 *)
let prepare_tyMthd mthd ce =
    let ce    = PUSH1(Int 4)                   >>> ce in (*                                                  4 <<< .. *)
    let ce    = DUP1                           >>> ce in (*                                           4  <<< 4 <<< .. *)
    let ce    = mem_alloc                          ce in (*                                     alloc(4) <<< 4 <<< .. *)
    let ce    = push_method ce mthd                   in (*                      method_sig <<< alloc(4) <<< 4 <<< .. *)
    let ce    = DUP2                           >>> ce in (*         alloc(4) <<< method_sig <<< alloc(4) <<< 4 <<< .. *)
                MSTORE                         >>> ce    (* M[alloc(4)] := method_sig           alloc(4) <<< 4 <<< .. *)

(* take a := 256bit , b := 256bit(=32byte) , and return sha3(a++b) *)  
let keccak_cat le ce =                                   (*                    a <<< b <<< .. *) 
    let ce    = PUSH1 (Int 0x00)               >>> ce in (*           0x00 <<< a <<< b <<< .. *)
    let ce    = MSTORE                         >>> ce in (* M[0x00]=a                b <<< .. *)
    let ce    = PUSH1 (Int 0x20)               >>> ce in (*                 0x20 <<< b <<< .. *)
    let ce    = MSTORE                         >>> ce in (* M[0x20]=b                      .. *) 
    let ce    = PUSH1 (Int 0x40)               >>> ce in (*                       0x40 <<< .. *)
    let ce    = PUSH1 (Int 0x00)               >>> ce in (*              0x0  <<< 0x40 <<< .. *)
                SHA3                           >>> ce    (*        sha3(M[0x00..0x3F]) <<< .. *)

let incr_top ce (inc : int) =
    let ce    = PUSH32 (Int inc)               >>> ce in
                ADD                            >>> ce   


















(* CODEGEN *) 


(* sumsize := the size of arguments allocated *)  

(** [add_cnstrctr_arg_to_mem ce arg] realizes [arg] on the memory according to the ABI.  
 *  This increases the stack top (sumsize) by the size of the new allocation. *)
let rec mstore_mthd_arg le pack ce (arg:ty expr) =
    let ty          = snd arg       in 
    assert (fits_in_one_stor_slot ty) ; 
    let i,a   = (match pack with 
                 | ABIPack   -> 32           ,R_ 
                 | TightPack -> size_of_ty ty,L_)      in   (*                                          sumsize >>> .. *)
    let ce    = PUSH1 (Int i)          >>>ce           in   (*                                 size >>> sumsize >>> .. *)
    let ce    = DUP1                   >>>ce           in   (*                       size  >>> size >>> sumsize >>> .. *)
    let ce    = mem_alloc                 ce           in   (*                 alloc(size) >>> size >>> sumsize >>> .. *)
    let ce    = arg                    >>>>>(a,le,ce)  in   (*         arg >>> alloc(size) >>> size >>> sumsize >>> .. *)
    let ce    = SWAP1                  >>>ce           in   (*         alloc(size) >>> arg >>> size >>> sumsize >>> .. *)
    let ce    = MSTORE                 >>>ce           in   (* M[alloc(size)] := arg           size >>> sumsize >>> .. *)
    let ce    = ADD                    >>>ce           in   (*                                     size+sumsize >>> .. *)
    ce

(** [add_cnstrctr_args_to_mem args] realizes [args] on the memory according to the ABI.  
 * This leaves the amount of memory on the stack.
 *  Usually this function is called right after the cnstrctr code is set up in the memory, *)
and mstore_mthd_args pack (args:ty expr list) le ce =
    let ce    = PUSH1(Int 0)            >>>ce           in  (*                  0 >>> ..   *)
    let ce    = foldl(mstore_mthd_arg le pack)ce args   in  (*            sumsize >>> ..   *) 
    ce


(* [new_instance_into_mem] 
 *
 *              ADDRESS              MEMORY 
 *          +---------------------+-----------------+                         
 *          |                  0  | rntime_code     |                                   
 *          |                ...  |  ...            |                                   
 *          |               size  | cnstrctr_code   |
 *          |                ...  |  ...            |
 *          |         size+wsize  | arg1            |                                   
 *          |                     |  ...            |
 *          |                     | arg2            |
 *          |                     |  ...            |
 *          |---------------------|-----------------|
 *          | argsize+size+wsize  |                 |
 *          |                ...  |                 |                         
 *)                                                                 

and new_instance_into_mem le ce n =
    let cn_name   =  n.new_head in
    let cn_idx    =  lookup_cn_idx_of_ce ce cn_name in 
    let ce    = PUSH32(CnstrctrCodeSize cn_idx)     >>>ce   in (*                                                             size >>> .. *) 
    let ce    = PUSH32(RntimeCnstrctrOffset cn_idx) >>>ce   in (*                                                    cie  >>> size >>> .. *)
    let ce    = mstore_code                            ce   in (*                                             alloc(size) >>> size >>> .. *)
    let ce    = SWAP1                               >>>ce   in (*                                             size >>> alloc(size) >>> .. *)
    let ce    = mstore_whole_code                      ce   in (*                  alloc(wsize) >>> wsize >>> size >>> alloc(size) >>> .. *)
    let ce    = mstore_mthd_args ABIPack n.new_args le ce   in (*     argssize >>> alloc(wsize) >>> wsize >>> size >>> alloc(size) >>> .. *)
    let ce    = SWAP1                               >>>ce   in (*     alloc(wsize) >>> argssize >>> wsize >>> size >>> alloc(size) >>> .. *)
    let ce    = POP                                 >>>ce   in (*                      argssize >>> wsize >>> size >>> alloc(size) >>> .. *)
    let ce    = ADD                                 >>>ce   in (*                          argssize+wsize >>> size >>> alloc(size) >>> .. *)
    let ce    = ADD                                 >>>ce   in (*                              argssize+wsize+size >>> alloc(size) >>> .. *)
    let ce    = SWAP1                               >>>ce   in (*                                      alloc(size) >>>   totalsize >>> .. *)
    ce

and codegen_fncall_expr le ce align (fncall:ty fncall) (reT:ty) =
    if      fncall.call_head = "pre_ecdsarecover" then (
                assert (align = R_) ; 
                codegen_ecdsarecover le ce fncall.call_args )   (* XXX: need to pass alignment *)
    else if fncall.call_head = "keccak256" then (
                assert (align = R_) ; 
                codegen_keccak256    le ce fncall.call_args )   (* XXX: need to pass alignment *) 
    else if fncall.call_head = "iszero" then 
                codegen_iszero le ce align fncall.call_args reT
    else        err "codegen_fncall_expr: unknown function head."

and codegen_iszero le ce align args reT = match args with
    | [arg] ->  assert (reT = TyBool) ; 
                let ce =  arg       >>>>> (align,le,ce) in
                          ISZERO    >>>         ce 
    | _     ->  err "codegen_iszero: Wrong number of args"
       
and codegen_keccak256 le ce args =
    let ce    = get_alloc ce                                    in (* stack: [..., offset] *)
    let ce    = mstore_mthd_args TightPack args le ce           in (* stack: [..., offset, size] *)
    let ce    = SWAP1                           >>>ce           in (* stack: [..., size, offset] *)
    let ce    = SHA3                            >>>ce           in
    ce

and codegen_ecdsarecover le ce args = match args with
    | [h; v; r; s] ->
        let start_size = stack_size ce                          in  (* stack: [] *)
        let ce = PUSH1 (Int 32)                 >>>ce           in  (* stack: [out size] *)
        let ce = DUP1                           >>>ce           in  (* stack: [out size, out size] *)
        let ce = mem_alloc                         ce           in  (* stack: [out size, out address] *)
        let ce = DUP2                           >>>ce           in  (* stack: [out size, out address, out size] *)
        let ce = DUP2                           >>>ce           in  (* stack: [out size, out address, out size, out address] *)
        let ce = get_alloc                         ce           in
        let ce = mstore_mthd_args ABIPack args  le ce           in  (* stack: [out size, out address, out size, out address, mem_offset, mem_total_size] *)
        let ce = SWAP1                          >>>ce           in  (* stack: [out size, out address, out size, out address, in size, in offset] *)
        let ce = PUSH1 (Int 0)                  >>>ce           in  (* stack: [out size, out address, out size, out address, in size, in offset, value] *)
        assert (stack_size ce = start_size+7) ;
        let ce = PUSH1 (Int 1)                  >>>ce           in  (* stack: [out size, out address, out size, out address, in size, in offset, value, to] *)
        let ce = PUSH4 (Int 10000)              >>>ce           in  (* stack: [out size, out address, out size, out offset, in size, in offset, value, to, gas] *)
        let ce = CALL                           >>>ce           in  (* stack: [out size, out address, success?] *)
        let ce = throw_if_zero ce                               in
        let ce = POP                            >>>ce           in  (* stack: [out size, out address] *)
        let ce = SWAP1                          >>>ce           in  (* stack: [out address, out size] *)
        let ce = POP                            >>>ce           in  (* we know it's 32 *) (* stack: [out address] *)
        let ce = MLOAD                          >>>ce           in  (* stack: [output] *)
        assert (stack_size ce = start_size+1) ;    
        ce
    | _ -> err "pre_ecdsarecover has a wrong number of args"

and codegen_new_expr le ce n =    
    assert(is_throw_only n.new_msg.msg_reentrance) ;               (* This is NOT a REENTRANCE GUARD which @pirapira instists. <- I do not know why ??? *) 
    let ce    = swap_pc_with_0 ce                               in (* stack : [pc_bkp] *)
    let ce    = new_instance_into_mem le ce n                   in (* stack : [pc_bkp, size, alloc(size)] *)
    let ce    = match n.new_msg.msg_value with                     (* value is the amount sent at the contract creation *) 
       | None     -> PUSH1 (Int 0)              >>>ce              (* no value in the new contract *)
       | Some e   -> e                          >>>>>(R_,le,ce) in (* stack : [pc_bkp, size, alloc(size), v ] *)
    let ce    = CREATE                          >>>ce           in (* stack : [pc_bkp, create_result] *)
    let ce    = throw_if_zero                      ce           in (* stack : [pc_bkp, create_result] *)
    let ce    = SWAP1                           >>>ce           in (* stack : [create_result, pc_bkp] *)
                restore_pc                         ce              (* stack : [create_result] *)

and gen_array_storLoc le ce aa =
    let arr   = aa.array_name                                   in
    let idx   = aa.array_index                                  in
    let ce    = idx                             >>>>>(R_,le,ce) in (*                    index <<< .. *)
    let ce    = arr                             >>>>>(R_,le,ce) in (*      array_loc <<< index <<< .. *)
                keccak_cat le ce                                   (*   sha3(array_loc++index) <<< .. *) 
      
and codegen_array le ce (aa:ty array) =
    let ce    = gen_array_storLoc le ce aa in
                SLOAD >>> ce 

(* if the stack top is zero, 
 *      then set up an array seed at aa, 
 *           replace the zero with the new seed *)
and setup_array_storLoc le ce aa =
    let label = fresh_label ()                                  in       (* stack: [result, result] *)
    let ce    = DUP1                            >>>ce           in       (* stack: [result, result] *)
    let ce    = PUSH4(Label label)              >>>ce           in       (* stack: [result, result, shortcut] *)
    let ce    = JUMPI                           >>>ce           in       (* stack: [result] *)
    let ce    = POP                             >>>ce           in       (* stack: [] *)
    let ce    = gen_array_storLoc le ce aa                      in       (* stack: [stor_index] *)
    let ce    = PUSH1 (Int 1)                   >>>ce           in       (* stack: [stor_index, 1] *)
    let ce    = SLOAD                           >>>ce           in       (* stack: [stor_index, orig_seed] *)
    let ce    = DUP1                            >>>ce           in       (* stack: [stor_index, orig_seed, orig_seed] *)
    let ce    = incr_top ce 1                                   in       (* stack: [stor_index, orig_seed, orig_seed + 1] *)
    let ce    = PUSH1 (Int 1)                   >>>ce           in       (* stack: [stor_index, orig_seed, orig_seed + 1, 1] *)
    let ce    = SSTORE                          >>>ce           in       (* stack: [stor_index, orig_seed] *)
    let ce    = DUP1                            >>>ce           in       (* stack: [stor_index, orig_seed, orig_seed] *)
    let ce    = SWAP2                           >>>ce           in       (* stack: [orig_seed, orig_seed, stor_index] *)
    let ce    = SSTORE                          >>>ce           in       (* stack: [orig_seed] *)
                JUMPDEST label                  >>>ce                    (* stack: [result] *)

(*   if the stack top is zero, 
 *      then  1.  set up an array seed at aa, 
 *            2.  replace the zero with the new seed *)
and setup_array_storLoc_of_loc le ce loc =
    let stor_idx = (match loc with
      | Loc.Stor stor_range     ->  assert (stor_range.Loc.stor_size = (Int 1)) ;
                                    stor_range.Loc.stor_start
      | _                       ->  err "setup array seed at non-storage") in
    let label = fresh_label ()                                  in    (* stack: [result, result] *)
    let ce    = DUP1                            >>>ce           in    (* stack: [result, result] *)
    let ce    = PUSH4(Label label)              >>>ce           in    (* stack: [result, result, shortcut] *)
    let ce    = JUMPI                           >>>ce           in    (* stack: [result] *)
    let ce    = POP                             >>>ce           in    (* stack: [] *)
    let ce    = PUSH32 stor_idx                 >>>ce           in    (* stack: [stor_index] *)
    let ce    = PUSH1(Int 1)                    >>>ce           in    (* stack: [stor_index, 1] *)
    let ce    = SLOAD                           >>>ce           in    (* stack: [stor_index, orig_seed] *)
    let ce    = DUP1                            >>>ce           in    (* stack: [stor_index, orig_seed, orig_seed] *)
    let ce    = incr_top ce 1                                   in    (* stack: [stor_index, orig_seed, orig_seed + 1] *)
    let ce    = PUSH1(Int 1)                    >>>ce           in    (* stack: [stor_index, orig_seed, orig_seed + 1, 1] *)
    let ce    = SSTORE                          >>>ce           in    (* stack: [stor_index, orig_seed] *)
    let ce    = DUP1                            >>>ce           in    (* stack: [stor_index, orig_seed, orig_seed] *)
    let ce    = SWAP2                           >>>ce           in    (* stack: [orig_seed, orig_seed, stor_index] *)
    let ce    = SSTORE                          >>>ce           in    (* stack: [orig_seed] *)
    let ce    = JUMPDEST label                  >>>ce           in    (* stack: [result] *)
    ce






(* le is not updated here.  It can be only updated in
 * a variable initialization *)

and codegen_expr le ce align  ((e,t):ty expr) : ce =
    let ret = (match e,t with
    | EpAddr((c,TyInstnce _)as e),TyAddr->  e                       >>>>>(align,le,ce) 

    | EpAddr _, _           ->  errc"EpAddr"

    | EpValue,TyUint256     ->              CALLVALUE               >>>ce      (* Value (wei) Transferred to the account *) 
    | EpValue,_             ->  errc"EpValue"

    | EpSender,TyAddr       ->  let ce  =   CALLER                  >>>ce      in
                                            align_addr ce align     
    | EpSender,_            ->  errc"EpSender"

    | EpArray a,ty          ->  assert (align = R_) ; 
                                let ce  =   codegen_array le ce (read_array a) in
                                begin match ty with
                                | TyMap _-> setup_array_storLoc le ce (read_array a)
                                | _      -> ce                   end 
    | EpThis,_              ->  let ce  =   ADDRESS                 >>>ce   in
                                            align_addr ce align     

    | EpIdent id,ty         ->  (match lookup le id with
                                (** if things are just DUP'ed, location env should not be updated.  
                                 *  If they are SLOADED,   the location env should be updated. *)
                                | Some loc  ->  let le,ce = copy_to_top le ce align ty loc in
                                                begin match ty with
                                                | TyMap _   -> setup_array_storLoc_of_loc le ce loc
                                                | _         -> ce           end
                                | None      ->  err ("codegen_expr: identifier's location not found: "^id) )

    | EpFalse,TyBool        ->  assert (align = R_) ;
                                            PUSH1(Big zero_big_int) >>>ce  
    | EpFalse, _            ->  errc"EpFalse"

    | EpTrue,TyBool         ->  assert (align = R_) ; 
                                            PUSH1(Big unit_big_int) >>>ce  
                                
    | EpTrue, _             ->  errc"EpTrue"

    | EpDecLit256 d,TyUint256-> assert (align = R_) ; 
                                            PUSH32(Big d)  >>> ce  
    | EpDecLit256 d, _      ->  errc("EpDecLit256 "^(string_of_big_int d))

    | EpDecLit8 d, TyUint8  ->  assert (align = R_) ; 
                                            PUSH1(Big d)   >>> ce  
                                  
    | EpDecLit8 d, _        ->  errc("EpDecLit8 "^(string_of_big_int d))

    | EpLand(l,r),TyBool    ->  assert (align=R_) ; 
                                let la  =   fresh_label () in
                                let ce  =   l              >>>>>(R_,le,ce)    in (* stack: [..., l] *)
                                let ce  =   DUP1           >>>ce              in (* stack: [..., l, l] *)
                                let ce  =   ISZERO         >>>ce              in (* stack: [..., l, !l] *)
                                let ce  =   PUSH4(Label la)>>>ce              in (* stack: [..., l, !l, shortcut] *)
                                let ce  =   JUMPI          >>>ce              in (* stack: [..., l] *)
                                let ce  =   POP            >>>ce              in (* stack: [...] *)
                                let ce  =   r              >>>>>(R_,le,ce)    in (* stack: [..., r] *)
                                let ce  =   JUMPDEST la    >>>ce              in
                                let ce  =   ISZERO         >>>ce              in
                                let ce  =   ISZERO         >>>ce              in
                                ce
    | EpLand (_, _), _      ->  errc"EpLand"

    | EpNot expr, TyBool    ->  let ce  =   expr           >>>>>(align,le,ce) in
                                let ce  =   ISZERO         >>>ce              in 
                                align_bool ce align   
    | EpNot sub, _          ->  errc"EpNot"

    | EpNow,TyUint256       ->           TIMESTAMP      >>>ce 
    | EpNow,_               ->  errc"EpNow"

    | EpNeq(l,r),TyBool     ->  let ce  =   r              >>>>>(R_,le,ce)    in
                                let ce  =   l              >>>>>(R_,le,ce)    in
                                let ce  =   EQ             >>>ce              in
                                let ce  =   ISZERO         >>>ce              in
                                let ce  =   align_bool ce align            in
                                ce
    | EpNeq _, _            ->  errc"EpNeq"

    | EpLt(l,r),TyBool      ->  let ce  =   r              >>>>>(R_,le,ce)    in
                                let ce  =   l              >>>>>(R_,le,ce)    in
                                let ce  =   LT             >>>ce              in
                                let ce  =   align_bool ce align            in
                                ce
    | EpLt _, _             ->  errc"EpLt"

    | EpPlus(l,r),TyUint256 ->  let ce  =   r              >>>>>(R_,le,ce)    in
                                let ce  =   l              >>>>>(R_,le,ce)    in
                                            ADD            >>>ce 
    | EpPlus(l,r),TyUint8   ->  let ce  =   r              >>>>>(R_,le,ce)    in
                                let ce  =   l              >>>>>(R_,le,ce)    in
                                            ADD            >>>ce 
    | EpPlus(l,r),_         ->  errc"EpPlus"

    | EpMinus(l,r),TyUint256->  let ce  =   r              >>>>>(R_,le,ce)    in
                                let ce  =   l              >>>>>(R_,le,ce)    in
                                            SUB            >>>ce 
    | EpMinus(l,r),TyUint8  ->  let ce  =   r              >>>>>(R_,le,ce)    in
                                let ce  =   l              >>>>>(R_,le,ce)    in
                                            SUB            >>>ce 
    | EpMinus(l,r),_        ->  errc"EpMinus"

    | EpMult(l,r),TyUint256 ->
                                let ce  =   r              >>>>>(R_,le,ce)    in
                                let ce  =   l              >>>>>(R_,le,ce)    in
                                            MUL            >>>ce
    | EpMult(l,r),TyUint8   ->
                                let ce  =   r              >>>>>(R_,le,ce)    in
                                let ce  =   l              >>>>>(R_,le,ce)    in
                                            MUL            >>>ce
    | EpMult (l, r), _      ->  errc"EpMult"

    | EpGt(l,r),TyBool      ->  let ce  =   r              >>>>>(R_,le,ce)    in
                                let ce  =   l              >>>>>(R_,le,ce)    in
                                let ce  =   GT             >>>ce              in
                                            align_bool ce align   
                                
    | EpGt _, _             ->  errc"EpGt"

    | EpBalance e,TyUint256 ->
                                let ce  =   e              >>>>>(R_,le,ce)    in
                                            BALANCE        >>>ce
    | EpBalance inner, _    ->  errc"EpBalance"

    | EpEq(l,r),TyBool      ->  let ce  =   r              >>>>>(R_,le,ce)    in
                                let ce  =   l              >>>>>(R_,le,ce)    in
                                let ce  =   EQ             >>>ce              in
                                            align_bool ce align      
    | EpEq _, _             ->  errc"EpEq"

    | EpSend s, _           ->  assert (align = R_) ; 
                                            codegen_send_expr le ce s

    | EpNew n,TyInstnce c   ->  assert (align = R_) ; 
                                            codegen_new_expr le ce n 
    | EpNew n, _            ->  errc"EpNew"

    | EpFnCall fncall,reT   ->  codegen_fncall_expr le ce align fncall reT

    | EpParen _, _          ->  errc"EpParen"

    | EpSingleDeref(ref,tyRef),ty ->
                                let size = size_of_ty ty in
                                assert (size <= 32)         ;   (* assuming word-size *)
                                assert (tyRef=TyRef[ty])    ;
                                assert (align=R_)           ; 
                                let ce  =   (ref,tyRef)    >>>>>(R_,le,ce) in (* pushes the pointer *)
                                            MLOAD          >>>ce 

    | EpTupleDeref _,_      ->  err"codegen EpTupleDeref should not happen."

    ) in

    assert (stack_size ret = stack_size ce + 1) ; 
    ret

and (>>>>>) expr (align,le,ce)  = codegen_expr le ce align expr 
and errc str                    = err("codegen_expr: " ^ str ^ " of unexpected type")




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
    let ce = foldl(prepare_arg le)ce args in
    assert(stack_size ce = start_size+1) ; 
    ce

(** [prepare_input_in_mem] prepares the input for CALL opcode in the memory.
 *  That leaves "..., in size, in offset" (top) on the stack.
 *)
and prepare_input_in_mem le ce s mthd =
    let start_size  = stack_size ce   in
    let args        = s.send_args     in
    let ce = prepare_tyMthd mthd ce in   (* stack : [sig size, &(tyMthd) ] *)
    let ce = prepare_args args  le ce in   (* stack : [sigsize, &(tyMthd), argsize] *)
    let ce = SWAP1              >>>ce in   (* stack : [sigsize, argsize, &(tyMthd)] *)
    let ce = SWAP2              >>>ce in   (* stack : [&(tyMthd), argsize, sigsize] *)
    let ce = ADD                >>>ce in   (* stack : [&(tyMthd), total size] *)
    let ce = SWAP1              >>>ce in   (* stack : [total size, &(tyMthd)] *)
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
    | TyInstnce c_name -> 
        let callee_idx          = lookup_cn_idx_of_ce ce c_name                  in 
        let callee              = cntrct_lookup ce callee_idx           in
        begin match s.send_mthd with
        | None              -> err "Unknown Method Name"
        | Some mthd_name    -> (
        let m : mthd_info = lookup_mthd_info ce callee mthd_name in
        assert(is_throw_only s.send_msg.msg_reentrance) ; 
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
        assert (is_throw_only s.send_msg.msg_reentrance) ; 
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
    let ce = match s.send_msg.msg_value with 
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
    let ce = PUSH32 (RntimeCntrctOffset idx)   >>> ce in
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
let sstore_code_from_mem ce =
    let start_size  = stack_size ce         in (* TODO: check that size is a multiple of 32 *)
    let label       = fresh_label()         in
    let exit        = fresh_label()         in
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
let sstore_args_from_mem le ce idx =
    let ce = PUSH32(InitDataSize idx)           >>>ce   in (*                                    datasize <<< mem_start <<< size <<< .. *)
    let ce = CODESIZE                           >>>ce   in (*                       codesize <<< datasize <<< mem_start <<< size <<< .. *) 
    let ce = EQ                                 >>>ce   in (* (eq := if codesize==datasize then 1 else 0) <<< mem_start <<< size <<< .. *) 
    let ce = ISZERO                             >>>ce   in (*                                      not eq <<< mem_start <<< size <<< .. *) 
    let ce = PUSH1 (Int 2)                      >>>ce   in (*                                2 <<< not eq <<< mem_start <<< size <<< .. *)
    (* IF noteq THEN GOTO 2     *) 
    let ce = JUMPI                              >>>ce   in (*                                                 mem_start <<< size <<< .. *) 
    (* ELSE                     *) 
    let ce = PUSH32(StorCnstrctrArgsBegin idx)  >>>ce   in (*                                         idx <<< mem_start <<< size <<< .. *)
    sstore_code_from_mem ce


(** [copy_args_from_code_to_mem]
 *  copies cnstrctr args at the end of the bytecode into the memory.  
 *  The number of bytes is deidxed using the cntrct interface.
 *  The memory usage counter at byte [0x40] is increased accordingly.
 *  After this, the stack contains the size and the beginning of the memory piece that contains the args.
 *  Output [rest of the stack, mem_size, mem_begin].
 *)
let mstore_args le ce (cn:ty cntrct) =
    let args = L.map snd (Eth.cnstrctr_args cn) in 
    let total_size = Eth.total_size_of_tyArgs args in
    let start_size = stack_size ce        in (* [] *)
    let ce = PUSH32(Int total_size) >>>ce in (* [totalsize] *)
    let ce = DUP1                   >>>ce in (* [totalsize, totalsize] *)
    let ce = mem_alloc                 ce in (* [totalsize, alloc] *)
    let ce = DUP2                   >>>ce in (* [totalsize, alloc, totalsize] *)
    let ce = DUP1                   >>>ce in (* [totalsize, alloc, totalsize, totalsize] *)
    let ce = CODESIZE               >>>ce in (* [totalsize, alloc, totalsize, totalsize, codesize] *)
    let ce = SUB                    >>>ce in (* [totalsize, alloc, totalsize, codebegin] *)
    let ce = DUP3                   >>>ce in (* [totalsize, alloc, totalsize, codebegin, alloc] *)
    let ce = CODECOPY               >>>ce in (* [totalsize, alloc] *)
    assert (start_size + 2 = stack_size ce) ; 
    ce  

    (* CODECOPY  to from len :=  M[to .. to+len-1] = I[from .. from+len-1]  *)






(* S[1] := ARRAY SEED COUNTER *) 
(*
 * S[loc] := old seed 
 * S[1]   := S[1] + 1       *)
let setup_seed (le,ce) (array_loc:SL.stor_addr) =
    let label       = fresh_label()   in
    let start_size  = stack_size ce     in
    let ce = PUSH4 (Int array_loc)  >>>ce in (*                           loc <<< .. *)
    let ce = PUSH4 (Label label)    >>>ce in (*                 label <<< loc <<< .. *)
    let ce = JUMPI                  >>>ce in (* IF loc != 0 GOTO label *) 
    let ce = PUSH1 (Int 1)          >>>ce in (*                            1  <<< .. *)
    let ce = SLOAD                  >>>ce in (*                          S[1] <<< .. *)
    let ce = DUP1                   >>>ce in (*                 S[1] <<< S[1] <<< .. *)
    let ce = PUSH4 (Int array_loc)  >>>ce in (*        loc  <<< S[1] <<< S[1] <<< .. *)
    let ce = SSTORE                 >>>ce in (* S[loc]:=S[1]             S[1] <<< .. *)
    let ce = incr_top ce 1                in (*                        S[1]+1 <<< .. *)
    let ce = PUSH1 (Int 1)          >>>ce in (*                 1  <<< S[1]+1 <<< .. *)
    let ce = SSTORE                 >>>ce in (* S[1]:=S[1]+1                      .. *)
    let ce = JUMPDEST label         >>>ce in (*                                   .. *)
    assert (stack_size ce = start_size) ; 
    le, ce


(* let setup_array_seed_counter_to_one_if_not_initialized ce = *)
let reset_array_seed_counter ce = 
    let start_size  = stack_size ce     in
    let label       = fresh_label()   in
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
    let arrLocs       = SL.array_locations cn in
    let _,ce          = foldl setup_seed (le,ce) arrLocs in
    ce




(* CODEGEN CONSTRUCTOR *) 

type cnstrctr_compiled      =
                            { cnstrctr_ce           : ce
                            ; cnstrctr_ty           : tyCntrct
                            ; cnstrctr_cn           : ty cntrct         }

type rntime_compiled        =                                              (* what form should the cnstrctr code be encoded? *)
                            { rntime_ce             : ce                   (* 1. pseudo program.      easy                   *)
                            ; rntime_cn_offsets     : int idx_list      }  (* 2. pseudo codegen_env.  maybe uniform          *)
                            
let codegen_cnstrctr_bytecode (cns:ty cntrct idx_list) idx : ce (* containing the program *)  =
    let cn      = lookup_index idx cns                          in 
    let le      = cnstrctr_initial_env idx cn                   in
    let ce      = empty_ce (lookup_cn_idx_of_cns cns) cns       in
    let ce      = init_mem_alloc                         ce     in
    let ce      = mstore_args          le ce cn                 in (* stack: [arg_mem_size, arg_mem_begin] *)
    let ce      = sstore_args_from_mem le ce idx                in (* stack: [] *)
    let ce      = setup_array_seeds    le ce cn                 in
    let ce      = set_cntrct_pc           ce idx                in (* stack: [] *)
    let ce      = mstore_rntime_code      ce idx                in (* stack: [code_len, code_start_on_memory] *)
                  RETURN >>> ce 

let compile_cnstrctr cns idx  : cnstrctr_compiled =
    let cn      = L.assoc idx cns in 
    { cnstrctr_ce           = codegen_cnstrctr_bytecode cns idx
    ; cnstrctr_ty           = typeof_cntrct cn 
    ; cnstrctr_cn           = cn                                }

let compile_cnstrctrs cns : cnstrctr_compiled idx_list =
    idxmap (compile_cnstrctr cns) cns

let empty_rntime_compiled lookup_cn_idx layouts =
    { rntime_ce             = empty_ce lookup_cn_idx layouts
    ; rntime_cn_offsets = []                                    }


let initial_rntime_compiled lookup_cn_idx layouts : rntime_compiled =
    let ce  =   empty_ce lookup_cn_idx layouts in
    let ce  =   get_cntrct_pc                  ce  in
    let ce  =   JUMP                        >>>ce  in
    { rntime_ce                 = ce
    ; rntime_cn_offsets     = [] }




(* DISPATCHER *) 

let push_mthd_label ce idx m_hd = 
                PUSH32(RntimeMthdLabel(idx,m_hd)) >>>ce 

let dispatcher_usualMthd idx le ce m =                  (*                                         ABCD <<< .. *)  
    let ce  =   DUP1                       >>>ce    in  (*                                ABCD <<< ABCD <<< .. *)
    let ce  =   push_method ce m                    in  (*                          m <<< ABCD <<< ABCD <<< .. *)
    let ce  =   EQ                         >>>ce    in  (*                          m=ABCD?1:0 <<< ABCD <<< .. *)
    let ce  =   push_mthd_label ce idx(Method m)    in  (*            Rntime(m) <<< m=ABCD?1:0 <<< ABCD <<< .. *)
    let ce  =   JUMPI                      >>>ce    in  (* if m=ABCD then GOTO Rntime(m)           ABCD <<< .. *)
    ce

let dispatcher_defaultMthd idx le ce =
    let ce  =   push_mthd_label ce idx Default      in
    let ce  =   JUMP                       >>>ce    in
    ce

let push_inputdata32_from b ce =
    let ce  =   PUSH32 b                   >>>ce    in
    let ce  =   CALLDATALOAD               >>>ce    in
    ce

let add_throw ce =        (* Just  using the same method as solc. *)
    let ce  =   PUSH1 (Int 2)              >>>ce    in
    let ce  =   JUMP                       >>>ce    in
    ce

let dispatcher le ce idx cntrct =
    let tyMthds         = L.map         (fun x ->   x.mthd_head  ) cntrct.mthds in
    let uMthds          = BL.filter_map (function | Default  -> None 
                                                  | Method m -> Some m) tyMthds in 
    let default_exists  = L.exists      (function | Default  -> true
                                                  | _        -> false ) tyMthds in 
    let ce    =   push_inputdata32_from (Int 0) ce              in (*               ABCDxxxxxxxxxxxxxxxxxxxxxxxxxxxxx <<< .. *)
    let ce    =   shiftR_top ce Eth.(word_bits-sig_bits)        in (*                                            ABCD <<< .. *)                             
    let ce    =   foldl(dispatcher_usualMthd idx le)ce uMthds   in 
    let ce    =   POP                        >>>ce              in (*                                                     .. *)
    let ce    =   if  default_exists
                      then dispatcher_defaultMthd idx le ce 
                      else add_throw ce                         in 
    le,ce

let add_mthd_label idx (m:mthd_head) ce =
    let label =   fresh_label()                         in
    let ce    =   JUMPDEST label             >>>ce      in
    register_entrypoint(Mthd(idx,m))label ; 
    ce




(***************************************)
(*           SSTORE ARGS               *)
(***************************************)

let push_args le ce args =
    let ce = L.fold_right (fun arg ce->arg >>>>>(R_,le,ce)) args ce in 
    le,ce

let sstore_word_to (le,ce) stor_loc =
    let ce = PUSH32 (Int stor_loc)      >>>ce   in
    let ce = SSTORE                     >>>ce   in
    le,ce

let sstore_words_to le ce stor_locs = foldl sstore_word_to (le,ce) stor_locs

let sstore_args le ce offset idx args = 
    let cntrct  =   try cntrct_lookup ce idx
                    with e ->(eprintf "set_cntrct_args: looking up %d\n" idx; raise e) in 
    let arglocs = SL.arg_locations offset cntrct  in
    assert (L.length arglocs = L.length args) ; 
    let le,ce   =   push_args       le ce args      in
    let le,ce   =   sstore_words_to le ce arglocs   in
    le,ce



let set_cont_to_fncall le ce layout (fncall, ty_expr) =
    let hd      =   fncall.call_head in
    let args    =   fncall.call_args in
    let idx     =   lookup_cn_idx_of_ce ce hd in 
    let ce      =   set_cntrct_pc ce idx in
    let offset  =   layout.SL.stor_cnstrctr_args_begin idx in
    let le,ce   =   try sstore_args le ce offset idx args
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



(*************************************)
(*      MSTORE Exprs                 *)
(*************************************)


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
    let ce      = PUSH1(Int 32)          >>> ce  in  (* ..., val, 32 *)
    let ce      = DUP1                   >>> ce  in  (* ..., val, 32, 32 *)
    let ce      = mem_alloc                  ce  in  (* ..., val, 32, alloc(32) *)
    let ce      = SWAP2                  >>> ce  in  (* ..., alloc(32), 32, val *)
    let ce      = DUP3                   >>> ce  in  (* ..., alloc(32), 32, val, alloc(32) *)
    let ce      = MSTORE                 >>> ce  in  (* ..., alloc(32), 32 *)
    let ce      = SWAP1                  >>> ce  in  (* ..., 32, alloc(32) *)
    ce

let mstore_expr le ce pack ((e,ty):ty expr) =
    let start_size  = stack_size ce                 in
    let align   = match pack with | ABIPack   -> R_
                                  | TightPack -> L_ in
    let ce = (e,ty)             >>>>>(align,le,ce)  in     (*             value <<< .. *)
    let ce = mstore_word ty le ce                   in     (*  alloc(32) <<< 32 <<< .. *)
    assert (stack_size ce = 2+start_size) ; 
    le,ce

let rec mstore_exprs le ce pack = function 
    | []        ->  let ce    = PUSH1(Int 0)                >>> ce  in
                    let ce    = PUSH1(Int 0)                >>> ce  in
                    le, ce
    | e::es     ->  let le,ce = mstore_expr le ce pack e            in (* stack : [size, offset] *)
                    let ce    = SWAP1                       >>> ce  in (* stack : [offset, size] *)
                    let le,ce = mstore_exprs le ce pack es          in (* stack : [offset, size, size', offset] *)
                    let ce    = POP                         >>> ce  in (* stack : [offset, size, size'] *)
                    let ce    = ADD                         >>> ce  in (* stack : [offset, size_sum] *)
                    let ce    = SWAP1                       >>> ce  in (* stack : [size_sum, offset] *)
                    le, ce



                    
let add_return le ce layout ret =
    let start_size = stack_size ce          in
    let le,ce   = set_cont le ce layout ret.ret_cont                  in
    let ce      = match ret.ret_expr with
    | Some e    ->  
    let le,ce   = mstore_expr le ce ABIPack e         in
                  RETURN                      >>>ce 
    | None      ->STOP                        >>>ce   in
    assert (stack_size ce = start_size) ; 
    le,ce

let sstore_to_array le ce layout (aa : ty array) =
    let array   = aa.array_name       in
    let index   = aa.array_index       in
    let ce      = index          >>>>>(R_,le,ce) in   (* stack : [value, index] *)
    let ce      = array          >>>>>(R_,le,ce) in   (* stack : [value, index, array_seed] *)
    let ce      = keccak_cons    le ce           in   (* stack : [value, kec(array_seed ^ index)] *)
    let ce      = SSTORE         >>>ce           in
    ce

let sstore_to_lexpr le ce layout = function | LEpArray aa -> 
    let start_size = stack_size ce in
    let ce = sstore_to_array le ce layout aa in
    assert (start_size = stack_size ce + 1) ; 
    ce

let add_assign le ce layout l r =
    let start_size = stack_size ce in
    let ce      = r              >>>>>(R_,le,ce) in
    assert (1+start_size = stack_size ce) ; 
    let ce      = sstore_to_lexpr le ce layout l in
    assert (start_size = stack_size ce) ; 
    le, ce

let push_event ce event =
    let hash    = Eth.event_sig_hash event in
    let b       = Eth.hex_to_big_int hash  in 
                  PUSH4 (Big b)         >>>ce           


let add_varDecl le ce layout i =
    let pos     = stack_size ce                         in
    let name    = i.varDecl_id                          in
    let ce      = i.varDecl_val         >>>>>(R_,le,ce) in
    let loc     = Loc.Stack (pos + 1)                   in
    let le      = add_pair le(name,loc)                 in
    le, ce






let rec add_if_single le ce layout cond body =
    let label       = fresh_label ()              in
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
    let label       = fresh_label()     in
    let next        = fresh_label()     in
    let start_size  = stack_size ce     in
    let ce      = codegen_expr le ce R_ cond       in
    let ce      = ISZERO                    >>>ce  in
    let ce      = PUSH4(Label label)        >>>ce  in
    let ce      = JUMPI                     >>>ce  in
    let _,ce    = add_stmts le ce layout bodyT     in (* location env needs to be discarded *)
    let ce      = PUSH4(Label next)         >>>ce  in
    let ce      = JUMP                      >>>ce  in
    let ce      = JUMPDEST label            >>>ce  in
    let _,ce    = add_stmts le ce layout bodyF     in (* location env needs to be discarded *)
    let ce      = JUMPDEST next             >>>ce  in
    assert (start_size = stack_size ce) ; 
    le,ce

and add_stmts le ce layout ss =
    foldl (add_stmt layout) (le,ce) ss

and add_stmt layout (le,ce) = function 
    | SmAbort                       ->  le, add_throw ce
    | SmReturn ret                  ->  add_return        le ce layout ret
    | SmAssign (l,r)                ->  add_assign        le ce layout l r
    | SmVarDecl i                   ->  add_varDecl       le ce layout i
    | SmIfThen(cond,body)           ->  add_if_single     le ce layout cond body 
    | SmIfThenElse(cond,bT,bF)      ->  add_if            le ce layout cond bT bF
    | SmSelfDestruct expr           ->  add_self_destruct le ce layout expr
    | SmExpr expr                   ->  add_expr_stmt     le ce layout expr
    | SmLog(name,args,Some ev)      ->  add_log_stmt      le ce layout name args ev
    | SmLog(name,args,None)         ->  err "add_stmt: type check first"

and add_log_stmt le ce layout name args event =
    let idxArgs,args= split_event_args event args       in
    let le,ce   = push_args le ce idxArgs               in
    let ce      = push_event   ce event                 in
    let le,ce   = mstore_exprs le ce ABIPack args       in (* stack : [..., size, offset] *)
    let n       = L.length idxArgs + 1                  in
    let ce      = log n                 >>>ce           in  (* deindexee N in logN *)
    le, ce

and add_expr_stmt le ce layout expr =
    let ce      = expr                  >>>>>(R_,le,ce) in
    let ce      = POP                   >>>ce           in
    le, ce

and add_self_destruct le ce layout expr =
    let ce      = expr                  >>>>>(R_,le,ce) in
    let ce      = SELFDESTRUCT          >>>ce           in
    le, ce











let calldatasize m =
    let args    = m.mthd_args in
    4 (* for signature *) + Eth.total_size_of_args args   

let add_mthd_argLen_chk m ce = match m with  
    | Default       -> ce
    | Method  m     ->
    let ce      = PUSH4(Int(calldatasize m))    >>>ce   in
    let ce      = CALLDATASIZE                  >>>ce   in
    let ce      = EQ                            >>>ce   in
    let ce      = ISZERO                        >>>ce   in
    let ce      = PUSH1(Int 0)                  >>>ce   in
    let ce      = JUMPI                         >>>ce   in
    ce

let add_mthd layout idx (le,ce) m =
    let ce      = add_mthd_label idx   m.mthd_head ce   in
    let ce      = add_mthd_argLen_chk  m.mthd_head ce   in
    let le      = add_empty_locEnv                 le   in
    let le      = add_mthd_argLocs     m           le   in
    let le,ce   = add_stmts le ce layout m.mthd_body    in
    le,ce

let add_mthds layout idx  = ($$$) foldl add_mthd layout idx


let codegen_append_cntrct_bytecode le ce layout((idx,cntrct):idx*ty cntrct) =
    let label = fresh_label ()                          in      
    let ce    = JUMPDEST label              >>>ce       in     (* jump dest for contract *) 
    register_entrypoint (Cntrct idx) label ;                   (* update the entrypoint database with (id,pc) pair *) 
    let ce    = init_mem_alloc                 ce       in
    let le,ce = dispatcher le ce idx cntrct             in
    let le,ce = add_mthds layout idx(le,ce)cntrct.mthds in  
    ce

let append_rntime layout (prev:rntime_compiled) (idx,cn) : rntime_compiled =
    { rntime_ce         = codegen_append_cntrct_bytecode (rntime_initial_env cn) prev.rntime_ce layout(idx,cn)
    ; rntime_cn_offsets = insert idx (code_len prev.rntime_ce) prev.rntime_cn_offsets}

let compile_rntime layout (cns:ty cntrct idx_list) : rntime_compiled = 
    foldl (append_rntime layout) (initial_rntime_compiled (lookup_cn_idx_of_cns cns) cns) cns

let stor_layout_from_cnstrctr_compiled (cc : cnstrctr_compiled) : SL.cntrct_stor_layout =
    SL.stor_layout_of_cntrct cc.cnstrctr_cn (extract_program cc.cnstrctr_ce)

let sizes_of_cnstrctrs (cnstrctrs : cnstrctr_compiled idx_list) : int list =
    let lengths = map    (fun cc -> code_len cc.cnstrctr_ce) cnstrctrs in
    let lengths = L.sort (fun a b-> compare(fst a)(fst b)) lengths in
    L.map snd lengths

let rec calculate_offsets_inner ret current = function
    | []        -> L.rev ret
    | hd::tl    -> calculate_offsets_inner (current::ret) (current + hd) tl

let calculate_offsets init l   = calculate_offsets_inner [] init l

let stor_layout_from_rntime_compiled (rc:rntime_compiled) (cnstrctrs:cnstrctr_compiled idx_list) : SL.rntime_stor_layout =
    let sizes_of_cnstrctrs       = sizes_of_cnstrctrs cnstrctrs in
    let offsets_of_cnstrctrs     = calculate_offsets (code_len rc.rntime_ce) sizes_of_cnstrctrs in
    let sum_of_cnstrctr_sizes    = BL.sum sizes_of_cnstrctrs in
    SL.(
        { rntime_code_size             = sum_of_cnstrctr_sizes + code_len rc.rntime_ce
        ; rntime_offset_of_idx         = rc.rntime_cn_offsets
        ; rntime_size_of_cnstrctr      = to_idx_list sizes_of_cnstrctrs
        ; rntime_offset_of_cnstrctr    = to_idx_list offsets_of_cnstrctrs })

let concat_programs_rev (programs : 'imm Evm.program list) =
    let rev_programs = L.rev programs in
    L.concat rev_programs

(** cnstrctrs_packed concatenates cnstrctr code.
 *  Since the code is stored in the reverse order, the concatenation is also reversed. *)
let cnstrctrs_packed layout (cnstrctrs : cnstrctr_compiled idx_list) =
    let programs            = map (fun cc -> extract_program cc.cnstrctr_ce) cnstrctrs in
    let programs            = L.sort (fun a b -> compare (fst a) (fst b)) programs in
    let programs            = L.map snd programs in
    concat_programs_rev programs

let compose_bytecode (cnstrctrs : cnstrctr_compiled idx_list)
                     (rntime : rntime_compiled) idx : big_int Evm.program =
    let cntrcts_stor_layout : (idx * SL.cntrct_stor_layout) list =
      L.map (fun (id, const) -> (id, stor_layout_from_cnstrctr_compiled const)) cnstrctrs in
    let rntime_layout       = stor_layout_from_rntime_compiled rntime cnstrctrs in
    let layout              = SL.cnstrct_post_stor_layout cntrcts_stor_layout rntime_layout in
    let pseudo_cnstrctr     = lookup_index idx cnstrctrs in
    let imm_cnstrctr        = SL.realize_program layout idx (extract_program pseudo_cnstrctr.cnstrctr_ce) in
    let pseudo_rntime_core  = extract_program rntime.rntime_ce in
    (* Sicne the code is stored in the reverse order, the concatenation is also reversed. *)
    let imm_rntime          = SL.realize_program layout idx ((cnstrctrs_packed layout cnstrctrs)@pseudo_rntime_core) in
    (* the code is stored in the reverse order *)
    imm_rntime @ imm_cnstrctr

let compose_rntime_bytecode (cnstrctrs : cnstrctr_compiled idx_list)
                     (rntime : rntime_compiled) : big_int Evm.program =
    let cntrcts_stor_layout : (idx * SL.cntrct_stor_layout) list =
      L.map (fun (id, const) -> (id, stor_layout_from_cnstrctr_compiled const)) cnstrctrs in
    let rntime_layout  = stor_layout_from_rntime_compiled rntime cnstrctrs in
    let layout          = SL.cnstrct_post_stor_layout cntrcts_stor_layout rntime_layout in
    (* TODO: 0 in the next line is a bit ugly. *)
    let imm_rntime     = SL.realize_program layout 0 ((cnstrctrs_packed layout cnstrctrs)@(extract_program rntime.rntime_ce)) in
    imm_rntime
