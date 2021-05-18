open Label
open Big_int
open Printf 

open Imm
open CodegenEnv
open LocationEnv
open Evm
open Syntax
open ContractId

module Loc  = Location 
module Eth  = Ethereum 
module BL   = BatList
module L    = List
module LI   = LayoutInfo

let err = failwith 



let copy_stor_range_to_top le ce (range : imm Loc.stor_range) =
    assert (is_constant_int 1 range.Loc.stor_size) ; 
    let offset:imm  = range.Loc.stor_start in
    let ce = PUSH32 offset     >>> ce in
    let ce = SLOAD             >>> ce in
    le, ce

let copy_nth_stack_elem le ce (n:int) =
    let start_size    = stack_size ce  in
    let diff          = start_size - n in
    assert (diff >= 0) ; 
    let ce = dup_suc_n diff     >>> ce in
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
    let ce  = PUSH4 (Int range.Loc.calldata_offset) >>> ce  in
    let ce  = CALLDATALOAD      >>> ce in
    let ce  = shiftR_top ce ((32 - range.Loc.calldata_size) * 8) in
    le, ce


type alignment          = L_ 
                        | R_


let align_boolean ce alignment = 
    assert (alignment = R_) ; 
    ce


let align_address ce    = function 
    | R_        ->  ce
    | L_        ->  shiftL_top ce (12 * 8)

let align_from_right_aligned (ce:ce) alignment tyT =
    match alignment with
    | R_        ->  ce
    | L_        ->  let size   = size_of_ty tyT in
                    assert (size <= 32) ;
                    if size=32 then ce else (* no alignment *) 
                        let shift = (32 - size) * 8 in
                        let ce = PUSH1 (Int shift)  >>> ce in (* stack: [shift] *)
                        let ce = PUSH1 (Int 2)      >>> ce in (* stack: [shift, 2] *)
                        let ce = EXP                >>> ce in (* stack: [2 ** shift] *)
                        let ce = MUL                >>> ce in
                        ce

let copy_to_top le ce alignment ty (l:Loc.location) =
    let le,ce = Loc.( match l with
      | Storage range   -> copy_stor_range_to_top le ce range
      | CachedStorage _ -> err "copy_to_top: CachedStorage"
      | Volatile _      -> err "copy_to_top: Volatile"
      | Code _          -> err "copy_to_top: Code"
      | Calldata range  -> copy_calldata_to_top le ce range
      | Stack s         -> copy_nth_stack_elem le ce s ) in
    let ce = align_from_right_aligned ce alignment ty in
    (* le needs to remember the alignment *)
    le, ce

let swap_entrance_pc_with_zero ce =
    let ce = PUSH1 (Int 0)      >>> ce in
    let ce = SLOAD              >>> ce in
    let ce = PUSH1 (Int 0)      >>> ce in
    let ce = DUP1               >>> ce in
    let ce = SSTORE             >>> ce in
    ce

(** [restore_entrance_pc] moves the topmost stack element to the entrance pc *)
let restore_entrance_pc ce =
    let ce = PUSH1 (Int 0)      >>> ce in
    let ce = SSTORE             >>> ce in
    ce

(** [throw_if_zero] peeks the topmost stack element and throws if it's zero *)
let throw_if_zero ce =
    let ce = DUP1               >>> ce in
    let ce = ISZERO             >>> ce in
    let ce = PUSH1 (Int 0)      >>> ce in
    let ce = JUMPI              >>> ce in
    ce

(** [mem_alloc] behaves like an opcode
 * that takes a desired memory size as an arg.
 * This pushes the allocated address.
 *)

(* mem_alloc  :=   len := pop; allocate len; push oldAlloc *) 
(* M[64]  :=   the address of mem alloc *) 
(* MSTORE :=   x=pop ; y=pop ; M[x]=y i *) 
(* MLOAD  :=   x=pop ; push M[x]        *) 
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
type memoryPacking = TightPacking | ABIPacking

(** [copy_runtime_code_to_mem ce contracts cid]
 * /adds opcodes to [ce] so that in the final state the memory contains the runtime code
 * for all contracts that are reachable from [cid] in the
 * list [contracts] in the
 * addresses [code_start, code_start + code_size).
 * This adds two elements to the stack, resulting in
 * [..., code_length, code_start) *)
let copy_runtime_code_to_mem ce cid =
    let start_size = stack_size ce in                 (*                                                                           .. *)  
    let ce = PUSH32(RuntimeCodeSize)        >>> ce in (*                                                              codesize <<< .. *)
    let ce = DUP1                           >>> ce in (*                                                 codesize <<< codesize <<< .. *)  
    let ce = mem_alloc                          ce in (*                                          alloc(codesize) <<< codesize <<< .. *)
    let ce = DUP2                           >>> ce in (*                             codesize <<< alloc(codesize) <<< codesize <<< .. *)
    let ce = PUSH32(RuntimeCodeOffset cid)  >>> ce in (*                     cid <<< codesize <<< alloc(codesize) <<< codesize <<< .. *)
    let ce = DUP3                           >>> ce in (* alloc(codesize) <<< cid <<< codesize <<< alloc(codesize) <<< codesize <<< .. *)
    let ce = CODECOPY                       >>> ce in (*                                          alloc(codesize) <<< codesize <<< .. *)
    assert (stack_size ce = start_size+2) ; 
    ce

let copy_code_to_mem ce =                             (*                                                      cid <<< codesize <<< .. *)
    let ce = DUP2                           >>> ce in (*                                         codesize <<< cid <<< codesize <<< .. *)
    let ce = mem_alloc                          ce in (*                                  alloc(codesize) <<< cid <<< codesize <<< .. *)
    let ce = SWAP1                          >>> ce in (*                                  cid <<< alloc(codesize) <<< codesize <<< .. *)
    let ce = DUP3                           >>> ce in (*                     codesize <<< cid <<< alloc(codesize) <<< codesize <<< .. *)
    let ce = SWAP1                          >>> ce in (*                     cid <<< codesize <<< alloc(codesize) <<< codesize <<< .. *)
    let ce = DUP3                           >>> ce in (* alloc(codesize) <<< cid <<< codesize <<< alloc(codesize) <<< codesize <<< .. *) 
    let ce = CODECOPY                       >>> ce in (*                                          alloc(codesize) <<< codesize <<< .. *)  
    ce

(** [copy_whole_code_to_mem] allocates enough memory to accomodate the
 *  whole of the currently running code, and copies it there.
 *  After this, [size, offset] of the memory region is left on the stack.
 *)
let copy_whole_code_to_mem ce =
    let start_size = stack_size ce               in (*                                                                           .. *)        
    let ce = CODESIZE                       >>> ce in (*                                                              codesize <<< .. *)
    let ce = DUP1                           >>> ce in (*                                                 codesize <<< codesize <<< .. *)
    let ce = mem_alloc                          ce in (*                                          alloc(codesize) <<< codesize <<< .. *)
    let ce = DUP2                           >>> ce in (*                             codesize <<< alloc(codesize) <<< codesize <<< .. *)
    let ce = PUSH1(Int 0)                   >>> ce in (*                      0  <<< codesize <<< alloc(codesize) <<< codesize <<< .. *)
    let ce = DUP3                           >>> ce in (* alloc(codesize) <<<  0  <<< codesize <<< alloc(codesize) <<< codesize <<< .. *)
    let ce = CODECOPY                       >>> ce in (*                                          alloc(codesize) <<< codesize <<< .. *)
    assert(start_size + 2 = stack_size ce) ;        
    ce

let push_method (ce:ce) (a_mthd:mthd_info) =
    let hash  = Eth.hash_of_mthd_info_ty a_mthd in
    let ce    = PUSH4(Big(Eth.hex_to_big_int hash)) >>> ce      in 
    ce

(** [prepare_functiohn_signature ce mthd]
 *  Allocates 4 bytes on the memory, and puts the function signature of the arg there.
 *  After that, the stack has (..., signature size, signature offset )
 *)
let prepare_function_signature ce mthd =
    let start_size = stack_size ce in
    let ce = PUSH1(Int 4)       >>> ce in (* stack : (..., 4) *)
    let ce = DUP1               >>> ce in (* stack : (..., 4, 4) *)
    let ce = mem_alloc ce              in (* stack : (..., 4, signature_offset) *)
    let ce = push_method ce mthd       in (* stack : (..., 4, signature_offset, sig) *)
    let ce = DUP2               >>> ce in (* stack : (..., 4, signature_offset, sig, signature_offset) M[4] := method_sig *) 
    let ce = MSTORE             >>> ce in (* stack : (..., 4, signature_offset) *)
    let () = assert (stack_size ce = start_size + 2) in
    ce

let keccak_cons le ce =
    let start_size = stack_size     ce in (* put the top into 0x00 *)
    let ce = PUSH1 (Int 0x0)    >>> ce in
    let ce = MSTORE             >>> ce in (* put the top into 0x20 *)
    let ce = PUSH1 (Int 0x20)   >>> ce in
    let ce = MSTORE             >>> ce in (* take the sah3 of 0x00--0x40 *)
    let ce = PUSH1 (Int 0x40)   >>> ce in
    let ce = PUSH1 (Int 0x0 )   >>> ce in
    let ce = SHA3               >>> ce in
    let () = assert (stack_size ce + 1 = start_size) in
    ce

let incr_top ce (inc : int) =
    let ce = PUSH32 (Int inc)   >>> ce in
    let ce = ADD                >>> ce in
    ce


















(* CODEGEN *) 


(** [add_constructor_arg_to_mem ce arg] realizes [arg] on the memory
 *  according to the ABI.  This increases the stack top element by the size of the
 *  new allocation. *)
let rec add_constructor_arg_to_mem le (packing:memoryPacking) ce (arg:ty expr) =
    let start_size  = stack_size ce in
    let ty          = snd arg in
    printf"it's appending type %s\n"(string_of_ty ty) ; 
    assert (fits_in_one_stor_slot ty) ; 
    let i,a= (match packing with 
              | ABIPacking    -> 32           , R_ 
              | TightPacking  -> size_of_ty ty, L_ ) in (* stack : [acc] *)
    let ce = PUSH1 (Int i)              >>> ce in   (* stack : [acc, size] *)
    let ce = DUP1                       >>> ce in   (* stack : [acc, size, size] *)
    let ce = mem_alloc ce                      in   (* stack : [acc, size, offset] *)
    let ce = codegen_expr le ce         a arg  in   (* stack : [acc, size, offset, val] *)
    let ce = SWAP1                      >>> ce in   (* stack : [acc, size, val, offset] *)
    let ce = MSTORE                     >>> ce in   (* stack : [acc, size] *)
    let ce = ADD                        >>> ce in
    assert (stack_size ce = start_size) ; 
    ce

(** [add_constructor_args_to_mem args] realizes [args] on the memory
 *  according to the ABI.  This leaves the amount of memory on the stack.
 *  Usually this function is called right after the constructor code is set up in the memory,
 *  so the offset of the memory is not returned.
 *  (This makes it easy for the zero-arg mthd)
 *)
and add_constructor_args_to_mem le(pack:memoryPacking)ce(args:ty expr list) =
    let start_size  = stack_size            ce in
    let ce = PUSH1(Int 0)               >>> ce in (* stack [0] *)
    let ce = L.fold_left(add_constructor_arg_to_mem le pack)ce args in
    assert (start_size+1 = stack_size ce) ; 
    ce

and produce_init_code_in_mem (le : le) ce new_expr =
    let name =  new_expr.new_head in
    let cid  =  try cid_lookup ce name
                with Not_found->eprintf"A contract of name %s is unknown.\n%!"name;raise Not_found in
    let ce = PUSH32(ConstructorCodeSize cid)            >>> ce  in
    let ce = PUSH32(ConstructorInRuntimeCodeOffset cid) >>> ce  in (* stack: [codesize, codeoffset] *)
    let ce = copy_code_to_mem ce                                in (* stack: [mem_size, mem_offset] *)
    let ce = copy_whole_code_to_mem ce                          in (* stack: [mem_size, mem_offset, mem_second_size, mem_second_offset] *)
    (* I still need to add the constructor args *)
    let ce = add_constructor_args_to_mem le ABIPacking ce new_expr.new_args in  (* stack: [mem_size, mem_offset, mem_second_size, mem_second_offset, mem_args_size] *)
    let ce = SWAP1                                      >>> ce in (* stack: [mem_size, mem_offset, mem_second_size, mem_args_size, mem_second_offset] *)
    let ce = POP                                        >>> ce in (* stack: [mem_size, mem_offset, mem_second_size, mem_args_size] *)
    let ce = ADD                                        >>> ce in (* stack: [mem_size, mem_offset, mem_second_args_size] *)
    let ce = SWAP1                                      >>> ce in (* stack: [mem_size, mem_second_args_size, mem_offset] *)
    let ce = SWAP2                                      >>> ce in (* stack: [mem_offset, mem_second_args_size, mem_size] *)
    let ce = ADD                                        >>> ce in (* stack: [mem_offset, mem_total_size] *)
    let ce = SWAP1                                      >>> ce in (* stack: [mem_total_size, mem_offset] *)
    ce

and codegen_function_call_expr (le:le) ce alignment (function_call:ty function_call) (retT:ty) =
    if function_call.call_head = "pre_ecdsarecover" then (
        assert (alignment = R_) ; 
        codegen_ecdsarecover le ce function_call.call_args retT )   (* XXX: need to pass alignment *)
    else if function_call.call_head = "keccak256" then (
        assert (alignment = R_) ; 
        codegen_keccak256    le ce function_call.call_args retT )   (* XXX: need to pass alignment *) 
    else if function_call.call_head = "iszero" then 
        codegen_iszero le ce alignment function_call.call_args retT
    else
        err "codegen_function_call_expr: unknown function head."

and codegen_iszero le ce alignment args retT = match args with
    | [arg]   ->  assert (retT = TyBool) ; 
                  let ce = codegen_expr le ce alignment arg in
                  ISZERO  >>> ce 
    | _       ->  err "codegen_iszero: seeing a wrong number of args"
       
and codegen_keccak256 le ce args rettyp =
    let start_size = stack_size ce in
    let ce = get_alloc ce in
    (* stack: [..., offset] *)
    let ce = add_constructor_args_to_mem le TightPacking ce args in
    (* stack: [..., offset, size] *)
    let ce =  SWAP1       >>> ce in
    (* stack: [..., size, offset] *)
    let ce =  SHA3        >>> ce in
    let () = assert(stack_size ce = start_size + 1) in
    ce

and codegen_ecdsarecover le ce args rettyp = match args with
    | [h; v; r; s] ->
        let start_size = stack_size     ce  in  (* stack: [] *)
        let ce = PUSH1 (Int 32)     >>> ce  in  (* stack: [out size] *)
        let ce = DUP1               >>> ce  in  (* stack: [out size, out size] *)
        let ce = mem_alloc              ce  in  (* stack: [out size, out address] *)
        let ce = DUP2               >>> ce  in  (* stack: [out size, out address, out size] *)
        let ce = DUP2               >>> ce  in  (* stack: [out size, out address, out size, out address] *)
        let ce = get_alloc              ce  in
        let ce = add_constructor_args_to_mem le ABIPacking ce args in  (* stack: [out size, out address, out size, out address, mem_offset, mem_total_size] *)
        let ce = SWAP1              >>> ce  in  (* stack: [out size, out address, out size, out address, in size, in offset] *)
        let ce = PUSH1 (Int 0)      >>> ce  in  (* stack: [out size, out address, out size, out address, in size, in offset, value] *)
        assert (stack_size ce = start_size+7) ;
        let ce = PUSH1 (Int 1)      >>> ce  in  (* stack: [out size, out address, out size, out address, in size, in offset, value, to] *)
        let ce = PUSH4 (Int 10000)  >>> ce  in  (* stack: [out size, out address, out size, out offset, in size, in offset, value, to, gas] *)
        let ce = CALL               >>> ce  in  (* stack: [out size, out address, success?] *)
        assert (stack_size ce = start_size+3) ;
        let ce = throw_if_zero ce           in
        let ce = POP                >>> ce  in  (* stack: [out size, out address] *)
        assert (stack_size ce = start_size+2) ; 
        let ce = SWAP1              >>> ce  in  (* stack: [out address, out size] *)
        let ce = POP                >>> ce  in  (* we know it's 32 *) (* stack: [out address] *)
        let ce = MLOAD              >>> ce  in  (* stack: [output] *)
        assert (stack_size ce = start_size+1) ;    
        ce
    | _ -> err "pre_ecdsarecover has a wrong number of args"

and codegen_new_expr (le : le) ce (new_expr : ty new_expr) (contractname : string) =
    let start_size = stack_size ce in   (* assert that the reentrance info is throw *)
    assert(is_throw_only new_expr.new_msg_info.msg_reentrance_info) ;  (* set up the reentrance guard *)
    let ce = swap_entrance_pc_with_zero ce          in      (* stack : [entrance_pc_bkp] *)
    let ce = produce_init_code_in_mem le ce new_expr in      (* stack : [entrance_pc_bkp, size, offset] *)
    let ce = (match new_expr.new_msg_info.msg_value_info with
       | None     -> (PUSH1 (Int 0)) >>> ce             (* no value info means value of zero *)
       | Some e   -> codegen_expr le ce R_ e) in   (* stack : [entrance_pc_bkp, size, offset, value] *)
    let ce =  CREATE            >>> ce in                   (* stack : [entrance_pc_bkp, create_result] *)
    (* check the return value, if zero, throw *)
    let ce = throw_if_zero          ce in                   (* stack : [entrance_pc_bkp, create_result] *)
    let ce =  SWAP1             >>> ce in                   (* stack : [create_result, entrance_pc_bkp] *)
    (* remove the reentrance guard *)
    let ce = restore_entrance_pc ce in                      (* stack : [create_result] *)
    assert (stack_size ce = start_size + 1) ; 
    ce

and generate_array_access_index le ce aa =
    let array = aa.array_access_array in
    let index = aa.array_access_index in
    let ce    = codegen_expr le ce R_ index in
    let ce    = codegen_expr le ce R_ array in
    let ce    = keccak_cons le ce in
    ce

and codegen_array_access (le : le) ce (aa : ty array_access) =
    let ce    = generate_array_access_index le ce aa in
    SLOAD >>> ce 

(* if the stack top is zero, set up an array seed at aa, and replace the zero with the new seed *)
and setup_array_seed_at_array_access le ce aa =
    let label = get_new_label ()           in       (* stack: [result, result] *)
    let ce = DUP1                   >>> ce in       (* stack: [result, result] *)
    let ce = PUSH4(Label label)     >>> ce in       (* stack: [result, result, shortcut] *)
    let ce = JUMPI                  >>> ce in       (* stack: [result] *)
    let ce = POP                    >>> ce in       (* stack: [] *)
    let ce = generate_array_access_index le ce aa in(* stack: [stor_index] *)
    let ce = PUSH1 (Int 1)          >>> ce in       (* stack: [stor_index, 1] *)
    let ce = SLOAD                  >>> ce in       (* stack: [stor_index, orig_seed] *)
    let ce = DUP1                   >>> ce in       (* stack: [stor_index, orig_seed, orig_seed] *)
    let ce = incr_top ce 1                 in       (* stack: [stor_index, orig_seed, orig_seed + 1] *)
    let ce = PUSH1 (Int 1)          >>> ce in       (* stack: [stor_index, orig_seed, orig_seed + 1, 1] *)
    let ce = SSTORE                 >>> ce in       (* stack: [stor_index, orig_seed] *)
    let ce = DUP1                   >>> ce in       (* stack: [stor_index, orig_seed, orig_seed] *)
    let ce = SWAP2                  >>> ce in       (* stack: [orig_seed, orig_seed, stor_index] *)
    let ce = SSTORE                 >>> ce in       (* stack: [orig_seed] *)
    let ce = JUMPDEST label         >>> ce in       (* stack: [result] *)
    ce

(*   if the stack top is zero, set up an array seed at aa, and replace the zero with the new seed *)
and setup_array_seed_at_location le ce loc =
    let stor_idx = (match loc with
      | Loc.Storage stor_range    ->  let () = assert (stor_range.Loc.stor_size = (Int 1)) in
                                      stor_range.Loc.stor_start
      | _                         ->  err "setup array seed at non-storage") in
    let label = get_new_label ()    in    (* stack: [result, result] *)
    let ce = DUP1                   >>> ce in    (* stack: [result, result] *)
    let ce = PUSH32(Label label)    >>> ce in    (* stack: [result, result, shortcut] *)
    let ce = JUMPI                  >>> ce in    (* stack: [result] *)
    let ce = POP                    >>> ce in    (* stack: [] *)
    let ce = PUSH32 stor_idx     >>> ce in    (* stack: [stor_index] *)
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
and codegen_expr (le:le) (ce:ce) (alignment:alignment) ((e,t):ty expr) : ce =
    let ret = (match e,t with
    | AddressExpr((c,TyContractInstance _)as inner),TyAddr ->
                                let ce = codegen_expr le ce alignment inner in
                                (* c is a contract instance.
                                 * The concrete representation of a contact instance is already the address *)
                                ce
    | AddressExpr _, _       -> err "codegen_expr: AddressExpr of unexpected type"
    | ValueExpr,TyUint256    -> CALLVALUE >>> ce 
    | ValueExpr,_            -> err "ValueExpr of strange type"
    | SenderExpr,TyAddr      -> let ce = CALLER     >>> ce in
                                let ce = align_address ce alignment in
                                ce
    | SenderExpr,_           -> err "codegen_expr: SenderExpr of strange type"
    | ArrayAccessExpr aa,ty  ->
                                let ce = codegen_array_access le ce (read_array_access aa) in
                                assert (alignment = R_) ; 
                                (match ty with
                                | TyMap _   -> setup_array_seed_at_array_access le ce (read_array_access aa)
                                | _         -> ce    ) 
    | ThisExpr,_             -> let ce = ADDRESS >>> ce in
                                let ce = align_address ce alignment in
                                ce
    | IdentifierExpr id,ty   -> (match lookup le id with
                                (** if things are just DUP'ed, location env should not be updated.  
                                 *  If they are SLOADED,   the location env should be updated. *)
                                | Some location         ->  let (le, ce) = copy_to_top le ce alignment ty location in
                                                            begin match ty with
                                                            | TyMap _   -> setup_array_seed_at_location le ce location
                                                            | _         -> ce           end
                                | None                  ->  err ("codegen_expr: identifier's location not found: "^id) )
    | FalseExpr,TyBool       ->
                                let ce = PUSH1(Big zero_big_int)   >>> ce in
                                assert (alignment = R_) ; 
                                ce
    | FalseExpr, _           -> err "codegen_expr: FalseExpr of unexpected type"
    | TrueExpr,TyBool        ->
                                let ce = PUSH1(Big unit_big_int)   >>> ce in 
                                assert (alignment = R_) ; 
                                ce
    | TrueExpr, _            -> err "codegen_expr: TrueExpr of unexpected type"
    | DecLit256Expr d,TyUint256  ->
                                let ce = PUSH32(Big d)             >>> ce in
                                assert (alignment = R_) ; 
                                ce
    | DecLit256Expr d, _     -> err ("codegen_expr: DecLit256Expr of unexpected type: "^(string_of_big_int d))
    | DecLit8Expr d, TyUint8 ->
                                let ce = PUSH1(Big d)              >>> ce in
                                assert (alignment = R_) ; 
                                ce
    | DecLit8Expr d, _       -> err ("codegen_expr: DecLit8Expr of unexpected type: "^(string_of_big_int d))
    | LandExpr(l,r),TyBool   ->
                                let label = get_new_label () in
                                assert (alignment=R_) ; 
                                let ce = codegen_expr le ce R_ l in (* stack: [..., l] *)
                                let ce = DUP1                       >>> ce in (* stack: [..., l, l] *)
                                let ce = ISZERO                     >>> ce in (* stack: [..., l, !l] *)
                                let ce = PUSH4(Label label)         >>> ce in (* stack: [..., l, !l, shortcut] *)
                                let ce = JUMPI                      >>> ce in (* stack: [..., l] *)
                                let ce = POP                        >>> ce in (* stack: [...] *)
                                let ce = codegen_expr le ce R_ r in (* stack: [..., r] *)
                                let ce = JUMPDEST label             >>> ce in
                                let ce = ISZERO                     >>> ce in
                                let ce = ISZERO                     >>> ce in
                                ce
    | LandExpr (_, _), _     -> err "codegen_expr: LandExpr of unexpected type"
    | NotExpr sub, TyBool    ->
                                let ce = codegen_expr le ce alignment sub   in
                                let ce = ISZERO                     >>> ce in 
                                let ce = align_boolean ce alignment        in
                                ce
    | NotExpr sub, _         -> err "codegen_expr: NotExpr of unexpected type"
    | NowExpr,TyUint256      -> TIMESTAMP   >>> ce 
    | NowExpr,_              -> err "codegen_expr: NowExpr of unexpected type"
    | NeqExpr(l,r),TyBool    ->
                                let ce = codegen_expr le ce R_ r   in
                                let ce = codegen_expr le ce R_ l   in (* l later because it should come at the top *)
                                let ce = EQ                         >>> ce  in
                                let ce = ISZERO                     >>> ce  in
                                let ce = align_boolean ce alignment         in
                                ce
    | NeqExpr _, _           -> err "codegen_expr: NeqExpr of unexpected type"
    | LtExpr(l,r),TyBool     ->
                                let ce = codegen_expr le ce R_ r   in
                                let ce = codegen_expr le ce R_ l   in
                                let ce = LT                         >>> ce  in
                                let ce = align_boolean ce alignment         in
                                ce
    | LtExpr _, _            -> err "codegen_expr: LtExpr of unexpected type"
    | PlusExpr(l,r),TyUint256->
                                let ce = codegen_expr le ce R_ r in
                                let ce = codegen_expr le ce R_ l in
                                ADD >>> ce 
    | PlusExpr(l,r),TyUint8  ->
                                let ce = codegen_expr le ce R_ r  in
                                let ce = codegen_expr le ce R_ l  in
                                ADD >>> ce 
    | PlusExpr(l,r),_        -> err "codegen_expr PlusExpr of unexpected type"
    | MinusExpr(l,r),TyUint256->
                                let ce = codegen_expr le ce R_ r  in
                                let ce = codegen_expr le ce R_ l  in
                                SUB >>> ce 
    | MinusExpr(l,r),TyUint8 ->
                                let ce = codegen_expr le ce R_ r  in
                                let ce = codegen_expr le ce R_ l  in
                                SUB >>> ce 
    | MinusExpr(l,r),_       -> err "codegen_expr MinusExpr of unexpected type"
    | MultExpr(l,r),TyUint256->
                                let ce = codegen_expr le ce R_ r  in
                                let ce = codegen_expr le ce R_ l  in
                                MUL >>> ce
    | MultExpr(l,r),TyUint8  ->
                                let ce = codegen_expr le ce R_ r  in
                                let ce = codegen_expr le ce R_ l  in
                                MUL >>> ce
    | MultExpr (l, r), _     -> err "codegen_expr: MultExpr of unexpected type"
    | GtExpr(l,r),TyBool     ->
                                let ce = codegen_expr le ce R_ r  in
                                let ce = codegen_expr le ce R_ l  in
                                let ce = GT                         >>> ce in
                                let ce = align_boolean ce alignment in 
    (* XXX there should be some type system making sure this the above line exists *)
                                ce
    | GtExpr _, _            -> err "codegen_expr GtExpr of unexpected type"
    | BalanceExpr inner,TyUint256 ->
                                let ce = codegen_expr le ce R_ inner in
                                BALANCE >>> ce
    | BalanceExpr inner, _   -> err "codegen_expr: BalanceExpr of unexpected type"
    | EqualityExpr(l,r),TyBool->
                                let ce = codegen_expr le ce R_ r  in
                                let ce = codegen_expr le ce R_ l  in
                                let ce = EQ                         >>> ce in
                                let ce = align_boolean ce alignment in
                                ce
    | EqualityExpr _, _      -> err "codegen_expr EqualityExpr of unexpected type"
    | SendExpr s, _          ->
                                assert (alignment = R_) ; 
                                codegen_send_expr le ce s
    | NewExpr new_e, TyContractInstance ctyp ->
                                assert (alignment = R_) ; 
                                codegen_new_expr le ce new_e ctyp
    | NewExpr new_e, _       -> err "expr code gen for new expression with unexpected type"
    | FunctionCallExpr fn_call,retT ->
                                codegen_function_call_expr le ce alignment fn_call retT
    | ParenthExpr _, _       -> err "ParenthExpr not expected."
    | SingleDerefExpr(ref,ref_ty), value_ty ->
                                assert (ref_ty    = TyRef[value_ty]) ;
                                assert (alignment = R_   ) ; 
                                let size = size_of_ty value_ty in
                                assert (size <= 32) ;   (* assuming word-size *)
                                let ce = codegen_expr le ce R_ (ref, ref_ty) in (* pushes the pointer *)
                                MLOAD                      >>> ce 
    | TupleDerefExpr _,_     -> err "code generation for TupleDerefExpr should not happen.  Instead, try to decompose it into several assignments."
    ) in
    assert (stack_size ret = stack_size ce + 1) ; 
    ret


(** [prepare_arg ce arg] places an arg in the memory, and increments the stack top position by the size of the arg. *)
and prepare_arg le ce arg =    (* stack: (..., accum) *)
    let start_size    = stack_size ce         in
    let size          = size_of_ty (snd arg)  in
    assert (size = 32) ; 
    let ce = PUSH1 (Int size)     >>> ce in   (* stack: (..., accum, size) *)
    let ce = codegen_expr le ce R_ arg in    (* stack: (..., accum, size, val) *)
    let ce = DUP2                 >>> ce in   (* stack: (..., accum, size, val, size) *)
    let ce = mem_alloc                ce in   (* stack: (..., accum, size, val, offset) *)
    let ce = MSTORE               >>> ce in   (* stack: (..., accum, size) *)
    let ce = ADD                  >>> ce in   (* stack: (..., new_accum) *)
    assert (stack_size ce = start_size) ; 
    ce
(** [prepare_args] prepares args in the memory.
 *  This leaves (..., args size) on the stack.
 *  Since this is called always immediately after allocating memory for the signature,
 *  the offset of the memory is not necessary.
 *  Also, when there are zero amount of memory desired, it's easy to just return zero.
 *)
and prepare_args le ce args =
    let start_size = stack_size ce in
    let ce = PUSH1 (Int 0)      >>> ce  in
    let ce = L.fold_left (prepare_arg le) ce args in
    assert (stack_size ce = start_size + 1) ; 
    ce

(** [prepare_input_in_mem] prepares the input for CALL opcode in the memory.
 *  That leaves "..., in size, in offset" (top) on the stack.
 *)
and prepare_input_in_mem le ce s mthd : ce =
    let start_size = stack_size ce in
    let ce = prepare_function_signature ce mthd in    (* stack : [signature size, signature offset] *)
    let args = s.send_args in
    let ce = prepare_args le ce args in (* this should leave only one number on the stack!! *)   (* stack : [signature size, signature offset, args size] *)
    let ce = SWAP1              >>> ce in (* stack : [signature size, args size, signature offset] *)
    let ce = SWAP2              >>> ce in (* stack : [signature offset, args size, signature size] *)
    let ce = ADD                >>> ce in (* stack : [signature offset, total size] *)
    let ce = SWAP1              >>> ce in (* stack : [total size, signature offset] *)
    assert (stack_size ce = start_size + 2) ; 
    ce
(** [obtain_ret_values_from_mem] assumes stack (..., out size, out offset),
    and copies the outputs onto the stack.  The first comes top-most. *)
(* XXX currently supports one-word output only *)
and obtain_ret_values_from_mem ce = (* stack: out size, out offset *)
    let ce = DUP2               >>> ce in (* stack: out size, out offset, out size *)
    let ce = PUSH1 (Int 32)     >>> ce in (* stack: out size, out offset, out size, 32 *)
    let ce = EQ                 >>> ce in (* stack: out size, out offset, out size = 32 *)
    let ce = ISZERO             >>> ce in (* stack: out size, out offset, out size != 32 *)
    let ce = PUSH1 (Int 0)      >>> ce in (* stack: out size, out offset, out size != 32, 0 *)
    let ce = JUMPI              >>> ce in (* stack: out size, out offset *)
    let ce = MLOAD              >>> ce in (* stack: out size, out *)
    let ce = SWAP1              >>> ce in (* stack: out, out size *)
    let ce = POP                >>> ce in (* stack: out *)
    ce
and codegen_send_expr le ce (s:ty send_expr) =
    let start_size = stack_size ce in
    let head_contract = s.send_head_contract in
    match snd head_contract with
    | TyContractInstance c_name -> 
        let callee_cid  =   
            try cid_lookup ce c_name
            with Not_found ->eprintf"A contract of name %s is unknown.\n%!"c_name;raise Not_found in
        let callee : ty contract = contract_lookup ce callee_cid in
        let contract_lookup_by_name(name:string) : ty contract =
            let cid =  
                try cid_lookup ce name
                with Not_found ->eprintf"A contract of name %s is unknown.\n%!"c_name;raise Not_found in 
            contract_lookup ce cid in
        begin match s.send_head_method with
        | None             -> err "could not find the method name"
        | Some method_name -> (
           let m : mthd_info = lookup_mthd_info callee method_name contract_lookup_by_name in
           assert(is_throw_only s.send_msg_info.msg_reentrance_info) ; 
           let ce = swap_entrance_pc_with_zero ce       in (* stack : [entrance_pc_bkp] *)
           let ret_ty      = m.mthd_ret_ty   in
           let ret_size    = size_of_tys ret_ty         in (* stack : [entrance_bkp] *)
           let ce = PUSH1 (Int ret_size)        >>> ce  in (* stack : [entrance_bkp, out size] *)
           let ce = DUP1                        >>> ce  in (* stack : [entrance_bkp, out size, out size] *)
           assert (stack_size ce = start_size+3) ;
           let ce = mem_alloc                       ce  in (* stack : [entrance_bkp, out size, out offset] *)
           let ce = DUP2                        >>> ce  in (* stack : [entrance_bkp, out size, out offset, out size] *)
           let ce = DUP2                        >>> ce  in (* stack : [entrance_bkp, out size, out offset, out size, out offset] *)
           let ce = prepare_input_in_mem le ce s m      in (* stack : [entrance_bkp, out size, out offset, out size, out offset, in size, in offset] *)
           let ce = (match s.send_msg_info.msg_value_info with
              | None   -> PUSH1 (Int 0)         >>> ce    (* no value info means value of zero *)
              | Some e -> codegen_expr le ce R_ e) in (* stack : [entrance_bkp, out size, out offset, out size, out offset, in size, in offset, value] *)
           let ce = codegen_expr le ce R_ s.send_head_contract in
           let ce = PUSH4(Int 3000)             >>> ce  in
           let ce = GAS                         >>> ce  in
           let ce = SUB                         >>> ce  in (* stack : [entrance_bkp, out size, out offset, out size, out offset, in size, in offset, value, to, gas] *)
           let ce = CALL                        >>> ce  in (* stack : [entrance_bkp, out size, out offset, success] *)
           assert (stack_size ce = start_size+4) ; 
           let ce = ISZERO                      >>> ce  in
           let ce = PUSH1(Int 0)                >>> ce  in
           let ce = JUMPI                       >>> ce  in (* stack : [entrance_bkp, out size, out offset] *)
           assert (stack_size ce = start_size+3) ; 
           let ce = SWAP2                       >>> ce  in (* stack : [out offset, out size entrance_bkp] *)
           let ce = restore_entrance_pc ce              in (* stack : [out offset, out size] *)
           let ce = SWAP1                       >>> ce  in (* stack : [out size, out offset] *)
           let ce = obtain_ret_values_from_mem ce in (* stack : [outputs] *)
           ce )
        end
    | TyAddr        -> (
        assert (is_throw_only s.send_msg_info.msg_reentrance_info) ; 
        let ce = swap_entrance_pc_with_zero ce in (* stack : [entrance_pc_bkp] *)
        let return_size = 0                         in (* stack : [entrance_bkp] *)
        let ce = PUSH1(Int return_size)     >>> ce  in (* stack : [entrance_bkp, 0] *)
        let ce = DUP1                       >>> ce  in (* stack : [entrance_bkp, 0, 0] *)
        assert (stack_size ce=start_size+3) ; 
        let ce = DUP2                       >>> ce  in (* stack : [entrance_bkp, 0, 0, 0] *)
        let ce = DUP2                       >>> ce  in (* stack : [entrance_bkp, 0, 0, 0, 0] *)
        let ce = DUP2                       >>> ce  in (* stack : [entrance_bkp, 0, 0, 0, 0, 0] *)
        let ce = DUP2                       >>> ce  in (* stack : [entrance_bkp, 0,        0,          0,        0,          0,       0] *)
                                                       (* stack : [entrance_bkp, out size, out offset, out size, out offset, in size, in offset] *)
        let ce = (match s.send_msg_info.msg_value_info with
           | None   -> PUSH1 (Int 0)        >>> ce  (* no value info means value of zero *)
           | Some e -> codegen_expr le ce R_ e) in (* stack : [entrance_bkp, out size, out offset, out size, out offset, in size, in offset, value] *)
        let ce = codegen_expr le ce R_ s.send_head_contract in
        let ce = PUSH4 (Int 3000)           >>> ce  in
        let ce = GAS                        >>> ce  in
        let ce = SUB                        >>> ce  in (* stack : [entrance_bkp, out size, out offset, out size, out offset, in size, in offset, value, to, gas] *)
        let ce = CALL                       >>> ce  in (* stack : [entrance_bkp, out size, out offset, success] *)
        assert (stack_size ce=start_size+4) ; 
        let ce = ISZERO                     >>> ce  in
        let ce = PUSH1 (Int 0)              >>> ce  in
        let ce = JUMPI                      >>> ce  in (* stack : [entrance_bkp, out size, out offset] *)
        assert (stack_size ce=start_size+3) ; 
        let ce = SWAP2                      >>> ce  in (* stack : [out offset, out size entrance_bkp] *)
        let ce = restore_entrance_pc ce             in (* stack : [0, 0] *)
        let ce = POP                        >>> ce  in (* XXX: Some optimizations possible. *) (* stack : [0] *)
        ce )
    | TyVoid        -> err "send expression with TyVoid?"
    | TyUint256     -> err "send expression with TyUint256?"
    | TyUint8       -> err "send expression with TyUint8?"
    | _             -> err "send expression with unknown type"

















let codegen_stmt (orig:ce) (s:ty stmt) : ce = err "codegen_stmt"
  (* is this enough? also add stmt Id's around?
   * I think this is enough. *)

let move_info_around (assumption:ce) (goal:le) : ce = err "move_info_around"

let codegen_bytecode (src:ty contract) : imm Evm.program = err "codegen_bytecode"


(* [init_mem_alloc] initialize as M[64] := 96 *)
let init_mem_alloc (ce:ce) =
  let ce = PUSH1 (Int 96)           >>> ce in
  let ce = PUSH1 (Int 64)           >>> ce in
  let ce = MSTORE                   >>> ce in
  ce




(****************************)
(*      CONTRACT PC         *) 
(****************************)
(**
 * [set_contract_pc ce id] puts the program counter for the contract specified by
   [id] in the storage at index [StoragePCIndex]
 *)
let set_contract_pc ce (cid:cid) =
  let start_size = stack_size ce in
  let ce = PUSH32 (ContractOffsetInRuntimeCode cid) >>> ce in
  let ce = PUSH32 StoragePCIndex                    >>> ce in
  let ce = SSTORE                                   >>> ce in
  assert (stack_size ce = start_size) ; 
  ce
(**
 * [get_contract_pc ce] pushes the value at [StoragePCIndex] in storage.
 *)
let get_contract_pc ce =
  let start_size = stack_size ce in
  let ce = PUSH32 StoragePCIndex    >>> ce in
  let ce = SLOAD                    >>> ce in
  assert (stack_size ce = start_size + 1) ;
  ce



  

(****************************)
(*        COPY CODE         *) 
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
    let ce = JUMPDEST label         >>> ce in    (*                          cid <<< mem_start <<< size <<< .. *)
    let ce = DUP3                   >>> ce in    (*                 size <<< cid <<< mem_start <<< size <<< .. *)
    let ce = ISZERO                 >>> ce in    (*          size_iszero <<< cid <<< mem_start <<< size <<< .. *)
    let ce = PUSH4(Label exit)      >>> ce in    (* exit <<< size_iszero <<< cid <<< mem_start <<< size <<< .. *)
    assert (stack_size ce = start_size+2) ;
(* if size_iszero then GOTO exit *) 
    let ce = JUMPI                  >>> ce in    (*                          cid <<< mem_start <<< size <<< .. *)   
    let ce = DUP2                   >>> ce in    (*            mem_start <<< cid <<< mem_start <<< size <<< .. *) 
    let ce = MLOAD                  >>> ce in    (*         M[mem_start] <<< cid <<< mem_start <<< size <<< .. *)
    let ce = DUP2                   >>> ce in    (* cid <<< M[mem_start] <<< cid <<< mem_start <<< size <<< .. *)
    (* SSTORE := pop x; pop y; S[x]=y *)
    let ce = SSTORE                 >>> ce in    (*                          cid <<< mem_start <<< size <<< .. *)  
    (* decrease size *)
    let ce = PUSH32(Int 32)         >>> ce in    (*                   32 <<< cid <<< mem_start <<< size <<< .. *)
    let ce = SWAP1                  >>> ce in    (*                   cid <<< 32 <<< mem_start <<< size <<< .. *)
    let ce = SWAP3                  >>> ce in    (*                   size <<< 32 <<< mem_start <<< cid <<< .. *)
    let ce = SUB                    >>> ce in    (*                       size-32 <<< mem_start <<< cid <<< .. *)
    let ce = SWAP2                  >>> ce in    (*                       cid <<< mem_start <<< size-32 <<< .. *) 
    (* 1 word is 32 bytes. *)
    let ce = incr_top ce 1                 in    (*                     cid+1 <<< mem_start <<< size-32 <<< .. *)
    let ce = SWAP1                  >>> ce in    (*                     mem_start <<< cid+1 <<< size-32 <<< .. *)
    (* increase mem_src_start *)
    let ce = incr_top ce 32                in    (*                  mem_start+32 <<< cid+1 <<< size-32 <<< .. *)
    let ce = SWAP1                  >>> ce in    (*                  cid+1 <<< mem_start+32 <<< size-32 <<< .. *)
    assert (stack_size ce = start_size) ;       
    let ce = PUSH4(Label label)     >>> ce in    (*        label <<< cid+1 <<< mem_start+32 <<< size-32 <<< .. *) 
    let ce = JUMP                   >>> ce in    (*                  cid+1 <<< mem_start+32 <<< size-32 <<< .. *)
    let ce = set_stack_size ce start_size  in
    let ce = JUMPDEST exit          >>> ce in    (*                          cid <<< mem_start <<< size <<< .. *)
    let ce = POP                    >>> ce in    (*                                  mem_start <<< size <<< .. *) 
    let ce = POP                    >>> ce in    (*                                                size <<< .. *) 
    let ce = POP                    >>> ce in    (*                                                         .. *)
    assert (stack_size ce = start_size - 3) ;
    ce

(** [copy_args_from_mem_to_stor le ce]
 *  adds opcodes to ce s.t.  the constructor args stored in the mem are copied to the storage.
 *
 *  Precondition  of the stack : [..., total, mem_start]
 *  Postcondition of the stack : [...] 
 *  Final storage has the args in [ConstructorArgumentBegin...ConstructorArgumentBegin + ConstructorArgumentLength]
 *)
let copy_args_from_mem_to_stor le ce (cid:cid) =
    let ce = PUSH32(InitDataSize cid) >>> ce in (*                                    datasize <<< mem_start <<< size <<< .. *)
    let ce = CODESIZE                 >>> ce in (*                       codesize <<< datasize <<< mem_start <<< size <<< .. *) 
    let ce = EQ                       >>> ce in (* (eq := if codesize==datasize then 1 else 0) <<< mem_start <<< size <<< .. *) 
    let ce = ISZERO                   >>> ce in (*                                      not eq <<< mem_start <<< size <<< .. *) 
    let ce = PUSH1 (Int 2)            >>> ce in (*                                2 <<< not eq <<< mem_start <<< size <<< .. *)
    (* IF noteq THEN GOTO 2     *) 
    let ce = JUMPI                    >>> ce in (*                                                 mem_start <<< size <<< .. *) 
    (* ELSE                     *) 
    let ce = PUSH32(StorageConstructorArgumentsBegin cid)>>>ce in (*                       cid <<< mem_start <<< size <<< .. *)
    copy_code_from_mem ce


(** [copy_args_from_code_to_mem]
 *  copies constructor args at the end of the bytecode into the memory.  
 *  The number of bytes is decided using the contract interface.
 *  The memory usage counter at byte [0x40] is increased accordingly.
 *  After this, the stack contains the size and the beginning of the memory piece that contains the args.
 *  Output [rest of the stack, mem_size, mem_begin].
 *)
let copy_args_from_code_to_mem(le:le)(ce:ce)(con:ty contract) : ce =
    let total_size = Eth.total_size_of_intf_args (L.map snd (Eth.constructor_args con)) in
    let start_size = stack_size ce         in (* [] *)
    let ce = PUSH32(Int total_size) >>> ce in (* [total_size] *)
    let ce = DUP1                   >>> ce in (* [total_size, total_size] *)
    let ce = mem_alloc                  ce in (* [total_size, mem_start] *)
    let ce = DUP2                   >>> ce in (* [total_size, mem_start, total_size] *)
    let ce = DUP1                   >>> ce in (* [total_size, mem_start, total_size, total_size] *)
    let ce = CODESIZE               >>> ce in (* [total_size, mem_start, total_size, total_size, code size] *)
    let ce = SUB                    >>> ce in (* [total_size, mem_start, total_size, code_begin] *)
    let ce = DUP3                   >>> ce in (* [total size, mem_start, total_size, code_begin, mem_start *)
    let ce = CODECOPY               >>> ce in (* [total size, mem_start] *)
    assert (start_size + 2 = stack_size ce) ; 
    ce





let cid_lookup_in_assoc (contracts:ty contract with_cid) (name:string) : cid =
    lookup_id (fun c -> c.contract_name = name) contracts


let setup_seed (le,ce) (loc:LI.stor_location) =
    let label       = get_new_label()   in
    let start_size  = stack_size ce     in
    let ce = PUSH4 (Int loc)        >>> ce in (* stack: [seed] *)
    let ce = PUSH4 (Label label)    >>> ce in
    let ce = JUMPI                  >>> ce in
    let ce = PUSH1 (Int 1)          >>> ce in (* stack: [1] *)
    let ce = SLOAD                  >>> ce in (* stack: [orig_seed] *)
    let ce = DUP1                   >>> ce in (* stack: [orig_seed, orig_seed] *)
    let ce = PUSH4 (Int loc)        >>> ce in (* stack: [orig_seed, orig_seed, loc] *)
    let ce = SSTORE                 >>> ce in (* stack: [orig_seed] *)
    let ce = incr_top ce 1                 in (* stack: [orig_seed + 1] *)
    let ce = PUSH1 (Int 1)          >>> ce in (* stack: [orig_seed + 1, 1] *)
    let ce = SSTORE                 >>> ce in
    let ce = JUMPDEST label         >>> ce in (* stack: [] *)
    assert (stack_size ce = start_size) ; 
    (le, ce)


let setup_array_seed_counter_to_one_if_not_initialized ce =
    let start_size  = stack_size ce     in
    let label       = get_new_label()   in
    let ce = PUSH1 (Int 1)          >>> ce in
    let ce = SLOAD                  >>> ce in
    let ce = PUSH4 (Label label)    >>> ce in
    let ce = JUMPI                  >>> ce in
    (* the mthd where it has to be changed *)
    let ce = PUSH1 (Int 1)          >>> ce in
    let ce = DUP1                   >>> ce in
    let ce = SSTORE                 >>> ce in
    let ce = JUMPDEST label         >>> ce in
    assert (stack_size ce = start_size) ; 
    ce




let setup_array_seeds le ce (contract: ty contract) : ce =
    let ce            = setup_array_seed_counter_to_one_if_not_initialized ce in
    let array_locs    = LI.array_locations contract in
    let _,ce          = L.fold_left setup_seed (le,ce) array_locs in
    ce




let codegen_constructor_bytecode ((contracts:ty contract with_cid),(cid:cid)) : (ce (* containing the program *) ) =
    let le      = constructor_initial_env cid (choose_contract cid contracts) in
    let ce      = empty_ce (cid_lookup_in_assoc contracts) contracts in
    let ce      = init_mem_alloc ce in
    (* implement some kind of fold function over the arg list
     * each step generates new (le,ce) *)
    let ce      = copy_args_from_code_to_mem le ce(choose_contract cid contracts) in (* stack: [arg_mem_size, arg_mem_begin] *)
    let ce:ce   = copy_args_from_mem_to_stor le ce cid in               (* stack: [] *)
    (* set up array seeds *)
    let ce:ce   = setup_array_seeds le ce (choose_contract cid contracts) in
    let ce      = set_contract_pc ce cid in                                     (* stack: [] *)
    let ce      = copy_runtime_code_to_mem ce cid in                            (* stack: [code_length, code_start_on_memory] *)
    RETURN >>> ce 



type constructor_compiled   =
                            { constructor_codegen_env : ce
                            ; constructor_interface : Contract.contract_interface
                            ; constructor_contract : ty contract
                            }

type runtime_compiled       =
                            { runtime_codegen_env : ce
                            ; runtime_contract_offsets : int with_cid
                            (* what form should the constructor code be encoded?
                               1. pseudo program.  easy
                               2. pseudo codegen_env.  maybe uniform
                             *)
                            }

let empty_runtime_compiled cid_lookup layouts =
    { runtime_codegen_env       = (empty_ce cid_lookup layouts)
    ; runtime_contract_offsets  = []
    }

let compile_constructor ((lst, cid) : (ty contract with_cid * cid)) : constructor_compiled =
    { constructor_codegen_env   = codegen_constructor_bytecode (lst, cid)
    ; constructor_interface     = Contract.contract_intf_of (L.assoc cid lst)
    ; constructor_contract      = L.assoc cid lst
    }

let compile_constructors (contracts:ty contract with_cid) : constructor_compiled with_cid =
    pair_map (fun cid _ -> compile_constructor (contracts, cid)) contracts

let initial_runtime_compiled (cid_lookup : string -> cid) layouts : runtime_compiled =
    let ce = empty_ce cid_lookup layouts in
    let ce = get_contract_pc    ce in
    let ce = JUMP           >>> ce in
    { runtime_codegen_env       = ce
    ; runtime_contract_offsets  = []
    }

let push_mthd_dest (ce:ce)(cid:cid)(mthd_hdr:mthd_head) =
    PUSH32(CaseOffsetInRuntimeCode(cid,mthd_hdr)) >>> ce 

let add_dispatcher_for_a_mthd_info le ce cid mthd_hdr =
    let start_size = stack_size ce in
    let ce = DUP1                       >>> ce in
    let ce = push_method ce mthd_hdr in
    let ce = EQ                         >>> ce in
    let ce = push_mthd_dest ce cid (Method mthd_hdr) in
    let ce = JUMPI                      >>> ce in
    assert (stack_size ce = start_size) ; 
    ce

let add_dispatcher_for_default_mthd le ce cid =
    let start_size = stack_size ce in
    let ce = push_mthd_dest ce cid Default in
    let ce = JUMP                       >>> ce in
    assert (stack_size ce = start_size) ;
    ce

let push_word_of_inputdata_at_byte ce b =
    let start_size = stack_size ce in
    let ce = PUSH32 b                   >>> ce in
    let ce = CALLDATALOAD               >>> ce in
    assert (stack_size ce = start_size + 1) ; 
    ce


let add_throw ce =
    (* Just using the same method as solc. *)
    let ce = PUSH1 (Int 2)              >>> ce in
    let ce = JUMP                       >>> ce in
    ce

let add_dispatcher le ce cid contract =
    let start_size = stack_size ce in

    (* load the first four bytes of the input data *)
    let ce = push_word_of_inputdata_at_byte ce (Int 0) in
    let ce = shiftR_top ce Eth.(word_bits - sig_bits) in
    let () = assert (stack_size ce = start_size + 1) in
    let mthd_signatures = L.map (fun x->x.mthd_head) contract.mthds in
    let mthd_infos = BL.filter_map 
                        (function   | Default   -> None 
                                    | Method u -> Some u) mthd_signatures in
    let ce = L.fold_left (fun ce mthd_signature ->
                            add_dispatcher_for_a_mthd_info le ce cid mthd_signature) ce mthd_infos in
    let ce = POP                        >>> ce  in (* the signature in input is not necessary anymore *)
    let ce =
      if L.exists (fun h -> match h with Default -> true | _ -> false) mthd_signatures then
        add_dispatcher_for_default_mthd le ce cid
      else add_throw ce
    in
    (le, ce)

let add_mthd_dest ce (cid : cid) (h : mthd_head) =
    let label   = get_new_label ()              in
    let ce = JUMPDEST label             >>> ce  in
    Entrypoint.(register_entrypoint(Case(cid,h))label) ; 
    ce


(** [prepare_words_on_stack le ce [arg0 arg1]] evaluates
 * [arg1] and then [arg0] and puts them onto the stack.
 * [arg0] will be the topmost element of the stack.
 *)
let prepare_words_on_stack le ce (args : ty expr list) =
    (le, L.fold_right (fun arg ce' -> codegen_expr le ce' R_ arg) args ce)

let store_word_into_stor_location (le, ce) (arg_location : LI.stor_location) =
    let ce = PUSH32 (Int arg_location)  >>> ce  in
    let ce = SSTORE                     >>> ce  in
    (le, ce)

(** [store_words_into_stor_locations le ce arg_locations] moves the topmost stack element to the
 *  location indicated by [arg_locations] and the next element to the next location and so on.
 *  The stack elements will disappear.
 *)
let store_words_into_stor_locations le ce arg_locations =
    L.fold_left store_word_into_stor_location (le, ce) arg_locations

let set_contract_args le ce offset cid (args : ty expr list) =
    let contract =  begin 
        try contract_lookup ce cid
        with e -> eprintf "set_contract_args: looking up %d\n" cid; raise e end in 
    let arg_locations : LI.stor_location list = LI.arg_locations offset contract  in
    assert (L.length arg_locations = L.length args) ; 
    let le,ce  = prepare_words_on_stack le ce args in
    let le,ce  = store_words_into_stor_locations le ce arg_locations in
    (* TODO: In a special case where no movements are necessary, we can then skip these args. *)
    (le, ce)

let set_continuation_to_function_call le ce layout (fcall, typ_expr) =
    let head : string = fcall.call_head in
    let args : ty expr list = fcall.call_args in
    let cid   =   begin
        try cid_lookup ce head
        with Not_found -> eprintf "contract of name %s not found\n%!" head;raise Not_found end in 
    let ce    =   set_contract_pc ce cid in
    let offset=   layout.LI.stor_constructor_args_begin cid in
    let le,ce =   begin 
        try set_contract_args le ce offset cid args
        with e ->   eprintf "name of contract: %s\n" head;
                    eprintf "set_continuation_to_function_call cid: %d\n" cid;raise e end in
    (le, ce)



(*
 * set_continuation sets the storage contents.
 * So that the next message call would start from the continuation.
 *)
let set_continuation le ce (layout : LI.stor_layout) (cont_expr, typ_expr) =
    let start_size = stack_size ce in
    let (le, ce) = match cont_expr with
        | FunctionCallExpr fcall -> set_continuation_to_function_call le ce layout (fcall, typ_expr)
        | _ -> err "strange_continuation" in
    let () = assert (stack_size ce = start_size) in
    (le, ce)

(*
 * Before this, the stack contains
 * ..., value (depends on typ).
 * The value would be stored in memory
 * After this, the stack contains
 * ..., size_in_bytes, offset_in_mem
 *)
let move_top_to_mem typ le ce =
    let () = assert (size_of_ty typ <= 32) in (* ..., value *)
    let ce = append_opcode ce (PUSH1 (Int 32)) in (* ..., value, 32 *)
    let ce = append_opcode ce DUP1 in   (* ..., value, 32, 32 *)
    let ce = mem_alloc ce in            (* ..., value, 32, addr *)
    let ce = append_opcode ce SWAP2 in  (* ..., addr, 32, value *)
    let ce = append_opcode ce DUP3 in   (* ..., addr, 32, value, addr *)
    let ce = append_opcode ce MSTORE in (* ..., addr, 32 *)
    let ce = append_opcode ce SWAP1 in  (* ..., 32, addr *)
    ce

(*
 * after this, the stack contains
 * ..., size, offset_in_mem
 *)
let place_expr_in_mem le ce packing ((e,ty):ty expr) =
    let start_size  = stack_size ce in
    let alignment   = match packing with
        | ABIPacking    -> R_
        | TightPacking  -> L_ in
    let ce = codegen_expr le ce alignment (e, ty) in
    assert (stack_size ce = 1 + start_size) ; 
    (* the stack layout depends on typ *)
    let ce = move_top_to_mem ty le ce in
    assert (stack_size ce = 2 + start_size) ; 
    le, ce

(*
 * When called on [a, b, c], a shoud occupy the smallest address, and c should occupy the largest address.
 * after this, the stack contains
 * ..., size, offset_in_mem
 *)
let rec place_exprs_in_mem le ce packing = function 
    | []        ->
                    let ce      = PUSH1(Int 0)                      >>> ce in
                    let ce      = PUSH1(Int 0)                      >>> ce in
                    le, ce
    | expr::rest ->
                    let le,ce   = place_expr_in_mem le ce packing expr in (* stack : [size, offset] *)
                    let ce      = SWAP1                             >>> ce in (* stack : [offset, size] *)
                    let le,ce   = place_exprs_in_mem le ce packing rest in (* this recursion is a bit awkward *) (* stack : [offset, size, size', offset] *)
                    let ce      = POP                               >>> ce in (* stack : [offset, size, size'] *)
                    let ce      = ADD                               >>> ce in (* stack : [offset, size_sum] *)
                    let ce      = SWAP1                             >>> ce in (* stack : [size_sum, offset] *)
                    le, ce


(*
 * return_mem_content assumes the stack left after place_expr_in_mem
 * ..., size, offset_in_mem
 *)
let return_mem_content le ce = RETURN >>> ce 

let add_return le ce (layout : LI.stor_layout) ret =
    let start_size = stack_size ce      in
    let e = ret.ret_expr              in
    let c = ret.ret_cont             in
    let (le, ce) = set_continuation le ce layout c in
    let ce = match e with
      | Some e  ->
                    let (le, ce) = place_expr_in_mem le ce ABIPacking e in
                    return_mem_content le ce
      | None    ->
                    append_opcode ce STOP in
    assert (stack_size ce = start_size) ; 
    (le, ce)

let put_stacktop_into_array_access le ce layout (aa : ty array_access) =
    let array = aa.array_access_array in
    let index = aa.array_access_index in
    let ce = codegen_expr le ce R_ index in    (* stack : [value, index] *)
    let ce = codegen_expr le ce R_ array in    (* stack : [value, index, array_seed] *)
    let ce = keccak_cons le ce in   (* stack : [value, kec(array_seed ^ index)] *)
    let ce = append_opcode ce SSTORE in
    ce

let put_stacktop_into_lexpr le ce layout l =
    let start_size = stack_size ce in
    let ce  = match l with
        | ArrayAccessLExpr aa -> put_stacktop_into_array_access le ce layout aa in
    assert (start_size = stack_size ce + 1) ; 
    ce

let add_assign le ce layout l r =
    let start_size = stack_size ce in
    (* produce r on the stack and then think about where to put that *)
    let ce = codegen_expr le ce R_ r in
    assert (1 + start_size = stack_size ce) ; 
    let ce = put_stacktop_into_lexpr le ce layout l in
    assert (start_size = stack_size ce) ; 
    (le, ce)

let push_event ce event =
    let hash    = Eth.event_signature_hash event in
    let ce      = PUSH4 (Big (Eth.hex_to_big_int hash)) >>> ce in
    ce

let add_var_declare le ce layout i =
    let pos     = stack_size ce in
    let ce      = codegen_expr le ce R_ i.var_declare_value in
    let name    = i.var_declare_name in
    let loc     = Loc.Stack (pos + 1) in
    let le      = add_pair le name loc in
    (le, ce)






let rec add_if_single le ce (layout : LI.stor_layout) cond body =
    let label       = get_new_label () in
    let start_size  = stack_size ce in
    let ce      = codegen_expr le ce R_ cond in
    let ce      = ISZERO                    >>> ce  in
    let ce      = PUSH4(Label label)        >>> ce  in 
    let ce      = JUMPI                     >>> ce  in
    let le,ce   = add_stmts le ce layout body       in
    let ce      = JUMPDEST label            >>> ce  in
    assert (start_size = stack_size ce) ; 
    le,ce

and add_if le ce (layout : LI.stor_layout) cond bodyT bodyF =
    let label       = get_new_label()   in
    let next        = get_new_label()   in
    let start_size  = stack_size ce     in
    let ce      = codegen_expr le ce R_ cond in
    let ce      = ISZERO                    >>> ce in
    let ce      = PUSH4(Label label)        >>> ce in
    let ce      = JUMPI                     >>> ce in
    let _,ce    = add_stmts le ce layout bodyT     in (* location env needs to be discarded *)
    let ce      = PUSH4(Label next)         >>> ce in
    let ce      = JUMP                      >>> ce in
    let ce      = JUMPDEST label            >>> ce in
    let _,ce    = add_stmts le ce layout bodyF     in (* location env needs to be discarded *)
    let ce      = JUMPDEST next             >>> ce in
    assert (start_size = stack_size ce) ; 
    le,ce

and add_stmts le ce layout ss =
  L.fold_left (fun (le, ce) s -> add_stmt le ce layout s) (le, ce) ss

and add_stmt le ce (layout : LI.stor_layout) = function 
    | AbortStmt                         -> (le, add_throw ce)
    | ReturnStmt ret                    -> add_return           le ce layout ret
    | AssignStmt (l,r)                  -> add_assign           le ce layout l r
    | VarDeclStmt i                -> add_var_declare    le ce layout i
    | IfThenOnly(cond,body)             -> add_if_single        le ce layout cond body (* this is a special case of the next *)
    | IfThenElse(cond,bodyT,bodyF)      -> add_if               le ce layout cond bodyT bodyF
    | SelfDestructStmt expr              -> add_self_destruct    le ce layout expr
    | ExprStmt expr                       -> add_expr_stmt         le ce layout expr
    | LogStmt(name,args,Some event)     -> add_log_stmt         le ce layout name args event
    | LogStmt(name,args,None)           -> err "add_stmt: type check first"

and add_log_stmt le ce layout name (args : ty expr list) event =
    let start_size = stack_size ce in
    (* get the indexed *)
    let indexed_args,non_indexed_args = split_event_args event args in
    (* prepare indexed args on the stack *)
    let le,ce   = prepare_words_on_stack le ce indexed_args in
    (* prepare the event signature on the stack *)
    let ce      = push_event ce event in
    (* prepare non-indexed args in the memory *)
    let le,ce   = place_exprs_in_mem le ce ABIPacking non_indexed_args in (* stack : [..., size, offset] *)
    let n       = L.length indexed_args + 1 in
    let ce      = log n >>> ce  in
    (* decide N in logN *)
    assert (stack_size ce = start_size) ;
    le, ce

and add_expr_stmt le ce layout expr =
    let ce = codegen_expr le ce R_ expr in
    let ce = append_opcode ce POP in
    le, ce

and add_self_destruct le ce layout expr =
    let ce = codegen_expr le ce R_ expr in
    let ce = append_opcode ce SELFDESTRUCT in
    le, ce













let add_mthd_arg_locations (le : le) (mthd : ty mthd) =
  let additions : (string * Loc.location) list = Eth.args_with_locations mthd in
  let ret = add_pairs le additions in
  ret

let calldatasize_of_usual_header us =
    let args = us.mthd_args in
    4 (* for signature *) +
        try     BL.sum (L.map (fun x -> Eth.(intf_ty_size(intf_ty_of_ty x.ty)))args) 
        with    Invalid_argument _ -> 0

let add_mthd_arg_len_chk ce = function 
    | Default       -> (* no check, the choice is arguable *) ce
    | Method us     ->
     let ce = PUSH4(Int(calldatasize_of_usual_header us))   >>> ce in
     let ce = CALLDATASIZE                                  >>> ce in
     let ce = EQ                                            >>> ce in
     let ce = ISZERO                                        >>> ce in
     let ce = PUSH1(Int 0)                                  >>> ce in
     let ce = JUMPI                                         >>> ce in
     ce

let add_mthd (le:le) (ce:ce) layout (cid:cid) (mthd:ty mthd) =
    let ce = add_mthd_dest ce cid mthd.mthd_head in
    let ce = add_mthd_arg_len_chk ce mthd.mthd_head in
    let le = add_empty_block le in
    let le = add_mthd_arg_locations le mthd in
    let ((le:le),ce) = L.fold_left (fun((le:le),ce)stmt->add_stmt le ce layout stmt)(le,ce)mthd.mthd_body in
    (le, ce)

let codegen_append_contract_bytecode le ce layout((cid,contract):cid*ty contract) =
    (* jump destination for the contract *)
    let entry_label   = get_new_label () in
    let ce            = append_opcode ce (JUMPDEST entry_label) in
    (* update the entrypoint database with (id, pc) pair *)
    let ()            = Entrypoint.(register_entrypoint (Contract cid) entry_label) in
    let ce            = init_mem_alloc ce in
    (* add jumps to the mthds *)
    let (le, ce)      = add_dispatcher le ce cid contract in
    (* add the mthds *)
    let mthds         = contract.mthds in
    let (le, ce)      = L.fold_left (fun (le,ce)mthd -> add_mthd le ce layout cid mthd)(le, ce) mthds in
    ce

let append_runtime layout (prev : runtime_compiled)
                   ((cid : cid), (contract : ty contract))
                   : runtime_compiled =
  { runtime_codegen_env = codegen_append_contract_bytecode (runtime_initial_env contract) prev.runtime_codegen_env layout (cid, contract)
  ; runtime_contract_offsets = insert cid (code_length prev.runtime_codegen_env) prev.runtime_contract_offsets
  }

let compile_runtime layout (contracts : ty contract with_cid) : runtime_compiled = 
    L.fold_left (append_runtime layout) (initial_runtime_compiled (cid_lookup_in_assoc contracts) contracts) contracts

let stor_layout_from_constructor_compiled (cc : constructor_compiled) : LI.contract_stor_layout =
    LI.stor_layout_of_contract cc.constructor_contract (extract_program cc.constructor_codegen_env)

let sizes_of_constructors (constructors : constructor_compiled with_cid) : int list =
    let lengths = map (fun cc -> code_length cc.constructor_codegen_env) constructors in
    let lengths = L.sort (fun a b -> compare (fst a) (fst b)) lengths in
    L.map snd lengths

let rec calculate_offsets_inner ret current lst = match lst with
    | [] -> L.rev ret
    | hd::tl ->
       (* XXX: fix the append *)
       calculate_offsets_inner (current :: ret) (current + hd) tl

let calculate_offsets initial lst   = calculate_offsets_inner [] initial lst

let stor_layout_from_runtime_compiled (rc:runtime_compiled) (constructors:constructor_compiled with_cid) : LI.runtime_stor_layout =
    let sizes_of_constructors       = sizes_of_constructors constructors in
    let offsets_of_constructors     = calculate_offsets (code_length rc.runtime_codegen_env) sizes_of_constructors in
    let sum_of_constructor_sizes    = BL.sum sizes_of_constructors in
    LI.(
        { runtime_code_size = sum_of_constructor_sizes + code_length rc.runtime_codegen_env
        ; runtime_offset_of_cid = rc.runtime_contract_offsets
        ; runtime_size_of_constructor = fold_with_cid sizes_of_constructors
        ; runtime_offset_of_constructor = fold_with_cid offsets_of_constructors
        })

let concat_programs_rev (programs : 'imm Evm.program list) =
    let rev_programs = L.rev programs in
    L.concat rev_programs

(** constructors_packed concatenates constructor code.
 *  Since the code is stored in the reverse order, the concatenation is also reversed.
 *)
let constructors_packed layout (constructors : constructor_compiled with_cid) =
    let programs            = map (fun cc -> extract_program cc.constructor_codegen_env) constructors in
    let programs            = L.sort (fun a b -> compare (fst a) (fst b)) programs in
    let programs            = L.map snd programs in
    concat_programs_rev programs

let compose_bytecode (constructors : constructor_compiled with_cid)
                     (runtime : runtime_compiled) (cid : cid) : big_int Evm.program =
    let contracts_stor_layout : (cid * LI.contract_stor_layout) list =
      L.map (fun (id, const) -> (id, stor_layout_from_constructor_compiled const)) constructors in
    let runtime_layout      = stor_layout_from_runtime_compiled runtime constructors in
    let layout              = LI.construct_post_stor_layout contracts_stor_layout runtime_layout in
    let pseudo_constructor  = choose_contract cid constructors in
    let imm_constructor     = LI.realize_program layout cid (extract_program pseudo_constructor.constructor_codegen_env) in
    let pseudo_runtime_core = extract_program runtime.runtime_codegen_env in
    (* XXX: This part is somehow not modular. *)
    (* Sicne the code is stored in the reverse order, the concatenation is also reversed. *)
    let imm_runtime         = LI.realize_program layout cid ((constructors_packed layout constructors)@pseudo_runtime_core) in
    (* the code is stored in the reverse order *)
    imm_runtime@imm_constructor

let compose_runtime_bytecode (constructors : constructor_compiled with_cid)
                     (runtime : runtime_compiled) : big_int Evm.program =
    let contracts_stor_layout : (cid * LI.contract_stor_layout) list =
      L.map (fun (id, const) -> (id, stor_layout_from_constructor_compiled const)) constructors in
    let runtime_layout  = stor_layout_from_runtime_compiled runtime constructors in
    let layout          = LI.construct_post_stor_layout contracts_stor_layout runtime_layout in
    (* TODO: 0 in the next line is a bit ugly. *)
    let imm_runtime     = LI.realize_program layout 0 ((constructors_packed layout constructors)@(extract_program runtime.runtime_codegen_env)) in
    imm_runtime
