open Big_int
open Syntax
open ContractId
open Imm
open Printf 

module Eth  = Ethereum
module BL   = BatList
module L    = List

type stor_location = int

(* Stor Layout that should be available after the cnstrctr compilation finishes *)
type stor_layout            =
                            { idxs                      : idx list
                            ; cnstrctr_code_size        : idx -> int
                              (* numbers about the storage *)
                              (* The storage during the runtime looks like this: *)
                              (* |S[0]  := PROGRAM COUNTER     (might be entry_pc_of_current_cntrct)
                               * |S[1]  := ARRAY SEED COUNTER  
                               * |S[2]  := pod cntrct arg0
                               * |S[3]  := pod cntrct arg1
                               * |S[4]  := pod cntrct arg2
                               * | ...
                                 |S[n]  := array0's seed
                                 |S[n+1]:= array1's seed
                                 | ... 
                                 | *)
                              (* In addition, array elements are placed at the same location as in Solidity *)
                            ; stor_current_pc_index     : int
                            ; stor_array_counter_index  : int
                            ; stor_cnstrctr_args_begin  : idx -> int
                            ; stor_cnstrctr_args_size   : idx -> int
                            ; stor_array_seeds_begin    : idx -> int
                            ; stor_array_seeds_size     : idx -> int
                            }

(* Stor Layout that should be available after the runtime compilation finishes. *)
type post_stor_layout       =
                            { (* The initial data is organized like this: *)
                              (* |cnstrctr code
                               * |runtime  code
                               * |cnstrctr args|  *)
                              init_data_size            : idx -> int
                              (* runtime_coode_offset is equal to cnstrctr_code_size *)
                            ; runtime_code_size         : int
                            ; cntrct_offset_in_runtime_code : int idx_list
                            (* And then, the runtime code is organized like this: *)
                            (* |dispatcher that jumps into the stored pc
                             * |runtime code for cntrct A
                             * |runtime code for cntrct B
                             * |runtime code for cntrct C| *)

                            ; cnstrctr_in_runtime_code_offset : int idx_list

                            (* And then, the runtime code for a particular cntrct is organized like this: *)
                            (* |dispatcher that jumps into a method
                             * |runtime code for method f
                             * |runtime code for method g| *)
                            ; l : stor_layout
                            }


let pr_stor_layout l        =   printf "stor_layout\n";
                                printf "  init_data_size:\n"


type cntrct_stor_layout   =
                            { cntrct_cnstrctr_code_size     : int
                            ; cntrct_arg_size               : int (** the number of words that the cntrct args occupy *)
                            ; cntrct_num_array_seeds        : int (** the number of args that arrays *)
                            ; cntrct_args                   : ty list (** the list of arg types *)
                            }


type runtime_stor_layout    =
                            { runtime_code_size             : int
                            ; runtime_offset_of_idx         : int idx_list
                            ; runtime_offset_of_cnstrctr    : int idx_list
                            ; runtime_size_of_cnstrctr      : int idx_list
                            }


let compute_cnstrctr_code_size lst idx =
    let c : cntrct_stor_layout = choose_cntrct idx lst in
    c.cntrct_cnstrctr_code_size


let compute_cnstrctr_args_size lst idx =
    let c : cntrct_stor_layout = choose_cntrct idx lst in
    c.cntrct_arg_size

let compute_cnstrctr_args_begin lst runtime idx =
    compute_cnstrctr_code_size lst idx + runtime.runtime_code_size

let compute_init_data_size lst runtime idx =
    compute_cnstrctr_args_begin lst runtime idx + compute_cnstrctr_args_size lst idx

let compute_stor_cnstrctr_args_begin lst idx = 2

let compute_stor_array_seeds_begin lst idx = 
    compute_stor_cnstrctr_args_begin lst idx + compute_cnstrctr_args_size lst idx

let compute_stor_array_seeds_size lst idx =
    let c = choose_cntrct idx lst in
    c.cntrct_num_array_seeds

let cnstrct_stor_layout(lst:(idx*cntrct_stor_layout)list) : stor_layout =
    { idxs                      = L.map fst lst
    ; cnstrctr_code_size        = compute_cnstrctr_code_size lst
    ; stor_current_pc_index     = 0 (* This is a magic const. *)
    ; stor_array_counter_index  = 1 (* This is also a magic const. *)
    ; stor_cnstrctr_args_begin  = compute_stor_cnstrctr_args_begin lst
    ; stor_cnstrctr_args_size   = compute_cnstrctr_args_size lst
    ; stor_array_seeds_begin    = compute_stor_array_seeds_begin lst
    ; stor_array_seeds_size     = compute_stor_array_seeds_size lst
    }

let cnstrct_post_stor_layout (lst:(idx*cntrct_stor_layout)list)
      (runtime:runtime_stor_layout) : post_stor_layout =
    { init_data_size                        = compute_init_data_size lst runtime
    ; runtime_code_size                     = runtime.runtime_code_size
    ; cntrct_offset_in_runtime_code         = runtime.runtime_offset_of_idx
    ; l                                     = cnstrct_stor_layout lst
    ; cnstrctr_in_runtime_code_offset       = runtime.runtime_offset_of_cnstrctr
    }

(* Assuming the layout described above, this definition makes sense. *)
let runtime_code_offset (layout:stor_layout) idx : int =
    layout.cnstrctr_code_size idx

let rec realize_imm(layout:post_stor_layout)(init_idx:idx) = 
    let big = big_int_of_int in function 
    | Big b                                 ->  b
    | Int i                                 ->  big i
    | Label l                               ->  big (Label.lookup_value l)
    | StorPCIndex                           ->  big (layout.l.stor_current_pc_index)
    | StorCnstrctrArgsBegin       idx       ->  big (layout.l.stor_cnstrctr_args_begin idx)
    | StorCnstrctrArgsSize        idx       ->  big (layout.l.stor_cnstrctr_args_size idx)
    | InitDataSize                idx       ->  big (layout.init_data_size idx)
    | RuntimeCodeOffset           idx       ->  big (runtime_code_offset layout.l idx)
    | RuntimeCodeSize                       ->  big (layout.runtime_code_size)
    | CnstrctrCodeSize            idx       ->  big (layout.l.cnstrctr_code_size idx)
    | CnstrctrInRuntimeCodeOffset idx       ->  big (choose_cntrct idx layout.cnstrctr_in_runtime_code_offset)
    | CntrctOffsetInRuntimeCode   idx       ->  big (choose_cntrct idx layout.cntrct_offset_in_runtime_code)
    | MthdAddrInRuntimeCode(idx,mthd_hd)  ->  let label = Entrypoint.(lookup_entrypoint (Case (idx, mthd_hd))) in
                                                big (Label.lookup_value label)
    | Minus (a, b)                          ->  sub_big_int (realize_imm layout init_idx a) (realize_imm layout init_idx b)

let realize_opcode (l : post_stor_layout) (init_idx : idx) (i : imm Evm.opcode) = Evm.(
    match i with
    | PUSH1  imm      -> PUSH1  (realize_imm l init_idx imm)
    | PUSH4  imm      -> PUSH4  (realize_imm l init_idx imm)
    | PUSH32 imm      -> PUSH32 (realize_imm l init_idx imm)
    | NOT             -> NOT
    | TIMESTAMP       -> TIMESTAMP
    | EQ              -> EQ
    | ISZERO          -> ISZERO
    | LT              -> LT
    | GT              -> GT
    | BALANCE         -> BALANCE
    | STOP            -> STOP
    | ADD             -> ADD
    | MUL             -> MUL
    | SUB             -> SUB
    | DIV             -> DIV
    | SDIV            -> SDIV
    | MOD             -> MOD
    | SMOD            -> SMOD
    | ADDMOD          -> ADDMOD
    | MULMOD          -> MULMOD
    | EXP             -> EXP
    | SIGNEXTEND      -> SIGNEXTEND
    | SHA3            -> SHA3
    | ADDRESS         -> ADDRESS
    | ORIGIN          -> ORIGIN
    | CALLER          -> CALLER
    | CALLVALUE       -> CALLVALUE
    | CALLDATALOAD    -> CALLDATALOAD
    | CALLDATASIZE    -> CALLDATASIZE
    | CALLDATACOPY    -> CALLDATACOPY
    | CODESIZE        -> CODESIZE
    | CODECOPY        -> CODECOPY
    | GASPRICE        -> GASPRICE
    | EXTCODESIZE     -> EXTCODESIZE
    | EXTCODECOPY     -> EXTCODECOPY
    | POP             -> POP
    | MLOAD           -> MLOAD
    | MSTORE          -> MSTORE
    | MSTORE8         -> MSTORE8
    | SLOAD           -> SLOAD
    | SSTORE          -> SSTORE
    | JUMP            -> JUMP
    | JUMPI           -> JUMPI
    | PC              -> PC
    | MSIZE           -> MSIZE
    | GAS             -> GAS
    | JUMPDEST l      -> JUMPDEST l
    | LOG0            -> LOG0
    | LOG1            -> LOG1
    | LOG2            -> LOG2
    | LOG3            -> LOG3
    | LOG4            -> LOG4
    | CREATE          -> CREATE
    | CALL            -> CALL
    | CALLCODE        -> CALLCODE
    | RETURN          -> RETURN
    | DELEGATECALL    -> DELEGATECALL
    | SELFDESTRUCT    -> SELFDESTRUCT
    | SWAP1           -> SWAP1
    | SWAP2           -> SWAP2
    | SWAP3           -> SWAP3
    | SWAP4           -> SWAP4
    | SWAP5           -> SWAP5
    | SWAP6           -> SWAP6
    | DUP1            -> DUP1
    | DUP2            -> DUP2
    | DUP3            -> DUP3
    | DUP4            -> DUP4
    | DUP5            -> DUP5
    | DUP6            -> DUP6
    | DUP7            -> DUP7
    )

let realize_program l init_idx p = L.map (realize_opcode l init_idx) p

let stor_layout_of_cntrct (cn:ty cntrct) (cnstrctr_code : imm Evm.program) =
    { cntrct_cnstrctr_code_size = Evm.size_of_program cnstrctr_code
    ; cntrct_arg_size           = Eth.total_size_of_tyArgs (L.map snd (Eth.cnstrctr_args cn))
    ; cntrct_num_array_seeds    = L.length (Eth.arrays_in_cntrct cn)
    ; cntrct_args               = L.map (fun a->a.ty) (cn.cntrct_args)
    }

let rec arg_locations_inner offset used_plain_args used_mapping_seeds num_of_plains = function 
    | []    ->  []
    | h::t  ->  if is_mapping h 
                    then (offset + num_of_plains + used_mapping_seeds) ::
                        arg_locations_inner offset used_plain_args(used_mapping_seeds+1)num_of_plains t
                    else (offset + used_plain_args) ::
                        arg_locations_inner offset(used_plain_args+1)used_mapping_seeds num_of_plains t


(* this needs to take stor_cnstrctr_args_begin *)
let arg_locations offset (cn:ty cntrct) : stor_location list =
    let arg_tys       = L.map (fun a->a.ty) cn.cntrct_args in
    assert (L.for_all fits_in_one_stor_slot arg_tys) ; 
    let num_of_plains = count_plain_args arg_tys  in
    let ret           = arg_locations_inner offset 0 0 num_of_plains arg_tys in 
    ret 

let array_locations (cn:ty cntrct) : stor_location list =
    let arg_tys       = L.map (fun a->a.ty) cn.cntrct_args in
    assert (L.for_all fits_in_one_stor_slot arg_tys) ;
    let num_of_plains = count_plain_args arg_tys  in
    let total_num     = L.length arg_tys          in
    if total_num=num_of_plains 
        then []
        else BL.(range (2 + num_of_plains) `To (total_num + 1))

