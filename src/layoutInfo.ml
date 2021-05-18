open Big_int
open Syntax
open ContractId
open Imm
open Printf 

module Eth  = Ethereum
module BL   = BatList
module L    = List

type stor_location = int
(* Storage Layout that should be available after the constructor compilation finishes *)
type stor_layout            =
                            { cids                        : cid list
                            ; constructor_code_size       : cid -> int
                              (* numbers about the storage *)
                              (* The storage during the runtime looks like this: *)
                              (* |current pc (might be entry_pc_of_current_contract)
                               * |array seed counter
                               * |pod contract arg0
                               * |pod contract arg1
                               * |pod contract arg2
                               * | ...
                                 |array0's seed
                                 |array1's seed
                                 | ... 
                                 | *)
                              (* In addition, array elements are placed at the same location as in Solidity *)
                            ; stor_current_pc_index       : int
                            ; stor_array_counter_index    : int
                            ; stor_constructor_args_begin : cid -> int
                            ; stor_constructor_args_size  : cid -> int
                            ; stor_array_seeds_begin      : cid -> int
                            ; stor_array_seeds_size       : cid -> int
                            }

(* Storage Layout that should be available after the runtime compilation finishes. *)
type post_stor_layout       =
                            { (* The initial data is organized like this: *)
                              (* |constructor code|runtime code|constructor args|  *)
                              init_data_size : cid -> int
                              (* runtime_coode_offset is equal to constructor_code_size *)
                            ; runtime_code_size : int
                            ; contract_offset_in_runtime_code : int with_cid
                            (* And then, the runtime code is organized like this: *)
                            (* |dispatcher that jumps into the stored pc|runtime code for contract A|runtime code for contract B|runtime code for contract C| *)

                            ; constructor_in_runtime_code_offset : int with_cid

                            (* And then, the runtime code for a particular contract is organized like this: *)
                            (* |dispatcher that jumps into a case|runtime code for case f|runtime code for case g| *)
                            ; l : stor_layout
                            }


let pr_stor_layout l        =   printf "stor_layout\n";
                                printf "  init_data_size:\n"

type contract_stor_layout   =
                            { contract_constructor_code_size  : int
                            ; contract_arg_size               : int (** the number of words that the contract args occupy *)
                            ; contract_num_array_seeds        : int (** the number of args that arrays *)
                            ; contract_args                   : ty list (** the list of arg types *)
                            }

type runtime_stor_layout    =
                            { runtime_code_size               : int
                            ; runtime_offset_of_cid           : int with_cid
                            ; runtime_offset_of_constructor   : int with_cid
                            ; runtime_size_of_constructor     : int with_cid
                            }

let compute_constructor_code_size lst cid =
    let c : contract_stor_layout = choose_contract cid lst in
    c.contract_constructor_code_size

let compute_constructor_args_size lst cid =
    let c : contract_stor_layout = choose_contract cid lst in
    c.contract_arg_size

let compute_constructor_args_begin lst runtime cid =
    compute_constructor_code_size lst cid + runtime.runtime_code_size

let compute_init_data_size lst runtime cid =
    compute_constructor_args_begin lst runtime cid + compute_constructor_args_size lst cid

let compute_stor_constructor_args_begin lst cid = 2

let compute_stor_array_seeds_begin lst cid = 
    compute_stor_constructor_args_begin lst cid + compute_constructor_args_size lst cid

let compute_stor_array_seeds_size lst cid =
    let c = choose_contract cid lst in
    c.contract_num_array_seeds

let construct_stor_layout(lst:(cid*contract_stor_layout)list) : stor_layout =
  { cids                        = L.map fst lst
  ; constructor_code_size       = compute_constructor_code_size lst
  ; stor_current_pc_index       = 0 (* This is a magic constant. *)
  ; stor_array_counter_index    = 1 (* This is also a magic constant. *)
  ; stor_constructor_args_begin = compute_stor_constructor_args_begin lst
  ; stor_constructor_args_size  = compute_constructor_args_size lst
  ; stor_array_seeds_begin      = compute_stor_array_seeds_begin lst
  ; stor_array_seeds_size       = compute_stor_array_seeds_size lst
  }

let construct_post_stor_layout (lst:(cid*contract_stor_layout)list)
      (runtime:runtime_stor_layout) : post_stor_layout =
  { init_data_size                      = compute_init_data_size lst runtime
  ; runtime_code_size                   = runtime.runtime_code_size
  ; contract_offset_in_runtime_code     = runtime.runtime_offset_of_cid
  ; l                                   = construct_stor_layout lst
  ; constructor_in_runtime_code_offset  = runtime.runtime_offset_of_constructor
  }

(* Assuming the layout described above, this definition makes sense. *)
let runtime_code_offset (layout:stor_layout) (cid:cid) : int =
  layout.constructor_code_size cid

let rec realize_imm(layout:post_stor_layout)(initial_cid:cid) = 
    let big = big_int_of_int in function 
  | Big b                                   ->  b
  | Int i                                   ->  big i
  | Label l                             ->  big (Label.lookup_value l)
  | StoragePCIndex              ->  big (layout.l.stor_current_pc_index)
  | StorageConstructorArgumentsBegin cid    ->  big (layout.l.stor_constructor_args_begin cid)
  | StorageConstructorArgumentsSize cid     ->  big (layout.l.stor_constructor_args_size cid)
  | InitDataSize cid                        ->  big (layout.init_data_size cid)
  | RuntimeCodeOffset cid                   ->  big (runtime_code_offset layout.l cid)
  | RuntimeCodeSize                         ->  big (layout.runtime_code_size)
  | ConstructorCodeSize cid                 ->  big (layout.l.constructor_code_size cid)
  | ConstructorInRuntimeCodeOffset cid      ->  big (choose_contract cid layout.constructor_in_runtime_code_offset)
  | ContractOffsetInRuntimeCode cid         ->  big (choose_contract cid layout.contract_offset_in_runtime_code)
  | CaseOffsetInRuntimeCode(cid,mthd_head)->  let label = Entrypoint.(lookup_entrypoint (Case (cid, mthd_head))) in
                                                big (Label.lookup_value label)
  | Minus (a, b)                            ->  sub_big_int (realize_imm layout initial_cid a) (realize_imm layout initial_cid b)

let realize_opcode (l : post_stor_layout) (initial_cid : cid) (i : imm Evm.opcode) = Evm.(
  match i with
  | PUSH1  imm      -> PUSH1  (realize_imm l initial_cid imm)
  | PUSH4  imm      -> PUSH4  (realize_imm l initial_cid imm)
  | PUSH32 imm      -> PUSH32 (realize_imm l initial_cid imm)
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
  | SELFDESTRUCT         -> SELFDESTRUCT
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

let realize_program (l : post_stor_layout) (initial_cid : cid) (p : imm Evm.program) : big_int Evm.program
  = L.map (realize_opcode l initial_cid) p

let stor_layout_of_contract (c : ty contract) (constructor_code : imm Evm.program) =
  { contract_constructor_code_size  = Evm.size_of_program constructor_code
  ; contract_arg_size               = Eth.total_size_of_intf_args (L.map snd (Eth.constructor_args c))
  ; contract_num_array_seeds        = L.length (Eth.arrays_in_contract c)
  ; contract_args                   = L.map (fun a -> a.ty) (c.contract_args)
  }

let rec arg_locations_inner (offset : int) (used_plain_args : int) (used_mapping_seeds : int)
                            (num_of_plains : int)
                            (args : ty list) : stor_location list =
  match args with
  | []          -> []
  | h :: t      -> if is_mapping h 
    then (offset + num_of_plains + used_mapping_seeds) ::
         arg_locations_inner offset used_plain_args (used_mapping_seeds + 1) num_of_plains t
    else (offset + used_plain_args) ::
         arg_locations_inner offset (used_plain_args + 1) used_mapping_seeds num_of_plains t


(* this needs to take stor_constructor_args_begin *)
let arg_locations (offset : int) (cntr : ty contract) : stor_location list =
  let arg_types     = L.map (fun a -> a.ty) cntr.contract_args in
  assert (L.for_all fits_in_one_stor_slot arg_types) ; 
  let num_of_plains = count_plain_args arg_types in
  let ret           = arg_locations_inner offset 0 0 num_of_plains arg_types in 
  ret 

let array_locations (cntr : ty contract) : stor_location list =
  let arg_types = L.map (fun a -> a.ty) cntr.contract_args in
  let () = assert (L.for_all fits_in_one_stor_slot arg_types) in
  let num_of_plains = count_plain_args arg_types in
  let total_num = L.length arg_types in
  if total_num = num_of_plains 
      then []
      else BL.(range (2 + num_of_plains) `To (total_num + 1))
