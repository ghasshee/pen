open Big_int
open Printf 
open Misc
open Syntax
open Label
open IndexedList
open Imm

module Eth  = Ethereum
module BL   = BatList
module L    = List

type stor_addr = int

(* Stor Layout that should be available after the cnstrctr compilation finishes *)
type stor_layout            =
                            { stor_program_counter      : int
                            ; stor_array_counter        : int
                            ; idxs                      : idx list
                            ; cnstrctr_code_size        : idx -> int
                            ; stor_cnstrctr_args_begin  : idx -> int
                            ; stor_cnstrctr_args_size   : idx -> int
                            ; stor_array_seeds_begin    : idx -> int
                            ; stor_array_seeds_size     : idx -> int
                              (* numbers about the storage *)
                              (* The storage during the rntime looks like this: *)
                              (* |S[0]  := PROGRAM COUNTER     
                               * |S[1]  := ARRAY SEED COUNTER  
                               * |S[2]  := pod cntrct arg0
                               * |S[3]  := pod cntrct arg1
                               * |S[4]  := pod cntrct arg2
                               * | ...
                               * |S[k+1]:= pod cntrct argk-1
                                 |S[k+2]:= array0's seed
                                 | ...
                                 |S[n+1]:= arraym's seed
                                 | *)
                              (* In addition, array elements are placed at the same location as in Solidity *)
                            }

(* Stor Layout that should be available after the rntime compilation finishes. *)
(* rntime_code_offset = cnstrctr_code_size *)

(* the rntime code is organized like this:                                                                *)
(* +-----------------------------------------+          the rntime code for a particular cntrct           *)
(* |dispatcher that jumps into the stored pc |        +-------------------------------------+             *)
(* |rntime code for cntrct A   ---------------------->|dispatcher that jumps into a method  |             *)
(* |rntime code for cntrct B                 |        |rntime code for method f             |             *)
(* |rntime code for cntrct C                 |        |rntime code for method g|            |             *)
(* +-----------------------------------------+        +-------------------------------------+             *)
type post_stor_layout       =                                                   (* The initial data is organized like this: *)
                            { init_data_size            : idx -> int            (* |cnstrctr code                           *)          
                            ; rntime_code_size          : int                   (* |rntime  code                            *)
                            ; rntime_cntrct_offsets     : int idx_list          (* |cnstrctr args|                          *)     
                            ; rntime_cnstrctr_offsets   : int idx_list
                            ; l : stor_layout
                            }


let pr_stor_layout l        =   printf "stor_layout\n";
                                printf "  init_data_size:\n"


type cntrct_stor_layout     =
                            { cntrct_cnstrctr_code_size     : int
                            ; cntrct_arg_size               : int (** the number of words that the cntrct args occupy *)
                            ; cntrct_num_array_seeds        : int (** the number of args that arrays *)
                            ; cntrct_args                   : ty list (** the list of arg types *)
                            }


type rntime_stor_layout     =
                            { rntime_code_size             : int
                            ; rntime_offset_of_idx         : int idx_list
                            ; rntime_offset_of_cnstrctr    : int idx_list
                            ; rntime_size_of_cnstrctr      : int idx_list
                            }





let compute_cnstrctr_code_size l idx        =
    let c : cntrct_stor_layout = lookup_index idx l in
    c.cntrct_cnstrctr_code_size

let compute_cnstrctr_args_size l idx        =
    let c : cntrct_stor_layout = lookup_index idx l in
    c.cntrct_arg_size

let compute_cnstrctr_args_begin l rntime idx =
    compute_cnstrctr_code_size l idx + rntime.rntime_code_size

let compute_init_data_size l rntime idx     =
    compute_cnstrctr_args_begin l rntime idx + compute_cnstrctr_args_size l idx

let compute_stor_cnstrctr_args_begin        = konst 2

let compute_stor_array_seeds_begin l idx    = 
    compute_stor_cnstrctr_args_begin idx + compute_cnstrctr_args_size l idx

let compute_stor_array_seeds_size lst idx   =
    let c = lookup_index idx lst in
    c.cntrct_num_array_seeds

let cnstrct_stor_layout(l:cntrct_stor_layout idx_list) : stor_layout =
    { idxs                              = L.map fst l
    ; cnstrctr_code_size                = compute_cnstrctr_code_size l
    ; stor_program_counter              = 0     (* fixed constant. *)
    ; stor_array_counter                = 1     (* fixed constant. *) 
    ; stor_cnstrctr_args_begin          = compute_stor_cnstrctr_args_begin
    ; stor_cnstrctr_args_size           = compute_cnstrctr_args_size l
    ; stor_array_seeds_begin            = compute_stor_array_seeds_begin l
    ; stor_array_seeds_size             = compute_stor_array_seeds_size l
    }

let cnstrct_post_stor_layout (l:cntrct_stor_layout idx_list) (rntime:rntime_stor_layout) : post_stor_layout =
    { init_data_size                    = compute_init_data_size l rntime
    ; rntime_code_size                  = rntime.rntime_code_size
    ; rntime_cntrct_offsets      = rntime.rntime_offset_of_idx
    ; rntime_cnstrctr_offsets    = rntime.rntime_offset_of_cnstrctr
    ; l                                 = cnstrct_stor_layout l
    }

(* Assuming the layout described above, this definition makes sense. *)
let rntime_code_offset layout idx : int =
    layout.cnstrctr_code_size idx

let rec realize_imm(layout:post_stor_layout)(init_idx:idx) = function 
    | Big b                         ->  b
    | Int i                         ->  big i
    | Label l                       ->  big (Label.lookup_label l)
    | StorPCIndex                   ->  big (layout.l.stor_program_counter)
    | StorCnstrctrArgsBegin idx     ->  big (layout.l.stor_cnstrctr_args_begin idx)
    | StorCnstrctrArgsSize  idx     ->  big (layout.l.stor_cnstrctr_args_size idx)
    | InitDataSize          idx     ->  big (layout.init_data_size idx)
    | RntimeCodeOffset      idx     ->  big (rntime_code_offset layout.l idx)
    | RntimeCodeSize                ->  big (layout.rntime_code_size)
    | CnstrctrCodeSize      idx     ->  big (layout.l.cnstrctr_code_size idx)
    | RntimeCnstrctrOffset  idx     ->  big (lookup_index idx layout.rntime_cnstrctr_offsets)
    | RntimeCntrctOffset    idx     ->  big (lookup_index idx layout.rntime_cntrct_offsets)
    | RntimeMthdLabel(idx,mthd_hd)  ->  let label = lookup_entrypoint (Mthd (idx, mthd_hd)) in
                                        big (Label.lookup_label label)
    | Minus (a, b)                  ->  sub_big_int (realize_imm layout init_idx a) (realize_imm layout init_idx b)

let realize_opcode (l:post_stor_layout) (init_idx:idx) (i:imm Evm.opcode) = Evm.(match i with
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
    ; cntrct_arg_size           = Eth.total_size_of_argTys (L.map snd (Eth.argTys_of_cntrct cn))
    ; cntrct_num_array_seeds    = L.length (Eth.getArr_of_cntrct cn)
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
let arg_locations offset (cn:ty cntrct) : stor_addr list =
    let arg_tys       = L.map (fun a->a.ty) cn.cntrct_args in
    assert (L.for_all fits_in_one_stor_slot arg_tys) ; 
    let num_of_plains = count_plain_args arg_tys  in
    let ret           = arg_locations_inner offset 0 0 num_of_plains arg_tys in 
    ret 

let array_locations (cn:ty cntrct) : stor_addr list =
    let arg_tys       = L.map (fun a->a.ty) cn.cntrct_args  in
    assert (L.for_all fits_in_one_stor_slot arg_tys) ;
    let num_of_plains = count_plain_args arg_tys            in
    let total_num     = L.length arg_tys                    in
    if total_num=num_of_plains 
        then []
        else BL.(range (2 + num_of_plains) `To (total_num + 1))

