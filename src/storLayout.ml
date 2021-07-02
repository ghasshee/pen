open Big_int
open Printf 
open Misc
open Syntax
open Label
open IndexList
open Location
open Context 

module BL   = BatList
module L    = List


(* Stor Layout that should be available after the cnstrctr compilation finishes *)
type storLayout            =
                            { program_counter           : int           (* numbers about the storage                                     *)
                            ; arraySeeds_counter        : int           (* The storage during the rntime looks like this:                *)    
                            ; idxs                      : idx list      (* |S[0]  := PROGRAM COUNTER                                     *)
                            ; cnstrctrCode_size         : idx -> int    (* |S[1]  := ARRAY SEED COUNTER                                  *)
                            ; stor_cnstrctrArgs_begin   : idx -> int    (* |S[2]  := pod cntrct arg0   --+                ---+           *)
                            ; stor_cnstrctrArgs_size    : idx -> int    (* |S[3]  := pod cntrct arg1     |                   |           *)
                            ; stor_arraySeeds_begin     : idx -> int    (* |S[4]  := pod cntrct arg2     | ( k ) args        |           *)
                            ; stor_arraySeeds_size      : idx -> int    (* | ...                         |                   | n args    *)
                            }                                           (* |S[k+1]:= pod cntrct argk-1 --+                   |           *)
                                                                        (* |S[k+2]:= array0's seed     --+                   |           *)
                                                                        (* | ...                         | (n-k) arrSeeds    |           *)
                                                                        (* |S[n+1]:= arraym's seed     --+                ---+           *)
                                                                        (* |                                                             *)
                                                                        (* array elements are placed at the same location as in Solidity *)
                              

(* Stor Layout that should be available after the rntime compilation finishes. *)
(* rntimeCode_offset = cnstrctrCode_size *)

(* init_data := Contract Creation Code                                                                          *) 
(* The initial data is organized like this:                                                                     *)
(* +---------------+                                                                                            *)
(* | cnstrctr code |                                                                                            *)
(* | rntime   code --+                                                                                          *)
(* | cnstrctr args | |                                                                                          *)
(* +---------------+ |                                                                                          *)
(*                   |                                                                                          *)
(*                   |  the rntime code is organized like this:                                                 *)
(*                   |  +----------------------------------+       the rntime code for a particular cntrct      *)
(*                   +->|dispatcher that jumps to StoredPC |      +--------------------------------------+      *)
(*                      |rntime code for cntrct A   ------------->| dispatcher that jumps into a method  |      *)
(*                      |rntime code for cntrct B          |      | rntime code for method f             |      *)
(*                      |rntime code for cntrct C          |      | rntime code for method g             |      *)
(*                      +----------------------------------+      +--------------------------------------+      *)

type post_storLayout       =                                                   
                            { init_data_size            : idx -> int                      
                            ; rn_codesize               : int                   
                            ; rn_cntrct_offsets         : int idx_list               
                            ; rn_cnstrctr_offsets       : int idx_list
                            ; l                         : storLayout
                            }

type cn_storLayout     =
                            { cn_cnstrctrCode_size      : int
                            ; cn_args_size              : int (** the number of words that the cntrct args occupy *)
                            ; cn_num_arraySeeds         : int (** the number of args that arrays *)
                            ; cn_args                   : ty list (** the list of arg types *)
                            }

type rn_storLayout     =
                            { rn_codesize               : int
                            ; rn_cn_offsets             : int idx_list
                            ; rn_cnstrctr_offsets       : int idx_list
                            ; rn_cnstrctr_sizes         : int idx_list
                            }





let compute_cnstrctrCode_size l idx        =
    let cnSL = lookup_index idx l in
    cnSL.cn_cnstrctrCode_size

let compute_cnstrctrArgs_size l idx        =
    let cnSL = lookup_index idx l in
    cnSL.cn_args_size

let compute_cnstrctrArgs_begin l rntime idx =
    compute_cnstrctrCode_size l idx + rntime.rn_codesize

let compute_init_data_size l rntime idx     =
    compute_cnstrctrArgs_begin l rntime idx + compute_cnstrctrArgs_size l idx

let compute_stor_cnstrctrArgs_begin        = konst 2

let compute_stor_arraySeeds_begin l idx    = 
    compute_stor_cnstrctrArgs_begin idx + compute_cnstrctrArgs_size l idx

let compute_stor_arraySeeds_size lst idx   =
    let c = lookup_index idx lst in
    c.cn_num_arraySeeds

let cnstrct_storLayout(l:cn_storLayout idx_list) : storLayout =
    { idxs                              = L.map fst l
    ; cnstrctrCode_size                 = compute_cnstrctrCode_size l
    ; program_counter                   = 0     (* fixed constant. *)
    ; arraySeeds_counter                = 1     (* fixed constant. *) 
    ; stor_cnstrctrArgs_begin           = compute_stor_cnstrctrArgs_begin
    ; stor_cnstrctrArgs_size            = compute_cnstrctrArgs_size l
    ; stor_arraySeeds_begin             = compute_stor_arraySeeds_begin l
    ; stor_arraySeeds_size              = compute_stor_arraySeeds_size l
    }

let cnstrct_post_storLayout (l:cn_storLayout idx_list) (rntime:rn_storLayout) : post_storLayout =
    { init_data_size                    = compute_init_data_size l rntime
    ; rn_codesize                       = rntime.rn_codesize
    ; rn_cntrct_offsets                 = rntime.rn_cn_offsets
    ; rn_cnstrctr_offsets               = rntime.rn_cnstrctr_offsets
    ; l                                 = cnstrct_storLayout l
    }

(* Assuming the layout described above, this definition makes sense. *)
let rntimeCode_offset layout idx : int =
    layout.cnstrctrCode_size idx

let rec realize_imm(layout:post_storLayout)(init_idx:idx) = function 
    | Big b                         ->  b
    | Int i                         ->  big i
    | Label l                       ->  big (Label.lookup_label l)
    | StorPCIndex                   ->  big (layout.l.program_counter)
    | StorCnstrctrArgsBegin idx     ->  big (layout.l.stor_cnstrctrArgs_begin idx)
    | StorCnstrctrArgsSize  idx     ->  big (layout.l.stor_cnstrctrArgs_size idx)
    | InitDataSize          idx     ->  big (layout.init_data_size idx)
    | RntimeCodeOffset      idx     ->  big (rntimeCode_offset layout.l idx)
    | RntimeCodeSize                ->  big (layout.rn_codesize)
    | CnstrctrCodeSize      idx     ->  big (layout.l.cnstrctrCode_size idx)
    | RntimeCnstrctrOffset  idx     ->  big (lookup_index idx layout.rn_cnstrctr_offsets)
    | RntimeCntrctOffset    idx     ->  big (lookup_index idx layout.rn_cntrct_offsets)
    | RntimeMthdLabel(idx,mthd_hd)  ->  let label = lookup_entry (Mthd (idx, mthd_hd)) in
                                        big (Label.lookup_label label)
    | Minus (a, b)                  ->  sub_big_int (realize_imm layout init_idx a) (realize_imm layout init_idx b)

let realize_opcode (l:post_storLayout) (init_idx:idx) (i:imm Evm.opcode) = Evm.(match i with
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

let storLayout_of_cntrct (cn:ty cntrct) (cnstrctrCode : imm Evm.program) =
    { cn_cnstrctrCode_size = Evm.size_of_program cnstrctrCode
    ; cn_args_size          = total_size_of_argTys (L.map snd (argTys_of_cntrct cn))
    ; cn_num_arraySeeds    = L.length  (getArr_cntrct cn)
    ; cn_args               = L.map     (fun a->a.ty) (cn.cntrct_args)
    }

let rec arg_locations_inner offset used_plain_args used_mapping_seeds num_of_plains = function 
    | []    ->  []
    | h::t  ->  if is_mapping h 
                    then (offset + num_of_plains + used_mapping_seeds) ::
                        arg_locations_inner offset used_plain_args(used_mapping_seeds+1)num_of_plains t
                    else (offset + used_plain_args) ::
                        arg_locations_inner offset(used_plain_args+1)used_mapping_seeds num_of_plains t

(* this needs to take stor_cnstrctrArgs_begin *)
let arg_locations offset (cn:ty cntrct) : int list =
    let arg_tys       = L.map (fun a->a.ty) cn.cntrct_args in
    assert (L.for_all fits_in_one_stor_slot arg_tys) ; 
    let num_of_plains = count_plain_args arg_tys  in
    let ret           = arg_locations_inner offset 0 0 num_of_plains arg_tys in 
    ret 

let array_locations (cn:ty cntrct) : int list =
    let arg_tys       = L.map (fun a->a.ty) cn.cntrct_args  in
    assert (L.for_all fits_in_one_stor_slot arg_tys) ;
    let num_of_plains = count_plain_args arg_tys            in
    let total_num     = L.length arg_tys                    in
    if total_num=num_of_plains 
        then []
        else BL.(range (2 + num_of_plains) `To (total_num + 1))

(*                                                                          *)
(*            Storage of a contract                                         *)
(*            +---+-----------------+                                       *)
(*            |0  |PC               |                                       *)
(*            |1  |ArraySeedCounter |                                       *)
(*            |2  |arg1 ----+       |    ------+                            *)
(*            |3  |arg2     |       |          |                            *)
(*            |4  |arg3     + num of plains    |                            *)
(*            |.. |         |       |          + total_num (of args)        *)
(*            |k+1|argk ----+       |          |                            *)
(*            |k+2|arr1             |          |                            *)
(*            |.. | ..              |          |                            *)
(*            |n+1|arrm   ---------------------+                            *)
(*            |   |                 |                                       *)
(*            +---+-----------------+                                       *)
(*                                                                          *)
(*                                                                          *)








