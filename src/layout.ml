open Big_int
open Printf 
open Misc
open Syntax
open Label
open Location
open Context 
open Evm 

module BL   = BatList
module L    = List

(*********************************************)
(***            CREATION INFO              ***)
(*********************************************)

let pc          = 0
let ac          = 1
let fldbegin    = 2

type creation_info              =   { cr_size       : int                   (* The Storage in Runtime                                        *)
                                    ; var_size      : int                   (* Array elements are placed as in Solidity                      *)
                                    ; arr_size      : int                   (*                                                               *)
                                    ; fld_types     : ty list           }   (*  S[0]  := PROGRAM COUNTER                                     *)
                                                                            (*  S[1]  := ARRAY SEED COUNTER                                  *)
type storage                    =   { pc            : int                   (*  S[2]  := pod cntrct arg0   --+                ---+           *)
                                    ; ac            : int                   (*   ...                         |   k  args         | n args    *)              
                                    ; cnidxs        : idx list              (*  S[k+1]:= pod cntrct argk-1 --+                   |           *)
                                    ; cr_sizes      : idx -> int            (*  S[k+2]:= array0's seed     --+                   |           *)
                                    ; vars          : idx -> int data       (*   ...                         | (n-k) arrSeeds    |           *)
                                    ; arrs          : idx -> int data   }   (*  S[n+1]:= arraym's seed     --+                ---+           *)  
                                                                          
let calc_cr_sizes l idx         =   (lookup idx l).cr_size
let calc_var_pos  l idx         =   { offst         = 2 
                                    ; size          =     (lookup idx l).var_size      }
let calc_arr_pos  l idx         =   { offst         = 2 + (lookup idx l).var_size
                                    ; size          =     (lookup idx l).arr_size      } 
    
let init_storage i_cr_infos     =   { pc            = 0              
                                    ; ac            = 1
                                    ; cnidxs        = idxs          i_cr_infos 
                                    ; cr_sizes      = calc_cr_sizes i_cr_infos 
                                    ; vars          = calc_var_pos  i_cr_infos
                                    ; arrs          = calc_arr_pos  i_cr_infos         } 

(*********************************************)
(***            RUNTIME INFO               ***)
(*********************************************)

type rntime_info                =   { rn_size       : int
                                    ; cns_pos       : int ilist 
                                    ; crs_pos       : int ilist
                                    ; crs_sizes     : int ilist                        }

(* init_data := Contract Creation Code                                                                  *) 
(* The initial data is organized like this:                                                             *)
(* +---------------+                                                                                    *)
(* | creation code |      <--- this creation code is discarded in the creation                          *)
(* | runtime  code---+                                                                                  *)
(* | creation codes| |  rntime code is organized like this:                                             *)
(* +---------------+ |  +---------------------------------+    rntime code for a particular cntrct:     *)
(*                   +--> dispatcher that jumps to S[PC]  |    +-------------------------------------+  *)
(*                      | rntime code for cntrct A ------------> dispatcher that jumps into a method |  *)
(*                      | rntime code for cntrct B        |    | rntime code for method f            |  *)
(*                      | rntime code for cntrct C        |    | rntime code for method g            |  *)
(*                      +---------------------------------+    +-------------------------------------+  *)

type layout                     =   { initdata_size : idx -> int                      
                                    ; rntime_size   : int                   
                                    ; rn_cns_pos    : int ilist               
                                    ; rn_crs_pos    : int ilist
                                    ; stor          : storage        }

let calc_cr_begin  l ri idx     =   calc_cr_sizes l idx + ri.rn_size
let calc_initdata_size l ri idx =   calc_cr_begin l ri idx + (lookup idx l).var_size
let init_layout   l ri          =   { initdata_size = calc_initdata_size l ri
                                    ; rntime_size   = ri.rn_size
                                    ; rn_cns_pos    = ri.cns_pos
                                    ; rn_crs_pos    = ri.crs_pos
                                    ; stor          = init_storage l          }


(*************************)
(* SIZE DECISION of PUSH *) 
(*************************)

let log_size b   = 
    if b <! big 0 then err "log_size takes POSITIVE value" else 
    if b <! big 0x100 ^! big 1  then 1  else  
    if b <! big 0x100 ^! big 2  then 2  else  
    if b <! big 0x100 ^! big 4  then 4  else 
    if b <! big 0x100 ^! big 5  then 5  else 
    if b <! big 0x100 ^! big 8  then 8  else  
    if b <! big 0x100 ^! big 16 then 16 else 
    if b <! big 0x100 ^! big 20 then 20 else 
    if b <! big 0x100 ^! big 32 then 32 else err "log_size: TOO BIG INT"

exception SizeDeterminedLater

let vsize_of_imm = function 
    | Big b                         ->  log_size b
    | Int i                         ->  log_size (big i)
    (*| Label l                       ->  begin try log_size (big (lookup_label l)) with _ -> err"vsize_of_imm" end *)
    (*| RnMthdLabel (idx,mhd)         ->  log_size (big (lookup_entry (Mthd(idx,mhd)))) *)
    | StorPC                        ->  1 
    | StorVarBegin      idx         ->  1
    | _                             ->  raise SizeDeterminedLater 

let vsize_of_opcode = function 
    | PUSH imm                      ->  1 + vsize_of_imm imm
    | opcode                        ->  size_of_opcode opcode 

let vsize_of_prog = 
    let rec loop later = function 
    | []                            -> 0, later
    | o :: os                       -> let size, later = loop later os in 
                                       try  vsize_of_opcode o + size, later 
                                       with SizeDeterminedLater -> size, 1 + later  in 
    loop 0

let push_n n imm = match n with 
    | 1  -> PUSH1  imm   | 8  -> PUSH8  imm 
    | 2  -> PUSH2  imm   | 16 -> PUSH16 imm 
    | 4  -> PUSH4  imm   | 20 -> PUSH20 imm   
    | 5  -> PUSH5  imm   | 32 -> PUSH32 imm 
    | n     -> err "push_n : NonSupportedNumber"

let numerize_imm = function
    | Big b                     -> Big (b)
    | Int i                     -> Big (big i)
    (*| Label l                   -> begin try Big (big (lookup_label l)) with _ -> err "numerize_imm" end*) 
    (*| RnMthdLabel (idx,mhd)     -> Big (big (lookup_label (lookup_entry (Mthd(idx,mhd))))) *)
    | StorPC                    -> Big (big 0)
    | StorVarBegin _            -> Big (big 2)
    | imm                       -> imm

let dec_PUSH sol_push_sz = function 
    | PUSH imm                  -> begin match numerize_imm imm with 
        | Big b                     -> begin 
                                        if b <! big 0 then err "PUSH VALUE cannot be NEGATIVE" else
                                        if b <! big 256^! big 1    then PUSH1  (Big b)  else
                                        if b <! big 256^! big 2    then PUSH2  (Big b)  else
                                        if b <! big 256^! big 4    then PUSH4  (Big b)  else 
                                        if b <! big 256^! big 5    then PUSH5  (Big b)  else 
                                        if b <! big 256^! big 8    then PUSH8  (Big b)  else 
                                        if b <! big 256^! big 16   then PUSH16 (Big b)  else
                                        if b <! big 256^! big 20   then PUSH20 (Big b)  else 
                                        if b <! big 256^! big 32   then PUSH32 (Big b)  else
                                        err "classify_PUSH: TOO BIG INT" end 
        | imm                       -> push_n sol_push_sz imm end 
    | opcode                    -> opcode ;;


(***********************************)
(* PROGRAM REALIZATION with LAYOUT *) 
(***********************************)

let realize_imm lyt = function 
    | Big b                         ->  b
    | Int i                         ->  big i
    | Label l                       ->  big (lookup_label l)
    | RnMthdLabel    (idx,mhd)      ->  big (lookup_label (lookup_entry (Mthd(idx,mhd)))) 
    | StorPC                        ->  big (lyt.stor.pc)
    | StorVarBegin          idx     ->  big (lyt.stor.vars idx).offst

    | InitDataSize          idx     ->  big (lyt.initdata_size idx)
    | CrSize                idx     ->  big (lyt.stor.cr_sizes idx)
    | RnSize                        ->  big (lyt.rntime_size)
    | RnCrOffset            idx     ->  big (lookup idx lyt.rn_crs_pos)
    | RnCnOffset            idx     ->  big (lookup idx lyt.rn_cns_pos)

let realize_opcode lyt  = function 
    | PUSH1  imm      -> PUSH1 (realize_imm lyt imm)
    | PUSH2  imm      -> PUSH2 (realize_imm lyt imm)
    | PUSH4  imm      -> PUSH4 (realize_imm lyt imm)
    | PUSH5  imm      -> PUSH5 (realize_imm lyt imm)
    | PUSH8  imm      -> PUSH8 (realize_imm lyt imm)
    | PUSH16 imm      -> PUSH16(realize_imm lyt imm)
    | PUSH20 imm      -> PUSH20(realize_imm lyt imm)
    | PUSH32 imm      -> PUSH32(realize_imm lyt imm)
    | POP             -> POP              | MLOAD           -> MLOAD          
    | SHL             -> SHL              | MSTORE          -> MSTORE             
    | SHR             -> SHR              | MSTORE8         -> MSTORE8
    | NOT             -> NOT              | SLOAD           -> SLOAD
    | TIMESTAMP       -> TIMESTAMP        | SSTORE          -> SSTORE
    | ISZERO          -> ISZERO           | JUMP            -> JUMP
    | EQ              -> EQ               | JUMPI           -> JUMPI
    | LT              -> LT               | PC              -> PC
    | GT              -> GT               | MSIZE           -> MSIZE
    | BALANCE         -> BALANCE          | GAS             -> GAS
    | STOP            -> STOP             | JUMPDEST l      -> JUMPDEST l
    | ADD             -> ADD              | LOG0            -> LOG0
    | MUL             -> MUL              | LOG1            -> LOG1
    | SUB             -> SUB              | LOG2            -> LOG2
    | DIV             -> DIV              | LOG3            -> LOG3
    | SDIV            -> SDIV             | LOG4            -> LOG4
    | MOD             -> MOD              | CREATE          -> CREATE
    | SMOD            -> SMOD             | CALL            -> CALL
    | ADDMOD          -> ADDMOD           | CALLCODE        -> CALLCODE
    | MULMOD          -> MULMOD           | RETURN          -> RETURN
    | EXP             -> EXP              | DELEGATECALL    -> DELEGATECALL
    | SIGNEXTEND      -> SIGNEXTEND       | SELFDESTRUCT    -> SELFDESTRUCT
    | SHA3            -> SHA3             | SWAP1           -> SWAP1
    | ADDRESS         -> ADDRESS          | SWAP2           -> SWAP2
    | ORIGIN          -> ORIGIN           | SWAP3           -> SWAP3
    | CALLER          -> CALLER           | SWAP4           -> SWAP4
    | CALLVALUE       -> CALLVALUE        | SWAP5           -> SWAP5
    | CALLDATALOAD    -> CALLDATALOAD     | SWAP6           -> SWAP6
    | CALLDATASIZE    -> CALLDATASIZE     | DUP1            -> DUP1
    | CALLDATACOPY    -> CALLDATACOPY     | DUP2            -> DUP2
    | CODESIZE        -> CODESIZE         | DUP3            -> DUP3
    | CODECOPY        -> CODECOPY         | DUP4            -> DUP4
    | GASPRICE        -> GASPRICE         | DUP5            -> DUP5
    | EXTCODESIZE     -> EXTCODESIZE      | DUP6            -> DUP6
    | EXTCODECOPY     -> EXTCODECOPY      | DUP7            -> DUP7              
    | Comment s       -> Comment s        
                                                                          
let realize_prog lyt p = L.map (realize_opcode lyt) p 




let rec gen_arg_locs offst vid aid varsz = function 
    | []        ->  []
    | ty::tys   ->  if is_mapping ty
                    then offst + varsz + aid :: gen_arg_locs offst vid (aid+1) varsz tys
                    else offst         + vid :: gen_arg_locs offst (vid+1) aid varsz tys

let arg_locs_of_cn offst cn : int list =
    let fldtys          = fldTys_of_cn cn               in
    let varsz           = count_vars fldtys             in
    gen_arg_locs offst 0 0 varsz fldtys 

let arr_locs_of_cn cn : int list =
    let fldtys          = fldTys_of_cn cn               in
    let varsize         = count_vars fldtys             in
    let fldsize         = L.length fldtys               in
    if fldsize = varsize
        then []
        else BL.(range (2+varsize) `To (fldsize+1))

(*                                                                      *)
(*          Storage of a contract                                       *)
(*          +---+-----------------+                                     *)
(*          |0  |PC               |                                     *)
(*          |1  |ArraySeedCounter |                                     *)
(*          |2  |arg1 ----+       |    ----+                            *)
(*          |3  |arg2     |       |        |                            *)
(*          |4  |arg3     + num of vars    |                            *)
(*          |.. |         |       |        + total_num (of args)        *)
(*          |k+1|argk ----+       |        |                            *)
(*          |k+2|aid1             |        |                            *)
(*          |.. | ..              |        |                            *)
(*          |n+1|aidm   -------------------+                            *)
(*          |   |                 |                                     *)
(*          +---+-----------------+                                     *)
(*                                                                      *)


