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


(**********************************************************)
(***                                                    ***)
(**********************************************************)

type creation_info              =   { creation_size           : int
                                    ; var_size          : int      
                                    ; arr_size          : int    
                                    ; fld_types         : ty list       }   

let cr_info_of_cn cn initCode   =   { creation_size           = size_of_program initCode                                
                                    ; var_size          = size_of_vars_in_cn  cn                                  
                                    ; arr_size          = len  (arrTys_of_cn  cn)                             
                                    ; fld_types         = fldTys_of_cn      cn } 
                                                                            
type storage                    =   { pc                : int               (* The Storage in Runtime                                        *) 
                                    ; ac                : int               (* Array elements are placed as in Solidity                      *)                  
                                    ; cnidxs            : idx list          (*                                                               *)    
                                    ; cr_size           : idx -> int        (*  S[0]  := PROGRAM COUNTER                                     *)    
                                    ; vars              : idx -> int data   (*  S[1]  := ARRAY SEED COUNTER                                  *)    
                                    ; arrs              : idx -> int data } (*  S[2]  := pod cntrct arg0   --+                ---+           *)    
                                                                            (*   ...                         |   k  args         | n args    *)  
                                                                            (*  S[k+1]:= pod cntrct argk-1 --+                   |           *)  
                                                                            (*  S[k+2]:= array0's seed     --+                   |           *)  
                                                                            (*   ...                         | (n-k) arrSeeds    |           *)  
                                                                            (*  S[n+1]:= arraym's seed     --+                ---+           *)  

let calc_cr_size l idx : int    =   (lookup idx l).creation_size

let calc_var_pos l idx          =   { offst             = 2 
                                    ; size              =     (lookup idx l).var_size      }
let calc_arr_pos l idx          =   { offst             = 2 + (lookup idx l).var_size
                                    ; size              =     (lookup idx l).arr_size      } 
    
let init_storage idx_lyts       :   storage 
                                =   { pc                = 0              
                                    ; ac                = 1
                                    ; cnidxs            = idxs idx_lyts 
                                    ; cr_size           = calc_cr_size idx_lyts 
                                    ; vars              = calc_var_pos idx_lyts
                                    ; arrs              = calc_arr_pos idx_lyts               } 

(**********************************************************)
(***                                                    ***)
(**********************************************************)
type rntimeInfo                 =   { rntimeCodeSize    : int
                                    ; rntimeCnOffsts    : int idxlist   (* == cnstrCodeSizes *) 
                                    ; rntimeCnstrOffsts : int idxlist
                                    ; rntimeCnstrSizes  : int idxlist                           }

(* init_data := Contract Creation Code                                                                          *) 
(* The initial data is organized like this:                                                                     *)
(* +---------------+                                                                                            *)
(* | cnstr  code   |                                                                                            *)
(* | rntime code  ---+                                                                                          *)
(* | cnstr  args   | |                                                                                          *)
(* +---------------+ |                                                                                          *)
(*                   |  the rntime code is organized like this:                                                 *)
(*                   |  +----------------------------------+       the rntime code for a particular cntrct      *)
(*                   +->|dispatcher that jumps to StoredPC |      +--------------------------------------+      *)
(*                      |rntime code for cntrct A     ----------->| dispatcher that jumps into a method  |      *)
(*                      |rntime code for cntrct B          |      | rntime code for method f             |      *)
(*                      |rntime code for cntrct C          |      | rntime code for method g             |      *)
(*                      +----------------------------------+      +--------------------------------------+      *)

type cnLayout                   =   { initDataSize              : idx -> int                      
                                    ; rntimeCodeSize            : int                   
                                    ; rntimeCnOffsts            : int idxlist               
                                    ; rntimeCnstrOffsts         : int idxlist
                                    ; sl                        : storage        }

let calc_cnstrArgBegin l (ri:rntimeInfo) idx    =   calc_cr_size l idx + ri.rntimeCodeSize
let calc_initDataSize  l (ri:rntimeInfo) idx    =   calc_cnstrArgBegin l ri idx + (lookup idx l).var_size
let cnstrct_cnLayout   l (ri:rntimeInfo)        =   { initDataSize          = calc_initDataSize l ri
                                                    ; rntimeCodeSize        = ri.rntimeCodeSize
                                                    ; rntimeCnOffsts        = ri.rntimeCnOffsts
                                                    ; rntimeCnstrOffsts     = ri.rntimeCnstrOffsts
                                                    ; sl                    = init_storage l          }

let rec realize_imm cnLayt (init_idx:idx)   = function 
    | Big b                         ->  b
    | Int i                         ->  big i
    | Label l                       ->  big (Label.lookup_label l)
    | StorPCIndex                   ->  big (cnLayt.sl.pc)
    | StorFieldsBegin       idx     ->  big (cnLayt.sl.vars idx).offst
    | StorFieldsSize        idx     ->  big (cnLayt.sl.vars idx).size
    | InitDataSize          idx     ->  big (cnLayt.initDataSize idx)
    | RntimeCodeOffset      idx     ->  big (cnLayt.sl.cr_size idx)
    | RntimeCodeSize                ->  big (cnLayt.rntimeCodeSize)
    | CnstrCodeSize         idx     ->  big (cnLayt.sl.cr_size idx)
    | RntimeCnstrOffset     idx     ->  big (lookup idx cnLayt.rntimeCnstrOffsts)
    | RntimeCntrctOffset    idx     ->  big (lookup idx cnLayt.rntimeCnOffsts)
    | RntimeMthdLabel(idx,mthd_hd)  ->  big (Label.lookup_label (lookup_entry (Mthd(idx,mthd_hd)))) 

let realize_opcode cnLayt (init_idx:idx)    = function 
    | PUSH1  imm      -> PUSH1  (realize_imm cnLayt init_idx imm)
    | PUSH4  imm      -> PUSH4  (realize_imm cnLayt init_idx imm)
    | PUSH32 imm      -> PUSH32 (realize_imm cnLayt init_idx imm)
    | Comment s       -> Comment s 
    | NOT             -> NOT
    | TIMESTAMP       -> TIMESTAMP
    | ISZERO          -> ISZERO
    | EQ              -> EQ
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

let realize_program l init_idx p = L.map (realize_opcode l init_idx) p

let rec gen_field_locs offset used_plains used_seeds num_plains = function 
    | []        ->  []
    | ty::tys   ->  if is_mapping ty
                    then (offset + num_plains + used_seeds) :: gen_field_locs offset used_plains (used_seeds+1) num_plains tys
                    else (offset + used_plains)             :: gen_field_locs offset (used_plains+1) used_seeds num_plains tys

(* this needs to take stor_fieldVars_begin *)
let var_locs_of_cn offset cn : int list =
    let fldtys          = fldTys_of_cn cn               in
    let num_vars        = count_plain_args fldtys       in
    gen_field_locs offset 0 0 num_vars fldtys 

let arr_locs_of_cn cn : int list =
    let field_tys       = fldTys_of_cn cn                 in
    let num_vars        = count_plain_args field_tys           in
    let total_num       = L.length field_tys              in
    if total_num=num_vars 
        then []
        else BL.(range (2 + num_vars) `To (total_num + 1))

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


