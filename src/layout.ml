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

type creation_info              =   { cr_size           : int
                                    ; var_size          : int      
                                    ; arr_size          : int    
                                    ; fld_types         : ty list       }   

let cr_info_of_cn cn initCode   =   { cr_size           = size_of_prog initCode                                
                                    ; var_size          = size_of_vars_in_cn  cn                                  
                                    ; arr_size          = len  (arrTys_of_cn  cn)                             
                                    ; fld_types         = fldTys_of_cn      cn } 
                                                                            
type storage                    =   { pc                : int               (* The Storage in Runtime                                        *) 
                                    ; ac                : int               (* Array elements are placed as in Solidity                      *)                  
                                    ; cnidxs            : idx list          (*                                                               *)    
                                    ; cr_sizes          : idx -> int        (*  S[0]  := PROGRAM COUNTER                                     *)    
                                    ; vars              : idx -> int data   (*  S[1]  := ARRAY SEED COUNTER                                  *)    
                                    ; arrs              : idx -> int data } (*  S[2]  := pod cntrct arg0   --+                ---+           *)    
                                                                            (*   ...                         |   k  args         | n args    *)  
                                                                            (*  S[k+1]:= pod cntrct argk-1 --+                   |           *)  
                                                                            (*  S[k+2]:= array0's seed     --+                   |           *)  
                                                                            (*   ...                         | (n-k) arrSeeds    |           *)  
                                                                            (*  S[n+1]:= arraym's seed     --+                ---+           *)  

let calc_cr_sizes l idx         =   (lookup idx l).cr_size

let calc_var_pos  l idx         =   { offst             = 2 
                                    ; size              =     (lookup idx l).var_size      }
let calc_arr_pos  l idx         =   { offst             = 2 + (lookup idx l).var_size
                                    ; size              =     (lookup idx l).arr_size      } 
    
let init_storage istors       :   storage 
                                =   { pc                = 0              
                                    ; ac                = 1
                                    ; cnidxs            = idxs istors
                                    ; cr_sizes          = calc_cr_sizes istors 
                                    ; vars              = calc_var_pos  istors
                                    ; arrs              = calc_arr_pos  istors             } 

(**********************************************************)
(***                                                    ***)
(**********************************************************)
type rntime_info                =   { rn_size         : int
                                    ; rn_cns_pos      : int ilist   (* == cnstrCodeSizes *) 
                                    ; rn_crs_pos      : int ilist
                                    ; rn_crs_sizes    : int ilist                           }

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

type cnLayout                   =   { initdata_size      : idx -> int                      
                                    ; rn_size            : int                   
                                    ; rn_cns_pos         : int ilist               
                                    ; rn_crs_pos         : int ilist
                                    ; stor               : storage        }

let calc_cr_arg_begin  l (ri:rntime_info) idx   =   calc_cr_sizes l idx + ri.rn_size
let calc_initdata_size l (ri:rntime_info) idx   =   calc_cr_arg_begin l ri idx + (lookup idx l).var_size
let cnstrct_cnLayout   l (ri:rntime_info)       =   { initdata_size         = calc_initdata_size l ri
                                                    ; rn_size               = ri.rn_size
                                                    ; rn_cns_pos            = ri.rn_cns_pos
                                                    ; rn_crs_pos            = ri.rn_crs_pos
                                                    ; stor                  = init_storage l          }

let rec realize_imm cnLayt (init_idx:idx)   = function 
    | Big b                         ->  b
    | Int i                         ->  big i
    | Label l                       ->  big (Label.lookup_label l)
    | StorPCIndex                   ->  big (cnLayt.stor.pc)
    | StorFldBegin          idx     ->  big (cnLayt.stor.vars idx).offst
    | StorFldSize           idx     ->  big (cnLayt.stor.vars idx).size
    | InitDataSize          idx     ->  big (cnLayt.initdata_size idx)
    | RnCodeOffset          idx     ->  big (cnLayt.stor.cr_sizes idx)
    | RnCodeSize                    ->  big (cnLayt.rn_size)
    | CreationSize          idx     ->  big (cnLayt.stor.cr_sizes idx)
    | RnCrOffset            idx     ->  big (lookup idx cnLayt.rn_crs_pos)
    | RnCnOffset            idx     ->  big (lookup idx cnLayt.rn_cns_pos)
    | RnMthdLabel    (idx,mthd_hd)  ->  big (Label.lookup_label (lookup_entry (Mthd(idx,mthd_hd)))) 

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

let realize_prog l init_idx p = L.map (realize_opcode l init_idx) p

let rec gen_field_locs offset used_plains used_seeds num_plains = function 
    | []        ->  []
    | ty::tys   ->  if is_mapping ty
                    then (offset + num_plains + used_seeds) :: gen_field_locs offset used_plains (used_seeds+1) num_plains tys
                    else (offset + used_plains)             :: gen_field_locs offset (used_plains+1) used_seeds num_plains tys

(* this needs to take stor_fieldVars_begin *)
let var_locs_of_cn offset cn : int list =
    let fldtys          = fldTys_of_cn cn               in
    let varsize         = count_vars fldtys             in
    gen_field_locs offset 0 0 varsize fldtys 

let arr_locs_of_cn cn : int list =
    let fldtys          = fldTys_of_cn cn               in
    let varsize         = count_vars fldtys             in
    let fldsize         = L.length fldtys               in
    if fldsize = varsize
        then []
        else BL.(range (2+varsize) `To (fldsize+1))

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


