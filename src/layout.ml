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

type cnstrInfo                  =
                                { cnstrCodeSize     : int
                                ; fieldVarsSize     : int      
                                ; fieldArrsSize     : int    
                                ; fieldTypes        : ty list       }   

let cnstrInfo_of_cn cn initCode =   
                                { cnstrCodeSize     = size_of_program initCode                                
                                ; fieldVarsSize     = size_of_vars_in_cn  cn                                  
                                ; fieldArrsSize     = len  (arrTys_of_cn cn)                             
                                ; fieldTypes        = tys_of_vars cn.fields     } 
                                                                         
                                                                        (* The Storage in Runtime                                        *) 
                                                                        (* Array elements are placed as in Solidity                      *)    
                                                                        (*                                                               *)
                                                                        (*  S[0]  := PROGRAM COUNTER                                     *)
type storLayout                 =                                       (*  S[1]  := ARRAY SEED COUNTER                                  *)
                                { pc                : int               (*  S[2]  := pod cntrct arg0   --+                ---+           *)
                                ; ac                : int               (*   ...                         |   k  args         | n args    *)                  
                                ; cnidxs            : idx list          (*  S[k+1]:= pod cntrct argk-1 --+                   |           *)    
                                ; cnstrSize         : idx -> int        (*  S[k+2]:= array0's seed     --+                   |           *)    
                                ; fieldVars         : idx -> int data   (*   ...                         | (n-k) arrSeeds    |           *)    
                                ; fieldArrs         : idx -> int data } (*  S[n+1]:= arraym's seed     --+                ---+           *)    
                                                       
let calc_cnstrSize l idx        =   (lookup idx l).cnstrCodeSize

let calc_fieldVars l idx        =   { offst         = 2 
                                    ; size          =     (lookup idx l).fieldVarsSize         }
let calc_fieldArrs l idx        =   { offst         = 2 + (lookup idx l).fieldVarsSize
                                    ; size          =     (lookup idx l).fieldArrsSize         } 
    
let cnstrct_storLayout idx_lyts =   { pc            = 0              
                                    ; ac            = 1
                                    ; cnidxs        = idxs idx_lyts 
                                    ; cnstrSize     = calc_cnstrSize idx_lyts 
                                    ; fieldVars     = calc_fieldVars idx_lyts
                                    ; fieldArrs     = calc_fieldArrs idx_lyts     } 
    
                                                       

(**********************************************************)
(***                                                    ***)
(**********************************************************)
type rntimeInfo                 =
                                { rntimeCodeSize            : int
                                ; rntimeCnOffsts            : int idxlist   (* == cnstrCodeSizes *) 
                                ; rntimeCnstrOffsts         : int idxlist
                                ; rntimeCnstrSizes          : int idxlist      }

(* init_data := Contract Creation Code                                                                          *) 
(* The initial data is organized like this:                                                                     *)
(* +---------------+                                                                                            *)
(* | cnstr  code   |                                                                                            *)
(* | rntime code  ---+                                                                                          *)
(* | cnstr  args   | |                                                                                          *)
(* +---------------+ |                                                                                          *)
(*                   |                                                                                          *)
(*                   |  the rntime code is organized like this:                                                 *)
(*                   |  +----------------------------------+       the rntime code for a particular cntrct      *)
(*                   +->|dispatcher that jumps to StoredPC |      +--------------------------------------+      *)
(*                      |rntime code for cntrct A     ----------->| dispatcher that jumps into a method  |      *)
(*                      |rntime code for cntrct B          |      | rntime code for method f             |      *)
(*                      |rntime code for cntrct C          |      | rntime code for method g             |      *)
(*                      +----------------------------------+      +--------------------------------------+      *)

type cnLayout                   =                                                   
                                { initDataSize              : idx -> int                      
                                ; rntimeCodeSize            : int                   
                                ; rntimeCnOffsts            : int idxlist               
                                ; rntimeCnstrOffsts         : int idxlist
                                ; sl                        : storLayout        }

let calc_cnstrArgBegin l (ri:rntimeInfo) idx    =   calc_cnstrSize l idx + ri.rntimeCodeSize
let calc_initDataSize  l (ri:rntimeInfo) idx    =   calc_cnstrArgBegin l ri idx + (lookup idx l).fieldVarsSize
let cnstrct_cnLayout   l (ri:rntimeInfo)        =   { initDataSize          = calc_initDataSize l ri
                                                    ; rntimeCodeSize        = ri.rntimeCodeSize
                                                    ; rntimeCnOffsts        = ri.rntimeCnOffsts
                                                    ; rntimeCnstrOffsts     = ri.rntimeCnstrOffsts
                                                    ; sl                    = cnstrct_storLayout l          }


let rec realize_imm cnLayt (init_idx:idx)   = function 
    | Big b                         ->  b
    | Int i                         ->  big i
    | Label l                       ->  big (Label.lookup_label l)
    | StorPCIndex                   ->  big (cnLayt.sl.pc)
    | StorFieldsBegin       idx     ->  big (cnLayt.sl.fieldVars idx).offst
    | StorFieldsSize        idx     ->  big (cnLayt.sl.fieldVars idx).size
    | InitDataSize          idx     ->  big (cnLayt.initDataSize idx)
    | RntimeCodeOffset      idx     ->  big (cnLayt.sl.cnstrSize idx)
    | RntimeCodeSize                ->  big (cnLayt.rntimeCodeSize)
    | CnstrCodeSize         idx     ->  big (cnLayt.sl.cnstrSize idx)
    | RntimeCnstrOffset     idx     ->  big (lookup idx cnLayt.rntimeCnstrOffsts)
    | RntimeCntrctOffset    idx     ->  big (lookup idx cnLayt.rntimeCnOffsts)
    | RntimeMthdLabel(idx,mthd_hd)  ->  let label = lookup_entry (Mthd(idx,mthd_hd)) in big (Label.lookup_label label)
    | Minus (a, b)                  ->  sub_big_int (realize_imm cnLayt init_idx a) (realize_imm cnLayt init_idx b)

let realize_opcode cnLayt (init_idx:idx)    = function 
    | PUSH1  imm      -> PUSH1  (realize_imm cnLayt init_idx imm)
    | PUSH4  imm      -> PUSH4  (realize_imm cnLayt init_idx imm)
    | PUSH32 imm      -> PUSH32 (realize_imm cnLayt init_idx imm)
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
let fieldVar_locations offset (cn:ty cntrct) : int list =
    let field_tys     = tys_of_vars cn.fields       in
    let num_of_plains = count_plain_args field_tys      in
    let ret           = gen_field_locs offset 0 0 num_of_plains field_tys in 
    ret 

let fieldArr_locations (cn:ty cntrct) : int list =
    let field_tys     = tys_of_vars cn.fields       in
    let num_of_plains = count_plain_args field_tys      in
    let total_num     = L.length field_tys              in
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


