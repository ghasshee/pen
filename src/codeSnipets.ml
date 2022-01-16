
(* M[0x40]  :=   the address of mem alloc     *) 
(* MSTORE   :=   x=pop() ; y=pop() ; M[x]=y   *) 
(* MLOAD    :=   x=pop() ; push M[x]          *) 
(* CODECOPY  to from len :=  M[to .. to+len-1]=I_b[from .. from+len-1]  *)

open Printf 
open Big_int

open Misc
open Label
open Syntax
open Location
open Layout
open CodegenEnv
open Evm

module Crpt  = Crypto 
module BL   = BatList
module L    = List

let error_label = Int 3

(******************************************************)
(***     1. ERROR HANDLING                          ***)
(******************************************************)
let error_loop ce = 
    let error   =   fresh_label ()                          in 
    let boost   =   fresh_label ()                          in 
    let ce      =   PUSH1(Label boost)              =>> ce  in 
    let ce      =   JUMP                            =>> ce  in 
    let ce      =   Comment "BEGIN ERROR LOOP"      =>> ce  in 
    let ce      =   JUMPDEST error                  =>> ce  in 
    let ce      =   PUSH1(Label error)              =>> ce  in 
    let ce      =   JUMP                            =>> ce  in 
    let ce      =   Comment "END   ERROR LOOP"      =>> ce  in 
                    JUMPDEST boost                  =>> ce  

let throw ce         = (* the same with solc. *)
    let ce      =   PUSH1 error_label               =>> ce  in
                    JUMP                            =>> ce 

let throw_if ce     =                                           (*                               cond >> .. *)
    let ce      =   PUSH1 error_label               =>> ce  in  (*                   errlabel >> cond >> .. *)
                    JUMPI                           =>> ce      (* if cond then goto err                 .. *) 

let throw_if_0 ce    =                                          (*                                  i >> .. *)   
    let ce      =   DUP1                            =>> ce  in  (*                             i >> i >> .. *)
    let ce      =   ISZERO                          =>> ce  in  (*                 i==0 ? 1 : 0  >> i >> .. *)
                    throw_if                            ce      (*                                  i >> .. *) 
      
let throw_if_NEQ ce   =                                         (*                             a >> b >> .. *) 
    let ce      =   EQ                              =>> ce  in  (*                               a==b >> .. *)
    let ce      =   ISZERO                          =>> ce  in  (*                               a!=b >> .. *)    
                    throw_if                            ce      (* if a!=b then throw                    .. *) 

let goto    la ce   = 
    let ce      =   PUSH4(Label la)                 =>> ce  in 
                    JUMP                            =>> ce 

let if_GOTO la ce   = 
    let ce      =   PUSH4(Label la)                 =>> ce  in 
                    JUMPI                           =>> ce 

let if_0_GOTO la ce = 
    let ce      =   ISZERO                          =>> ce  in 
                    if_GOTO la                          ce  

let repeat opcode n ce = foldn n ((=>>)opcode) ce


(******************************************************)
(***     2. STACK OPERATIONS                        ***)
(******************************************************)

let push_storRange ce (data : imm data) =
    let i = match data.size with | Big b -> str_of_big b | Int i -> str_of_int i in 
    printf "stor_size is %s\n" i ; 
    assert (is_const_int 1 data.size) ; 
    let ce      =   PUSH32 data.offst               =>> ce  in
                    SLOAD                           =>> ce 

let dup_nth_from_bottom n ce  =
                dup_succ(get_stack_size ce - n)     =>> ce 

let shiftRtop ce bits =
    assert (0 <= bits && bits < 256) ; 
    if bits=0 then ce else                                      (*                                         x >> .. *) 
    let ce      = PUSH1 (Int bits)                  =>> ce  in  (*                                 bits >> x >> .. *)
    let ce      = PUSH1 (Int 2)                     =>> ce  in  (*                            2 >> bits >> x >> .. *)
    let ce      = EXP                               =>> ce  in  (*                              2**bits >> x >> .. *) 
    let ce      = SWAP1                             =>> ce  in  (*                              x >> 2**bits >> .. *) 
                  DIV                               =>> ce      (*                               x/(2**bits) >> .. *) 

let shiftLtop ce bits =
    assert (0 <= bits && bits < 256) ; 
    if bits=0 then ce else                                      (*                                         x >> .. *)
    let ce      = PUSH1 (Int bits)                  =>> ce  in  (*                                 bits >> x >> .. *)                   
    let ce      = PUSH1 (Int 2)                     =>> ce  in  (*                            2 >> bits >> x >> .. *) 
    let ce      = EXP                               =>> ce  in  (*                              2**bits >> x >> .. *) 
                  MUL                               =>> ce      (*                               (2**bits)*x >> .. *) 

let incr_top inc ce =
    let ce      = PUSH32 (Int inc)                  =>> ce  in
                  ADD                               =>> ce      

let sincr idx ce = 
    let ce      = PUSH1(Int idx)                    =>> ce  in  (*                                         i >> .. *) 
    let ce      = SLOAD                             =>> ce  in  (*                                      S[i] >> .. *) 
    let ce      = DUP1                              =>> ce  in  (*                              S[i] >> S[i] >> .. *) 
    let ce      = incr_top 1                            ce  in  (*                            S[i]+1 >> S[i] >> .. *) 
    let ce      = PUSH1(Int idx)                    =>> ce  in  (*                       i >> S[i]+1 >> S[i] >> .. *) 
                  SSTORE                            =>> ce      (* S[i]:=S[i]+1                         S[i] >> .. *) 

let calldataload ce data =
    assert (0 < data.size && data.size <= 32);
    let ce      = PUSH4 (Int data.offst)            =>> ce  in
                  CALLDATALOAD                      =>> ce  

let keccak_cat ce =                                             (*                                    a >> b >> .. *) 
    let ce      = PUSH1 (Int 0x00)                  =>> ce  in  (*                            0x00 >> a >> b >> .. *)
    let ce      = MSTORE                            =>> ce  in  (* M[0x00]=a                               b >> .. *)
    let ce      = PUSH1 (Int 0x20)                  =>> ce  in  (*                                 0x20 >> b >> .. *)
    let ce      = MSTORE                            =>> ce  in  (* M[0x20]=b                                    .. *) 
    let ce      = PUSH1 (Int 0x40)                  =>> ce  in  (*                                      0x40 >> .. *)
    let ce      = PUSH1 (Int 0x00)                  =>> ce  in  (*                              0x0  >> 0x40 >> .. *)
                  SHA3                              =>> ce      (*                       sha3(M[0x00..0x3F]) >> .. *)
                                                                (*                          sha3(a++b)             *)

let check_NOT_GT bound ce =                                     (*                                         x >> .. *)
    let ce      = DUP1                              =>> ce  in  (*                                    x >> x >> .. *)
    let ce      = PUSH32 bound                      =>> ce  in  (*                           bound >> x >> x >> .. *) 
    let ce      = LT                                =>> ce  in  (*                          bound<x?1:0 >> x >> .. *) 
                  throw_if                              ce      (* IF x<bound THEN error                   x >> .. *)

let check_NOT_LT bound ce =                                     (*                                         x >> .. *)
    let ce      = DUP1                              =>> ce  in  (*                                    x >> x >> .. *)
    let ce      = PUSH32 bound                      =>> ce  in  (*                           bound >> x >> x >> .. *) 
    let ce      = GT                                =>> ce  in  (*                          bound>x?1:0 >> x >> .. *) 
                  throw_if                              ce      (* IF x<bound then error                   x >> .. *)


(******************************************************)
(***     4. PROGRAM COUNTER on STORAGE              ***)
(******************************************************)
                
let reset_PC ce   =
    let ce      = PUSH1 StorPC                      =>> ce    in  (*                                       0 >> .. *)
    let ce      = SLOAD                             =>> ce    in  (*                                    S[0] >> .. *)
    let ce      = PUSH1 StorPC                      =>> ce    in  (*                               0 >> S[0] >> .. *)
    let ce      = DUP1                              =>> ce    in  (*                          0 >> 0 >> S[0] >> .. *)
                  SSTORE                            =>> ce        (* S'[0]=0                            S[0] >> .. *)

let restore_PC ce       =                                         (*                                  bkp_PC >> .. *)
    let ce      = PUSH1 StorPC                      =>> ce    in  (*                             0 >> bkp_PC >> .. *)
                  SSTORE                            =>> ce        (* S'[0]=bkp_pc                               .. *)             

let set_PC idx ce =                                               (*                                            .. *)
    let ce      = PUSH32(RnCnOffset idx)            =>> ce    in  (*                            rn_cn_offset >> .. *) 
    let ce      = PUSH1 StorPC                      =>> ce    in  (*                  storPC >> rn_cn_offset >> .. *) 
                  SSTORE                            =>> ce        (* S[storPC] := rn_cn_offset                  .. *) 

let get_PC ce =
    let ce      = PUSH1 StorPC                      =>> ce    in
                  SLOAD                             =>> ce 


(****************************************)
(***     5. MEMORY OPERATIONS         ***)
(****************************************)

let _KECCAK1    = Int 0x00
let _KECCAK2    = Int 0x20 
let _HP         = Int 0x40         (* HEAP Pointer *) 
let _HP_MIN     = Int 0x1000000    (* Initial HEAP Head *) 
let _MSP        = Int 0x80         (* Memory Stack : Another Stack different from EVM Stack *) 
let _MS_MIN     = Int 0x800000
let _MS_MAX     = let Int i = _HP_MIN in Int (i-1)  
let _EP         = Int 0x60         (* Escaping Variable Record Pointer *) 
let _EP_MIN     = Int 0x100
let _EP_MAX     = let Int i = _MS_MIN in Int (i-2)

let push_MSP    = PUSH1 _MSP
let push_EP     = PUSH1 _EP 
let push_MS_MIN = PUSH4 _MS_MIN
let push_MS_MAX = PUSH4 _MS_MAX
let push_EP_MIN = PUSH4 _EP_MIN
let push_EP_MAX = PUSH4 _EP_MAX

let mPUSH_from_STACK ce = 
    let ce      = push_MSP                          =>> ce  in  (*                                                         sp >> x >> .. *)
    let ce      = MLOAD                             =>> ce  in  (*                                                      M[sp] >> x >> .. *) 
    let ce      = check_NOT_GT _MS_MAX                  ce  in  (*                                                      M[sp] >> x >> .. *)
    let ce      = DUP1                              =>> ce  in  (*                                             M[sp] >> M[sp] >> x >> .. *)         
    let ce      = PUSH1(Int 0x20)                   =>> ce  in  (*                                    0x20 >>  M[sp] >> M[sp] >> x >> .. *)
    let ce      = ADD                               =>> ce  in  (*                                        0x20+M[sp] >> M[sp] >> x >> .. *)
    let ce      = push_MSP                          =>> ce  in  (*                                  SP >> 0x20+M[sp] >> M[sp] >> x >> .. *)
    let ce      = MSTORE                            =>> ce  in  (* M[sp]    := M[sp]+0x20                               M[sp] >> x >> .. *)
                  MSTORE                            =>> ce      (* M[M[sp]] := x                                                      .. *) 

let mPUSH      x  ce = mPUSH_from_STACK ( PUSH32 x  =>> ce  ) 

let ePUSH         ce =                                          (*                                                   x >> retlabel >> .. *) 
    let ce      = push_EP                           =>> ce  in  (*                                             EP >> x >> retlabel >> .. *)
    let ce      = MLOAD                             =>> ce  in  (*                                          M[EP] >> x >> retlabel >> .. *)
    let ce      = check_NOT_GT _EP_MAX                  ce  in  (*                                          M[EP] >> x >> retlabel >> .. *)
    let ce      = DUP1                              =>> ce  in  (*                                 M[EP] >> M[EP] >> x >> retlabel >> .. *)
    let ce      = PUSH1(Int 0x40)                   =>> ce  in  (*                         0x40 >> M[EP] >> M[EP] >> x >> retlabel >> .. *)
    let ce      = ADD                               =>> ce  in  (*                            0x40+M[EP] >> M[EP] >> x >> retlabel >> .. *)
    let ce      = push_EP                           =>> ce  in  (*                      EP >> 0x40+M[EP] >> M[EP] >> x >> retlabel >> .. *)
    let ce      = MSTORE                            =>> ce  in  (* M[EP] := M[EP]+0x40                      M[EP] >> x >> retlabel >> .. *)
    let ce      = SWAP1                             =>> ce  in  (*                                          x >> M[EP] >> retlabel >> .. *)              
    let ce      = DUP2                              =>> ce  in  (*                                M[EP] >>  x >> M[EP] >> retlabel >> .. *) 
    let ce      = MSTORE                            =>> ce  in  (* M[M[EP]] := x                                 M[EP] >> retlabel >> .. *)
    let ce      = push_EP                           =>> ce  in  (*                                       0x20 >> M[EP] >> retlabel >> .. *)
    let ce      = ADD                               =>> ce  in  (*                                          0x20+M[EP] >> retlabel >> .. *)
                  MSTORE                            =>> ce      (* M[M[EP]+0x20] := retAddr                                           .. *)

let get_escaped_arg ce =                                        (*                                                                    .. *)
    let ce      = PUSH1 (Int 0x40)                  =>> ce  in  (*                                                            0x40 >> .. *)
    let ce      = push_EP                           =>> ce  in  (*                                                      EP >> 0x40 >> .. *)
    let ce      = MLOAD                             =>> ce  in  (*                                                   M[EP] >> 0x40 >> .. *)
    let ce      = SUB                               =>> ce  in  (*                                                      M[EP]-0x40 >> .. *)
                  MLOAD                             =>> ce      (*                                                     escaped_arg >> .. *)

let ePOP          ce =                                          (*                                                                    .. *)  
    let ce      = push_EP                           =>> ce  in  (*                                                             EP  >> .. *)
    let ce      = MLOAD                             =>> ce  in  (*                                                           M[EP] >> .. *) 
    let ce      = PUSH1 (Int 0x40)                  =>> ce  in  (*                                                   0x40 >> M[EP] >> .. *)
    let ce      = PUSH1 (Int 0x20)                  =>> ce  in  (*                                           0x20 >> 0x40 >> M[EP] >> .. *) 
    let ce      = DUP2                              =>> ce  in  (*                                  M[EP] >> 0x20 >> 0x40 >> M[EP] >> .. *)  
    let ce      = SUB                               =>> ce  in  (*                                     M[EP]-0x20 >> 0x40 >> M[EP] >> .. *)
    let ce      = check_NOT_LT _EP_MIN                  ce  in  (*                                     M[EP]-0x20 >> 0x40 >> M[EP] >> .. *)
    let ce      = MLOAD                             =>> ce  in  (*                                        retAddr >> 0x40 >> M[EP] >> .. *)
    let ce      = SWAP2                             =>> ce  in  (*                                        M[EP] >> 0x40 >> retAddr >> .. *)
    let ce      = SUB                               =>> ce  in  (*                                           M[EP]-0x40 >> retAddr >> .. *)
    let ce      = push_EP                           =>> ce  in  (*                                     EP >> M[EP]-0x40 >> retAddr >> .. *)
                  MSTORE                            =>> ce      (* M[EP] := M[EP]-0x40                                     retAddr >> .. *)

let mPOP_to_STACK ce = 
    let ce      = PUSH1 (Int 0x20)                  =>> ce  in  (*                                                            0x20 >> .. *) 
    let ce      = push_MSP                          =>> ce  in  (*                                                      sp >> 0x20 >> .. *)
    let ce      = MLOAD                             =>> ce  in  (*                                                   M[sp] >> 0x20 >> .. *) 
    let ce      = check_NOT_LT _MS_MIN                  ce  in  (*                                                   M[sp] >> 0x20 >> .. *)    
    let ce      = SUB                               =>> ce  in  (*                                                      M[sp]-0x20 >> .. *)
    let ce      = DUP1                              =>> ce  in  (*                                        M[sp]-0x20 >> M[sp]-0x20 >> .. *)
    let ce      = push_MSP                          =>> ce  in  (*                                  SP >> M[sp]-0x20 >> M[sp]-0x20 >> .. *) 
    let ce      = MSTORE                            =>> ce  in  (* M[sp] := M[sp]-0x20                                  M[sp]-0x20 >> .. *) 
                  MLOAD                             =>> ce      (*                                                   M[M[sp]-0x20] >> .. *)

let mPOP    ce  = 
    let ce      = PUSH1 (Int 0x20)                  =>> ce  in  (*                                                            0x20 >> .. *) 
    let ce      = push_MSP                          =>> ce  in  (*                                                     SP  >> 0x20 >> .. *)
    let ce      = MLOAD                             =>> ce  in  (*                                                   M[SP] >> 0x20 >> .. *) 
    let ce      = check_NOT_LT _MS_MIN                  ce  in  (*                                                   M[SP] >> 0x20 >> .. *)
    let ce      = SUB                               =>> ce  in  (*                                                      M[SP]-0x20 >> .. *)
    let ce      = push_MSP                          =>> ce  in  (*                                                SP >> M[SP]-0x20 >> .. *) 
                  MSTORE                            =>> ce      (* M[SP] := M[SP]-0x20                                             >> .. *) 




(**  [malloc]                              Addr     Val              Addr     Val     
 *                                         +--------+--------+       +--------+--------+
 *                                         | 0x40   |   a    |       | 0x40   | a+size |
 *      BEFORE            AFTER            +--------+--------+       +--------+--------+
 *                                         | ...    |  ...   |       | ...    |  ...   |
 *   +----------+      +----------+        +--------+--------+       +--------+--------+
 *   |   size   |      |    a     |      -->   a    |     0  |       |   a    |     0  |
 * --+----------+--  --+----------+--      +--------+--------+       +--------+--------+
 *                                         | ...    |     0  |       | ...    |     0  |
 *  malloc :=                              +--------+--------+       +--------+--------+
 *      size := pop();                     | a+size |        |     --> a+size |        |
 *      a    := alloc(size);               +--------+--------+       +--------+--------+
 *      push(a)                                BEFORE MEM                AFTER MEM      *)

let init_malloc ce =                                            (* initialize as M[64] := 96  ( M[0x40] := 0x60 ) *)
    let ce      = PUSH1 (Int 0x60)                  =>> ce  in
    let ce      = PUSH1 (Int 0x40)                  =>> ce  in
                  MSTORE                            =>> ce    

let malloc ce   =                                               (*  STACK                                            len >> .. *)
    let ce      = PUSH1 (Int 0x40)                  =>> ce  in  (*                                             64 >> len >> .. *)
    let ce      = DUP1                              =>> ce  in  (*                                       64 >> 64 >> len >> .. *)
    let ce      = MLOAD                             =>> ce  in  (*                                    M[64] >> 64 >> len >> .. *)
    let ce      = DUP1                              =>> ce  in  (*                           M[64] >> M[64] >> 64 >> len >> .. *)
    let ce      = SWAP3                             =>> ce  in  (*                           len >> M[64] >> 64 >> M[64] >> .. *)
    let ce      = ADD                               =>> ce  in  (*                              M[64+len] >> 64 >> M[64] >> .. *)
    let ce      = SWAP1                             =>> ce  in  (*                              64 >> M[64+len] >> M[64] >> .. *)
                  MSTORE                            =>> ce      (*                                                 M[64] >> .. *) 

let get_malloc ce    =                               
    let ce      = PUSH1 (Int 0x40)                  =>> ce  in  (*                                                0x40   >> .. *) 
                  MLOAD                             =>> ce      (*                                              M[0x40]  >> .. *) 
      
let mstore_code ce =                                            (*                                           idx >> size >> .. *)
    let ce      = DUP2                              =>> ce  in  (*                                   size >> idx >> size >> .. *)
    let ce      = malloc                                ce  in  (*                            alloc(size) >> idx >> size >> .. *)
    let ce      = SWAP1                             =>> ce  in  (*                            idx >> alloc(size) >> size >> .. *)
    let ce      = DUP3                              =>> ce  in  (*                    size >> idx >> alloc(size) >> size >> .. *)
    let ce      = SWAP1                             =>> ce  in  (*                    idx >> size >> alloc(size) >> size >> .. *)
    let ce      = DUP3                              =>> ce  in  (*     alloc(size) >> idx >> size >> alloc(size) >> size >> .. *) 
                  CODECOPY                          =>> ce      (*                                   alloc(size) >> size >> .. *)  

let mstore_whole_code ce =
    let ce      = CODESIZE                          =>> ce  in  (*                                                  size >> .. *)
    let ce      = DUP1                              =>> ce  in  (*                                          size >> size >> .. *)
    let ce      = malloc                                ce  in  (*                                   alloc(size) >> size >> .. *)
    let ce      = DUP2                              =>> ce  in  (*                           size >> alloc(size) >> size >> .. *)
    let ce      = PUSH1(Int 0)                      =>> ce  in  (*                     0  >> size >> alloc(size) >> size >> .. *)
    let ce      = DUP3                              =>> ce  in  (*     alloc(size) >>  0  >> size >> alloc(size) >> size >> .. *)
                  CODECOPY                          =>> ce      (*       to           from           alloc(size) >> size >> .. *)

let push_mthd_hash m ce =
    let b       = Crpt.(big_of_hex $ hash_ty_mthd)m         in  
                  PUSH4(Big b)                      =>> ce    

let push_evnt_hash ev ce =
    let b       = Crpt.(big_of_hex $ hash_of_evnt)ev        in  
                  PUSH4(Big b)                      =>> ce             

let mstore_mthd_hash mthd ce =
    let ce      = PUSH1(Int 4)                      =>> ce  in  (*                                                     4 >> .. *)
    let ce      = DUP1                              =>> ce  in  (*                                               4  >> 4 >> .. *)
    let ce      = malloc                                ce  in  (*                                         alloc(4) >> 4 >> .. *)
    let ce      = push_mthd_hash mthd                   ce  in  (*                                 hash >> alloc(4) >> 4 >> .. *)
    let ce      = DUP2                              =>> ce  in  (*                     alloc(4) >> hash >> alloc(4) >> 4 >> .. *)
                  MSTORE                            =>> ce      (* M[alloc(4)] := hash                     alloc(4) >> 4 >> .. *)


