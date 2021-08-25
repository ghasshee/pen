
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

module Eth  = Crypto 
module BL   = BatList
module L    = List


(******************************************************)
(***     1. ERROR HANDLING                          ***)
(******************************************************)

let throw ce         = 
    let ce      =   PUSH1(Int 0)                    >>ce    in
                    JUMP                            >>ce 

let throw_if_0 ce    =                                          (*                                  i >> .. *)   
    let ce      =   DUP1                            >>ce    in  (*                             i >> i >> .. *)
    let ce      =   ISZERO                          >>ce    in  (*                             b >> i >> .. *)
    let ce      =   PUSH1(Int 0)                    >>ce    in  (*                        0 >> b >> i >> .. *)
                    JUMPI                           >>ce        (* {GOTO 0 if b}                    i >> .. *)
      
let throw_if_NEQ ce   =                                         (*                             a >> b >> .. *) 
    let ce      =   EQ                              >>ce    in  (*                               a==b >> .. *)
    let ce      =   ISZERO                          >>ce    in  (*                               a!=b >> .. *)    
    let ce      =   PUSH1(Int 0)                    >>ce    in  (*                          0 >> a!=b >> .. *)
                    JUMPI                           >>ce        (* {IF a!=b THEN GOTO 0}                 .. *)

let if_0_GOTO lbl ce = 
    let ce      =   ISZERO                          >>ce    in 
    let ce      =   PUSH4(Label lbl)                >>ce    in 
                    JUMPI                           >>ce    

let goto la   ce    = 
    let ce      =   PUSH4(Label la)                 >>ce    in 
                    JUMP                            >>ce 

let repeat opcode n ce = foldn n ((>>)opcode) ce


(******************************************************)
(***     2. STACK OPERATIONS                        ***)
(******************************************************)

let push_storRange ce (data : imm data) =
    let i = match data.size with | Big b -> string_of_big b | Int i -> string_of_int i in 
    printf "stor_size is %s\n" i ; 
    assert (is_const_int 1 data.size) ; 
    let ce      =   PUSH32 data.offst               >>ce    in
                    SLOAD                           >>ce 

let dup_nth_from_bottom n ce  =
    let diff   =(get_stack_size ce)-n in assert(diff>=0) ; 
                dup_succ diff                       >>ce 

let shiftRtop ce bits =
    assert (0 <= bits && bits < 256) ; 
    let ce      = Comment(sprintf "ShiftR %d bits" bits)>>ce in 
    if bits=0 then ce else                                      (*                 x >> .. *) 
    let ce      = PUSH1 (Int bits)                  >>ce    in  (*         bits >> x >> .. *)
    let ce      = PUSH1 (Int 2)                     >>ce    in  (*    2 >> bits >> x >> .. *)
    let ce      = EXP                               >>ce    in  (*      2**bits >> x >> .. *) 
    let ce      = SWAP1                             >>ce    in  (*      x >> 2**bits >> .. *) 
                  DIV                               >>ce        (*       x/(2**bits) >> .. *) 

let shiftLtop ce bits =
    assert (0 <= bits && bits < 256) ; 
    if bits=0 then ce else                                      (*                 x >> .. *)
    let ce      = PUSH1 (Int bits)                  >>ce    in  (*         bits >> x >> .. *)                   
    let ce      = PUSH1 (Int 2)                     >>ce    in  (*    2 >> bits >> x >> .. *) 
    let ce      = EXP                               >>ce    in  (*      2**bits >> x >> .. *) 
                  MUL                               >>ce        (*       (2**bits)*x >> .. *) 

let incr_top (inc : int) ce =
    let ce      = PUSH32 (Int inc)                  >>ce    in
                  ADD                               >>ce      

let sincr (idx: int) ce = 
    let ce      = PUSH1(Int idx)                    >>ce    in  (*                                      i >> .. *) 
    let ce      = SLOAD                             >>ce    in  (*                                   S[i] >> .. *) 
    let ce      = DUP1                              >>ce    in  (*                           S[i] >> S[i] >> .. *) 
    let ce      = incr_top 1                          ce    in  (*                         S[i]+1 >> S[i] >> .. *) 
    let ce      = PUSH1(Int idx)                    >>ce    in  (*                    i >> S[i]+1 >> S[i] >> .. *) 
                  SSTORE                            >>ce        (* S[i]:=S[i]+1                      S[i] >> .. *) 

let calldataload ce data =
    assert (0 < data.size && data.size <= 32);
    let ce      = PUSH4 (Int data.offst)            >>ce    in
                  CALLDATALOAD                      >>ce  

let keccak_cat ce =                                             (*                                               a >> b >> .. *) 
    let ce      = PUSH1 (Int 0x00)                  >>ce    in  (*                                       0x00 >> a >> b >> .. *)
    let ce      = MSTORE                            >>ce    in  (* M[0x00]=a                                          b >> .. *)
    let ce      = PUSH1 (Int 0x20)                  >>ce    in  (*                                            0x20 >> b >> .. *)
    let ce      = MSTORE                            >>ce    in  (* M[0x20]=b                                               .. *) 
    let ce      = PUSH1 (Int 0x40)                  >>ce    in  (*                                                 0x40 >> .. *)
    let ce      = PUSH1 (Int 0x00)                  >>ce    in  (*                                         0x0  >> 0x40 >> .. *)
                  SHA3                              >>ce        (*                                  sha3(M[0x00..0x3F]) >> .. *)
                                                                (*                                     sha3(a++b)             *)


(******************************************************)
(***     4. PROGRAM COUNTER on STORAGE              ***)
(******************************************************)
                
let reset_PC ce   =
    let ce    = PUSH1 (Int 0)                       >>ce    in  (*                             0 >> .. *)
    let ce    = SLOAD                               >>ce    in  (*                          S[0] >> .. *)
    let ce    = PUSH1 (Int 0)                       >>ce    in  (*                     0 >> S[0] >> .. *)
    let ce    = DUP1                                >>ce    in  (*                0 >> 0 >> S[0] >> .. *)
                SSTORE                              >>ce        (* S'[0]=0                  S[0] >> .. *)


(** [restore_PC]   *)  
(*                                                 
 *     BEFORE             AFTER                    
 *    +--------+                                     
 *    | bkp_pc |                                     
 *  --+--------+--    --+--------+--  *)
let restore_PC ce       =                                        (*                   bkp_pc >> .. *)
    let ce    = PUSH1(Int 0)                        >>ce    in   (*              0 >> bkp_pc >> .. *)
                SSTORE                              >>ce         (* S'[0]=bkp_pc                .. *)             



(****************************************)
(***     5. MEMORY OPERATIONS         ***)
(****************************************)

(**  [malloc]                              Addr     Val              Addr     Val     
 *                                         +--------+--------+       +--------+--------+
 *                                         |   64   |   a    |       |   64   | a+size |
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

let malloc ce    =                                              (*  STACK                                            len >> .. *)
    let ce    = PUSH1 (Int 0x40)                    >>ce    in  (*                                             64 >> len >> .. *)
    let ce    = DUP1                                >>ce    in  (*                                       64 >> 64 >> len >> .. *)
    let ce    = MLOAD                               >>ce    in  (*                                    M[64] >> 64 >> len >> .. *)
    let ce    = DUP1                                >>ce    in  (*                           M[64] >> M[64] >> 64 >> len >> .. *)
    let ce    = SWAP3                               >>ce    in  (*                           len >> M[64] >> 64 >> M[64] >> .. *)
    let ce    = ADD                                 >>ce    in  (*                              M[64+len] >> 64 >> M[64] >> .. *)
    let ce    = SWAP1                               >>ce    in  (*                              64 >> M[64+len] >> M[64] >> .. *)
                MSTORE                              >>ce        (*                                                 M[64] >> .. *) 

let get_malloc ce    =                                 
    let ce    = PUSH1 (Int 0x40)                    >>ce    in  (*           0x40   >> .. *) 
                MLOAD                               >>ce        (*         M[0x40]  >> .. *) 
      
let mstore_code ce =                                            (*                                            idx >> size >> .. *)
    let ce    = DUP2                                >>ce    in  (*                                    size >> idx >> size >> .. *)
    let ce    = malloc                                ce    in  (*                             alloc(size) >> idx >> size >> .. *)
    let ce    = SWAP1                               >>ce    in  (*                             idx >> alloc(size) >> size >> .. *)
    let ce    = DUP3                                >>ce    in  (*                     size >> idx >> alloc(size) >> size >> .. *)
    let ce    = SWAP1                               >>ce    in  (*                     idx >> size >> alloc(size) >> size >> .. *)
    let ce    = DUP3                                >>ce    in  (*      alloc(size) >> idx >> size >> alloc(size) >> size >> .. *) 
                CODECOPY                            >>ce        (*                                    alloc(size) >> size >> .. *)  

let mstore_whole_code ce =
    let ce    = CODESIZE                            >>ce    in  (*                                                   size >> .. *)
    let ce    = DUP1                                >>ce    in  (*                                           size >> size >> .. *)
    let ce    = malloc                                ce    in  (*                                    alloc(size) >> size >> .. *)
    let ce    = DUP2                                >>ce    in  (*                            size >> alloc(size) >> size >> .. *)
    let ce    = PUSH1(Int 0)                        >>ce    in  (*                      0  >> size >> alloc(size) >> size >> .. *)
    let ce    = DUP3                                >>ce    in  (*      alloc(size) >>  0  >> size >> alloc(size) >> size >> .. *)
                CODECOPY                            >>ce        (*        to           from           alloc(size) >> size >> .. *)

let push_mthd_hash m ce =
    let b     = Eth.(big_of_hex $ hash_ty_mthd)m            in  
                PUSH4(Big b)                        >>ce    

let push_evnt_hash ev ce =
    let b     = Eth.(big_of_hex $ hash_of_evnt)ev           in  
                PUSH4(Big b)                        >>ce             

let mstore_mthd_hash mthd ce =
    let ce    = PUSH1(Int 4)                        >>ce    in  (*                                                       4 >> .. *)
    let ce    = DUP1                                >>ce    in  (*                                                 4  >> 4 >> .. *)
    let ce    = malloc                                ce    in  (*                                           alloc(4) >> 4 >> .. *)
    let ce    = push_mthd_hash mthd                   ce    in  (*                                   hash >> alloc(4) >> 4 >> .. *)
    let ce    = DUP2                                >>ce    in  (*                       alloc(4) >> hash >> alloc(4) >> 4 >> .. *)
                MSTORE                              >>ce        (* M[alloc(4)] := hash                       alloc(4) >> 4 >> .. *)



(**   6.1. Init MemAlloc       *****)
let init_malloc ce =                                            (* initialize as M[64] := 96  ( M[0x40] := 0x60 ) *)
    let ce      = PUSH1 (Int 0x60)                  >>ce    in
    let ce      = PUSH1 (Int 0x40)                  >>ce    in
                  MSTORE                            >>ce    

(**   6.2.  CONTRACT PC        *****) 
let set_cntrct_pc ce idx =                                      (*                                                       .. *)
    let ce      = PUSH32(RntimeCntrctOffset idx)    >>ce    in  (*                                       rn_cn_offset >> .. *) 
    let ce      = PUSH32 StorPCIndex                >>ce    in  (*                             storPC >> rn_cn_offset >> .. *) 
                  SSTORE                            >>ce        (* S[storPC] := rn_cn_offset                             .. *) 

let get_cntrct_pc ce =
    let ce      = PUSH1 StorPCIndex                 >>ce    in
                  SLOAD                             >>ce 
