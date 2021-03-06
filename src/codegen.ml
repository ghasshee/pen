(* M[64]  :=   the address of mem alloc     *) 
(* MSTORE :=   x=pop() ; y=pop() ; M[x]=y   *) 
(* MLOAD  :=   x=pop() ; push M[x]          *) 
(* CODECOPY  to from len :=  M[to .. to+len-1]=I_b[from .. from+len-1]  *)

open Label
open Big_int
open Printf 
open Misc
open IndexList
open Location
open CodegenEnv
open LocationEnv
open Evm
open Syntax
open TypeEnv
open Type

module Eth  = Crypto 
module BL   = BatList
module L    = List
module SL   = StorLayout

(******************************************************)
(***     1. ERROR HANDLING                          ***)
(******************************************************)

let throw ce         = (* the same with solc. *)
    let ce      =   PUSH1(Int 2)                    >>ce    in
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

let push_storRange ce (range : imm stor_range) =
    let i = match range.stor_size with | Big b -> string_of_big b | Int i -> string_of_int i in 
    printf "stor_size is %s\n" i ; 
    assert (is_const_int 1 range.stor_size) ; 
    let offset  = range.stor_start                          in
    let ce      =   PUSH32 offset                   >>ce    in
                    SLOAD                           >>ce 

let dup_nth_from_bottom n ce  =
    let diff   =(stack_size ce)-n in assert(diff>=0) ; 
                dup_succ diff                       >>ce 

let shiftRtop ce bits =
    assert (0 <= bits && bits < 256) ; 
    if bits=0 then ce else                                      (*                 x >> .. *) 
    let ce    = PUSH1 (Int bits)                    >>ce    in  (*         bits >> x >> .. *)
    let ce    = PUSH1 (Int 2)                       >>ce    in  (*    2 >> bits >> x >> .. *)
    let ce    = EXP                                 >>ce    in  (*      2**bits >> x >> .. *) 
    let ce    = SWAP1                               >>ce    in  (*      x >> 2**bits >> .. *) 
                DIV                                 >>ce        (*       x/(2**bits) >> .. *) 

let shiftLtop ce bits =
    assert (0 <= bits && bits < 256) ; 
    if bits=0 then ce else                                      (*                 x >> .. *)
    let ce    = PUSH1 (Int bits)                    >>ce    in  (*         bits >> x >> .. *)                   
    let ce    = PUSH1 (Int 2)                       >>ce    in  (*    2 >> bits >> x >> .. *) 
    let ce    = EXP                                 >>ce    in  (*      2**bits >> x >> .. *) 
                MUL                                 >>ce        (*       (2**bits)*x >> .. *) 

let incr_top (inc : int) ce =
    let ce    = PUSH32 (Int inc)                    >>ce    in
                ADD                                 >>ce      

let sincr (idx : int) ce = 
    let ce    = PUSH1(Int idx)                      >>ce    in  (*                                      i >> .. *) 
    let ce    = SLOAD                               >>ce    in  (*                                   S[i] >> .. *) 
    let ce    = DUP1                                >>ce    in  (*                           S[i] >> S[i] >> .. *) 
    let ce    = incr_top 1                            ce    in  (*                         S[i]+1 >> S[i] >> .. *) 
    let ce    = PUSH1(Int idx)                      >>ce    in  (*                    i >> S[i]+1 >> S[i] >> .. *) 
                SSTORE                              >>ce        (* S[i]:=S[i]+1                      S[i] >> .. *) 

let calldataload ce (range : calldata_range) =
    let start = range.calldata_start                        in 
    let size  = range.calldata_size                         in 
    assert (0 < size && size <= 32);
    let ce    = PUSH4 (Int start)                   >>ce    in
                CALLDATALOAD                        >>ce  

let keccak_cat ce =                                             (*                                               a >> b >> .. *) 
    let ce    = PUSH1 (Int 0x00)                    >>ce    in  (*                                       0x00 >> a >> b >> .. *)
    let ce    = MSTORE                              >>ce    in  (* M[0x00]=a                                          b >> .. *)
    let ce    = PUSH1 (Int 0x20)                    >>ce    in  (*                                            0x20 >> b >> .. *)
    let ce    = MSTORE                              >>ce    in  (* M[0x20]=b                                               .. *) 
    let ce    = PUSH1 (Int 0x40)                    >>ce    in  (*                                                 0x40 >> .. *)
    let ce    = PUSH1 (Int 0x00)                    >>ce    in  (*                                         0x0  >> 0x40 >> .. *)
                SHA3                                >>ce        (*                                  sha3(M[0x00..0x3F]) >> .. *)
                                                                (*                                     sha3(a++b)             *)

(******************************************************)
(***     3. ALIGNMENT R ? L ?                       ***)
(******************************************************)
                
type alignment              = L 
                            | R

let align_addr ce       = function 
    | R                 ->  ce
    | L                 ->  shiftLtop ce (12 * 8)
let align_to_L ce ty    = function 
    | R                 ->  ce
    | L                 ->  let size = size_of_ty ty in
                            assert (size <= 32) ;
                            shiftLtop ce ((32-size)*8) 

let push_loc ce aln ty  = function 
    | Code       _      ->  err "push_loc: Code"  
    | Calldata rng      ->  calldataload  ce rng
    | Stor     rng      ->  let ce = push_storRange ce rng in 
                            align_to_L ce ty aln  
    | Stack      n      ->  let ce = dup_nth_from_bottom n ce in 
                            align_to_L ce ty aln 


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

let malloc ce    =                                          (*  STACK                                            len >> .. *)
    let ce    = PUSH1 (Int 64)                  >>ce    in  (*                                             64 >> len >> .. *)
    let ce    = DUP1                            >>ce    in  (*                                       64 >> 64 >> len >> .. *)
    let ce    = MLOAD                           >>ce    in  (*                                    M[64] >> 64 >> len >> .. *)
    let ce    = DUP1                            >>ce    in  (*                           M[64] >> M[64] >> 64 >> len >> .. *)
    let ce    = SWAP3                           >>ce    in  (*                           len >> M[64] >> 64 >> M[64] >> .. *)
    let ce    = ADD                             >>ce    in  (*                              M[64+len] >> 64 >> M[64] >> .. *)
    let ce    = SWAP1                           >>ce    in  (*                              64 >> M[64+len] >> M[64] >> .. *)
                MSTORE                          >>ce        (*                                                 M[64] >> .. *) 

let get_malloc ce    =   (* 64 == 0x40 *)                           
    let ce    = PUSH1 (Int 64)                  >>ce    in  (* 64    >> .. *) 
                MLOAD                           >>ce        (* M[64] >> .. *) 
      
let mstore_code ce =                                        (*                                            idx >> size >> .. *)
    let ce    = DUP2                            >>ce    in  (*                                    size >> idx >> size >> .. *)
    let ce    = malloc                            ce    in  (*                             alloc(size) >> idx >> size >> .. *)
    let ce    = SWAP1                           >>ce    in  (*                             idx >> alloc(size) >> size >> .. *)
    let ce    = DUP3                            >>ce    in  (*                     size >> idx >> alloc(size) >> size >> .. *)
    let ce    = SWAP1                           >>ce    in  (*                     idx >> size >> alloc(size) >> size >> .. *)
    let ce    = DUP3                            >>ce    in  (*      alloc(size) >> idx >> size >> alloc(size) >> size >> .. *) 
                CODECOPY                        >>ce        (*                                    alloc(size) >> size >> .. *)  

let mstore_whole_code ce =
    let ce    = CODESIZE                        >>ce    in  (*                                                   size >> .. *)
    let ce    = DUP1                            >>ce    in  (*                                           size >> size >> .. *)
    let ce    = malloc                            ce    in  (*                                    alloc(size) >> size >> .. *)
    let ce    = DUP2                            >>ce    in  (*                            size >> alloc(size) >> size >> .. *)
    let ce    = PUSH1(Int 0)                    >>ce    in  (*                      0  >> size >> alloc(size) >> size >> .. *)
    let ce    = DUP3                            >>ce    in  (*      alloc(size) >>  0  >> size >> alloc(size) >> size >> .. *)
                CODECOPY                        >>ce        (*        to           from           alloc(size) >> size >> .. *)

let push_mthd_hash m ce =
    let b     = Eth.(big_of_hex $ hash_ty_mthd)m        in  
                PUSH4(Big b)                    >>ce    

let push_evnt_hash ev ce =
    let b     = Eth.(big_of_hex $ hash_of_evnt)ev       in  
                PUSH4(Big b)                    >>ce             

let mstore_mthd_hash mthd ce =
    let ce    = PUSH1(Int 4)                    >>ce    in  (*                                                       4 >> .. *)
    let ce    = DUP1                            >>ce    in  (*                                                 4  >> 4 >> .. *)
    let ce    = malloc                            ce    in  (*                                           alloc(4) >> 4 >> .. *)
    let ce    = push_mthd_hash mthd               ce    in  (*                                   hash >> alloc(4) >> 4 >> .. *)
    let ce    = DUP2                            >>ce    in  (*                       alloc(4) >> hash >> alloc(4) >> 4 >> .. *)
                MSTORE                          >>ce        (* M[alloc(4)] := hash                       alloc(4) >> 4 >> .. *)

type memoryPack             = TightPack   (* [Tight] uses [size_of_ty] bytes    on mem *) 
                            | ABIPack     (* [ABI]   uses multiples of 32 bytes on mem *) 


(*****************************************)
(***     6. CONTRACT CREATION          ***)
(*****************************************)

(*****   6.1. Init MemAlloc       *****)
let init_malloc ce =                                            (* initialize as M[64] := 96  ( M[0x40] := 0x60 ) *)
    let ce    = PUSH1 (Int 96)                  >>ce    in
    let ce    = PUSH1 (Int 64)                  >>ce    in
                MSTORE                          >>ce    

(*****   6.2.  CONTRACT PC        *****) 
let set_cntrct_pc ce idx =                                  (*                                                       .. *)
    let ce    = PUSH32(RntimeCntrctOffset idx)  >>ce    in  (*                                       rn_cn_offset >> .. *) 
    let ce    = PUSH32 StorPCIndex              >>ce    in  (*                             storPC >> rn_cn_offset >> .. *) 
                SSTORE                          >>ce        (* S[storPC] := rn_cn_offset                             .. *) 

let get_cntrct_pc ce =
    let ce    = PUSH32 StorPCIndex              >>ce    in
                SLOAD                           >>ce 

(*****   6.3.  setup ARGS         *****) 
let mstore_cnstrArgs ce cn      =                     (* [mstore_cnstrArgs] copies cnstrArgs at the end of the bytecode into the memory.  *) 
    let args    = L.map snd (argTys_of_cntrct cn)           in  (* M[0x40](==M[64]) is increased accordingly                              *)
    let size    = total_size_of_argTys args                 in  (*                                                                     .. *)
    let ce      = PUSH32(Int size)                  >>ce    in  (*                                                             size >> .. *)
    let ce      = DUP1                              >>ce    in  (*                                                     size >> size >> .. *)
    let ce      = malloc                              ce    in  (*                                              alloc(size) >> size >> .. *)
    let ce      = DUP2                              >>ce    in  (*                                      size >> alloc(size) >> size >> .. *)
    let ce      = DUP1                              >>ce    in  (*                              size >> size >> alloc(size) >> size >> .. *)
    let ce      = CODESIZE                          >>ce    in  (*                  codesize >> size >> size >> alloc(size) >> size >> .. *)
    let ce      = SUB                               >>ce    in  (*                     codesize-size >> size >> alloc(size) >> size >> .. *)
    let ce      = DUP3                              >>ce    in  (*      alloc(size) >> codesize-size >> size >> alloc(size) >> size >> .. *)
                  CODECOPY                          >>ce        (*          to             from                 alloc(size) >> size >> .. *)
                                                                (*                                               codebign                 *)
let check_codesize cnidx ce     =  
    let ce      = PUSH32(InitDataSize cnidx)        >>ce    in  (*                                    datasize >> mem_start >> size >> .. *)
    let ce      = CODESIZE                          >>ce    in  (*                        codesize >> datasize >> mem_start >> size >> .. *) 
                  throw_if_NEQ                        ce        (* IF not eq THEN error *) 

let sstore_cnstrArgs ce idx     =                               
    let label   = fresh_label()                             in
    let exit    = fresh_label()                             in  (*                                                mem_start >> size >> .. *)  
    let ce      = check_codesize idx                  ce    in 
    let ce      = PUSH32(StorCnstrctrArgsBegin idx) >>ce    in  (*                                         idx >> mem_start >> size >> .. *)
    let ce   = JUMPDEST label                       >>ce    in  (*                                         idx >> mem_start >> size >> .. *)
    let ce      = DUP3                              >>ce    in  (*                                 size >> idx >> mem_start >> size >> .. *)
    let ce      = if_0_GOTO exit                      ce    in  (* IF size==0 THEN GOTO exit               idx >> mem_start >> size >> .. *)   
    let ce      = DUP2                              >>ce    in  (*                            mem_start >> idx >> mem_start >> size >> .. *) 
    let ce      = MLOAD                             >>ce    in  (*                         M[mem_start] >> idx >> mem_start >> size >> .. *)
    let ce      = DUP2                              >>ce    in  (*                  idx >> M[mem_start] >> idx >> mem_start >> size >> .. *)
    let ce      = SSTORE                            >>ce    in  (* S[idx]=M[mem_start]                     idx >> mem_start >> size >> .. *)  
    let ce      = PUSH32(Int 32)                    >>ce    in  (*                                   32 >> idx >> mem_start >> size >> .. *)
    let ce      = SWAP1                             >>ce    in  (*                                   idx >> 32 >> mem_start >> size >> .. *)
    let ce      = SWAP3                             >>ce    in  (*                                   size >> 32 >> mem_start >> idx >> .. *)
    let ce      = SUB                               >>ce    in  (*                                      size-32 >> mem_start >> idx >> .. *)
    let ce      = SWAP2                             >>ce    in  (*                                      idx >> mem_start >> size-32 >> .. *) 
    let ce      = incr_top 1(*word*)                  ce    in  (*                                    idx+1 >> mem_start >> size-32 >> .. *)
    let ce      = SWAP1                             >>ce    in  (*                                    mem_start >> idx+1 >> size-32 >> .. *)
    let ce      = incr_top 32                         ce    in  (*                                 mem_start+32 >> idx+1 >> size-32 >> .. *)
    let ce      = SWAP1                             >>ce    in  (*                                 idx+1 >> mem_start+32 >> size-32 >> .. *)
    let ce      = goto label                          ce    in  (*                                 idx+1 >> mem_start+32 >> size-32 >> .. *)
    let ce   = JUMPDEST exit                        >>ce    in  (*                                         idx >> mem_start >> size >> .. *)
                  repeat POP 3                        ce        (*                                                                     .. *)
(*  S[1]   := m   <- array seed                                               *)  
(*  S[2]   := the value of arg1 is stored in message call                     *)  
(*            ..                                                              *)  
(*  S[k+1] := the value of argk is stored in message call                     *)  

(*****   6.4. setup ARG ARRAYs     *****)                   
let reset_salloc_array ce = 
    let ce      = PUSH1 (Int 1)                     >>ce    in  (*                                           1 >> .. *)
    let ce      = DUP1                              >>ce    in  (*                                      1 >> 1 >> .. *)
                  SSTORE                            >>ce        (* S[1]:=1                                        .. *) 

let salloc_argArr ce (arrArgLoc:int) =   
    let label   = fresh_label()                             in  (*                                                .. *) 
    let ce      = PUSH4 (Int arrArgLoc)             >>ce    in  (*                                        seed >> .. *)
    let ce      = SLOAD                             >>ce    in  (*                                     S[seed] >> .. *) 
    let ce      = PUSH4 (Label label)               >>ce    in  (*                            label >> S[seed] >> .. *)
    let ce      = JUMPI                             >>ce    in  (* IF S[seed]!=0 GOTO label                       .. *) 
    let ce      = sincr 1                             ce    in  (*                                      S[1]++ >> .. *)      
    let ce      = PUSH4 (Int arrArgLoc)             >>ce    in  (*                                seed >> S[1] >> .. *)
    let ce      = SSTORE                            >>ce    in  (* S[seed]:=S[1]                                  .. *)
                  JUMPDEST label                    >>ce        (*                                                .. *)

let init_salloc_argArr_if_not ce = 
    let label   = fresh_label ()                            in  (*                                                   *) 
    let ce      = PUSH1 (Int 1)                     >>ce    in  (*                                                   *) 
    let ce      = SLOAD                             >>ce    in  (*                                                   *)
    let ce      = PUSH4 (Label label)               >>ce    in  (*                                                   *) 
    let ce      = JUMPI                             >>ce    in  (* IF S[1]!=0 then GOTO label                  >> .. *)
    let ce      = reset_salloc_array                  ce    in  (*                                      1 >> 1 >> .. *) 
                  JUMPDEST label                    >>ce    

let setup_argArrays ce cn =
    let ce      = init_salloc_argArr_if_not           ce    in   
    let arrLocs = SL.array_locations cn                     in  
                  foldl salloc_argArr ce arrLocs 
(*  S[0]   : PC                                                 *)  
(*  S[1]   := m   <----- array seed                             *)  
(*  S[2]   :      <- arg1 : the value is stored in message call *) 
(*            ..      ..                ...                     *)
(*  S[k+1] :      <- argk : the value is stored in message call *) 
(*  S[k+2] := 1    ----+                                        *)  
(*  S[k+3] := 2        | arrays                                 *)  
(*   ..                |                                        *)  
(*  S[n+1] := m    ----+                                        *)  

(*****   6.5.  CODECOPY            *****) 
let mstore_rntimeCode ce idx =                                  (*                                                              *)
    let ce    = PUSH32(RntimeCodeSize)          >>ce        in  (*                                                   size >> .. *)
    let ce    = DUP1                            >>ce        in  (*                                           size >> size >> .. *)  
    let ce    = malloc                            ce        in  (*                                    alloc(size) >> size >> .. *)
    let ce    = DUP2                            >>ce        in  (*                            size >> alloc(size) >> size >> .. *)
    let ce    = PUSH32(RntimeCodeOffset idx)    >>ce        in  (*                     idx >> size >> alloc(size) >> size >> .. *)
    let ce    = DUP3                            >>ce        in  (*      alloc(size) >> idx >> size >> alloc(size) >> size >> .. *)
                CODECOPY                        >>ce            (*                                    alloc(size) >> size >> .. *)
                                                                (*                                     codebegin                *)
(*****   6.6.  CONTRACT CREATION   *****)
type cnstrctrCode       =   { cnstrctr_ce           : ce
                            ; cnstrctr_ty           : ty
                            ; cnstrctr_cn           : ty cntrct         }

let ce_of_cc cc         =   cc.cnstrctr_ce
let program_of_cc       =   extract_program $ ce_of_cc  

let codegen_cnstrctr_bytecode cns idx = (* return ce which contains the program *) 
    let cn      =   lookup_index idx cns                      in 
    let ce      =   empty_ce (lookup_cn_of_cns cns) cns       in  (*                                                                                 *)
    let ce      =   init_malloc                     ce        in  (* M[64] := 96                                                                     *)
    let ce      =   mstore_cnstrArgs                ce cn     in  (*                                            alloc(argssize) << argssize << ..    *)
    let ce      =   sstore_cnstrArgs                ce idx    in  (* S[i..i+sz-1]:= argCodes               i << alloc(argssize) << argssize << ..    *)
    let ce      =   setup_argArrays                 ce cn     in  (* S[1]        := #array                 i << alloc(argssize) << argssize << ..    *)
    let ce      =   set_cntrct_pc                   ce idx    in  (* S[PC]       := rntime_cn_offst (returned body)                                  *)
    let ce      =   mstore_rntimeCode               ce idx    in  (*                                      alloc(codesize) << codesize << i <<  ..    *)
                    RETURN                        >>ce            (* OUTPUT(M[code]) as The BODY code                                    i <<  ..    *)

let compile_cnstrctr cns idx  : cnstrctrCode =
    let cn      =   L.assoc idx cns in 
    { cnstrctr_ce           = codegen_cnstrctr_bytecode cns idx
    ; cnstrctr_ty           = typeof_cntrct cn 
    ; cnstrctr_cn           = cn                                }

let compile_cnstrctrs cns : cnstrctrCode idx_list =
    idxmap (compile_cnstrctr cns) cns

(***************************************)
(***     7.    RUNTIME               ***)
(***************************************)

type rntimeCode             =   { rntime_ce             : ce                                                       
                                ; rntime_cn_offsets     : int idx_list  }

let empty_rntimeCode lookup_cn layouts =
    { rntime_ce             = empty_ce lookup_cn layouts
    ; rntime_cn_offsets     = []                                    }

let init_rntimeCode lookup_cn layouts : rntimeCode =
    let ce      =   empty_ce lookup_cn layouts                      in
    let ce      =   get_cntrct_pc                             ce    in
    let ce      =   JUMP                                    >>ce    in
    { rntime_ce             = ce
    ; rntime_cn_offsets     = [] }

(***************************************)
(***     8.  DISPATHER               ***)
(***************************************)

let dispatcher_usualMthd idx le ce m =                                  (*                                           ABCD >> .. *)  
    let ce      =   DUP1                                    >>ce    in  (*                                   ABCD >> ABCD >> .. *)
    let ce      =   push_mthd_hash m                          ce    in  (*                              m >> ABCD >> ABCD >> .. *)
    let ce      =   EQ                                      >>ce    in  (*                             m=ABCD?1:0 >> ABCD >> .. *)
    let ce      =   PUSH32(RntimeMthdLabel(idx,m))   >>ce    in  (*                Rntime(m) >> m=ABCD?1:0 >> ABCD >> .. *)
                    JUMPI                                   >>ce        (* if m=ABCD then GOTO Rntime(m)             ABCD >> .. *)

let dispatcher_defaultMthd idx le ce =
    let ce      =   PUSH32(RntimeMthdLabel(idx,TyDefault))    >>ce    in
                    JUMP                                    >>ce     

let push_inputdata32_from databegin ce =
    let ce      =   PUSH32 databegin                        >>ce    in
                    CALLDATALOAD                            >>ce

let dispatcher le ce idx cntrct =
    let tyMthds =   L.map (fun x -> x.mthd_head) cntrct.mthds       in
    let uMthds  =   filter_usualMthd tyMthds                        in 
    let ce      =   push_inputdata32_from(Int 0)              ce    in  (*               ABCDxxxxxxxxxxxxxxxxxxxxxxxxxxxxx >> .. *)
    let ce      =   shiftRtop ce Eth.(word_bits-sig_bits)           in  (*                                            ABCD >> .. *)                             
    let ce      =   foldl(dispatcher_usualMthd idx le)ce uMthds     in  (* JUMP to Method ABCD                                   *)   
    let ce      =   POP                                     >>ce    in  (*                                                    .. *)
    let ce      =   if  default_exists tyMthds
                        then dispatcher_defaultMthd idx le    ce        (* JUMP to Default Method                             .. *) 
                        else throw ce                               in  (* JUMP to error                                      .. *) 
    le,ce

(*********************************************)
(***     9.    CODEGEN  PRECONTRACT        ***)
(*********************************************)

let rec codegen_call le ce aln cr reT = 
    match cr.call_id with 
    | "pre_ecdsarecover"    ->  assert(aln=R);  codegen_ECDSArecover le ce      cr.call_args
    | "keccak256"           ->  assert(aln=R);  codegen_keccak256    le ce      cr.call_args    
    | "iszero"              ->                  codegen_iszero       le ce aln  cr.call_args reT
    | _                     ->  err "codegen_call: Direct Contract Call is Not supported. Specify a Method Call."

and codegen_iszero le ce aln args reT = match args with
    | [arg] ->  assert(reT=TyBool) ; 
                let ce =  arg                   >>>>(aln,le,ce) in
                          ISZERO                >>ce 
       
and codegen_keccak256 le ce args =
    let ce    = get_malloc ce                                   in  
    let ce    = mstore_mthd_args TightPack args le ce           in  
    let ce    = SWAP1                           >>ce            in  
                SHA3                            >>ce              

and codegen_ECDSArecover le ce args = match args with  
    | [h; v; r; s] ->
    let ce    = PUSH1 (Int 32)                  >>ce            in  
    let ce    = DUP1                            >>ce            in  
    let ce    = malloc                            ce            in  
    let ce    = repeat DUP2 2                     ce            in  
    let ce    = get_malloc                        ce            in
    let ce    = mstore_mthd_args ABIPack args le  ce            in  
    let ce    = SWAP1                           >>ce            in  
    let ce    = PUSH1 (Int 0)                   >>ce            in  
    let ce    = PUSH1 (Int 1)                   >>ce            in  
    let ce    = PUSH4 (Int 10000)               >>ce            in  
    let ce    = CALL                            >>ce            in  
    let ce    = throw_if_0 ce                                   in
    let ce    = POP                             >>ce            in  
    let ce    = SWAP1                           >>ce            in  
    let ce    = POP                             >>ce            in  
                MLOAD                           >>ce                (* stack: [output] *)
    | _         -> err "pre_ecdsarecover has a wrong number of args"

(*********************************************)
(***    10.    CODEGEN  EXPR               ***)
(*********************************************)
(*
 *              ADDRESS              MEMORY 
 *          +---------------------+-----------------+                         
 *          |                  0  | rntimeCode      |                                   
 *          |                ...  |  ...            |                                   
 *          |                ...  | RETURN          |                                   
 *          |               size  | cnstrctrCode    |
 *          |                ...  |  ...            |
 *          |                ...  | RETURN          |
 *          |         size+wsize  | arg1            |                                   
 *          |                     |  ...            |
 *          |                     | arg2            |
 *          |                     |  ...            |
 *          |---------------------|-----------------|
 *          | argsize+size+wsize  |                 |
 *          |                ...  |                 |             *)                                                                 

and mstore_new_instance le ce n     =
    let cnName  =   n.new_id                                            in
    let cnIdx   =   lookup_cn_of_ce ce cnName                           in 
    let ce      =   PUSH32(CnstrctrCodeSize cnIdx)      >>ce            in  (*                                                        size >> .. *) 
    let ce      =   PUSH32(RntimeCnstrctrOffset cnIdx)  >>ce            in  (*                                             cn_idx  >> size >> .. *)
    let ce      =   mstore_code                           ce            in  (*                                         alloc(size) >> size >> .. *)
    let ce      =   SWAP1                               >>ce            in  (*                                         size >> alloc(size) >> .. *)
    let ce      =   mstore_whole_code                     ce            in  (*                alloc(wsize) >> wsize >> size >> alloc(size) >> .. *)
    let ce      =   mstore_mthd_args ABIPack n.new_args le ce           in  (*    argssize >> alloc(wsize) >> wsize >> size >> alloc(size) >> .. *)
    let ce      =   SWAP1                               >>ce            in  (*    alloc(wsize) >> argssize >> wsize >> size >> alloc(size) >> .. *)
    let ce      =   POP                                 >>ce            in  (*                    argssize >> wsize >> size >> alloc(size) >> .. *)
    let ce      =   ADD                                 >>ce            in  (*                       argssize+wsize >> size >> alloc(size) >> .. *)
    let ce      =   ADD                                 >>ce            in  (*                          argssize+wsize+size >> alloc(size) >> .. *)
                    SWAP1                               >>ce                (*                                  alloc(size) >>   totalsize >> .. *)

and codegen_new le ce n             =    
    let ce      =   reset_PC ce                                         in  (*                                             PCbkp >> .. *)
    let ce      =   mstore_new_instance le ce n                         in  (*                      alloc(size) >> size >> PCbkp >> .. *)
    let ce      =   match n.new_msg with                                    (* msg is the amount sent                                  *) 
    | EpFalse,_  -> PUSH1 (Int 0)                       >>ce                (*                                                         *)
    | e          -> e                                   >>>>(R,le,ce)   in  (*             value >> alloc(size) >> size >> PCbkp >> .. *)
    let ce      =   CREATE                              >>ce            in  (*                             createResult >> PCbkp >> .. *)
    let ce      =   throw_if_0                            ce            in  (*                             createResult >> PCbkp >> .. *)
    let ce      =   SWAP1                               >>ce            in  (*                             PCbkp >> CreateResult >> .. *)
                    restore_PC                            ce                (*                                      CreateResult >> .. *)

and codegen_array aa le ce          =                                       (*                                                      .. *)
    let ce      =   keccak_of_aa aa                    le ce            in  (*                                      keccak(a[i]) >> .. *)
                    SLOAD                               >>ce                (*                                   S[keccak(a[i])] >> .. *) 

and keccak_of_aa aa le ce           =                                       (* S[keccak(a[i])] := the seed of array *) 
    let arr     =   aa.arrId                                            in
    let idx     =   aa.arrIndex                                         in  (*                                                      .. *)
    let ce      =   idx                                 >>>>(R,le,ce)   in  (*                                             index >> .. *)    
    let ce      =   arr                                 >>>>(R,le,ce)   in  (*                                array_loc >> index >> .. *)
                    keccak_cat ce                                           (*                            sha3(array_loc++index) >> .. *) 
      
and salloc_array aa le ce           =
    let push    =   keccak_of_aa aa le                                  in  (*                                        S[storIdx] >> .. *)
                    salloc_array_of_push push ce                            (* S[storIdx]:= newSeed                      newSeed >> .. *)     
                                                                          
and salloc_array_of_loc le ce(Stor rg) =       
    assert(rg.stor_size=Int 1) ;
    let storIdx =   rg.stor_start                                       in  (*                                        S[storIdx] >> .. *)     
    let push ce =   PUSH32 storIdx                      >>ce            in  
                    salloc_array_of_push push             ce                (* S[storIdx]:= newSeed                      newSeed >> .. *)     

and salloc_array_of_push push_array_seed ce =   
    let label   =   fresh_label ()                                      in  (*                                        S[storIdx] >> .. *) 
    let ce      =   DUP1                                >>ce            in  (*                          S[storIdx] >> S[storIdx] >> .. *) 
    let ce      =   PUSH4(Label label)                  >>ce            in  (*                 label >> S[storIdx] >> S[storIdx] >> .. *) 
    let ce      =   JUMPI                               >>ce            in  (* {IF S[storIdx]!=0 GOTO label}          S[storIdx] >> .. *) 
    let ce      =   POP                                 >>ce            in  (*                                                      .. *) 
    let ce      =   push_array_seed                       ce            in  
    let ce      =   sincr 1                               ce            in  (*                                 S[1]++ >> storIdx >> .. *) 
    let ce      =   DUP1                                >>ce            in  (*                     newseed >> newseed >> storIdx >> .. *) 
    let ce      =   SWAP2                               >>ce            in  (*                     storIdx >> newseed >> newseed >> .. *) 
    let ce      =   SSTORE                              >>ce            in  (* S[storIdx]:= newseed                      newseed >> .. *)
                    JUMPDEST label                      >>ce                (*                                                         *)

(* le is not updated here.  
 * le can only be updated in a variable initialization *)
and codegen_expr le ce aln      = function 
(*  | SmAbort,TyVoid                ->  throw ce   *)
    | EpAddr(c,TyInstnce i),TyAddr  ->                  (c,TyInstnce i)         >>>>(aln,le,ce) 
    | EpValue       ,TyUint256      ->                  CALLVALUE               >>ce      (* Value (wei) Transferred to the account *) 
    | EpNow         ,TyUint256      ->                  TIMESTAMP               >>ce 
    | EpFalse       ,TyBool         ->  assert(aln=R);  PUSH1(Big big_0)        >>ce  
    | EpTrue        ,TyBool         ->  assert(aln=R);  PUSH1(Big big_1)        >>ce  
    | EpUint256 d   ,TyUint256      ->  assert(aln=R);  PUSH32(Big d)           >>ce  
    | EpUint8 d     ,TyUint8        ->  assert(aln=R);  PUSH1(Big d)            >>ce  
    | EpPlus (l,r)  ,TyUint256      ->                  op ADD l r             le ce              
    | EpPlus (l,r)  ,TyUint8        ->                  op ADD l r             le ce               
    | EpMinus(l,r)  ,TyUint256      ->                  op SUB l r             le ce              
    | EpMinus(l,r)  ,TyUint8        ->                  op SUB l r             le ce              
    | EpMult (l,r)  ,TyUint256      ->                  op MUL l r             le ce              
    | EpMult (l,r)  ,TyUint8        ->                  op MUL l r             le ce              
    | EpLT   (l,r)  ,TyBool         ->  assert(aln=R);  op LT  l r             le ce            
    | EpGT   (l,r)  ,TyBool         ->  assert(aln=R);  op GT  l r             le ce            
    | EpEq   (l,r)  ,TyBool         ->  assert(aln=R);  op EQ  l r             le ce            
    | EpNEq  (l,r)  ,TyBool         ->  let ce      =   op EQ  l r             le ce            in
                                        assert(aln=R);  ISZERO                  >>ce            
    | EpNot expr    ,TyBool         ->  let ce      =   expr                    >>>>(aln,le,ce) in
                                        assert(aln=R);  ISZERO                  >>ce          
    | EpLAnd (l,r)  ,TyBool         ->                  checked_codegen_LAnd l r le ce aln   
    | EpSend s      ,_              ->  assert(aln=R);  codegen_send le ce s
    | EpNew n       ,TyInstnce _    ->  assert(aln=R);  codegen_new  le ce n 
    | EpCall call   ,tyRet          ->                  codegen_call le ce aln call tyRet
    | EpBalance e   ,TyUint256      ->  let ce      =   e                       >>>>(R,le,ce)   in
                                                        BALANCE                 >>ce
    | EpSender      ,TyAddr         ->  let ce      =   CALLER                  >>ce            in
                                                        align_addr ce aln
    | EpThis        ,_              ->  let ce      =   ADDRESS                 >>ce            in
                                                        align_addr ce aln     
    | EpArray a_i   ,TyMap _        ->  let ce      =   codegen_array a_i le ce                 in  (*            S[keccak(a[i])] >> .. *)
                                        assert(aln=R);  salloc_array a_i le ce                      (*                     S[1]++ >> .. *)
    | EpArray a     ,      _        ->  assert(aln=R);  codegen_array a   le ce                     (*               S[keccak(a)] >> .. *)
    | EpIdent id    ,ty             ->  (match lookup le id with | Some loc     ->  
                                        let ce      =   push_loc ce aln ty loc                  in  (*                        loc >> .. *)
                                        begin match ty  with
                                        | TyMap _   ->  salloc_array_of_loc le ce loc               (*                               .. *)
                                        | _         ->  ce                                      end)
    | EpDeref(ref,tyR),ty           ->  let size    =   size_of_ty ty                           in
                                        assert (size<=32 && tyR=TyRef ty && aln=R) ;                (* assuming word-size *)
                                        let ce      =   (ref,tyR)               >>>>(R,le,ce)   in  (* pushes the pointer *)
                                                        MLOAD                   >>ce 
    | _, _                          ->  errc"codegen_expr: Error"

and op operator l r le ce =
    let ce    = r                               >>>>(R,le,ce)   in 
    let ce    = l                               >>>>(R,le,ce)   in 
                operator                        >>ce 

and (>>>>) expr (aln,le,ce)  = codegen_expr le ce aln expr 

and checked_codegen_LAnd l r le ce aln = 
    assert(aln=R);         
    let la      =   fresh_label ()                              in  (*                                                        .. *)
    let ce      =   l                           >>>>(R,le,ce)   in  (*                                                   l >> .. *)
    let ce      =   DUP1                        >>ce            in  (*                                              l >> l >> .. *)
    let ce      =   if_0_GOTO la                  ce            in  (*                                                   l >> .. *)
    let ce      =   POP                         >>ce            in  (*                                                        .. *)
    let ce      =   r                           >>>>(R,le,ce)   in  (*                                                   r >> .. *)
    let ce      = JUMPDEST la                   >>ce            in  (*                                                   r >> .. *)
                    repeat ISZERO 2               ce                (*                                                l&&r >> .. *)  

and mstore_mthd_args pck args le ce =
    let ce    = PUSH1(Int 0)                    >>ce            in  (*                                                   0 >> .. *)
                foldl (mstore_mthd_arg pck le) ce args              (*                                             sumsize >> .. *) 

and mstore_mthd_arg pck le ce arg  =
    let ty      = snd arg in  assert (fits_in_one_stor_slot ty) ; 
    let i,a     = match pck with | ABIPack   -> 32           ,R
                                 | TightPack -> size_of_ty ty,L in  (*                                                 sum >> .. *)
    let ce      = PUSH1 (Int i)                 >>ce            in  (*                                         size >> sum >> .. *)
    let ce      = arg                           >>>>(a,le,ce)   in  (*                                  arg >> size >> sum >> .. *)
    let ce      = DUP2                          >>ce            in  (*                          size >> arg >> size >> sum >> .. *)
    let ce      = malloc                          ce            in  (*                   alloc(size) >> arg >> size >> sum >> .. *)
    let ce      = MSTORE                        >>ce            in  (* M[alloc(size)] := arg                   size >> sum >> .. *)
                  ADD                           >>ce                (*                                            size+sum >> .. *)

and mstore_mthd_hash_args mthd args le ce =                         (*                                                        .. *)
    let ce      = mstore_mthd_hash mthd           ce            in  (*                                         &mhash >> 4 >> .. *)
    let ce      = mstore_mthd_args ABIPack args le ce           in  (*                              argsize >> &mhash >> 4 >> .. *)
    let ce      = SWAP1                         >>ce            in  (*                              4 >> argsize >> &mhash >> .. *)
    let ce      = SWAP2                         >>ce            in  (*                              4 >> argsize >> &mhash >> .. *)
    let ce      = ADD                           >>ce            in  (*                                 argsize+4 >> &mhash >> .. *)
                  SWAP1                         >>ce                (*                                 &mhash >> argsize+4 >> .. *)

and mload_ret_value ce =                                            (*                           retbegin >> retsize >> .. *)
    let ce      = DUP2                          >>ce            in  (*                retsize >> retbegin >> retsize >> .. *)
    let ce      = PUSH1 (Int 32)                >>ce            in  (*          32 >> retsize >> retbegin >> retsize >> .. *)
    let ce      = throw_if_NEQ                    ce            in  (* IF 32!=retsize ERROR      retbegin >> retsize >> .. *)
    let ce      = MLOAD                         >>ce            in  (*                     M[retbegin.. ] >> retsize >> .. *)
    let ce      = SWAP1                         >>ce            in  (*                     retsize >> M[retbegin.. ] >> .. *)
                  POP                           >>ce                (*                                      retvalue >> .. *)
      
and codegen_send le ce (s:ty _send) =
    let args    = s.sd_args                                     in
    let cn      = s.sd_cn                                       in 
    let m       = s.sd_mthd                                     in 
    match snd cn with
    | TyInstnce cnname ->  (* msg-call to a contract *) 
    let idx     = lookup_cn_of_ce ce cnname                     in 
    let callee  = cntrct_lookup ce idx                          in
    begin match m with | Some name  -> 
    let m       = lookup_mthd_info ce callee name               in
    let TyMethod(id,_,ret) = m in 
    let retSize = size_of_ty ret                                in  (*                                                                                                     PCbkp >> .. *)
    let ce      = reset_PC                        ce            in  (*                                                                                                     PCbkp >> .. *)
    let ce      = PUSH1(Int retSize)            >>ce            in  (*                                                                                          retsize >> PCbkp >> .. *)
    let ce      = DUP1                          >>ce            in  (*                                                                               retsize >> retsize >> PCbkp >> .. *)
    let ce      = malloc                          ce            in  (*                                                                              retbegin >> retsize >> PCbkp >> .. *)
    let ce      = repeat DUP2 2                   ce            in  (*                                                       retbegin >> retsize >> retbegin >> retsize >> PCbkp >> .. *)
    let ce      = mstore_mthd_hash_args m args le ce            in  (*                               &mhash >> argssize+4 >> retbegin >> retsize >> retbegin >> retsize >> PCbkp >> .. *)
    let ce      = push_msg_and_gas s           le ce            in  (*  gas-3000 >> cnAddr >> msg >> &mhash >> argssize+4 >> retbegin >> retsize >> retbegin >> retsize >> PCbkp >> .. *)
    let ce      = call_and_restore_PC             ce            in  (*                                                                              retbegin >> retsize >> PCbkp >> .. *)
    let ce      = SWAP1                         >>ce            in  (*                               &mhash >> argssize+4 >> retbegin >> retsize >> retbegin >> retsize >> PCbkp >> .. *)
                  mload_ret_value                 ce            end (*                               &mhash >> argssize+4 >> retbegin >> retsize >> retbegin >> retsize >> PCbkp >> .. *)
    | TyAddr        ->   (* send value to an EOA *) 
    let retSize = 0                                             in 
    let ce      = reset_PC                        ce            in  (*                                                          PCbkp >> .. *) 
    let ce      = PUSH1(Int retSize)            >>ce            in  (*                                                     0 >> PCbkp >> .. *) 
    let ce      = DUP1                          >>ce            in  (*                                                0 >> 0 >> PCbkp >> .. *) 
    let ce      = repeat DUP2 4                   ce            in  (*                            0 >> 0 >> 0 >> 0 >> 0 >> 0 >> PCbkp >> .. *) 
    let ce      = push_msg_and_gas s           le ce            in  
    let ce      = call_and_restore_PC             ce            in  (*                                                0 >> 0 >> PCbkp >> .. *)
                  POP                           >>ce                (*                                                     0 >> PCbkp >> .. *)
    | _             -> err "send expr with Wrong type"

and push_msg_and_gas s le ce = 
    let ce = match s.sd_msg with                                    (*                                                     .. *)
    | EpFalse,_ -> PUSH1(Int 0)                 >>ce            
    | e         -> e                            >>>>(R,le,ce)   in  (*                                            value >> .. *) 
    let ce      = s.sd_cn                       >>>>(R,le,ce)   in  (*                                  cnAddr >> value >> .. *)
    let ce      = PUSH4(Int 3000)               >>ce            in  (*                          3000 >> cnAddr >> value >> .. *)
    let ce      = GAS                           >>ce            in  (*                   gas >> 3000 >> cnAddr >> value >> .. *)
                  SUB                           >>ce                (*                      gas-3000 >> cnAddr >> value >> .. *)

and call_and_restore_PC ce =                                        (*  gas-3000 >> cnAddr >> msg >> &mhash >> argssize+4 >> retbegin >> retsize >> retbegin >> retsize >> PCbkp >> .. *)
    let ce      = CALL                          >>ce            in  (*                                                                  retvalue >> retbegin >> retsize >> PCbkp >> .. *)
    let ce      = PUSH1(Int 0)                  >>ce            in  (*                                                             0 >> retvalue >> retbegin >> retsize >> PCbkp >> .. *)
    let ce      = JUMPI                         >>ce            in  (*  IF retvalue==0 THEN GOTO 0                                                  retbegin >> retsize >> PCbkp >> .. *)
    let ce      = SWAP2                         >>ce            in  (*                                                                              PCbkp >> retsize >> retbegin >> .. *)
                  restore_PC ce                                     (*                                                                                       retsize >> retbegin >> .. *)

(* stmt --> expr #TODO *) 
and codegen_expr_stmt le ce expr =
    let ce      = expr                          >>>>(R,le,ce)   in
    let ce      = POP                           >>ce            in
    le, ce

and codegen_selfDstrct le ce expr =    
    let ce      = expr                          >>>>(R,le,ce)   in
    let ce      = SELFDESTRUCT                  >>ce            in
    le, ce

and sstore_to_lexpr le ce (LEpArray a) = 
    let arr     = a.arrId                                       in
    let idx     = a.arrIndex                                    in
    let ce      = idx                           >>>>(R,le,ce)   in   (* stack : [value, index] *)
    let ce      = arr                           >>>>(R,le,ce)   in   (* stack : [value, index, array_seed] *)
    let ce      = keccak_cat                      ce            in   (* stack : [value, kec(array_seed ^ index)] *)
                  SSTORE                        >>ce                

and codegen_assign le ce l r =
    let ce      = r                             >>>>(R,le,ce)   in
    let ce      = sstore_to_lexpr le ce l                       in
    le, ce

and codegen_decl le ce i  = 
    let pos     = stack_size ce                                 in
    let name    = i.declId                                      in
    let ce      = i.declVal                     >>>>(R,le,ce)   in
    let le      = add_loc le(name, Stack(pos+1))                in
    le, ce

and codegen_if_then le ce layt cond stmts =
    let label   = fresh_label ()                                in
    let ce      = cond                          >>>>(R,le,ce)   in
    let ce      = if_0_GOTO label                 ce            in 
    let le,ce   = codegen_stmts stmts layt     le ce            in
    let ce      = JUMPDEST label                >>ce            in
    le,ce

and codegen_if le ce layt cond ss1 ss2 =
    let next    = fresh_label()                                 in
    let endif   = fresh_label()                                 in
    let ce      = cond                          >>>>(R,le,ce)   in
    let ce      = if_0_GOTO next                  ce            in 
    let _,ce    = codegen_stmts ss1 layt       le ce            in (* location env needs to be discarded *)
    let ce      = goto endif                      ce            in
    let ce      = JUMPDEST next                 >>ce            in
    let _,ce    = codegen_stmts ss2 layt       le ce            in (* location env needs to be discarded *)
    let ce      = JUMPDEST endif                >>ce            in
    le,ce

and mstore_exprs le ce pack = function 
    | []        ->  let ce    = PUSH1(Int 0)            >>ce    in
                    let ce    = PUSH1(Int 0)            >>ce    in
                    le, ce
    | e::es     ->  let le,ce = mstore_expr le ce pack e        in (*                                      alloc(size) >> size >> .. *)
                    let ce    = SWAP1                   >>ce    in (*                                      size >> alloc(size) >> .. *)
                    let le,ce = mstore_exprs le ce pack es      in (*   0 >> 0 >> size' >> alloc(size') >> size >> alloc(size) >> .. *)
                    let ce    = POP                     >>ce    in (*        0 >> size' >> alloc(size') >> size >> alloc(size) >> .. *)       
                    let ce    = ADD                     >>ce    in (*             size' >> alloc(size') >> size >> alloc(size) >> .. *)    
                    let ce    = SWAP1                   >>ce    in (*             alloc(size') >> size' >> size >> alloc(size) >> .. *)     
                    le, ce   (* POP                                                           size' >> size >> alloc(size) >> .. *) 
                             (* ADD                                                             size+size'  >> alloc(size) >> .. *) 
                             (* SWAP1                                                           alloc(size) >> size+size'  >> .. *)
                    
and codegen_log_stmt le ce name args evnt =
    let visible, args= split_evnt_args evnt args                in
    let ce      = push_args le visible            ce            in
    let ce      = push_evnt_hash  evnt            ce            in
    let le,ce   = mstore_exprs le ce ABIPack args               in  (* stack : [..., size, offset] *)
    let n       = L.length visible + 1                          in
    let ce      = log n                         >>ce            in  (* deindexee N in logN *)
    le, ce

(***************************************)
(***     11. CODEGEN RETURN          ***)
(***************************************)

and push_args le                    = foldr (fun arg ce->arg>>>>(R,le,ce))  
and sstore_words_to stor_locs ce    = foldl sstore_word_to ce stor_locs
and sstore_word_to ce stor_loc      =
    let ce      =   PUSH32 (Int stor_loc)       >>ce            in
                    SSTORE                      >>ce       

and sstore_args le ce offset idx args = 
    let cntrct  =   cntrct_lookup ce idx                in 
    let arglocs =   SL.arg_locations offset cntrct      in
    assert(L.length arglocs=L.length args) ; 
    let ce      =   push_args       le args ce          in  (*                                         argk >> .. >> arg1 >> .. *)
    let ce      =   sstore_words_to arglocs ce          in  (*  S[l_k]:=argk; .. ; S[l_1]:=arg1                              .. *)
    le,ce

and cont_call le ce (layt:SL.storLayout) (EpCall cont,_) = 
    let cn      =   cont.call_id                        in
    let args    =   cont.call_args                      in
    let idx     =   lookup_cn_of_ce ce cn               in 
    let offset  =   layt.stor_cnstrctrArgs_begin idx    in  
    let ce      =   set_cntrct_pc ce idx                in  (* S[PC] := rntime_offset_of_cntrct                              .. *) 
                    sstore_args le ce offset idx args       (* S[l_k]:=argk; .. ; S[l_1]:=arg1                               .. *)

(*       mstore_word ty 
 *
 *     BEFORE           AFTER               
 *
 *                   +---------+            
 *                   |alloc(32)|            
 *   +---------+     +---------+            
 *   |  value  |     |    32   |            
 * --+---------+-- --+---------+--    *)

and mstore_word ty ce = assert (size_of_ty ty <= 32)  ;         (*                                   val >> .. *)   (* Here, FUN TYPE is excluded <- Problem #TODO *) 
    let ce      = PUSH1(Int 32)             >>ce            in  (*                             32 >> val >> .. *)
    let ce      = DUP1                      >>ce            in  (*                       32 >> 32 >> val >> .. *)
    let ce      = malloc                      ce            in  (*                alloc(32) >> 32 >> val >> .. *)
    let ce      = SWAP2                     >>ce            in  (*                val >> 32 >> alloc(32) >> .. *)
    let ce      = DUP3                      >>ce            in  (*   alloc(32) >> val >> 32 >> alloc(32) >> .. *)
    let ce      = MSTORE                    >>ce            in  (* M[alloc(32)]:=val     32 >> alloc(32) >> .. *)
                  SWAP1                     >>ce                (*                       alloc(32) >> 32 >> .. *)

and mstore_expr le ce pack (e,ty) =
    let a       = match pack with | ABIPack   -> R
                                  | TightPack -> L          in
    let ce      = (e,ty)                    >>>>(a,le,ce)   in  (*                                    e >> .. *)
    let ce      = mstore_word ty ce                         in  (* M[alloc(32)]:=e      alloc(32) >> 32 >> .. *)
    le,ce

and codegen_return le ce layt ret =
    let le,ce   = cont_call le ce layt ret.ret_cont         in
    let ce      = match ret.ret_expr with
    | (TmUnit,_)->              STOP                >>ce      
    | e         ->  let le,ce = mstore_expr le ce ABIPack e in
                                RETURN              >>ce    in 
    le,ce

(*************************************)
(**      12. CODEGEN STMT           **)
(*************************************)

and codegen_stmts stmts lyt le ce   = foldl (codegen_stmt lyt) (le,ce) stmts
and codegen_stmt layt (le,ce)       = function 
    | SmAbort                       ->  le, throw ce  
    | SmAssign (l,r)                ->  codegen_assign      le ce l r
    | SmDecl i                      ->  codegen_decl        le ce i
    | SmSlfDstrct expr              ->  codegen_selfDstrct  le ce expr
    | SmLog(name,args,Some ev)      ->  codegen_log_stmt    le ce name args ev
    | SmLog(name,args,None)         ->  err "add_stmt: type check first"
    | SmExpr expr                   ->  codegen_expr_stmt   le ce expr
    | SmIfThen(cond,e)              ->  codegen_if_then     le ce layt cond e 
    | SmIf(cond,e1,e2)              ->  codegen_if          le ce layt cond e1 e2 
    | SmReturn ret                  ->  codegen_return      le ce layt ret


(********************************************)
(***     13. CODEGEN CONTRACT             ***)
(********************************************)

let label_mthd idx m ce =
    let label   =   fresh_label()                               in
    register_entry(Mthd(idx,m))label ; 
                    JUMPDEST label                  >>ce      

let calldatasize (TyMethod(_,args,_)) =
    4 (* for signature *) + total_size_of_args args   

let codegen_mthd_argLen_chk m ce = match m with  
    | TyDefault       -> ce
    | TyMethod(_,_,_)  ->
    let ce      = PUSH4(Int(calldatasize m))        >>ce        in
    let ce      = CALLDATASIZE                      >>ce        in
                  throw_if_NEQ                        ce    

let codegen_mthd layout idx (le,ce) m =
    let ce      = label_mthd  idx m.mthd_head         ce        in
    let ce      = codegen_mthd_argLen_chk m.mthd_head ce        in
    let le      = add_emptyEnv                     le           in
    let le      = add_mthd_argLocs    m            le           in
    let le,ce   = codegen_stmts m.mthd_body layout le ce        in
    le,ce

let codegen_mthds layout    = ($$$) foldl codegen_mthd layout 

let codegen_cntrct le ce layt (idx,cntrct) =
    let label   = fresh_label ()                                in      
    register_entry (Cntrct idx) label ;                    
    let ce      = JUMPDEST label                    >>ce        in     
    let ce      = init_malloc                         ce        in  (* M[64] := 96                                          *)                                  
    let le,ce   = dispatcher le ce idx cntrct                   in  (*                                                      *)
    let le,ce   = codegen_mthds layt idx (le,ce) cntrct.mthds   in  
    ce

(********************************************)
(***     14. MAKE BYTECODE                ***)
(********************************************)

let append_rntime lyt rc (idx,cn)   =
    { rntime_ce             = codegen_cntrct(rntime_init_le cn)rc.rntime_ce lyt(idx,cn)
    ; rntime_cn_offsets     = insert idx(code_len rc.rntime_ce)rc.rntime_cn_offsets    }

let compile_rntime lyt cns          = 
    let init_rc = init_rntimeCode (lookup_cn_of_cns cns) cns                        in 
    foldl (append_rntime lyt) init_rc cns

let storLayout_of_cnstrctrCode cc   = 
    SL.storLayout_of_cntrct cc.cnstrctr_cn (program_of_cc cc)

let sizes_of_cnstrctrCodes ccs      =
    let lengths = map (code_len $ ce_of_cc) ccs                                     in
    let lengths = idx_sort lengths                                                  in
    L.map snd lengths

let offsets_of_sizes init l         =
    let rec loop offsets current        = function 
        | []            -> L.rev offsets
        | size::rest    -> loop(current::offsets)(current+size)rest                 in 
    loop [] init l 

let storLayout_of_rntimeCode rc ccs =
    let ccs_sizes           = sizes_of_cnstrctrCodes ccs                            in
    let ccs_offsets         = offsets_of_sizes (code_len rc.rntime_ce) ccs_sizes    in
    let ccs_totalsize       = BL.sum ccs_sizes                                      in
    SL. { rn_codesize           = ccs_totalsize + code_len rc.rntime_ce
        ; rn_cn_offsets         = rc.rntime_cn_offsets
        ; rn_cnstrctr_sizes     = to_idx_list ccs_sizes
        ; rn_cnstrctr_offsets   = to_idx_list ccs_offsets }

(*  Since the code is stored in the reverse order, the concatenation is also reversed. *)
let concat_programs_rev programs    =
    let rev_programs        = L.rev programs                                        in
    L.concat rev_programs

let program_of_cnstrctrs ccs        =
    let programs            = map program_of_cc ccs                                 in
    let programs            = idx_sort  programs                                    in
    let programs            = L.map snd programs                                    in
    concat_programs_rev programs

let compose_bytecode ccs rc idx : big_int Evm.program =
    let cnLayouts           = map storLayout_of_cnstrctrCode  ccs                   in
    let rnLayout            =     storLayout_of_rntimeCode rc ccs                   in
    let layt                = SL.cnstrct_post_storLayout cnLayouts rnLayout         in
    let _cnstrctr           = lookup_index idx ccs                                  in
    let imm_cnstrctr        = SL.realize_program layt idx(program_of_cc _cnstrctr)  in
    let cns_program         = program_of_cnstrctrs ccs                              in 
    let rn_program          = extract_program rc.rntime_ce                          in
    let imm_rntime          = SL.realize_program layt idx(cns_program @ rn_program) in
    imm_rntime @ imm_cnstrctr   (* the code is stored in the reverse order *)
