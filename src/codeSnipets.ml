
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

let error_label = Int 0 

(******************************************************)
(***     1. ERROR HANDLING                          ***)
(******************************************************)

let _THROW vm         = (* the same with solc. *)
    let vm      =   PUSH error_label                @>> vm  in
                    JUMP                            @>> vm 

let _THROW_IF vm     =                                          (*                               cond >> .. *)
    let vm      =   PUSH error_label                @>> vm  in  (*                   errlabel >> cond >> .. *)
                    JUMPI                           @>> vm      (* if cond then goto err                 .. *) 

let _THROW_IFN vm    =                                          (*                                  i >> .. *)   
    let vm      =   DUP1                            @>> vm  in  (*                             i >> i >> .. *)
    let vm      =   ISZERO                          @>> vm  in  (*                 i==0 ? 1 : 0  >> i >> .. *)
                    _THROW_IF                           vm      (*                                  i >> .. *) 
      
let _THROW_IF_NEQ vm =                                          (*                             a >> b >> .. *) 
    let vm      =   EQ                              @>> vm  in  (*                               a==b >> .. *)
    let vm      =   ISZERO                          @>> vm  in  (*                               a!=b >> .. *)    
                    _THROW_IF                           vm      (* if a!=b then throw                    .. *) 

let _GOTO    la vm   = 
    let vm      =   PUSH(Label la)                  @>> vm  in 
                    JUMP                            @>> vm 

let _GOTO_IF la vm   =                                           (* IF stacktop != 0 GOTO label *)
    let vm      =   PUSH(Label la)                  @>> vm  in  
                    JUMPI                           @>> vm 

let _GOTO_IFN la vm =                                           (* IF stacktop == 0 GOTO label *)
    let vm      =   ISZERO                          @>> vm  in 
                    _GOTO_IF la                         vm  

let repeat opcode n vm = foldn n ((@>>)opcode) vm


(******************************************************)
(***     2. STACK OPERATIONS                        ***)
(******************************************************)

let push_storRange vm (data : imm data) =
    (*let i = match data.size with | Big b -> str_of_big b | Int i -> str_of_int i in 
     #DEBUG pf "stor_size is %s\n" i ;  *) 
    assert (is_const_int 1 data.size) ; 
    let vm      =   PUSH  data.offst                @>> vm  in
                    SLOAD                           @>> vm 

let _DUP_N_FRM_BOT n vm  =
                    dup_succ(vm.stack_height - n)   @>> vm 

let _SHR bits vm =
    assert (0 <= bits && bits < 256) ; 
    if bits=0 then vm else                                      (*                                         x >> .. *) 
    let vm      =   PUSH (Int bits)                 @>> vm  in  (*                                 bits >> x >> .. *)
                    SHR                             @>> vm      (*                               x/(2**bits) >> .. *) 

let _SHL bits vm =
    assert (0 <= bits && bits < 256) ; 
    if bits=0 then vm else                                      (*                                         x >> .. *)
    let vm      =   PUSH (Int bits)                 @>> vm  in  (*                                 bits >> x >> .. *)                   
                    SHL                             @>> vm      (*                               (2**bits)*x >> .. *) 

let _PLUS n   vm =
    let vm      =   PUSH (Int n)                    @>> vm  in
                    ADD                             @>> vm      

let _PLUS_S idx n vm = 
    let vm      =   PUSH(Int idx)                   @>> vm  in  (*                                         i >> .. *) 
    let vm      =   SLOAD                           @>> vm  in  (*                                      S[i] >> .. *) 
    let vm      =   DUP1                            @>> vm  in  (*                              S[i] >> S[i] >> .. *) 
    let vm      =   _PLUS n                             vm  in  (*                            S[i]+1 >> S[i] >> .. *) 
    let vm      =   PUSH(Int idx)                   @>> vm  in  (*                       i >> S[i]+1 >> S[i] >> .. *) 
                    SSTORE                          @>> vm      (* S[i]:=S[i]+1                         S[i] >> .. *) 

let _CALLDATALOAD data vm =
    assert (0 < data.size && data.size <= 32);
    let vm      =   PUSH (Int data.offst)           @>> vm  in
                    CALLDATALOAD                    @>> vm  

let _KEC_CAT vm =                                             (*                                    a >> b >> .. *) 
    let vm      =   PUSH (Int 0x00)                 @>> vm  in  (*                            0x00 >> a >> b >> .. *)
    let vm      =   MSTORE                          @>> vm  in  (* M[0x00]=a                               b >> .. *)
    let vm      =   PUSH (Int 0x20)                 @>> vm  in  (*                                 0x20 >> b >> .. *)
    let vm      =   MSTORE                          @>> vm  in  (* M[0x20]=b                                    .. *) 
    let vm      =   PUSH (Int 0x40)                 @>> vm  in  (*                                      0x40 >> .. *)
    let vm      =   PUSH (Int 0x00)                 @>> vm  in  (*                              0x0  >> 0x40 >> .. *)
                    SHA3                            @>> vm      (*                       sha3(M[0x00..0x3F]) >> .. *)
                                                                (*                          sha3(a++b)             *)

let _IS_LE bound vm =                                     (*                                         x >> .. *)
    let vm      =   DUP1                            @>> vm  in  (*                                    x >> x >> .. *)
    let vm      =   PUSH bound                      @>> vm  in  (*                           bound >> x >> x >> .. *) 
    let vm      =   LT                              @>> vm  in  (*                          bound<x?1:0 >> x >> .. *) 
                    _THROW_IF                           vm      (* IF x<bound THEN error                   x >> .. *)

let _IS_GE bound vm =                                     (*                                         x >> .. *)
    let vm      =   DUP1                            @>> vm  in  (*                                    x >> x >> .. *)
    let vm      =   PUSH bound                      @>> vm  in  (*                           bound >> x >> x >> .. *) 
    let vm      =   GT                              @>> vm  in  (*                          bound>x?1:0 >> x >> .. *) 
                    _THROW_IF                           vm      (* IF x<bound then error                   x >> .. *)


(******************************************************)
(***     4. PROGRAM COUNTER on STORAGE              ***)
(******************************************************)
                
let _RESET_PC vm     =
    let vm      =   PUSH StorPC                     @>> vm    in  (*                                       0 >> .. *)
    let vm      =   SLOAD                           @>> vm    in  (*                                    S[0] >> .. *)
    let vm      =   PUSH StorPC                     @>> vm    in  (*                               0 >> S[0] >> .. *)
    let vm      =   DUP1                            @>> vm    in  (*                          0 >> 0 >> S[0] >> .. *)
                    SSTORE                          @>> vm        (* S'[0]=0                            S[0] >> .. *)

let _RESTORE_PC vm   =                                             (*                                  bkp_PC >> .. *)
    let vm      =   PUSH StorPC                     @>> vm    in  (*                             0 >> bkp_PC >> .. *)
                    SSTORE                          @>> vm        (* S'[0]=bkp_pc                               .. *)             

let _SET_PC idx vm   =                                             (*                                            .. *)
    let vm      =   PUSH (RnCnOffset idx)           @>> vm    in  (*                            rn_cn_offset >> .. *) 
    let vm      =   PUSH StorPC                     @>> vm    in  (*                  storPC >> rn_cn_offset >> .. *) 
                    SSTORE                          @>> vm        (* S[storPC] := rn_cn_offset                  .. *) 

let get_PC vm       =
    let vm      =   PUSH StorPC                     @>> vm    in
                    SLOAD                           @>> vm 


(****************************************)
(***     5. MEMORY OPERATIONS         ***)
(****************************************)

let _KECCAK1    =   Int 0x00
let _KECCAK2    =   Int 0x20 
let _HP         =   Int 0x40         (* HEAP Pointer *) 
let _EP         =   Int 0x60         (* Escaping Variable Record Pointer *) 
let _SP         =   Int 0x80         (* Memory Stack : Another Stack different from EVM Stack *) 
let _EMIN       =   Int 0x100
let _SMIN       =   Int 0x8000 
let _HMIN       =   Int 0x10000      (* Initial HEAP Head *) 
let _EMAX       =   let Int i = _SMIN in Int (i-2)
let _SMAX       =   let Int i = _HMIN in Int (i-1)  



let mPUSH_from_STACK vm = 
    let vm      =   PUSH _SP                        @>> vm  in  (*                                                         sp >> x >> .. *)
    let vm      =   MLOAD                           @>> vm  in  (*                                                      M[sp] >> x >> .. *) 
    let vm      =   _IS_LE _SMAX                        vm  in  (*                                                      M[sp] >> x >> .. *)
    let vm      =   DUP1                            @>> vm  in  (*                                             M[sp] >> M[sp] >> x >> .. *)         
    let vm      =   PUSH(Int 0x20)                  @>> vm  in  (*                                    0x20 >>  M[sp] >> M[sp] >> x >> .. *)
    let vm      =   ADD                             @>> vm  in  (*                                        0x20+M[sp] >> M[sp] >> x >> .. *)
    let vm      =   PUSH _SP                        @>> vm  in  (*                                  SP >> 0x20+M[sp] >> M[sp] >> x >> .. *)
    let vm      =   MSTORE                          @>> vm  in  (* M[sp]    := M[sp]+0x20                               M[sp] >> x >> .. *)
                    MSTORE                          @>> vm      (* M[M[sp]] := x                                                      .. *) 

let mPUSH x vm  =   mPUSH_from_STACK ( PUSH x  @>> vm  ) 

let ePUSH vm        =                                           (*                                                   x >> retlabel >> .. *) 
    let vm      =   PUSH _EP                        @>> vm  in  (*                                             EP >> x >> retlabel >> .. *)
    let vm      =   MLOAD                           @>> vm  in  (*                                          M[EP] >> x >> retlabel >> .. *)
    let vm      =   _IS_LE _EMAX                        vm  in  (*                                          M[EP] >> x >> retlabel >> .. *)
    let vm      =   DUP1                            @>> vm  in  (*                                 M[EP] >> M[EP] >> x >> retlabel >> .. *)
    let vm      =   PUSH(Int 0x40)                  @>> vm  in  (*                         0x40 >> M[EP] >> M[EP] >> x >> retlabel >> .. *)
    let vm      =   ADD                             @>> vm  in  (*                            0x40+M[EP] >> M[EP] >> x >> retlabel >> .. *)
    let vm      =   PUSH _EP                        @>> vm  in  (*                      EP >> 0x40+M[EP] >> M[EP] >> x >> retlabel >> .. *)
    let vm      =   MSTORE                          @>> vm  in  (* M[EP] := M[EP]+0x40                      M[EP] >> x >> retlabel >> .. *)
    let vm      =   SWAP1                           @>> vm  in  (*                                          x >> M[EP] >> retlabel >> .. *)              
    let vm      =   DUP2                            @>> vm  in  (*                                M[EP] >>  x >> M[EP] >> retlabel >> .. *) 
    let vm      =   MSTORE                          @>> vm  in  (* M[M[EP]] := x                                 M[EP] >> retlabel >> .. *)
    let vm      =   PUSH _EP                        @>> vm  in  (*                                       0x20 >> M[EP] >> retlabel >> .. *)
    let vm      =   ADD                             @>> vm  in  (*                                          0x20+M[EP] >> retlabel >> .. *)
                    MSTORE                          @>> vm      (* M[M[EP]+0x20] := retAddr                                           .. *)

let get_escaped_arg vm =                                        (*                                                                    .. *)
    let vm      =   PUSH (Int 0x40)                 @>> vm  in  (*                                                            0x40 >> .. *)
    let vm      =   PUSH _EP                        @>> vm  in  (*                                                      EP >> 0x40 >> .. *)
    let vm      =   MLOAD                           @>> vm  in  (*                                                   M[EP] >> 0x40 >> .. *)
    let vm      =   SUB                             @>> vm  in  (*                                                      M[EP]-0x40 >> .. *)
                    MLOAD                           @>> vm      (*                                                     escaped_arg >> .. *)

let ePOP vm =                                                   (*                                                                    .. *)  
    let vm      =   PUSH _EP                        @>> vm  in  (*                                                             EP  >> .. *)
    let vm      =   MLOAD                           @>> vm  in  (*                                                           M[EP] >> .. *) 
    let vm      =   PUSH (Int 0x40)                 @>> vm  in  (*                                                   0x40 >> M[EP] >> .. *)
    let vm      =   PUSH (Int 0x20)                 @>> vm  in  (*                                           0x20 >> 0x40 >> M[EP] >> .. *) 
    let vm      =   DUP3                            @>> vm  in  (*                                  M[EP] >> 0x20 >> 0x40 >> M[EP] >> .. *)  
    let vm      =   SUB                             @>> vm  in  (*                                     M[EP]-0x20 >> 0x40 >> M[EP] >> .. *)
    let vm      =   _IS_GE _EMIN                        vm  in  (*                                     M[EP]-0x20 >> 0x40 >> M[EP] >> .. *)
    let vm      =   MLOAD                           @>> vm  in  (*                                        retAddr >> 0x40 >> M[EP] >> .. *)
    let vm      =   SWAP2                           @>> vm  in  (*                                        M[EP] >> 0x40 >> retAddr >> .. *)
    let vm      =   SUB                             @>> vm  in  (*                                           M[EP]-0x40 >> retAddr >> .. *)
    let vm      =   PUSH _EP                        @>> vm  in  (*                                     EP >> M[EP]-0x40 >> retAddr >> .. *)
                    MSTORE                          @>> vm      (* M[EP] := M[EP]-0x40                                     retAddr >> .. *)

let mPOP_to_STACK vm = 
    let vm      =   PUSH (Int 0x20)                 @>> vm  in  (*                                                            0x20 >> .. *) 
    let vm      =   PUSH _SP                        @>> vm  in  (*                                                      sp >> 0x20 >> .. *)
    let vm      =   MLOAD                           @>> vm  in  (*                                                   M[sp] >> 0x20 >> .. *) 
    let vm      =   _IS_GE _SMIN                        vm  in  (*                                                   M[sp] >> 0x20 >> .. *)    
    let vm      =   SUB                             @>> vm  in  (*                                                      M[sp]-0x20 >> .. *)
    let vm      =   DUP1                            @>> vm  in  (*                                        M[sp]-0x20 >> M[sp]-0x20 >> .. *)
    let vm      =   PUSH _SP                        @>> vm  in  (*                                  SP >> M[sp]-0x20 >> M[sp]-0x20 >> .. *) 
    let vm      =   MSTORE                          @>> vm  in  (* M[sp] := M[sp]-0x20                                  M[sp]-0x20 >> .. *) 
                    MLOAD                           @>> vm      (*                                                   M[M[sp]-0x20] >> .. *)

let mPOP vm =   
    let vm      =   PUSH (Int 0x20)                 @>> vm  in  (*                                                            0x20 >> .. *) 
    let vm      =   PUSH _SP                        @>> vm  in  (*                                                     SP  >> 0x20 >> .. *)
    let vm      =   MLOAD                           @>> vm  in  (*                                                   M[SP] >> 0x20 >> .. *) 
    let vm      =   _IS_GE _SMIN                        vm  in  (*                                                   M[SP] >> 0x20 >> .. *)
    let vm      =   SUB                             @>> vm  in  (*                                                      M[SP]-0x20 >> .. *)
    let vm      =   PUSH _SP                        @>> vm  in  (*                                                SP >> M[SP]-0x20 >> .. *) 
                    MSTORE                          @>> vm      (* M[SP] := M[SP]-0x20                                             >> .. *) 




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

let init_malloc vm =                                            (* initialize as M[0x40] := 0x1000000 *)
    let vm      =   PUSH _HMIN                      @>> vm  in
    let vm      =   PUSH _HP                        @>> vm  in
                    MSTORE                          @>> vm    



(* _MALLOC : (size) -> (maddr, size) *)
let _MALLOC vm  =                                              (* .. >> size                                                  *)
    let vm      =   PUSH _HP                        @>> vm  in  (* .. >> size >> 0x40                                          *)
    let vm      =   MLOAD                           @>> vm  in  (* .. >> size >> M[0x40]                                       *)
    let vm      =   DUP1                            @>> vm  in  (* .. >> size >> M[0x40] >> M[0x40]                            *)
    let vm      =   DUP3                            @>> vm  in  (* .. >> size >> M[0x40] >> M[0x40] >> size                    *)
    let vm      =   ADD                             @>> vm  in  (* .. >> size >> M[0x40] >> size+M[0x40]                       *)
    let vm      =   PUSH _HP                        @>> vm  in  (* .. >> size >> M[0x40] >> size+M[0x40] >> 0x40               *)
                    MSTORE                          @>> vm      (* .. >> size >> M[0x40]                                       *)  
    
(* _HEAPHEAD : () -> (maddr) *)
let _HEAPHEAD vm   =                             
    let vm      =   PUSH _HP                        @>> vm  in  (*                                                0x40   >> .. *) 
                    MLOAD                           @>> vm      (*                                              M[0x40]  >> .. *) 


let _MSTORE_CODE vm  =                                          (* size >> from                                       *)
    let vm      =   DUP2                            @>> vm  in  (* size >> from >> size                               *)
    let vm      =   _MALLOC                             vm  in  (* size >> from >> size >> alloc(size)                *)
    let vm      =   SWAP2                           @>> vm  in  (* size >> alloc(size) >> size >> from                *)
    let vm      =   DUP3                            @>> vm  in  (* size >> alloc(size) >> size >> from >> alloc(size) *)
                    CODECOPY                        @>> vm      (* size >> alloc(size)                                *)
                    

let _MSTORE_WHOLECODE vm =
    let vm      =   CODESIZE                        @>> vm  in  (*                                                  size >> .. *)
    let vm      =   _MALLOC                             vm  in  (*                                   alloc(size) >> size >> .. *)
    let vm      =   PUSH(Int 0)                     @>> vm  in  (*                             0  >> alloc(size) >> size >> .. *)
    let vm      =   DUP3                            @>> vm  in  (*                    size >>  0  >> alloc(size) >> size >> .. *)
    let vm      =   SWAP2                           @>> vm  in  (*                    alloc(size) >>  0  >> size >> size >> .. *)
                    CODECOPY                        @>> vm      (*                                                  size >> .. *)

let _PUSH_MHASH m vm =
    let b       =   Crpt.(big_of_hex $ hash_ty_mthd)m       in  
                    PUSH(Big b)                     @>> vm    

let _PUSH_EVHASH ev vm =
    let b       =   Crpt.(big_of_hex $ hash_of_evnt)ev      in  
                    PUSH(Big b)                     @>> vm             

let _MSTORE_MHASH mthd vm =
    let vm      =   PUSH(Int 0x04)                  @>> vm  in  (*                                                     4 >> .. *)
    let vm      =   _MALLOC                             vm  in  (*                                         alloc(4) >> 4 >> .. *)
    let vm      =   _PUSH_MHASH mthd                    vm  in  (*                                 hash >> alloc(4) >> 4 >> .. *)
    let vm      =   DUP2                            @>> vm  in  (*                     alloc(4) >> hash >> alloc(4) >> 4 >> .. *)
                    MSTORE                          @>> vm      (* M[alloc(4)] := hash                     alloc(4) >> 4 >> .. *)







(******************************************************)
(***     0. ALIGNMENT R ? L ?                       ***)
(******************************************************)
                
type alignment              = L | R

let _SHIFT_IF_L aln ty vm  = match aln with 
    | R                     ->  vm
    | L                     ->  let size = size_of_ty ty in
                                assert (size <= 32) ;
                                _SHL ((32-size)*8) vm

let push_loc loc aln ty vm  = match loc with 
    | Code       _          ->  err "push_loc: Code"  
    | Calldata range        ->  _CALLDATALOAD range     vm
    | Stor     range        ->  let vm = push_storRange vm range in 
                                _SHIFT_IF_L aln ty vm
    | Stack      n          ->  let vm = _DUP_N_FRM_BOT n vm in 
                                _SHIFT_IF_L aln ty vm










(**   Storage Setup       **)                   

let _RESET_AC         vm = 
    let vm      = PUSH (Int 1)                      @>> vm      in  (*                                           1 >> .. *)
    let vm      = PUSH (Int 1)                      @>> vm      in  (*                                      1 >> 1 >> .. *)
                  SSTORE                            @>> vm          (* S[1]:=1                                        .. *) 

let if_not_init_AC   vm = 
    let exit    = fresh_label ()                                in  (*                                                .. *) 
    let vm      = PUSH (Int 1)                      @>> vm      in  (*                                          AC >> .. *) 
    let vm      = SLOAD                             @>> vm      in  (*                                       S[AC] >> .. *)
    let vm      = _GOTO_IF exit                         vm      in  (* IF S[AC] != 0 GOTO exit                        .. *)
    let vm      = _RESET_AC                             vm      in  (* S[AC] := 1                                     .. *) 
                  JUMPDEST exit                     @>> vm          (*                                                .. *)

let _SALLOC_ARR vm (a_i:int) =   
    let exit    = fresh_label()                                 in  (*                                                .. *) 
    let vm      = PUSH (Int a_i)                    @>> vm      in  (*                                        a_i  >> .. *)
    let vm      = SLOAD                             @>> vm      in  (*                                      S[a_i] >> .. *) 
    let vm      = _GOTO_IF exit                         vm      in  (* IF S[a_i]!=0 GOTO exit                         .. *) 
    let vm      = _PLUS_S 1 1                           vm      in  (*                                     S[AC]++ >> .. *)      
    let vm      = PUSH (Int a_i)                    @>> vm      in  (*                                a_i >> S[AC] >> .. *)
    let vm      = SSTORE                            @>> vm      in  (* S[a_i] := S[AC]                                .. *)
                  JUMPDEST exit                     @>> vm          (*                                                .. *)

let _SALLOC_ARRS cn vm =
    let vm      = if_not_init_AC                        vm      in   
                  foldl _SALLOC_ARR vm (arr_locs_of_cn cn)










(*  INITIAL DATA  *) 


let _CHK_CODESIZE cnidx vm     =  
    let vm      = PUSH (InitDataSize cnidx)         @>> vm      in  (*                                    datasize >> mem_start >> size >> .. *)
    let vm      = CODESIZE                          @>> vm      in  (*                        codesize >> datasize >> mem_start >> size >> .. *) 
                  _THROW_IF_NEQ                         vm          (* IF not eq THEN error                                                   *) 



                  
let _MSTORE_VARGS cn vm =                                             (* This copies fieldVars at the end of the bytecode into MEM.             *) 
    let size    = size_of_vars_in_cn cn                         in  (* M[0x40](==M[64]) is increased accordingly                              *)
    let vm      = PUSH (Int size)                   @>> vm      in  (*                                                             size >> .. *)
    let vm      = DUP1                              @>> vm      in  (*                                                     size >> size >> .. *)
    let vm      = _MALLOC                               vm      in  (*                                              alloc(size) >> size >> .. *)
    let vm      = DUP2                              @>> vm      in  (*                                      size >> alloc(size) >> size >> .. *)
    let vm      = DUP1                              @>> vm      in  (*                              size >> size >> alloc(size) >> size >> .. *)
    let vm      = CODESIZE                          @>> vm      in  (*                  codesize >> size >> size >> alloc(size) >> size >> .. *)
    let vm      = SUB                               @>> vm      in  (*                     codesize-size >> size >> alloc(size) >> size >> .. *)
    let vm      = DUP3                              @>> vm      in  (*      alloc(size) >> codesize-size >> size >> alloc(size) >> size >> .. *)
                  CODECOPY                          @>> vm          (*          to             from                 alloc(size) >> size >> .. *)

let _SSTORE_VARGS cnidx vm   =                               
    let loop    = fresh_label()                                 in
    let exit    = fresh_label()                                 in  (*                                                  alloc(size)   >>   size    >> .. *)  
    let vm      = PUSH (StorVarBegin cnidx)         @>> vm      in  (*                                          idx >>  alloc(size)   >>   size    >> .. *)
    let vm   = JUMPDEST loop                        @>> vm      in  (*                                          idx >>  alloc(size)   >>   size    >> .. *)
    let vm      = DUP3                              @>> vm      in  (*                                 size >>  idx >>  alloc(size)   >>   size    >> .. *)
    let vm      = _GOTO_IFN exit                        vm      in  (* IF size == 0 GOTO exit                   idx >>  alloc(size)   >>   size    >> .. *)   
    let vm      = DUP2                              @>> vm      in  (*                            mem_start >>  idx >>  alloc(size)   >>   size    >> .. *) 
    let vm      = MLOAD                             @>> vm      in  (*                         M[mem_start] >>  idx >>  alloc(size)   >>   size    >> .. *)
    let vm      = DUP2                              @>> vm      in  (*                  idx >> M[mem_start] >>  idx >>  alloc(size)   >>   size    >> .. *)
    let vm      = SSTORE                            @>> vm      in  (* S[idx] := M[mem_start]                   idx >>  alloc(size)   >>   size    >> .. *)  
    let vm      = PUSH (Int 0x20)                   @>> vm      in  (*                                 0x20 >>  idx >>  alloc(size)   >>   size    >> .. *)
    let vm      = SWAP1                             @>> vm      in  (*                                  idx >> 0x20 >>  alloc(size)   >>   size    >> .. *)
    let vm      = SWAP3                             @>> vm      in  (*                                 size >> 0x20 >>  alloc(size)   >>   idx     >> .. *)
    let vm      = SUB                               @>> vm      in  (*                                   size- 0x20 >>  alloc(size)   >>   idx     >> .. *)
    let vm      = SWAP2                             @>> vm      in  (*                                          idx >>  alloc(size)   >> size-0x20 >> .. *) 
    let vm      = _PLUS 1                               vm      in  (*                                        idx+1 >>  alloc(size)   >> size-0x20 >> .. *)
    let vm      = SWAP1                             @>> vm      in  (*                                    mem_start >>       idx+1    >> size-0x20 >> .. *)
    let vm      = _PLUS 0x20                            vm      in  (*                               mem_start+0x20 >>       idx+1    >> size-0x20 >> .. *)
    let vm      = SWAP1                             @>> vm      in  (*                                        idx+1 >> mem_start+0x20 >> size-0x20 >> .. *)
    let vm      = _GOTO loop                            vm      in  (*                                        idx+1 >> mem_start+0x20 >> size-0x20 >> .. *)
    let vm   = JUMPDEST exit                        @>> vm      in  (*                                          idx >>   mem_start    >>   size    >> .. *)
                  repeat POP 3                          vm          (*                                                                     .. *)

