
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

let throw vm         = (* the same with solc. *)
    let vm      =   PUSH error_label                @>> vm  in
                    JUMP                            @>> vm 

let throw_if vm     =                                           (*                               cond >> .. *)
    let vm      =   PUSH error_label                @>> vm  in  (*                   errlabel >> cond >> .. *)
                    JUMPI                           @>> vm      (* if cond then goto err                 .. *) 

let throw_if_0 vm    =                                          (*                                  i >> .. *)   
    let vm      =   DUP1                            @>> vm  in  (*                             i >> i >> .. *)
    let vm      =   ISZERO                          @>> vm  in  (*                 i==0 ? 1 : 0  >> i >> .. *)
                    throw_if                            vm      (*                                  i >> .. *) 
      
let throw_if_NEQ vm   =                                         (*                             a >> b >> .. *) 
    let vm      =   EQ                              @>> vm  in  (*                               a==b >> .. *)
    let vm      =   ISZERO                          @>> vm  in  (*                               a!=b >> .. *)    
                    throw_if                            vm      (* if a!=b then throw                    .. *) 

let goto    la vm   = 
    let vm      =   PUSH(Label la)                  @>> vm  in 
                    JUMP                            @>> vm 

let if_GOTO la vm   =                                           (* IF stacktop != 0 GOTO label *)
    let vm      =   PUSH(Label la)                  @>> vm  in  
                    JUMPI                           @>> vm 

let if_0_GOTO la vm =                                           (* IF stacktop == 0 GOTO label *)
    let vm      =   ISZERO                          @>> vm  in 
                    if_GOTO la                          vm  

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

let dup_nth_from_bottom n vm  =
                    dup_succ(vm.stack_height - n)   @>> vm 

let shiftR bits vm =
    assert (0 <= bits && bits < 256) ; 
    if bits=0 then vm else                                      (*                                         x >> .. *) 
    let vm      =   PUSH (Int bits)                 @>> vm  in  (*                                 bits >> x >> .. *)
                    SHR                             @>> vm      (*                               x/(2**bits) >> .. *) 

let shiftL bits vm =
    assert (0 <= bits && bits < 256) ; 
    if bits=0 then vm else                                      (*                                         x >> .. *)
    let vm      =   PUSH (Int bits)                 @>> vm  in  (*                                 bits >> x >> .. *)                   
                    SHL                             @>> vm      (*                               (2**bits)*x >> .. *) 

let incr       n vm =
    let vm      =   PUSH (Int n)                    @>> vm  in
                    ADD                             @>> vm      

let sincr idx  n vm = 
    let vm      =   PUSH(Int idx)                   @>> vm  in  (*                                         i >> .. *) 
    let vm      =   SLOAD                           @>> vm  in  (*                                      S[i] >> .. *) 
    let vm      =   DUP1                            @>> vm  in  (*                              S[i] >> S[i] >> .. *) 
    let vm      =   incr n                              vm  in  (*                            S[i]+1 >> S[i] >> .. *) 
    let vm      =   PUSH(Int idx)                   @>> vm  in  (*                       i >> S[i]+1 >> S[i] >> .. *) 
                    SSTORE                          @>> vm      (* S[i]:=S[i]+1                         S[i] >> .. *) 

let calldataload data vm =
    assert (0 < data.size && data.size <= 32);
    let vm      =   PUSH (Int data.offst)           @>> vm  in
                    CALLDATALOAD                    @>> vm  

let keccak_cat vm =                                             (*                                    a >> b >> .. *) 
    let vm      =   PUSH (Int 0x00)                 @>> vm  in  (*                            0x00 >> a >> b >> .. *)
    let vm      =   MSTORE                          @>> vm  in  (* M[0x00]=a                               b >> .. *)
    let vm      =   PUSH (Int 0x20)                 @>> vm  in  (*                                 0x20 >> b >> .. *)
    let vm      =   MSTORE                          @>> vm  in  (* M[0x20]=b                                    .. *) 
    let vm      =   PUSH (Int 0x40)                 @>> vm  in  (*                                      0x40 >> .. *)
    let vm      =   PUSH (Int 0x00)                 @>> vm  in  (*                              0x0  >> 0x40 >> .. *)
                    SHA3                            @>> vm      (*                       sha3(M[0x00..0x3F]) >> .. *)
                                                                (*                          sha3(a++b)             *)

let check_NOT_GT bound vm =                                     (*                                         x >> .. *)
    let vm      =   DUP1                            @>> vm  in  (*                                    x >> x >> .. *)
    let vm      =   PUSH bound                      @>> vm  in  (*                           bound >> x >> x >> .. *) 
    let vm      =   LT                              @>> vm  in  (*                          bound<x?1:0 >> x >> .. *) 
                    throw_if                            vm      (* IF x<bound THEN error                   x >> .. *)

let check_NOT_LT bound vm =                                     (*                                         x >> .. *)
    let vm      =   DUP1                            @>> vm  in  (*                                    x >> x >> .. *)
    let vm      =   PUSH bound                      @>> vm  in  (*                           bound >> x >> x >> .. *) 
    let vm      =   GT                              @>> vm  in  (*                          bound>x?1:0 >> x >> .. *) 
                    throw_if                            vm      (* IF x<bound then error                   x >> .. *)


(******************************************************)
(***     4. PROGRAM COUNTER on STORAGE              ***)
(******************************************************)
                
let reset_PC vm     =
    let vm      =   PUSH StorPC                     @>> vm    in  (*                                       0 >> .. *)
    let vm      =   SLOAD                           @>> vm    in  (*                                    S[0] >> .. *)
    let vm      =   PUSH StorPC                     @>> vm    in  (*                               0 >> S[0] >> .. *)
    let vm      =   DUP1                            @>> vm    in  (*                          0 >> 0 >> S[0] >> .. *)
                    SSTORE                          @>> vm        (* S'[0]=0                            S[0] >> .. *)

let restore_PC vm   =                                             (*                                  bkp_PC >> .. *)
    let vm      =   PUSH StorPC                     @>> vm    in  (*                             0 >> bkp_PC >> .. *)
                    SSTORE                          @>> vm        (* S'[0]=bkp_pc                               .. *)             

let set_PC idx vm   =                                             (*                                            .. *)
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
let _HP_MIN     =   Int 0x1000000    (* Initial HEAP Head *) 
let _MSP        =   Int 0x80         (* Memory Stack : Another Stack different from EVM Stack *) 
let _MS_MIN     =   Int 0x800000
let _MS_MAX     =   let Int i = _HP_MIN in Int (i-1)  
let _EP         =   Int 0x60         (* Escaping Variable Record Pointer *) 
let _EP_MIN     =   Int 0x100
let _EP_MAX     =   let Int i = _MS_MIN in Int (i-2)


let push_MSP    =   PUSH _MSP
let push_EP     =   PUSH _EP 
let push_MS_MIN =   PUSH _MS_MIN
let push_MS_MAX =   PUSH _MS_MAX
let push_EP_MIN =   PUSH _EP_MIN
let push_EP_MAX =   PUSH _EP_MAX
let push_HP     =   PUSH _HP
let push_HP_MIN =   PUSH _HP_MIN


let mPUSH_from_STACK vm = 
    let vm      =   push_MSP                        @>> vm  in  (*                                                         sp >> x >> .. *)
    let vm      =   MLOAD                           @>> vm  in  (*                                                      M[sp] >> x >> .. *) 
    let vm      =   check_NOT_GT _MS_MAX                vm  in  (*                                                      M[sp] >> x >> .. *)
    let vm      =   DUP1                            @>> vm  in  (*                                             M[sp] >> M[sp] >> x >> .. *)         
    let vm      =   PUSH(Int 0x20)                  @>> vm  in  (*                                    0x20 >>  M[sp] >> M[sp] >> x >> .. *)
    let vm      =   ADD                             @>> vm  in  (*                                        0x20+M[sp] >> M[sp] >> x >> .. *)
    let vm      =   push_MSP                        @>> vm  in  (*                                  SP >> 0x20+M[sp] >> M[sp] >> x >> .. *)
    let vm      =   MSTORE                          @>> vm  in  (* M[sp]    := M[sp]+0x20                               M[sp] >> x >> .. *)
                    MSTORE                          @>> vm      (* M[M[sp]] := x                                                      .. *) 

let mPUSH x vm  =   mPUSH_from_STACK ( PUSH x  @>> vm  ) 

let ePUSH vm        =                                           (*                                                   x >> retlabel >> .. *) 
    let vm      =   push_EP                         @>> vm  in  (*                                             EP >> x >> retlabel >> .. *)
    let vm      =   MLOAD                           @>> vm  in  (*                                          M[EP] >> x >> retlabel >> .. *)
    let vm      =   check_NOT_GT _EP_MAX                vm  in  (*                                          M[EP] >> x >> retlabel >> .. *)
    let vm      =   DUP1                            @>> vm  in  (*                                 M[EP] >> M[EP] >> x >> retlabel >> .. *)
    let vm      =   PUSH(Int 0x40)                  @>> vm  in  (*                         0x40 >> M[EP] >> M[EP] >> x >> retlabel >> .. *)
    let vm      =   ADD                             @>> vm  in  (*                            0x40+M[EP] >> M[EP] >> x >> retlabel >> .. *)
    let vm      =   push_EP                         @>> vm  in  (*                      EP >> 0x40+M[EP] >> M[EP] >> x >> retlabel >> .. *)
    let vm      =   MSTORE                          @>> vm  in  (* M[EP] := M[EP]+0x40                      M[EP] >> x >> retlabel >> .. *)
    let vm      =   SWAP1                           @>> vm  in  (*                                          x >> M[EP] >> retlabel >> .. *)              
    let vm      =   DUP2                            @>> vm  in  (*                                M[EP] >>  x >> M[EP] >> retlabel >> .. *) 
    let vm      =   MSTORE                          @>> vm  in  (* M[M[EP]] := x                                 M[EP] >> retlabel >> .. *)
    let vm      =   push_EP                         @>> vm  in  (*                                       0x20 >> M[EP] >> retlabel >> .. *)
    let vm      =   ADD                             @>> vm  in  (*                                          0x20+M[EP] >> retlabel >> .. *)
                    MSTORE                          @>> vm      (* M[M[EP]+0x20] := retAddr                                           .. *)

let get_escaped_arg vm =                                        (*                                                                    .. *)
    let vm      =   PUSH (Int 0x40)                 @>> vm  in  (*                                                            0x40 >> .. *)
    let vm      =   push_EP                         @>> vm  in  (*                                                      EP >> 0x40 >> .. *)
    let vm      =   MLOAD                           @>> vm  in  (*                                                   M[EP] >> 0x40 >> .. *)
    let vm      =   SUB                             @>> vm  in  (*                                                      M[EP]-0x40 >> .. *)
                    MLOAD                           @>> vm      (*                                                     escaped_arg >> .. *)

let ePOP vm =                                                   (*                                                                    .. *)  
    let vm      =   push_EP                         @>> vm  in  (*                                                             EP  >> .. *)
    let vm      =   MLOAD                           @>> vm  in  (*                                                           M[EP] >> .. *) 
    let vm      =   PUSH (Int 0x40)                 @>> vm  in  (*                                                   0x40 >> M[EP] >> .. *)
    let vm      =   PUSH (Int 0x20)                 @>> vm  in  (*                                           0x20 >> 0x40 >> M[EP] >> .. *) 
    let vm      =   DUP3                            @>> vm  in  (*                                  M[EP] >> 0x20 >> 0x40 >> M[EP] >> .. *)  
    let vm      =   SUB                             @>> vm  in  (*                                     M[EP]-0x20 >> 0x40 >> M[EP] >> .. *)
    let vm      =   check_NOT_LT _EP_MIN                vm  in  (*                                     M[EP]-0x20 >> 0x40 >> M[EP] >> .. *)
    let vm      =   MLOAD                           @>> vm  in  (*                                        retAddr >> 0x40 >> M[EP] >> .. *)
    let vm      =   SWAP2                           @>> vm  in  (*                                        M[EP] >> 0x40 >> retAddr >> .. *)
    let vm      =   SUB                             @>> vm  in  (*                                           M[EP]-0x40 >> retAddr >> .. *)
    let vm      =   push_EP                         @>> vm  in  (*                                     EP >> M[EP]-0x40 >> retAddr >> .. *)
                    MSTORE                          @>> vm      (* M[EP] := M[EP]-0x40                                     retAddr >> .. *)

let mPOP_to_STACK vm = 
    let vm      =   PUSH (Int 0x20)                 @>> vm  in  (*                                                            0x20 >> .. *) 
    let vm      =   push_MSP                        @>> vm  in  (*                                                      sp >> 0x20 >> .. *)
    let vm      =   MLOAD                           @>> vm  in  (*                                                   M[sp] >> 0x20 >> .. *) 
    let vm      =   check_NOT_LT _MS_MIN                vm  in  (*                                                   M[sp] >> 0x20 >> .. *)    
    let vm      =   SUB                             @>> vm  in  (*                                                      M[sp]-0x20 >> .. *)
    let vm      =   DUP1                            @>> vm  in  (*                                        M[sp]-0x20 >> M[sp]-0x20 >> .. *)
    let vm      =   push_MSP                        @>> vm  in  (*                                  SP >> M[sp]-0x20 >> M[sp]-0x20 >> .. *) 
    let vm      =   MSTORE                          @>> vm  in  (* M[sp] := M[sp]-0x20                                  M[sp]-0x20 >> .. *) 
                    MLOAD                           @>> vm      (*                                                   M[M[sp]-0x20] >> .. *)

let mPOP vm =   
    let vm      =   PUSH (Int 0x20)                 @>> vm  in  (*                                                            0x20 >> .. *) 
    let vm      =   push_MSP                        @>> vm  in  (*                                                     SP  >> 0x20 >> .. *)
    let vm      =   MLOAD                           @>> vm  in  (*                                                   M[SP] >> 0x20 >> .. *) 
    let vm      =   check_NOT_LT _MS_MIN                vm  in  (*                                                   M[SP] >> 0x20 >> .. *)
    let vm      =   SUB                             @>> vm  in  (*                                                      M[SP]-0x20 >> .. *)
    let vm      =   push_MSP                        @>> vm  in  (*                                                SP >> M[SP]-0x20 >> .. *) 
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
    let vm      =   push_HP_MIN                     @>> vm  in
    let vm      =   push_HP                         @>> vm  in
                    MSTORE                          @>> vm    

let malloc vm   =                                               (*  STACK                                            len >> .. *)
    let vm      =   push_HP                         @>> vm  in  (*                                             64 >> len >> .. *)
    let vm      =   DUP1                            @>> vm  in  (*                                       64 >> 64 >> len >> .. *)
    let vm      =   MLOAD                           @>> vm  in  (*                                    M[64] >> 64 >> len >> .. *)
    let vm      =   DUP1                            @>> vm  in  (*                           M[64] >> M[64] >> 64 >> len >> .. *)
    let vm      =   SWAP3                           @>> vm  in  (*                           len >> M[64] >> 64 >> M[64] >> .. *)
    let vm      =   ADD                             @>> vm  in  (*                              M[64+len] >> 64 >> M[64] >> .. *)
    let vm      =   SWAP1                           @>> vm  in  (*                              64 >> M[64+len] >> M[64] >> .. *)
                    MSTORE                          @>> vm      (*                                                 M[64] >> .. *) 

let get_malloc vm   =                             
    let vm      =   push_HP                         @>> vm  in  (*                                                0x40   >> .. *) 
                    MLOAD                           @>> vm      (*                                              M[0x40]  >> .. *) 
      
let mstore_code vm  =                                           (*                                           idx >> size >> .. *)
    let vm      =   DUP2                            @>> vm  in  (*                                   size >> idx >> size >> .. *)
    let vm      =   malloc                              vm  in  (*                            alloc(size) >> idx >> size >> .. *)
    let vm      =   SWAP1                           @>> vm  in  (*                            idx >> alloc(size) >> size >> .. *)
    let vm      =   DUP3                            @>> vm  in  (*                    size >> idx >> alloc(size) >> size >> .. *)
    let vm      =   SWAP1                           @>> vm  in  (*                    idx >> size >> alloc(size) >> size >> .. *)
    let vm      =   DUP3                            @>> vm  in  (*     alloc(size) >> idx >> size >> alloc(size) >> size >> .. *) 
                    CODECOPY                        @>> vm      (*                                   alloc(size) >> size >> .. *)  

let mstore_whole_code vm =
    let vm      =   CODESIZE                        @>> vm  in  (*                                                  size >> .. *)
    let vm      =   DUP1                            @>> vm  in  (*                                          size >> size >> .. *)
    let vm      =   malloc                              vm  in  (*                                   alloc(size) >> size >> .. *)
    let vm      =   DUP2                            @>> vm  in  (*                           size >> alloc(size) >> size >> .. *)
    let vm      =   PUSH(Int 0)                     @>> vm  in  (*                     0  >> size >> alloc(size) >> size >> .. *)
    let vm      =   DUP3                            @>> vm  in  (*     alloc(size) >>  0  >> size >> alloc(size) >> size >> .. *)
                    CODECOPY                        @>> vm      (*       to           from           alloc(size) >> size >> .. *)

let push_mthd_hash m vm =
    let b       =   Crpt.(big_of_hex $ hash_ty_mthd)m       in  
                    PUSH(Big b)                     @>> vm    

let push_evnt_hash ev vm =
    let b       =   Crpt.(big_of_hex $ hash_of_evnt)ev      in  
                    PUSH(Big b)                     @>> vm             

let mstore_mthd_hash mthd vm =
    let vm      =   PUSH(Int 0x04)                  @>> vm  in  (*                                                     4 >> .. *)
    let vm      =   DUP1                            @>> vm  in  (*                                               4  >> 4 >> .. *)
    let vm      =   malloc                              vm  in  (*                                         alloc(4) >> 4 >> .. *)
    let vm      =   push_mthd_hash mthd                 vm  in  (*                                 hash >> alloc(4) >> 4 >> .. *)
    let vm      =   DUP2                            @>> vm  in  (*                     alloc(4) >> hash >> alloc(4) >> 4 >> .. *)
                    MSTORE                          @>> vm      (* M[alloc(4)] := hash                     alloc(4) >> 4 >> .. *)







(******************************************************)
(***     0. ALIGNMENT R ? L ?                       ***)
(******************************************************)
                
type alignment              = L | R

let shift_by_aln aln ty vm  = match aln with 
    | R                     ->  vm
    | L                     ->  let size = size_of_ty ty in
                                assert (size <= 32) ;
                                shiftL ((32-size)*8) vm

let push_loc loc aln ty vm  = match loc with 
    | Code       _          ->  err "push_loc: Code"  
    | Calldata range        ->  calldataload range      vm
    | Stor     range        ->  let vm = push_storRange vm range in 
                                shift_by_aln aln ty vm
    | Stack      n          ->  let vm = dup_nth_from_bottom n vm in 
                                shift_by_aln aln ty vm










(**   Storage Setup       **)                   

let reset_AC         vm = 
    let vm      = PUSH (Int 1)                      @>> vm      in  (*                                           1 >> .. *)
    let vm      = DUP1                              @>> vm      in  (*                                      1 >> 1 >> .. *)
                  SSTORE                            @>> vm          (* S[1]:=1                                        .. *) 

let if_not_init_AC   vm = 
    let exit    = fresh_label ()                                in  (*                                                .. *) 
    let vm      = PUSH (Int 1)                      @>> vm      in  (*                                          AC >> .. *) 
    let vm      = SLOAD                             @>> vm      in  (*                                       S[AC] >> .. *)
    let vm      = if_GOTO exit                          vm      in  (* IF S[AC] != 0 GOTO exit                        .. *)
    let vm      = reset_AC                              vm      in  (* S[AC] := 1                                     .. *) 
                  JUMPDEST exit                     @>> vm          (*                                                .. *)

let salloc_arr vm (a_i:int) =   
    let exit    = fresh_label()                                 in  (*                                                .. *) 
    let vm      = PUSH (Int a_i)                    @>> vm      in  (*                                        a_i  >> .. *)
    let vm      = SLOAD                             @>> vm      in  (*                                      S[a_i] >> .. *) 
    let vm      = if_GOTO exit                          vm      in  (* IF S[a_i]!=0 GOTO exit                         .. *) 
    let vm      = sincr 1 1                             vm      in  (*                                     S[AC]++ >> .. *)      
    let vm      = PUSH (Int a_i)                    @>> vm      in  (*                                a_i >> S[AC] >> .. *)
    let vm      = SSTORE                            @>> vm      in  (* S[a_i] := S[AC]                                .. *)
                  JUMPDEST exit                     @>> vm          (*                                                .. *)

let salloc_arrs cn vm =
    let vm      = if_not_init_AC                        vm      in   
                  foldl salloc_arr vm (arr_locs_of_cn cn)










(*  INITIAL DATA  *) 


let check_codesize cnidx vm     =  
    let vm      = PUSH (InitDataSize cnidx)         @>> vm      in  (*                                    datasize >> mem_start >> size >> .. *)
    let vm      = CODESIZE                          @>> vm      in  (*                        codesize >> datasize >> mem_start >> size >> .. *) 
                  throw_if_NEQ                          vm          (* IF not eq THEN error                                                   *) 



                  
let mstore_var_args cn vm =                                             (* This copies fieldVars at the end of the bytecode into MEM.             *) 
    let size    = size_of_vars_in_cn cn                         in  (* M[0x40](==M[64]) is increased accordingly                              *)
    let vm      = PUSH (Int size)                   @>> vm      in  (*                                                             size >> .. *)
    let vm      = DUP1                              @>> vm      in  (*                                                     size >> size >> .. *)
    let vm      = malloc                                vm      in  (*                                              alloc(size) >> size >> .. *)
    let vm      = DUP2                              @>> vm      in  (*                                      size >> alloc(size) >> size >> .. *)
    let vm      = DUP1                              @>> vm      in  (*                              size >> size >> alloc(size) >> size >> .. *)
    let vm      = CODESIZE                          @>> vm      in  (*                  codesize >> size >> size >> alloc(size) >> size >> .. *)
    let vm      = SUB                               @>> vm      in  (*                     codesize-size >> size >> alloc(size) >> size >> .. *)
    let vm      = DUP3                              @>> vm      in  (*      alloc(size) >> codesize-size >> size >> alloc(size) >> size >> .. *)
                  CODECOPY                          @>> vm          (*          to             from                 alloc(size) >> size >> .. *)

let sstore_var_args cnidx vm   =                               
    let loop    = fresh_label()                                 in
    let exit    = fresh_label()                                 in  (*                                                  alloc(size)   >>   size    >> .. *)  
    let vm      = PUSH (StorVarBegin cnidx)         @>> vm      in  (*                                          idx >>  alloc(size)   >>   size    >> .. *)
    let vm   = JUMPDEST loop                        @>> vm      in  (*                                          idx >>  alloc(size)   >>   size    >> .. *)
    let vm      = DUP3                              @>> vm      in  (*                                 size >>  idx >>  alloc(size)   >>   size    >> .. *)
    let vm      = if_0_GOTO exit                        vm      in  (* IF size == 0 GOTO exit                   idx >>  alloc(size)   >>   size    >> .. *)   
    let vm      = DUP2                              @>> vm      in  (*                            mem_start >>  idx >>  alloc(size)   >>   size    >> .. *) 
    let vm      = MLOAD                             @>> vm      in  (*                         M[mem_start] >>  idx >>  alloc(size)   >>   size    >> .. *)
    let vm      = DUP2                              @>> vm      in  (*                  idx >> M[mem_start] >>  idx >>  alloc(size)   >>   size    >> .. *)
    let vm      = SSTORE                            @>> vm      in  (* S[idx] := M[mem_start]                   idx >>  alloc(size)   >>   size    >> .. *)  
    let vm      = PUSH (Int 0x20)                   @>> vm      in  (*                                 0x20 >>  idx >>  alloc(size)   >>   size    >> .. *)
    let vm      = SWAP1                             @>> vm      in  (*                                  idx >> 0x20 >>  alloc(size)   >>   size    >> .. *)
    let vm      = SWAP3                             @>> vm      in  (*                                 size >> 0x20 >>  alloc(size)   >>   idx     >> .. *)
    let vm      = SUB                               @>> vm      in  (*                                   size- 0x20 >>  alloc(size)   >>   idx     >> .. *)
    let vm      = SWAP2                             @>> vm      in  (*                                          idx >>  alloc(size)   >> size-0x20 >> .. *) 
    let vm      = incr 1                                vm      in  (*                                        idx+1 >>  alloc(size)   >> size-0x20 >> .. *)
    let vm      = SWAP1                             @>> vm      in  (*                                    mem_start >>       idx+1    >> size-0x20 >> .. *)
    let vm      = incr 0x20                             vm      in  (*                               mem_start+0x20 >>       idx+1    >> size-0x20 >> .. *)
    let vm      = SWAP1                             @>> vm      in  (*                                        idx+1 >> mem_start+0x20 >> size-0x20 >> .. *)
    let vm      = goto loop                             vm      in  (*                                        idx+1 >> mem_start+0x20 >> size-0x20 >> .. *)
    let vm   = JUMPDEST exit                        @>> vm      in  (*                                          idx >>   mem_start    >>   size    >> .. *)
                  repeat POP 3                          vm          (*                                                                     .. *)

