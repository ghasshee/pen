
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
open CodeSnipets
open Evm
open Context
open Type

module Crpt  = Crypto 
module BL   = BatList
module L    = List


let calldatasize (TyMthd(_,args,_)) =
    4 (* for signature *) + size_of_args args   

(******************************************************)
(***     0. ALIGNMENT R ? L ?                       ***)
(******************************************************)
                
type alignment              = L | R

let align_addr ce           = function 
    | R                     ->  ce
    | L                     ->  shiftLtop ce (12 * 8)
let align_to_L ce ty        = function 
    | R                     ->  ce
    | L                     ->  let size = size_of_ty ty in
                                assert (size <= 32) ;
                                shiftLtop ce ((32-size)*8) 

type memoryPack             = Tight   (* [Tight] uses [size_of_ty] bytes    on mem *) 
                            | ABIpk   (* [ABI]   uses multiples of 32 bytes on mem *) 


(*****************************************)
(***  1. CONTRACT CREATION CODE        ***)
(*****************************************)

(**   1.1  Var Fileds Setup        **) 
let mstore_fieldVars ce cn      =                               (* This copies fieldVars at the end of the bytecode into MEM.             *) 
    let size    = size_of_vars_in_cn cn                     in  (* M[0x40](==M[64]) is increased accordingly                              *)
    let ce      = PUSH32(Int size)                  >>>>ce    in  (*                                                             size >> .. *)
    let ce      = DUP1                              >>>>ce    in  (*                                                     size >> size >> .. *)
    let ce      = malloc                              ce    in  (*                                              alloc(size) >> size >> .. *)
    let ce      = DUP2                              >>>>ce    in  (*                                      size >> alloc(size) >> size >> .. *)
    let ce      = DUP1                              >>>>ce    in  (*                              size >> size >> alloc(size) >> size >> .. *)
    let ce      = CODESIZE                          >>>>ce    in  (*                  codesize >> size >> size >> alloc(size) >> size >> .. *)
    let ce      = SUB                               >>>>ce    in  (*                     codesize-size >> size >> alloc(size) >> size >> .. *)
    let ce      = DUP3                              >>>>ce    in  (*      alloc(size) >> codesize-size >> size >> alloc(size) >> size >> .. *)
                  CODECOPY                          >>>>ce        (*          to             from                 alloc(size) >> size >> .. *)
                                                                (*                                               codebegin                *)
let check_codesize cnidx ce     =  
    let ce      = PUSH32(InitDataSize cnidx)        >>>>ce    in  (*                                    datasize >> mem_start >> size >> .. *)
    let ce      = CODESIZE                          >>>>ce    in  (*                        codesize >> datasize >> mem_start >> size >> .. *) 
                  throw_if_NEQ                        ce        (* IF not eq THEN error *) 

let sstore_fieldVars ce cnidx     =                               
    let label   = fresh_label()                             in
    let exit    = fresh_label()                             in  (*                                                mem_start >> size >> .. *)  
    let ce      = check_codesize cnidx                ce    in 
    let ce      = PUSH32(StorFieldsBegin cnidx)     >>>>ce    in  (*                                          idx >>   mem_start    >>   size    >> .. *)
    let ce   = JUMPDEST label                       >>>>ce    in  (*                                          idx >>   mem_start    >>   size    >> .. *)
    let ce      = DUP3                              >>>>ce    in  (*                                 size >>  idx >>   mem_start    >>   size    >> .. *)
    let ce      = if_0_GOTO exit                      ce    in  (* IF size==0 THEN GOTO exit                idx >>   mem_start    >>   size    >> .. *)   
    let ce      = DUP2                              >>>>ce    in  (*                            mem_start >>  idx >>   mem_start    >>   size    >> .. *) 
    let ce      = MLOAD                             >>>>ce    in  (*                         M[mem_start] >>  idx >>   mem_start    >>   size    >> .. *)
    let ce      = DUP2                              >>>>ce    in  (*                  idx >> M[mem_start] >>  idx >>   mem_start    >>   size    >> .. *)
    let ce      = SSTORE                            >>>>ce    in  (* S[idx]=M[mem_start]                      idx >>   mem_start    >>   size    >> .. *)  
    let ce      = PUSH1(Int 0x20)                   >>>>ce    in  (*                                 0x20 >>  idx >>   mem_start    >>   size    >> .. *)
    let ce      = SWAP1                             >>>>ce    in  (*                                  idx >> 0x20 >>   mem_start    >>   size    >> .. *)
    let ce      = SWAP3                             >>>>ce    in  (*                                 size >> 0x20 >>   mem_start    >>   idx     >> .. *)
    let ce      = SUB                               >>>>ce    in  (*                                   size- 0x20 >>   mem_start    >>   idx     >> .. *)
    let ce      = SWAP2                             >>>>ce    in  (*                                          idx >>   mem_start    >> size-0x20 >> .. *) 
    let ce      = incr_top 1(*word*)                  ce    in  (*                                        idx+1 >>   mem_start    >> size-0x20 >> .. *)
    let ce      = SWAP1                             >>>>ce    in  (*                                    mem_start >>       idx+1    >> size-0x20 >> .. *)
    let ce      = incr_top 0x20                       ce    in  (*                               mem_start+0x20 >>       idx+1    >> size-0x20 >> .. *)
    let ce      = SWAP1                             >>>>ce    in  (*                                        idx+1 >> mem_start+0x20 >> size-0x20 >> .. *)
    let ce      = goto label                          ce    in  (*                                        idx+1 >> mem_start+0x20 >> size-0x20 >> .. *)
    let ce   = JUMPDEST exit                        >>>>ce    in  (*                                          idx >>   mem_start    >>   size    >> .. *)
                  repeat POP 3                        ce        (*                                                                     .. *)
(*  S[1]   := m   <- array seed                                               *)  
(*  S[2]   := the value of arg1 is stored in message call                     *)  
(*            ..                                                              *)  
(*  S[k+1] := the value of argk is stored in message call                     *)  



(**   1.2. Array Fields Setup    **)                   
let reset_salloc_array ce = 
    let ce      = PUSH1 (Int 1)                     >>>>ce    in  (*                                           1 >> .. *)
    let ce      = DUP1                              >>>>ce    in  (*                                      1 >> 1 >> .. *)
                  SSTORE                            >>>>ce        (* S[1]:=1                                        .. *) 

let salloc_fieldArr ce (arrArgLoc:int) =   
    let label   = fresh_label()                             in  (*                                                .. *) 
    let ce      = PUSH4 (Int arrArgLoc)             >>>>ce    in  (*                                        seed >> .. *)
    let ce      = SLOAD                             >>>>ce    in  (*                                     S[seed] >> .. *) 
    let ce      = PUSH4 (Label label)               >>>>ce    in  (*                            label >> S[seed] >> .. *)
    let ce      = JUMPI                             >>>>ce    in  (* IF S[seed]!=0 GOTO label                       .. *) 
    let ce      = sincr 1                             ce    in  (*                                      S[1]++ >> .. *)      
    let ce      = PUSH4 (Int arrArgLoc)             >>>>ce    in  (*                                seed >> S[1] >> .. *)
    let ce      = SSTORE                            >>>>ce    in  (* S[seed]:=S[1]                                  .. *)
                  JUMPDEST label                    >>>>ce        (*                                                .. *)

let init_salloc_fieldArr_if_not ce = 
    let label   = fresh_label ()                            in  (*                                                   *) 
    let ce      = PUSH1 (Int 1)                     >>>>ce    in  (*                                                   *) 
    let ce      = SLOAD                             >>>>ce    in  (*                                                   *)
    let ce      = PUSH4 (Label label)               >>>>ce    in  (*                                                   *) 
    let ce      = JUMPI                             >>>>ce    in  (* IF S[1]!=0 then GOTO label                  >> .. *)
    let ce      = reset_salloc_array                  ce    in  (*                                      1 >> 1 >> .. *) 
                  JUMPDEST label                    >>>>ce    

let setup_fieldArrs ce cn =
    let ce      = init_salloc_fieldArr_if_not         ce    in   
    let arrLocs = fieldArr_locations cn                     in  
                  foldl salloc_fieldArr ce arrLocs 

(**   1.3. Runtime CODE COPY         **) 
let mstore_rntimeCode ce idx =                                  (*                                                           .. *)
    let ce      = PUSH32(RntimeCodeSize)            >>>>ce    in  (*                                                   size >> .. *)
    let ce      = DUP1                              >>>>ce    in  (*                                           size >> size >> .. *)  
    let ce      = malloc                              ce    in  (*                                    alloc(size) >> size >> .. *)
    let ce      = DUP2                              >>>>ce    in  (*                            size >> alloc(size) >> size >> .. *)
    let ce      = PUSH32(RntimeCodeOffset idx)      >>>>ce    in  (*                     idx >> size >> alloc(size) >> size >> .. *)
    let ce      = DUP3                              >>>>ce    in  (*      alloc(size) >> idx >> size >> alloc(size) >> size >> .. *)
                  CODECOPY                          >>>>ce        (*                                    alloc(size) >> size >> .. *)
                                                                (*                                     codebegin                *)
(**   1.4.  CONTRACT CREATION   *****)
type cnstrCode          =   { cnstr_ce           : ce
                            ; cnstr_ty           : ty
                            ; cnstr_cn           : ty toplevel } 

let ce_of_cc cc         =   cc.cnstr_ce
let program_of_cc       =   extract_program $ ce_of_cc  

let codegen_cnstr_bytecode cns idx = (* return ce which contains the program *) 
    let cn      =   lookup idx cns                            in 
    let TmCn(id,flds,mthds) = cn in 
    let ce      =   empty_ce (lookup_cnidx_of_cns cns) cns    in  (*                                                                                 *)
    let ce      =   Comment ("Begin Constructor of Cntract "^id) >>>>ce in 
    let ce      =   init_malloc                     ce        in  (* M[64] := 96                                                                     *)
    let ce      =   mstore_fieldVars                ce cn     in  (*                                            alloc(argssize) << argssize << ..    *)
    let ce      =   sstore_fieldVars                ce idx    in  (* S[i..i+sz-1]:= argCodes               i << alloc(argssize) << argssize << ..    *)
    let ce      =   setup_fieldArrs                 ce cn     in  (* S[1]        := #array                 i << alloc(argssize) << argssize << ..    *)
    let ce      =   set_PC                          ce idx    in  (* S[PC]       := rntime_cn_offst (returned body)                                  *)
    let ce      =   mstore_rntimeCode               ce idx    in  (*                                      alloc(codesize) << codesize << i <<  ..    *)
    let ce      =   RETURN                        >>>>ce        in  (* OUTPUT(M[code]) as The BODY code                                    i <<  ..    *)
                    Comment ("End Constructor of Cntract " ^ id) >>>>ce 

let compile_cnstr cns idx  : cnstrCode =
    let cn      =   L.assoc idx cns in 
    { cnstr_ce           = codegen_cnstr_bytecode cns idx
    ; cnstr_ty           = typeof_cn cn 
    ; cnstr_cn           = cn                                }

let compile_cnstrs cns : cnstrCode idxlist =
    idxmap (compile_cnstr cns) cns


(***************************************)
(***     2.    RUNTIME               ***)
(***************************************)

type rntimeCode             =   { rntime_ce             : ce                                                       
                                ; rntime_cn_offsets     : int idxlist  }

let empty_rntimeCode lookup_cn layouts =
    { rntime_ce             = empty_ce lookup_cn layouts
    ; rntime_cn_offsets     = []                                    }

let init_rntimeCode lookup_cn layouts : rntimeCode =
    let ce      =   empty_ce lookup_cn layouts                      in
    let ce      =   error_loop ce                                   in 
    let ce      =   get_PC                                    ce    in
    let ce      =   JUMP                                    >>>>ce    in
    { rntime_ce             = ce
    ; rntime_cn_offsets     = [] }

(***************************************)
(***     3.  DISPATHER               ***)
(***************************************)

let dispatch_method idx le ce m =                                       (*                                           ABCD >> .. *)  
    let ce      =   DUP1                                    >>>>ce    in  (*                                   ABCD >> ABCD >> .. *)
    let ce      =   push_mthd_hash m                          ce    in  (*                              m >> ABCD >> ABCD >> .. *)
    let ce      =   EQ                                      >>>>ce    in  (*                             m=ABCD?1:0 >> ABCD >> .. *)
    let ce      =   PUSH32(RntimeMthdLabel(idx,m))          >>>>ce    in  (*                Rntime(m) >> m=ABCD?1:0 >> ABCD >> .. *)
                    JUMPI                                   >>>>ce        (* if m=ABCD then GOTO Rntime(m)             ABCD >> .. *)

let dispatch_default idx le ce =
    let ce      =   PUSH32(RntimeMthdLabel(idx,TyDefault))  >>>>ce    in
                    JUMP                                    >>>>ce     

let push_inputdata32_from databegin ce =
    let ce      =   PUSH32 databegin                        >>>>ce    in
                    CALLDATALOAD                            >>>>ce

let dispatcher le ce idx (TmCn(_,_,mthds)) = 
    let tyMthds =   L.map(function TmMthd(head,_) -> head) mthds    in
    let uMthds  =   filter_method tyMthds                           in 
    let ce      =   Comment "BEGIN Method Dispatchers "     >>>>ce    in 
    let ce      =   push_inputdata32_from(Int 0)              ce    in  (*               ABCDxxxxxxxxxxxxxxxxxxxxxxxxxxxxx >> .. *)
    let ce      =   shiftRtop ce Crpt.(word_bits-sig_bits)           in  (*                                            ABCD >> .. *)                             
    let ce      =   foldl(dispatch_method idx le)ce uMthds          in  (* JUMP to Method ABCD                                   *)   
    let ce      =   POP                                     >>>>ce    in  (*                                                    .. *)
    let ce      =   if  default_exists tyMthds
                        then dispatch_default idx      le ce        (* JUMP to Default Method                             .. *) 
                        else throw ce                               in  (* JUMP to error                                      .. *) 
    let ce      =   Comment "END Method Dispatchers "       >>>>ce    in 
    le,ce

(*********************************************)
(***     4.    CODEGEN  PRECONTRACT        ***)
(*********************************************)

let rec codegen_predef_call aln ly le ce id args rety = match id with 
    | "pre_ecdsarecover"    ->  assert(aln=R);  codegen_ECDSArecover ly le ce     args
    | "keccak256"           ->  assert(aln=R);  codegen_keccak256    ly le ce     args    
    | "iszero"              ->                  codegen_iszero       ly le ce aln args rety
    | _                     ->  err "codegen_predef_call: Direct Contract Call is Not supported. Specify a Method."

and codegen_iszero ly le ce aln args reT = match args with
    | [arg] ->  assert(reT=TyBool) ; 
                let ce =  arg                   >>(aln,ly,le,ce)  in
                          ISZERO                >>>>ce 
       
and codegen_keccak256 ly le ce args =
    let ce    = get_malloc ce                                       in  
    let ce    = mstore_mthd_args Tight args ly le ce                in  
    let ce    = SWAP1                           >>>>ce                in  
                SHA3                            >>>>ce                  

and codegen_ECDSArecover ly le ce = function 
    | [h;v;r;s] as args ->
    let ce    = PUSH1 (Int 32)                  >>>>ce                in  
    let ce    = DUP1                            >>>>ce                in  
    let ce    = malloc                            ce                in  
    let ce    = repeat DUP2 2                     ce                in  
    let ce    = get_malloc                        ce                in
    let ce    = mstore_mthd_args ABIpk args ly le ce                in  
    let ce    = SWAP1                           >>>>ce                in  
    let ce    = PUSH1 (Int 0)                   >>>>ce                in  
    let ce    = PUSH1 (Int 1)                   >>>>ce                in  
    let ce    = PUSH4 (Int 10000)               >>>>ce                in  
    let ce    = CALL                            >>>>ce                in  
    let ce    = throw_if_0 ce                                       in
    let ce    = POP                             >>>>ce                in  
    let ce    = SWAP1                           >>>>ce                in  
    let ce    = POP                             >>>>ce                in  
                MLOAD                           >>>>ce                    (* stack: [output] *)
    | _         -> err "pre_ecdsarecover has a wrong number of args"

(*********************************************)
(***    5.    CODEGEN  TERM               ***)
(*********************************************)
(*
 *              ADDRESS              MEMORY 
 *          +---------------------+-----------------+                         
 *          |                  0  | rntimeCode      |                                   
 *          |                ...  |  ...            |                                   
 *          |                ...  | RETURN          |                                   
 *          |               size  | cnstrCode       |
 *          |                ...  |  ...            |
 *          |                ...  | RETURN          |
 *          |         size+wsize  | arg1            |                                   
 *          |                     |  ...            |
 *          |                     | arg2            |
 *          |                     |  ...            |
 *          |---------------------|-----------------|
 *          | argsize+size+wsize  |                 |
 *          |                ...  |                 |             *)                                                                 

and push_loc ce aln ty      = function 
    | Code       _          ->  err "push_loc: Code"  
    | Calldata rng          ->  calldataload  ce rng
    | Stor     rng          ->  let ce = push_storRange ce rng in 
                                align_to_L ce ty aln  
    | Stack      n          ->  let ce = dup_nth_from_bottom n ce in 
                                align_to_L ce ty aln 

and mstore_new_instance id args msg ly le ce  =
    let cnidx   =   lookup_cnidx_of_ce ce id                            in 
    let ce      =   PUSH32(CnstrCodeSize cnidx)         >>>>ce            in  (*                                                        size >> .. *) 
    let ce      =   PUSH32(RntimeCnstrOffset cnidx)     >>>>ce            in  (*                                             cn_idx  >> size >> .. *)
    let ce      =   mstore_code                           ce            in  (*                                         alloc(size) >> size >> .. *)
    let ce      =   SWAP1                               >>>>ce            in  (*                                         size >> alloc(size) >> .. *)
    let ce      =   mstore_whole_code                     ce            in  (*                alloc(wsize) >> wsize >> size >> alloc(size) >> .. *)
    let ce      =   mstore_mthd_args ABIpk args     ly le ce            in  (*    argssize >> alloc(wsize) >> wsize >> size >> alloc(size) >> .. *)
    let ce      =   SWAP1                               >>>>ce            in  (*    alloc(wsize) >> argssize >> wsize >> size >> alloc(size) >> .. *)
    let ce      =   POP                                 >>>>ce            in  (*                    argssize >> wsize >> size >> alloc(size) >> .. *)
    let ce      =   ADD                                 >>>>ce            in  (*                       argssize+wsize >> size >> alloc(size) >> .. *)
    let ce      =   ADD                                 >>>>ce            in  (*                          argssize+wsize+size >> alloc(size) >> .. *)
                    SWAP1                               >>>>ce                (*                                  alloc(size) >>   totalsize >> .. *)

and codegen_new id args msg ly le ce               =    
    let ce      =   reset_PC ce                                         in  (*                                             PCbkp >> .. *)
    let ce      =   mstore_new_instance id args msg ly le ce            in  (*                      alloc(size) >> size >> PCbkp >> .. *)
    let ce      =   msg                           >>(R,ly,le,ce)      in  (*             value >> alloc(size) >> size >> PCbkp >> .. *)
    let ce      =   CREATE                              >>>>ce            in  (*                             createResult >> PCbkp >> .. *)
    let ce      =   throw_if_0                            ce            in  (*                             createResult >> PCbkp >> .. *)
    let ce      =   SWAP1                               >>>>ce            in  (*                             PCbkp >> CreateResult >> .. *)
                    restore_PC                            ce                (*                                      CreateResult >> .. *)

and codegen_array aid aidx ly le ce    =                                    (*                                                      .. *)
    let ce      =   keccak_of_array aid aidx        ly le ce            in  (*                                      keccak(a[i]) >> .. *)
                    SLOAD                               >>>>ce                (*                                   S[keccak(a[i])] >> .. *) 

and keccak_of_array aid aidx ly le ce  =                                    (* S[keccak(a[i])] := the seed of array *) 
    let ce      =   aidx                                >>(R,ly,le,ce)in  (*                                             index >> .. *)    
    let ce      =   aid                                 >>(R,ly,le,ce)in  (*                                array_loc >> index >> .. *)
                    keccak_cat                            ce                (*                            sha3(array_loc++index) >> .. *) 
      
and salloc_array aid aidx ly le ce     =
    let push ce =   keccak_of_array aid aidx        ly le ce            in  (*                                        S[KEC(a_i)] >> .. *)
                    salloc_array_of_push push             ce                (* S[KEC(a_i)]:= newSeed                      newSeed >> .. *)     
                                                                          
and salloc_array_of_loc le ce(Stor data) =       
    assert(data.size=Int 1) ;                                               (*                                        S[KEC(a_i)] >> .. *)     
    let push ce =   PUSH32 data.offst                   >>>>ce            in  
                    salloc_array_of_push push             ce                (* S[KEC(a_i)]:= newSeed                      newSeed >> .. *)     

and salloc_array_of_push push_array_seed ce =   
    let label   =   fresh_label ()                                      in  (*                                        S[KEC(a_i)] >> .. *) 
    let ce      =   DUP1                                >>>>ce            in  (*                          S[KEC(a_i)] >> S[KEC(a_i)] >> .. *) 
    let ce      =   PUSH4(Label label)                  >>>>ce            in  (*                 label >> S[KEC(a_i)] >> S[KEC(a_i)] >> .. *) 
    let ce      =   JUMPI                               >>>>ce            in  (* {IF S[KEC(a_i)]!=0 GOTO label}          S[KEC(a_i)] >> .. *) 
    let ce      =   POP                                 >>>>ce            in  (*                                                      .. *) 
    let ce      =   push_array_seed                       ce            in  
    let ce      =   sincr 1                               ce            in  (*                                 S[1]++ >> KEC(a_i) >> .. *) 
    let ce      =   DUP1                                >>>>ce            in  (*                     newseed >> newseed >> KEC(a_i) >> .. *) 
    let ce      =   SWAP2                               >>>>ce            in  (*                     KEC(a_i) >> newseed >> newseed >> .. *) 
    let ce      =   SSTORE                              >>>>ce            in  (* S[KEC(a_i)]:= newseed                      newseed >> .. *)
                    JUMPDEST label                      >>>>ce                (*                                                         *)

(* le is not updated here.  
 * le can only be updated in a variable initialization *)
and (>>) e (aln,ly,le,ce)         = codegen_tm ly le ce aln e 
and codegen_tm ly le ce aln  e      = pe_tm e; pe(str_of_ctx le); match e with 
    | TmApp(t1,t2)          ,_          ->                  codegen_app     ly le ce (TmApp(t1,t2))
    | TmAbs(x,tyX,t)        ,_          ->                  codegen_abs     ly le ce (TmAbs(x,tyX,t))
    | TmFix(f,n,tyF,t)      ,_          ->                  codegen_fix     ly le ce (TmFix(f,n,tyF,t))
    | TmI(i,n)              ,_          ->                  codegen_idx     ly le ce (TmI(i,n))          
    | TmIStrct(i)           ,_          ->                  codegen_idstrct ly le ce (TmIStrct(i))
    | TmIf(b,t1,t2)         ,_          ->                  codegen_if      ly le ce (TmIf(b,t1,t2)) 
    | EpAddr(c,TyInstnc i)  ,TyAddr     ->                  (c,TyInstnc i)          >>(aln,ly,le,ce) 
    | Balanc e              ,_          ->                  BALANCE >>>> ( e        >>(R,ly,le,ce) )
    | EpValue               ,_          ->                  CALLVALUE               >>>>ce      (* Value (wei) Transferred to the account *) 
    | TmZero                ,_          ->                  PUSH1(Int 0)            >>>>ce 
    | EpNow                 ,_          ->                  TIMESTAMP               >>>>ce 
    | TmFalse               ,_          ->  assert(aln=R);  PUSH1(Int 0)            >>>>ce  
    | TmTrue                ,_          ->  assert(aln=R);  PUSH1(Int 1)            >>>>ce 
    | TmU256  d             ,_          ->  assert(aln=R);  PUSH32(Big d)           >>>>ce  
    | TmU8 d                ,_          ->  assert(aln=R);  PUSH1(Big d)            >>>>ce  
    | TmAdd (l,r)           ,_          ->                  op ADD l r             ly le ce             
    | TmSub(l,r)            ,_          ->                  op SUB l r             ly le ce             
    | TmMul  (l,r)          ,_          ->                  op MUL l r             ly le ce             
    | TmLT   (l,r)          ,_          ->  assert(aln=R);  op LT  l r             ly le ce           
    | TmGT   (l,r)          ,_          ->  assert(aln=R);  op GT  l r             ly le ce           
    | TmEQ   (l,r)          ,_          ->  assert(aln=R);  op EQ  l r             ly le ce           
    | TmNEQ  (l,r)          ,_          ->  assert(aln=R);  ISZERO >>>> (op EQ l r   ly le ce)             
    | TmNOT    e            ,_          ->  assert(aln=R);  ISZERO >>>> ( e >>(aln,ly,le,ce) )
    | TmLAND (l,r)          ,_          ->                  checked_codegen_LAnd l r ly le ce aln   
    | TmSend(cn,m,args,msg) ,_          ->  assert(aln=R);  codegen_send cn m args msg ly le ce 
    | TmNew(id,args,msg)    ,TyInstnc _ ->  assert(aln=R);  codegen_new id args msg ly le ce 
    | TmCall(id,args)       ,rety       ->                  codegen_predef_call aln ly le ce id args rety
    | EpSender              ,TyAddr     ->                  align_addr (CALLER  >>>>ce)  aln
    | EpThis                ,_          ->                  align_addr (ADDRESS >>>>ce) aln     
    | TmArray(id,idx)       ,TyMap _    ->  let ce      =   codegen_array id idx ly le ce           in  (*            S[keccak(a[i])] >> .. *)
                                            assert(aln=R);  salloc_array  id idx ly le ce               (*                     S[1]++ >> .. *)
    | TmArray(id,idx)       ,      _    ->  assert(aln=R);  codegen_array id idx ly le ce               (*               S[keccak(a)] >> .. *)
    | TmId id               ,TyMap(a,b) ->  let loc     =   lookup_le id le                         in 
                                            let ce      =   push_loc ce aln(TyMap(a,b))loc          in 
                                                            salloc_array_of_loc le ce loc          
    | TmId id               ,ty         ->  let loc     =   lookup_le id le                         in  
                                                            push_loc ce aln ty loc                      (*                        loc >> .. *)
    | TmDeref(ref,tyR)      ,ty         ->  assert(size_of_ty ty<=32 && tyR=TyRef ty && aln=R) ;                    (* assuming word-size *)
                                            let ce      =   (ref,tyR)               >>(R,ly,le,ce)   in  (* pushes the pointer *)
                                                            MLOAD                   >>>>ce 
    | e                                 ->  let _,ce    =   codegen_tm_eff aln ly le ce e      in 
                                                            PUSH1(Int 0)            >>>>ce 

and codegen_tm_eff aln ly le ce           = function 
    | TmAbort               ,TyErr      ->  le, throw ce                               
    | TmLog(id,args,Some ev),TyUnit     ->  codegen_log         ly le ce id args ev    
    | TmSfDstr tm           ,TyUnit     ->  codegen_selfDstrct  ly le ce tm          
    | TmAssign(l,r)         ,TyUnit     ->  codegen_assign      ly le ce l r        
    | TmReturn(ret,cont)    ,_          ->  codegen_return      ly le ce ret cont     
    | e                                 ->  pf "codegen_tm: %s " (str_of_tm e); raise Not_found
    
and op operator l r ly le ce =
    let ce    = r                                       >>(R,ly,le,ce)  in 
    let ce    = l                                       >>(R,ly,le,ce)  in 
                operator                                >>>>ce 
            
and push_msg_and_gas msg cn ly le ce = 
    let ce      = msg                                   >>(R,ly,le,ce)  in  (*                                            value >> .. *) 
    let ce      = cn                                    >>(R,ly,le,ce)  in  (*                                  cnAddr >> value >> .. *)
    let ce      = PUSH4(Int 3000)                       >>>>ce          in  (*                          3000 >> cnAddr >> value >> .. *)
    let ce      = GAS                                   >>>>ce          in  (*                   gas >> 3000 >> cnAddr >> value >> .. *)
                  SUB                                   >>>>ce              (*                      gas-3000 >> cnAddr >> value >> .. *)

and call_and_restore_PC ce =                                                (*  gas-3000 >> cnAddr >> msg >> &mhash >> argssize+4 >> retbegin >> retsize >> retbegin >> retsize >> PCbkp >> .. *)
    let ce      = CALL                                  >>>>ce          in  (*                                                                   success >> retbegin >> retsize >> PCbkp >> .. *)
    let ce      = PUSH1(Int 0)                          >>>>ce          in  (*                                                              0 >> success >> retbegin >> retsize >> PCbkp >> .. *)
    let ce      = JUMPI                                 >>>>ce          in  (*  IF success==0 THEN GOTO 0                                                   retbegin >> retsize >> PCbkp >> .. *)
    let ce      = SWAP2                                 >>>>ce          in  (*                                                                              PCbkp >> retsize >> retbegin >> .. *)
                  restore_PC                                ce              (*                                                                                       retsize >> retbegin >> .. *)

and mload_ret_value ce =                                                    (*                                 retsize >> retbegin >> .. *)
    let ce      = PUSH1 (Int 32)                        >>>>ce          in  (*                           32 >> retsize >> retbegin >> .. *)
    let ce      = throw_if_NEQ                              ce          in  (* IF 32!=retsize ERROR                       retbegin >> .. *)
                  MLOAD                                 >>>>ce              (*                                         M[retbegin] >> .. *)

and codegen_send cn m args msg ly le ce = match snd cn with
    | TyInstnc cnname  ->  (* msg-call to a cont        ract *) 
    let cnidx   = lookup_cnidx_of_ce ce cnname                          in 
    let callee  = lookup_cn ce cnidx                                    in
    let Some mname = m                                                  in 
    let m       = lookup_mthd_head ce callee mname                      in
    let TyMthd(id,_,reTy) = m                                           in 
    let retsize = size_of_ty reTy                                       in  (*                                                                                                              .. *)
    let ce      = Comment("BEGINE send to "^id)         >>>>ce          in  
    let ce      = reset_PC                                  ce          in  (*                                                                                                     PCbkp >> .. *)
    let ce      = PUSH1(Int retsize)                    >>>>ce          in  (*                                                                                          retsize >> PCbkp >> .. *)
    let ce      = DUP1                                  >>>>ce          in  (*                                                                               retsize >> retsize >> PCbkp >> .. *)
    let ce      = malloc                                    ce          in  (*                                                                              retbegin >> retsize >> PCbkp >> .. *)
    let ce      = repeat DUP2 2                             ce          in  (*                                                       retbegin >> retsize >> retbegin >> retsize >> PCbkp >> .. *)
    let ce      = mstore_mhash_and_args m args        ly le ce          in  (*                               &mhash >> argssize+4 >> retbegin >> retsize >> retbegin >> retsize >> PCbkp >> .. *)
    let ce      = push_msg_and_gas msg cn             ly le ce          in  (*  gas-3000 >> cnAddr >> msg >> &mhash >> argssize+4 >> retbegin >> retsize >> retbegin >> retsize >> PCbkp >> .. *)
    let ce      = call_and_restore_PC                       ce          in  (*                                                                                       retsize >> retbegin >> .. *)
    let ce      = mload_ret_value                           ce          in  (*                                                                                                       ret >> .. *)
                  Comment("END send to "^id)            >>>>ce 
    | TyAddr            ->  (* send value to an EOA *) 
    let retsize = 0                                                     in 
    let ce      = Comment "BEGINE send to Addr"         >>>>ce          in  (*                                                                   .. *) 
    let ce      = reset_PC                                  ce          in  (*                                                          PCbkp >> .. *) 
    let ce      = PUSH1(Int retsize)                    >>>>ce          in  (*                                                     0 >> PCbkp >> .. *) 
    let ce      = repeat DUP1 5                             ce          in  (*                            0 >> 0 >> 0 >> 0 >> 0 >> 0 >> PCbkp >> .. *) 
    let ce      = push_msg_and_gas msg cn             ly le ce          in  (* gas-3000 >> addr >> msg >> 0 >> 0 >> 0 >> 0 >> 0 >> 0 >> PCbkp >> .. *) 
    let ce      = call_and_restore_PC                       ce          in  (*                                                         0 >> 0 >> .. *)
    let ce      = POP                                   >>>>ce          in  (*                                                              0 >> .. *)
                  Comment("END send to Addr")           >>>>ce 
    | _             -> err "send tm with Wrong type"


and sstore_to_lval (TmArray(id,idx),_) ly le ce     =                       (*                                    rval >> .. *)
    let ce      = keccak_of_array id idx              ly le ce          in  (*                 KEC(aseed^aidx) >> rval >> .. *)
                  SSTORE                                >>>>ce              (* S[KEC(aseed^aidx)] := rval                 .. *)

and codegen_assign ly le ce l r     =
    let ce      = Comment "BEGIN Assignment"            >>>>ce          in 
    let ce      = r                                     >>(R,ly,le,ce)  in  (*                                    r >> .. *)
    let ce      = sstore_to_lval l                    ly le ce          in  (* S[KEC(l)] := r                          .. *)  
    let ce      = Comment "END Assignment"              >>>>ce          in 
    le,ce 

and checked_codegen_LAnd l r ly le ce aln = 
    assert(aln=R);         
    let la      =   fresh_label ()                                      in  (*                                                        .. *)
    let ce      =   l                                   >>(R,ly,le,ce)  in  (*                                                   l >> .. *)
    let ce      =   DUP1                                >>>>ce          in  (*                                              l >> l >> .. *)
    let ce      =   if_0_GOTO la                            ce          in  (*                                                   l >> .. *)
    let ce      =   POP                                 >>>>ce          in  (*                                                        .. *)
    let ce      =   r                                   >>(R,ly,le,ce)  in  (*                                                   r >> .. *)
    let ce      = JUMPDEST la                           >>>>ce          in  (*                                                   r >> .. *)
                    repeat ISZERO 2                       ce                (*                                                l&&r >> .. *)  

and mstore_mthd_args pck args ly le ce =
    let ce    = PUSH1(Int 0)                            >>>>ce          in  (*                                                   0 >> .. *)
                foldl (mstore_mthd_arg pck le ly) ce args                   (*                                             sumsize >> .. *) 

and mstore_mthd_arg pck le ly ce arg  =
    let ty      = get_ty arg                                            in  
    let i,a     = match pck with | ABIpk -> 32           ,R
                                 | Tight -> size_of_ty ty,L             in  (*                                                 sum >> .. *)
    let ce      = PUSH1 (Int i)                         >>>>ce          in  (*                                         size >> sum >> .. *)
    let ce      = arg                                   >>(a,ly,le,ce)  in  (*                                  arg >> size >> sum >> .. *)
    let ce      = DUP2                                  >>>>ce          in  (*                          size >> arg >> size >> sum >> .. *)
    let ce      = malloc                                    ce          in  (*                   alloc(size) >> arg >> size >> sum >> .. *)
    let ce      = MSTORE                                >>>>ce          in  (* M[alloc(size)] := arg                   size >> sum >> .. *)
                  ADD                                   >>>>ce              (*                                            size+sum >> .. *)

and mstore_mhash_and_args mthd args ly le ce =                              (*                                                        .. *)
    let ce      = mstore_mthd_hash mthd                     ce          in  (*                                         &mhash >> 4 >> .. *)
    let ce      = mstore_mthd_args ABIpk args ly         le ce          in  (*                              argsize >> &mhash >> 4 >> .. *)
    let ce      = SWAP1                                 >>>>ce          in  (*                              &mhash >> argsize >> 4 >> .. *)
    let ce      = SWAP2                                 >>>>ce          in  (*                              4 >> argsize >> &mhash >> .. *)
    let ce      = ADD                                   >>>>ce          in  (*                                 argsize+4 >> &mhash >> .. *)
                  SWAP1                                 >>>>ce              (*                                 &mhash >> argsize+4 >> .. *)

and codegen_mthd_argLen_chk m ce = match m with  
    | TyDefault     -> ce
    | TyMthd _      ->
    let ce      = PUSH4(Int(calldatasize m))            >>>>ce          in
    let ce      = CALLDATASIZE                          >>>>ce          in
                  throw_if_NEQ                              ce    

and escape_ARG arg retlabel ly le ce a = 
    let ce      = Comment "ESCAPE START"                >>>>ce          in 
    let ce      = PUSH32(Label retlabel)                >>>>ce          in     
    let ce      = arg                                   >>(a,ly,le,ce)  in 
    let ce      = ePUSH                                     ce          in
                  Comment "ESCAPE DONE"                 >>>>ce 

and codegen_app_idxrec aln ly le ce (TmApp((TmIRec(i),_),arg)) = 
    let ce      = Comment "BEGIN APP-REC"               >>>>ce          in
    let retaddr = fresh_label ()                                        in 
    let start   = lookup_recursion_param le                             in 
    let ce      = escape_ARG arg retaddr ly le ce aln                   in 
    let ce      = goto start                                ce          in 
    let ce      = JUMPDEST retaddr                      >>>>ce          in       
                  Comment "END APP-REC"                 >>>>ce 

and codegen_fix ly le ce (TmFix(phi,n,ty,tm)) = 
    let ce      = Comment "BEGIN FIX"                   >>>>ce          in 
    let start   = fresh_label ()                                        in 
    printf "! add_recursion_param(label%d)\n" start;     
    let le      = add_recursion_param le start                          in
    let ce      = JUMPDEST start                        >>>>ce          in 
    let ce      = tm                                    >>(R,ly,le,ce)  in  (*                               tm >> .. *)
    let ce      = ePOP                                      ce          in  (*                    retAddr >> tm >> .. *) 
    let ce      = JUMP                                  >>>>ce          in  (* GOTO retAddr                  tm >> .. *)
    let ce      = Comment "END FIX"                     >>>>ce          in 
    ce 

and codegen_idstrct ly le ce (TmIStrct(i)) = 
    let ce      = Comment "BEGIN Struct Parameter"      >>>>ce          in
    let ce      = get_escaped_arg                           ce          in 
                  Comment "END Struct Parameter"        >>>>ce

and codegen_app ly le ce (TmApp(t1,t2)) = match fst t1 with 
    | TmI(i,n) -> codegen_app ly le ce (TmApp(lookup_brjidx i le,t2))
    | TmIRec(i)-> codegen_app_idxrec R ly le ce (TmApp(t1,t2))
    | TmFix(f,n,ty,tm)-> 
    let ret     = fresh_label ()                                        in 
    let ce      = escape_ARG t2 ret ly le ce R                          in 
    let ce      = codegen_fix ly le ce (TmFix(f,n,ty,tm))               in
                  JUMPDEST ret                          >>>>ce 
    | _ -> 
    printf "! add_brjidx %s\n" (str_of_tm t2); 
    let le      = add_brjidx le t2                                      in       
                  t1                                    >>(R,ly,le,ce)  

and codegen_abs ly le ce (TmAbs(x,tyX,t)) = 
    let ce      = t                                     >>(R,ly,le,ce)  in
    ce 

and codegen_idx ly le ce (TmI(i,n))           = 
    let tm      = lookup_brjidx i le                                    in 
    ps"codegen_idx: ";pr_tm tm;
    let ce      = tm                                    >>(R,ly,le,ce)  in 
    ce 

and codegen_if ly le ce (TmIf(b,t1,t2)) = 
    let elif    = fresh_label()                                         in 
    let fi      = fresh_label()                                         in
    let ce      = Comment "IF"                          >>>>ce          in 
    let ce      = b                                     >>(R,ly,le,ce)  in 
    let ce      = if_0_GOTO elif                            ce          in 
    let ce      = Comment "THEN"                        >>>>ce          in 
    let ce      = t1                                    >>(R,ly,le,ce)  in
    let ce      = goto fi                                   ce          in 
    let ce      = Comment "ELSE"                        >>>>ce          in 
    let ce      = JUMPDEST elif                         >>>>ce          in 
    let ce      = t2                                    >>(R,ly,le,ce)  in 
    let ce      = Comment "FI"                          >>>>ce          in 
    let ce      = JUMPDEST fi                           >>>>ce          in 
    ce

and codegen_mthd ly cnidx (le,ce) (TmMthd(hd,bd))  =
    let ce      = Comment ("BEGIN " ^str_of_ty hd)      >>>>ce          in  
    let label   = fresh_label()                                         in 
    register_entry (Mthd(cnidx,hd)) label; 
    let le      = add_mthdCallerArgLocs(TmMthd(hd,bd))(add_empty_ctx (add_empty_brj le))    in
    let ce    = JUMPDEST label                          >>>>ce          in 
    let ce      = codegen_mthd_argLen_chk hd                ce          in
    let ce      = bd                                    >>(R,ly,le,ce)  in
    let ce      = Comment ("END "^str_of_ty hd)         >>>>ce          in  
    le,ce

and codegen_selfDstrct ly le ce tm =    
    let ce      = tm                                    >>(R,ly,le,ce)  in
    let ce      = SELFDESTRUCT                          >>>>ce          in
    le, ce

and mstore_tms ly le ce pack = function 
    | []        ->  le,         repeat (PUSH1(Int 0)) 2     ce    
    | e::es     ->  let le,ce = mstore_tm ly le ce pack e               in (*                                      alloc(size) >> size >> .. *)
                    let ce    = SWAP1                   >>>>ce          in (*                                      size >> alloc(size) >> .. *)
                    let le,ce = mstore_tms ly le ce pack es             in (*   0 >> 0 >> size' >> alloc(size') >> size >> alloc(size) >> .. *)
                    let ce    = POP                     >>>>ce          in (*        0 >> size' >> alloc(size') >> size >> alloc(size) >> .. *)       
                    let ce    = ADD                     >>>>ce          in (*             size' >> alloc(size') >> size >> alloc(size) >> .. *)    
                    let ce    = SWAP1                   >>>>ce          in (*             alloc(size') >> size' >> size >> alloc(size) >> .. *)     
                    le, ce   (* POP                                                           size' >> size >> alloc(size) >> .. *) 
                             (* ADD                                                             size+size'  >> alloc(size) >> .. *) 
                             (* SWAP1                                                           alloc(size) >> size+size'  >> .. *)
                    
and codegen_log      (ly:storLayout) le ce name args evnt =
    let visible, args= split_ev_args evnt args                          in
    let ce      = push_args le ly visible                   ce          in
    let ce      = push_evnt_hash  evnt                      ce          in
    let le,ce   = mstore_tms ly le ce ABIpk args                        in  (* stack : [..., size, offset] *)
    let n       = L.length visible + 1                                  in
    let ce      = log n                                 >>>>ce          in  (* deindexee N in logN *)
    le, ce

(***************************************)
(***     5. CODEGEN RETURN          ***)
(***************************************)

and push_args le (ly:storLayout)    = foldr (fun arg ce -> arg >> (R,ly,le,ce))  
and sstore_words_to stor_locs ce    = foldl sstore_word_to ce stor_locs
and sstore_word_to ce stor_loc      =
    let ce      =   PUSH32 (Int stor_loc)               >>>>ce          in
                    SSTORE                              >>>>ce       

and sstore_vars (ly:storLayout) le ce offset idx vars = 
    let cntrct  =   lookup_cn ce idx                                    in 
    let varlocs =   fieldVar_locations offset cntrct                    in
    assert(L.length varlocs=L.length vars) ; 
    let ce      =   push_args le ly vars ce                             in  (*                                         argk >> .. >> arg1 >> .. *)
    let ce      =   sstore_words_to varlocs ce                          in  (*  S[l_k]:=argk; .. ; S[l_1]:=arg1                              .. *)
    le,ce

and cont_call ly le ce (TmCall(cn,args),_) = 
    let idx     =   lookup_cnidx_of_ce ce cn                            in 
    let offset  =   (ly.fieldVars idx).offst                            in  
    let ce      =   Comment "Setting Cont"              >>>>ce          in 
    let ce      =   set_PC ce idx                                       in  (*  S[PC] := rntime_offset_of_cn                                 .. *) 
                    sstore_vars ly le ce offset idx args                    (*  S[l_k]:= argk; .. ; S[l_1]:= arg1                            .. *)

(*       mstore_word ty 
 *
 *     BEFORE           AFTER               
 *
 *                   +---------+            
 *                   |alloc(32)|            
 *   +---------+     +---------+            
 *   |  value  |     |    32   |            
 * --+---------+-- --+---------+--    *)

and mstore_word ty ce = assert (size_of_ty ty <= 32)  ;                 (*                                   val >> .. *)   (* Here, FUN TYPE is excluded <- Problem #TODO *) 
    let ce      = PUSH1(Int 32)                         >>>>ce          in  (*                             32 >> val >> .. *)
    let ce      = DUP1                                  >>>>ce          in  (*                       32 >> 32 >> val >> .. *)
    let ce      = malloc                                    ce          in  (*                alloc(32) >> 32 >> val >> .. *)
    let ce      = SWAP2                                 >>>>ce          in  (*                val >> 32 >> alloc(32) >> .. *)
    let ce      = DUP3                                  >>>>ce          in  (*   alloc(32) >> val >> 32 >> alloc(32) >> .. *)
    let ce      = MSTORE                                >>>>ce          in  (* M[alloc(32)]:=val     32 >> alloc(32) >> .. *)
                  SWAP1                                 >>>>ce              (*                       alloc(32) >> 32 >> .. *)

and mstore_tm ly le ce pack (e,ty) =
    let a       = match pack with | ABIpk   -> R
                                  | Tight   -> L                        in
    let ce      = (e,ty)                                >>(a,ly,le,ce)  in  (*                                    e >> .. *)
    let ce      = mstore_word ty ce                                     in  (* M[alloc(32)]:=e      alloc(32) >> 32 >> .. *)
    le,ce

and codegen_return ly le ce ret cont =
    let ce          =   Comment "BEGIN RETURN"          >>>>ce          in 
    let le,ce       =   cont_call ly le ce cont                         in
    let ce          =   match ret with
    | (TmUnit,_)    ->  Comment"END RETURN" >>>> (STOP  >>>>ce)       
    | e             ->  
    let le,ce       =   mstore_tm ly le ce ABIpk e                      in
                        RETURN                          >>>>ce          in 
    let ce          =   Comment "END RETURN"            >>>>ce          in 
    le,ce


(********************************************)
(***     6. CODEGEN CONTRACT             ***)
(********************************************)

let codegen_mthds ly    = ($$$) foldl codegen_mthd ly

let codegen_cntrct ly le ce (idx,TmCn(id,flds,mthds)) =
    let label = fresh_label()                                       in 
    register_entry (Cntrct idx) label ;                    
    let ce    = JUMPDEST label                      >>>>ce          in     
    let ce      = init_malloc                           ce          in  (* M[0x40]:=0x60                                  *)                                  
    let le,ce   = dispatcher le ce idx (TmCn(id,flds,mthds))        in  (*                                                *)
    let le,ce   = codegen_mthds ly idx (le,ce) mthds                in  
    ce

(********************************************)
(***     7. MAKE BYTECODE                ***)
(********************************************)

let append_rntime ly rc (idx,cn)   =
    let _ = pe("compiling contract" ^ str_of_int idx) in   
    { rntime_ce             = codegen_cntrct ly(rntime_init_le cn)rc.rntime_ce(idx,cn)
    ; rntime_cn_offsets     = insert idx(code_len rc.rntime_ce)rc.rntime_cn_offsets    }

let compile_rntime ly cns          = 
    let init_rc = init_rntimeCode (lookup_cnidx_of_cns cns) cns                     in 
    foldl (append_rntime ly) init_rc cns

let cnstrInfo_of_cnstrCode cc       = cnstrInfo_of_cn cc.cnstr_cn (program_of_cc cc)

let sizes_of_cnstrCodes ccs         =
    let lengths = map (code_len $ ce_of_cc) ccs                                     in
    let lengths = idx_sort lengths                                                  in
    L.map snd lengths

let offsets_of_sizes init l         =
    let rec loop offsets current        = function 
        | []            -> L.rev offsets
        | size::rest    -> loop(current::offsets)(current+size)rest                 in 
    loop [] init l 

let rntimeInfo_of_rntimeCode rc ccs : rntimeInfo =
    let ccs_sizes           = sizes_of_cnstrCodes ccs                               in
    let ccs_offsets         = offsets_of_sizes (code_len rc.rntime_ce) ccs_sizes    in
    let ccs_totalsize       = BL.sum ccs_sizes                                      in
    { rntimeCodeSize        = ccs_totalsize + code_len rc.rntime_ce
    ; rntimeCnstrSizes      = to_idxlist ccs_sizes
    ; rntimeCnOffsts        = rc.rntime_cn_offsets
    ; rntimeCnstrOffsts     = to_idxlist ccs_offsets }

(*  Since the code is stored in the reverse order, the concatenation is also reversed. *)
let concat_programs_rev programs    =
    let rev_programs        = L.rev programs                                        in
    L.concat rev_programs

let program_of_cnstrs ccs           =
    let programs            = map program_of_cc ccs                                 in
    let programs            = idx_sort  programs                                    in
    let programs            = L.map snd programs                                    in
    concat_programs_rev programs

let compose_bytecode ccs rc idx : big_int Evm.program =
    let cnstrInfos          = map cnstrInfo_of_cnstrCode  ccs                       in
    let rntimeInfo          = rntimeInfo_of_rntimeCode rc ccs                       in
    let cnly                = cnstrct_cnLayout cnstrInfos rntimeInfo                in
    let cnstrCode           = lookup idx ccs                                        in
    let imm_cnstr           = realize_program cnly idx(program_of_cc cnstrCode)     in
    let cns_program         = program_of_cnstrs ccs                                 in 
    let rn_program          = extract_program rc.rntime_ce                          in
    let imm_rntime          = realize_program cnly idx(cns_program@rn_program) in
    imm_rntime @ imm_cnstr   (* the code is stored in the reverse order *)
