
(* M[0x40]  :=   the address of mem alloc     *) 
(* MSTORE   :=   x=pop() ; y=pop() ; M[x]=y   *) 
(* MLOAD    :=   x=pop() ; push M[x]          *) 
(* CODECOPY  to from len :=  M[to .. to+len-1]=I_b[from .. from+len-1]  *)

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

let align_addr aln ce       = match aln with 
    | R                     ->  ce
    | L                     ->  shiftLtop ce (12 * 8)
let align_to_L ce ty        = function 
    | R                     ->  ce
    | L                     ->  let size = size_of_ty ty in
                                assert (size <= 32) ;
                                shiftLtop ce ((32-size)*8) 


(*****************************************)
(***  1. CONTRACT CREATION CODE        ***)
(*****************************************)

(**   1.1  Var Fileds Setup        **) 
let mstore_fieldVars cn ce =                                   (* This copies fieldVars at the end of the bytecode into MEM.             *) 
    let size    = size_of_vars_in_cn cn                         in  (* M[0x40](==M[64]) is increased accordingly                              *)
    let ce      = PUSH32(Int size)                  =>> ce      in  (*                                                             size >> .. *)
    let ce      = DUP1                              =>> ce      in  (*                                                     size >> size >> .. *)
    let ce      = malloc                                ce      in  (*                                              alloc(size) >> size >> .. *)
    let ce      = DUP2                              =>> ce      in  (*                                      size >> alloc(size) >> size >> .. *)
    let ce      = DUP1                              =>> ce      in  (*                              size >> size >> alloc(size) >> size >> .. *)
    let ce      = CODESIZE                          =>> ce      in  (*                  codesize >> size >> size >> alloc(size) >> size >> .. *)
    let ce      = SUB                               =>> ce      in  (*                     codesize-size >> size >> alloc(size) >> size >> .. *)
    let ce      = DUP3                              =>> ce      in  (*      alloc(size) >> codesize-size >> size >> alloc(size) >> size >> .. *)
                  CODECOPY                          =>> ce          (*          to             from                 alloc(size) >> size >> .. *)
                                                                    (*                                               codebegin                *)
let check_codesize cnidx ce     =  
    let ce      = PUSH32(InitDataSize cnidx)        =>> ce      in  (*                                    datasize >> mem_start >> size >> .. *)
    let ce      = CODESIZE                          =>> ce      in  (*                        codesize >> datasize >> mem_start >> size >> .. *) 
                  throw_if_NEQ                          ce        (* IF not eq THEN error *) 

let sstore_fieldVars cnidx ce   =                               
    let label   = fresh_label()                                 in
    let exit    = fresh_label()                                 in  (*                                                mem_start >> size >> .. *)  
    let ce      = check_codesize cnidx                  ce      in 
    let ce      = PUSH32(StorFieldsBegin cnidx)     =>> ce      in  (*                                          idx >>   mem_start    >>   size    >> .. *)
    let ce   = JUMPDEST label                       =>> ce      in  (*                                          idx >>   mem_start    >>   size    >> .. *)
    let ce      = DUP3                              =>> ce      in  (*                                 size >>  idx >>   mem_start    >>   size    >> .. *)
    let ce      = if_0_GOTO exit                        ce      in  (* IF size==0 THEN GOTO exit                idx >>   mem_start    >>   size    >> .. *)   
    let ce      = DUP2                              =>> ce      in  (*                            mem_start >>  idx >>   mem_start    >>   size    >> .. *) 
    let ce      = MLOAD                             =>> ce      in  (*                         M[mem_start] >>  idx >>   mem_start    >>   size    >> .. *)
    let ce      = DUP2                              =>> ce      in  (*                  idx >> M[mem_start] >>  idx >>   mem_start    >>   size    >> .. *)
    let ce      = SSTORE                            =>> ce      in  (* S[idx]=M[mem_start]                      idx >>   mem_start    >>   size    >> .. *)  
    let ce      = PUSH1(Int 0x20)                   =>> ce      in  (*                                 0x20 >>  idx >>   mem_start    >>   size    >> .. *)
    let ce      = SWAP1                             =>> ce      in  (*                                  idx >> 0x20 >>   mem_start    >>   size    >> .. *)
    let ce      = SWAP3                             =>> ce      in  (*                                 size >> 0x20 >>   mem_start    >>   idx     >> .. *)
    let ce      = SUB                               =>> ce      in  (*                                   size- 0x20 >>   mem_start    >>   idx     >> .. *)
    let ce      = SWAP2                             =>> ce      in  (*                                          idx >>   mem_start    >> size-0x20 >> .. *) 
    let ce      = incr_top 1(*word*)                    ce      in  (*                                        idx+1 >>   mem_start    >> size-0x20 >> .. *)
    let ce      = SWAP1                             =>> ce      in  (*                                    mem_start >>       idx+1    >> size-0x20 >> .. *)
    let ce      = incr_top 0x20                         ce      in  (*                               mem_start+0x20 >>       idx+1    >> size-0x20 >> .. *)
    let ce      = SWAP1                             =>> ce      in  (*                                        idx+1 >> mem_start+0x20 >> size-0x20 >> .. *)
    let ce      = goto label                            ce      in  (*                                        idx+1 >> mem_start+0x20 >> size-0x20 >> .. *)
    let ce   = JUMPDEST exit                        =>> ce      in  (*                                          idx >>   mem_start    >>   size    >> .. *)
                  repeat POP 3                          ce          (*                                                                     .. *)
(*  S[1]   := m   <- array seed                                               *)  
(*  S[2]   := the value of arg1 is stored in message call                     *)  
(*            ..                                                              *)  
(*  S[k+1] := the value of argk is stored in message call                     *)  



(**   1.2. Array Fields Setup    **)                   
let reset_salloc_array ce = 
    let ce      = PUSH1 (Int 1)                     =>> ce          in  (*                                           1 >> .. *)
    let ce      = DUP1                              =>> ce          in  (*                                      1 >> 1 >> .. *)
                  SSTORE                            =>> ce              (* S[1]:=1                                        .. *) 

let salloc_fieldArr ce (arrArgLoc:int) =   
    let label   = fresh_label()                                     in  (*                                                .. *) 
    let ce      = PUSH4 (Int arrArgLoc)             =>> ce          in  (*                                        seed >> .. *)
    let ce      = SLOAD                             =>> ce          in  (*                                     S[seed] >> .. *) 
    let ce      = PUSH4 (Label label)               =>> ce          in  (*                            label >> S[seed] >> .. *)
    let ce      = JUMPI                             =>> ce          in  (* IF S[seed]!=0 GOTO label                       .. *) 
    let ce      = sincr 1                               ce          in  (*                                      S[1]++ >> .. *)      
    let ce      = PUSH4 (Int arrArgLoc)             =>> ce          in  (*                                seed >> S[1] >> .. *)
    let ce      = SSTORE                            =>> ce          in  (* S[seed]:=S[1]                                  .. *)
                  JUMPDEST label                    =>> ce              (*                                                .. *)

let init_salloc_fieldArr_if_not ce = 
    let label   = fresh_label ()                                    in  (*                                                   *) 
    let ce      = PUSH1 (Int 1)                     =>> ce          in  (*                                                   *) 
    let ce      = SLOAD                             =>> ce          in  (*                                                   *)
    let ce      = PUSH4 (Label label)               =>> ce          in  (*                                                   *) 
    let ce      = JUMPI                             =>> ce          in  (* IF S[1]!=0 then GOTO label                  >> .. *)
    let ce      = reset_salloc_array                    ce          in  (*                                      1 >> 1 >> .. *) 
                  JUMPDEST label                    =>> ce          

let setup_fieldArrs cn ce =
    let ce      = init_salloc_fieldArr_if_not           ce          in   
    let arrLocs = fieldArr_locations cn                             in  
                  foldl salloc_fieldArr ce arrLocs 

(**   1.3. Runtime CODE COPY         **) 
let mstore_rntimeCode idx ce =                                          (*                                                           .. *)
    let ce      = PUSH32(RntimeCodeSize)            =>> ce          in  (*                                                   size >> .. *)
    let ce      = DUP1                              =>> ce          in  (*                                           size >> size >> .. *)  
    let ce      = malloc                                ce          in  (*                                    alloc(size) >> size >> .. *)
    let ce      = DUP2                              =>> ce          in  (*                            size >> alloc(size) >> size >> .. *)
    let ce      = PUSH32(RntimeCodeOffset idx)      =>> ce          in  (*                     idx >> size >> alloc(size) >> size >> .. *)
    let ce      = DUP3                              =>> ce          in  (*      alloc(size) >> idx >> size >> alloc(size) >> size >> .. *)
                  CODECOPY                          =>> ce              (*                                    alloc(size) >> size >> .. *)
                                                                        (*                                     codebegin                *)
(**   1.4.  CONTRACT CREATION   *****)
type creation           =   { cr_ce           : ce
                            ; cr_ty           : ty
                            ; cr_cn           : ty toplevel } 

let ce_of_cr cr         =   cr.cr_ce
let program_of_cr       =   extract_program $ ce_of_cr  
let program_of_crs      =   L.concat $ L.rev $ L.map snd $ idx_sort $ map program_of_cr 


let codegen_creation cns idx = (* return ce which contains the program *) 
    let TmCn(id,_,_) as cn = lookup idx cns in 
    let ce      =   empty_ce (lookup_cnidx cns) cns            in  (*                                                                                 *)
    let ce      =   Comment("Begin Cnstrctr of Cntract "^id)=>> ce          in 
    let ce      =   init_malloc                                 ce          in  (* M[64] := 96                                                                     *)
    let ce      =   mstore_fieldVars  cn                        ce          in  (*                                            alloc(argssize) << argssize << ..    *)
    let ce      =   sstore_fieldVars  idx                       ce          in  (* S[i..i+sz-1]:= argCodes               i << alloc(argssize) << argssize << ..    *)
    let ce      =   setup_fieldArrs   cn                        ce          in  (* S[1]        := #array                 i << alloc(argssize) << argssize << ..    *)
    let ce      =   set_PC            idx                       ce          in  (* S[PC]       := rntime_cn_offst (returned body)                                  *)
    let ce      =   mstore_rntimeCode idx                       ce          in  (*                                      alloc(codesize) << codesize << i <<  ..    *)
    let ce      =   RETURN                                  =>> ce          in  (* OUTPUT(M[code]) as The BODY code                                    i <<  ..    *)
                    Comment("End Cnstrctr of Cntract "^id)  =>> ce 

let compile_creation cns idx  : creation =
    let cn      =   L.assoc idx cns in 
    { cr_ce           = codegen_creation cns idx
    ; cr_ty           = typeof_cn cn 
    ; cr_cn           = cn                                }

let compile_creations cns : creation idxlist =
    idxmap (compile_creation cns) cns


(***************************************)
(***     2.    RUNTIME               ***)
(***************************************)

type rntimeCode             =   { rntime_ce             : ce                                                       
                                ; rntime_cn_offsets     : int idxlist  }

let empty_rntimeCode lookup_cn layouts =
    { rntime_ce             = empty_ce lookup_cn layouts
    ; rntime_cn_offsets     = []                                    }

let init_rntimeCode lookup_cn layouts : rntimeCode =
    let ce      =   empty_ce lookup_cn layouts                          in
    let ce      =   error_loop ce                                       in 
    let ce      =   get_PC                                      ce      in
    let ce      =   JUMP                                    =>> ce      in
    { rntime_ce             = ce
    ; rntime_cn_offsets     = [] }

(***************************************)
(***     3.  DISPATHER               ***)
(***************************************)

let dispatch_method idx le ce m =                                           (*                                           ABCD >> .. *)  
    let ce      =   DUP1                                    =>> ce      in  (*                                   ABCD >> ABCD >> .. *)
    let ce      =   push_mthd_hash m                            ce      in  (*                              m >> ABCD >> ABCD >> .. *)
    let ce      =   EQ                                      =>> ce      in  (*                             m=ABCD?1:0 >> ABCD >> .. *)
    let ce      =   PUSH32(RntimeMthdLabel(idx,m))          =>> ce      in  (*                Rntime(m) >> m=ABCD?1:0 >> ABCD >> .. *)
                    JUMPI                                   =>> ce          (* if m=ABCD then GOTO Rntime(m)             ABCD >> .. *)

let dispatch_default idx le ce =
    let ce      =   PUSH32(RntimeMthdLabel(idx,TyDefault))  =>> ce      in
                    JUMP                                    =>> ce     

let push_inputdata32_from databegin ce =
    let ce      =   PUSH32 databegin                        =>> ce      in
                    CALLDATALOAD                            =>> ce

let dispatcher idx (TmCn(_,_,mthds)) le ce = 
    let tyMthds =   L.map(function TmMthd(head,_) -> head) mthds        in
    let uMthds  =   filter_method tyMthds                               in 
    let ce      =   Comment "BEGIN Method Dispatchers "     =>> ce      in 
    let ce      =   push_inputdata32_from(Int 0)                ce      in  (*               ABCDxxxxxxxxxxxxxxxxxxxxxxxxxxxxx >> .. *)
    let ce      =   shiftRtop ce Crpt.(word_bits-sig_bits)              in  (*                                            ABCD >> .. *)                             
    let ce      =   foldl(dispatch_method idx le)ce uMthds              in  (* JUMP to Method ABCD                                   *)   
    let ce      =   POP                                     =>> ce      in  (*                                                    .. *)
    let ce      =   if  default_exists tyMthds
                        then dispatch_default idx            le ce          (* JUMP to Default Method                             .. *) 
                        else throw ce                                   in  (* JUMP to error                                      .. *) 
    let ce      =   Comment "END Method Dispatchers "       =>> ce      in 
    le,ce

(*********************************************)
(***     4.    CODEGEN  PRECONTRACT        ***)
(*********************************************)

let rec codegen_pre_call id args rety aln ly le ce = match id with 
    | "pre_ecdsarecover"    ->  assert(aln=R);      codegen_ECDSArecover args   ly le ce     
    | "keccak256"           ->  assert(aln=R);      codegen_keccak256 args      ly le ce       
    | "iszero"              ->  assert(rety=TyBool);codegen_iszero args aln     ly le ce 
    | _                     ->  err "codegen_predef_call: Direct Contract Call is Not supported. Specify a Method."

and codegen_iszero [arg] aln ly le ce =
    let ce    = arg                    >>(aln,ly,le,ce)     in
                ISZERO                          =>> ce 
       
and codegen_keccak256 args   ly le ce =
    let ce    = get_malloc ce                               in  
    let ce    = mstore_mthd_args L args       ly le ce      in  
    let ce    = SWAP1                           =>> ce      in  
                SHA3                            =>> ce                

and codegen_ECDSArecover args ly le ce = match args with [h;v;r;s] ->  
    let ce    = PUSH1 (Int 32)                  =>> ce      in  
    let ce    = DUP1                            =>> ce      in  
    let ce    = malloc                              ce      in  
    let ce    = repeat DUP2 2                       ce      in  
    let ce    = get_malloc                          ce      in
    let ce    = mstore_mthd_args R args       ly le ce      in  
    let ce    = SWAP1                           =>> ce      in  
    let ce    = PUSH1 (Int 0)                   =>> ce      in  
    let ce    = PUSH1 (Int 1)                   =>> ce      in  
    let ce    = PUSH4 (Int 10000)               =>> ce      in  
    let ce    = CALL                            =>> ce      in  
    let ce    = throw_if_0 ce                               in
    let ce    = POP                             =>> ce      in  
    let ce    = SWAP1                           =>> ce      in  
    let ce    = POP                             =>> ce      in  
                MLOAD                           =>> ce                    (* stack: [output] *)
    | _         -> err "pre_ecdsarecover has a wrong number of args"

(*********************************************)
(***    5.    CODEGEN  TERM                ***)
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
    let cnidx   =   lookup_cnidx_at_ce id                   ce      in 
    let ce      =   PUSH32(CnstrCodeSize cnidx)         =>> ce      in  (*                                                        size >> .. *) 
    let ce      =   PUSH32(RntimeCnstrOffset cnidx)     =>> ce      in  (*                                             cn_idx  >> size >> .. *)
    let ce      =   mstore_code                             ce      in  (*                                         alloc(size) >> size >> .. *)
    let ce      =   SWAP1                               =>> ce      in  (*                                         size >> alloc(size) >> .. *)
    let ce      =   mstore_whole_code                       ce      in  (*                alloc(wsize) >> wsize >> size >> alloc(size) >> .. *)
    let ce      =   mstore_mthd_args R args           ly le ce      in  (*    argssize >> alloc(wsize) >> wsize >> size >> alloc(size) >> .. *)
    let ce      =   SWAP1                               =>> ce      in  (*    alloc(wsize) >> argssize >> wsize >> size >> alloc(size) >> .. *)
    let ce      =   POP                                 =>> ce      in  (*                    argssize >> wsize >> size >> alloc(size) >> .. *)
    let ce      =   ADD                                 =>> ce      in  (*                       argssize+wsize >> size >> alloc(size) >> .. *)
    let ce      =   ADD                                 =>> ce      in  (*                          argssize+wsize+size >> alloc(size) >> .. *)
                    SWAP1                               =>> ce          (*                                  alloc(size) >>   totalsize >> .. *)

and codegen_new id args msg ly le ce   =    
    let ce      =   reset_PC ce                                     in  (*                                             PCbkp >> .. *)
    let ce      =   mstore_new_instance id args msg   ly le ce      in  (*                      alloc(size) >> size >> PCbkp >> .. *)
    let ce      =   msg                          >>(R,ly,le,ce)     in  (*             value >> alloc(size) >> size >> PCbkp >> .. *)
    let ce      =   CREATE                              =>> ce      in  (*                             createResult >> PCbkp >> .. *)
    let ce      =   throw_if_0                              ce      in  (*                             createResult >> PCbkp >> .. *)
    let ce      =   SWAP1                               =>> ce      in  (*                             PCbkp >> CreateResult >> .. *)
                    restore_PC                              ce          (*                                      CreateResult >> .. *)

and codegen_array aid aidx ly le ce    =                                (*                                                      .. *)
    let ce      =   keccak_of_array aid aidx          ly le ce      in  (*                                      keccak(a[i]) >> .. *)
                    SLOAD                               =>> ce          (*                                   S[keccak(a[i])] >> .. *) 

and keccak_of_array aid aidx ly le ce  =                                (* S[keccak(a[i])] := the seed of array *) 
    let ce      =   aidx                         >>(R,ly,le,ce)     in  (*                                             index >> .. *)    
    let ce      =   aid                          >>(R,ly,le,ce)     in  (*                                array_loc >> index >> .. *)
                    keccak_cat                              ce          (*                            sha3(array_loc++index) >> .. *) 
      
and salloc_array aid aidx ly le ce     =
    let push ce =   keccak_of_array aid aidx          ly le ce      in  (*                                        S[KEC(a_i)] >> .. *)
                    salloc_array_of_push push               ce          (* S[KEC(a_i)]:= newSeed                      newSeed >> .. *)     
                                                                      
and salloc_array_of_loc le ce(Stor data) =       
    assert(data.size=Int 1) ;                                           (*                                        S[KEC(a_i)] >> .. *)     
    let push ce =   PUSH32 data.offst                   =>> ce      in  
                    salloc_array_of_push push               ce          (* S[KEC(a_i)]:= newSeed                      newSeed >> .. *)     

and salloc_array_of_push push_array_seed ce =   
    let label   =   fresh_label ()                                  in  (*                                        S[KEC(a_i)] >> .. *) 
    let ce      =   DUP1                                =>> ce      in  (*                          S[KEC(a_i)] >> S[KEC(a_i)] >> .. *) 
    let ce      =   PUSH4(Label label)                  =>> ce      in  (*                 label >> S[KEC(a_i)] >> S[KEC(a_i)] >> .. *) 
    let ce      =   JUMPI                               =>> ce      in  (* {IF S[KEC(a_i)]!=0 GOTO label}          S[KEC(a_i)] >> .. *) 
    let ce      =   POP                                 =>> ce      in  (*                                                      .. *) 
    let ce      =   push_array_seed                         ce      in  
    let ce      =   sincr 1                                 ce      in  (*                                 S[1]++ >> KEC(a_i) >> .. *) 
    let ce      =   DUP1                                =>> ce      in  (*                     newseed >> newseed >> KEC(a_i) >> .. *) 
    let ce      =   SWAP2                               =>> ce      in  (*                     KEC(a_i) >> newseed >> newseed >> .. *) 
    let ce      =   SSTORE                              =>> ce      in  (* S[KEC(a_i)]:= newseed                      newseed >> .. *)
                    JUMPDEST label                      =>> ce          (*                                                         *)

(* le is not updated here.  
 * le can only be updated in a variable initialization *)
and (>>) e (aln,ly,le,ce)           = codegen_tm ly le ce aln e 
and codegen_tm ly le ce aln e       = pe_tm e; pe(str_of_ctx le); match e with 
    | TmApp(t1,t2)          ,_              ->                  codegen_app     (TmApp(t1,t2))            ly le ce 
    | TmAbs(x,tyX,t)        ,_              ->                  codegen_abs     (TmAbs(x,tyX,t))          ly le ce 
    | TmFix(f,n,ty,t)       ,_              ->                  codegen_fix     (TmFix(f,n,ty,t))         ly le ce 
    | TmI(i,n)              ,_              ->                  codegen_idx     (TmI(i,n))                ly le ce 
    | TmIStrct(i)           ,_              ->                  codegen_istrct  (TmIStrct(i))             ly le ce 
    | TmIf(b,t1,t2)         ,_              ->                  codegen_if      (TmIf(b,t1,t2))           ly le ce 
    | Balanc   e            ,_              ->                  BALANCE     =>>     (    e           >>(R,ly,le,ce))
    | EpValue               ,_              ->                  CALLVALUE                                   =>> ce      (* Value (wei) Transferred to the account *) 
    | TmZero                ,_              ->                  PUSH1(Int 0)                                =>> ce 
    | EpNow                 ,_              ->                  TIMESTAMP                                   =>> ce 
    | TmFalse               ,_              ->  assert(aln=R);  PUSH1(Int 0)                                =>> ce  
    | TmTrue                ,_              ->  assert(aln=R);  PUSH1(Int 1)                                =>> ce 
    | TmU256   d            ,_              ->  assert(aln=R);  PUSH32(Big d)                               =>> ce  
    | TmU8     d            ,_              ->  assert(aln=R);  PUSH1(Big d)                                =>> ce  
    | TmAdd  (l,r)          ,_              ->                  codegen_op ADD l r                        ly le ce             
    | TmSub  (l,r)          ,_              ->                  codegen_op SUB l r                        ly le ce             
    | TmMul  (l,r)          ,_              ->                  codegen_op MUL l r                        ly le ce             
    | TmLT   (l,r)          ,_              ->  assert(aln=R);  codegen_op LT  l r                        ly le ce           
    | TmGT   (l,r)          ,_              ->  assert(aln=R);  codegen_op GT  l r                        ly le ce           
    | TmEQ   (l,r)          ,_              ->  assert(aln=R);  codegen_op EQ  l r                        ly le ce           
    | TmNEQ  (l,r)          ,_              ->  assert(aln=R);  ISZERO  =>>     codegen_op EQ l r         ly le ce
    | TmNOT    e            ,_              ->  assert(aln=R);  ISZERO  =>>          (    e        >>(aln,ly,le,ce)) 
    | TmLAND (l,r)          ,_              ->                  checked_codegen_LAnd l r aln              ly le ce 
    | TmCall(id,args)       ,rety           ->                  codegen_pre_call id args rety aln         ly le ce
    | TmSend((e,TyAddr),m,args,msg)     ,_  ->  assert(aln=R);  codegen_send_eoa (e,TyAddr)         msg   ly le ce 
    | TmSend((c,TyInstnc n),m,args,msg) ,_  ->  assert(aln=R);  codegen_send_cn(c,TyInstnc n)m args msg   ly le ce 
    | TmNew(id,args,msg)    ,TyInstnc _     ->  assert(aln=R);  codegen_new    id args msg                ly le ce 
    | EpAddr(c,TyInstnc i)  ,TyAddr         ->                  (c,TyInstnc i)                     >>(aln,ly,le,ce) 
    | EpSender              ,TyAddr         ->                  align_addr aln             (CALLER          =>> ce) 
    | EpThis                ,_              ->                  align_addr aln             (ADDRESS         =>> ce) 
    | TmArray(id,idx)       ,TyMap _        ->  let ce      =   codegen_array id idx                      ly le ce      in  (*            S[keccak(a[i])] >> .. *)
                                                assert(aln=R);  salloc_array  id idx                      ly le ce          (*                     S[1]++ >> .. *)
    | TmArray(id,idx)       ,      _        ->  assert(aln=R);  codegen_array id idx                      ly le ce          (*               S[keccak(a)] >> .. *)
    | TmId id               ,TyMap(a,b)     ->  let loc     =   lookup_le id le                                         in 
                                                let ce      =   push_loc ce aln(TyMap(a,b))loc                          in 
                                                                salloc_array_of_loc le ce loc                  
    | TmId id               ,ty             ->  let loc     =   lookup_le id le                                         in  
                                                                push_loc ce aln ty loc                                      (*                        loc >> .. *)
    | TmDeref(ref,tyR)      ,ty             ->  assert(size_of_ty ty<=32 && tyR=TyRef ty && aln=R) ;                        (* assuming word-size *)
                                                let ce      =   (ref,tyR)                            >>(R,ly,le,ce)     in  (* pushes the pointer *)
                                                                MLOAD                                       =>> ce 
    | e                                     ->  let _,ce    =   codegen_tm_eff e              aln         ly le ce      in 
                                                                PUSH1(Int 0)                                =>> ce 

and codegen_tm_eff tm aln ly le ce      =   match tm with 
    | TmAbort               ,TyErr          ->  le, throw ce                               
    | TmLog(id,args,Some ev),TyUnit         ->  codegen_log id args ev  ly le ce     
    | TmSfDstr tm           ,TyUnit         ->  codegen_selfdstr  tm    ly le ce 
    | TmAssign(l,r)         ,TyUnit         ->  codegen_assign l r      ly le ce  
    | TmReturn(ret,cont)    ,_              ->  codegen_return ret cont ly le ce    
    | e                                     ->  pf "codegen_tm: %s " (str_of_tm e); raise Not_found
    
and codegen_op operator l r ly le ce =
    let ce    = r                                       >>(R,ly,le,ce)  in 
    let ce    = l                                       >>(R,ly,le,ce)  in 
                operator                                =>> ce 
            
and push_msg_and_gas msg cn ly le ce = 
    let ce      = msg                                   >>(R,ly,le,ce)  in  (*                                            value >> .. *) 
    let ce      = cn                                    >>(R,ly,le,ce)  in  (*                                  cnAddr >> value >> .. *)
    let ce      = PUSH4(Int 3000)                       =>> ce          in  (*                          3000 >> cnAddr >> value >> .. *)
    let ce      = GAS                                   =>> ce          in  (*                   gas >> 3000 >> cnAddr >> value >> .. *)
                  SUB                                   =>> ce              (*                      gas-3000 >> cnAddr >> value >> .. *)

and call_and_restore_PC ce =                                                (*  gas-3000 >> cnAddr >> msg >> &mhash >> argssize+4 >> retbegin >> retsize >> retbegin >> retsize >> PCbkp >> .. *)
    let ce      = CALL                                  =>> ce          in  (*                                                                   success >> retbegin >> retsize >> PCbkp >> .. *)
    let ce      = PUSH1(Int 0)                          =>> ce          in  (*                                                              0 >> success >> retbegin >> retsize >> PCbkp >> .. *)
    let ce      = JUMPI                                 =>> ce          in  (*  IF success==0 THEN GOTO 0                                                   retbegin >> retsize >> PCbkp >> .. *)
    let ce      = SWAP2                                 =>> ce          in  (*                                                                              PCbkp >> retsize >> retbegin >> .. *)
                  restore_PC                                ce              (*                                                                                       retsize >> retbegin >> .. *)

and mload_ret_value ce =                                                    (*                                 retsize >> retbegin >> .. *)
    let ce      = PUSH1 (Int 32)                        =>> ce          in  (*                           32 >> retsize >> retbegin >> .. *)
    let ce      = throw_if_NEQ                              ce          in  (* IF 32!=retsize ERROR                       retbegin >> .. *)
                  MLOAD                                 =>> ce              (*                                         M[retbegin] >> .. *)

and codegen_send_cn cn m args msg ly le ce =  (* msg-call to a contract *) 
    let TyInstnc cname = snd cn                                         in  
    let cnidx   = lookup_cnidx_at_ce cname                  ce          in 
    let callee  = lookup_cn cnidx                           ce          in
    let Some mname = m                                                  in 
    let m       = lookup_mthd_head ce callee mname                      in
    let TyMthd(id,_,reTy) = m                                           in 
    let retsize = size_of_ty reTy                                       in  (*                                                                                                              .. *)
    let ce      = Comment("BEGINE send to "^id)         =>> ce          in  
    let ce      = reset_PC                                  ce          in  (*                                                                                                     PCbkp >> .. *)
    let ce      = PUSH1(Int retsize)                    =>> ce          in  (*                                                                                          retsize >> PCbkp >> .. *)
    let ce      = DUP1                                  =>> ce          in  (*                                                                               retsize >> retsize >> PCbkp >> .. *)
    let ce      = malloc                                    ce          in  (*                                                                              retbegin >> retsize >> PCbkp >> .. *)
    let ce      = repeat DUP2 2                             ce          in  (*                                                       retbegin >> retsize >> retbegin >> retsize >> PCbkp >> .. *)
    let ce      = mstore_mhash_and_args m args        ly le ce          in  (*                               &mhash >> argssize+4 >> retbegin >> retsize >> retbegin >> retsize >> PCbkp >> .. *)
    let ce      = push_msg_and_gas msg cn             ly le ce          in  (*  gas-3000 >> cnAddr >> msg >> &mhash >> argssize+4 >> retbegin >> retsize >> retbegin >> retsize >> PCbkp >> .. *)
    let ce      = call_and_restore_PC                       ce          in  (*                                                                                       retsize >> retbegin >> .. *)
    let ce      = mload_ret_value                           ce          in  (*                                                                                                       ret >> .. *)
                  Comment("END send to "^id)            =>> ce 

and codegen_send_eoa eoa msg ly le ce =   (* send value to an EOA *) 
    let ce      = Comment "BEGINE send to Addr"         =>> ce          in  (*                                                                   .. *) 
    let ce      = reset_PC                                  ce          in  (*                                                          PCbkp >> .. *) 
    let ce      = PUSH1(Int 0)                          =>> ce          in  (*                                                     0 >> PCbkp >> .. *) 
    let ce      = repeat DUP1 5                             ce          in  (*                            0 >> 0 >> 0 >> 0 >> 0 >> 0 >> PCbkp >> .. *) 
    let ce      = push_msg_and_gas msg eoa            ly le ce          in  (* gas-3000 >> addr >> msg >> 0 >> 0 >> 0 >> 0 >> 0 >> 0 >> PCbkp >> .. *) 
    let ce      = call_and_restore_PC                       ce          in  (*                                                         0 >> 0 >> .. *)
    let ce      = POP                                   =>> ce          in  (*                                                              0 >> .. *)
                  Comment("END send to Addr")           =>> ce 


and sstore_to_lval(TmArray(a,i),_) ly le ce     =                           (*                                  rval >> .. *)
    let ce      = keccak_of_array a i                 ly le ce          in  (*                      KEC(a^i) >> rval >> .. *)
                  SSTORE                                =>> ce              (* S[KEC(a^i)] := rval                      .. *)

and codegen_assign l r ly le ce = 
    let ce      = Comment "BEGIN Assignment"            =>> ce          in 
    let ce      = r                              >>(R,ly,le,ce)         in  (*                                    r >> .. *)
    let ce      = sstore_to_lval l                    ly le ce          in  (* S[KEC(l)] := r                          .. *)  
    let ce      = Comment "END Assignment"              =>> ce          in 
    le,ce 

and checked_codegen_LAnd l r aln    ly le ce = 
    assert(aln=R);         
    let la      =   fresh_label ()                                      in  (*                                                        .. *)
    let ce      =   l                            >>(R,ly,le,ce)         in  (*                                                   l >> .. *)
    let ce      =   DUP1                                =>> ce          in  (*                                              l >> l >> .. *)
    let ce      =   if_0_GOTO la                            ce          in  (*                                                   l >> .. *)
    let ce      =   POP                                 =>> ce          in  (*                                                        .. *)
    let ce      =   r                            >>(R,ly,le,ce)         in  (*                                                   r >> .. *)
    let ce      = JUMPDEST la                           =>> ce          in  (*                                                   r >> .. *)
                    repeat ISZERO 2                       ce                (*                                                l&&r >> .. *)  

and mstore_mthd_args aln args       ly le ce =
    let ce    = PUSH1(Int 0)                            =>> ce          in  (*                                                   0 >> .. *)
                foldl (mstore_mthd_arg aln le ly) ce args                   (*                                             sumsize >> .. *) 

and mstore_mthd_arg aln le ly ce arg  =
    let ty      = get_ty arg                                            in  
    let i       = match aln with | L -> size_of_ty ty 
                                 | R -> 32                              in  (*                                                 sum >> .. *)
    let ce      = PUSH1 (Int i)                         =>> ce          in  (*                                         size >> sum >> .. *)
    let ce      = arg                                   >>(aln,ly,le,ce)in  (*                                  arg >> size >> sum >> .. *)
    let ce      = DUP2                                  =>> ce          in  (*                          size >> arg >> size >> sum >> .. *)
    let ce      = malloc                                    ce          in  (*                   alloc(size) >> arg >> size >> sum >> .. *)
    let ce      = MSTORE                                =>> ce          in  (* M[alloc(size)] := arg                   size >> sum >> .. *)
                  ADD                                   =>> ce              (*                                            size+sum >> .. *)

and mstore_mhash_and_args mthd args ly le ce =                              (*                                                        .. *)
    let ce      = mstore_mthd_hash mthd                     ce          in  (*                                         &mhash >> 4 >> .. *)
    let ce      = mstore_mthd_args R args ly             le ce          in  (*                              argsize >> &mhash >> 4 >> .. *)
    let ce      = SWAP1                                 =>> ce          in  (*                              &mhash >> argsize >> 4 >> .. *)
    let ce      = SWAP2                                 =>> ce          in  (*                              4 >> argsize >> &mhash >> .. *)
    let ce      = ADD                                   =>> ce          in  (*                                 argsize+4 >> &mhash >> .. *)
                  SWAP1                                 =>> ce              (*                                 &mhash >> argsize+4 >> .. *)

and codegen_mthd_argLen_chk m ce = match m with  
    | TyDefault     -> ce
    | TyMthd _      ->
    let ce      = PUSH4(Int(calldatasize m))            =>> ce          in
    let ce      = CALLDATASIZE                          =>> ce          in
                  throw_if_NEQ                              ce    

and escape_ARG arg retlabel a       ly le ce = 
    let ce      = Comment "ESCAPE START"                =>> ce          in 
    let ce      = PUSH32(Label retlabel)                =>> ce          in     
    let ce      = arg                                   >>(a,ly,le,ce)  in 
    let ce      = ePUSH                                     ce          in
                  Comment "ESCAPE DONE"                 =>> ce 

and codegen_app_rec(TmApp((TmIRec(i),_),arg)) a ly le ce = 
    let ce      = Comment "BEGIN APP-REC"               =>> ce          in
    let retaddr = fresh_label ()                                        in 
    let start   = lookup_recursion_param le                             in 
    let ce      = escape_ARG arg retaddr            a ly le ce          in 
    let ce      = goto start                                ce          in 
    let ce      = JUMPDEST retaddr                      =>> ce          in       
                  Comment "END APP-REC"                 =>> ce 

and codegen_fix(TmFix(phi,n,ty,tm)) ly le ce = 
    let ce      = Comment "BEGIN FIX"                   =>> ce          in 
    let start   = fresh_label ()                                        in 
    pf "! add_recursion_param(label%d)\n" start;     
    let le      = add_recursion_param le start                          in
    let ce      = JUMPDEST start                        =>> ce          in 
    let ce      = tm                                    >>(R,ly,le,ce)  in  (*                               tm >> .. *)
    let ce      = ePOP                                      ce          in  (*                    retAddr >> tm >> .. *) 
    let ce      = JUMP                                  =>> ce          in  (* GOTO retAddr                  tm >> .. *)
    let ce      = Comment "END FIX"                     =>> ce          in 
    ce 

and codegen_istrct (TmIStrct(i))    ly le ce = 
    let ce      = Comment "BEGIN Struct Parameter"      =>> ce          in
    let ce      = get_escaped_arg                           ce          in 
                  Comment "END Struct Parameter"        =>> ce

and codegen_app (TmApp(t1,t2)) ly le ce = match fst t1 with 
    | TmI(i,n) -> let t1 = lookup_brjidx i le                           in 
                  codegen_app       (TmApp(t1,t2))    ly le ce 
    | TmIRec(i)-> codegen_app_rec   (TmApp(t1,t2))  R ly le ce 
    | TmFix(f,n,ty,tm)-> 
    let ret     = fresh_label ()                                        in 
    let ce      = escape_ARG t2 ret                 R ly le ce          in 
    let ce      = codegen_fix (TmFix(f,n,ty,tm))      ly le ce          in
                  JUMPDEST ret                          =>> ce 
    | _ -> 
    pf "! add_brjidx %s\n" (str_of_tm t2); 
    let le      = add_brjidx le t2                                      in       
                  t1                                    >>(R,ly,le,ce)  

and codegen_abs (TmAbs(x,tyX,t))    ly le ce = 
    let ce      = t                                     >>(R,ly,le,ce)  in
    ce 

and codegen_idx (TmI(i,n))          ly le ce = 
    let tm      = lookup_brjidx i le                                    in 
                  tm                                    >>(R,ly,le,ce)   
       

and codegen_if (TmIf(b,t1,t2))      ly le ce = 
    let elif    = fresh_label()                                         in 
    let fi      = fresh_label()                                         in
    let ce      = Comment "IF"                          =>> ce          in 
    let ce      = b                                     >>(R,ly,le,ce)  in 
    let ce      = if_0_GOTO elif                            ce          in 
    let ce      = Comment "THEN"                        =>> ce          in 
    let ce      = t1                                    >>(R,ly,le,ce)  in
    let ce      = goto fi                                   ce          in 
    let ce      = Comment "ELSE"                        =>> ce          in 
    let ce      = JUMPDEST elif                         =>> ce          in 
    let ce      = t2                                    >>(R,ly,le,ce)  in 
    let ce      = Comment "FI"                          =>> ce          in 
    let ce      = JUMPDEST fi                           =>> ce          in 
    ce

and codegen_mthd ly cnidx (le,ce) (TmMthd(hd,bd))  =
    let ce      = Comment ("BEGIN " ^str_of_ty hd)      =>> ce          in  
    let label   = fresh_label()                                         in 
    register_entry (Mthd(cnidx,hd)) label; 
    let le      = add_mthdCallerArgLocs(TmMthd(hd,bd))(add_empty_ctx (add_empty_brj le))    in
    let ce    = JUMPDEST label                          =>> ce          in 
    let ce      = codegen_mthd_argLen_chk hd                ce          in
    let ce      = bd                                    >>(R,ly,le,ce)  in
    let ce      = Comment ("END "^str_of_ty hd)         =>> ce          in  
    le,ce

and codegen_selfdstr tm ly le ce =    
    let ce      = tm                                    >>(R,ly,le,ce)  in
    let ce      = SELFDESTRUCT                          =>> ce          in
    le, ce

and mstore_tms l aln ly le ce = match l with  
    | []        ->  le,         repeat (PUSH1(Int 0)) 2     ce    
    | tm::tms   ->  let le,ce = mstore_tm tm aln      ly le ce          in (*                                      alloc(size) >> size >> .. *)
                    let ce    = SWAP1                   =>> ce          in (*                                      size >> alloc(size) >> .. *)
                    let le,ce = mstore_tms tms aln    ly le ce          in (*   0 >> 0 >> size' >> alloc(size') >> size >> alloc(size) >> .. *)
                    let ce    = POP                     =>> ce          in (*        0 >> size' >> alloc(size') >> size >> alloc(size) >> .. *)       
                    let ce    = ADD                     =>> ce          in (*             size' >> alloc(size') >> size >> alloc(size) >> .. *)    
                    let ce    = SWAP1                   =>> ce          in (*             alloc(size') >> size' >> size >> alloc(size) >> .. *)     
                    le, ce   (* POP                                                           size' >> size >> alloc(size) >> .. *) 
                             (* ADD                                                             size+size'  >> alloc(size) >> .. *) 
                             (* SWAP1                                                           alloc(size) >> size+size'  >> .. *)
                    
and codegen_log _ args ev ly le ce =
    let visible, args= split_ev_args ev args                            in
    let ce      = push_args le ly visible                   ce          in
    let ce      = push_evnt_hash  ev                        ce          in
    let le,ce   = mstore_tms args                   R ly le ce          in  (* stack : [..., size, offset] *)
    let n       = L.length visible + 1                                  in
    let ce      = log n                                 =>> ce          in  (* deindexee N in logN *)
    le, ce

(***************************************)
(***     5. CODEGEN RETURN          ***)
(***************************************)

and push_args le (ly:storage)    = foldr (fun arg ce -> arg >> (R,ly,le,ce))  
and sstore_words_to stor_locs ce    = foldl sstore_word_to ce stor_locs
and sstore_word_to ce stor_loc      =
    let ce      =   PUSH32 (Int stor_loc)               =>> ce          in
                    SSTORE                              =>> ce       

and sstore_vars offst idx vars ly le ce = 
    let cntrct  =   lookup_cn idx                           ce          in 
    let varlocs =   fieldVar_locations offst cntrct                     in
    assert(L.length varlocs=L.length vars) ; 
    let ce      =   push_args le ly vars ce                             in  (*                                         argk >> .. >> arg1 >> .. *)
    let ce      =   sstore_words_to varlocs ce                          in  (*  S[l_k]:=argk; .. ; S[l_1]:=arg1                              .. *)
    le,ce

and cont_call (TmCall(cn,args),_) ly le ce = 
    let idx     =   lookup_cnidx_at_ce cn                   ce          in 
    let offst   =   (ly.vars idx).offst                            in  
    let ce      =   Comment "Setting Cont"              =>> ce          in 
    let ce      =   set_PC idx                              ce          in  (*  S[PC] := rntime_offset_of_cn                                 .. *) 
                    sstore_vars offst idx args        ly le ce              (*  S[l_k]:= argk; .. ; S[l_1]:= arg1                            .. *)

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
    let ce      = PUSH1(Int 32)                         =>> ce          in  (*                             32 >> val >> .. *)
    let ce      = DUP1                                  =>> ce          in  (*                       32 >> 32 >> val >> .. *)
    let ce      = malloc                                    ce          in  (*                alloc(32) >> 32 >> val >> .. *)
    let ce      = SWAP2                                 =>> ce          in  (*                val >> 32 >> alloc(32) >> .. *)
    let ce      = DUP3                                  =>> ce          in  (*   alloc(32) >> val >> 32 >> alloc(32) >> .. *)
    let ce      = MSTORE                                =>> ce          in  (* M[alloc(32)]:=val     32 >> alloc(32) >> .. *)
                  SWAP1                                 =>> ce              (*                       alloc(32) >> 32 >> .. *)

and mstore_tm (tm,ty) aln ly le ce =
    let ce      = (tm,ty)                               >>(aln,ly,le,ce)in  (*                                    tm >> .. *)
    let ce      = mstore_word ty ce                                     in  (* M[alloc(32)]:=tm      alloc(32) >> 32 >> .. *)
    le,ce

and codegen_return ret cont ly le ce =
    let ce          =   Comment "BEGIN RETURN"          =>> ce          in 
    let le,ce       =   cont_call cont                ly le ce          in
    let ce          =   match ret with
    | (TmUnit,_)    ->  Comment"END RETURN" =>> ( STOP  =>> ce )       
    | tm            ->  
    let le,ce       =   mstore_tm tm                R ly le ce          in
                        RETURN                          =>> ce          in 
    let ce          =   Comment "END RETURN"            =>> ce          in 
    le,ce


(********************************************)
(***     6. CODEGEN CONTRACT             ***)
(********************************************)

let codegen_mthds ly    = ($$$) foldl codegen_mthd ly

let codegen_cntrct ly le ce (idx,TmCn(id,flds,mthds)) =
    let label = fresh_label()                                       in 
    register_entry (Cntrct idx) label ;                    
    let ce    = JUMPDEST label                          =>> ce      in     
    let ce      = init_malloc                               ce      in  (* M[0x40]:=0x60                                  *)                                  
    let le,ce   = dispatcher idx(TmCn(id,flds,mthds))   le  ce      in  (*                                                *)
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
    let init_rc = init_rntimeCode (lookup_cnidx cns) cns                            in 
    foldl (append_rntime ly) init_rc cns

let info_of_cr cr       = cr_info_of_cn cr.cr_cn (program_of_cr cr)

let sizes_of_crs crs         =
    let len_s = map (code_len $ ce_of_cr) crs                                     in
    let len_s = idx_sort len_s                                                  in
    L.map snd len_s

let offsets_of_sizes init l         =
    let rec loop offsets current        = function 
        | []            -> L.rev offsets
        | size::rest    -> loop(current::offsets)(current+size)rest                 in 
    loop [] init l 

let rntimeInfo_of_rntimeCode rc crs : rntimeInfo =
    let crs_sizes           = sizes_of_crs crs                               in
    let crs_offsets         = offsets_of_sizes (code_len rc.rntime_ce) crs_sizes    in
    let crs_totalsize       = BL.sum crs_sizes                                      in
    { rntimeCodeSize        = crs_totalsize + code_len rc.rntime_ce
    ; rntimeCnstrSizes      = to_idxlist crs_sizes
    ; rntimeCnOffsts        = rc.rntime_cn_offsets
    ; rntimeCnstrOffsts     = to_idxlist crs_offsets }

(*  Since the code is stored in the reverse order, the concatenation is also reversed. *)
let compose_bytecode crs rc idx : big_int Evm.program =
    let cnstrInfos          = map info_of_cr  crs                       in
    let rntimeInfo          = rntimeInfo_of_rntimeCode rc crs                       in
    let cnly                = cnstrct_cnLayout cnstrInfos rntimeInfo                in
    let cr                  = lookup idx crs                                        in
    let imm_cr              = realize_program cnly idx(program_of_cr cr)            in
    let cns_program         = program_of_crs crs                                    in 
    let rn_program          = extract_program rc.rntime_ce                          in
    let imm_rntime          = realize_program cnly idx(cns_program@rn_program) in
    imm_rntime @ imm_cr     (* the code is stored in the reverse order *)
