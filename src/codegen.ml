
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

let align_addr aln vm       = match aln with 
    | R                     ->  vm
    | L                     ->  shiftLtop vm (12 * 8)
let align_to_L vm ty        = function 
    | R                     ->  vm
    | L                     ->  let size = size_of_ty ty in
                                assert (size <= 32) ;
                                shiftLtop vm ((32-size)*8) 


(*****************************************)
(***  1. CONTRACT CREATION CODE        ***)
(*****************************************)

(**   1.1  Stor Var Setup        **) 
let mstore_vars cn vm =                                             (* This copies fieldVars at the end of the bytecode into MEM.             *) 
    let size    = size_of_vars_in_cn cn                         in  (* M[0x40](==M[64]) is increased accordingly                              *)
    let vm      = PUSH4(Int size)                   =>> vm      in  (*                                                             size >> .. *)
    let vm      = DUP1                              =>> vm      in  (*                                                     size >> size >> .. *)
    let vm      = malloc                                vm      in  (*                                              alloc(size) >> size >> .. *)
    let vm      = DUP2                              =>> vm      in  (*                                      size >> alloc(size) >> size >> .. *)
    let vm      = DUP1                              =>> vm      in  (*                              size >> size >> alloc(size) >> size >> .. *)
    let vm      = CODESIZE                          =>> vm      in  (*                  codesize >> size >> size >> alloc(size) >> size >> .. *)
    let vm      = SUB                               =>> vm      in  (*                     codesize-size >> size >> alloc(size) >> size >> .. *)
    let vm      = DUP3                              =>> vm      in  (*      alloc(size) >> codesize-size >> size >> alloc(size) >> size >> .. *)
                  CODECOPY                          =>> vm          (*          to             from                 alloc(size) >> size >> .. *)
                                                                    (*                                               codebegin                *)
let check_codesize cnidx vm     =  
    let vm      = PUSH32(InitDataSize cnidx)        =>> vm      in  (*                                    datasize >> mem_start >> size >> .. *)
    let vm      = CODESIZE                          =>> vm      in  (*                        codesize >> datasize >> mem_start >> size >> .. *) 
                  throw_if_NEQ                          vm          (* IF not eq THEN error                                                   *) 

let init_stor_vars cnidx vm   =                               
    let label   = fresh_label()                                 in
    let exit    = fresh_label()                                 in  (*                                                   mem_start    >>   size    >> .. *)  
(*   let vm      = check_codesize cnidx                  vm      in *)
    let vm      = PUSH4(StorFldBegin cnidx)         =>> vm      in  (*                                          idx >>   mem_start    >>   size    >> .. *)
    let vm   = JUMPDEST label                       =>> vm      in  (*                                          idx >>   mem_start    >>   size    >> .. *)
    let vm      = DUP3                              =>> vm      in  (*                                 size >>  idx >>   mem_start    >>   size    >> .. *)
    let vm      = if_0_GOTO exit                        vm      in  (* IF size==0 THEN GOTO exit                idx >>   mem_start    >>   size    >> .. *)   
    let vm      = DUP2                              =>> vm      in  (*                            mem_start >>  idx >>   mem_start    >>   size    >> .. *) 
    let vm      = MLOAD                             =>> vm      in  (*                         M[mem_start] >>  idx >>   mem_start    >>   size    >> .. *)
    let vm      = DUP2                              =>> vm      in  (*                  idx >> M[mem_start] >>  idx >>   mem_start    >>   size    >> .. *)
    let vm      = SSTORE                            =>> vm      in  (* S[idx]=M[mem_start]                      idx >>   mem_start    >>   size    >> .. *)  
    let vm      = PUSH1(Int 0x20)                   =>> vm      in  (*                                 0x20 >>  idx >>   mem_start    >>   size    >> .. *)
    let vm      = SWAP1                             =>> vm      in  (*                                  idx >> 0x20 >>   mem_start    >>   size    >> .. *)
    let vm      = SWAP3                             =>> vm      in  (*                                 size >> 0x20 >>   mem_start    >>   idx     >> .. *)
    let vm      = SUB                               =>> vm      in  (*                                   size- 0x20 >>   mem_start    >>   idx     >> .. *)
    let vm      = SWAP2                             =>> vm      in  (*                                          idx >>   mem_start    >> size-0x20 >> .. *) 
    let vm      = incr                                  vm      in  (*                                        idx+1 >>   mem_start    >> size-0x20 >> .. *)
    let vm      = SWAP1                             =>> vm      in  (*                                    mem_start >>       idx+1    >> size-0x20 >> .. *)
    let vm      = incr_n   0x20                         vm      in  (*                               mem_start+0x20 >>       idx+1    >> size-0x20 >> .. *)
    let vm      = SWAP1                             =>> vm      in  (*                                        idx+1 >> mem_start+0x20 >> size-0x20 >> .. *)
    let vm      = goto label                            vm      in  (*                                        idx+1 >> mem_start+0x20 >> size-0x20 >> .. *)
    let vm   = JUMPDEST exit                        =>> vm      in  (*                                          idx >>   mem_start    >>   size    >> .. *)
                  repeat POP 3                          vm          (*                                                                     .. *)

(**   1.2. Stor Arr Setup       **)                   
let reset_salloc_arr vm = 
    let vm      = PUSH1 (Int 1)                     =>> vm      in  (*                                           1 >> .. *)
    let vm      = DUP1                              =>> vm      in  (*                                      1 >> 1 >> .. *)
                  SSTORE                            =>> vm          (* S[1]:=1                                        .. *) 

let salloc_arr vm (arrArgLoc:int) =   
    let label   = fresh_label()                                 in  (*                                                .. *) 
    let vm      = PUSH4 (Int arrArgLoc)             =>> vm      in  (*                                        seed >> .. *)
    let vm      = SLOAD                             =>> vm      in  (*                                     S[seed] >> .. *) 
    let vm      = PUSH4 (Label label)               =>> vm      in  (*                            label >> S[seed] >> .. *)
    let vm      = JUMPI                             =>> vm      in  (* IF S[seed]!=0 GOTO label                       .. *) 
    let vm      = sincr 1                               vm      in  (*                                      S[1]++ >> .. *)      
    let vm      = PUSH4 (Int arrArgLoc)             =>> vm      in  (*                                seed >> S[1] >> .. *)
    let vm      = SSTORE                            =>> vm      in  (* S[seed]:=S[1]                                  .. *)
                  JUMPDEST label                    =>> vm          (*                                                .. *)

let init_salloc_arr_if_not vm = 
    let label   = fresh_label ()                                in  (*                                                   *) 
    let vm      = PUSH1 (Int 1)                     =>> vm      in  (*                                                   *) 
    let vm      = SLOAD                             =>> vm      in  (*                                                   *)
    let vm      = PUSH4 (Label label)               =>> vm      in  (*                                                   *) 
    let vm      = JUMPI                             =>> vm      in  (* IF S[1]!=0 then GOTO label                  >> .. *)
    let vm      = reset_salloc_arr                      vm      in  (*                                      1 >> 1 >> .. *) 
                  JUMPDEST label                    =>> vm      

let init_stor_arrs cn vm =
    let vm      = init_salloc_arr_if_not                vm      in   
    let arrLocs = arr_locs_of_cn cn                             in  
                  foldl salloc_arr vm arrLocs 

(**   1.3. Runtime CODE COPY         **) 
let mstore_rn_code idx vm =                                         (*                                                           .. *)
    let vm      = PUSH32(RnSize)                    =>> vm      in  (*                                                   size >> .. *)
    let vm      = DUP1                              =>> vm      in  (*                                           size >> size >> .. *)  
    let vm      = malloc                                vm      in  (*                                    alloc(size) >> size >> .. *)
    let vm      = DUP2                              =>> vm      in  (*                            size >> alloc(size) >> size >> .. *)
    let vm      = PUSH32(RnOffset idx)              =>> vm      in  (*                     idx >> size >> alloc(size) >> size >> .. *)
    let vm      = DUP3                              =>> vm      in  (*      alloc(size) >> idx >> size >> alloc(size) >> size >> .. *)
                  CODECOPY                          =>> vm          (*                                    alloc(size) >> size >> .. *)
                                                                    (*                                     codebegin                *)
(**   1.4.  CONTRACT CREATION   *****)
type creation           =   { cr_vm           : vm
                            ; cr_ty           : ty
                            ; cr_cn           : ty toplevel } 

let vm_of_cr cr         =   cr.cr_vm
let size_of_cr          =   code_len     $ vm_of_cr 
let prog_of_cr          =   extract_prog $ vm_of_cr  
let sizes_of_crs        =   L.map snd $ idx_sort $ map size_of_cr
let progs_of_crs        =   L.map snd $ idx_sort $ map prog_of_cr 
let prog_of_crs         =   L.concat $ L.rev $ progs_of_crs 


let codegen_creation cns idx = (* return vm which contains the program *) 
    let TmCn(id,_,_) as cn = lookup idx cns in 
    let vm      =   empty_vm (lookup_cnidx cns) cns             in  (*                                                                                 *)
    let vm      =   Comment("Begin Creation "^id)   =>> vm      in 
    let vm      =   init_malloc                         vm      in  (* M[0x40] := 0x60                                                                 *)
    let vm      =   mstore_vars        cn               vm      in  (*                                            alloc(argssize) << argssize << ..    *)
    let vm      =   init_stor_vars    idx               vm      in  (* S[i..i+sz-1]:= argCodes               i << alloc(argssize) << argssize << ..    *)
    let vm      =   init_stor_arrs     cn               vm      in  (* S[1]        := #array                 i << alloc(argssize) << argssize << ..    *)
    let vm      =   set_PC            idx               vm      in  (* S[PC]       := rn_cn_offst     (returned body)                                  *)
    let vm      =   mstore_rn_code    idx               vm      in  (*                                      alloc(codesize) << codesize << i <<  ..    *)
    let vm      =   RETURN                          =>> vm      in  (* OUTPUT(M[code]) as The BODY code                                    i <<  ..    *)
                    Comment("End Creation "^id)     =>> vm 

let init_creation cns idx  : creation =
    let cn      =   L.assoc idx cns in 
    { cr_vm           = codegen_creation cns idx
    ; cr_ty           = typeof_cn cn 
    ; cr_cn           = cn                                }

let init_creations cns : creation ilist =
    imap (init_creation cns) cns


(***************************************)
(***     3.  DISPATHER               ***)
(***************************************)

let dispatch_method idx le vm m =                                           (*                                           ABCD >> .. *)  
    let vm      =   DUP1                                    =>> vm      in  (*                                   ABCD >> ABCD >> .. *)
    let vm      =   push_mthd_hash m                            vm      in  (*                              m >> ABCD >> ABCD >> .. *)
    let vm      =   EQ                                      =>> vm      in  (*                             m=ABCD?1:0 >> ABCD >> .. *)
    let vm      =   PUSH8(RnMthdLabel(idx,m))               =>> vm      in  (*                Rntime(m) >> m=ABCD?1:0 >> ABCD >> .. *)
                    JUMPI                                   =>> vm          (* if m=ABCD then GOTO Rntime(m)             ABCD >> .. *)

let dispatch_default idx le vm =
    let vm      =   PUSH8(RnMthdLabel(idx,TyDefault))       =>> vm      in
                    JUMP                                    =>> vm     

let dispatcher idx (TmCn(_,_,mthds)) le vm = 
    let tyMthds =   L.map(function TmMthd(head,_) -> head) mthds        in
    let uMthds  =   filter_method tyMthds                               in 
    let vm      =   Comment "BEGIN Method Dispatchers "     =>> vm      in 
    let vm      =   PUSH1 (Int 0x00)                        =>> vm      in  (* get inputdata[0x00] *) 
    let vm      =   CALLDATALOAD                            =>> vm      in  (*               ABCDxxxxxxxxxxxxxxxxxxxxxxxxxxxxx >> .. *)
    let vm      =   shiftRtop vm Crpt.(word_bits-sig_bits)              in  (*                                            ABCD >> .. *)                             
    let vm      =   foldl(dispatch_method idx le)vm uMthds              in  (* JUMP to Method ABCD                                   *)   
    let vm      =   POP                                     =>> vm      in  (*                                                    .. *)
    let vm      =   if  default_exists tyMthds
                        then dispatch_default idx            le vm          (* JUMP to Default Method                             .. *) 
                        else throw vm                                   in  (* JUMP to error                                      .. *) 
    let vm      =   Comment "END Method Dispatchers "       =>> vm      in 
    le,vm

(*********************************************)
(***     4.    CODEGEN  PRECONTRACT        ***)
(*********************************************)

let rec codegen_pre_call id args rety aln ly le vm = match id with 
    | "pre_ecdsarecover"    ->  assert(aln=R);      codegen_ECDSArecover args   ly le vm     
    | "keccak256"           ->  assert(aln=R);      codegen_keccak256 args      ly le vm       
    | "iszero"              ->  assert(rety=TyBool);codegen_iszero args aln     ly le vm 
    | _                     ->  err "codegen_predef_call: Direct Contract Call is Not supported. Specify a Method."

and codegen_iszero [arg] aln ly le vm =
    let vm    = arg                    >>(aln,ly,le,vm)     in
                ISZERO                          =>> vm 
       
and codegen_keccak256 args   ly le vm =
    let vm    = get_malloc vm                               in  
    let vm    = mstore_mthd_args L args       ly le vm      in  
    let vm    = SWAP1                           =>> vm      in  
                SHA3                            =>> vm                

and codegen_ECDSArecover args ly le vm = match args with [h;v;r;s] ->  
    let vm    = PUSH1 (Int 0x20)                =>> vm      in  
    let vm    = DUP1                            =>> vm      in  
    let vm    = malloc                              vm      in  
    let vm    = repeat DUP2 2                       vm      in  
    let vm    = get_malloc                          vm      in
    let vm    = mstore_mthd_args R args       ly le vm      in  
    let vm    = SWAP1                           =>> vm      in  
    let vm    = PUSH1 (Int 0)                   =>> vm      in  
    let vm    = PUSH1 (Int 1)                   =>> vm      in  
    let vm    = PUSH4 (Int 10000)               =>> vm      in  
    let vm    = CALL                            =>> vm      in  
    let vm    = throw_if_0 vm                               in
    let vm    = POP                             =>> vm      in  
    let vm    = SWAP1                           =>> vm      in  
    let vm    = POP                             =>> vm      in  
                MLOAD                           =>> vm                    (* stack: [output] *)
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

and push_loc vm aln ty      = function 
    | Code       _          ->  err "push_loc: Code"  
    | Calldata rng          ->  calldataload  vm rng
    | Stor     rng          ->  let vm = push_storRange vm rng in 
                                align_to_L vm ty aln  
    | Stack      n          ->  let vm = dup_nth_from_bottom n vm in 
                                align_to_L vm ty aln 

and mstore_new_instance id args msg ly le vm  =
    let cnidx   =   lookup_cnidx_at_vm id                   vm      in 
    let vm      =   PUSH32(CrSize     cnidx)            =>> vm      in  (*                                                        size >> .. *) 
    let vm      =   PUSH32(RnCrOffset cnidx)            =>> vm      in  (*                                             cn_idx  >> size >> .. *)
    let vm      =   mstore_code                             vm      in  (*                                         alloc(size) >> size >> .. *)
    let vm      =   SWAP1                               =>> vm      in  (*                                         size >> alloc(size) >> .. *)
    let vm      =   mstore_whole_code                       vm      in  (*                alloc(wsize) >> wsize >> size >> alloc(size) >> .. *)
    let vm      =   mstore_mthd_args R args           ly le vm      in  (*    argssize >> alloc(wsize) >> wsize >> size >> alloc(size) >> .. *)
    let vm      =   SWAP1                               =>> vm      in  (*    alloc(wsize) >> argssize >> wsize >> size >> alloc(size) >> .. *)
    let vm      =   POP                                 =>> vm      in  (*                    argssize >> wsize >> size >> alloc(size) >> .. *)
    let vm      =   ADD                                 =>> vm      in  (*                       argssize+wsize >> size >> alloc(size) >> .. *)
    let vm      =   ADD                                 =>> vm      in  (*                          argssize+wsize+size >> alloc(size) >> .. *)
                    SWAP1                               =>> vm          (*                                  alloc(size) >>   totalsize >> .. *)

and codegen_new id args msg ly le vm   =    
    let vm      =   reset_PC vm                                     in  (*                                             PCbkp >> .. *)
    let vm      =   mstore_new_instance id args msg   ly le vm      in  (*                      alloc(size) >> size >> PCbkp >> .. *)
    let vm      =   msg                          >>(R,ly,le,vm)     in  (*             value >> alloc(size) >> size >> PCbkp >> .. *)
    let vm      =   CREATE                              =>> vm      in  (*                             createResult >> PCbkp >> .. *)
    let vm      =   throw_if_0                              vm      in  (*                             createResult >> PCbkp >> .. *)
    let vm      =   SWAP1                               =>> vm      in  (*                             PCbkp >> CreateResult >> .. *)
                    restore_PC                              vm          (*                                      CreateResult >> .. *)

and codegen_array aid aidx ly le vm    =                                (*                                                      .. *)
    let vm      =   keccak_of_array aid aidx          ly le vm      in  (*                                      keccak(a[i]) >> .. *)
                    SLOAD                               =>> vm          (*                                   S[keccak(a[i])] >> .. *) 

and keccak_of_array aid aidx ly le vm  =                                (* S[keccak(a[i])] := the seed of array *) 
    let vm      =   aidx                         >>(R,ly,le,vm)     in  (*                                             index >> .. *)    
    let vm      =   aid                          >>(R,ly,le,vm)     in  (*                                array_loc >> index >> .. *)
                    keccak_cat                              vm          (*                            sha3(array_loc++index) >> .. *) 
      
and salloc_array aid aidx ly le vm     =
    let push vm =   keccak_of_array aid aidx          ly le vm      in  (*                                        S[KEC(a_i)] >> .. *)
                    salloc_array_of_push push               vm          (* S[KEC(a_i)]:= newSeed                      newSeed >> .. *)     
                                                                      
and salloc_array_of_loc le vm(Stor data) =       
    assert(data.size=Int 1) ;                                           (*                                        S[KEC(a_i)] >> .. *)     
    let push vm =   PUSH32 data.offst                   =>> vm      in  
                    salloc_array_of_push push               vm          (* S[KEC(a_i)]:= newSeed                      newSeed >> .. *)     

and salloc_array_of_push push_array_seed vm =   
    let label   =   fresh_label ()                                  in  (*                                        S[KEC(a_i)] >> .. *) 
    let vm      =   DUP1                                =>> vm      in  (*                          S[KEC(a_i)] >> S[KEC(a_i)] >> .. *) 
    let vm      =   PUSH4(Label label)                  =>> vm      in  (*                 label >> S[KEC(a_i)] >> S[KEC(a_i)] >> .. *) 
    let vm      =   JUMPI                               =>> vm      in  (* {IF S[KEC(a_i)]!=0 GOTO label}          S[KEC(a_i)] >> .. *) 
    let vm      =   POP                                 =>> vm      in  (*                                                      .. *) 
    let vm      =   push_array_seed                         vm      in  
    let vm      =   sincr 1                                 vm      in  (*                                 S[1]++ >> KEC(a_i) >> .. *) 
    let vm      =   DUP1                                =>> vm      in  (*                     newseed >> newseed >> KEC(a_i) >> .. *) 
    let vm      =   SWAP2                               =>> vm      in  (*                     KEC(a_i) >> newseed >> newseed >> .. *) 
    let vm      =   SSTORE                              =>> vm      in  (* S[KEC(a_i)]:= newseed                      newseed >> .. *)
                    JUMPDEST label                      =>> vm          (*                                                         *)

(* le is not updated here.  
 * le can only be updated in a variable initialization *)
and (>>) e (aln,ly,le,vm)           = codegen_tm ly le vm aln e 
and codegen_tm ly le vm aln e       = (* #DEBUG pe_tm e; pe(str_of_ctx le); *)
    match e with 
    | TmApp(t1,t2)          ,_              ->                  codegen_app     (TmApp(t1,t2))            ly le vm 
    | TmAbs(x,tyX,t)        ,_              ->                  codegen_abs     (TmAbs(x,tyX,t))          ly le vm 
    | TmFix(f,n,ty,t)       ,_              ->                  codegen_fix     (TmFix(f,n,ty,t))         ly le vm 
    | TmI(i,n)              ,_              ->                  codegen_idx     (TmI(i,n))                ly le vm 
    | TmIStrct(i)           ,_              ->                  codegen_istrct  (TmIStrct(i))             ly le vm 
    | TmIf(b,t1,t2)         ,_              ->                  codegen_if      (TmIf(b,t1,t2))           ly le vm 
    | Balanc   e            ,_              ->                  BALANCE     =>>     (    e           >>(R,ly,le,vm))
    | EpValue               ,_              ->                  CALLVALUE                                   =>> vm      (* Value (wei) Transferred to the account *) 
    | TmZero                ,_              ->                  PUSH1(Int 0)                                =>> vm 
    | EpNow                 ,_              ->                  TIMESTAMP                                   =>> vm 
    | TmFalse               ,_              ->  assert(aln=R);  PUSH1(Int 0)                                =>> vm  
    | TmTrue                ,_              ->  assert(aln=R);  PUSH1(Int 1)                                =>> vm 
    | TmU256   d            ,_              ->  assert(aln=R);  PUSH32(Big d)                               =>> vm  
    | TmU8     d            ,_              ->  assert(aln=R);  PUSH1(Big d)                                =>> vm  
    | TmAdd  (l,r)          ,_              ->                  codegen_op ADD l r                        ly le vm             
    | TmSub  (l,r)          ,_              ->                  codegen_op SUB l r                        ly le vm             
    | TmMul  (l,r)          ,_              ->                  codegen_op MUL l r                        ly le vm             
    | TmLT   (l,r)          ,_              ->  assert(aln=R);  codegen_op LT  l r                        ly le vm           
    | TmGT   (l,r)          ,_              ->  assert(aln=R);  codegen_op GT  l r                        ly le vm           
    | TmEQ   (l,r)          ,_              ->  assert(aln=R);  codegen_op EQ  l r                        ly le vm           
    | TmNEQ  (l,r)          ,_              ->  assert(aln=R);  ISZERO  =>>     codegen_op EQ l r         ly le vm
    | TmNOT    e            ,_              ->  assert(aln=R);  ISZERO  =>>          (    e        >>(aln,ly,le,vm)) 
    | TmLAND (l,r)          ,_              ->                  checked_codegen_LAnd l r aln              ly le vm 
    | TmCall(id,args)       ,rety           ->                  codegen_pre_call id args rety aln         ly le vm
    | TmSend((e,TyAddr),m,args,msg)    ,_   ->  assert(aln=R);  codegen_send_eoa (e,TyAddr)         msg   ly le vm 
    | TmSend((c,TyInstnc n),m,args,msg),_   ->  assert(aln=R);  codegen_send_cn(c,TyInstnc n)m args msg   ly le vm 
    | TmNew(id,args,msg)    ,TyInstnc _     ->  assert(aln=R);  codegen_new    id args msg                ly le vm 
    | EpAddr(c,TyInstnc i)  ,TyAddr         ->                  (c,TyInstnc i)                     >>(aln,ly,le,vm) 
    | EpSender              ,TyAddr         ->                  align_addr aln             (CALLER          =>> vm) 
    | EpThis                ,_              ->                  align_addr aln             (ADDRESS         =>> vm) 
    | TmArr(id,idx)       ,TyMap _        ->  let vm      =   codegen_array id idx                      ly le vm  in  (*            S[keccak(a[i])] >> .. *)
                                                assert(aln=R);  salloc_array  id idx                      ly le vm      (*                     S[1]++ >> .. *)
    | TmArr(id,idx)       ,      _        ->  assert(aln=R);  codegen_array id idx                      ly le vm      (*               S[keccak(a)] >> .. *)
    | TmId id               ,TyMap(a,b)     ->  let loc     =   lookup_le id le                                     in 
                                                let vm      =   push_loc vm aln(TyMap(a,b))loc                      in 
                                                                salloc_array_of_loc le vm loc                  
    | TmId id               ,ty             ->  let loc     =   lookup_le id le                                     in  
                                                                push_loc vm aln ty loc                                  (*                        loc >> .. *)
    | TmDeref(ref,tyR)      ,ty             ->  assert(size_of_ty ty<=32 && tyR=TyRef ty && aln=R) ;                    (* assuming word-size *)
                                                let vm      =   (ref,tyR)                            >>(R,ly,le,vm) in  (* pushes the pointer *)
                                                                MLOAD                                       =>> vm 
    | e                                     ->  let _,vm    =   codegen_tm_eff e              aln         ly le vm  in 
                                                                PUSH1(Int 0)                                =>> vm 

and codegen_tm_eff tm aln ly le vm      =   match tm with 
    | TmAbort               ,TyErr          ->  le, throw vm                               
    | TmLog(id,args,Some ev),TyUnit         ->  codegen_log id args ev  ly le vm     
    | TmSfDstr tm           ,TyUnit         ->  codegen_selfdstr  tm    ly le vm 
    | TmAssign(l,r)         ,TyUnit         ->  codegen_assign l r      ly le vm  
    | TmReturn(ret,cont)    ,_              ->  codegen_return ret cont ly le vm    
    | e                                     ->  pf "codegen_tm: %s " (str_of_tm e); raise Not_found
    
and codegen_op operator l r ly le vm =
    let vm    = r                                >>(R,ly,le,vm)     in 
    let vm    = l                                >>(R,ly,le,vm)     in 
                operator                                =>> vm 
            
and push_msg_and_gas msg cn ly le vm = 
    let vm      = msg                            >>(R,ly,le,vm)     in  (*                                            value >> .. *) 
    let vm      = cn                             >>(R,ly,le,vm)     in  (*                                  cnAddr >> value >> .. *)
    let vm      = PUSH4(Int 3000)                       =>> vm      in  (*                          3000 >> cnAddr >> value >> .. *)
    let vm      = GAS                                   =>> vm      in  (*                   gas >> 3000 >> cnAddr >> value >> .. *)
                  SUB                                   =>> vm          (*                      gas-3000 >> cnAddr >> value >> .. *)

and call_and_restore_PC vm =                                            (*  gas-3000 >> cnAddr >> msg >> &mhash >> argssize+4 >> retbegin >> retsize >> retbegin >> retsize >> PCbkp >> .. *)
    let vm      = CALL                                  =>> vm      in  (*                                                                   success >> retbegin >> retsize >> PCbkp >> .. *)
    let vm      = PUSH1(Int 0)                          =>> vm      in  (*                                                              0 >> success >> retbegin >> retsize >> PCbkp >> .. *)
    let vm      = JUMPI                                 =>> vm      in  (*  IF success==0 THEN GOTO 0                                                   retbegin >> retsize >> PCbkp >> .. *)
    let vm      = SWAP2                                 =>> vm      in  (*                                                                              PCbkp >> retsize >> retbegin >> .. *)
                  restore_PC                                vm          (*                                                                                       retsize >> retbegin >> .. *)

and mload_ret_value vm =                                                (*                                 retsize >> retbegin >> .. *)
    let vm      = PUSH1 (Int 32)                        =>> vm      in  (*                           32 >> retsize >> retbegin >> .. *)
    let vm      = throw_if_NEQ                              vm      in  (* IF 32!=retsize ERROR                       retbegin >> .. *)
                  MLOAD                                 =>> vm          (*                                         M[retbegin] >> .. *)

and codegen_send_cn cn m args msg ly le vm =  (* msg-call to a contract *) 
    let TyInstnc cname = snd cn                                     in  
    let cnidx   = lookup_cnidx_at_vm cname                  vm      in 
    let callee  = lookup_cn cnidx                           vm      in
    let Some mname = m                                              in 
    let m       = lookup_mthd_head vm callee mname                  in
    let TyMthd(id,_,reTy) = m                                       in 
    let retsize = size_of_ty reTy                                   in  (*                                                                                                              .. *)
    let vm      = Comment("BEGINE send to "^id)         =>> vm      in  
    let vm      = reset_PC                                  vm      in  (*                                                                                                     PCbkp >> .. *)
    let vm      = PUSH1(Int retsize)                    =>> vm      in  (*                                                                                          retsize >> PCbkp >> .. *)
    let vm      = DUP1                                  =>> vm      in  (*                                                                               retsize >> retsize >> PCbkp >> .. *)
    let vm      = malloc                                    vm      in  (*                                                                              retbegin >> retsize >> PCbkp >> .. *)
    let vm      = repeat DUP2 2                             vm      in  (*                                                       retbegin >> retsize >> retbegin >> retsize >> PCbkp >> .. *)
    let vm      = mstore_mhash_and_args m args        ly le vm      in  (*                               &mhash >> argssize+4 >> retbegin >> retsize >> retbegin >> retsize >> PCbkp >> .. *)
    let vm      = push_msg_and_gas msg cn             ly le vm      in  (*  gas-3000 >> cnAddr >> msg >> &mhash >> argssize+4 >> retbegin >> retsize >> retbegin >> retsize >> PCbkp >> .. *)
    let vm      = call_and_restore_PC                       vm      in  (*                                                                                       retsize >> retbegin >> .. *)
    let vm      = mload_ret_value                           vm      in  (*                                                                                                       ret >> .. *)
                  Comment("END send to "^id)            =>> vm 

and codegen_send_eoa eoa msg ly le vm =   (* send value to an EOA *) 
    let vm      = Comment "BEGINE send to Addr"         =>> vm      in  (*                                                                   .. *) 
    let vm      = reset_PC                                  vm      in  (*                                                          PCbkp >> .. *) 
    let vm      = PUSH1(Int 0)                          =>> vm      in  (*                                                     0 >> PCbkp >> .. *) 
    let vm      = repeat DUP1 5                             vm      in  (*                            0 >> 0 >> 0 >> 0 >> 0 >> 0 >> PCbkp >> .. *) 
    let vm      = push_msg_and_gas msg eoa            ly le vm      in  (* gas-3000 >> addr >> msg >> 0 >> 0 >> 0 >> 0 >> 0 >> 0 >> PCbkp >> .. *) 
    let vm      = call_and_restore_PC                       vm      in  (*                                                         0 >> 0 >> .. *)
    let vm      = POP                                   =>> vm      in  (*                                                              0 >> .. *)
                  Comment("END send to Addr")           =>> vm 


and sstore_to_lval(TmArr(a,i),_) ly le vm     =                       (*                                  rval >> .. *)
    let vm      = keccak_of_array a i                 ly le vm      in  (*                      KEC(a^i) >> rval >> .. *)
                  SSTORE                                =>> vm          (* S[KEC(a^i)] := rval                      .. *)

and codegen_assign l r ly le vm = 
    let vm      = Comment "BEGIN Assignment"            =>> vm      in 
    let vm      = r                              >>(R,ly,le,vm)     in  (*                                    r >> .. *)
    let vm      = sstore_to_lval l                    ly le vm      in  (* S[KEC(l)] := r                          .. *)  
    let vm      = Comment "END Assignment"              =>> vm      in 
    le,vm 

and checked_codegen_LAnd l r aln    ly le vm = 
    assert(aln=R);         
    let la      =   fresh_label ()                                  in  (*                                                        .. *)
    let vm      =   l                            >>(R,ly,le,vm)     in  (*                                                   l >> .. *)
    let vm      =   DUP1                                =>> vm      in  (*                                              l >> l >> .. *)
    let vm      =   if_0_GOTO la                            vm      in  (*                                                   l >> .. *)
    let vm      =   POP                                 =>> vm      in  (*                                                        .. *)
    let vm      =   r                            >>(R,ly,le,vm)     in  (*                                                   r >> .. *)
    let vm      = JUMPDEST la                           =>> vm      in  (*                                                   r >> .. *)
                    repeat ISZERO 2                       vm            (*                                                l&&r >> .. *)  

and mstore_mthd_args aln args       ly le vm =
    let vm    = PUSH1(Int 0)                            =>> vm      in  (*                                                   0 >> .. *)
                foldl (mstore_mthd_arg aln le ly) vm args               (*                                             sumsize >> .. *) 

and mstore_mthd_arg aln le ly vm arg  =
    let ty      = get_ty arg                                        in  
    let i       = match aln with | L -> size_of_ty ty 
                                 | R -> 32                          in  (*                                                 sum >> .. *)
    let vm      = PUSH1 (Int i)                         =>> vm      in  (*                                         size >> sum >> .. *)
    let vm      = arg                          >>(aln,ly,le,vm)     in  (*                                  arg >> size >> sum >> .. *)
    let vm      = DUP2                                  =>> vm      in  (*                          size >> arg >> size >> sum >> .. *)
    let vm      = malloc                                    vm      in  (*                   alloc(size) >> arg >> size >> sum >> .. *)
    let vm      = MSTORE                                =>> vm      in  (* M[alloc(size)] := arg                   size >> sum >> .. *)
                  ADD                                   =>> vm          (*                                            size+sum >> .. *)

and mstore_mhash_and_args mthd args ly le vm =                          (*                                                        .. *)
    let vm      = mstore_mthd_hash mthd                     vm      in  (*                                         &mhash >> 4 >> .. *)
    let vm      = mstore_mthd_args R args ly             le vm      in  (*                              argsize >> &mhash >> 4 >> .. *)
    let vm      = SWAP1                                 =>> vm      in  (*                              &mhash >> argsize >> 4 >> .. *)
    let vm      = SWAP2                                 =>> vm      in  (*                              4 >> argsize >> &mhash >> .. *)
    let vm      = ADD                                   =>> vm      in  (*                                 argsize+4 >> &mhash >> .. *)
                  SWAP1                                 =>> vm          (*                                 &mhash >> argsize+4 >> .. *)

and codegen_mthd_argLen_chk m vm = match m with  
    | TyDefault     -> vm
    | TyMthd _      ->
    let vm      = PUSH4(Int(calldatasize m))            =>> vm      in
    let vm      = CALLDATASIZE                          =>> vm      in
                  throw_if_NEQ                              vm        

and escape_ARG arg retlabel a       ly le vm = 
    let vm      = Comment "ESCAPE START"                =>> vm      in 
    let vm      = PUSH32(Label retlabel)                =>> vm      in     
    let vm      = arg                            >>(a,ly,le,vm)     in 
    let vm      = ePUSH                                     vm      in
                  Comment "ESCAPE DONE"                 =>> vm 

and codegen_app_rec(TmApp((TmIRec(i),_),arg)) a ly le vm = 
    let vm      = Comment "BEGIN APP-REC"               =>> vm      in
    let retaddr = fresh_label ()                                    in 
    let start   = lookup_recursion_param le                         in 
    let vm      = escape_ARG arg retaddr            a ly le vm      in 
    let vm      = goto start                                vm      in 
    let vm      = JUMPDEST retaddr                      =>> vm      in       
                  Comment "END APP-REC"                 =>> vm 

and codegen_fix(TmFix(phi,n,ty,tm)) ly le vm = 
    let vm      = Comment "BEGIN FIX"                   =>> vm      in 
    let start   = fresh_label ()                                    in 
    (* #DEBUG pf "! add_recursion_param(label%d)\n" start;     *)
    let le      = add_recursion_param le start                      in
    let vm      = JUMPDEST start                        =>> vm      in 
    let vm      = tm                             >>(R,ly,le,vm)     in  (*                               tm >> .. *)
    let vm      = ePOP                                      vm      in  (*                    retAddr >> tm >> .. *) 
    let vm      = JUMP                                  =>> vm      in  (* GOTO retAddr                  tm >> .. *)
                  Comment "END FIX"                     =>> vm      
       

and codegen_istrct (TmIStrct(i))    ly le vm = 
    let vm      = Comment "BEGIN Struct Parameter"      =>> vm      in
    let vm      = get_escaped_arg                           vm      in 
                  Comment "END Struct Parameter"        =>> vm

and codegen_app (TmApp(t1,t2)) ly le vm = match fst t1 with 
    | TmI(i,n) -> let t1 = lookup_brjidx i le                       in 
                  codegen_app       (TmApp(t1,t2))    ly le vm 
    | TmIRec(i)-> codegen_app_rec   (TmApp(t1,t2))  R ly le vm 
    | TmFix(f,n,ty,tm)-> 
    let ret     = fresh_label ()                                    in 
    let vm      = escape_ARG t2 ret                 R ly le vm      in 
    let vm      = codegen_fix (TmFix(f,n,ty,tm))      ly le vm      in
                  JUMPDEST ret                          =>> vm 
    | _ -> 
    (* #DEBUG pf "! add_brjidx %s\n" (str_of_tm t2); *)
    let le      = add_brjidx le t2                                  in       
                  t1                             >>(R,ly,le,vm)      

and codegen_abs (TmAbs(x,tyX,t))    ly le vm = 
    let vm      = t                              >>(R,ly,le,vm)     in
    vm 

and codegen_idx (TmI(i,n))          ly le vm = 
    let tm      = lookup_brjidx i le                                in 
                  tm                             >>(R,ly,le,vm)       
       

and codegen_if (TmIf(b,t1,t2))      ly le vm = 
    let elif    = fresh_label()                                     in 
    let fi      = fresh_label()                                     in
    let vm      = Comment "IF"                          =>> vm      in 
    let vm      = b                              >>(R,ly,le,vm)     in 
    let vm      = if_0_GOTO elif                            vm      in 
    let vm      = Comment "THEN"                        =>> vm      in 
    let vm      = t1                             >>(R,ly,le,vm)     in
    let vm      = goto fi                                   vm      in 
    let vm      = Comment "ELSE"                        =>> vm      in 
    let vm      = JUMPDEST elif                         =>> vm      in 
    let vm      = t2                             >>(R,ly,le,vm)     in 
    let vm      = Comment "FI"                          =>> vm      in 
    let vm      = JUMPDEST fi                           =>> vm      in 
    vm

and codegen_mthd ly cnidx (le,vm) (TmMthd(hd,bd))  =
    let vm      = Comment ("BEGIN " ^str_of_ty hd)      =>> vm      in  
    let label   = fresh_label()                                     in 
    register_entry (Mthd(cnidx,hd)) label; 
    let le      = add_mthdCallerArgLocs(TmMthd(hd,bd))(add_empty_ctx (add_empty_brj le))    in
    let vm    = JUMPDEST label                          =>> vm      in 
    let vm      = codegen_mthd_argLen_chk hd                vm      in
    let vm      = bd                             >>(R,ly,le,vm)     in
    let vm      = Comment ("END "^str_of_ty hd)         =>> vm      in  
    le,vm

and codegen_selfdstr tm ly le vm =    
    let vm      = tm                             >>(R,ly,le,vm)     in
    let vm      = SELFDESTRUCT                          =>> vm      in
    le, vm

and mstore_tms l aln ly le vm = match l with  
    | []        ->  le,         repeat (PUSH1(Int 0)) 2     vm      
    | tm::tms   ->  let le,vm = mstore_tm tm aln      ly le vm      in  (*                                      alloc(size) >> size >> .. *)
                    let vm    = SWAP1                   =>> vm      in  (*                                      size >> alloc(size) >> .. *)
                    let le,vm = mstore_tms tms aln    ly le vm      in  (*   0 >> 0 >> size' >> alloc(size') >> size >> alloc(size) >> .. *)
                    let vm    = POP                     =>> vm      in  (*        0 >> size' >> alloc(size') >> size >> alloc(size) >> .. *)       
                    let vm    = ADD                     =>> vm      in  (*             size' >> alloc(size') >> size >> alloc(size) >> .. *)    
                    let vm    = SWAP1                   =>> vm      in  (*             alloc(size') >> size' >> size >> alloc(size) >> .. *)     
                    le, vm   (* POP                                                                    size' >> size >> alloc(size) >> .. *) 
                             (* ADD                                                                      size+size'  >> alloc(size) >> .. *) 
                             (* SWAP1                                                                    alloc(size) >> size+size'  >> .. *)
                    
and codegen_log _ args ev ly le vm =
    let visible, args= split_ev_args ev args                        in
    let vm      = push_args le ly visible                   vm      in
    let vm      = push_evnt_hash  ev                        vm      in
    let le,vm   = mstore_tms args                   R ly le vm      in  (* stack : [..., size, offset] *)
    let n       = L.length visible + 1                              in
    let vm      = log n                                 =>> vm      in  (* deindexee N in logN *)
    le, vm

(***************************************)
(***     5. CODEGEN RETURN           ***)
(***************************************)

and push_args le stor               = foldr (fun arg vm -> arg >> (R,stor,le,vm))  
and sstore_words_to stor_locs vm    = foldl sstore_word_to vm stor_locs
and sstore_word_to  vm stor_loc     =
    let vm      =   PUSH32 (Int stor_loc)               =>> vm      in
                    SSTORE                              =>> vm      

and sstore_vars offst idx vars ly le vm = 
    let cn      =   lookup_cn idx                           vm      in 
    let varlocs =   var_locs_of_cn offst cn                         in
    assert(L.length varlocs=L.length vars) ; 
    let vm      =   push_args le ly vars vm                         in  (*                                         argk >> .. >> arg1 >> .. *)
    let vm      =   sstore_words_to varlocs vm                      in  (*  S[l_k]:=argk; .. ; S[l_1]:=arg1                              .. *)
    le,vm

and cont_call (TmCall(cn,args),_) ly le vm = 
    let idx     =   lookup_cnidx_at_vm cn                   vm      in 
    let offst   =   (ly.vars idx).offst                             in  
    let vm      =   Comment "Setting Cont"              =>> vm      in 
    let vm      =   set_PC idx                              vm      in  (*  S[PC] := rntime_offset_of_cn                                 .. *) 
                    sstore_vars offst idx args        ly le vm          (*  S[l_k]:= argk; .. ; S[l_1]:= arg1                            .. *)

(*       mstore_word ty 
 *
 *     BEFORE           AFTER               
 *
 *                   +---------+            
 *                   |alloc(32)|            
 *   +---------+     +---------+            
 *   |  value  |     |    32   |            
 * --+---------+-- --+---------+--    *)

and mstore_word ty vm = assert (size_of_ty ty <= 32)  ;                 (*                                   val >> .. *)   (* Here, FUN TYPE is excluded <- Problem #TODO *) 
    let vm      = PUSH1(Int 32)                         =>> vm      in  (*                             32 >> val >> .. *)
    let vm      = DUP1                                  =>> vm      in  (*                       32 >> 32 >> val >> .. *)
    let vm      = malloc                                    vm      in  (*                alloc(32) >> 32 >> val >> .. *)
    let vm      = SWAP2                                 =>> vm      in  (*                val >> 32 >> alloc(32) >> .. *)
    let vm      = DUP3                                  =>> vm      in  (*   alloc(32) >> val >> 32 >> alloc(32) >> .. *)
    let vm      = MSTORE                                =>> vm      in  (* M[alloc(32)]:=val     32 >> alloc(32) >> .. *)
                  SWAP1                                 =>> vm          (*                       alloc(32) >> 32 >> .. *)

and mstore_tm (tm,ty) aln ly le vm =
    let vm      = (tm,ty)                      >>(aln,ly,le,vm)     in  (*                                    tm >> .. *)
    let vm      = mstore_word ty vm                                 in  (* M[alloc(32)]:=tm      alloc(32) >> 32 >> .. *)
    le,vm

and codegen_return ret cont ly le vm =
    let vm      =   Comment "BEGIN RETURN"              =>> vm      in 
    let le,vm   =   cont_call cont                    ly le vm      in
    let vm      =   match ret with
    | TmUnit,_  ->  Comment"END RETURN" =>> ( STOP  =>> vm )       
    | tm        ->  
    let le,vm   =   mstore_tm tm                    R ly le vm      in
                    RETURN                              =>> vm      in 
    let vm      =   Comment "END RETURN"                =>> vm      in 
    le,vm


(********************************************)
(***     5. CODEGEN CONTRACT             ***)
(********************************************)

let codegen_mthds ly    = ($$$) foldl codegen_mthd ly

let codegen_cntrct ly le vm (idx,TmCn(id,flds,mthds)) =
    let label   = fresh_label()                                     in 
    let ()      = register_entry (Cntrct idx) label                 in 
    let vm    = JUMPDEST label                          =>> vm      in     
    let vm      = init_malloc                               vm      in  (* M[0x40]:=0x60                                  *)                                  
    let le,vm   = dispatcher idx(TmCn(id,flds,mthds))   le  vm      in  (*                                                *)
    let le,vm   = codegen_mthds ly idx (le,vm) mthds                in  
    vm

(***************************************)
(***     6.    RUNTIME               ***)
(***************************************)

type rntime             =   { rn_vm         : vm                                                       
                            ; rn_cns_pos    : int ilist  }

let init_rntime lookup_cn lyts =
    let vm      =   empty_vm lookup_cn lyts                             in
    let vm      =   error_loop                                  vm      in 
    let vm      =   get_PC                                      vm      in
    let vm      =   JUMP                                    =>> vm      in
    { rn_vm             = vm
    ; rn_cns_pos        = [] }

let append_rntime ly rc (idx,cn)   = (* #DEBUG pe("compiling contract" ^ str_of_int idx); *)
    { rn_vm             = codegen_cntrct ly(rntime_init_le cn)rc.rn_vm(idx,cn)
    ; rn_cns_pos        = insert idx(code_len rc.rn_vm)rc.rn_cns_pos    }

let compile_rntime ly cns   = 
    let init_rc             = init_rntime (lookup_cnidx cns) cns            in 
    foldl (append_rntime ly) init_rc cns

(********************************************)
(***     7. MAKE BYTECODE                ***)
(********************************************)


let cr_infos_of_crs =   map (fun cr -> 
                        { cr_size           = size_of_prog (prog_of_cr cr)
                        ; var_size          = size_of_vars_in_cn cr.cr_cn                                  
                        ; arr_size          = len  (arrTys_of_cn cr.cr_cn)                             
                        ; fld_types         = fldTys_of_cn       cr.cr_cn  } )
                                                                            
let offsts_of_sizes init sizes =
    let rec loop offsts current    = function 
        | []            ->  L.rev offsts
        | size::rest    ->  loop(current::offsts)(current+size)rest   in 
    loop [] init sizes

let rn_info_of_rn rn crs : rntime_info =
    let crs_sizes   =   sizes_of_crs crs                              in
    let crs_offsts  =   offsts_of_sizes(code_len rn.rn_vm)crs_sizes   in
    let crs_size    =   BL.sum crs_sizes                              in 
                        { rn_size           = crs_size + code_len rn.rn_vm
                        ; cns_pos           = rn.rn_cns_pos
                        ; crs_pos           = to_ilist crs_offsts 
                        ; crs_sizes         = to_ilist crs_sizes    }

(*  Since codes are stored in reverse order, their concat is also reserved. *)
let compose_bytecode crs rc idx : big_int Evm.program =
    let cr_infos    =   cr_infos_of_crs   crs                         in
    let rn_info     =    rn_info_of_rn rc crs                         in
    let layt        =   init_layout cr_infos rn_info                  in
    let cr          =   lookup idx crs                                in
    let imm_cr      =   realize_prog layt idx(prog_of_cr cr)          in
    let cns_prog    =   prog_of_crs crs                               in 
    let rn_prog     =   extract_prog rc.rn_vm                         in
    let imm_rntime  =   realize_prog layt idx(cns_prog @ rn_prog)     in
    imm_rntime @ imm_cr     (* the code is stored in the reverse order *)
