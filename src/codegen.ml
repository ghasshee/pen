
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

(***************************************)
(***     1.  DISPATHER               ***)
(***************************************)

let dispatch_mthd idx le vm m =                                           (*                                           ABCD >> .. *)  
    let vm      =   DUP1                                    @>> vm      in  (*                                   ABCD >> ABCD >> .. *)
    let vm      =   push_mthd_hash m                            vm      in  (*                              m >> ABCD >> ABCD >> .. *)
    let vm      =   EQ                                      @>> vm      in  (*                             m=ABCD?1:0 >> ABCD >> .. *)
    let vm      =   PUSH(RnMthdLabel(idx,m))                @>> vm      in  (*                Rntime(m) >> m=ABCD?1:0 >> ABCD >> .. *)
                    JUMPI                                   @>> vm          (* if m=ABCD then GOTO Rntime(m)             ABCD >> .. *)

let dispatch_dflt idx le vm   =
    let vm      =   PUSH(RnMthdLabel(idx,TyDefault))        @>> vm      in
                    JUMP                                    @>> vm     

let dispatcher idx (TmCn(_,_,mthds)) le vm = 
    let tyMthds =   L.map(function TmMthd(hd,_) -> hd)mthds             in
    let uMthds  =   filter_method tyMthds                               in 
    let vm      =   Comment "BEGIN Method Dispatchers "     @>> vm      in 
    let vm      =   PUSH (Int 0x00)                         @>> vm      in  (* get inputdata[0x00] *) 
    let vm      =   CALLDATALOAD                            @>> vm      in  (*               ABCDxxxxxxxxxxxxxxxxxxxxxxxxxxxxx >> .. *)
    let vm      =   shiftR Crpt.(word_bits-sig_bits)            vm      in  (*                                            ABCD >> .. *)                             
    let vm      =   foldl(dispatch_mthd idx le)vm uMthds                in  (* JUMP to Method ABCD                                   *)   
    let vm      =   POP                                     @>> vm      in  (*                                                    .. *)
    let vm      =   if  default_exists tyMthds
                        then dispatch_dflt idx               le vm          (* JUMP to Default Method                             .. *) 
                        else throw vm                                   in  (* JUMP to error                                      .. *) 
    let vm      =   Comment "END Method Dispatchers "       @>> vm      in 
    le,vm

(*********************************************)
(***     2.    CODEGEN  PRECONTRACT        ***)
(*********************************************)

let rec codegen_pre_call id args rety aln ly le vm = match id with 
    | "pre_ecdsarecover"    ->  assert(aln=R);      codegen_ECDSArecover args   ly le vm     
    | "keccak256"           ->  assert(aln=R);      codegen_keccak256 args      ly le vm       
    | "iszero"              ->  assert(rety=TyBool);codegen_iszero args aln     ly le vm 
    | _                     ->  err "codegen_predef_call: Direct Contract Call is Not supported. Specify a Method."

and codegen_iszero [arg] aln ly le vm =
    let vm      =   arg                   @> (aln,ly,le,vm)     in
                    ISZERO                          @>> vm 
       
and codegen_keccak256 args   ly le vm =
    let vm      =   get_malloc vm                               in  
    let vm      =   mstore_mthd_args L args       ly le vm      in  
    let vm      =   SWAP1                           @>> vm      in  
                    SHA3                            @>> vm                

and codegen_ECDSArecover args ly le vm = match args with [h;v;r;s] ->  
    let vm      =   PUSH (Int 0x20)                 @>> vm      in  
    let vm      =   DUP1                            @>> vm      in  
    let vm      =   malloc                              vm      in  
    let vm      =   repeat DUP2 2                       vm      in  
    let vm      =   get_malloc                          vm      in
    let vm      =   mstore_mthd_args R args       ly le vm      in  
    let vm      =   SWAP1                           @>> vm      in  
    let vm      =   PUSH (Int 0)                    @>> vm      in  
    let vm      =   PUSH (Int 1)                    @>> vm      in  
    let vm      =   PUSH (Int 10000)                @>> vm      in  
    let vm      =   CALL                            @>> vm      in  
    let vm      =   throw_if_0 vm                               in
    let vm      =   POP                             @>> vm      in  
    let vm      =   SWAP1                           @>> vm      in  
    let vm      =   POP                             @>> vm      in  
                    MLOAD                           @>> vm                    (* stack: [output] *)
    | _             -> err "pre_ecdsarecover has a wrong number of args"

(*********************************************)
(***    3.    CODEGEN  TERM                ***)
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

and mstore_new_instance id args msg ly le vm  =
    let cnidx   =   lookup_cnidx_at_vm id                   vm      in 
    let vm      =   PUSH(CrSize     cnidx)              @>> vm      in  (*                                                        size >> .. *) 
    let vm      =   PUSH(RnCrOffset cnidx)              @>> vm      in  (*                                             cn_idx  >> size >> .. *)
    let vm      =   mstore_code                             vm      in  (*                                         alloc(size) >> size >> .. *)
    let vm      =   SWAP1                               @>> vm      in  (*                                         size >> alloc(size) >> .. *)
    let vm      =   mstore_whole_code                       vm      in  (*                alloc(wsize) >> wsize >> size >> alloc(size) >> .. *)
    let vm      =   mstore_mthd_args R args           ly le vm      in  (*    argssize >> alloc(wsize) >> wsize >> size >> alloc(size) >> .. *)
    let vm      =   SWAP1                               @>> vm      in  (*    alloc(wsize) >> argssize >> wsize >> size >> alloc(size) >> .. *)
    let vm      =   POP                                 @>> vm      in  (*                    argssize >> wsize >> size >> alloc(size) >> .. *)
    let vm      =   ADD                                 @>> vm      in  (*                       argssize+wsize >> size >> alloc(size) >> .. *)
    let vm      =   ADD                                 @>> vm      in  (*                          argssize+wsize+size >> alloc(size) >> .. *)
                    SWAP1                               @>> vm          (*                                  alloc(size) >>   totalsize >> .. *)

and codegen_new id args msg ly le vm   =    
    let vm      =   reset_PC vm                                     in  (*                                             PCbkp >> .. *)
    let vm      =   mstore_new_instance id args msg   ly le vm      in  (*                      alloc(size) >> size >> PCbkp >> .. *)
    let vm      =   msg                         @> (R,ly,le,vm)     in  (*             value >> alloc(size) >> size >> PCbkp >> .. *)
    let vm      =   CREATE                              @>> vm      in  (*                             createResult >> PCbkp >> .. *)
    let vm      =   throw_if_0                              vm      in  (*                             createResult >> PCbkp >> .. *)
    let vm      =   SWAP1                               @>> vm      in  (*                             PCbkp >> CreateResult >> .. *)
                    restore_PC                              vm          (*                                      CreateResult >> .. *)

and kec_of_arr aid aidx ly le vm  =                                     (* kec(a_i) := the seed of array *) 
    let vm      =   aidx                        @> (R,ly,le,vm)     in  (*                                             index >> .. *)    
    let vm      =   aid                         @> (R,ly,le,vm)     in  (*                                array_loc >> index >> .. *)
                    keccak_cat                              vm          (*                            sha3(array_loc++index) >> .. *) 
      
and codegen_arr a i ly le vm    =                                       (*                                                      .. *)
    let vm      =   kec_of_arr a i                    ly le vm      in  (*                                      keccak(a[i]) >> .. *)
                    SLOAD                               @>> vm          (*                                   S[keccak(a[i])] >> .. *) 

and codegen_aid (Stor data) le vm = 
    let vm      =   PUSH data.offst                     @>> vm      in 
    let vm      =   DUP1                                @>> vm      in 
    let vm      =   SLOAD                               @>> vm      in 
                    salloc_aid                              vm 

and codegen_secondary_arr a i ly le vm = 
    let vm      =   kec_of_arr a i                    ly le vm      in (*                   kec(a_i) >> .. *)
    let vm      =   DUP1                                @>> vm      in (*            kec(a_i) >> kec(a_i) >> .. *)
    let vm      =   SLOAD                               @>> vm      in (*           S[kec(a_i) >> kec(a_i) >> .. *)
                    salloc_aid                              vm 

and salloc_aid vm = 
    let exit    =   fresh_label ()                                  in
    let label   =   fresh_label ()                                  in (*                                 S[loc] >> loc >> .. *)
    let vm      =   DUP1                                @>> vm      in (*                       S[loc] >> S[loc] >> loc >> .. *)
    let vm      =   if_GOTO label                           vm      in (* IF S[loc] != 0 GOTO label       S[loc] >> loc >> .. *) 
    let vm      =   POP                                 @>> vm      in (*                                           loc >> .. *)
    let vm      =   sincr 1 1                               vm      in (*                                S[AC]++ >> loc >> .. *)
    let vm      =   DUP1                                @>> vm      in (*                     new_aid >> new_aid >> loc >> .. *)         
    let vm      =   SWAP2                               @>> vm      in (*                     loc >> new_aid >> new_aid >> .. *)
    let vm      =   SSTORE                              @>> vm      in (* S[loc] := new_aid                     new_aid >> .. *)
    let vm      =   goto exit                               vm      in (*                                       new_aid >> .. *)
    let vm      =   JUMPDEST label                      @>> vm      in (*                                 S[loc] >> loc >> .. *)
    let vm      =   SWAP1                               @>> vm      in (*                                 loc >> S[loc] >> .. *)
    let vm      =   POP                                 @>> vm      in (*                                        S[loc] >> .. *)
                    JUMPDEST exit                       @>> vm         (*                                        S[loc] >> .. *)

(* le is not updated here.  
 * le can only be updated in a variable initialization *)
and (@>) e (aln,ly,le,vm)           = codegen_tm ly le vm aln e 
and codegen_tm ly le vm aln e       = (* #DEBUG pe_tm e; pe(str_of_ctx le); *)
    match e with 
    | TmApp(t1,t2)          ,_              ->                  codegen_app     (TmApp(t1,t2))            ly le vm 
    | TmAbs(x,tyX,t)        ,_              ->                  codegen_abs     (TmAbs(x,tyX,t))          ly le vm 
    | TmFix(f,n,ty,t)       ,_              ->                  codegen_fix     (TmFix(f,n,ty,t))         ly le vm 
    | TmI(i,n)              ,_              ->                  codegen_idx     (TmI(i,n))                ly le vm 
    | TmIStrct(i)           ,_              ->                  codegen_istrct  (TmIStrct(i))             ly le vm 
    | TmIf(b,t1,t2)         ,_              ->                  codegen_if      (TmIf(b,t1,t2))           ly le vm 
    | Balanc   e            ,_              ->                  BALANCE     @>>          e          @> (R,ly,le,vm)
    | EpValue               ,_              ->                  CALLVALUE                                   @>> vm   (* Value (wei) Transferred to the account *) 
    | TmZero                ,_              ->                  PUSH(Int 0)                                 @>> vm 
    | EpNow                 ,_              ->                  TIMESTAMP                                   @>> vm 
    | TmFalse               ,_              ->  assert(aln=R);  PUSH(Int 0)                                 @>> vm  
    | TmTrue                ,_              ->  assert(aln=R);  PUSH(Int 1)                                 @>> vm 
    | TmU256   d            ,_              ->  assert(aln=R);  PUSH(Big d)                                 @>> vm  
    | TmU8     d            ,_              ->  assert(aln=R);  PUSH(Big d)                                 @>> vm  
    | TmAdd  (l,r)          ,_              ->                  codegen_op ADD l r                        ly le vm             
    | TmSub  (l,r)          ,_              ->                  codegen_op SUB l r                        ly le vm             
    | TmMul  (l,r)          ,_              ->                  codegen_op MUL l r                        ly le vm             
    | TmLT   (l,r)          ,_              ->  assert(aln=R);  codegen_op LT  l r                        ly le vm           
    | TmGT   (l,r)          ,_              ->  assert(aln=R);  codegen_op GT  l r                        ly le vm           
    | TmEQ   (l,r)          ,_              ->  assert(aln=R);  codegen_op EQ  l r                        ly le vm           
    | TmNEQ  (l,r)          ,_              ->  assert(aln=R);  ISZERO  @>>     codegen_op EQ l r         ly le vm
    | TmNOT    e            ,_              ->  assert(aln=R);  ISZERO  @>>               e       @> (aln,ly,le,vm)  
    | TmLAND (l,r)          ,_              ->                  checked_codegen_LAnd l r aln              ly le vm 
    | TmCall(id,args)       ,rety           ->                  codegen_pre_call id args rety aln         ly le vm
    | TmSend((e,TyAddr  ),m,ags,msg),_      ->  assert(aln=R);  codegen_send_eoa(e,TyAddr)         msg    ly le vm 
    | TmSend((c,TyIstc n),m,ags,msg),_      ->  assert(aln=R);  codegen_send_cn(c,TyIstc n) m ags  msg    ly le vm 
    | TmNew(id,args,msg)    ,TyIstc _       ->  assert(aln=R);  codegen_new    id args msg                ly le vm 
    | TmAddr(c,TyIstc i)    ,TyAddr         ->                  (c,TyIstc i)                      @> (aln,ly,le,vm) 
    | TmSender              ,TyAddr         ->                  shift_by_aln aln TyAddr    (CALLER          @>> vm) 
    | TmThis                ,_              ->                  shift_by_aln aln TyAddr    (ADDRESS         @>> vm) 
    | TmArr(a,i) (*a[i][j]*),TyMap _        ->  assert(aln=R);  codegen_secondary_arr a i                 ly le vm     
    | TmArr(ai,j)(*a[i][j]*),      _        ->  assert(aln=R);  codegen_arr ai j                          ly le vm     
    | TmId id               ,TyMap(a,b)     ->                  codegen_aid (lookup_le id le)                le vm 
    | TmId id               ,ty             ->                  push_loc (lookup_le id le) aln ty               vm     
    | TmDeref(ref,tyR)      ,ty             ->  assert(aln=R);  assert(size_of_ty ty<=32 && tyR=TyRef ty); 
                                                                MLOAD   @>> (ref,tyR)               @> (R,ly,le,vm) 
    | e                                     ->  let _,vm    =   codegen_tm_eff e              aln         ly le vm  in 
                                                                PUSH (Int 0)                                @>> vm 

and codegen_tm_eff tm aln ly le vm      =   match tm with 
    | TmAbort               ,TyErr          ->  le, throw vm                               
    | TmLog(id,args,Some ev),TyUnit         ->  codegen_log id args ev  ly le vm     
    | TmSfDstr tm           ,TyUnit         ->  codegen_selfdstr  tm    ly le vm 
    | TmAssign(l,r)         ,TyUnit         ->  codegen_assign l r      ly le vm  
    | TmReturn(ret,cont)    ,_              ->  codegen_return ret cont ly le vm    
    | e                                     ->  pf "codegen_tm: %s " (str_of_tm e); raise Not_found
    
and codegen_op op l r ly le vm          =   op @>> l @> (R,ly,le, r @> (R,ly,le,vm))      
            
and push_msg_and_gas msg cn ly le vm = 
    let vm      =   msg                         @> (R,ly,le,vm)     in  (*                                            value >> .. *) 
    let vm      =   cn                          @> (R,ly,le,vm)     in  (*                                  cnAddr >> value >> .. *)
    let vm      =   PUSH(Int 3000)                      @>> vm      in  (*                          3000 >> cnAddr >> value >> .. *)
    let vm      =   GAS                                 @>> vm      in  (*                   gas >> 3000 >> cnAddr >> value >> .. *)
                    SUB                                 @>> vm          (*                      gas-3000 >> cnAddr >> value >> .. *)

and call_and_restore_PC vm =                                            (*  gas-3000 >> cnAddr >> msg >> &mhash >> argssize+4 >> retbegin >> retsize >> retbegin >> retsize >> PCbkp >> .. *)
    let vm      =   CALL                                @>> vm      in  (*                                                                   success >> retbegin >> retsize >> PCbkp >> .. *)
    let vm      =   PUSH(Int 0)                         @>> vm      in  (*                                                              0 >> success >> retbegin >> retsize >> PCbkp >> .. *)
    let vm      =   JUMPI                               @>> vm      in  (*  IF success==0 THEN GOTO 0                                                   retbegin >> retsize >> PCbkp >> .. *)
    let vm      =   SWAP2                               @>> vm      in  (*                                                                              PCbkp >> retsize >> retbegin >> .. *)
                    restore_PC                              vm          (*                                                                                       retsize >> retbegin >> .. *)

and mload_ret_value vm =                                                (*                                 retsize >> retbegin >> .. *)
    let vm      =   PUSH (Int 32)                       @>> vm      in  (*                           32 >> retsize >> retbegin >> .. *)
    let vm      =   throw_if_NEQ                            vm      in  (* IF 32!=retsize ERROR                       retbegin >> .. *)
                    MLOAD                               @>> vm          (*                                         M[retbegin] >> .. *)

and codegen_send_cn cn m args msg ly le vm =  (* msg-call to a contract *) 
    let TyIstc cnm = snd cn                                     in  
    let cnidx   =   lookup_cnidx_at_vm cnm                vm      in 
    let callee  =   lookup_cn cnidx                         vm      in
    let Some mnm = m                                              in 
    let m       =   lookup_mthd_head vm callee mnm                in
    let TyMthd(id,_,reTy) = m                                       in 
    let retsize =   size_of_ty reTy                                 in  (*                                                                                                              .. *)
    let vm      =   Comment("BEGINE send to "^id)       @>> vm      in  
    let vm      =   reset_PC                                vm      in  (*                                                                                                     PCbkp >> .. *)
    let vm      =   PUSH(Int retsize)                   @>> vm      in  (*                                                                                          retsize >> PCbkp >> .. *)
    let vm      =   DUP1                                @>> vm      in  (*                                                                               retsize >> retsize >> PCbkp >> .. *)
    let vm      =   malloc                                  vm      in  (*                                                                              retbegin >> retsize >> PCbkp >> .. *)
    let vm      =   repeat DUP2 2                           vm      in  (*                                                       retbegin >> retsize >> retbegin >> retsize >> PCbkp >> .. *)
    let vm      =   mstore_mhash_and_args m args      ly le vm      in  (*                               &mhash >> argssize+4 >> retbegin >> retsize >> retbegin >> retsize >> PCbkp >> .. *)
    let vm      =   push_msg_and_gas msg cn           ly le vm      in  (*  gas-3000 >> cnAddr >> msg >> &mhash >> argssize+4 >> retbegin >> retsize >> retbegin >> retsize >> PCbkp >> .. *)
    let vm      =   call_and_restore_PC                     vm      in  (*                                                                                       retsize >> retbegin >> .. *)
    let vm      =   mload_ret_value                         vm      in  (*                                                                                                       ret >> .. *)
                    Comment("END send to "^id)          @>> vm 

and codegen_send_eoa eoa msg ly le vm =   (* send value to an EOA *) 
    let vm      =   Comment "BEGINE send to Addr"       @>> vm      in  (*                                                                   .. *) 
    let vm      =   reset_PC                                vm      in  (*                                                          PCbkp >> .. *) 
    let vm      =   PUSH(Int 0)                         @>> vm      in  (*                                                     0 >> PCbkp >> .. *) 
    let vm      =   repeat DUP1 5                           vm      in  (*                            0 >> 0 >> 0 >> 0 >> 0 >> 0 >> PCbkp >> .. *) 
    let vm      =   push_msg_and_gas msg eoa          ly le vm      in  (* gas-3000 >> addr >> msg >> 0 >> 0 >> 0 >> 0 >> 0 >> 0 >> PCbkp >> .. *) 
    let vm      =   call_and_restore_PC                     vm      in  (*                                                         0 >> 0 >> .. *)
    let vm      =   POP                                 @>> vm      in  (*                                                              0 >> .. *)
                    Comment("END send to Addr")         @>> vm 

and sstore_to_lval(TmArr(a,i),_) ly le vm     =                         (*                                  rval >> .. *)
    let vm      =   kec_of_arr a i                    ly le vm      in  (*                      KEC(a^i) >> rval >> .. *)
                    SSTORE                              @>> vm          (* S[KEC(a^i)] := rval                      .. *)

and codegen_assign l r ly le vm = 
    let vm      =   Comment "BEGIN Assignment"          @>> vm      in 
    let vm      =   r                           @> (R,ly,le,vm)     in  (*                                    r >> .. *)
    let vm      =   sstore_to_lval l                  ly le vm      in  (* S[KEC(l)] := r                          .. *)  
    let vm      =   Comment "END Assignment"            @>> vm      in 
    le,vm 

and checked_codegen_LAnd l r aln ly le vm = 
    assert(aln=R);         
    let la      =   fresh_label ()                                  in  (*                                                        .. *)
    let vm      =   l                           @> (R,ly,le,vm)     in  (*                                                   l >> .. *)
    let vm      =   DUP1                                @>> vm      in  (*                                              l >> l >> .. *)
    let vm      =   if_0_GOTO la                            vm      in  (*                                                   l >> .. *)
    let vm      =   POP                                 @>> vm      in  (*                                                        .. *)
    let vm      =   r                           @> (R,ly,le,vm)     in  (*                                                   r >> .. *)
    let vm      = JUMPDEST la                           @>> vm      in  (*                                                   r >> .. *)
                    repeat ISZERO 2                         vm          (*                                                l&&r >> .. *)  

and mstore_mthd_args aln args ly le vm =
    let vm      =   PUSH(Int 0)                         @>> vm      in  (*                                                   0 >> .. *)
                    foldl (mstore_mthd_arg aln le ly) vm args           (*                                             sumsize >> .. *) 

and mstore_mthd_arg aln le ly vm arg  =
    let ty      =   get_ty arg                                      in  
    let i       =   match aln with | L -> size_of_ty ty 
                                   | R -> 32                        in  (*                                                 sum >> .. *)
    let vm      =   PUSH (Int i)                        @>> vm      in  (*                                         size >> sum >> .. *)
    let vm      =   arg                       @> (aln,ly,le,vm)     in  (*                                  arg >> size >> sum >> .. *)
    let vm      =   DUP2                                @>> vm      in  (*                          size >> arg >> size >> sum >> .. *)
    let vm      =   malloc                                  vm      in  (*                   alloc(size) >> arg >> size >> sum >> .. *)
    let vm      =   MSTORE                              @>> vm      in  (* M[alloc(size)] := arg                   size >> sum >> .. *)
                    ADD                                 @>> vm          (*                                            size+sum >> .. *)

and mstore_mhash_and_args mthd args ly le vm =                        (*                                                        .. *)
    let vm      =   mstore_mthd_hash mthd                   vm      in  (*                                         &mhash >> 4 >> .. *)
    let vm      =   mstore_mthd_args R args ly           le vm      in  (*                              argsize >> &mhash >> 4 >> .. *)
    let vm      =   SWAP1                               @>> vm      in  (*                              &mhash >> argsize >> 4 >> .. *)
    let vm      =   SWAP2                               @>> vm      in  (*                              4 >> argsize >> &mhash >> .. *)
    let vm      =   ADD                                 @>> vm      in  (*                                 argsize+4 >> &mhash >> .. *)
                    SWAP1                               @>> vm          (*                                 &mhash >> argsize+4 >> .. *)

and codegen_mthd_argLen_chk m vm = match m with  
    | TyDefault     -> vm
    | TyMthd _      ->
    let vm      =   PUSH(Int(calldatasize m))           @>> vm      in
    let vm      =   CALLDATASIZE                        @>> vm      in
                    throw_if_NEQ                            vm        

and escape_ARG arg retlabel a       ly le vm = 
    let vm      =   Comment "ESCAPE START"              @>> vm      in 
    let vm      =   PUSH(Label retlabel)                @>> vm      in     
    let vm      =   arg                         @> (a,ly,le,vm)     in 
    let vm      =   ePUSH                                   vm      in
                    Comment "ESCAPE DONE"               @>> vm 

and codegen_app_rec(TmApp((TmIRec(i),_),arg)) a ly le vm = 
    let vm      =   Comment "BEGIN APP-REC"             @>> vm      in
    let retaddr =   fresh_label ()                                  in 
    let start   =   lookup_recursion_param le                       in 
    let vm      =   escape_ARG arg retaddr          a ly le vm      in 
    let vm      =   goto start                              vm      in 
    let vm      =   JUMPDEST retaddr                    @>> vm      in       
                    Comment "END APP-REC"               @>> vm 

and codegen_fix(TmFix(phi,n,ty,tm)) ly le vm = 
    let vm      =   Comment "BEGIN FIX"                 @>> vm      in 
    let start   =   fresh_label ()                                  in 
    (* #DEBUG pf "! add_recursion_param(label%d)\n" start;     *)
    let le      =   add_recursion_param le start                    in
    let vm      =   JUMPDEST start                      @>> vm      in 
    let vm      =   tm                          @> (R,ly,le,vm)     in  (*                               tm >> .. *)
    let vm      =   ePOP                                    vm      in  (*                    retAddr >> tm >> .. *) 
    let vm      =   JUMP                                @>> vm      in  (* GOTO retAddr                  tm >> .. *)
                    Comment "END FIX"                   @>> vm      
       

and codegen_istrct (TmIStrct(i))    ly le vm = 
    let vm      =   Comment "BEGIN Struct Parameter"    @>> vm      in
    let vm      =   get_escaped_arg                         vm      in 
                    Comment "END Struct Parameter"      @>> vm

and codegen_app (TmApp(t1,t2)) ly le vm = match fst t1 with 
    | TmI(i,n) ->   let t1 = lookup_brjidx i le                     in 
                    codegen_app     (TmApp(t1,t2))    ly le vm 
    | TmIRec(i)->   codegen_app_rec (TmApp(t1,t2))  R ly le vm 
    | TmFix(f,n,ty,tm)-> 
    let ret     =   fresh_label ()                                  in 
    let vm      =   escape_ARG t2 ret               R ly le vm      in 
    let vm      =   codegen_fix (TmFix(f,n,ty,tm))    ly le vm      in
                    JUMPDEST ret                        @>> vm 
    | _ -> 
    (* #DEBUG pf "! add_brjidx %s\n" (str_of_tm t2); *)
    let le      =   add_brjidx le t2                                in       
                    t1                          @> (R,ly,le,vm)      

and codegen_abs (TmAbs(x,tyX,t))    ly le vm = 
    let vm      =   t                           @> (R,ly,le,vm)     in
    vm 

and codegen_idx (TmI(i,n))          ly le vm = 
    let tm      =   lookup_brjidx i le                              in 
                    tm                          @> (R,ly,le,vm)       
       

and codegen_if (TmIf(b,t1,t2))      ly le vm = 
    let elif    =   fresh_label()                                   in 
    let fi      =   fresh_label()                                   in
    let vm      =   Comment "IF"                        @>> vm      in 
    let vm      =   b                           @> (R,ly,le,vm)     in 
    let vm      =   if_0_GOTO elif                          vm      in 
    let vm      =   Comment "THEN"                      @>> vm      in 
    let vm      =   t1                          @> (R,ly,le,vm)     in
    let vm      =   goto fi                                 vm      in 
    let vm      =   Comment "ELSE"                      @>> vm      in 
    let vm      =   JUMPDEST elif                       @>> vm      in 
    let vm      =   t2                          @> (R,ly,le,vm)     in 
    let vm      =   Comment "FI"                        @>> vm      in 
    let vm      =   JUMPDEST fi                         @>> vm      in 
    vm

and codegen_mthd ly cnidx (le,vm) (TmMthd(hd,bd))  =
    let vm      =   Comment ("BEGIN " ^str_of_ty hd)    @>> vm      in  
    let label   =   fresh_label()                                   in 
    register_entry (Mthd(cnidx,hd)) label; 
    let le      =   add_mthdCallerArgLocs(TmMthd(hd,bd))(add_empty_ctx (add_empty_brj le))    in
    let vm      = JUMPDEST label                        @>> vm      in 
    let vm      =   codegen_mthd_argLen_chk hd              vm      in
    let vm      =   bd                          @> (R,ly,le,vm)     in
    let vm      =   Comment ("END "^str_of_ty hd)       @>> vm      in  
    le,vm

and codegen_selfdstr tm ly le vm =    
    let vm      = tm                            @> (R,ly,le,vm)     in
    let vm      = SELFDESTRUCT                          @>> vm      in
    le, vm

and mstore_tms l aln ly le vm = match l with  
    | []        ->  le,         repeat (PUSH(Int 0)) 2      vm      
    | tm::tms   ->  let le,vm = mstore_tm tm aln      ly le vm      in  (*                                      alloc(size) >> size >> .. *)
                    let vm    = SWAP1                   @>> vm      in  (*                                      size >> alloc(size) >> .. *)
                    let le,vm = mstore_tms tms aln    ly le vm      in  (*   0 >> 0 >> size' >> alloc(size') >> size >> alloc(size) >> .. *)
                    let vm    = POP                     @>> vm      in  (*        0 >> size' >> alloc(size') >> size >> alloc(size) >> .. *)       
                    let vm    = ADD                     @>> vm      in  (*             size' >> alloc(size') >> size >> alloc(size) >> .. *)    
                    let vm    = SWAP1                   @>> vm      in  (*             alloc(size') >> size' >> size >> alloc(size) >> .. *)     
                    le, vm   (* POP                                                                    size' >> size >> alloc(size) >> .. *) 
                             (* ADD                                                                      size+size'  >> alloc(size) >> .. *) 
                             (* SWAP1                                                                    alloc(size) >> size+size'  >> .. *)
                    
and codegen_log _ args ev ly le vm =
    let visible, args= split_ev_args ev args                        in
    let vm      =   push_args le ly visible                 vm      in
    let vm      =   push_evnt_hash  ev                      vm      in
    let le,vm   =   mstore_tms args                 R ly le vm      in  (* stack : [..., size, offset] *)
    let n       =   L.length visible + 1                            in
    let vm      =   log n                               @>> vm      in  (* deindexee N in logN *)
    le, vm

(***************************************)
(***     4. CODEGEN RETURN           ***)
(***************************************)

and push_args le sto                = foldr (fun arg vm -> arg @> (R,sto,le,vm))  
and sstore_words_to sto_locs vm     = foldl sstore_word_to vm sto_locs
and sstore_word_to  vm sto_loc      =
    let vm      =   PUSH (Int sto_loc)                  @>> vm      in
                    SSTORE                              @>> vm      

and sstore_vars offst idx vars sto le vm = 
    let cn      =   lookup_cn idx                           vm      in 
    let varlocs =   var_locs_of_cn offst cn                         in
    assert(L.length varlocs=L.length vars) ; 
    let vm      =   push_args le sto vars vm                        in  (*                                         argk >> .. >> arg1 >> .. *)
    let vm      =   sstore_words_to varlocs vm                      in  (*  S[l_k]:=argk; .. ; S[l_1]:=arg1                              .. *)
    le,vm

and cont_call (TmCall(cn,args),_) sto le vm = 
    let idx     =   lookup_cnidx_at_vm cn                   vm      in 
    let offst   =   (sto.vars idx).offst                            in  
    let vm      =   Comment "Setting Cont"              @>> vm      in 
    let vm      =   set_PC idx                              vm      in  (*  S[PC] := rntime_offset_of_cn                                 .. *) 
                    sstore_vars offst idx args       sto le vm          (*  S[l_k]:= argk; .. ; S[l_1]:= arg1                            .. *)

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
    let vm      =   PUSH(Int 32)                        @>> vm      in  (*                             32 >> val >> .. *)
    let vm      =   DUP1                                @>> vm      in  (*                       32 >> 32 >> val >> .. *)
    let vm      =   malloc                                  vm      in  (*                alloc(32) >> 32 >> val >> .. *)
    let vm      =   SWAP2                               @>> vm      in  (*                val >> 32 >> alloc(32) >> .. *)
    let vm      =   DUP3                                @>> vm      in  (*   alloc(32) >> val >> 32 >> alloc(32) >> .. *)
    let vm      =   MSTORE                              @>> vm      in  (* M[alloc(32)]:=val     32 >> alloc(32) >> .. *)
                    SWAP1                               @>> vm          (*                       alloc(32) >> 32 >> .. *)

and mstore_tm (tm,ty) aln sto le vm =
    let vm      = (tm,ty)                    @> (aln,sto,le,vm)     in  (*                                    tm >> .. *)
    let vm      = mstore_word ty vm                                 in  (* M[alloc(32)]:=tm      alloc(32) >> 32 >> .. *)
    le,vm

and codegen_return ret cont sto le vm =
    let vm      =   Comment "BEGIN RETURN"              @>> vm      in 
    let le,vm   =   cont_call cont                   sto le vm      in
    let vm      =   (match ret with
    | TmUnit,_  ->  STOP                                @>> vm        
    | tm        ->  
    let le,vm   =   mstore_tm tm                   R sto le vm      in
                    RETURN                              @>> vm  )   in 
    let vm      =   Comment "END RETURN"                @>> vm      in 
    le,vm


(********************************************)
(***     5. CODEGEN CONTRACT             ***)
(********************************************)

let codegen_mthds sto    = ($$$) foldl codegen_mthd sto

let codegen_cntrct sto le vm (idx,TmCn(id,flds,mthds)) =
    let label   =   fresh_label()                                     in 
    let ()      =   register_entry (Cntrct idx) label                 in 
    let vm      = JUMPDEST label                          @>> vm      in     
    let vm      =   init_malloc                               vm      in  (* M[0x40]:=0x60                                  *)                                  
    let le,vm   =   dispatcher idx(TmCn(id,flds,mthds))   le  vm      in  (*                                                *)
    let le,vm   =   codegen_mthds sto idx (le,vm) mthds               in  
    vm

(***************************************)
(***     6.    RUNTIME               ***)
(***************************************)

type rntime             =   { rn_vm         : vm                                                       
                            ; rn_cns_pos    : int ilist  }

let init_rntime lookup_cn cns =
    let vm      =   empty_vm lookup_cn cns                      in
    let vm      =   error_loop                          vm      in 
    let vm      =   get_PC                              vm      in
    let vm      =   JUMP                            @>> vm      in
    { rn_vm         = vm
    ; rn_cns_pos    = [] }

let append_rntime sto rc (idx,cn)   = (* #DEBUG pe("compiling contract" ^ str_of_int idx); *)
    { rn_vm         = codegen_cntrct sto (rn_init_ctx cn) rc.rn_vm (idx,cn)
    ; rn_cns_pos    = insert idx (code_len rc.rn_vm) rc.rn_cns_pos    }

let compile_rntime sto cns   = 
    let rn      =   init_rntime (lookup_cnidx cns) cns          in 
                    foldl (append_rntime sto)  rn  cns

(*****************************************)
(***      7.   CREATION                ***)
(*****************************************)

let mstore_rn_code idx vm =                                         (*                                                           .. *)
    let vm      =   PUSH(RnSize)                    @>> vm      in  (*                                                   size >> .. *)
    let vm      =   DUP1                            @>> vm      in  (*                                           size >> size >> .. *)  
    let vm      =   malloc                              vm      in  (*                                    alloc(size) >> size >> .. *)
    let vm      =   DUP2                            @>> vm      in  (*                            size >> alloc(size) >> size >> .. *)
    let vm      =   PUSH(CrSize idx)                @>> vm      in  (*                     idx >> size >> alloc(size) >> size >> .. *)
    let vm      =   DUP3                            @>> vm      in  (*      alloc(size) >> idx >> size >> alloc(size) >> size >> .. *)
                    CODECOPY                        @>> vm          (*                                    alloc(size) >> size >> .. *)
                                                                    (*                                     codebegin                *)
let codegen_creation cns idx = (* return vm which contains the program *) 
    let TmCn(id,_,_) as cn = lookup idx cns in 
    let vm      =   empty_vm (lookup_cnidx cns) cns             in  (*                                                                          *)
    let vm      =   Comment("Begin Creation "^id)   @>> vm      in 
    let vm      =   init_malloc                         vm      in  (* M[0x40] := 0x60                                                          *)
    let vm      =   salloc_arrs        cn               vm      in  (* S[1]    := #array                i << alloc(argssize) << argssize << ..  *)
    let vm      =   set_PC            idx               vm      in  (* S[PC]   := rn_cn_offst    (returned body)                                *)
    let vm      =   mstore_rn_code    idx               vm      in  (*                                 alloc(codesize) << codesize << i <<  ..  *)
    let vm      =   RETURN                          @>> vm      in  (* OUTPUT(M[code]) as The BODY code                               i <<  ..  *)
                    Comment("End Creation "^id)     @>> vm 

type creation           =   { cr_vm           : vm
                            ; cr_ty           : ty
                            ; cr_cn           : ty toplevel } 

let init_cr cns idx     =   let cn      =   L.assoc idx cns in 
                            { cr_vm           = codegen_creation cns idx
                            ; cr_ty           = typeof_cn cn 
                            ; cr_cn           = cn                                }

let init_crs cns        =   imap (init_cr cns) cns

(********************************************)
(***     7. MAKE BYTECODE                ***)
(********************************************)

let vm_of_cr cr         =   cr.cr_vm
let size_of_cr          =   code_len     $ vm_of_cr 
let prog_of_cr          =   extract_prog $ vm_of_cr  
let sizes_of_crs        =   L.map snd $ idx_sort $ map size_of_cr
let progs_of_crs        =   L.map snd $ idx_sort $ map prog_of_cr 
let prog_of_crs         =   L.concat  $ L.rev    $ progs_of_crs 

(* 
let rec offsts_of_sizes init = function 
    | []            ->  []
    | size::sizes   ->  init :: offsts_of_sizes (init+size) sizes  *)

(* tail-rec-opt + rev ver. *) 
let offsts_of_sizes init sizes = 
    let rec loop offsts current = function 
        | []                -> L.rev offsts
        | size::sizes       -> loop (current :: offsts) (size+current) sizes in 
    loop [] init sizes

let cr_infos_of_crs     =   map (fun cr -> 
                        { cr_size           = size_of_prog (prog_of_cr cr)
                        ; var_size          = size_of_vars_in_cn cr.cr_cn                                  
                        ; arr_size          = len  (arrTys_of_cn cr.cr_cn)                             
                        ; fld_types         = fldTys_of_cn       cr.cr_cn  } )
                                                                            
let rn_info_of_rn rn crs =
    let crs_sizes   =   sizes_of_crs crs                            in
    let crs_offsts  =   offsts_of_sizes(code_len rn.rn_vm)crs_sizes in
    let crs_size    =   BL.sum crs_sizes                            in 
                        { rn_size           = crs_size + code_len rn.rn_vm
                        ; cns_pos           = rn.rn_cns_pos
                        ; crs_pos           = to_ilist crs_offsts 
                        ; crs_sizes         = to_ilist crs_sizes    }

let compose_bytecode crs rn idx : big_int Evm.program =
    let rn_info     =   rn_info_of_rn rn  crs                       in (* rn_size *)
    let cr_infos    =   cr_infos_of_crs   crs                       in 
    let layt        =   init_layout cr_infos rn_info                in
    let cr          =   lookup idx crs                              in
    let cr_prog     =   prog_of_cr cr                               in
    let crs_prog    =   prog_of_crs crs                             in 
    let rn_prog     =   extract_prog rn.rn_vm                       in
    let cr_code     =   realize_prog layt cr_prog                   in
    let crs_code    =   realize_prog layt crs_prog                  in 
    let rn_code     =   realize_prog layt rn_prog                   in 
    crs_code @ rn_code @ cr_code  
    (* the generated  OPCODE list is stored in reverse order *)
