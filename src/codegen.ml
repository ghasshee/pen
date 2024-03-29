(* M[0x40]  :=   the address of mem alloc     *) 
(* MSTORE   :=   x=pop() ; y=pop() ; M[x]=y   *) 
(* MLOAD    :=   x=pop() ; push M[x]          *) 
(* CODECOPY  to from len :=  M[to .. to+len-1] = I_b[from .. from+len-1]  *)

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

let dispatch_mthd idx le vm m =                                             (*                                           ABCD >> .. *)  
    let vm      =   DUP1                                    @>> vm      in  (*                                   ABCD >> ABCD >> .. *)
    let vm      =   _PUSH_MHASH m                               vm      in  (*                              m >> ABCD >> ABCD >> .. *)
    let vm      =   EQ                                      @>> vm      in  (*                             m=ABCD?1:0 >> ABCD >> .. *)
    let vm      =   PUSH(RnMthdLabel(idx,m))                @>> vm      in  (*                Rntime(m) >> m=ABCD?1:0 >> ABCD >> .. *)
                    JUMPI                                   @>> vm          (* if m=ABCD then GOTO Rntime(m)             ABCD >> .. *)

let dispatch_dflt idx le vm   =
    let vm      =   PUSH(RnMthdLabel(idx,TyDflt))           @>> vm      in
                    JUMP                                    @>> vm     

let dispatcher idx (TmCn(_,_,mthds)) le vm = 
    let tyMthds =   L.map(function TmMthd(hd,_) -> hd)mthds             in
    let uMthds  =   filter_method tyMthds                               in 
    let vm      =   Comment "BEGIN Method Dispatchers "     @>> vm      in 
    let vm      =   PUSH (Int 0x00)                         @>> vm      in  (* get inputdata[0x00] *) 
    let vm      =   CALLDATALOAD                            @>> vm      in  (*               ABCDxxxxxxxxxxxxxxxxxxxxxxxxxxxxx >> .. *)
    let vm      =   _SHR   Crpt.(word_bits - sig_bits)          vm      in  (*                                            ABCD >> .. *)                             
    let vm      =   foldl(dispatch_mthd idx le)vm uMthds                in  (* JUMP to Method ABCD                                   *)   
    let vm      =   POP                                     @>> vm      in  (*                                                    .. *)
    let vm      =   if  default_exists tyMthds
                        then dispatch_dflt idx               le vm          (* JUMP to Default Method                             .. *) 
                        else _THROW                             vm      in  (* JUMP to error                                      .. *) 
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
    let vm      =   _HEAPHEAD                              vm      in  
    let vm      =   _MSTORE_MARGS L args          ly le vm      in  
    let vm      =   SWAP1                           @>> vm      in  
                    SHA3                            @>> vm                

and codegen_ECDSArecover args ly le vm = match args with [h;v;r;s] ->  
    let vm      =   PUSH (Int 0x20)                 @>> vm      in  (* 0x20                                          *)
    let vm      =   _MALLOC                             vm      in  (* 0x20 >> malloc(0x20)                          *)
    let vm      =   repeat DUP2 2                       vm      in  (* 0x20 >> malloc(0x20) >> 0x20 >> malloc(0x20)  *)
    let vm      =   _HEAPHEAD                              vm      in  (* 0x20 >> malloc(0x20) >> 0x20 >> malloc(0x20) >> M[0x40] *) 
    let vm      =   _MSTORE_MARGS R args          ly le vm      in  
    let vm      =   SWAP1                           @>> vm      in  
    let vm      =   PUSH (Int 0)                    @>> vm      in  
    let vm      =   PUSH (Int 1)                    @>> vm      in  
    let vm      =   PUSH (Int 10000)                @>> vm      in  
    let vm      =   CALL                            @>> vm      in  
    let vm      =   _THROW_IFN vm                               in
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
    let cnidx   =   lookup_cnidx vm.cns id                          in 
    let vm      =   PUSH(CrSize     cnidx)              @>> vm      in  (*                                                        size >> .. *) 
    let vm      =   PUSH(RnCrOffset cnidx)              @>> vm      in  (*                                             cn_ofst >> size >> .. *)
    let vm      =   _MSTORE_CODE                            vm      in  (*                                         alloc(size) >> size >> .. *)
    let vm      =   SWAP1                               @>> vm      in  (*                                         size >> alloc(size) >> .. *)
    let vm      =   _MSTORE_WHOLECODE                       vm      in  (*                                wsize >> size >> alloc(size) >> .. *)
    let vm      =   _MSTORE_MARGS R args              ly le vm      in  (*                   argssize >>  wsize >> size >> alloc(size) >> .. *)
    let vm      =   ADD                                 @>> vm      in  (*                       argssize+wsize >> size >> alloc(size) >> .. *)
    let vm      =   ADD                                 @>> vm      in  (*                          argssize+wsize+size >> alloc(size) >> .. *)
                    SWAP1                               @>> vm          (*                                  alloc(size) >>   totalsize >> .. *)

and codegen_new id args msg ly le vm   =    
    let vm      =   _RESET_PC                               vm      in  (*                                             PCbkp >> .. *)
    let vm      =   mstore_new_instance id args msg   ly le vm      in  (*                      alloc(size) >> size >> PCbkp >> .. *)
    let vm      =   msg                         @> (R,ly,le,vm)     in  (*             value >> alloc(size) >> size >> PCbkp >> .. *)
    let vm      =   CREATE                              @>> vm      in  (*                             createResult >> PCbkp >> .. *)
    let vm      =   _THROW_IFN                              vm      in  (*                             createResult >> PCbkp >> .. *)
    let vm      =   SWAP1                               @>> vm      in  (*                             PCbkp >> CreateResult >> .. *)
                    _RESTORE_PC                             vm          (*                                      CreateResult >> .. *)

and _KEC_ARR aid aidx ly le vm  =                                       (* kec(a_i) := the seed of array *) 
    let vm      =   aidx                        @> (R,ly,le,vm)     in  (*                                             index >> .. *)    
    let vm      =   aid                         @> (R,ly,le,vm)     in  (*                                array_loc >> index >> .. *)
                    _KEC_CAT                                vm          (*                            sha3(array_loc++index) >> .. *) 
      
and codegen_arr a i ly le vm    =                                       (*                                                      .. *)
    let vm      =   _KEC_ARR a i                      ly le vm      in  (*                                      keccak(a[i]) >> .. *)
                    SLOAD                               @>> vm          (*                                   S[keccak(a[i])] >> .. *) 

and codegen_aid (Stor data) le vm = 
    let vm      =   PUSH data.offst                     @>> vm      in 
    let vm      =   DUP1                                @>> vm      in 
    let vm      =   SLOAD                               @>> vm      in 
                    _SALLOC_AID                             vm 

and codegen_secondary_arr a i ly le vm = 
    let vm      =   _KEC_ARR a i                      ly le vm      in  (*                         kec(a_i) >> .. *)
    let vm      =   DUP1                                @>> vm      in  (*             kec(a_i) >> kec(a_i) >> .. *)
    let vm      =   SLOAD                               @>> vm      in  (*          S[kec(a_i)] >> kec(a_i) >> .. *)
                    _SALLOC_AID                             vm          (*                         new_aid  >> .. *)

and _SALLOC_AID vm = 
    let exit    =   fresh_label ()                                  in
    let label   =   fresh_label ()                                  in  (*                                 S[loc] >> loc >> .. *)
    let vm      =   DUP1                                @>> vm      in  (*                       S[loc] >> S[loc] >> loc >> .. *)
    let vm      =   _GOTO_IF label                          vm      in  (* IF S[loc] != 0 GOTO label       S[loc] >> loc >> .. *) 
    let vm      =   POP                                 @>> vm      in  (*                                           loc >> .. *)
    let vm      =   _PLUS_S 1 1                             vm      in  (*                                S[AC]++ >> loc >> .. *)
    let vm      =   DUP1                                @>> vm      in  (*                     new_aid >> new_aid >> loc >> .. *)         
    let vm      =   SWAP2                               @>> vm      in  (*                     loc >> new_aid >> new_aid >> .. *)
    let vm      =   SSTORE                              @>> vm      in  (* S[loc] := new_aid                     new_aid >> .. *)
    let vm      =   _GOTO exit                              vm      in  (*                                       new_aid >> .. *)
    let vm      =   JUMPDEST label                      @>> vm      in  (*                                 S[loc] >> loc >> .. *)
    let vm      =   SWAP1                               @>> vm      in  (*                                 loc >> S[loc] >> .. *)
    let vm      =   POP                                 @>> vm      in  (*                                        S[loc] >> .. *)
                    JUMPDEST exit                       @>> vm          (*                                        S[loc] >> .. *)

(* le is not updated here.  
 * le can only be updated in a variable initialization *)
and (@>) e (aln,ly,le,vm)           = codegen_tm ly le vm aln e 
and codegen_tm ly le vm aln e       = (* #DEBUG pe_tm e; pe(str_of_ctx le); *)
    match e with 
    | EpValue               ,_          ->   CALLVALUE                                   @>> vm   (* Value (wei) Transferred to the account *) 
    | TmZero                ,_          ->   PUSH (Int 0)                                @>> vm 
    | EpNow                 ,_          ->   TIMESTAMP                                   @>> vm 
    | TmFalse               ,_          ->   PUSH (Int 0)                                @>> vm  
    | TmTrue                ,_          ->   PUSH (Int 1)                                @>> vm 
    | TmU256   d            ,_          ->   PUSH (Big d)                                @>> vm  
    | TmU8     d            ,_          ->   PUSH (Big d)                                @>> vm  
    | Balanc   e            ,_          ->   BALANCE     @>>          e          @> (R,ly,le,vm)
    | TmApp    _            ,_          ->   codegen_app     (fst e)                   ly le vm 
    | TmAbs    _            ,_          ->   codegen_abs     (fst e)                   ly le vm 
    | TmFix    _            ,_          ->   codegen_fix     (fst e)                   ly le vm 
    | TmI      _            ,_          ->   codegen_idx     (fst e)                   ly le vm 
    | TmIStrct _            ,_          ->   codegen_istrct  (fst e)                   ly le vm 
    | TmIf     _            ,_          ->   codegen_if      (fst e)                   ly le vm 
    | TmAdd  (l,r)          ,_          ->   codegen_op ADD l r                        ly le vm             
    | TmSub  (l,r)          ,_          ->   codegen_op SUB l r                        ly le vm             
    | TmMul  (l,r)          ,_          ->   codegen_op MUL l r                        ly le vm             
    | TmLT   (l,r)          ,_          ->   codegen_op LT  l r                        ly le vm           
    | TmGT   (l,r)          ,_          ->   codegen_op GT  l r                        ly le vm           
    | TmEQ   (l,r)          ,_          ->   codegen_op EQ  l r                        ly le vm           
    | TmNEQ  (l,r)          ,_          ->   ISZERO  @>>     codegen_op EQ l r         ly le vm
    | TmNOT    e            ,_          ->   ISZERO  @>>               e       @> (aln,ly,le,vm)  
    | TmLAND (l,r)          ,_          ->   codegen_LAnd l r aln                      ly le vm 
    | TmCall(id,args)       ,rety       ->   codegen_pre_call id args rety aln         ly le vm
    | TmSend((e,TyAdr  ),m,ags,msg),_   ->   codegen_send_eoa(e,TyAdr)         msg    ly le vm 
    | TmSend((c,TyIsc n),m,ags,msg),_   ->   codegen_send_cn(c,TyIsc n) m ags  msg    ly le vm 
    | TmNew(id,args,msg)    ,TyIsc _    ->   codegen_new    id args msg                ly le vm 
    | TmAddr(c,TyIsc i)     ,TyAdr      ->   (c,TyIsc i)                      @> (aln,ly,le,vm) 
    | TmSender              ,TyAdr      ->   _SHIFT_IF_L  aln TyAdr    (CALLER          @>> vm) 
    | TmThis                ,_          ->   _SHIFT_IF_L  aln TyAdr    (ADDRESS         @>> vm) 
    | TmArr(a,i) (*a[i][j]*),TyMap _    ->   codegen_secondary_arr a i                 ly le vm     
    | TmArr(ai,j)(*a[i][j]*),      _    ->   codegen_arr ai j                          ly le vm     
    | TmId id               ,TyMap _    ->   codegen_aid (lookup_le id le)                le vm 
    | TmId id               ,ty         ->   push_loc (lookup_le id le) aln ty               vm     
    | TmDeref(ref,tyR)      ,ty         ->   MLOAD   @>> (ref,tyR)               @> (R,ly,le,vm) 
    | e                                 ->   let _,vm = codegen_tm_eff e   aln         ly le vm  in 
                                                                PUSH (Int 0)                 @>> vm 

and codegen_tm_eff tm aln ly le vm  =   match tm with 
    | TmAbort               ,TyErr      ->  le, _THROW                    vm                               
    | TmLog(id,args,Some ev),TyUnit     ->  codegen_log id args ev  ly le vm     
    | TmSfDstr tm           ,TyUnit     ->  codegen_selfdstr  tm    ly le vm 
    | TmAsgn(l,r)         ,TyUnit     ->  codegen_assign l r      ly le vm  
    | TmRet(ret,cont)    ,_          ->  codegen_return ret cont ly le vm    
    | e                                 ->  pf "codegen_tm: %s " (str_of_tm e); raise Not_found
    
and codegen_op op l r ly le vm      =   op @>> l @> (R,ly,le, r @> (R,ly,le,vm))      
            
and _PUSH    expr ly le vm          =   expr    @> (R,ly,le,vm)         (*                                            value >> .. *) 

and _PUSH_GAS  vm =
    let vm      =   PUSH(Int 3000)                      @>> vm      in  (*                          3000 >> cnAddr >> value >> .. *)
    let vm      =   GAS                                 @>> vm      in  (*                   gas >> 3000 >> cnAddr >> value >> .. *)
                    SUB                                 @>> vm          (*                      gas-3000 >> cnAddr >> value >> .. *)

and _CALL_RESTOREPC vm =                                                (*  gas-3000 >> cnAddr >> msg >> &mhash >> argsize+4 >> retbegin >> retsz >> retbegin >> retsz >> PCbkp *)
    let vm      =   CALL                                @>> vm      in  (*                                                                success >> retbegin >> retsz >> PCbkp *)
    let vm      =   PUSH(Int 0)                         @>> vm      in  (*                                                           0 >> success >> retbegin >> retsz >> PCbkp *)
    let vm      =   JUMPI                               @>> vm      in  (*  IF success==0 THEN GOTO 0                                                retbegin >> retsz >> PCbkp *)
    let vm      =   SWAP2                               @>> vm      in  (*                                                                           PCbkp >> retsz >> retbegin *)
                    _RESTORE_PC                             vm          (*                                                                                    retsz >> retbegin *)

and _MLOAD_RET vm =                                                     (*                                 retsize >> retbegin >> .. *)
    let vm      =   PUSH (Int 0x20)                     @>> vm      in  (*                         0x20 >> retsize >> retbegin >> .. *)
    let vm      =   _THROW_IF_NEQ                           vm      in  (* IF 0x20!=retsize ERROR                     retbegin >> .. *)
                    MLOAD                               @>> vm          (*                                         M[retbegin] >> .. *)

and codegen_send_cn cn m args msg ly le vm =  (* msg-call to a contract *) 
    let TyIsc cnm = snd cn                                         in  
    let cnidx   =   lookup_cnidx vm.cns cnm                         in 
    let callee  =   lookup cnidx vm.cns                             in
    let Some mnm = m                                                in 
    let mhd     =   find_mhead vm callee mnm                        in
    let TyMthd(id,_,rety) = mhd                                     in 
    let retsz   =   size_of_ty rety                                 in  

    let vm      =   Comment("BEGINE send to "^id)       @>> vm      in  
    let vm      =   _RESET_PC                               vm      in  (*                                                                                               PCbkp *)
    let vm      =   PUSH(Int retsz)                     @>> vm      in  (*                                                                                      retsz >> PCbkp *)
    let vm      =   _MALLOC                                 vm      in  (*                                                                          retbegin >> retsz >> PCbkp *)
    let vm      =   repeat DUP2 2                           vm      in  (*                                                     retbegin >> retsz >> retbegin >> retsz >> PCbkp *)
    let vm      =   _MSTORE_MHASHMARGS mhd args       ly le vm      in  (*                              &mhash >> argsize+4 >> retbegin >> retsz >> retbegin >> retsz >> PCbkp *)
    let vm      =   _PUSH msg                         ly le vm      in  (*                       msg >> &mhash >> argsize+4 >> retbegin >> retsz >> retbegin >> retsz >> PCbkp *)
    let vm      =   _PUSH cn                          ly le vm      in  (*             cnAddr >> msg >> &mhash >> argsize+4 >> retbegin >> retsz >> retbegin >> retsz >> PCbkp *)
    let vm      =   _PUSH_GAS                               vm      in  (* gas-3000 >> cnAddr >> msg >> &mhash >> argsize+4 >> retbegin >> retsz >> retbegin >> retsz >> PCbkp *)
    let vm      =   _CALL_RESTOREPC                         vm      in  (*                                                                                   retsz >> retbegin *)
    let vm      =   _MLOAD_RET                              vm      in  (*                                                                                                 ret *)
                    Comment("END send to "^id)          @>> vm 

and codegen_send_eoa eoa msg ly le vm =   (* send value to an EOA *) 
    let vm      =   Comment "BEGINE send to EOA"        @>> vm      in  (*                                                                   .. *) 
    let vm      =   _RESET_PC                               vm      in  (*                                                          PCbkp >> .. *) 
    let vm      =   repeat (PUSH(Int 0)) 6                  vm      in  (*                            0 >> 0 >> 0 >> 0 >> 0 >> 0 >> PCbkp >> .. *) 
    let vm      =   _PUSH msg                         ly le vm      in  (*             addr >> msg >> 0 >> 0 >> 0 >> 0 >> 0 >> 0 >> PCbkp >> .. *) 
    let vm      =   _PUSH eoa                         ly le vm      in  (*             addr >> msg >> 0 >> 0 >> 0 >> 0 >> 0 >> 0 >> PCbkp >> .. *) 
    let vm      =   _PUSH_GAS                               vm      in  (* gas-3000 >> addr >> msg >> 0 >> 0 >> 0 >> 0 >> 0 >> 0 >> PCbkp >> .. *) 
    let vm      =   _CALL_RESTOREPC                         vm      in  (*                                                         0 >> 0 >> .. *)
    let vm      =   POP                                 @>> vm      in  (*                                                              0 >> .. *)
                    Comment "END send to EOA"           @>> vm 

and _SSTORE_TO(TmArr(a,i),_) ly le vm     =                             (*                                  rval >> .. *)
    let vm      =   _KEC_ARR a i                      ly le vm      in  (*                      KEC(a^i) >> rval >> .. *)
                    SSTORE                              @>> vm          (* S[KEC(a^i)] := rval                      .. *)

and codegen_assign l r ly le vm = 
    let vm      =   Comment "BEGIN Assign"              @>> vm      in 
    let vm      =   r                           @> (R,ly,le,vm)     in  (*                                    r >> .. *)
    let vm      =   _SSTORE_TO l                      ly le vm      in  (* S[KEC(l)] := r                          .. *)  
    let vm      =   Comment "END Assign"                @>> vm      in 
    le,vm 

and codegen_LAnd l r aln ly le vm = 
    assert(aln=R);         
    let la      =   fresh_label ()                                  in  (*                                                        .. *)
    let vm      =   l                           @> (R,ly,le,vm)     in  (*                                                   l >> .. *)
    let vm      =   DUP1                                @>> vm      in  (*                                              l >> l >> .. *)
    let vm      =   _GOTO_IFN la                            vm      in  (*                                                   l >> .. *)
    let vm      =   POP                                 @>> vm      in  (*                                                        .. *)
    let vm      =   r                           @> (R,ly,le,vm)     in  (*                                                   r >> .. *)
    let vm      = JUMPDEST la                           @>> vm      in  (*                                                   r >> .. *)
                    repeat ISZERO 2                         vm          (*                                                l&&r >> .. *)  

and _MSTORE_MARGS aln args ly le vm =
    let vm      =   PUSH(Int 0)                         @>> vm      in  (*                                                   0 >> .. *)
                    foldl (_MSTORE_MARG aln le ly) vm args           (*                                             sumsize >> .. *) 

and _MSTORE_MARG aln le ly vm arg  =
    let ty      =   get_ty arg                                      in  
    let i       =   match aln with | L -> size_of_ty ty 
                                   | R -> 0x20                      in  (*                                                 sum >> .. *)
    let vm      =   PUSH (Int i)                        @>> vm      in  (*                                         size >> sum >> .. *)
    let vm      =   _MALLOC                                 vm      in  (*                          alloc(size) >> size >> sum >> .. *)
    let vm      =   arg                       @> (aln,ly,le,vm)     in  (*                   arg >> alloc(size) >> size >> sum >> .. *)
    let vm      =   SWAP1                               @>> vm      in  (*                   alloc(size) >> arg >> size >> sum >> .. *)
    let vm      =   MSTORE                              @>> vm      in  (* M[alloc(size)] := arg                   size >> sum >> .. *)
                    ADD                                 @>> vm          (*                                            size+sum >> .. *)

and _MSTORE_MHASHMARGS mthd args ly le vm =                             (*                                                        .. *)
    let vm      =   _MSTORE_MHASH    mthd                   vm      in  (*                                         &mhash >> 4 >> .. *)
    let vm      =   _MSTORE_MARGS R args              ly le vm      in  (*                              argsize >> &mhash >> 4 >> .. *)
    let vm      =   SWAP1                               @>> vm      in  (*                              &mhash >> argsize >> 4 >> .. *)
    let vm      =   SWAP2                               @>> vm      in  (*                              4 >> argsize >> &mhash >> .. *)
    let vm      =   ADD                                 @>> vm      in  (*                                 argsize+4 >> &mhash >> .. *)
                    SWAP1                               @>> vm          (*                                 &mhash >> argsize+4 >> .. *)

and codegen_mthd_argLen_chk m vm = match m with  
    | TyDflt     -> vm
    | TyMthd _   ->
    let vm      =   PUSH(Int(calldatasize m))           @>> vm      in
    let vm      =   CALLDATASIZE                        @>> vm      in
                    _THROW_IF_NEQ                           vm        

and escape_ARG arg retlabel a       ly le vm = 
    let vm      =   Comment "ESCAPE START"              @>> vm      in 
    let vm      =   PUSH(Label retlabel)                @>> vm      in     
    let vm      =   arg                         @> (a,ly,le,vm)     in 
    let vm      =   ePUSH                                   vm      in
                    Comment "ESCAPE DONE"               @>> vm 

and codegen_app_rec(TmApp((TmIRec i,_),arg)) a ly le vm = 
    let vm      =   Comment "BEGIN APP-REC"             @>> vm      in
    let retaddr =   fresh_label ()                                  in 
    let start   =   lookup_rec_param le                             in 
    let vm      =   escape_ARG arg retaddr          a ly le vm      in 
    let vm      =   _GOTO start                             vm      in 
    let vm      =   JUMPDEST retaddr                    @>> vm      in       
                    Comment "END APP-REC"               @>> vm 

and codegen_fix(TmFix(phi,n,ty,tm)) ly le vm = 
    let vm      =   Comment "BEGIN FIX"                 @>> vm      in 
    let start   =   fresh_label ()                                  in 
    (* #DEBUG pf "! add_rec_param(label%d)\n" start;     *)
    let le      =   add_rec_param le start                    in
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
    | _ -> (* #DEBUG pf "! add_brjidx %s\n" (str_of_tm t2); *)
    let le      =   add_brjidx le t2                                in       
                    t1                          @> (R,ly,le,vm)      

and codegen_abs (TmAbs(x,_,t)) ly le vm =   
                    t                           @> (R,ly,le,vm)     
       
and codegen_idx (TmI(i,n))      ly le vm = 
                    lookup_brjidx i le          @> (R,ly,le,vm)       

and codegen_if (TmIf(b,t1,t2))      ly le vm = 
    let elif    =   fresh_label()                                   in 
    let fi      =   fresh_label()                                   in
    let vm      =   Comment "IF"                        @>> vm      in 
    let vm      =   b                           @> (R,ly,le,vm)     in 
    let vm      =   _GOTO_IFN elif                          vm      in 
    let vm      =   Comment "THEN"                      @>> vm      in 
    let vm      =   t1                          @> (R,ly,le,vm)     in
    let vm      =   _GOTO fi                                vm      in 
    let vm      =   Comment "ELSE"                      @>> vm      in 
    let vm      =   JUMPDEST elif                       @>> vm      in 
    let vm      =   t2                          @> (R,ly,le,vm)     in 
    let vm      =   Comment "FI"                        @>> vm      in 
                    JUMPDEST fi                         @>> vm      

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
    let vm      =   tm                          @> (R,ly,le,vm)     in
    let vm      =   SELFDESTRUCT                        @>> vm      in
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
    let vm      =   _PUSH_EVHASH    ev                      vm      in
    let le,vm   =   mstore_tms args                 R ly le vm      in  (* stack : [..., size, offset] *)
    let n       =   L.length visible + 1                            in
    let vm      =   log n                               @>> vm      in  (* deindexee N in logN *)
    le, vm

(***************************************)
(***     4. CODEGEN RETURN           ***)
(***************************************)

and push_args le sto       = foldr (fun arg vm -> arg @> (R,sto,le,vm))  
and sstores_to locs vm     = foldl sstore_to vm locs
and sstore_to  vm loc      = SSTORE @>> PUSH (Int loc) @>> vm      

and sstore_vars offst idx args sto le vm = 
    let cn      =   lookup idx vm.cns                               in
    let arglocs =   arg_locs_of_cn offst cn                         in
    assert(len arglocs = len args) ; 
    let vm      =   push_args le sto args                   vm      in  (*                                         argk >> .. >> arg1 >> .. *)
    let vm      =   sstores_to arglocs                      vm      in  (*  S[l_k]:=argk; .. ; S[l_1]:=arg1                              .. *)
    le,vm

and cont_call (TmCall(cn,args),_) sto le vm = 
    let idx     =   lookup_cnidx vm.cns cn                          in 
    let offst   =   (sto.vars idx).offst                            in  
    let vm      =   Comment "Setting Cont"              @>> vm      in 
    let vm      =   _SET_PC idx                              vm      in  (*  S[PC] := rntime_offset_of_cn                                 .. *) 
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
    let vm      =   _MALLOC                                vm      in  (*                alloc(32) >> 32 >> val >> .. *)
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
    | tm        ->  RETURN @>> snd ( mstore_tm tm  R sto le vm ))   in 
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
                            { rn_vm         = empty_vm cns 
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


let mstore_rn_code idx vm = 
    let vm      =   PUSH(RnSize)                    @>> vm  in  (*                                                      size >> .. *)
    let vm      =   PUSH _HP                        @>> vm  in  (*                                             0x40  >> size >> .. *)
    let vm      =   MLOAD                           @>> vm  in  (*                                           M[0x40] >> size >> .. *) 
    let vm      =   DUP1                            @>> vm  in  (*                                M[0x40] >> M[0x40] >> size >> .. *)    
    let vm      =   PUSH(RnSize)                    @>> vm  in  (*                        size >> M[0x40] >> M[0x40] >> size >> .. *)
    let vm      =   ADD                             @>> vm  in  (*                           M[0x40]+size >> M[0x40] >> size >> .. *)
    let vm      =   PUSH _HP                        @>> vm  in  (*                   0x40 >> M[0x40]+size >> M[0x40] >> size >> .. *)
    let vm      =   MSTORE                          @>> vm  in  (*                                           M[0x40] >> size >> .. *)
    let vm      =   PUSH(RnSize)                    @>> vm  in  (*                               size >> alloc(size) >> size >> .. *)
    let vm      =   PUSH(CrSize idx)                @>> vm  in  (*                     crsize >> size >> alloc(size) >> size >> .. *)
    let vm      =   DUP3                            @>> vm  in  (*          M[0x40] >> crsize >> size >> alloc(size) >> size >> .. *)
                    CODECOPY                        @>> vm      (*                                       alloc(size) >> size >> .. *)
    

(*
let mstore_rn_code idx vm =                                         (*                                                              .. *)
    let vm      =   PUSH(RnSize)                    @>> vm      in  (*                                                      size >> .. *)
    let vm      =   PUSH(RnSize)                    @>> vm      in  (*                                              size >> size >> .. *)  
    let vm      =   malloc                              vm      in  (*                                       alloc(size) >> size >> .. *)
    let vm      =   PUSH(RnSize)                    @>> vm      in  (*                               size >> alloc(size) >> size >> .. *)
    let vm      =   PUSH(CrSize idx)                @>> vm      in  (*                     crsize >> size >> alloc(size) >> size >> .. *)
    let vm      =   DUP3                            @>> vm      in  (*                0 >> crsize >> size >> alloc(size) >> size >> .. *)
                    CODECOPY                        @>> vm          (*                                       alloc(size) >> size >> .. *)
                                                                    (*                                        codebegin                *)
                                                                    (* CODECOPY to from size *)   *)
let codegen_creation cns idx = (* return vm which contains the program *) 
    let TmCn(id,_,_) as cn = lookup idx cns in 
    let vm      =   empty_vm cns                                in  (*                                                                          *)
    let vm      =   Comment("Begin Creation "^id)   @>> vm      in 
    let vm      =   init_malloc                         vm      in  (* M[0x40] := 0x60                                                          *)
    let vm      =   _SALLOC_ARRS        cn               vm      in  (* S[1]    := #array                i << alloc(argssize) << argssize << ..  *)
    let vm      =   _SET_PC            idx               vm      in  (* S[PC]   := rn_cn_offst    (returned body)                                *)
    let vm      =   mstore_rn_code    idx               vm      in  (*                                 alloc(codesize) << codesize << i <<  ..  *)
    let vm      =   RETURN                          @>> vm      in  (* OUTPUT(M[code]) as The BODY code                               i <<  ..  *)
    let vm      =   Comment("End Creation "^id)     @>> vm      in 
                    INVALID                         @>> vm      

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


(*  PUSH SIZE SOLVER *) 
let sizes_of_codes crs rn = 
    let sz_rn,la_rn             =   vsize_of_prog rn.rn_vm.program in
    let i_crs                   =   map (vsize_of_prog $ extract_prog $ vm_of_cr) crs in 
    let _,crs                   =   unzip i_crs in 
    let szs_crs,las_crs         =   unzip crs in 
    let sz_crs,la_crs           =   sum szs_crs, sum las_crs in 
    let sz,la                   =   sz_rn + sz_crs , la_rn + la_crs in 
    let temp_size               =   sz + la * (1 + 32) in 
    let imm_size                =   log_size (big temp_size) in 
    let push_size               =   imm_size + 1 in 
    imm_size, sz + la * push_size :: sz_rn + la_rn * push_size ::  L.map (fun (sz,la) -> sz + la * push_size) crs

let dec_push_prog immsz     =   L.map (dec_PUSH immsz) 
let update_prog_of_cr f cr  =   { cr with cr_vm = { cr.cr_vm with program = f cr.cr_vm.program } }
let update_prog_of_rn f rn  =   { rn with rn_vm = { rn.rn_vm with program = f rn.rn_vm.program } }

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
    let immsz,szs   =   sizes_of_codes crs rn in 
    (*let ()          =   pf "imm_size is %d ↦ PUSH%d" immsz immsz in 
    let ()          =   pr_ints szs in   *)
    let rn          =       update_prog_of_rn (dec_push_prog immsz) rn  in 
    let crs         =   map(update_prog_of_cr (dec_push_prog immsz))crs in 
    let _           =       update_prog_of_rn  mk_labels            rn  in 
    let _           =   map(update_prog_of_cr  mk_labels)           crs in
    let rn_info     =   rn_info_of_rn rn  crs                           in (* rn_size *)
    let cr_infos    =   cr_infos_of_crs   crs                           in 
    let layt        =   init_layout cr_infos rn_info                    in
    let cr          =   lookup idx crs                                  in
    let cr_prog     =   prog_of_cr cr                                   in
    let crs_prog    =   prog_of_crs crs                                 in 
    let rn_prog     =   extract_prog rn.rn_vm                           in
    let cr_code     =   realize_prog layt cr_prog                       in
    let crs_code    =   realize_prog layt crs_prog                      in 
    let rn_code     =   realize_prog layt rn_prog                       in 
    crs_code @ rn_code @ cr_code  
    (* the generated  OPCODE list is stored in reverse order *)
