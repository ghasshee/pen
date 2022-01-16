
open Printf 
open Misc
open Syntax
open Evm 
open Location 



type vm                             =   { stack_height  : int
                                        ; program       : imm program
                                        ; lookup_cnidx  : str -> idx
                                        ; cns           : ty toplevel ilist }

let empty_vm lookup cns             =   { stack_height  = 0
                                        ; program       = empty_program
                                        ; lookup_cnidx  = lookup
                                        ; cns           = cns               }

let lookup_cnidx_at_vm   nm vm      =   vm.lookup_cnidx nm 
let lookup_cnidx        cns nm      =   lookup_idx      (function   TmCn(id,_,_) ->    id=nm) cns
let lookup_icn_of_icns icns nm      =   find_by_filter  (function i,TmCn(id,f,m) -> if id=nm then i,TmCn(id,f,m) else raise Not_found) icns
let lookup_cn           idx vm      =   lookup idx vm.cns 
let cn_of_nm            vm  nm      =   lookup_cn (lookup_cnidx_at_vm nm vm) vm 

let extract_prog       vm           =   vm.program
let get_stack_height   vm           =   vm.stack_height
let set_stack_height   vm i         =   { vm with stack_height = i }
let code_len           vm           =   size_of_prog vm.program


(***************************************)
(*         APPEND OPCODE               *)
(***************************************)

let append_opcode vm opcode         = 
    if vm.stack_height < stack_popped opcode then raise StackUnderFlow else 
    begin match opcode with 
    | JUMPDEST l        ->  begin try ignore ( Label.lookup_label   l )
                            with Not_found ->  Label.register_label l (code_len vm) end
    | _                 ->  () end ; 
    let new_stack_height    = vm.stack_height - stack_popped opcode + stack_pushed opcode in
    if new_stack_height > 1024 then raise StackOverFlow else    
    { stack_height          = new_stack_height
    ; program               = opcode :: vm.program 
    ; lookup_cnidx          = vm.lookup_cnidx
    ; cns                   = vm.cns        }

let (=>>) op vm                     =   append_opcode vm op  


(*************************************************)
(*    BECOME := lookup continuation contracts    *) 
(*************************************************)

let rec  become (TmCn(_,_,mthds))   =   mthds_become mthds
and mthds_become ms                 =   L.concat (L.map mthd_become ms)
and mthd_become(TmMthd(_,body))     =   tm_become body 
and tms_become es                   =   L.concat (L.map tm_become es)
and tm_become  e                    =   match fst e with
    | TmAbort  | TmUnit  | TmTrue | TmFalse | EpNow | EpThis | EpValue | EpSender | TmId _  | TmU8 _ | TmU256 _ -> []
    | EpAddr e | TmNOT e | TmDeref e | Balanc e | TmSfDstr e ->  tm_become e
    | TmLT   (l,r) | TmGT   (l,r) | TmNEQ  (l,r) | TmEQ  (l,r)           
    | TmMul  (l,r) | TmAdd  (l,r) | TmLAND (l,r) | TmSub (l,r)          
                                    ->  (tm_become l) @ (tm_become r)
    | TmArr(id,idx)                 ->  tm_become idx
    | TmCall(id,args)               ->  tms_become args 
    | TmNew(id,args,msg)            ->  tms_become args @ tm_become msg
    | TmSend(cn,_,args,msg)         ->  tm_become cn @ tms_become args @ tm_become msg
    | TmLog(_,l,_)                  ->  tms_become l
    | TmAssign((TmArr(id,idx),_),r) ->  tm_become idx @ tm_become r
    | TmReturn(ret,cont)            ->  tm_become ret @ tm_become cont @ (match cnname_of_ret_cont cont with
                                                                     | Some name     -> [name]
                                                                     | None          -> [] )
                                


(* LOOKUP_USUALMETHOD *) 

let lookup_mthd_head_at_cn (TmCn(_,_,mthds)) mname =
    match L.filter (function | TmMthd(TyDefault,_)       -> false
                             | TmMthd(TyMthd(id,_,_),_)  -> id=mname) mthds with 
    | []            ->  raise Not_found
    | [a]           ->  let TmMthd(head,_) = a in head 
    | _::_::_       ->  eprintf "method %s duplicated\n%!" mname;err "lookup_mthd_info_in_cntrct" 

let rec lookup_mthd_head vm     = lookup_mthd_head_top vm [] 
and     lookup_mthd_head_top vm seen cn mname =
    if L.mem cn seen then raise Not_found else
    try  lookup_mthd_head_at_cn cn mname
    with Not_found  ->  let seen        = cn :: seen in
                        let becomes     = L.map(cn_of_nm vm)(become cn) in
                        let rec lookup_becomes seen = function 
                           | []         ->  raise Not_found
                           | b::bs      ->  try lookup_mthd_head_top vm seen b mname 
                                            with Not_found -> lookup_becomes(b::seen)bs  in
                        lookup_becomes seen becomes




