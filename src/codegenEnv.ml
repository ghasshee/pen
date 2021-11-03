(* CE := Codegen Environment *)

open Printf 
open Misc
open Syntax
open Evm 
open Location 



type ce                             =   { stack_size    : int
                                        ; program       : imm program
                                        ; lookup_cnidx  : str -> idx
                                        ; cntrcts       : ty toplevel idxlist }

let empty_ce lookup cns             =   { stack_size    = 0
                                        ; program       = empty_program
                                        ; lookup_cnidx  = lookup
                                        ; cntrcts       = cns               }

let lookup_cnidx_at_ce cname ce     =   ce.lookup_cnidx cname 
let lookup_cnidx   cns cname        =   lookup_idx      (function   TmCn(id,_,_) ->    id=cname) cns
let lookup_icn_of_icns icns name    =   find_by_filter  (function i,TmCn(id,f,m) -> if id=name then i,TmCn(id,f,m) else raise Not_found) icns
let lookup_cn           idx  ce     =   lookup idx ce.cntrcts 
let cntrct_of_name  cname ce        =   lookup_cn (lookup_cnidx_at_ce cname ce) ce 

let extract_program  ce             =   ce.program
let get_stack_size   ce             =   ce.stack_size
let set_stack_size   ce i           =   { ce with stack_size = i }
let code_len         ce             =   size_of_program ce.program


(***************************************)
(*         APPEND OPCODE               *)
(***************************************)

let append_opcode ce opcode         = 
    if ce.stack_size < stack_popped opcode then raise StackUnderFlow else 
    begin match opcode with 
    | JUMPDEST l   ->  begin try ignore ( Label.lookup_label   l )
                       with Not_found ->  Label.register_label l (code_len ce) end
    | _            ->  () end ; 
    let new_stack_size = ce.stack_size - stack_popped opcode + stack_pushed opcode in
    if new_stack_size > 1024 then raise StackOverFlow else    
    { stack_size        = new_stack_size
    ; program           = opcode :: ce.program 
    ; lookup_cnidx      = ce.lookup_cnidx
    ; cntrcts           = ce.cntrcts        }

let (=>>) op ce                    =   append_opcode ce op  


(*************************************************)
(*    BECOME := lookup continuation contracts    *) 
(*************************************************)

let rec  become (TmCn(_,_,mthds))   =   mthds_become mthds
and mthds_become ms                 =   L.concat (L.map mthd_become ms)
and mthd_become(TmMthd(_,body))     =   tm_become body 
and tms_become es                 =   L.concat (L.map tm_become es)
and tm_become  e                  =   match fst e with
    | TmAbort  | TmUnit  | TmTrue | TmFalse | EpNow | EpThis | EpValue | EpSender | TmId _  | TmU8 _ | TmU256 _ -> []
    | EpAddr e | TmNOT e | TmDeref e | Balanc e | TmSfDstr e ->  tm_become e
    | TmLT   (l,r) | TmGT   (l,r) | TmNEQ  (l,r) | TmEQ   (l,r)           
    | TmMul (l,r) | TmAdd (l,r) | TmLAND (l,r) | TmSub(l,r)          
                                        ->  (tm_become l) @ (tm_become r)
    | TmArray(id,idx)                   ->  tm_become idx
    | TmCall(id,args)                   ->  tms_become args 
    | TmNew(id,args,msg)                ->  tms_become args @ tm_become msg
    | TmSend(cn,_,args,msg)             ->  tm_become cn @ tms_become args @ tm_become msg
    | TmLog(_,l,_)                      ->  tms_become l
    | TmAssign((TmArray(id,idx),_),r)   ->  tm_become idx @ tm_become r
    | TmReturn(ret,cont)                ->  tm_become ret @ tm_become cont @ (match cnname_of_ret_cont cont with
                                                                         | Some name     -> [name]
                                                                         | None          -> [] )
                                


(* LOOKUP_USUALMETHOD *) 

let lookup_mthd_head_in_cntrct (TmCn(_,_,mthds)) mname =
    match L.filter (function | TmMthd(TyDefault,_)       -> false
                             | TmMthd(TyMthd(id,_,_),_)  -> id=mname) mthds with 
    | []            ->  raise Not_found
    | [a]           ->  let TmMthd(head,_) = a in head 
    | _::_::_       ->  eprintf "method %s duplicated\n%!" mname;err "lookup_mthd_info_in_cntrct" 

let rec lookup_mthd_head_inner ce (seen:ty toplevel list) cn mname : ty=
    if L.mem cn seen then raise Not_found else
    try  lookup_mthd_head_in_cntrct cn mname
    with Not_found  ->  let seen        = cn :: seen in
                        let becomes     = L.map (fun nm -> cntrct_of_name nm ce)(become cn) in
                        let rec lookup_becomes seen = function 
                           | []         ->  raise Not_found
                           | b::bs      ->  try lookup_mthd_head_inner ce seen b mname 
                                            with Not_found -> lookup_becomes (b :: seen) bs  in
                        lookup_becomes seen becomes

let lookup_mthd_head ce cn mname = lookup_mthd_head_inner ce [] cn mname 



