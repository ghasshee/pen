(* CE := Codegen Environment *)

open Printf 
open Misc
open Syntax
open Evm 
open Location 



type ce                             =   { stack_size    : int
                                        ; program       : imm program
                                        ; lookup_cnidx  : string -> idx
                                        ; cntrcts       : ty cntrct idxlist }

let empty_ce lookup cns             =   { stack_size    = 0
                                        ; program       = empty_program
                                        ; lookup_cnidx  = lookup
                                        ; cntrcts       = cns               }

let lookup_cnidx_of_ce  ce  name    =   ce.lookup_cnidx name 
let lookup_cnidx_of_cns cns name    =   lookup_idx      (fun cn->cn.id=name) cns
let lookup_icn_of_icns icns name    =   find_by_filter  (fun (i,cn)->if cn.id=name then(i,cn)else raise Not_found) icns
let lookup_cn           ce  idx     =   lookup idx ce.cntrcts 
let cntrct_of_name  ce              =   lookup_cn ce $ lookup_cnidx_of_ce ce 

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

let (>>) op ce                      = append_opcode ce op  


(*************************************************)
(*    BECOME := lookup continuation contracts    *) 
(*************************************************)

let rec  become cn                  =   mthds_become cn.mthds
and mthds_become ms                 =   L.concat (L.map mthd_become ms)
and mthd_become(TmMthd(_,body))     =   stmts_become body 
and stmts_become ss                 =   L.concat (L.map stmt_become ss)
and stmt_become                     =   function 
    | SmExpr        e               ->  expr_become e
    | SmDecl(_,_,v)                 ->  expr_become v
    | SmAssign(TmArray(id,idx),r)   ->  expr_become idx @ expr_become r
    | SmIf(c,b,b')                  ->  expr_become c @ stmts_become b @ stmts_become b'
and exprs_become es                 =   L.concat (L.map expr_become es)
and expr_become  e                  =   match fst e with
    | TmAbort  | TmUnit  | EpTrue | EpFalse | EpNow | EpThis | EpValue | EpSender | TmId _  | TmU8 _ | TmU256 _ -> []
    | EpAddr e | EpNot e | EpDeref e | EpBalance e | TmSlfDstrct e ->  expr_become e
    | EpLT   (l,r) | EpGT   (l,r) | EpNEq  (l,r) | TmEq   (l,r)           
    | TmMul (l,r) | EpPlus (l,r) | EpLAnd (l,r) | TmMinus(l,r)          
                                    ->  (expr_become l) @ (expr_become r)
    | TmArray(id,idx)               ->  expr_become idx
    | TmCall(id,args)               ->  exprs_become args 
    | TmNew(id,args,msg)            ->  exprs_become args @ expr_become msg
    | TmSend(cn,_,args,msg)         ->  expr_become cn @ exprs_become args @ expr_become msg
    | TmLog(_,l,_)                  ->  exprs_become l
    | TmReturn(ret,cont)            ->  expr_become ret @ expr_become cont @ (match cntrct_name_of_ret_cont cont with
                                                                         | Some name     -> [name]
                                                                         | None          -> [] )
                                


(* LOOKUP_USUALMETHOD *) 

let lookup_mthd_head_in_cntrct cn mname =
    match L.filter (function | TmMthd(TyDefault,_)       -> false
                             | TmMthd(TyMthd(id,_,_),_)  -> id=mname) cn.mthds with 
    | []            ->  raise Not_found
    | [a]           ->  let TmMthd(head,_) = a in head 
    | _::_::_       ->  eprintf "method %s duplicated\n%!" mname;err "lookup_mthd_info_in_cntrct" 

let rec lookup_mthd_head_inner ce (seen:ty cntrct list) cn mname : ty=
    if L.mem cn seen then raise Not_found else
    try  lookup_mthd_head_in_cntrct cn mname
    with Not_found  ->  let seen        = cn :: seen in
                        let becomes     = L.map (cntrct_of_name ce)(become cn) in
                        let rec lookup_becomes seen = function 
                           | []         ->  raise Not_found
                           | b::bs      ->  try lookup_mthd_head_inner ce seen b mname 
                                            with Not_found -> lookup_becomes (b :: seen) bs  in
                        lookup_becomes seen becomes

let lookup_mthd_head ce cn mname = lookup_mthd_head_inner ce [] cn mname 



