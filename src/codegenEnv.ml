(* CE := Codegen Environment *)

open Printf 
open Syntax
open IndexList
open Evm 
open Misc


type ce                             =   { stack_size    : int
                                        ; program       : Location.imm program
                                        ; lookup_cn     : string -> idx
                                        ; cntrcts       : ty cntrct idx_list }

let extract_program ce              =   ce.program
let lookup_cn_of_ce  ce  name       =   ce.lookup_cn name 
let lookup_cn_of_cns cns name       =   lookup_idx (fun cn->cn.cntrct_id=name) cns

let empty_ce lookup_cn cns          =   { stack_size    = 0
                                        ; program       = empty_program
                                        ; lookup_cn     = lookup_cn
                                        ; cntrcts       = cns               }

let code_len       ce               =   size_of_program ce.program
let stack_size     ce               =   ce.stack_size
let set_stack_size ce i             =   { ce with stack_size = i }

let cntrct_lookup ce idx            =   try lookup_index idx ce.cntrcts
                                        with e ->   Printf.eprintf "cntrct_lookup failed on %d\n%!" idx; 
                                                    pr_idx_mapping(fun x->x)(idxs ce.cntrcts); raise e;;

let cntrct_of_name  ce              =   ( cntrct_lookup ce ) $ ( lookup_cn_of_ce ce )   


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
    ; lookup_cn         = ce.lookup_cn
    ; cntrcts           = ce.cntrcts        }

let (>>) op ce                 = append_opcode ce op  





(*************************************************)
(* MIGHT BECOME := lookup continuation contracts *) 
(*************************************************)

(* returns the list of cont contract names *)
    
let rec stmt_become             =   function 
    | SmSlfDstrct   e        
    | SmExpr        e           ->  expr_become e
    | SmDecl        v           ->  expr_become v.declVal
    | SmAssign(LEpArray a,r)    ->  expr_become a.arrIndex @ expr_become r
(*    | SmIfThen(c,b)             ->  expr_become c @ stmts_become b
    | SmIf(c,b0,b1)             ->  expr_become c @ stmts_become b0 @ stmts_become b1 *)
    | SmLog(_,l,_)              ->  exprs_become l
    | SmReturn r                ->  (match r.ret_expr with
                                    | Some e        -> expr_become e
                                    | None          -> [] ) 
                                @   ( expr_become r.ret_cont ) 
                                @   ( match cntrct_name_of_ret_cont r.ret_cont with
                                    | Some name     -> [name]
                                    | None          -> [] )
and stmts_become ss             =   L.concat (L.map stmt_become ss)
and fncall_become f             =   exprs_become f.call_args
and new_become n                =   exprs_become n.new_args @ expr_become n.new_msg
and send_expr_become s          =   expr_become s.sd_cn @ exprs_become s.sd_args @ expr_become s.sd_msg
and exprs_become es             =   L.concat (L.map expr_become es)
and expr_become e               =   match fst e with
    | SmAbort                   ->  []
    | EpTrue | EpFalse | EpNow 
    | EpThis | EpValue | EpSender 
    | EpDecLit256   _  | EpDecLit8 _ 
    | EpIdent       _           ->  []
    | EpParen       e         
    | EpAddr        e         
    | EpNot         e         
    | EpDeref       e         
    | EpBalance     e           ->  expr_become e
    | EpLT       (l,r) | EpGT       (l,r)           
    | EpNeq      (l,r) | EpEq       (l,r)           
    | EpMult     (l,r) | EpPlus     (l,r)         
    | EpLAnd     (l,r)           
    | EpMinus    (l,r)          ->  (expr_become l) @ (expr_become r)
    | EpArray a                 ->  expr_become a.arrIndex
    | EpFnCall f                ->  fncall_become f
    | EpNew n                   ->  new_become n
    | EpSend s                  ->  send_expr_become s

let mthd_become  m        =   stmts_become m.mthd_body
let mthds_become ms       =   L.concat (L.map mthd_become ms)
let become cn             =   mthds_become cn.mthds


(* LOOKUP_USUALMETHOD *) 

let lookup_mthd_info_in_cntrct cn mname =
    let mthd = L.filter (fun c -> match c.mthd_head with
                        | Default  -> false
                        | Method m -> m.mthd_id=mname) cn.mthds in
    match mthd with
    | []            ->  raise Not_found
    | _::_::_       ->  eprintf "method %s duplicated\n%!" mname;err "lookup_mthd_info_in_cntrct" 
    | [a]           ->  begin match a.mthd_head with Method m -> m end


let rec lookup_mthd_info_inner ce (seen:ty cntrct list) cn mname : mthd_info =
    if L.mem cn seen then raise Not_found else
    try  lookup_mthd_info_in_cntrct cn mname
    with Not_found  ->  let seen        = cn :: seen in
                        let becomes     = L.map (cntrct_of_name ce)(become cn) in
                        let rec lookup_becomes seen = function 
                           | []         ->  raise Not_found
                           | b::bs      ->  try lookup_mthd_info_inner ce seen b mname 
                                            with Not_found -> lookup_becomes (b :: seen) bs  in
                        lookup_becomes seen becomes

let lookup_mthd_info ce cn mname = lookup_mthd_info_inner ce [] cn mname 



