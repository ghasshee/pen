(* CE := Codegen Environment *)


open Printf 
open Syntax
open ContractId
open Evm 
open Misc


type ce                             =
                                    { stack_size    : int
                                    ; program       : Imm.imm program
                                    ; idx_lookup    : string -> idx
                                    ; cntrcts       : ty cntrct idx_list }

let extract_program ce              =   ce.program
let idx_lookup ce name              =   try ce.idx_lookup name 
                                        with e -> (eprintf"Unknown Cntrct %s.\n%!"name;raise e)

let idx_lookup_in_assoc cns name = lookup_idx (fun cn -> cn.cntrct_name=name) cns

let empty_ce idx_lookup cntrcts     =
    { stack_size        = 0
    ; program           = empty_program
    ; idx_lookup        = idx_lookup
    ; cntrcts           = cntrcts  }

let code_length    ce               =   size_of_program ce.program
let stack_size     ce               =   ce.stack_size
let set_stack_size ce i             =   { ce with stack_size = i }



let cntrct_lookup ce idx          =   try choose_cntrct idx ce.cntrcts
                                      with e -> Printf.eprintf "cntrct_lookup failed on %d\n%!" idx; 
                                                pr_idx_mapping(fun x->x)(idxs ce.cntrcts); raise e;;

let cntrct_of_name  ce  = ( cntrct_lookup ce ) $ ( idx_lookup ce )   

(***************************************)
(*         APPEND OPCODE               *)
(***************************************)


let append_opcode ce opcode         =
    if ce.stack_size < stack_popped opcode then failwith "stack underflow" else    
    begin match opcode with
    | JUMPDEST l   ->  begin try ignore ( Label.lookup_value   l )
                       with Not_found ->  Label.register_value l (code_length ce) end
    | _            ->  ()   end ; 
    let new_stack_size = ce.stack_size - stack_popped opcode + stack_pushed opcode in
    if new_stack_size > 1024 then failwith "stack overflow" else    
    { stack_size    = new_stack_size
    ; program       = opcode :: ce.program 
    ; idx_lookup    = ce.idx_lookup
    ; cntrcts       = ce.cntrcts        }


let (>>>) op ce = append_opcode ce op  









(*************************************************)
(* MIGHT BECOME := lookup continuation contracts *) 
(*************************************************)

(* returns 
 *   the list of cont contract names 
 *   might_become : term -> [string] *)
    
let rec fncall_might_become f   =   exprs_might_become      f.call_args
and new_expr_might_become n     =   exprs_might_become      n.new_args 
                                @   msg_info_might_become   n.new_msg_info
and msg_info_might_become m     =   ( match m.msg_value_info with
                                    | None    -> []
                                    | Some e  -> expr_might_become e) 
                                @   [(* TODO: m.msg_reentrance_info *)]
and send_expr_might_become s    =   expr_might_become       s.send_cntrct
                                @   exprs_might_become      s.send_args
                                @   msg_info_might_become   s.send_msg_info
and exprs_might_become es       =   L.concat (L.map expr_might_become es)
and expr_might_become e         =   match fst e with
    | EpTrue                  
    | EpFalse                 
    | EpNow                   
    | EpValue                 
    | EpSender               
    | EpThis                  
    | EpDecLit256   _         
    | EpDecLit8     _         
    | EpIdent       _         ->  []
    | EpParen       e         
    | EpAddr        e         
    | EpNot         e         
    | EpSingleDeref e         
    | EpTupleDeref  e         
    | EpBalance     e         ->  expr_might_become e
    | EpLand      (l,r)           
    | EpLt        (l,r)           
    | EpGt        (l,r)           
    | EpNeq       (l,r)           
    | EpEq        (l,r)           
    | EpMinus     (l,r)
    | EpMult      (l,r)
    | EpPlus      (l,r)       ->  (expr_might_become l) @ (expr_might_become r)
    | EpArray(LExprArray a)   ->  expr_might_become a.array_index
    | EpFnCall f             ->  fncall_might_become f
    | EpNew n                 ->  new_expr_might_become n
    | EpSend s                ->  send_expr_might_become s

let rec stmt_might_become       =   function 
    | SmAbort                 ->  []
    | SmSelfDestruct e        
    | SmExpr         e        ->  expr_might_become e
    | SmVarDecl      v        ->  expr_might_become v.varDecl_val
    | SmAssign(LExprArray a,r)->  (expr_might_become a.array_index) @ (expr_might_become r)
    | SmIfThen(c,b)           ->  (expr_might_become c) @ (stmts_might_become b)
    | SmIfThenElse(c,b0,b1)       ->  (expr_might_become c) @ (stmts_might_become b0) @ (stmts_might_become b1)
    | SmLog(_,l,_)            ->  exprs_might_become l
    | SmReturn r              ->  (match r.ret_expr with
                                    | Some e        -> expr_might_become e
                                    | None          -> [] ) 
                                @   ( expr_might_become r.ret_cont ) 
                                @   ( match cntrct_name_of_ret_cont r.ret_cont with
                                    | Some name     -> [name]
                                    | None          -> [] )
and stmts_might_become ss       =   L.concat (L.map stmt_might_become ss)

let mthd_might_become  m        =   stmts_might_become m.mthd_body
let mthds_might_become ms       =   L.concat (L.map mthd_might_become ms)
let might_become cn             =   mthds_might_become cn.mthds




(* LOOKUP_USUAL_METHOD *) 

let lookup_mthd_info_in_cntrct cn mname =
    let mthd  = L.filter (fun c -> match c.mthd_head with
                          | Default  -> false
                          | Method m -> m.mthd_name=mname) cn.mthds in
    match mthd with
    | []        ->  raise Not_found
    | _::_::_   ->  eprintf "method %s duplicated\n%!" mname;err "lookup_mthd_info_in_cntrct" 
    | [a]       ->  begin match a.mthd_head with Method m -> m end


let rec lookup_mthd_info_inner ce (seen:ty cntrct list) cn mname : mthd_info =
    if L.mem cn seen then raise Not_found else
    try  lookup_mthd_info_in_cntrct cn mname
    with Not_found ->   let seen        = cn :: seen in
                        let becomes     = L.map (cntrct_of_name ce)(might_become cn) in
                        let rec lookup_becomes seen = function 
                           | []         ->  raise Not_found
                           | b::bs      ->  try lookup_mthd_info_inner ce seen b mname 
                                            with Not_found -> lookup_becomes (b :: seen) bs  in
                        lookup_becomes seen becomes

let lookup_mthd_info ce cn mthd_name = lookup_mthd_info_inner ce [] cn mthd_name 



    


