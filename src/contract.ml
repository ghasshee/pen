(* cont := continuation *) 


open Misc
open Syntax 
open Printf

module Eth = Ethereum 
module L   = List

type tyMthd     = Eth.tyMthd

let typeof_mthd m    = match m.mthd_head with
  | Method m    ->  Eth.{ tyRet     = m.mthd_retTy
                        ; name      = m.mthd_name
                        ; tyArgs    = L.map (fun x->x.ty) m.mthd_args }
  | Default     ->  Eth.{ tyRet     = TyTuple[]
                        ; name      = "" 
                        ; tyArgs    = []    }

type tyCntrct           =
                        { tyCntrct_name     : string   
                        ; tyCntrct_args     : ty list
                        ; tyCntrct_mthds    : tyMthd list
                        ; tyCntrct_conts    : string list (** this lists the names of cntrcts that this one can continue into *) }
                          (* Since [tyCntrct_args] contains bool[address] and such,
                           * is's not appropriate to use the ABI signature here.
                           * As a work around, at the time of deployment, these arrays are zeroed out. *)



(*********************************)
(*   Collect Continuation :      *)
(*    contract becomes what ?    *)
(*********************************)

let rec collect_cont_stmt   = function 
    | SmAbort               ->  []
    | SmSelfDestruct _      ->  []
    | SmExpr _              ->  []
    | SmAssign (_,_)        ->  []
    | SmVarDecl _           ->  []
    | SmIfThen (_,s)        ->  collect_cont_stmts s
    | SmIf (_,s,t)          ->  collect_cont_stmts s @ collect_cont_stmts t
    | SmLog _               ->  []  
    | SmReturn r            ->  begin
         match cntrct_name_of_ret_cont r.ret_cont with
         | None                 -> []
         | Some name            -> [name]   end

and collect_cont_stmts s =  L.concat (L.map collect_cont_stmt s)

let collect_cont_mthd (raw : 'expr mthd) : string list =
    L.concat (L.map collect_cont_stmt raw.mthd_body)

let collect_cont_cntrct (raw : 'expr cntrct) : string list =
    L.concat (L.map collect_cont_mthd raw.mthds)




(***********************************)
(*   generate contract Interface   *)
(***********************************)


let typeof_cntrct (cn : 'expr cntrct) : tyCntrct =
    { tyCntrct_name     = cn.cntrct_name
    ; tyCntrct_args     = L.map (fun x -> x.ty) cn.cntrct_args
    ; tyCntrct_mthds    = L.map typeof_mthd cn.mthds
    ; tyCntrct_conts    = collect_cont_cntrct cn }


(***********************************)
(* getInfo from contract Interface *)
(***********************************)


let find_tyMthd_in_cntrct mname tyCntrct : tyMthd option =
    getFstFilter (fun mi -> if mi.Eth.name=mname then Some mi else None) tyCntrct.tyCntrct_mthds

let find_tyMthd tyCntrcts mname = 
    match getFstFilter (find_tyMthd_in_cntrct mname) (L.map snd tyCntrcts) with 
    | Some ty   -> ty
    | None      -> err ("find_tyMthd: "^mname^"Not found")



