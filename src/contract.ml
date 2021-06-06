(* cont := continuation *) 


open Misc
open Syntax 

module Eth = Ethereum 
module L   = List

type mthd_intf     = Eth.fn_sig

let mthd_intf_of raw    = match raw.mthd_head with
  | Method m    ->  Eth.{ sig_return = m.mthd_ret_ty
                        ; sig_name   = m.mthd_name
                        ; sig_args   = L.map (fun x -> x.ty) m.mthd_args }
  | Default     ->  Eth.{ sig_return = []
                        ; sig_name   = "" (* is this a good choice? *)
                        ; sig_args   = []    }

type cntrct_intf =
                        { cntrct_intf_name : string   (* the name of the cntrct. *)
                        ; cntrct_intf_args : ty list
                          (* Since [cntrct_intf_args] contains bool[address] and such,
                           * is's not appropriate to use the ABI signature here.
                           * As a work around, at the time of deployment, these arrays are zeroed out. *)
                        ; cntrct_intf_mthds : mthd_intf list
                        ; cntrct_intf_conts : string list (** this lists the names of cntrcts that this one can continue into *) }



(*********************************)
(*   Collect Continuation :      *)
(*    contract becomes what ?    *)
(*********************************)

let rec collect_cont_in_stmt = function 
    | SmAbort             ->  []
    | SmReturn r          ->  begin
         match cntrct_name_of_ret_cont r.ret_cont with
         | None                 -> []
         | Some name            -> [name]   end
    | SmAssign (_,_)      ->  []
    | SmVarDecl _         ->  []
    | SmSelfDestruct _    ->  []
    | SmIfThen (_,s)      ->  collect_cont_in_stmts s
    | SmIfThenElse (_,s,t)    ->  collect_cont_in_stmts s @ collect_cont_in_stmts t
    | SmExpr _            ->  []
    | SmLog _             ->  []  

and collect_cont_in_stmts s =  L.concat (L.map collect_cont_in_stmt s)

let collect_cont_in_mthd (raw : 'expr mthd) : string list =
    L.concat (L.map collect_cont_in_stmt raw.mthd_body)

let collect_cont_in_cntrct (raw : 'expr cntrct) : string list =
    L.concat (L.map collect_cont_in_mthd raw.mthds)




(***********************************)
(*   generate contract Interface   *)
(***********************************)


let cntrct_intf_of (raw : 'expr cntrct) : cntrct_intf =
    { cntrct_intf_name     = raw.cntrct_name
    ; cntrct_intf_args     = L.map (fun x -> x.ty) raw.cntrct_args
    ; cntrct_intf_mthds    = L.map mthd_intf_of raw.mthds
    ; cntrct_intf_conts    = collect_cont_in_cntrct raw  }


(***********************************)
(* getInfo from contract Interface *)
(***********************************)


let find_mthd_sig_in_cntrct m_name (i : cntrct_intf) : mthd_intf option =
    filter_getFst (fun mi -> if mi.Eth.sig_name = m_name then Some mi else None) i.cntrct_intf_mthds

let find_mthd_sig intfs cn_name mthd_name = 
    filter_getFst (find_mthd_sig_in_cntrct mthd_name) (L.map snd intfs)



