(* cont := continuation *) 


open Misc
open Syntax 

module Eth = Ethereum 
module L   = List

type mthd_interface     = Eth.function_signature

let mthd_intf_of raw    = match raw.mthd_head with
  | Method m    ->  { Eth.sig_return = L.map Eth.intf_ty_of_ty m.mthd_ret_ty
                        ; sig_name   = m.mthd_name
                        ; sig_args   = L.map Eth.intf_ty_of_ty(L.map(fun x -> x.ty)m.mthd_args)}
  | Default     ->  { Eth.sig_return = []
                        ; sig_name   = "" (* is this a good choice? *)
                        ; sig_args   = []    }

type cntrct_interface =
                        { cntrct_intf_name : string   (* the name of the cntrct. *)
                        ; cntrct_intf_args : ty list
                          (* Since [cntrct_intf_args] contains bool[address] and such,
                           * is's not appropriate to use the ABI signature here.
                           * As a work around, at the time of deployment, these
                           * arrays are zeroed out.
                           *)
                        ; cntrct_intf_mthds : mthd_interface list
                        ; cntrct_intf_conts : string list
                          (** [cntrct_intf_transitions] lists the names of cntrcts that
                              this one can continue into *)
                        }

let rec collect_cont_in_stmt = function 
    | AbortStmt             ->  []
    | ReturnStmt r          ->  begin
         match cntrct_name_of_ret_cont r.ret_cont with
         | None                 -> []
         | Some name            -> [name]   end
    | AssignStmt (_, _)     ->  []
    | VarDeclStmt _         ->  []
    | SelfDestructStmt _    ->  []
    | IfThenOnly (_, s)     ->  collect_cont_in_stmts s
    | IfThenElse (_, s, t)  ->  collect_cont_in_stmts s @ collect_cont_in_stmts t
    | ExprStmt _            ->  []
    | LogStmt _             ->  []  

and collect_cont_in_stmts s =  L.concat (L.map collect_cont_in_stmt s)

let collect_cont_in_mthd (raw : 'expr mthd) : string list =
    L.concat (L.map collect_cont_in_stmt raw.mthd_body)

let collect_cont_in_cntrct (raw : 'expr cntrct) : string list =
    L.concat (L.map collect_cont_in_mthd raw.mthds)

let cntrct_intf_of (raw : 'expr cntrct) : cntrct_interface =
    { cntrct_intf_name     = raw.cntrct_name
    ; cntrct_intf_args     = L.map (fun x -> x.ty) raw.cntrct_args
    ; cntrct_intf_mthds    = L.map mthd_intf_of raw.mthds
    ; cntrct_intf_conts    = collect_cont_in_cntrct raw  }

let find_method_sig_in_cntrct (m_name : string) (i : cntrct_interface) : mthd_interface option =
    fst_some (fun mi -> if mi.Eth.sig_name = m_name then Some mi else None) i.cntrct_intf_mthds

let find_method_signature interfaces cntrct_name method_name = 
    fst_some (find_method_sig_in_cntrct method_name) (L.map snd interfaces)
