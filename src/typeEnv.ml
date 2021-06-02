
open Printf 
open Syntax
open ContractId
open Misc
(** The first element is the context for the innermost block *)

type ty_env                     = 
    { idents                    : arg list list
    ; events                    : event list
    ; retTyCheck_fn             : (ty option -> bool) option    }

let empty_ty_env                =
    { idents                    = []
    ; events                    = []
    ; retTyCheck_fn             = None  }

let forget_innermost tyenv      =   { tyenv with idents = List.tl tyenv.idents    }
let add_empty_block  tyenv      =   { tyenv with idents = [] :: tyenv.idents      }
let add_pair tyenv id ty loc    =   match tyenv.idents with
    | t::ts   -> { tyenv with idents = ({id=id;ty=ty;loc=loc}::t)::ts}
    | _       -> err "no current scope in type env"

let lookup_block name blk       =   filter_getFst (fun a -> if a.id=name then Some(a.ty,a.loc) else None) blk

let lookup env name             =   filter_getFst (lookup_block name) env.idents
let add_block h tenv            =   { tenv with idents = h :: tenv.idents }
let lookup_event tenv name      =
    try   BatList.find (fun e->e.event_name = name) tenv.events
    with  Not_found -> eprintf "Unknown Event %s\n" name; raise Not_found

let add_events evs tyenv        =   { tyenv with events = (values evs) @ tyenv.events }
let set_retTyCheck tyenv tyChk  =  match tyenv.retTyCheck_fn with
    | Some _            -> err "Trying to overwrite the expectations about the return values"
    | None              -> { tyenv with retTyCheck_fn = Some tyChk }

let lookup_retTyCheck tyenv     =   match tyenv.retTyCheck_fn with
    | None              -> failwith "undefined"
    | Some tyChk        -> tyChk
