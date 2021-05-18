open Printf 
(** The first element is the context for the innermost block *)
type type_env                   = 
    { identifiers                   : Syntax.arg list list
    ; events                        : Syntax.event list
    ; expected_returns              : (Syntax.typ option -> bool) option    }

let empty_type_env              =
    { identifiers                   = []
    ; events                        = []
    ; expected_returns              = None  }

let forget_innermost orig       =   { orig with identifiers = List.tl (orig.identifiers) }
let add_empty_block orig        =   { orig with identifiers = [] :: orig.identifiers }
let add_pair orig ident typ loc =   match orig.identifiers with
    | h::t    -> { orig with identifiers = (Syntax.{arg_ident=ident;arg_typ=typ;arg_location=loc}::h)::t}
    | _       -> failwith "no current scope in type env"

let lookup_block name block     =   Misc.first_some
    (fun a -> if a.Syntax.arg_ident=name then Some(a.Syntax.arg_typ,a.Syntax.arg_location) else None) block

let lookup env name             =   Misc.first_some (lookup_block name) env.identifiers
let add_block h orig            =   { orig with identifiers = h :: orig.identifiers }
let lookup_event env name       =
    try   BatList.find (fun e -> e.Syntax.event_name = name) env.events
    with  Not_found   -> eprintf "event %s not found\n" name; raise Not_found

let add_events events orig      =   { orig with events = (Assoc.values events) @ orig.events }
let remember_expected_returns orig f    =  match orig.expected_returns with
    | Some _    -> failwith "Trying to overwrite the expectations about the return values"
    | None      -> { orig with expected_returns = Some f }

let lookup_expected_returns t   =   match t.expected_returns with
    | None      -> failwith "undefined"
    | Some f    -> f
