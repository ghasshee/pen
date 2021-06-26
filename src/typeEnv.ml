
open Printf 
open Syntax
open IndexedList
open Misc

(** The first element is the context for the innermost block *)

type tyEnv                      = 
    { idents                    : var list list
    ; evnts                     : evnt list
    ; retTyChecker              : (ty option -> bool) option    }

let empty_tyEnv                 =
    { idents                    = []
    ; evnts                     = []
    ; retTyChecker              = None  }

let add_var tyenv id ty      =   match tyenv.idents with
    | t :: ts               ->  {   tyenv   with 
                                    idents  = ({id=id;ty=ty}::t) :: ts }
    | _                     ->  err "no current scope in type env"

let lookup_block name blk       =   getFstFilter (fun a -> if a.id=name then Some a.ty else None) blk

let lookup env name             =   getFstFilter (lookup_block name) env.idents
let add_block h tenv            =   { tenv with idents = h :: tenv.idents }
let lookup_evnt tenv name      =
    try   BatList.find (fun e->e.evnt_name = name) tenv.evnts
    with  Not_found -> eprintf "Unknown Event %s\n" name; raise Not_found

let add_evnts evs tyenv        =   { tyenv with evnts = (values evs) @ tyenv.evnts }
let set_retTyCheck tyenv tyChk  =  match tyenv.retTyChecker with
    | Some _                ->  err "Trying to overwrite the expectations about the return values"
    | None                  ->  { tyenv with retTyChecker = Some tyChk }

let lookup_retTyCheck tyenv     =   match tyenv.retTyChecker with
    | None                  ->  err "undefined"
    | Some tyChk            ->  tyChk
