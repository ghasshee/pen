
open Printf 
open Syntax
open IndexedList
open Misc

module BL = BatList

(* var list         := local variables in a scope       *)
(* var list list    := the whole variables in src code  *) 
type tyEnv                      = 
    { idents                    :   var list list
    ; evnts                     :   evnt list
    ; retTyChecker              :   (ty option -> bool) option    }

let empty_tyEnv                 =
    { idents                    =   []
    ; evnts                     =   []
    ; retTyChecker              =   None  }

let add_var tyenv id ty         =   match tyenv.idents with
    | t :: ts                   ->  { tyenv with idents=({id=id;ty=ty}::t)::ts }
    | _                         ->  err "no current scope in type env"
let lookup_block name blk       =   getFstFilter (fun a -> if a.id=name then Some a.ty else None) blk
let lookup env   name           =   getFstFilter (lookup_block name) env.idents
let lookup_evnt    tyenv name   =   BL.find (fun e->e.evnt_name=name) tyenv.evnts
let add_block h    tyenv        =   { tyenv with idents = h :: tyenv.idents }
let add_evnts      tyenv evs    =   { tyenv with evnts = (values evs) @ tyenv.evnts }
let set_retTyCheck tyenv tyChk  =   match tyenv.retTyChecker with
    | Some _                    ->  err "Trying to overwrite the expectations about the return values"
    | None                      ->  { tyenv with retTyChecker = Some tyChk }
let lookup_retTyCheck tyenv     =   match tyenv.retTyChecker with
    | None                      ->  err "undefined"
    | Some tyChk                ->  tyChk
