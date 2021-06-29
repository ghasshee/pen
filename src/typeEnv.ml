
open Printf 
open Syntax
open IndexedList
open Misc

module BL = BatList

(* var list         := local variables in a scope       *)
(* var list list    := the whole variables in src code  *) 
type context                    = 
    { ids                       :   var  list list
    ; evnts                     :   evnt list
    ; retTyChecker              :   (ty option -> bool) option    }


let empty_ctx                   =
    { ids                       =   []
    ; evnts                     =   []
    ; retTyChecker              =   None  }


let lookup_block name blk       =   getFstFilter (fun a -> if a.id=name then Some a.ty else None) blk
let lookup env   name           =   getFstFilter (lookup_block name) env.ids
let lookup_evnt  name ctx       =   BL.find (fun e->e.evnt_name=name) ctx.evnts
let lookup_retTyCheck ctx       =   match ctx.retTyChecker with
    | None                      ->  err "undefined"
    | Some tyChk                ->  tyChk


let add_block h    ctx          =   { ctx with ids          = h :: ctx.ids }
let add_evnts      ctx evs      =   { ctx with evnts        = values evs @ ctx.evnts }
let add_var ctx id ty           =   match ctx.ids with
    | t :: ts                   ->  { ctx with ids          = ({id=id;ty=ty}::t)::ts }
    | _                         ->  err"no current scope"
let add_retTyCheck ctx tyChk    =   match ctx.retTyChecker with
    | None                      ->  { ctx with retTyChecker = Some tyChk }
    | Some _                    ->  err"Do not Overwrite ret-ty-checker"
