
open Printf 
open Misc
open Syntax
open IndexList
open Location

module L    = List
module BL   = BatList


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
let lookup_id    name ctx       =   getFstFilter (lookup_block name) ctx.ids
let lookup_evnt  name ctx       =   BL.find (fun e->e.evnt_name=name) ctx.evnts
let lookup_retTyCheck ctx       =   match ctx.retTyChecker with
    | Some chkr                 ->  chkr
    | None                      ->  err "undefined"

let add_block ctx  blk          =   { ctx with ids          = blk :: ctx.ids }
let add_evnts ctx evs           =   { ctx with evnts        = values evs @ ctx.evnts }
let add_var   ctx id ty         =   match ctx.ids with
    | t :: ts                   ->  { ctx with ids          = ({id=id;ty=ty}::t)::ts }
    | _                         ->  err"no current scope"
let add_retTyCheck ctx tyChk    =   match ctx.retTyChecker with
    | None                      ->  { ctx with retTyChecker = Some tyChk }
    | Some _                    ->  err"Do not Overwrite ret-ty-checker"


    
type argTy                      =   string * ty 
type argArr                     =   string * ty * ty 

let get_ty  (_,ty)              =   ty
let get_tm  (x,_)               =   x

let getTy_var var               =   match var.ty with
    | TyMap (_,_)               ->  None
    | _                         ->  Some(var.id,var.ty)
let getTy_vars                  =   BL.filter_map getTy_var

let getArr_var var              =   match var.ty with
    | TyMap(k,v)                ->  Some (var.id, k, v)
    | _                         ->  None
let getArr_cntrct cn            =   BL.filter_map getArr_var (cn.cntrct_args)

let positions_of_argLens lens   =
    let rec loop ret used           =   function 
        | []                        ->  L.rev ret
        | alen::rest                ->  assert (alen>0 && alen<=32);
                                        loop (used+32-alen::ret) (used+32) rest in 
    loop [] 4(* signature length *) lens

let argLocs_of_mthd m           =   match m.mthd_head with
    | Default                   ->  []
    | Method h                  ->  let sizes       = L.map calldata_size_of_arg h.mthd_args in
                                    let positions   = positions_of_argLens sizes in
                                    let size_pos    = L.combine positions sizes in
                                    let locations   = L.map (fun(o,s)->Calldata{calldata_start=o;calldata_size=s}) size_pos in
                                    let names       = L.map (fun a -> a.id) h.mthd_args in
                                    let locEnv      = L.combine names locations in
                                    locEnv

let argTys_of_cntrct cn         =   getTy_vars cn.cntrct_args

let total_size_of_argTys tys    =   try     BL.sum (L.map size_of_ty tys) 
                                    with    Invalid_argument _          -> 0
let total_size_of_args args     =   try     BL.sum (L.map (fun arg-> size_of_ty arg.ty) args)
                                    with    Invalid_argument _          -> 0 




type tyMthd                     =   { tyRet       : ty 
                                    ; name        : string
                                    ; tyArgs      : ty list }

let typeof_mthd m               = match m.mthd_head with
  | Method m                    ->  { tyRet     = m.mthd_retTy
                                    ; name      = m.mthd_name
                                    ; tyArgs    = L.map (fun x->x.ty) m.mthd_args }
  | Default                     ->  { tyRet     = TyTuple[]
                                    ; name      = "" 
                                    ; tyArgs    = []    }

type tyCntrct                   =   { tyCntrct_name     : string   
                                    ; tyCntrct_args     : ty list
                                    ; tyCntrct_mthds    : tyMthd list
                                    ; tyCntrct_conts    : string list  }

     (* Since [tyCntrct_args] contains bool[address] and such,
      * is's not appropriate to use the ABI signature here.
      * As a work around, at the time of deployment, these arrays are zeroed out. *)


(*********************************)
(*   Collect Continuation :      *)
(*    contract becomes what ?    *)
(*********************************)

let rec collect_cont_stmt       = function 
    | SmAbort                   ->  []
    | SmSlfDstrct _             ->  []
    | SmExpr _                  ->  []
    | SmAssign (_,_)            ->  []
    | SmDecl _                  ->  []
    | SmIfThen (_,s)            ->  collect_cont_stmts s
    | SmIf (_,s,t)              ->  collect_cont_stmts s @ collect_cont_stmts t
    | SmLog _                   ->  []  
    | SmReturn r                ->  begin match cntrct_name_of_ret_cont r.ret_cont with
         | None                     -> []
         | Some name                -> [name]   end
and collect_cont_stmts s        =   L.concat (L.map collect_cont_stmt s)
let collect_cont_mthd   raw     =   L.concat (L.map collect_cont_stmt raw.mthd_body)
let collect_cont_cntrct raw     =   L.concat (L.map collect_cont_mthd raw.mthds)


(***********************************)
(*   generate contract Interface   *)
(***********************************)

let typeof_cntrct cn            =   { tyCntrct_name     = cn.cntrct_name
                                    ; tyCntrct_args     = L.map (fun x -> x.ty) cn.cntrct_args
                                    ; tyCntrct_mthds    = L.map typeof_mthd cn.mthds
                                    ; tyCntrct_conts    = collect_cont_cntrct cn                }
    

(***********************************)
(* getInfo from contract Interface *)
(***********************************)


let find_tyMthd_in_cntrct mname tyCntrct : tyMthd option =
    getFstFilter (fun mi -> if mi.name=mname then Some mi else None) tyCntrct.tyCntrct_mthds

let find_tyMthd tyCntrcts mname = 
    match getFstFilter (find_tyMthd_in_cntrct mname) (L.map snd tyCntrcts) with 
    | Some ty   -> ty
    | None      -> err ("find_tyMthd: "^mname^"Not found")



