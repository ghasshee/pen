
open    Printf 
open    Misc
open    Syntax
open    IndexList
open    Location

module  L   = List
module  BL  = BatList


(****************************************************)
(***              CONTEXT                         ***)
(****************************************************)

(* var list         := local variables in a scope       *)
(* var list list    := the whole variables in src code  *) 
type context                    =   { ids       :   ty  list list
                                    ; evnts     :   tyEvnt list
                                    ; retTyChkr :   (ty option->bool) option    }
    
let empty_ctx                   =   { ids       =   []
                                    ; evnts     =   []
                                    ; retTyChkr =   None    }


let lookup_block name blk       =   getFstByFilter (function TyVar(id,ty) -> if id=name then Some ty else None) blk
let lookup_id    name ctx       =   getFstByFilter (lookup_block name) ctx.ids
let lookup_evnt  name ctx       =   BL.find (fun (ev:tyEvnt)->ev.id=name) ctx.evnts
let lookup_retTyCheck ctx       =   match ctx.retTyChkr with
    | Some chkr                 ->  chkr
    | None                      ->  err "undefined"

let add_block ctx  blk          =   { ctx with ids          =   blk :: ctx.ids              }
let add_evnts ctx evs           =   { ctx with evnts        =   values evs @ ctx.evnts      }
let add_var   ctx id ty         =   match ctx.ids with
    | t :: ts                   ->  { ctx with ids          =   (TyVar(id,ty)::t) :: ts    }
    | _                         ->  err"no current scope"
let add_retTyChkr ctx tyChk     =   match ctx.retTyChkr with
    | None                      ->  { ctx with retTyChkr    =   Some tyChk                  }
    | Some _                    ->  err"Don't Overwrite ret-ty-checker"


(****************************************************)
(***              TYPED TERM                      ***)
(****************************************************)
    
type argTy                      =   string * ty 
type argArr                     =   string * ty * ty 

let get_ty  (_,ty)              =   ty
let get_tm  (x,_)               =   x

let argTy_of_var                =   function 
    | TyVar(_,TyMap (_,_))      ->  None
    | TyVar(id,ty)              ->  Some(id,ty)
let arrTy_of_var                =   function 
    | TyVar(id,TyMap(kTy,vTy))  ->  Some (id, kTy, vTy)
    | TyVar(_,_)                ->  None
let argTys_of_vars              =   BL.filter_map argTy_of_var 
let arrTys_of_cntrct cn         =   BL.filter_map arrTy_of_var (cn.cntrct_args)
let argTys_of_cntrct cn         =   argTys_of_vars cn.cntrct_args

let positions_of_argLens lens   =
    let rec loop ret used           =   function 
        | []                        ->  L.rev ret
        | alen::rest                ->  assert (alen>0 && alen<=32);
                                        loop (used+32-alen::ret) (used+32) rest in 
    loop [] 4(* signature length *) lens

let argLocs_of_mthd m           =   match m.mthd_head with
    | TyDefault                 ->  []
    | TyMethod(id,args,ret)     ->  let sizes       = L.map calldata_size_of_arg args in
                                    let positions   = positions_of_argLens sizes in
                                    let size_pos    = L.combine positions sizes in
                                    let locations   = L.map (fun(o,s)->Calldata{calldata_start=o;calldata_size=s}) size_pos in
                                    let names       = L.map id_of_var args in
                                    let locEnv      = L.combine names locations in
                                    locEnv

let total_size_of_argTys tys    =   try     BL.sum (L.map size_of_ty tys) 
                                    with    Invalid_argument _          -> 0
let total_size_of_args args     =   try     BL.sum (L.map (size_of_ty $ ty_of_var) args)
                                    with    Invalid_argument _          -> 0 

let string_of_tyMthd (TyMethod(id,args,ret))  =
    let argTys          = L.map snd (argTys_of_vars args)   in
    let strTys          = L.map string_of_ty argTys         in
    let tys             = String.concat "," strTys          in
    id    ^ "(" ^ tys ^ ")"

let string_of_evnt (ev:tyEvnt) =
    let args            = args_of_evnt_args ev.tyEvArgs     in 
    let argTys          = L.map snd (argTys_of_vars args)   in
    let strTys          = L.map string_of_ty argTys         in
    let tys             = String.concat "," strTys          in
    ev.id ^ "(" ^ tys ^ ")"

(***********************************)
(* getInfo from contract Interface *)
(***********************************)

let find_tyMthd_in_cntrct mname tyCntrct : ty option =
    getFstByFilter (function TyMethod(id,args,ret) as tyM -> if id=mname then Some tyM else None) tyCntrct.tyCnMthds

let find_tyMthd tyCntrcts mname = 
    match getFstByFilter (find_tyMthd_in_cntrct mname) (L.map snd tyCntrcts) with 
    | Some ty   -> ty
    | None      -> err ("find_tyMthd: "^mname^"Not found")

