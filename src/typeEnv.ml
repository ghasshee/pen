
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

(* var list         := local variables in a method scope *)
(* var list list    := the whole variables in src code   *) 

type bind                       = BdName  of string       (* Parser Context *) 
                                | BdTy    of string * ty 
                                | BdRetTy of ty 
                                | BdCtx   of context
                                | BdEvnt  of tyEvnt 

and  context                    = bind list 

let empty_ctx                   = [] 

let lookup_id_locally name      = function 
    | BdCtx local                   ->  getFstByFilter(function | BdTy(id,ty) when id=name  -> Some ty 
                                                                | _                         -> None     ) local
    | _                             ->  None  

let lookup_id name ctx          = getFstByFilter (lookup_id_locally name) ctx 

let rec lookup_evnt nm          = function 
    | BdEvnt(e)::_ when e.id=nm     -> e
    | _::xs                         -> lookup_evnt nm xs

let rec lookup_retTy            = function 
    | BdRetTy(ty)::_                -> ty 
    | _::xs                         -> lookup_retTy xs 

let bind_of_arg (TyVar(id,ty))  =   BdTy(id,ty)
let binds_of_args               =   L.map bind_of_arg

let add_block ctx local         =   BdCtx(local) :: ctx 

let rec add_evnts ctx           =   function 
    | []                            -> ctx
    | e::es                         -> BdEvnt e :: add_evnts ctx es

let rec add_var  ctx id ty      =   match ctx with 
    | []                            -> err "no current scope" 
    | BdCtx local:: rest            -> BdCtx (BdTy(id,ty)::local) :: rest 
    | _ :: rest                     -> add_var rest id ty

let rec add_retTy ctx retTy     = BdRetTy retTy :: ctx 



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


(***********************************)
(***      Type Equivalence       ***)
(***********************************)

let tyeqv t0 t1                 =   ( t0 = t1 )  ||  ( match t0, t1 with
                                | TyAddr, TyInstnce _   -> true
                                | _     , _             -> false ) 

