
open    Printf 
open    Misc
open    Syntax
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
                                | BdEvnt  of ty 
                                | BdLoc   of string * location 

and  context                    = bind list 

let empty_ctx                   = [] 

let lookup_id_locally name      = function 
    | BdCtx local                   ->  getFstByFilter(function | BdTy(id,ty) when id=name  -> Some ty 
                                                                | _                         -> None     ) local
    | _                             ->  None  

let lookup_id name ctx          = getFstByFilter (lookup_id_locally name) ctx 

let rec lookup_evnt nm          = function  
    | BdEvnt(TyEvnt(id,args))::_ when id=nm     -> TyEvnt(id,args) 
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
let arrTys_of_cntrct cn         =   BL.filter_map arrTy_of_var (cn.fieldss)
let argTys_of_cntrct cn         =   argTys_of_vars cn.fieldss

let argsSize_of_cn cn           =   size_of_tys (L.map get_ty (argTys_of_cntrct cn))  

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
                                    let locations   = L.map (fun(o,s)->Calldata{offst=o;size=s}) size_pos in
                                    let names       = L.map id_of_var args in
                                    let locEnv      = L.combine names locations in
                                    locEnv

let size_of_tys  tys            =   try     BL.sum (L.map size_of_ty tys) 
                                    with    Invalid_argument _          -> 0
let size_of_args args           =   try     BL.sum (L.map (size_of_ty $ ty_of_var) args)
                                    with    Invalid_argument _          -> 0 

let string_of_tyMthd (TyMethod(id,args,ret))  =
    let argTys          = L.map snd (argTys_of_vars args)   in
    let strTys          = L.map string_of_ty argTys         in
    let tys             = String.concat "," strTys          in
    id    ^ "(" ^ tys ^ ")"

let string_of_evnt  = function TyEvnt(id,tyEvArgs) -> 
    let args            = args_of_evnt_args tyEvArgs       in 
    let argTys          = L.map snd (argTys_of_vars args)   in
    let strTys          = L.map string_of_ty argTys         in
    let tys             = String.concat "," strTys          in
    id ^ "(" ^ tys ^ ")"

(***********************************)
(* getInfo from contract Interface *)
(***********************************)

let find_tyMthd_in_cntrct mname = function TyCntrct(_,_,tyCnMthds) -> 
    getFstByFilter (function TyMethod(id,args,ret) as tyM -> if id=mname then Some tyM else None) tyCnMthds

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






(**********************************)
(*   LL := LOCAL    LOCATIONs     *)
(*   LE := LOCATION ENVIRONMENTS  *) 
(**********************************)
type le                             =   context 
let rec size                        =   function 
    | []                                -> 0 
    | BdCtx local::rest                 -> len local + size rest  

let empty_le                        =   []
let add_empty_ll le                 =   BdCtx [] :: le

let rec lookup_loc k (BdCtx(local)) =   match local with  
    | []                                -> None 
    | BdLoc(s,loc)::rest                -> if k=s then Some loc else lookup_loc k (BdCtx(rest)) 
let lookup le key                   =   getFstByFilter (lookup_loc key) le

let add_loc le (key,loc)            =   match le with
    | []                                -> err "add_loc: no block"
    | BdCtx(h)::t                       -> BdCtx(BdLoc(key,loc) :: h) :: t
let add_locs le locs                =   foldl add_loc le locs
let add_mthd_argLocs mthd le        =   add_locs le (argLocs_of_mthd mthd)

let addVar(le,idx)(nm,ty)           =
    let size                = size_of_ty ty                             in
    let size                = if 0<size&&size<=32 then 1 else size/32   in 
    let loc                 = Stor{offst=Int idx;size=Int size}         in
    let le'                 = add_loc le (nm,loc)                       in
    le' , idx + size  
let addArr(le,idx)(nm,_,_)          =
    let size                = 1                                         in 
    let loc                 = Stor{offst=Int idx;size=Int size}         in
    let le'                 = add_loc le (nm,loc)                       in
    le' , idx + size  

let rntime_init_le (cn:ty cntrct) =
    let argTys              = argTys_of_cntrct cn               in
    let arrTys              = arrTys_of_cntrct cn               in  
    let init                = add_empty_ll empty_le             in
    let le, mid             = foldl addVar (init,2) argTys      in  
    let le, _               = foldl addArr (le,mid) arrTys      in
    le

