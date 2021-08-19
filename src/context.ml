
open    Printf 
open    Misc
open    Syntax
open    Location

module  L   = List
module  S   = String
module  BL  = BatList


(*******************************************)
(***              CONTEXT                ***)
(*******************************************)

(* var list         := local variables in a method scope *)
(* var list list    := the whole variables in src code   *) 

type context                    = bind list 
 and bind                       = BdName  of string       (* Parser Context *) 
                                | BdTy    of string * ty 
                                | BdRetTy of ty 
                                | BdCtx   of context
                                | BdEv    of string * ty list 
                                | BdLoc   of string * location 
type le                         =   context 

let empty_ctx                   = [] 
let add_empty_ctx ctx           = BdCtx[] :: ctx

let prBd                        = function 
    | BdName str                    -> printf "BdName(%s) " str 

let prBds                       = L.iter prBd 

let rec lookup_bruijn_idx nm    = function 
    | []                            -> (*eprintf"Context: bruijn idx for %s not found\n" nm;*) raise Not_found
    | BdName x :: xs                -> if x=nm then 0 else 1+(lookup_bruijn_idx nm xs) 

let add_bruijn_idx ctx x        = BdName x :: ctx 


let lookup_id_local   nm        = find_by_filter (function BdTy(id,ty)when id=nm -> ty          | _ -> raise Not_found) 
let lookup_id         nm        = find_by_filter (function BdCtx ctx -> lookup_id_local nm ctx  | _ -> raise Not_found)
let lookup_evnt       nm        = find_by_filter (function BdEv(id,l) when id=nm -> TyEv(id,l)  | _ -> raise Not_found)
let lookup_retTy                = find_by_filter (function BdRetTy ty -> ty                     | _ -> raise Not_found) 
let lookup_ll key               = find_by_filter (function BdLoc(s,loc) when key=s -> loc       | _ -> raise Not_found)
let lookup_le key               = find_by_filter (function BdCtx ctx -> lookup_ll key ctx       | _ -> raise Not_found)


let bind_of_ty                  = function 
    | TyEv(id,args)             ->  BdEv(id,args)
    | TyVar(id,ty)              ->  BdTy(id,ty)
let binds_of_tys               =   L.map bind_of_ty

let add_local ctx local         =   BdCtx   local :: ctx 
let add_retTy ctx retTy         =   BdRetTy retTy :: ctx 
let add_evnts ctx evs           =   foldl (fun xs x -> (L.cons $ bind_of_ty) x xs) ctx evs  

let rec add_var  ctx id ty      =   match ctx with 
    | []                            -> err "no current scope" 
    | BdCtx local:: rest            -> BdCtx (BdTy(id,ty)::local) :: rest 
    | _ :: rest                     -> add_var rest id ty

(************************************************)
(**         LL := LOCAL    LOCATIONs           **)
(**         LE := LOCATION ENVIRONMENTS        **) 
(************************************************)
    
let add_loc le (key,loc)        =   match le with
    | []                            -> err "add_loc: no block"
    | BdCtx(h)::t                   -> BdCtx(BdLoc(key,loc) :: h) :: t
let add_locs le locs            =   foldl add_loc le locs
let add_mthdArgLocs mthd le     =   add_locs le (argLocs_of_mthd mthd)

let add_fieldVar(le,idx)(TyVar(nm,ty))    =
    let size                = size_of_ty ty                             in
    let size                = if 0<size&&size<=32 then 1 else size/32   in 
    let loc                 = Stor{offst=Int idx;size=Int size}         in
    let le'                 = add_loc le (nm,loc)                       in
    le' , idx + size  
let add_fieldArr(le,idx)(TyVar(nm,TyMap(_,_))) =
    let size                = 1                                         in 
    let loc                 = Stor{offst=Int idx;size=Int size}         in
    let le'                 = add_loc le (nm,loc)                       in
    le' , idx + size  

let rntime_init_le (cn:ty cntrct) =
    let varTys              = varTys_of_cn cn                           in
    let arrTys              = arrTys_of_cn cn                           in  
    let init                = add_empty_ctx empty_ctx                   in
    let le, mid             = foldl add_fieldVar (init,2) varTys        in  
    let le, _               = foldl add_fieldArr (le,mid) arrTys        in
    le


