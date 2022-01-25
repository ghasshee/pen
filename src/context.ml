
open    Misc
open    Syntax
open    Location
open    Label

module  L   = List
module  S   = String
module  BL  = BatList


(*******************************************)
(***              CONTEXT                ***)
(*******************************************)

type context                    = bind list 
 and bind                       = BdName    of str       (* Parser Context *) 
                                | BdTy      of str * ty 
                                | BdRet     of ty 
                                | BdFrm     of context
                                | BdEv      of str * ty list 
                                | BdLoc     of str * location 
                                | BdI       of ty tmty 
                                | BdBrj     of context 
                                | BdRec     of label   (* BdRec(start) *) 
                                | BdRecName of str
                                | BdStruct  of str 
                            
let empty_ctx                   =   [] 
let add_empty_ctx ctx           =   BdFrm[] :: ctx
let add_empty_brj ctx           =   BdBrj[] :: ctx

let rec lookup_bruijn_idx nm    =   function 
    | []                        ->  (*eprintf"Context: bruijn idx for %s not found\n" nm;*) raise Not_found
    | BdName x :: xs            ->  if x=nm then 0 else 1+(lookup_bruijn_idx nm xs) 
    | _ :: xs                   ->  1 + lookup_bruijn_idx nm xs 

let add_bruijn_idx ctx x        =   BdName x :: ctx 

let rec lookup_rec_idx nm       =   function 
    | []                        ->  raise Not_found
    | BdRecName x :: xs         ->  if x=nm then 0 else 1+(lookup_rec_idx nm xs) 
    | _ :: xs                   ->  1 + lookup_rec_idx nm xs 

let add_rec_idx ctx x           =   BdRecName x :: ctx 

let rec lookup_struct_idx nm    =   function 
    | []                        ->  raise Not_found
    | BdStruct x :: xs          ->  if x=nm then 0 else 1+(lookup_struct_idx nm xs)
    | _ :: xs                   ->  1 + lookup_struct_idx nm xs 

let add_struct_idx ctx x        =   BdStruct x :: ctx 

let rec lookup_recursion_param  =   function 
    | []                        ->  pe"lookup_recursion_param: lookup failed"; raise Not_found
    | (BdRec(start))::rest      ->  start 
    | _ :: rest                 ->  lookup_recursion_param rest 

let add_recursion_param ctx start=  BdRec(start) :: ctx

let lookup_id_local   nm        =   find_by_filter (function BdTy(id,ty)when id=nm -> ty          | _ -> raise Not_found) 
let lookup_id         nm        =   find_by_filter (function BdFrm ctx -> lookup_id_local nm ctx  | _ -> raise Not_found)
let lookup_evnt       nm        =   find_by_filter (function BdEv(id,l) when id=nm -> TyEv(id,l)  | _ -> raise Not_found)
let lookup_retTy                =   find_by_filter (function BdRet ty -> ty                       | _ -> raise Not_found) 
let lookup_ll key               =   find_by_filter (function BdLoc(s,loc) when key=s -> loc       | _ -> raise Not_found)
let lookup_le key               =   find_by_filter (function BdFrm ctx -> lookup_ll key ctx       | _ -> raise Not_found)

let rec lookup_brjidx_local idx =   function 
    | []                        ->  raise Not_found
    | BdI(tm)::rest when idx=0  ->  tm 
    | _::rest                   ->  lookup_brjidx_local (idx-1) rest
let lookup_brjidx       idx     =   find_by_filter (function BdBrj ctx -> lookup_brjidx_local idx ctx| _ -> raise Not_found)


let bind_of_ty                  =   function 
    | TyEv(id,args)             ->  BdEv(id,args)
    | TyVar(id,ty)              ->  BdTy(id,ty)
let binds_of_tys                =   L.map bind_of_ty

let add_local ctx local         =   BdFrm   local :: ctx 
let add_retTy ctx retTy         =   BdRet retTy :: ctx 
let add_evnts ctx evs           =   foldl (fun xs x -> (L.cons $ bind_of_ty) x xs) ctx evs  

let rec (@@) (id,ty)            =   function   
    | []                        ->  err "add_var: no current scope" 
    | BdFrm local:: rest        ->  BdFrm (BdTy(id,ty)::local) :: rest 
    | x :: rest                 ->  x ::  ((id,ty) @@ rest)

let rec add_brjidx ctx tm       =   match ctx with 
    | []                        ->  err "add_brjidx: no current scope" 
    | BdBrj local:: rest        ->  BdBrj (BdI(tm)::local) :: rest
    | x :: rest                 ->  x :: add_brjidx rest tm 

(****************************************************)
(***          arg locations of mthd               ***)
(****************************************************)

let positions_of_argLens lens   =
    let rec loop ret used           =   function 
        | []                        ->  L.rev ret
        | alen::rest                ->  assert (0<alen&&alen<=32);
                                        loop (used+32-alen :: ret) (used+32) rest in 
    loop [] 4(* signature length *) lens

let callerArgLocs_of_mthd       =   function 
    | TmMthd(TyDefault,_)           ->  []
    | TmMthd(TyMthd(id,args,ret),_) ->  let sizes       = L.map calldata_size_of_arg args   in
                                        let positions   = positions_of_argLens sizes        in
                                        let size_pos    = L.combine positions sizes         in
                                        let locations   = L.map calldata size_pos           in
                                        let names       = L.map id_of_var args              in
                                                          L.combine names locations       

(************************************************)
(**         LL := LOCAL    LOCATIONs           **)
(**        ctx := LOCATION ENVIRONMENTS        **) 
(************************************************)
    
let rec add_loc ctx (key,loc)        = match ctx with
    | []                            -> err "add_loc: no block"
    | BdFrm(h)::t                   -> BdFrm(BdLoc(key,loc) :: h) :: t
    | _ :: rest                     -> add_loc rest (key,loc)
let add_locs ctx                     =   foldl add_loc ctx 
let add_mthdCallerArgLocs mthd ctx   =   add_locs ctx (callerArgLocs_of_mthd mthd)

let add_fieldVar(ctx,idx)(TyVar(id,ty))    =
    let size                = size_of_ty ty                             in
    let size                = if 0<size&&size<=32 then 1 else size/32   in 
    let loc                 = Stor{offst=Int idx;size=Int size}         in
    let ctx                 = add_loc ctx (id,loc)                      in
    ctx , idx + size  

let add_fieldArr(ctx,idx)(TyVar(id,TyMap(_,_))) =
    let size                = 1                                         in 
    let loc                 = Stor{offst=Int idx;size=Int size}         in
    let ctx                 = add_loc ctx (id,loc)                      in
    ctx , idx + size  

let rn_init_ctx cn =
    let varTys              = varTys_of_cn cn                           in
    let arrTys              = arrTys_of_cn cn                           in  
    let ctx                 = add_empty_ctx empty_ctx                   in
    let ctx, mid            = foldl add_fieldVar (ctx,2) varTys         in  
    let ctx, _              = foldl add_fieldArr (ctx,mid) arrTys       in
    ctx









let str_of_bind                 =   function 
    | BdRec(l)                  ->  sf "BdRec(label %d)" l 
    | BdBrj ctx                 ->  sf "BdBrj %s" "local" 
    | BdLoc(s,l)                ->  sf "BdLoc(%s,location)" s 
    | BdName s                  ->  sf "BdName(%s)"         s 
    | BdRecName s               ->  sf "BdRecName(%s)"      s 
    | BdStruct s                ->  sf "BdStruct(%s)"       s 
    | BdFrm ctx                 ->  sf "BdFrm %s" "local" 
    | _                         ->  sf "Bd" 

let rec str_of_ctx              =   function 
    | []                        ->  ""
    | x::xs                     ->  str_of_bind x ^ "," ^ str_of_ctx xs 

let prBd                        =   function 
    | BdName str                ->  pf "BdName(%s) "    str 
    | BdRecName str             ->  pf "BdRecName(%s) " str
    | BdStruct str              ->  pf "BdStruct(%s) "  str 

let prBds                       =   L.iter prBd 

