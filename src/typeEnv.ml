
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
type context                    =   { ids       :   tyVar  list list
                                    ; evnts     :   tyEvnt list
                                    ; retTyChkr :   (ty option->bool) option    }
    
let empty_ctx                   =   { ids       =   []
                                    ; evnts     =   []
                                    ; retTyChkr =   None    }


let lookup_block name blk       =   getFstFilter (fun (var:tyVar)-> if var.id=name then Some var.ty else None) blk
let lookup_id    name ctx       =   getFstFilter (lookup_block name) ctx.ids
let lookup_evnt  name ctx       =   BL.find (fun (ev:tyEvnt)->ev.id=name) ctx.evnts
let lookup_retTyCheck ctx       =   match ctx.retTyChkr with
    | Some chkr                 ->  chkr
    | None                      ->  err "undefined"

let add_block ctx  blk          =   { ctx with ids          =   blk :: ctx.ids              }
let add_evnts ctx evs           =   { ctx with evnts        =   values evs @ ctx.evnts      }
let add_var   ctx id ty         =   match ctx.ids with
    | t :: ts                   ->  { ctx with ids          =   ({id=id;ty=ty}::t) :: ts    }
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

let getArgTy  var               =   match var.ty with
    | TyMap (_,_)               ->  None
    | _                         ->  Some(var.id,var.ty)
let getArrTy   var              =   match var.ty with
    | TyMap(kTy,vTy)            ->  Some (var.id, kTy, vTy)
    | _                         ->  None
let getArgTys                   =   BL.filter_map getArgTy 
let getArrTy_cntrct cn          =   BL.filter_map getArrTy (cn.cntrct_args)

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
                                    let names       = L.map (fun (arg:tyVar)->arg.id) h.mthd_args in
                                    let locEnv      = L.combine names locations in
                                    locEnv

let argTys_of_cntrct cn         =   getArgTys cn.cntrct_args

let total_size_of_argTys tys    =   try     BL.sum (L.map size_of_ty tys) 
                                    with    Invalid_argument _          -> 0
let total_size_of_args args     =   try     BL.sum (L.map (fun arg-> size_of_ty arg.ty) args)
                                    with    Invalid_argument _          -> 0 


let string_of_tyMthd m  =
    let name_of_mthd    = m.mthd_id                         in
    let args            = getArgTys m.mthd_args             in
    let arg_tys         = L.map snd args                    in
    let str_tys         = L.map string_of_ty arg_tys        in
    let ty              = String.concat "," str_tys         in
    name_of_mthd ^ "(" ^ ty ^ ")"

let string_of_evnt (ev:tyEvnt) =
    (* do I consider indexed no? *)
    let name            = ev.id                             in
    let args            = args_of_evnt_args ev.tyEvArgs     in 
    let argTys          = getArgTys args                    in
    let tys             = L.map snd argTys                  in
    let tyNames         = L.map string_of_ty tys            in
    let args            = String.concat "," tyNames         in
    name ^ "(" ^ args ^ ")"

(***********************************)
(* getInfo from contract Interface *)
(***********************************)

let find_tyMthd_in_cntrct mname tyCntrct : tyMthd option =
    getFstFilter (fun (tyM:tyMthd) -> if tyM.id=mname then Some tyM else None) tyCntrct.tyCnMthds

let find_tyMthd tyCntrcts mname = 
    match getFstFilter (find_tyMthd_in_cntrct mname) (L.map snd tyCntrcts) with 
    | Some ty   -> ty
    | None      -> err ("find_tyMthd: "^mname^"Not found")

