open Format
open Support

let soi = string_of_int
(* -------------------------------------------------- *) 
(* Datatypes *)

type ty     =
    | TyVar     of int * int 
    | TyId      of string 
    | TyTop
    | TyRef     of ty 
    | TySource  of ty
    | TySink    of ty 
    | TyVariant of (string * ty) list 
    | TyRecord  of (string * ty) list 
    | TyArr     of ty * ty
    | TyList    of ty 
    | TyRec     of string * ty
    | TyFloat 
    | TyString  
    | TyUnit
    | TyBool
    | TyNat 
;;

type term =
    (* Recursive *)
    | TmFold        of info * ty 
    | TmUnfold      of info * ty 
    (* Ref *)
    | TmRef         of info * term 
    | TmDeref       of info * term 
    | TmLoc         of info * int 
    | TmAssign      of info * term * term 
    (* List *)
    | TmNil         of info * ty
    | TmCons        of info * ty * term * term 
    | TmIsNil       of info * ty * term 
    | TmHead        of info * ty * term 
    | TmTail        of info * ty * term 
    (* Fix *)
    | TmFix         of info * term 
    (* Float / String  *) 
    | TmString      of info * string 
    | TmFloat       of info * float
    | TmTimesfloat  of info * term * term 
    (* Variant *)
    | TmTag         of info * string * term * ty 
    | TmCase        of info * term * (string * (string * term)) list  (* <- (label*(variable*term) list *) 
    (* Record *)
    | TmProj        of info * term * string  
    | TmRecord      of info * (string * term) list 
    (* Ascription *) 
    | TmAscribe     of info * term * ty
    (* Unit *)
    | TmUnit        of info 
    (* Let  *)
    | TmLet         of info * string * term * term
    (* Lambda *) 
    | TmVar         of info * int * int 
    | TmAbs         of info * string * ty option * term 
    | TmApp         of info * term * term 
    (* Arith *) 
    | TmZero        of info
    | TmSucc        of info * term
    | TmPred        of info * term
    | TmIsZero      of info * term
    | TmTimes       of info * term * term 
    (* Bool *) 
    | TmTrue        of info
    | TmFalse       of info
    | TmIf          of info * term * term * term
;;

type bind = 
    | BindName
    | BindTyVar
    | BindTmVar     of ty
    | BindTmAbb     of term * (ty option) 
    | BindTyAbb     of ty 

;;

type context = 
    (string * bind) list 
;;

type command =
    | Eval of info * term
    | Bind of info * string * bind 
;;

(* -------------------------------------------------- *) 
(* Type Reconstruction *)

type constr             = (ty * ty) list 
type nextuvar           = NextUVar of string * uvargenerator
and  uvargenerator      = unit -> nextuvar 

let emptyconstr         = []
let combineconstr       = List.append


let uvargen             = 
    let rec f n ()          = NextUVar("?X" ^ string_of_int n, f(n+1))
    in f 0 ;;

(* -------------------------------------------------- *) 
(* Context Management *) 

let emptyctx                    =   []
let ctxlen          ctx         =   List.length ctx
let addbind         ctx x bind  =   (x,bind) :: ctx 
let addname         ctx x       =   addbind ctx x BindName 
let rec isnamebound ctx x       =   match ctx with 
    | []                                -> false
    | (y,_) :: rest                     -> if y=x then true else isnamebound rest x;;
let rec pickfreshname ctx x     =   if isnamebound ctx x 
                                        then pickfreshname ctx (x^"'")
                                        else ((x,BindName)::ctx), x
let     index2name fi ctx n     =   try let (xn,_) = List.nth ctx n in xn 
                                    with Failure _ -> error fi "Variable Lookup Failure"
let rec name2index fi ctx xn    =   match ctx with 
    | []                                -> error fi ("Identifier "^xn^" is unbound")
    | (x,_) :: rest                     -> if x=xn then 0 else 1+(name2index fi rest xn)


(* -------------------------------------------------- *) 
(* Shifting *)

let rec tyWalk onVar c          = let f = onVar in function 
    | TyVar(k,n)                -> onVar c k n
    | TyRef(tyT)                -> TyRef(tyWalk f c tyT) 
    | TySource(tyT)             -> TySource(tyWalk f c tyT)
    | TySink(tyT)               -> TySink(tyWalk f c tyT) 
    | TyVariant(fieldtys)       -> TyVariant(List.map (fun(l,tyT)->(l,tyWalk f c tyT)) fieldtys) 
    | TyRecord(fieldtys)        -> TyRecord(List.map (fun(l,tyT)->(l,tyWalk f c tyT)) fieldtys) 
    | TyArr(tyT1,tyT2)          -> TyArr(tyWalk f c tyT1,tyWalk f c tyT2) 
    | TyRec(x,tyT)              -> TyRec(x,tyWalk f (c+1) tyT)
    | tyT                       -> tyT

let rec tmWalk onVar onType c   = let (f,g) = (onVar,onType) in function 
    | TmVar(fi,k,n)             -> onVar c fi k n
    | TmRef(fi,t)               -> TmRef(fi,tmWalk f g c t)
    | TmDeref(fi,t)             -> TmDeref(fi,tmWalk f g c t) 
    | TmAssign(fi,t1,t2)        -> TmAssign(fi,tmWalk f g c t1,tmWalk f g c t2)
    | TmCase(fi,t,cases)        -> TmCase(fi,tmWalk f g c t,List.map(fun(li,(xi,ti))->li,(xi,tmWalk f g(c+1)ti))cases)
    | TmTag(fi,l,t,tyT)         -> TmTag(fi,l,tmWalk f g c t,g c tyT)
    | TmLet(fi,x,t1,t2)         -> TmLet(fi,x,tmWalk f g c t1, tmWalk f g(c+1)t2) 
    | TmAbs(fi,x,Some(tyT),t2)  -> TmAbs(fi,x,Some(g c tyT),tmWalk f g(c+1)t2)
    | TmAbs(fi,x,None,t2)       -> TmAbs(fi,x,None,tmWalk f g(c+1)t2)
    | TmApp(fi,t1,t2)           -> TmApp(fi,tmWalk f g c t1, tmWalk f g c t2) 
    | TmIf(fi,t1,t2,t3)         -> TmIf(fi,tmWalk f g c t1, tmWalk f g c t2, tmWalk f g c t3) 
    | TmSucc(fi,t)              -> TmSucc(fi,tmWalk f g c t) 
    | TmPred(fi,t)              -> TmPred(fi,tmWalk f g c t) 
    | TmIsZero(fi,t)            -> TmIsZero(fi,tmWalk f g c t)
    | TmAscribe(fi,t,tyT)       -> TmAscribe(fi,tmWalk f g c t,g c tyT) 
    | TmRecord(fi,flds)         -> TmRecord(fi,List.map(fun(l,t)->(l,tmWalk f g c t))flds)  
    | TmProj(fi,t,i)            -> TmProj(fi,tmWalk f g c t,i) 
    | TmFix(fi,t)               -> TmFix(fi,tmWalk f g c t)
    | TmTimesfloat(fi,t1,t2)    -> TmTimesfloat(fi,tmWalk f g c t1,tmWalk f g c t2)
    | TmTimes(fi,n,m)           -> TmTimes(fi,tmWalk f g c n,tmWalk f g c m) 
    | TmFold(fi,tyT)            -> TmFold(fi,g c tyT)
    | TmUnfold(fi,tyT)          -> TmUnfold(fi,g c tyT) 
    | t                         -> t

let tyShiftOnVar d c        = fun k n     ->  if k>=c then TyVar(k+d,n+d)     else TyVar(k,n+d) 
let tyShiftAbove d c        = tyWalk (tyShiftOnVar d) c
let tyShift d               = tyShiftAbove d 0    

let tmShiftOnVar d c        = fun fi k n  ->  if k>=c then TmVar(fi,k+d,n+d)  else TmVar(fi,k,n+d)
let tmShiftAbove d c        = tmWalk (tmShiftOnVar d) (tyShiftAbove d) c
let tmShift d               = tmShiftAbove d 0 

let bindshift d             = function 
    | BindTyAbb(tyT)            ->  BindTyAbb(tyShift d tyT) 
    | BindTmVar(tyT)            ->  BindTmVar(tyShift d tyT) 
    | BindTmAbb(t,tyT_opt)      ->  (let tyT_opt' = match tyT_opt with 
                                        | None      -> None
                                        | Some(tyT) -> Some(tyShift d tyT) in 
                                    BindTmAbb(tmShift d t, tyT_opt'))
    | b                         ->  b

                                        
(* -------------------------------------------------- *) 
(* Substitution *) 
let tySubstOnVar j tyS tyT  = fun    c x n ->   if x=j+c then tyShift c tyS else TyVar(x,n) 
let tySubst      j tyS tyT  = tyWalk(tySubstOnVar j tyS tyT)0 tyT
let tySubstTop     tyS tyT  = pe"TYSUBSTTOP    : [X↦S]T"; tyShift (-1) (tySubst 0 (tyShift 1 tyS) tyT)

let tmSubstOnVar j s t c    = fun fi x n ->   if x=j+c then tmShift c s else TmVar(fi, x, n) 
let tmSubst      j s t      = tmWalk (tmSubstOnVar j s t) (fun x y -> y) 0 t
let tmSubstTop     s t      = pe"SUBSTITUTE    : [x↦s]t"; tmShift (-1) (tmSubst 0 (tmShift 1 s) t) 


(* -------------------------------------------------- *) 
(* Extracting file info *)
let tmInfo  = function 
    | TmTimes(fi,_,_)       -> fi 
    | TmRef(fi,_)           -> fi 
    | TmDeref(fi,_)         -> fi 
    | TmAssign(fi,_,_)      -> fi 
    | TmLoc(fi,_)           -> fi 
    | TmUnit(fi)            -> fi 
    | TmVar(fi,_,_)         -> fi
    | TmAbs(fi,_,_,_)       -> fi
    | TmApp(fi,_,_)         -> fi 
    | TmLet(fi,_,_,_)       -> fi
    | TmTrue(fi)            -> fi
    | TmFalse(fi)           -> fi
    | TmIf(fi,_,_,_)        -> fi
    | TmZero(fi)            -> fi
    | TmSucc(fi,_)          -> fi
    | TmPred(fi,_)          -> fi
    | TmIsZero(fi,_)        -> fi 
    | TmAscribe(fi,_,_)     -> fi 
    | TmRecord(fi,_)        -> fi
    | TmProj(fi,_,_)        -> fi 
    | TmTag(fi,_,_,_)       -> fi
    | TmCase(fi,_,_)        -> fi 
    | TmString(fi,_)        -> fi 
    | TmFloat(fi,_)         -> fi
    | TmTimesfloat(fi,_,_)  -> fi 
    | TmFix(fi,_)           -> fi
    | TmFold(fi,_)          -> fi 
    | TmUnfold(fi,_)        -> fi 
    | TmNil(fi,_)           -> fi 
    | TmCons(fi,_,_,_)      -> fi 
    | TmHead(fi,_,_)        -> fi
    | TmTail(fi,_,_)        -> fi
    | TmIsNil(fi,_,_)       -> fi 
(* -------------------------------------------------- *) 
(* Bind *) 
let rec getbind fi ctx i        =   try let (_,bind) = List.nth ctx i in bindshift(i+1)bind
                                    with Failure _ -> error fi(getbind_err_msg i(ctxlen ctx))

let getTypeFromContext fi ctx n =   match getbind fi ctx n with 
    | BindTmVar(tyT)                    -> tyT
    | BindTmAbb(_,Some(tyT))            -> tyT
    | BindTmAbb(_,None)                 -> error fi ("No type recorded for variable "^(index2name fi ctx n))
    | _                                 -> error fi("getTypeFromContext: Wrong binding"^(index2name fi ctx n))

(* -------------------------------------------------- *) 
(* Value *)
let rec isnum ctx   = function 
    | TmZero(_)                     -> true
    | TmSucc(_,t1)                  -> isnum ctx t1
    | _                             -> false

let rec isval ctx   = function 
    | TmTrue(_)                     -> true
    | TmFalse(_)                    -> true
    | TmAbs(_,_,_,_)                -> true
    | TmLoc(_,_)                    -> true 
    | TmUnit(_)                     -> true
    | TmRecord(_,flds)              -> List.for_all (fun(l,t)->isval ctx t) flds 
    | TmTag(_,_,t,_)                -> isval ctx t
    | TmString(_,_)                 -> true
    | TmFloat(_,_)                  -> true
    | t when isnum ctx t            -> true
    | TmApp(_,TmFold(_,_),t)        -> isval ctx t 
    | _                             -> false


(* -------------------------------------------------- *) 
(* Printing *)
let obox0()         = open_hvbox 0
let oobox0()        = open_hovbox 0
let obox()          = open_hvbox 2
let cbox()          = close_box()
let br()            = print_break 0 0
let psbr outer      = if outer then ps() else br()
let small           = function
    | TmVar(_,_,_)              -> true
    | _                         -> false 

let rec pr_fldtys prTy outer ctx i  = 
    let pr_fld i (li,tyTi)  = if li<>(soi i)then pr li; pr":";prTy false ctx tyTi in function
    | []                        ->  ()
    | [f]                       ->  pr_fld i f
    | f::rest                   ->  pr_fld i f;pr",";psbr outer;pr_fldtys prTy outer ctx(i+1)rest

let rec pr_flds prTm outer ctx i = 
    let pr_fld i (li,ti)    = if li<>(soi i)then(pr li;pr"=");prTm false ctx ti in function 
    | []                        ->  ()
    | [f]                       ->  pr_fld i f
    | f::rest                   ->  pr_fld i f;pr",";psbr outer;pr_flds prTm outer ctx(i+1)rest 

(* -------------------------------------------------- *) 
(* Type Print *) 
let rec pr_Type outer ctx   = function
    | TyRec(x,tyT)              ->  let ctx',x = pickfreshname ctx x in 
                                    obox(); pr"μ";pr x;pr".";ps();pr_Type outer ctx' tyT; cbox()
    | TyRef(tyT)                ->  pr"Ref ";pr_AType false ctx tyT
    | TySource(tyT)             ->  pr"Source ";pr_AType false ctx tyT 
    | TySink(tyT)               ->  pr"Sink ";pr_AType false ctx tyT 
    | tyT                       ->  pr_ArrowType outer ctx tyT

and pr_ArrowType outer ctx  = function
    | TyArr(tyT1,tyT2)          ->  obox0(); 
                                    pr_AType false ctx tyT1;
                                    if outer then pr" ";pr "→";psbr outer;pr_AType outer ctx tyT2; 
                                    cbox()
    | tyT                       ->  pr_AType outer ctx tyT

and pr_AType outer ctx      = function

    | TyTop                     ->  pr "𝐓"
    | TyFloat                   ->  pr "𝐅"  
    | TyString                  ->  pr "𝐒" 
    | TyBool                    ->  pr "𝐁" 
    | TyNat                     ->  pr "𝐍"
    | TyUnit                    ->  pr "𝐔"
    | TyId(s)                   ->  pr s
    | TyVar(i,n)                ->  if true (* ctxlen ctx = n *)then pr(index2name dummyinfo ctx i)else pr"[BadIndex]"  
    | TyVariant(flds)           ->  pr"<"; oobox0(); pr_fldtys pr_Type outer ctx 1 flds; pr">"; cbox()
    | TyRecord(flds)            ->  pr"{"; oobox0(); pr_fldtys pr_Type outer ctx 1 flds; pr"}"; cbox()
    | tyT                       ->  pr"("; pr_Type outer ctx tyT; pr ")"
;;

let pr_ty ctx tyT               = pr_Type true ctx tyT


let rec pr_cases prTm outer ctx = 
    let pr_case (li,(xi,ti)) = 
    let (ctx',xi') = pickfreshname ctx xi in  
    pr"<";pr li;pr"=";pr xi';pr"> ==> ";prTm false ctx' ti in function 
    | []        -> ()
    | [c]       -> pr"| ";pr_case c;ps(); 
    | c::rest   -> pr"| ";pr_case c;ps();pr_cases prTm outer ctx rest

(* -------------------------------------------------- *) 
(* Term Print *)
let rec pr_Term outer ctx  = function 
    | TmAbs(_,x,None,t)         ->  let (ctx',x')=pickfreshname ctx x in obox();
        pr"λ";pr x';pr".";psbr((not(small t))||outer);pr_Term outer ctx' t;  cbox()
    | TmAbs(_,x,Some(tyX),t)    ->  let (ctx',x')=pickfreshname ctx x in obox();
        pr"λ";pr x';pr":";pr_Type false ctx tyX;pr".";psbr((not(small t))||outer);pr_Term outer ctx' t;  cbox()
    | TmIf(_, t1, t2, t3)       ->  obox0();
        pr"if "  ;pr_Term false ctx t1;ps();
        pr"then ";pr_Term false ctx t2;ps();
        pr"else ";pr_Term false ctx t3;      cbox()
    | TmLet(_,x,t1,t2)          ->  obox0();
        pr"let ";pr x;pr" = ";pr_Term false ctx t1;ps();pr"in";ps();pr_Term false(addname ctx x)t2;   cbox()
    | TmFix(_,t)                ->  pr"fix "   ;pr_ATerm false ctx t
    | TmCase(_,t,cases)         ->  obox();
        pr"case ";pr_Term false ctx t;pr" of";ps();pr_cases pr_Term outer ctx cases;  cbox()
    | TmAssign(_,t1,t2)         ->  obox();pr_AppTerm false ctx t1;pr" := ";pr_ATerm false ctx t2;cbox()
    | t                         -> pr_AppTerm outer ctx t

and pr_AppTerm outer ctx   = function 
    | TmApp(_, t1, t2)          ->  obox0();  pr_AppTerm false ctx t1;ps();pr_ATerm false ctx t2;  cbox();
    | TmFold(_,tyT)             ->  obox0();pr"fold [";pr_Type false ctx tyT;pr"]";cbox()
    | TmUnfold(_,tyT)           ->  obox0();pr"unfold [";pr_Type false ctx tyT;pr"]";cbox()
    | TmRef(_,t)                ->  pr"ref "   ;pr_ATerm false ctx t
    | TmDeref(_,t)              ->  pr"!"      ;pr_ATerm false ctx t  
    | TmPred(_,t)               ->  pr"pred "  ;pr_ATerm false ctx t
    | TmTimesfloat(_,t1,t2)     ->  pr_AppTerm outer ctx t1;pr" *. ";pr_AppTerm outer ctx t2
    | TmTimes(_,n,m)            ->  pr_AppTerm outer ctx n; pr" * ";pr_AppTerm outer ctx m
    | TmIsZero(_,t)             ->  pr"iszero ";pr_ATerm false ctx t
    | t                         ->  pr_PathTerm outer ctx t 

and pr_PathTerm outer ctx   = function
    | TmProj(_,t,l)             ->  pr_ATerm false ctx t; pr"."; pr l
    | t                         ->  pr_AscribeTerm outer ctx t 

and pr_AscribeTerm outer ctx  = function 
    | TmAscribe(_,t,tyT)        ->  obox0(); pr_AppTerm false ctx t;ps();pr"as ";pr_Type false ctx tyT;cbox()
    | t                         ->  pr_ATerm outer ctx t
    
and pr_ATerm outer ctx     = function 
    | TmVar(fi,x,n)             -> let l = ctxlen ctx in if l = n 
        then pr (index2name fi ctx x)
        else (pr"[ContextErr: ";pi l;pr"!=";pi n;pr" in { Γ:";pr(List.fold_left(fun s(x,_)->s^" "^x)""ctx);pr" }]")  
    | TmTrue(_)                 ->  pr "true"
    | TmFalse(_)                ->  pr "false"
    | TmRecord(fi,flds)         ->  pr"{"; oobox0(); pr_flds pr_Term outer ctx 1 flds; pr "}"; cbox()
    | TmTag(fi,l,t,tyT)         ->  obox(); 
        pr"<";pr l;pr"=";pr_Term false ctx t;pr">";ps();pr" : ";pr_Type outer ctx tyT;  cbox()
    | TmString(_,s)             ->  pr "\"";pr s; pr"\""
    | TmUnit(_)                 ->  pr "()" 
    | TmLoc(fi,l)               ->  pr "<loc #"; pi l; pr">"
    | TmFloat(_,f)              ->  print_float f
    | TmZero(fi)                ->  pr "0"
    | TmSucc(_,t)               ->  let rec f n = function 
        | TmZero(_)                 -> pi n 
        | TmSucc(_,s)               -> f (n+1) s
        | _                         -> pr"(succ ";pr_ATerm false ctx t;pr")" in f 1 t
    | t                         ->  pr "("; pr_Term outer ctx t; pr ")"

let pr_tm t = pr_Term true t 

let int_of_tm = function  
    | TmZero(_)                   -> 0 
    | TmSucc(_,t)                 -> ( let rec f n = function 
        | TmZero(_)                 -> n 
        | TmSucc(_,s)               -> f (n+1) s
        | _                         -> error dummyinfo "int_of_tm" in f 1 t)
    | _ -> error dummyinfo  "error int_of_tm Nat type expected"

let rec tm_of_int fi          = function 
    | 0 -> TmZero(fi) 
    | n -> TmSucc(fi,tm_of_int fi (n-1))

let pr_bind = function 
    | BindName                  -> pr"NEW NAME"
    | BindTmAbb(_,_)            -> pr"ABB TERM" 
    | BindTyAbb(_)              -> pr"ABB TYPE" 
    | BindTmVar(_)              -> pr"NEW TERM" 
    | BindTyVar                 -> pr"NEW TYPE" 

let pr_ctx ctx =
    pn();
    pe "xxxx CONTEXT Γ xxxxxxxxxxxxxx";
    let rec f = function 
        | []                  -> pe" NOTHING MORE "  
        | ((str,bind)::rest)  -> pr" ( ";pr str;pr", ";pr_bind bind;pr" )";pn();f rest in 
    f ctx ; 
    pe "xxxxxxxxxxxxxxxxxxxxxxxxxxxxx"; pn()

let pr_constr ctx constr = 
    let pc (tyS,tyT)        = pr_Type false ctx tyS; pr"="; pr_Type false ctx tyT in 
    let rec f               = function 
        | []                    -> ()
        | [c]                   -> (pc c)
        | c::rest               -> (pc c; pr", "; f rest) in
    pr"{"; f constr; pr"}";;

