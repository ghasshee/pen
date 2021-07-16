open Support
open Syntax
open Subtype

exception NoRuleApplies



(* -------- CONSTRAINTS  --------- *)

let rec substinty x tyT   = function 
        | TyArr(tyS1,tyS2)      -> TyArr(substinty x tyT tyS1,substinty x tyT tyS2) 
        | TyId(s)               -> (match tyT with 
            | TyRec(_,_)            -> tyT 
            | _                     -> if s=x then tyT else TyId(s))
        | tyT                   -> tyT

let substinconstr x tyT   =
    List.map (fun(tyS1,tyS2)->(substinty x tyT tyS1,substinty x tyT tyS2)) 

let apply_constr constr tyT =
    let f tyS = function 
        | TyId(x),tyC2      -> substinty x tyC2 tyS 
        | _                 -> raise NoRuleApplies  in 
    List.fold_left f tyT (List.rev constr);;

let rec occurcheck x      = function
    | TyArr(tyT1,tyT2)          -> occurcheck x tyT1 || occurcheck x tyT2
    | TyId(s)                   -> s=x
    | tyT                       -> false 


let rec unify fi ctx msg =  function 
| []    -> [] 
| c::cs -> let p(tyS,tyT)= pr"UNIFY: ";pr_Type false ctx tyS;pr", ";pr_Type false ctx tyT;pn() in 
( match (c::cs) with 
    | (tyT,TyRec(x,tyS))::rest  ->  p c;                                    
                                    unify fi ctx msg ((tyS,tyT)::rest)
    | (TyRec(x,tyS),tyT)::rest  ->  p c;                                    
                                    unify fi ctx msg ((tyS,tyT)::rest)
    | (TyId(x),tyT)::rest       ->  (p c ;if tyT = TyId(x) 
                                    then unify fi ctx msg rest
                                    else if occurcheck x tyT 
                                    then (unify fi ctx msg (substinconstr x tyT rest))@[TyId(x),TyRec(x,tyT)]
                                    else (unify fi ctx msg (substinconstr x tyT rest))@[TyId(x),tyT] )
    | (tyS,TyId(x))::rest       ->  (p c ; if tyS = TyId(x) 
                                    then unify fi ctx msg rest 
                                    else (if occurcheck x tyS 
                                    then (unify fi ctx msg (substinconstr x tyS rest))@[TyId(x),TyRec(x,tyS)]
                                    else (unify fi ctx msg (substinconstr x tyS rest))@[TyId(x),tyS]))
    | (TyNat,TyNat)::rest       ->  p c ; unify fi ctx msg rest
    | (TyBool,TyBool)::rest     ->  p c ; unify fi ctx msg rest
    | (TyArr(tyS1,tyS2),TyArr(tyT1,tyT2))::rest
                                ->  p c ; unify fi ctx msg ((tyS1,tyT1)::((tyS2,tyT2)::rest))
    | (tyS,tyT) :: rest         ->  p c ; error fi "Unsolvable constraints"
    | _                         ->  raise NoRuleApplies ) 


let rec recon ctx uvar t = 
    let p str = pr str;pr": (|Γ|=";pi(ctxlen ctx);pr") ";pr_tm ctx t;pn() in match t with 
    | TmVar(fi,i,_)             ->  p"CT-VAR        ";
                                    let tyT = getTypeFromContext fi ctx i in 
                                    (tyT, uvar, [])
    | TmAbs(fi,x,None,t)        ->  p"CT-ABS(UNTYPE)";
                                    let NextUVar(u,uvar') = uvar() in 
                                    let tyX     = TyId(u) in 
                                    let ctx'    = addbind ctx x (BindTmVar(tyX)) in 
                                    let (tyT,uvar'',constr') = recon ctx' uvar' t in 
                                    (TyArr(tyX,tyT),uvar'',constr')
    | TmAbs(fi,x,Some(tyX),t)   ->  p"CT-ABS        ";
                                    let ctx'    = addbind ctx x (BindTmVar(tyX)) in 
                                    let (tyT,uvar',constr') = recon ctx' uvar t in 
                                    (TyArr(tyX,tyT),uvar',constr')
    | TmApp(fi,t1,t2)           ->  p"CT-APP        ";
                                    let (tyT1,uvar',constr')    = recon ctx uvar t1 in 
                                    let (tyT2,uvar'',constr'')  = recon ctx uvar' t2 in
                                    let NextUVar(x,uvar''')   = uvar'' () in 
                                    TyId(x),uvar''',List.concat[[(tyT1,TyArr(tyT2,TyId(x)))];constr';constr'']
    | TmZero(fi)                ->  p"CT-ZERO       ";
                                    TyNat,uvar,[]
    | TmSucc(fi,t)              ->  p"CT-SUCC       ";
                                    let tyT,uvar',constr' = recon ctx uvar t in 
                                    TyNat,uvar',(tyT,TyNat)::constr'
    | TmPred(fi,t)              ->  p"CT-PRED       ";
                                    let tyT,uvar',constr' = recon ctx uvar t in 
                                    TyNat,uvar',(tyT,TyNat)::constr'
    | TmIsZero(fi,t)            ->  p"CT-ISZERO     ";
                                    let tyT,uvar',constr' = recon ctx uvar t in 
                                    TyBool,uvar',(tyT,TyNat)::constr'
    | TmTrue(fi)                ->  p"CT-TRUE       ";
                                    TyBool,uvar,[]
    | TmFalse(fi)               ->  p"CT-FALSE      ";
                                    TyBool,uvar,[]
    | TmIf(fi,t1,t2,t3)         ->  p"CT-IF         ";
                                    let tyT1,uvar1,constr1 = recon ctx uvar t1 in 
                                    let tyT2,uvar2,constr2 = recon ctx uvar1 t2 in 
                                    let tyT3,uvar3,constr3 = recon ctx uvar2 t3 in 
                                    tyT3,uvar3,List.concat[[tyT1,TyBool;tyT2,tyT3];constr1;constr2;constr3]
    | TmUnit(fi)                ->  p"CT-UNIT       ";     
                                    TyUnit,uvar,[]
    | TmLet(fi,x,t1,t2)         ->  p"CT-LETPOLY-ALG";
                                    let tyT1,uvar',constr'  = recon ctx uvar t1 in 
                                    let sol_t1              = unify fi ctx "letpoly alg faild:" constr' in 
                                    let generalised_T1      = apply_constr sol_t1 tyT1 in 
                                    let NextUVar(y,uvar'')  = uvar'() in 
                                    let ctx' = addbind ctx y (BindTmVar(generalised_T1)) in 
                                    let tyT2,uvar''',constr'' = recon ctx' uvar'' t2 in 
                                    tyT2,uvar''',constr''
                                    (*
    | TmLet(fi,x,t1,t2)         ->  p"CT-LETPOLY    ";
                                    let tyT2,uvar',constr' = recon ctx uvar (tmSubstTop t1 t2) in 
                                    tyT2,uvar', constr'
    | TmLet(fi,x,t1,t2)         ->  p"CT-LET        ";
                                    if not (isval ctx t1) then 
                                        let tyT1,uvar',constr' = recon ctx uvar t1 in 
                                        let ctx' = addbind ctx x (BindTmVar(tyT1)) in
                                        let tyT2,uvar'',constr'' = recon ctx' uvar' t2 in 
                                        tyT2, uvar'', constr'@constr''
                                    else 
                                        recon ctx uvar (tmSubstTop t1 t2)
                                        *)
    | _                         ->  raise NoRuleApplies
let recon' sol ctx uvar t = ()


(* ----------- TYPING --------------- *) 

let rec typeof ctx   t      = let p str = pr str;pr": (∣Γ∣=";pi(ctxlen ctx);pr") ";pr_tm ctx t;pn() in match t with
    | TmFold(fi,tyS)            ->  p"T-FLD         "; (match simplifyty ctx tyS with 
        | TyRec(_,tyT)              -> TyArr(tySubstTop tyS tyT,tyS) 
        | _                         -> error fi "Recursive Type Expected" )
    | TmUnfold(fi,tyS)          ->  p"T-UNFLD       "; (match simplifyty ctx tyS with 
        | TyRec(_,tyT)              -> TyArr(tyS,tySubstTop tyS tyT) 
        | _                         -> error fi "Recursive Type Expected" )
    | TmRef(fi,t)               ->  p"T-REF         "; TyRef(typeof ctx t)
    | TmDeref(fi,t)             ->  p"T-DEREF       "; (match simplifyty ctx (typeof ctx t) with 
        | TyRef(tyT)                -> tyT
        | TySource(tyT)             -> tyT 
        | _                         -> error fi "argument of ! does not have a Ref Type" )
    | TmAssign(fi,t1,t2)        ->  p"T-ASSIGN      "; (match simplifyty ctx (typeof ctx t1) with
        | TyRef(tyT)                ->  let tyT2 = typeof ctx t2 in  
                                        if subtype ctx tyT2 tyT && subtype ctx tyT tyT2 
                                            then TyUnit 
                                            else error fi":= cannot assign type"
        | TySink(tyT)               ->  if subtype ctx (typeof ctx t2) tyT 
                                            then TyUnit
                                            else error fi":= cannot assign type" 
        | _                         ->  error fi "arguments of := does not have matching type" ) 
    | TmFix(fi,t1)              ->  p"T-FIX         "; (match simplifyty ctx (typeof ctx t1) with 
        | TyArr(tyS,tyT)            ->  if subtype ctx tyT tyS 
                                            then tyT 
                                            else error fi"fix can take 'x' whose type: A -> A" 
        | _                         -> error fi"fix can only take x whose type is A -> A"  )
    | TmTag(fi,l,t1,(TyVariant(fldtys) as tyT)) ->  
                                    p"T-VARIANT     "; let tyT1 = typeof ctx t1 in
                                    if subtype ctx tyT1(List.assoc l fldtys)
                                        then tyT 
                                        else error fi "Variant Type Mismatch"
    | TmCase(fi,t1,cases)       ->  p"T-CASE        "; (match simplifyty ctx(typeof ctx t1) with 
        | TyVariant(fldtys)         ->  let findTypeInVariant (li,(xi,ti))  =   
                                            try List.assoc li fldtys 
                                            with Not_found -> error fi ("label "^li^"not found") in 
                                        List.iter (fun c -> findTypeInVariant c;()) cases;
                                        let typeofcase (li,(xi,ti)) = 
                                            let tyTi =  findTypeInVariant (li,(xi,ti)) in 
                                            tyShift(-1)(typeof(addbind ctx xi(BindTmVar(tyTi)))ti) in 
                                        let caseTys  = List.map typeofcase cases in 
                                        let theCaseTy::rest = caseTys in
                                        let checkTypeIsEquiv2theCaseTy tyT =   
                                            if tyeqv ctx tyT theCaseTy  
                                                then () 
                                                else error fi "fldsTyErr" in 
                                        List.iter checkTypeIsEquiv2theCaseTy rest; theCaseTy   
            | _                     ->  error fi "Expected variant type")
    | TmFloat(fi,f)             ->  p"T-FLOAT       "; TyFloat
    | TmTimesfloat(fi,t1,t2)    ->  p"T-TIMESFLOAT  "; 
                                    if tyeqv ctx TyFloat(typeof ctx t1) && tyeqv ctx TyFloat(typeof ctx t2) 
                                        then TyFloat 
                                        else error fi"TypeMismatch: (*.) requires Floats"  
    | TmString(fi,_)            ->  p"T-STRING      "; TyString
    | TmVar(fi,i,_)             ->  p"T-VAR         "; getTypeFromContext fi ctx i  
    | TmLet(fi,x,t1,t2)         ->  p"T-LET         "; tyShift(-1)(typeof(addbind ctx x(BindTmVar(typeof ctx t1)))t2)
    | TmAbs(fi,x,Some(tyT1),t2) ->  p"T-ABS         ";
            let ctx'    = addbind ctx x (BindTmVar(tyT1)) in    (*       Γ,x:T1 ∣- t2 : T2          *)  
            let tyT2    = typeof ctx' t2 in                     (*     --------------------- T-Abs  *)
            TyArr(tyT1,tyShift(-1)tyT2)                         (*     Γ ∣- λx:T1.t2 : T1→T2        *)
    | TmApp(fi,t1,t2)           ->  p"T-APP         ";     
            let tyT1 = typeof ctx t1 in                         (*   Γ |- t1 : T2→T12 ∧ Γ ∣- t2 : T2        *)
            let tyT2 = typeof ctx t2 in                         (*   ------------------------------- T-App  *)
            (match simplifyty ctx tyT1 with                     (*         Γ ∣- t1 t2 : T12                 *)   
                | TyArr(tyT11,tyT12)    -> if subtype ctx tyT2 tyT11 then tyT12 else error fi "type mismatch" 
                | _                     -> error fi "arrow type expected" )
    | TmRecord(fi,flds)         ->  p"T-RCD         "; TyRecord(List.map (fun(l,t)->(l,typeof ctx t)) flds)
    | TmProj(fi,t,l)            ->  p"T-PROJ        "; (match simplifyty ctx (typeof ctx t) with 
        | TyRecord(tyflds)          -> (try List.assoc l tyflds with Not_found -> error fi("label "^l^" not found")) 
        | _                         -> error fi "Record Type Expected" ) 
    | TmUnit(fi)                ->  p"T-UNIT        "; TyUnit
    | TmTrue(fi)                ->  p"T-TRUE        "; TyBool
    | TmFalse(fi)               ->  p"T-FALSE       "; TyBool
    | TmZero(fi)                ->  p"T-ZERO        "; TyNat
    | TmSucc(fi,t)              ->  p"T-SUCC        ";
                                    if subtype ctx(typeof ctx t)TyNat then TyNat else error fi "succ expects 𝐍"  
    | TmPred(fi,t)              ->  p"T-PRED        ";
                                    if subtype ctx(typeof ctx t)TyNat then TyNat else error fi "succ expects 𝐍"  
    | TmIsZero(fi,t)            ->  p"T-ISZERO      ";
                                    if subtype ctx(typeof ctx t)TyNat then TyBool else error fi "iszero expects 𝐍"
    | TmIf(fi,t1,t2,t3)         ->  p"T-IF          ";
        if (subtype ctx)(typeof ctx t1)TyBool 
            then    join ctx (typeof ctx t2) (typeof ctx t3)
            else    error fi "if-condition expects a boolean" 
    | TmAscribe(fi,t,tyT)       ->  p"T-ASCRIBE     ";
            if (subtype ctx)(typeof ctx t)tyT then tyT else error fi "Type Ascription Mismatch"       
    | _                         -> raise NoRuleApplies 




(* CAUTION typeof is used unneededly *)
(* -------------------------------------------------- *) 
(* Bind Print *)    
let prbindty ctx = function
    | BindName                  -> ()
    | BindTmVar(tyT)            -> pr": "; pr_ty ctx tyT 
    | BindTyVar                 -> () 
    | BindTyAbb(tyT)            -> pr"= "; pr_ty ctx tyT
    | BindTmAbb(t,Some(tyT))    -> pr"= "; pr_tm ctx t
    | BindTmAbb(t,None)         -> pr"= "; pr_tm ctx t 
