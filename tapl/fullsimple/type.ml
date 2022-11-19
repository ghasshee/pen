open Support
open Syntax

exception NoRuleApplies


(* ---------------------------------- *) 
let istyabb ctx i           = match getbind dummyinfo ctx i with 
    | BindTyAbb(_)                  -> true
    | _                             -> false

let gettyabb ctx i          = match getbind dummyinfo ctx i with 
    | BindTyAbb(tyT)                -> tyT
    | _                             -> raise NoRuleApplies 

let rec computety ctx tyT   = match tyT with 
    | TyVar(i,_)when istyabb ctx i  -> gettyabb ctx i 
    | _                             -> raise NoRuleApplies

let rec simplifyty ctx tyT  = try simplifyty ctx(computety ctx tyT) with NoRuleApplies -> tyT



let rec tyeqv ctx tyS tyT   = 
    let tyS = simplifyty ctx tyS in 
    let tyT = simplifyty ctx tyT in 
    match (tyS,tyT) with 
    | (TyVar(i,_),_) when istyabb ctx i -> tyeqv ctx(gettyabb ctx i)tyT
    | (_,TyVar(i,_)) when istyabb ctx i -> tyeqv ctx tyS(gettyabb ctx i)
    | TyVar(i,_),TyVar(j,_)             -> i=j
    | TyArr(tyS1,tyS2),TyArr(tyT1,tyT2) -> tyeqv ctx tyS1 tyT1 && tyeqv ctx tyS2 tyT2
    | TyRecord(flds1),TyRecord(flds2)   ->  List.length flds1 = List.length flds2 &&
                                            List.for_all(fun(li2,tyTi2)-> 
                                                try let tyTi1 = List.assoc li2 flds1 in tyeqv ctx tyTi1 tyTi2 
                                                with Not_found -> false) flds2
    | TyVariant(flds1),TyVariant(flds2) ->  List.length flds1 = List.length flds2 &&
                                            List.for_all(fun(li2,tyTi2)->
                                                try let tyTi1 = List.assoc li2 flds1 in tyeqv ctx tyTi1 tyTi2
                                                with Not_found -> false) flds2 
    | (tyS,tyT)                         -> tyS = tyT 

(* ----------- TYPING --------------- *) 

let rec typeof ctx   t      = let p str = pr str;pr"(∣Γ∣=";pi(ctxlen ctx);pr") ";pr_tm ctx t;pn() in match t with
    | TmFix(fi,t1)              ->  p "T-FIX         : "; (match typeof ctx t1 with 
            | TyArr(tyS,tyT)        -> if tyeqv ctx tyS tyT then tyT else error fi"fix can take 'x' whose type: A -> A" 
            | _                     -> error fi"fix can only take x whose type is A -> A"  )
    | TmTag(fi,l,t1,(TyVariant(fldtys) as tyT)) ->  
                                    p "T-VARIANT     : "; let tyT1 = typeof ctx t1 in
                                    if(tyeqv ctx)tyT1(List.assoc l fldtys)then tyT else error fi"Variant Type Mismatch"
    | TmCase(fi,t1,cases)       ->  p "T-CASE        : "; (match simplifyty ctx(typeof ctx t1) with 
            | TyVariant(fldtys)     ->  List.iter(fun(li,(xi,ti))->try List.assoc li fldtys;()
                                                with Not_found -> error fi("label "^li^"not in type")) cases;
                                        let caseTs = List.map (fun(li,(xi,ti))-> 
                                            let tyTi =  try List.assoc li fldtys
                                                        with Not_found -> error fi("label "^li^"not found") in
                                            tyShift(-1)(typeof(addbind ctx xi(BindTmVar(tyTi)))ti)) cases in 
                                        let tyT1::restTs = caseTs in 
                                        List.iter(fun tyT->if(tyeqv ctx)tyT tyT1 then() else error fi"fldsTyErr")restTs;
                                        tyT1  
            | _                     ->  error fi "Expected variant type")
    | TmFloat(fi,f)             ->  p "T-FLOAT       : "; TyFloat
    | TmTimesfloat(fi,t1,t2)    ->  p "T-TIMESFLOAT  : "; 
                                    if (tyeqv ctx)TyFloat(typeof ctx t1) && (tyeqv ctx)TyFloat(typeof ctx t2) 
                                    then TyFloat else error fi"TypeMismatch: (*.) requires Floats"  
    | TmString(fi,_)            ->  p "T-STRING      : "; TyString
    | TmVar(fi,i,_)             ->  p "T-VAR         : "; getTypeFromContext fi ctx i  
    | TmLet(fi,x,t1,t2)         ->  p "T-LET         : "; tyShift(-1)(typeof(addbind ctx x(BindTmVar(typeof ctx t1)))t2)
    | TmAbs(fi,x,tyT1,t2)       ->  p "T-ABS         : ";
            let ctx'    = addbind ctx x (BindTmVar(tyT1)) in    (*       Γ,x:T1 ∣- t2 : T2          *)  
            let tyT2    = typeof ctx' t2 in                     (*     --------------------- T-Abs  *)
            TyArr(tyT1,tyShift(-1)tyT2)                         (*     Γ ∣- λx:T1.t2 : T1→T2        *)
    | TmApp(fi,t1,t2)           ->  p "T-APP         : ";     
            let tyT1 = typeof ctx t1 in                         (*   Γ |- t1 : T2→T12 ∧ Γ ∣- t2 : T2        *)
            let tyT2 = typeof ctx t2 in                         (*   ------------------------------- T-App  *)
            (match simplifyty ctx tyT1 with                     (*         Γ ∣- t1 t2 : T12                 *)   
                | TyArr(tyT11,tyT12)    -> if (tyeqv ctx) tyT2 tyT11 then tyT12 else error fi "type mismatch" 
                | _                     -> error fi "arrow type expected" )
    | TmRecord(fi,flds)         ->  p "T-RCD         : "; TyRecord(List.map (fun(l,t)->(l,typeof ctx t)) flds)
    | TmProj(fi,t,l)            ->  p "T-PROJ        : "; (match typeof ctx t with 
        | TyRecord(tyflds)          -> (try List.assoc l tyflds with Not_found -> error fi("label "^l^" not found")) 
        | _                         -> error fi "Record Type Expected" ) 
    | TmUnit(fi)                ->  p "T-UNIT        : "; TyUnit
    | TmTrue(fi)                ->  p "T-TRUE        : "; TyBool
    | TmFalse(fi)               ->  p "T-FALSE       : "; TyBool
    | TmZero(fi)                ->  p "T-ZERO        : "; TyNat
    | TmSucc(fi,t)              ->  p "T-SUCC        : ";
                                    if (tyeqv ctx)(typeof ctx t)TyNat then TyNat else error fi "succ expects 𝐍"  
    | TmPred(fi,t)              ->  p "T-PRED        : ";
                                    if (tyeqv ctx)(typeof ctx t)TyNat then TyNat else error fi "succ expects 𝐍"  
    | TmIsZero(fi,t)            ->  p "T-ISZERO      : ";
                                    if (tyeqv ctx)(typeof ctx t)TyNat then TyBool else error fi "iszero expects 𝐍"
    | TmIf(fi,t1,t2,t3)         ->  p "T-IF          : ";
        if (tyeqv ctx) (typeof ctx t1) TyBool 
            then    let tyT2 = typeof ctx t2 in
                    if (tyeqv ctx)tyT2(typeof ctx t3) then tyT2 else error fi "The result types of if-stmt mismatch" 
            else error fi "if-condition expects a boolean" 
    | TmAscribe(fi,t,tyT)       ->  p "T-ASCRIBE     : ";
                                    if (tyeqv ctx)(typeof ctx t)tyT then tyT else error fi "Type Ascription Mismatch"       


(* ---- *) 

(* CAUTION typeof is used unneededly *)
(* -------------------------------------------------- *) 
(* Bind Print *)    
let prbindty ctx = function
    | BindName                  -> ()
    | BindTmVar(tyT)            -> pr": "; pr_ty ctx tyT 
    | BindTyVar                 -> () 
    | BindTyAbb(tyT)            -> pr"= "; pr_ty ctx tyT
    | BindTmAbb(t,Some(tyT))    -> pr"= "; pr_tm ctx t; pr" : ";pr_ty ctx tyT
    | BindTmAbb(t,None)         -> pr"= "; pr_tm ctx t; pr" : ";pr_ty ctx(typeof ctx t) 

