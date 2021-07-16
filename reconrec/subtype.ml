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

let rec simplifyty ctx tyT  =   pr"SIMPLIFYTY    : ";pr_ty ctx tyT;pn(); 
                                try let tyT' = simplifyty ctx(computety ctx tyT) in 
                                    pr"SIMPLIFIED    : ";pr_ty ctx tyT';pn();tyT'
                                with NoRuleApplies -> tyT

(* ------- TYPE EQUIVALENCE -------- *)

let rec tyeqv seen ctx tyS tyT = 
    List.mem (tyS,tyT) seen || match (tyS,tyT) with 
    | TyString,TyString                 -> true
    | TyFloat,TyFloat                   -> true
    | TyRec(x,tyS1),_                   -> tyeqv((tyS,tyT)::seen)ctx(tySubstTop tyS tyS1) tyT
    | _,TyRec(x,tyT1)                   -> tyeqv((tyS,tyT)::seen)ctx tyS(tySubstTop tyT tyT1)
    | TyId(b1),TyId(b2)                 -> b1 = b2 
    | TyVar(i,_),_ when istyabb ctx i   -> tyeqv seen ctx (gettyabb ctx i) tyT 
    | _,TyVar(i,_) when istyabb ctx i   -> tyeqv seen ctx tyS (gettyabb ctx i) 
    | TyVar(i,_),TyVar(j,_)             -> i=j
    | TyArr(tyS1,tyS2),TyArr(tyT1,tyT2) -> tyeqv seen ctx tyS1 tyT1 && tyeqv seen ctx tyS2 tyT2
    | TyRef(tyS1),TyRef(tyT1)           -> tyeqv seen ctx tyS1 tyT1 
    | TySink(tyS1),TySink(tyT1)         -> tyeqv seen ctx tyS1 tyT1 
    | TySource(tyS1),TySource(tyT1)     -> tyeqv seen ctx tyS1 tyT1 
    | TyRecord(fS),TyRecord(fT)         ->  List.length fS = List.length fT &&
                                            List.for_all(fun(li,tyTi)-> 
                                                try let tySi = List.assoc li fS in tyeqv seen ctx tySi tyTi 
                                                with Not_found -> false) fT
    | TyVariant(fS),TyVariant(fT)       ->  List.length fS = List.length fT &&
                                            List.for_all(fun(li,tyTi)->
                                                try let tySi = List.assoc li fS in tyeqv seen ctx tySi tyTi
                                                with Not_found -> false) fT
    | (tyS,tyT)                         -> tyS = tyT 

let tyeqv ctx tyS tyT           = tyeqv [] ctx tyS tyT 



let rec tyeqv ctx tyS tyT   = 
    let tyS = simplifyty ctx tyS in 
    let tyT = simplifyty ctx tyT in 
    match (tyS,tyT) with 
    | TyRec(x,tyS),TyRec(_,tyT)         ->  tyeqv (addname ctx x) tyS tyT
    | TyVar(i,_),_ when istyabb ctx i   ->  tyeqv ctx (gettyabb ctx i) tyT
    | _,TyVar(i,_) when istyabb ctx i   ->  tyeqv ctx tyS (gettyabb ctx i)
    | TyVar(i,_),TyVar(j,_)             ->  i=j
    | TyRef(tyS'),TyRef(tyT')           ->  tyeqv ctx tyS' tyT' 
    | TySource(tyS'),TySource(tyT')     ->  tyeqv ctx tyS' tyT' 
    | TySink(tyS'),TySink(tyT')         ->  tyeqv ctx tyS' tyT' 
    | TyArr(tyS1,tyS2),TyArr(tyT1,tyT2) ->  tyeqv ctx tyS1 tyT1 && tyeqv ctx tyS2 tyT2
    | TyRecord(fS),TyRecord(fT)         ->  List.length fS = List.length fT &&
                                            List.for_all(fun(li,tyTi)-> 
                                                try let tySi = List.assoc li fS in tyeqv ctx tySi tyTi
                                                with Not_found -> false) fT
    | TyVariant(fS),TyVariant(fT)       ->  List.length fS = List.length fT &&
                                            List.for_all(fun(li,tyTi)->
                                                try let tySi = List.assoc li fS in tyeqv ctx tySi tyTi
                                                with Not_found -> false) fT
    | (tyS,tyT)                         -> tyS = tyT 

(* ------- INFINITE SUBTYPING --------*)

let rec subtype a tyS tyT     = 
    if List.mem (tyS,tyT) a 
        then a
        else let a0 = (tyS,tyT)::a in match (tyS,tyT) with 
        | TyRecord(fS),TyRecord(fT)     ->  (match fS,fT with 
            | _,((li,tyTi)::rT)             ->  let tySi    = List.assoc li fS in 
                                                let rS      = List.remove_assoc li fS in 
                                                let a1      = subtype a0 tySi tyTi in
                                                subtype a1 (TyRecord(rS)) (TyRecord(rT))
            | _,_                           ->  raise NoRuleApplies)
        | TyArr(tyS1,tyS2),TyArr(tyT1,tyT2)
                                        ->  let a1 = subtype a0 tyT1 tyS1 in 
                                            subtype a1 tyS2 tyT2
        | _,TyRec(y,tyT1)               ->  subtype a0 tyS tyT    
        | _,_                           ->  raise NoRuleApplies 

        
(* --------- SUBTYPING -------------- *)

let rec subtype ctx tyS tyT     = 
    tyeqv ctx tyS tyT || 
    let tyS = simplifyty ctx tyS in
    let tyT = simplifyty ctx tyT in match (tyS,tyT) with 
    | _,TyTop                           ->  true
    | TyArr(s1,s2),TyArr(t1,t2)         ->  subtype ctx t1 s1 && subtype ctx s2 t2 
    | TyRecord(fS),TyRecord(fT)         ->  List.for_all ( fun(li,tyTi) -> 
                                                try let tySi = List.assoc li fS in subtype ctx tySi tyTi
                                                with Not_found -> false ) fT 
    | TyRef(tyS'),TyRef(tyT')           ->  subtype ctx tyS' tyT' && subtype ctx tyT' tyS' 
    | TyRef(tyS'),TySource(tyT')        ->  subtype ctx tyS' tyT'
    | TySource(tyS'),TySource(tyT')     ->  subtype ctx tyS' tyT'
    | TyRef(tyS'),TySink(tyT')          ->  subtype ctx tyT' tyS'
    | TySink(tyS'),TySink(tyT')         ->  subtype ctx tyT' tyS' 
    | _,_                               ->  false 



(* ---------- JOIN & MEET ---------- *)

let rec join ctx tyS tyT        =
    pr"JOINING TYPES    : "; pr_ty ctx tyS; pr" & "; pr_ty ctx tyT;pn();
    if subtype ctx tyS tyT then tyT else
    if subtype ctx tyT tyS then tyS else 
    let tyS = simplifyty ctx tyS in 
    let tyT = simplifyty ctx tyT in 
    match(tyS,tyT) with 
    | TyRecord(fS),TyRecord(fT)         ->  let lSs = List.map fst fS in 
                                            let lTs = List.map fst fT in 
                                            let commonls = List.find_all (fun l -> List.mem l lTs) lSs in 
                                            let f li = (li,join ctx(List.assoc li fS)(List.assoc li fT)) in 
                                            let commonflds = List.map f commonls in 
                                            TyRecord(commonflds)
    | TyArr(tyS1,tyS2),TyArr(tyT1,tyT2) ->  (try     TyArr(meet ctx tyS1 tyT1, join ctx tyS2 tyT2) 
                                            with Not_found  -> TyTop)
    | TyRef(tyS),TyRef(tyT)             ->  if subtype ctx tyS tyT && subtype ctx tyT tyS 
                                                then TyRef(tyS)
                                                else TySource(join ctx tyS tyT) 
    | TyRef(tyS),TySource(tyT)          ->  TySource(join ctx tyS tyT) 
    | TySource(tyS),TyRef(tyT)          ->  TySource(join ctx tyS tyT) 
    | TySource(tyS),TySource(tyT)       ->  TySource(join ctx tyS tyT) 
    | TyRef(tyS),TySink(tyT)            ->  TySink(meet ctx tyS tyT) 
    | TySink(tyS),TyRef(tyT)            ->  TySink(meet ctx tyS tyT) 
    | TySink(tyS),TySink(tyT)           ->  TySink(meet ctx tyS tyT) 
    
    | _                                 ->  TyTop

and meet ctx tyS tyT                = 
    if subtype ctx tyS tyT then tyS else 
    if subtype ctx tyT tyS then tyT else
    let tyS = simplifyty ctx tyS in 
    let tyT = simplifyty ctx tyT in 
    match(tyS,tyT) with 
    | TyRecord(fS),TyRecord(fT)         ->  let lSs = List.map fst fS in 
                                            let lTs = List.map fst fT in 
                                            let nomem l x = not (List.mem x l) in 
                                            let allLabels = List.append lSs (List.find_all(nomem lSs)lTs) in
                                            let f li =  if List.mem li allLabels 
                                                            then li, meet ctx (List.assoc li fS)(List.assoc li fT) 
                                                            else if List.mem li lSs 
                                                                then li, List.assoc li fS
                                                                else li, List.assoc li fT in 
                                            let allflds = List.map f allLabels in 
                                            TyRecord(allflds) 
    | TyArr(tyS1,tyS2),TyArr(tyT1,tyT2) ->  TyArr(join ctx tyS1 tyT1, meet ctx tyS2 tyT2) 
    | TyRef(tyS),TyRef(tyT)             ->  if subtype ctx tyS tyT && subtype ctx tyT tyS 
                                                then TyRef(tyS)
                                                else TySource(meet ctx tyS tyT) 
    | TyRef(tyS),TySource(tyT)          ->  TySource(meet ctx tyS tyT) 
    | TySource(tyS),TyRef(tyT)          ->  TySource(meet ctx tyS tyT) 
    | TySource(tyS),TySource(tyT)       ->  TySource(meet ctx tyS tyT) 
    | TyRef(tyS),TySink(tyT)            ->  TySink(join ctx tyS tyT) 
    | TySink(tyS),TyRef(tyT)            ->  TySink(join ctx tyS tyT) 
    | TySink(tyS),TySink(tyT)           ->  TySink(join ctx tyS tyT) 
    | _                                 ->  pr"hoge";raise Not_found  




