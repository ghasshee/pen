open Format
open Support
open Syntax
open Type

exception NoRuleApplies
exception EvalFloatFailure

let rec evalF1 = function
    | TmFloat(_,a)          -> a 
    | TmTimesfloat(_,a,b)   -> evalF1 a *. evalF1 b
    | _                     -> raise EvalFloatFailure

let rec eval1 ctx  t = let p str = pr str;pr_tm ctx t;pn() in match t with  
    | TmFix(fi,TmAbs(f,x,tyT1,t2))as t1 ->  p"E-FIXBETA     : "; tmSubstTop t1 t2
    | TmFix(fi,t)                       ->  p"E-FIX         : "; TmFix(fi,eval1 ctx t) 
    | TmTag(fi,l,t1,tyT1)               ->  p"E-TAG         : "; TmTag(fi,l,eval1 ctx t1,tyT1)
    | TmVar(fi,n,_)                     ->  p"E-VAR         : "; (match getbind fi ctx n with
        | BindTmAbb(t,_)                    -> t
        | _                                 -> raise NoRuleApplies) 
    | TmCase(fi,TmTag(_,l,v,_),cases) when isval ctx v  ->  
                                            p"E-CASETAGV    : ";
                                            (try let (x,t)= List.assoc l cases in  tmSubstTop v t
                                            with Not_found -> raise NoRuleApplies)
    | TmCase(fi,t0,cases)               ->  p"E-CASE        : "; TmCase(fi,eval1 ctx t0,cases) 
    | TmAscribe(fi,v,_)when isval ctx v ->  p"E-ASCRIBEVAR  : "; v
    | TmAscribe(fi,t1,tyT)              ->  p"E-ASCRIBE     : "; TmAscribe(fi,eval1 ctx t1,tyT) 
    | TmLet(fi,x,v1,t2)when isval ctx v1->  p"E-LETV        : "; tmSubstTop v1 t2 
    | TmLet(fi,x,t1,t2)                 ->  p"E-LET         : "; TmLet(fi,x,eval1 ctx t1,t2)
(*  | TmLet(fi,x,t1,t2)                 ->  p"E-LET(SUGAR)  : "; 
                                            TmApp(fi,TmAbs(fi,x,typeof ctx t1,t2),t1)  *)
    | TmApp(fi,TmAbs(_,x,tyT11,t12),v2)  (*  (λx:T11.t12) v2  ⇒  [x ↦ v2] t12    E-AppAbs  *)
        when isval ctx v2               ->  p"E-APPABS      : "; tmSubstTop v2 t12 
     (*                     t2 → t2'                            *
      *         ------------------------------- E-App2          *
      *                 v1 t2 → v1 t2'                          *) 
    | TmApp(fi,v1,t2)                                          
        when isval ctx v1               ->  p"E-APP1        : "; TmApp(fi,v1,eval1 ctx t2) 
    | TmApp(fi,t1,t2)                   ->  p"E-APP2        : "; TmApp(fi,eval1 ctx t1,t2) 
    | TmIf(_,TmTrue(_),t2,t3)           ->  p"E-IFTRUE      : "; t2
    | TmIf(_,TmFalse(_),t2,t3)          ->  p"E-IFFLASE     : "; t3
    | TmIf(fi,t1,t2,t3)                 ->  p"E-IF          : "; TmIf(fi,eval1 ctx t1, t2, t3)
    | TmSucc(fi,t1)                     ->  p"E-SUCC        : "; TmSucc(fi, eval1 ctx t1)
    | TmPred(_,TmZero(_))               ->  p"E-PREDZRO     : "; TmZero(dummyinfo)
    | TmPred(_,TmSucc(_,nv1)) 
        when isnum ctx nv1              ->  p"E-PREDSUC     : "; nv1
    | TmPred(fi,t1)                     ->  p"E-PRED        : "; TmPred(fi, eval1 ctx t1)
    | TmIsZero(_,TmZero(_))             ->  p"E-ISZROZRO    : "; TmTrue(dummyinfo)
    | TmTimesfloat(fi,t1,t2) as tm      ->  p"E-TIMESFLOAT  : "; TmFloat(fi,evalF1 tm)  
    | TmIsZero(_,TmSucc(_,nv1)) 
        when isnum ctx nv1              ->  p"E-ISZROSUC    : "; TmFalse(dummyinfo)
    | TmIsZero(fi,t1)                   ->  p"E-ISZRO       : "; TmIsZero(fi, eval1 ctx t1)
    | TmRecord(fi,fields)               ->  p"E-RCD         : "; let rec ev_flds = function
        | []                                -> raise NoRuleApplies                  (*                      *)
        | (l,v)::rest when isval ctx v      -> (l,v)::(ev_flds rest)                (*                      *)
        | (l,t)::rest                       -> ev_flds ((l,eval1 ctx t)::rest) in   (*                      *)
                                            TmRecord(fi,ev_flds fields)             (*                      *)
    | TmProj(fi,(TmRecord(f,flds)as v),l)                                           (*      E-ProjRcd       *)
        when isval ctx v                ->  p"E-PROJRCD     : "; 
                                            (try List.assoc l flds with Not_found -> raise NoRuleApplies)
    | TmProj(fi,t1,l)                   ->  p"E-PROJ        : "; TmProj(fi,eval1 ctx t1,l)        
    | _                                 ->  raise NoRuleApplies

let rec eval ctx t =
    try eval ctx (eval1 ctx t) 
    with NoRuleApplies -> t


(*------------ Binding ------------*)

let evalbind ctx            = function
    | BindTmAbb(t,tyT)          ->  BindTmAbb(eval ctx t,tyT) 
    | bind                      ->  bind

let checkbind fi ctx        = function 
    | BindTmAbb(t,None)         ->  BindTmAbb(t, Some(typeof ctx t))
    | BindTmAbb(t,Some(tyT))    ->  if tyeqv ctx(typeof ctx t)tyT 
                                        then BindTmAbb(t,Some(tyT))
                                        else error fi"TyAbbErr"
    | bind                      ->  bind  

let rec process_command ctx = function 
    | Eval(fi,t)                ->
            pn();
            pe"----------------------------------------------------";
            let tyT = typeof ctx t in
            pe"----------------   TYPE CHECKED !   ----------------";
            let t' = eval ctx t in
            pe"----------------   EVAL FINISHED !  ----------------"; 
            pr_ATerm true ctx t'; pb 1 2; pr ": "; pr_ty ctx tyT; pn();pn(); ctx
    | Bind(fi,x,bind)           ->  
            let bind' = checkbind fi ctx bind in 
            let bind'' = evalbind ctx bind' in 
            pr x;pr" ";prbindty ctx bind'';pn();addbind ctx x bind'' 


let rec process_commands ctx = function 
    | []                        ->  ctx 
    | cmd::cmds                 ->  oobox0;
                                    let ctx' = process_command ctx cmd in 
                                    Format.print_flush();
                                    process_commands ctx' cmds 



let pr_eval ctx cmd         = 
    open_hvbox 0; 
    let _ = process_command ctx cmd in 
    print_flush ()
