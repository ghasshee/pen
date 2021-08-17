open Format

(*
exception NoRuleApplies

(* --------------------- STORE ---------------------- *)
type store                  = term list 
let emptystore              = []
let addstore store v        = (List.length store, List.append store [v]) 
let lookuploc store l       = List.nth store l
let updatestore store n v   = 
    let rec f               = function 
        | (0,x::xs)                 -> v::xs
        | (i,x::xs)                 -> x::(f (i-1,xs))
        | _                         -> error dummyinfo "updatestore: BadIndex" in 
    f (n,store) 
let shiftstore i            = List.map (tmShift i) 

(* ---------------- EVAL FLOAT ------------------ *) 
let rec evalF1 ctx store = function
    | TmTimesfloat(fi,TmFloat(_,a),TmFloat(_,b)) -> TmFloat(fi,a*.b),store
    | TmTimesfloat(fi,(TmFloat(_,a)as f),t)      -> let t',s'=eval1 ctx store t in TmTimesfloat(fi,f,t'),s' 
    | TmTimesfloat(fi,t1,t2)                     -> let t1',s'=eval1 ctx store t1 in TmTimesfloat(fi,t1',t2),s'   
    | _                                          -> raise NoRuleApplies

(* ----------------- EVALUATION ------------------- *) 

and eval1 ctx store t = let p str = pr str;pr_tm ctx t; pn() in match t with  
    | TmRef(fi,v)when isval ctx v       ->  p"E-REFV        : "; let l,s' = addstore store v  in TmLoc(fi,l),s'
    | TmRef(fi,t)                       ->  p"E-REF         : "; let t',s'= eval1 ctx store t in TmRef(fi,t'),s'
    | TmDeref(fi,TmLoc(_,l))            ->  p"E-DEREFLOC    : "; (lookuploc store l,store)
    | TmDeref(fi,t)                     ->  p"E-DEREF       : "; let t',s'= eval1 ctx store t in TmDeref(fi,t'),s'
    | TmAssign(fi,TmLoc(_,l),v)when isval ctx v 
                                        ->  p"E-ASSIGN      : "; let s'=updatestore store l v in TmUnit(fi),s'
    | TmAssign(fi,v1,v2)when isval ctx v1 && isval ctx v2 
                                        ->  p"E-ASSIGN      : "; raise NoRuleApplies
    | TmAssign(fi,v,t)when isval ctx v  ->  p"E-ASSIGN1     : "; let t',s'=eval1 ctx store t in TmAssign(fi,v,t'),s'
    | TmAssign(fi,t1,t2)                ->  p"E-ASSIGN2     : "; let t1',s'=eval1 ctx store t1 in TmAssign(fi,t1',t2),s'
    | TmFix(fi,TmAbs(_,_,_,t2))         ->  p"E-FIXBETA     : "; tmSubstTop t t2,store
    | TmFix(fi,v) when isval ctx v      ->  p"E-FIXBETA     : "; raise NoRuleApplies 
    | TmFix(fi,t)                       ->  p"E-FIX         : "; let t',s'=eval1 ctx store t in TmFix(fi,t'),s' 
    | TmTag(fi,l,t,tyT)                 ->  p"E-TAG         : "; let t',s'=eval1 ctx store t in TmTag(fi,l,t',tyT),s'
    | TmIdx(fi,n,_)                     ->  p"E-VAR         : "; (match getbind fi ctx n with
        | BindTmAbb(t,_)                    -> t,store
        | _                                 -> raise NoRuleApplies) 
    | TmCase(fi,TmTag(_,l,v,_),cases) when isval ctx v  ->  
                                            p"E-CASETAGV    : ";
                                            (try let (x,t)= List.assoc l cases in  tmSubstTop v t,store
                                            with Not_found -> raise NoRuleApplies)
    | TmCase(fi,t,cases)                ->  p"E-CASE        : "; let t',s'=eval1 ctx store t in TmCase(fi,t',cases),s'
    | TmRecord(fi,flds)                 ->  p"E-RCD         : "; let rec ev_flds = ( function
        | []                                -> raise NoRuleApplies 
        | (l,v)::rest when isval ctx v      -> let rest',s' = ev_flds rest in ((l,v)::rest'),s'
        | (l,t)::rest                       -> let t',s'    = eval1 ctx store t in ((l,t')::rest),s' )
        in let flds',s'=ev_flds flds in TmRecord(fi,flds'),s'        
    | TmProj(fi,(TmRecord(_,flds)as v),l)                                           
        when isval ctx v                ->  p"E-PROJRCD     : "; 
                                            (try List.assoc l flds,store with Not_found -> raise NoRuleApplies)
    | TmProj(fi,t,l)                    ->  p"E-PROJ        : "; let t',s'=eval1 ctx store t in TmProj(fi,t',l),s'
    | TmAscribe(fi,v,_)when isval ctx v ->  p"E-ASCRIBEVAR  : "; v,store
    | TmAscribe(fi,u,tyT)               ->  p"E-ASCRIBE     : ";let t',s'=eval1 ctx store u in TmAscribe(fi,t',tyT),s' 
    | TmLet(fi,x,v1,t2)when isval ctx v1->  p"E-LETV        : "; tmSubstTop v1 t2, store
    | TmLet(fi,x,t1,t2)                 ->  p"E-LET         : "; let t1',s'=eval1 ctx store t1 in TmLet(fi,x,t1',t2),s'
    | TmApp(fi,TmUnfold(_,_),TmApp(_,TmFold(_,_),v)) when isval ctx v 
                                        ->  p"E-UNFLDFLD    : "; v,store
    | TmApp(fi,TmFold(f,tyT),t)         ->  p"E-FLD         : "; let t',s'=eval1 ctx store t in 
                                                                 TmApp(fi,TmFold(f,tyT),t'),s'
    | TmApp(fi,TmUnfold(f,tyT),t)       ->  p"E-UNFLD       : "; let t',s'=eval1 ctx store t in 
                                                                 TmApp(fi,TmUnfold(f,tyT),t'),s'
    | TmApp(fi,TmAbs(_,x,_,u),v) 
        when isval ctx v                ->  p"E-APPABS      : "; tmSubstTop v u ,store
    | TmApp(fi,v,t)                                          
        when isval ctx v                ->  p"E-APP1        : "; let t',s'=eval1 ctx store t in TmApp(fi,v,t'),s' 
    | TmApp(fi,t1,t2)                   ->  p"E-APP2        : "; let t1',s'=eval1 ctx store t1 in TmApp(fi,t1',t2),s'
    | TmIf(_,TmTrue(_),t2,t3)           ->  p"E-IFTRUE      : "; t2, store
    | TmIf(_,TmFalse(_),t2,t3)          ->  p"E-IFFLASE     : "; t3, store
    | TmIf(fi,t,t2,t3)                  ->  p"E-IF          : "; let t',s'=eval1 ctx store t in TmIf(fi,t',t2,t3),s'
    | TmSucc(fi,t)                      ->  p"E-SUCC        : "; let t',s'=eval1 ctx store t in TmSucc(fi,t'),s'
    | TmPred(_,TmZero(_))               ->  p"E-PREDZRO     : "; TmZero(dummyinfo),store
    | TmPred(_,TmSucc(_,nv1)) 
        when isnum ctx nv1              ->  p"E-PREDSUC     : "; nv1, store
    | TmPred(fi,t)                      ->  p"E-PRED        : "; let t',s'=eval1 ctx store t in TmPred(fi,t'),s'
    | TmIsZero(_,TmZero(_))             ->  p"E-ISZROZRO    : "; TmTrue(dummyinfo),store
    | TmTimesfloat(fi,_,_)              ->  p"E-TIMESFLOAT  : "; evalF1 ctx store t 
    | TmIsZero(_,TmSucc(_,nv1)) 
        when isnum ctx nv1              ->  p"E-ISZROSUC    : "; TmFalse(dummyinfo),store
    | TmIsZero(fi,t)                    ->  p"E-ISZRO       : "; let t',s'=eval1 ctx store t in TmIsZero(fi,t'),s'
    | _                                 ->  raise NoRuleApplies

let rec eval ctx store t =
    try let t',store' = eval1 ctx store t in eval ctx store' t' 
    with NoRuleApplies -> t,store


(*------------ Binding ------------*)

let evalbind ctx store      = function
    | BindTmAbb(t,tyT)          ->  let t',store' = eval ctx store t in BindTmAbb(t',tyT),store' 
    | bind                      ->  bind,store

let checkbind fi ctx        = function 
    | BindTmAbb(t,None)         ->  let tyT = typeof ctx t in BindTmAbb(t, Some(tyT))
    | BindTmAbb(t,Some(tyT))    ->  if tyeqv ctx(typeof ctx t)tyT then BindTmAbb(t,Some(tyT))else error fi"TyAbbErr"
    | bind                      ->  bind  

let process_command ctx store nextuvar constr = function 
    | Eval(fi,t)                ->
            pn();pr_tm ctx t;pn();
            pe"----------------------------------------------------";
            let tyT,nextuvar',constr' = recon ctx nextuvar t in
            pe"----------------   TYPE CHECKED !   ----------------";
            let t',store'   = eval ctx store t in
            pe"----------------   EVAL FINISHED !  ----------------"; 
            let constr''    = combineconstr constr constr' in 
            pr_constr ctx constr';pn(); flush stdout;
            pe"----------------  CONSTRAINTS LIST  ----------------";
            let sol         = unify fi ctx "Could not simplify constraints" constr' in
            pr_constr ctx sol ;pn(); 
            pe"----------------   SOLUTION FOUND ! ----------------";
            pr_tm ctx t';pb 1 2;pr ": ";pr_ty ctx (apply_constr sol (apply_constr sol tyT));pn();pn(); 
            ctx,store',nextuvar',constr''
    | Bind(fi,x,bind)           ->  
            pr x;pr" ";prbindty ctx bind;pn();
            pe"----------------   BINDING...   --------------------";
            let bind' = checkbind fi ctx bind in 
            let bind'',store' = evalbind ctx store bind' in 
            pe"----------------   BIND DONE !  --------------------";
            pn();pn(); addbind ctx x bind'',(shiftstore 1 store'), uvargen, constr

let rec process_commands ctx store nextuvar constr = function 
    | []                        ->  ctx,store,nextuvar,constr 
    | cmd::cmds                 ->  oobox0;
                                    let ctx',store',nextuvar',constr' 
                                        = process_command ctx store nextuvar constr cmd in 
                                    Format.print_flush();
                                    process_commands ctx' store' nextuvar' constr' cmds 


(*
let process_command ctx store = function 
    | Eval(fi,t)                ->
            pn();pr_tm ctx t;pn();
            pe"----------------------------------------------------";
            let tyT = typeof ctx t in
            pe"----------------   TYPE CHECKED !   ----------------";
            let t',store' = eval ctx store t in
            pe"----------------   EVAL FINISHED !  ----------------"; 
            pr_tm ctx t';pb 1 2;pr ": ";pr_ty ctx tyT;pn();pn(); ctx,store'
    | Bind(fi,x,bind)           ->  
            pr x;pr" ";prbindty ctx bind;pn();
            pe"----------------   BINDING...   --------------------";
            let bind' = checkbind fi ctx bind in 
            let bind'',store' = evalbind ctx store bind' in 
            pe"----------------   BIND DONE !  --------------------";
            pn();pn(); addbind ctx x bind'',(shiftstore 1 store')




let rec process_commands ctx store = function 
    | []                        ->  ctx,store 
    | cmd::cmds                 ->  oobox0;
                                    let ctx',store' = process_command ctx store cmd in 
                                    Format.print_flush();
                                    process_commands ctx' store' cmds 


*) 

*)

let rec eval = function 
    | [] -> []
    | (i,cn)::rest -> 
        
        (i,cn) :: rest 

let convert_abs2method = ()

