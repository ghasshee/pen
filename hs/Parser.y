{ 
module Parser where 

import Lexer 
import Prelude hiding (lookup, lex, EQ, LT, GT) 

import GCLL
import Tree
import Type
import Data
import Term
import Pattern
import AST
import PsrCtx



} 

%name parse
%tokentype  { Token      } 
%error      { parseError }

%token 
    data        { DATA              } 
    '_'         { WILD              } 
    '|'         { VBAR              } 
    case        { CASE              } 
    of          { OF                } 
    let         { LET'              } 
    true        { TRUE              } 
    false       { FALSE             } 
    num         { NUM $$            } 
    event       { EVENT             } 
    contract    { CONTRACT          } 
    method      { METHOD            } 
    return      { RETURN            } 
    become      { BECOME            } 
    if          { IF                } 
    then        { THEN              } 
    else        { ELSE              } 
    i8          { I8                } 
    u8          { U8                } 
    i256        { I256              } 
    u256        { U256              } 
    bool        { BOOL              } 
    new         { NEW               } 
    call        { CALL              } 
    sender      { SENDER            } 
    send        { SEND              } 
    msg         { MSG               } 
    amount      { AMOUNT            } 
    table       { TABLE             } 
    balance     { BALANCE           } 
    dstrct      { DESTRUCT          } 
    this        { THIS              } 
    now         { NOW               } 
    '()'        { UNIT              } 
    log         { LOG               } 
    kec         { KECCAK            } 
    '->'        { ARROW             } 
    '=>'        { DARROW            } 
    '('         { LPAREN            } 
    ')'         { RPAREN            } 
    '{'         { LBRACE            } 
    '}'         { RBRACE            } 
    '['         { LSQUARE           } 
    ']'         { RSQUARE           } 
    ';'         { SEMI              } 
    ':'         { COLON             } 
    '='         { EQ                } 
    ':='        { COLONEQ           } 
    ','         { COMMA             } 
    '.'         { DOT               } 
    '~'         { NOT               } 
    '>'         { GT                } 
    '<'         { LT                } 
    '>='        { GE                } 
    '<='        { LE                } 
    '=='        { EQEQ              } 
    '!='        { NEQ               } 
    '&&'        { AND               }
    '||'        { OR                } 
    '+'         { PLUS              } 
    '-'         { MINUS             } 
    '*'         { MULT              } 
    '/'         { DIV               } 
    '%'         { MOD               } 
    comment     { COMMENT $$        } 
    cid         { CID $$            } 
    id          { ID $$             } 
    E           { POSSIBLY          } 
    A           { NECESSARILY       } 
    X           { NEXT              } 
    F           { FUTURELY          } 
    G           { GLOBALLY          } 
    U           { UNTIL             } 
    always      { ALWAYS            } 
    never       { NEVER             } 
    by          { BY                } 
    and         { AND'              } 
    or          { OR'               } 


%right ':=' 
%right ';' 
%nonassoc '>' '<' 

%% 

TopLevel 
    : Contract TopLevel                 { $1 : $2           } 
    |                                   { []                }

Contract         
    : contract id '{' Tops '}'          { CN $2 (fst ($4 emptyCtx))     } 

Tops
    :                                   { \ctx ->   ([MT "__end__" TyERR [] (BODY Nothing [] (RED TmERR []) Nothing)],  ctx  )           } 
    | Mthd      Tops                    { \ctx ->   ($1 ctx : fst($2 ctx) , ctx)        }
    | Sto       Tops                    { \ctx  ->  let (svs, ctx')     = $1 ctx    in
                                                    let (top, ctx'')    = $2 ctx'   in 
                                                    (svs ++ top           , ctx'')      } 
    | Data      Tops                    { \ctx ->   let (dt, ctx')      = $1 ctx in       
                                                    let (top, ctx'')    = $2 ctx' in 
                                                    ([dt] ++ top, ctx'') } 


Data 
    : data cid IDs ':=' Constrs         { \ctx ->   let ctx'        = addCtx DTA $2 ctx in 
                                                    let (cs,ctx'')  = $5 ctx' in 
                                                    (DT $2 [] ($3) cs, ctx'' ) } 
Constrs 
    : Constr '|' Constrs                { \ctx ->   let (c,ctx')    = $1 ctx in 
                                                    let (cs, ctx'') = $3 ctx' in 
                                                    (c:cs, ctx'')               }
    | Constr                            { \ctx ->   let (c,ctx') = $1 ctx in 
                                                    ([c],  ctx')                }  
    |                                   { \ctx ->   ([],   ctx)                 } 
Constr 
    : cid CTys                          { \ctx -> (DConstr $1 $2, addCtx DTA $1 ctx) }
CTys 
    : ATy CTys                           { $1 : $2 } 
    |                                    { [] } 

Sto
    : IDs ':' Ty ';'                    { \ctx ->   mapStoTy $3 $1 ctx                  } 

Mthd 
    : method id Params ':' Ty ':=' Body { \ctx -> 
                                            let (params,ctx') = $3 ctx  in 
                                            MT $2 $5 params ($7 ctx')  }
Params  
    : Param Params                      { \ctx -> 
                                            let (param,ctx')   = $1 ctx in 
                                            let (params,ctx'') = $2 ctx' in 
                                            (param ++ params , ctx'')           } 
    |                                   { \ctx -> ([],ctx)                      } 


Param 
    : '(' IDs ':' Ty ')'                { \ctx -> mapParamTy $4       $2  ctx         }  
    | id                                { \ctx -> mapParamTy Untyped [$1] ctx         } 

IDs : id IDs                            { $1 : $2           } 
    |                                   { []                }   

Ty  : ATy                               { $1                } 
    |  TyApp                            { $1                } 
    | Ty '->' Ty                        { TyARR $1 $3       } 
    | '(' Tys ')'                       { TyPROD $2         } 

Tys : Ty ',' Tys                        { $1 : $3           } 
    |                                   { []                } 

ATy : bool                              { TyBOOL            } 
    | u8                                { TyU8              } 
    | i8                                { TyI8              } 
    | u256                              { TyU256            } 
    | i256                              { TyI256            } 
    | id                                { TyID $1           } 
    | cid                               { TyD $1            } 
    | '()'                              { TyUNIT            } 
    | '(' Ty ')'                        { $2                } 

TyApp 
    : Ty ATy                            { TyAPP $1 $2       }  


Body 
    : Predicate Tm Predicate            { \ctx -> 
                                            let (tm, ctx')   = $2 ctx in 
                                            BODY ($1 ctx') [] tm ($3 ctx')                   } 
    | Predicate Decls Tm Predicate      { \ctx -> 
                                            let (decls,ctx') = $2 ctx  in 
                                            let (tm,  ctx'') = $3 ctx' in
                                            BODY ($1 ctx'') decls tm ($4 ctx'')               } 

Decls 
    : Decl     Decls                    { \ctx -> 
                                            let (decl, ctx')    = $1 ctx in 
                                            let (decls, ctx'')  = $2 ctx' in 
                                            (decl:decls, ctx'')                 } 
    |                                   { \ctx -> ([], ctx)                     } 

Decl 
    : let id Params '=' Tm ';' Predicate { \ctx -> 
                                            case lookup' $2 ctx of 
                                            Just (TmSTO n)  -> 
                                                let id'             = '\'':$2               in 
                                                let ctx'            = addCtx STO $2 ctx     in 
                                                let ctx''''         = addCtx STO id' ctx    in 
                                                let ([], ctx'')     = $3 ctx'               in 
                                                let (tm, ctx''')    = $5 ctx''''            in 
                                                (SLET id' Untyped tm ($7 ctx'''), ctx') 
                                            _               -> 
                                                let ctx'            = addCtx VAR $2 ctx     in 
                                                let (params, ctx'') = $3 ctx'               in 
                                                case params of 
                                                []          -> 
                                                    let (tm, ctx''')    = $5 ctx''              in 
                                                    (LET  $2 Untyped tm ($7 ctx'''), ctx') 
                                                _           -> 
                                                    let (tm, ctx''')    = $5 ctx''              in 
                                                    (FLET $2 params Untyped tm ($7 ctx'''), ctx') } 

Predicate 
    : '{' Formulae '}'                  { \ctx -> Just ($2 ctx)                 } 
    |                                   { \ctx -> Nothing                       } 

Formulae
    : '~' AFormulae                     { \ctx -> FNot ($2 ctx)                 } 
    | E PathFormulae                    { \ctx -> FE    ($2 ctx)                 } 
    | A PathFormulae                    { \ctx -> FA    ($2 ctx)                 } 
    | always Formulae                   { \ctx -> FA (FG ($2 ctx))                } 
    | AppFormulae                       { $1 } 

AppFormulae 
    : Formulae and  Formulae            { \ctx -> FAnd ($1 ctx) ($3 ctx)        } 
    | TFormulae                         { \ctx -> $1 ctx                        } 

PathFormulae
    : X Formulae                        { \ctx -> FX     ($2 ctx)                } 
    | F Formulae                        { \ctx -> FF     ($2 ctx)                } 
    | G Formulae                        { \ctx -> FG     ($2 ctx)                } 
    | Formulae U Formulae               { \ctx -> FU     ($1 ctx) ($3 ctx)       } 

TFormulae 
    : Tm '==' Tm                        { \ctx -> FAtom(AEq(fst($1 ctx))(fst($3 ctx)))}
    | Tm '<' Tm                         { \ctx -> FAtom(ALt(fst($1 ctx))(fst($3 ctx)))}
    | Tm '>' Tm                         { \ctx -> FAtom(AGt(fst($1 ctx))(fst($3 ctx)))}
    | Tm '<=' Tm                        { \ctx -> FAtom(ALe(fst($1 ctx))(fst($3 ctx)))}
    | Tm '>=' Tm                        { \ctx -> FAtom(AGe(fst($1 ctx))(fst($3 ctx)))}
    | AFormulae                         { $1 } 

AFormulae   
    : '(' Formulae ')'                  { $2                                            } 
    | true                              { \ctx -> FTrue                                 } 


BOp : '=='                              { "=="  } 
    | '!='                              { "!="  } 
    | '&&'                              { "&&"  }
    | '||'                              { "||"  } 
    | '<'                               { "<"   } 
    | '>'                               { ">"   } 
    | '<='                              { "<="  } 
    | '>='                              { ">="  } 
    | '+'                               { "+"   } 
    | '-'                               { "-"   } 
    | '*'                               { "*"   } 
    | '/'                               { "/"   } 
    | '%'                               { "%"   } 


PatternTms 
    : Pattern '->' Tm  '|' PatternTms   { \ctx -> 
                                            let (p,  ctx')   = $1 ctx   in 
                                            let (t,  ctx'')  = $3 ctx'  in 
                                            let (ps, ctx''') = $5 ctx'' in 
                                            (RED (TmPATTERN p) [t] : ps , ctx''')  }
    | Pattern '->' Tm                   { \ctx -> 
                                            let (p, ctx')    = $1 ctx  in 
                                            let (t, ctx'')   = $3 ctx' in 
                                            ([RED (TmPATTERN p) [t]] , ctx'') } 
    |                                   { \ctx ->  ([], ctx)    }  

Pattern 
    : '_'                               { \ctx -> (PWild          , ctx)  }
    | id                                { \ctx -> let ctx' = ctx in 
                                                  (PVar $1        , ctx')  }
    | cid Patterns                      { \ctx -> let ctx' = ctx in 
                                                  let (ps, ctx'') = $2 ctx' in 
                                                  (PCon $1 ps, ctx'')  } 

Patterns
    : '(' Pattern      ')' Patterns     { \ctx -> let (p,ctx') = $2 ctx in 
                                                  let (ps,ctx'') = $4 ctx in 
                                                  ( p : ps , ctx'')     }  
    | '(' cid Patterns ')' Patterns     { \ctx -> let (ps, ctx') = $3 ctx in 
                                                  let (ps', ctx'') = $5 ctx' in 
                                                  (PCon $2 ps : ps' , ctx'')  }
    | '_'                  Patterns     { \ctx -> let (ps, ctx') = $2 ctx in 
                                                  (PWild : ps, ctx') }
    | id                   Patterns     { \ctx -> let ctx' = ctx in 
                                                  let (ps, ctx'') = $2 ctx' in 
                                                  (PVar $1 : ps, ctx'')  }
    |                                   { \ctx -> ([], ctx)  } 

Tm  : if Tm then Tm else Tm             { \ctx -> 
                                            let (b,ctx') = $2 ctx in 
                                            let (t1,ctx'') = $4 ctx' in 
                                            let (t2,ctx''') = $6 ctx'' in 
                                            (RED TmIF [b,t1,t2], ctx''') } 
    | case ATm of '|' PatternTms        { \ctx -> 
                                            let (t,ctx') = $2 ctx in 
                                            let (ps, ctx'') = $5 ctx' in 
                                            (RED TmCASE (t:ps), ctx'')  }
    | case ATm of PatternTms            { \ctx -> 
                                            let (t,ctx') = $2 ctx in 
                                            let (ps, ctx'') = $4 ctx' in 
                                            (RED TmCASE (t:ps), ctx'')  }
    | Tm BOp Tm                         { \ctx -> 
                                            let (t1,ctx') = $1 ctx in 
                                            let (t2,ctx'') = $3 ctx' in 
                                            (RED (TmBOP $2) [t1,t2], ctx'') } 
    | AppTm                             { $1                                } 




AppTm 
    : PathTm                            { $1                                } 
    | '~' PathTm                        { \ctx -> 
                                            let (t,ctx') = $2 ctx in 
                                            (RED TmNOT [t], ctx')           } 
    | '-' PathTm                        { \ctx -> 
                                            let (t,ctx') = $2 ctx in 
                                            (RED (TmUOP "-") [t], ctx')     } 

    | AppTm PathTm                      { \ctx -> 
                                            let (t1,ctx') = $1 ctx in 
                                            let (t2,ctx'') = $2 ctx' in 
                                            (RED TmAPP [t1,t2], ctx'')      } 

PathTm 
    : ATm                               { $1                                } 

ATm : '(' Tm ')'                        { $2                                } 
    | return Tm                         { \ctx -> 
                                            let (tm,ctx') = $2 ctx in 
                                            (RED TmRET [tm],ctx')            } 
    | Num                               { \ctx -> (RED $1 [], ctx)          } 
    | Bool                              { \ctx -> (RED $1 [], ctx)          } 
    | amount                            { \ctx -> (RED TmAMOUNT [], ctx)    } 
    | this                              { \ctx -> (RED TmTHIS   [], ctx)    } 
    | sender                            { \ctx -> (RED TmSENDER [], ctx)    } 
    | id                                { \ctx -> (RED (lookup $1 ctx) [], ctx) } 
    | cid                               { \ctx -> (RED (lookup $1 ctx) [], ctx) } 

Num : num                               { let f n = case n of 
                                                        0 -> DZero 
                                                        _ -> DSucc (f (n-1)) in 
                                                    TmDATA (f $1)                 } 

Bool
    : true                              { TmTRUE                    } 
    | false                             { TmFALSE                   } 






{


parseError :: [Token] -> a 
parseError t = error $ show t 

} 
