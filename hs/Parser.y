{ 
module Parser where 

import Lexer 
import Prelude hiding (lex, EQ, LT, GT) 

import GCLL
import Predicate 
import Tree
import Type
import Term
import AST
import PsrCtx



} 

%name parse
%tokentype  { Token      } 
%error      { parseError }

%token 
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
    case        { CASE              } 
    new         { NEW               } 
    call        { CALL              } 
    sender      { SENDER            } 
    send        { SEND              } 
    msg         { MSG               } 
    amount      { AMOUNT            } 
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
    comment     { COMMENT $$        } 
    id          { ID $$             } 
    E           { E'                } 
    A           { A'                } 
    X           { X'                } 
    F           { F'                } 
    G           { G'                } 
    U           { U'                } 
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
    : contract id '{' Top '}'           { CN $2 (fst ($4 emptyCtx))     } 

Top : StoVars   Top                     { \ctx  ->  let (svs, ctx')     = $1 ctx    in
                                                    let (top, ctx'')    = $2 ctx'   in 
                                                    (svs ++ top        , ctx'')           } 
    | Mthd      Top                     { \ctx ->   ($1 ctx : fst($2 ctx) , ctx)        }
    |                                   { \ctx ->   ([],               ctx  )           } 

Mthd 
    : method id Params ':' Ty ':=' 
      Predicate Body Predicate          { \ctx -> 
                                            let (params,ctx') = $3 ctx  in 
                                            let (body,ctx'')  = $8 ctx' in 
                                            MT $2 params $5 ($7 ctx'') body ($9 ctx'')  }

StoVars
    : IDs ':' Ty ';'                    { \ctx ->   mapStoTy $3 $1 ctx                  } 

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
    | Ty '->' Ty                        { TyABS $1 $3       } 
    | '(' Tys ')'                       { TyPROD $2         } 

Tys : Ty ',' Tys                        { $1 : $3           } 
    |                                   { []                } 

ATy : bool                              { TyBOOL            } 
    | u8                                { TyU8              } 
    | i8                                { TyI8              } 
    | u256                              { TyU256            } 
    | i256                              { TyI256            } 
    | '(' Ty ')'                        { $2                } 


Body 
    : Tm                                { \ctx -> 
                                            let (tm, ctx')   = $1 ctx in 
                                            (BODY []    tm, ctx' )                  } 
    | Decls Tm                          { \ctx -> 
                                            let (decls,ctx') = $1 ctx  in 
                                            let (tm,  ctx'') = $2 ctx' in
                                            (BODY decls tm, ctx'')                  } 

Decls 
    : Decl ';' Decls                    { \ctx -> 
                                            let (decl, ctx')    = $1 ctx in 
                                            let (decls, ctx'')  = $3 ctx' in 
                                            (decl:decls, ctx'')                 } 
    |                                   { \ctx -> ([], ctx)                     } 

Decl 
    : let id Params '=' Tm Predicate    { \ctx -> 
                                            let (params, ctx') = $3 ctx in 
                                            let (tm, ctx'')    = $5 ctx' in 
                                            (LET $2 params tm ($6 ctx''), ctx'')    }

Predicate 
    : '{' Formulae '}'                  { \ctx -> Just ($2 ctx)                 } 
    |                                   { \ctx -> Nothing                       } 

Formulae
    : AFormulae                         { \ctx -> $1 ctx                        } 
    | AFormulae and  AFormulae          { \ctx -> FAnd ($1 ctx) ($3 ctx)        } 
    | '~' AFormulae                     { \ctx -> FNot ($2 ctx)                 } 
    | E PathFormulae                    { \ctx -> E    ($2 ctx)                 } 
    | A PathFormulae                    { \ctx -> A    ($2 ctx)                 } 

PathFormulae
    : X AFormulae                       { \ctx -> X     ($2 ctx)                } 
    | F AFormulae                       { \ctx -> F     ($2 ctx)                } 
    | G AFormulae                       { \ctx -> G     ($2 ctx)                } 
    | AFormulae U AFormulae             { \ctx -> Union ($1 ctx) ($3 ctx)       } 

AFormulae   
    : '(' Formulae ')'                  { $2                                            } 
    | Tm '=' Tm                         { \ctx -> FAtomic(AEq(fst($1 ctx))(fst($3 ctx)))}
    | Tm '<' Tm                         { \ctx -> FAtomic(ALt(fst($1 ctx))(fst($3 ctx)))}
    | Tm '>' Tm                         { \ctx -> FAtomic(AGt(fst($1 ctx))(fst($3 ctx)))}
    | true                              { \ctx -> FTrue                                 } 


Tm  : AppTm                             { $1                                } 

AppTm 
    : PathTm                            { $1                                } 
    | '~' PathTm                        { \ctx -> 
                                            let (t,ctx') = $2 ctx in 
                                            (RED TmNOT [t], ctx')           } 
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

Num : num                               { TmU256 $1                 } 

Bool
    : true                              { TmTRUE                    } 
    | false                             { TmFALSE                   } 






{

parseError :: [Token] -> a 
parseError t = error $ show t 

} 
