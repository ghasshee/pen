{ 
module Parser where 

import Lexer 
import Prelude hiding (lex, EQ, LT, GT) 

import GCLL
import Tree
import Type
import Term
import AST


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


%right ':=' 
%right ';' 
%nonassoc '>' '<' 

%% 

TopLevel 
    : Top TopLevel                      { $1 : $2           } 
    |                                   { []                }

Top         
    : contract id Params '{' Mthds '}'  { CN $2 $3 $5       } 
    | event    id Ty                    { EV $2 $3          } 

Params  
    : Param Params                      { $1 ++ $2          }  
    |                                   { []                } 

Param 
    : '(' IDs ':' Ty ')'                { mapTy $4 $2       }  
    | id 
    { [($1, Untyped)]   } 

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

Mthds 
    : Mthd Mthds                        { $1 : $2           } 
    |                                   { []                } 

Mthd 
    : method id Params ':' Ty ':=' Body { MT $2 $3 $5 $7    } 

Body 
    : Tm                                { BODY [] $1        } 
    | Decls Tm                          { BODY $1 $2        } 

Decls 
    : Decl ';' Decls                    { $1 : $3           } 
    |                                   { []                } 

Decl 
    : let id Params '=' Tm              { LET $2 $3 $5      } 



Tm  : AppTm                             { $1                } 

AppTm 
    : PathTm                            { $1                } 
    | '~' PathTm                        { RED TmNOT [$2]    } 
    | AppTm PathTm                      { RED TmAPP [$1,$2] } 

PathTm 
    : ATm                               { $1                } 

ATm : '(' Tm ')'                        { $2                } 
    | return Tm                         { RED TmRET [$2]    } 
    | Num                               { RED $1    []      } 
    | Bool                              { RED $1    []      } 
    | amount                            { RED TmAMOUNT []   } 
    | this                              { RED TmTHIS []     } 
    | sender                            { RED TmSENDER []   } 

Num : num                               { TmU256 $1         } 

Bool
    : true                              { TmTRUE            } 
    | false                             { TmFALSE           } 






{

mapTy ty = map (\i -> (i,ty)) 

parseError :: [Token] -> a 
parseError t = error $ show t 


}
