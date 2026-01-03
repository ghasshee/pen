{ 
module Lexer where 


import Prelude hiding (lex, EQ, LT, GT) 

} 

%wrapper "basic" 

$non0       = [1-9] 
$digit      = [0-9] 
$space      = [\ \t] 
$letter     = [a-zA-Z] 
$capital    = [A-Z]
$hex        = [0-9A-Fa-f] 
$nl         = [\n] 
$symbol     = [\$\#\@\!\%\^\&\\\*\(\)\+\-\;\:\:\=\/\>\<\.\,\|] 

@any        = [$digit $space $letter $symbol]*     

@id         = [\_ $letter]* [$digit $letter]+  

@cid        = [$capital] [$digit $letter]* 

token :-
    $white+     ; 
    $nl+        ; 
    __end__     { \s -> error "cannot use __end__ : RESERVED for Compiler" } 
    true        { \s -> TRUE            } 
    false       { \s -> FALSE           } 
    data        { \s -> DATA            } 
    case        { \s -> CASE            } 
    of          { \s -> OF              } 
    "|"         { \s -> VBAR            } 
    "_"         { \s -> WILD            } 
    let         { \s -> LET'            } 
    $digit+     { \s -> NUM (read s)    }  
    0x$hex+     { \s -> NUM (read s)    } 
    event       { \s -> EVENT           }
    contract    { \s -> CONTRACT        } 
    method      { \s -> METHOD          }
    return      { \s -> RETURN          }
    become      { \s -> BECOME          }
    if          { \s -> IF              }
    then        { \s -> THEN            }
    else        { \s -> ELSE            }
    i256        { \s -> I256            }
    u256        { \s -> U256            }
    i8          { \s -> I8              }
    u8          { \s -> U8              }
    bool        { \s -> BOOL            }
    new         { \s -> NEW             }
    call        { \s -> CALL            }
    sender      { \s -> SENDER          }
    send        { \s -> SEND            }
    message     { \s -> MSG             }
    table       { \s -> TABLE           } 
    amount      { \s -> AMOUNT          }
    balance     { \s -> BALANCE         }
    destruct    { \s -> DESTRUCT        }
    this        { \s -> THIS            }
    now         { \s -> NOW             }
    "()"        { \s -> UNIT            }
    log         { \s -> LOG             }
    keccak      { \s -> KECCAK          }
    until       { \s -> UNTIL           } 
    exists      { \s -> EXISTS                  } 
    forall      { \s -> FORALL                  } 
    next        { \s -> NEXT        -- X        } 
    futurely    { \s -> FUTURELY    -- F        } 
    globally    { \s -> GLOBALLY    -- G        } 
    always      { \s -> ALWAYS      -- AG       } 
    never       { \s -> NEVER       -- not EF   } 
    possibly    { \s -> POSSIBLY    -- E        } 
    necessarily { \s -> NECESSARILY -- A        }
    by          { \s -> BY          -- ( F p ) U ( F q )} 
    "->"        { \s -> ARROW           }
    "=>"        { \s -> DARROW          }
    "("         { \s -> LPAREN          }
    ")"         { \s -> RPAREN          }
    "{"         { \s -> LBRACE          } 
    "}"         { \s -> RBRACE          } 
    "["         { \s -> LSQUARE         } 
    "]"         { \s -> RSQUARE         } 
    ";"         { \s -> SEMI            } 
    ":"         { \s -> COLON           } 
    ":="        { \s -> COLONEQ         } 
    ","         { \s -> COMMA           } 
    "."         { \s -> DOT             } 
    "~"         { \s -> NOT             } 
    "="         { \s -> EQ              } 
    "&&"        { \s -> AND             }
    "||"        { \s -> OR              } 
    "=="        { \s -> EQEQ            } 
    "!="        { \s -> NEQ             } 
    "<"         { \s -> LT              } 
    ">"         { \s -> GT              } 
    "<="        { \s -> LE              } 
    ">="        { \s -> GE              } 
    "+"         { \s -> PLUS            } 
    "-"         { \s -> MINUS           } 
    "*"         { \s -> MULT            } 
    "/"         { \s -> DIV             } 
    "%"         { \s -> MOD             } 
    "--" @any   { \s -> COMMENT s       } 
    @cid        { \s -> CID s           } 
    @id         { \s -> ID s            } 
    A           { \s -> A'              } 
    E           { \s -> E'              } 
    F           { \s -> F'              } 
    G           { \s -> G'              } 
    X           { \s -> X'              } 
    U           { \s -> U'              } 
    '/\'        { \s -> AND'            } 
    '\/'        { \s -> OR'             } 
    
{ 

data Token  
            = EOF 
            | COMMENT String 
            | DATA | VBAR | CASE | OF | WILD
            | EXISTS | FORALL
            | NEXT | FUTURELY | GLOBALLY | UNTIL  
            | POSSIBLY | NECESSARILY 
            | ALWAYS | NEVER | BY 
            | TRUE
            | FALSE 
            | NUM Integer 
            | LET' 
            | EVENT 
            | CONTRACT 
            | METHOD
            | RETURN 
            | BECOME 
            | IF | THEN | ELSE 
            | U8 | I8 | U256 | I256 
            | BOOL
            | LT | GT | LE | GE 
            | EQ | NEQ | EQEQ
            | AND | OR 
            | PLUS |MINUS | MULT | DIV | MOD 
            | NEW 
            | CALL 
            | SENDER | SEND 
            | MSG
            | AMOUNT | BALANCE | TABLE  
            | DESTRUCT | THIS | UNIT | LOG | KECCAK | NOW 
            | ARROW | DARROW 
            | LPAREN | RPAREN 
            | LBRACE | RBRACE | LSQUARE | RSQUARE 
            | SEMI | COLON | COLONEQ 
            | COMMA | DOT | NOT 
            | ID String | CID String 
            | A' | E' | F' | G' | X' | U' | AND' | OR' 
            deriving (Show, Eq, Read) 

-- lex = alexScanTokens 

lex = filter notComment . alexScanTokens where 
    notComment (COMMENT _) = False 
    notComment _ = True 
} 


    
