%{
    open Misc
    open Syntax 
    module BS = BatString
    
let reserved x                  =   BS.starts_with x "pre_" 
let check_reserved x            =   if reserved x           then err "Names 'pre_..' are reserved."
%}

%token CONTRACT
%token <string> ID
%token <Big_int.big_int> EUINT256
%token <Big_int.big_int> EUINT8
%token ADDRESS UINT256 UINT8 BYTES32
%token BOOL TRUE FALSE
%token PLUS MINUS MULT 
%token LT GT NOT NEQ EQ EQEQ LAND
%token DARROW
%token COMMA DOT SEMI
%token LPAR RPAR    LSQBR RSQBR     LBRACE RBRACE 
%token METHOD DEFAULT
%token IF THEN ELSE 
%token ALONG
%token RETURN BECOME
%token ABORT REENTRANCE
%token NEW SELFDESTRUCT
%token SENDER MSG VALUE BALANCE
%token THIS NOW UNIT
%token EVENT LOG 
%token BLOCK
%token INDEXED
%token EOF

%right DARROW

%left LAND
%left NEQ EQEQ LT GT
%left PLUS MINUS
%left MULT

%start <unit Syntax.toplevel list> file 
%%

%inline plist(X):
    delimited(LPAR,separated_list(COMMA,X),RPAR)    { $1                                                        }

file:
    | list(cntrct) EOF                              { $1                                                        }

cntrct:
    | CONTRACT ID plist(arg)LBRACE list(mthd)RBRACE { check_reserved $2; Cntrct{mthds=$5; cntrct_id=$2; cntrct_args=$3}  }
    | EVENT    ID plist(evnt_arg) SEMI              { Event (TyEvnt($2,$3))                                     }

mthd:
    | mthd_head block                               { {mthd_head=$1; mthd_body=$2}                              }

block:
    | LBRACE list(stmt) RBRACE                      { $2                                                        }

mthd_head:
    | DEFAULT                                       { TyDefault                                                 }
    | METHOD ty ID plist(arg)                       { TyMethod($3,$4,$2)                                        }
    | METHOD LPAR RPAR ID plist(arg)                { TyMethod($4,$5,TyTuple[])                                 }

arg:
    | ty ID                                         { check_reserved $2; TyVar($2,$1)                           }

evnt_arg:
    | arg                                           { let TyVar(id,ty)=$1 in TyEvVar(id,ty,false)               }
    | INDEXED ty ID                                 { TyEvVar($3,$2,true)                                       }

ty:
    | UINT256                                       { TyUint256                                                 }
    | UINT8                                         { TyUint8                                                   }
    | BYTES32                                       { TyBytes32                                                 }
    | ADDRESS                                       { TyAddr                                                    }
    | BOOL                                          { TyBool                                                    }
    | ty DARROW ty                                  { TyMap($1,$3)                                              }
    | ID                                            { TyInstnce $1                                              }

%inline body:
    | stmt                                          { [$1]                                                      }
    | block                                         { $1                                                        }
     
stmt:
    | ABORT SEMI                                    { SmAbort                                                   } 
    | RETURN ret THEN BECOME expr SEMI              { SmReturn{ret_expr=$2; ret_cont=$5}                        }
    | lexpr EQ expr SEMI                            { SmAssign($1,$3)                                           }
    | ty ID EQ expr SEMI                            { check_reserved $2;SmDecl{declTy=$1; declId=$2; declVal=$4}}
    | expr SEMI                                     { SmExpr $1                                                 }
    | IF expr THEN body ELSE body                   { SmIf($2,$4,$6)                                            }
    | IF expr THEN body                             { SmIfThen ($2, $4)                                         }
    | LOG ID expr_list SEMI                         { SmLog($2,$3,None)                                         }
    | SELFDESTRUCT expr SEMI                        { SmSlfDstrct $2                                            }

ret: 
    |                                               { TmUnit                                             ,()    }
    | expr                                          { $1                                                        }

%inline op:
    | PLUS                                          { fun(l,r)-> EpPlus(l,r)                                    }
    | MINUS                                         { fun(l,r)-> EpMinus(l,r)                                   }
    | MULT                                          { fun(l,r)-> EpMult(l,r)                                    }
    | LT                                            { fun(l,r)-> EpLT(l,r)                                      }
    | GT                                            { fun(l,r)-> EpGT(l,r)                                      }
    | EQEQ                                          { fun(l,r)-> EpEq(l,r)                                      }
    | NEQ                                           { fun(l,r)-> EpNEq(l,r)                                     }
    | LAND                                          { fun(l,r)-> EpLAnd(l,r)                                    } 

expr:
(*  | ABORT                                         { SmAbort,                                                          ()  } *)
    | TRUE                                          { EpTrue,                                                           ()  }
    | FALSE                                         { EpFalse,                                                          ()  }
(*  | IF expr THEN expr ELSE expr                   { TmIf($2,$4,$6),                                                   ()  } *)
    | EUINT256                                      { EpUint256 $1,                                                     ()  }
    | EUINT8                                        { EpUint8 $1,                                                       ()  }
    | expr op expr                                  { $2($1,$3) ,                                                       ()  }
    | NOT expr                                      { EpNot $2,                                                         ()  }
    | LPAR expr RPAR                                { EpParen $2,                                                       ()  }
    | VALUE   LPAR  MSG  RPAR                       { EpValue,                                                          ()  }
    | SENDER  LPAR  MSG  RPAR                       { EpSender,                                                         ()  }
    | BALANCE LPAR  expr RPAR                       { EpBalance $3,                                                     ()  }
    | NOW     LPAR BLOCK RPAR                       { EpNow,                                                            ()  }
    | ADDRESS LPAR  expr RPAR                       { EpAddr $3,                                                        ()  }
    | ID                                            { check_reserved $1; EpIdent $1,                                    ()  }
    | ID  expr_list                                 { Printf.printf "\n%s\n" $1; EpCall{call_id=$1;call_args=$2},       ()  }
    | NEW ID expr_list msg                          { check_reserved $2; EpNew {new_id=$2;new_args=$3; new_msg=$4},     ()  }
    | expr DOT DEFAULT LPAR RPAR msg                { EpSend{sd_cn=$1;sd_mthd=None   ;sd_args=[];sd_msg=$6},            ()  }
    | expr DOT ID   expr_list msg                   { EpSend{sd_cn=$1;sd_mthd=Some $3;sd_args=$4;sd_msg=$5},            ()  }
    | THIS                                          { EpThis,                                                           ()  }
    | expr LSQBR expr RSQBR                         { EpArray{arrId=$1;arrIndex=$3},                                    ()  }
     
%inline expr_list:
    | plist(expr)                                   { $1                                                                    }

msg:
    | value_info                                    { $1                                                                    }
     
value_info:
    | (* empty *)                                   { EpFalse,()                                                            }
    | ALONG expr                                    { $2                                                                    }
     
lexpr:                                              (* expr '[' expr ']' *) 
    | expr LSQBR expr RSQBR                         { LEpArray{arrId=$1; arrIndex=$3}                                       }
     
