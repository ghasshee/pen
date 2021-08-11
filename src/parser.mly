%{
    open Misc
    open Syntax 
    module BS = BatString
    
let reserved x                  =   if BS.starts_with x "pre_" then err "Names 'pre_..' are reserved." 
%}

%token CONTRACT
%token <string> ID
%token <Big_int.big_int> EUINT256
%token <Big_int.big_int> EUINT8
%token LAM ARROW LARROW
%token ADDRESS UINT256 UINT8 BYTES32
%token BOOL TRUE FALSE
%token PLUS MINUS MULT 
%token LT GT NOT NEQ EQ EQEQ LAND
%token DARROW
%token COMMA DOT SEMI COLON
%token LPAR RPAR    LSQBR RSQBR     LBRACE RBRACE 
%token METHOD DEFAULT
%token IF THEN ELSE 
%token WITH
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
    | CONTRACT ID plist(arg)LBRACE list(mthd)RBRACE { reserved $2; Cntrct{mthds=$5; cn_id=$2; fields =$3}  }
    | EVENT    ID plist(evnt_arg) SEMI              { Event (TyEv($2,$3))                                     }

mthd:
    | mthd_head block                               { {mthd_head=$1; mthd_body=$2}                              }

block:
    | LBRACE list(stmt) RBRACE                      { $2                                                        }

mthd_head:
    | DEFAULT                                       { TyDefault                                                 }
    | METHOD ty ID plist(arg)                       { TyMthd($3,$4,$2)                                        }
    | METHOD LPAR RPAR ID plist(arg)                { TyMthd($4,$5,TyTuple[])                                 }

arg:
    | ty ID                                         { reserved $2; TyVar($2,$1)                           }

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
    | lexpr EQ tm SEMI                              { SmAssign($1 [],$3 [])                                     }
    | ty ID EQ tm SEMI                              { reserved $2;SmDecl{declTy=$1; declId=$2; declVal=$4 []}   }
    | IF tm THEN body ELSE body                     { SmIf($2 [],$4,$6)                                         }
    | IF tm THEN body                               { SmIfThen ($2 [], $4)                                      }
    |  tm  SEMI                                     { SmExpr ($1 [])                                            }

ret: 
    |                                               { fun ctx -> TmUnit                                  ,()    }
    | tm                                            { $1                                                        }

%inline op:
    | LT                                            { fun l r -> EpLT(l,r)                                      }
    | GT                                            { fun l r -> EpGT(l,r)                                      }
    | EQEQ                                          { fun l r -> EpEq(l,r)                                      }
    | NEQ                                           { fun l r -> EpNEq(l,r)                                     }
    | LAND                                          { fun l r -> EpLAnd(l,r)                                    } 
    | PLUS                                          { fun l r -> EpPlus(l,r)                                    }
    | MULT                                          { fun l r -> EpMult(l,r)                                    }
    | MINUS                                         { fun l r -> EpMinus(l,r)                                   }

tm: 
    | appTm                                         { $1                                                                    } 
    | LAM ID COLON ty ARROW tm                      { fun ctx -> TmAbs($2, $4, $6 ctx)                                  ,() } 
    | lexpr EQ tm                                   { fun ctx -> TmAssign($1 ctx, $3 ctx)                               ,() }
    | LOG ID  arg_list                              { fun ctx -> TmLog($2,$3 ctx,None)                                  ,() }
    | SELFDESTRUCT tm                               { fun ctx -> TmSlfDstrct($2 ctx)                                    ,() }
appTm:
    | pathTm                                        { $1                                                                    }
    | appTm pathTm                                  { fun ctx -> TmApp($1 ctx,$2 ctx)                                   ,() } 
    | NOT   pathTm                                  { fun ctx -> EpNot ($2 ctx)                                         ,() }
    | ID   arg_list                                 { fun ctx -> EpCall{call_id=$1;call_args=$2 ctx}                    ,() }
pathTm:
    | aTm                                           { $1                                                                    }
aTm:
    | LPAR tm RPAR                                  { fun ctx -> EpParen ($2 ctx)                                       ,() }
    | tm op tm                                      { fun ctx -> $2 ($1 ctx)($3 ctx)                                    ,() }
    | RETURN ret THEN BECOME tm                     { fun ctx -> TmReturn($2 ctx,$5 ctx)                                ,() }
    | ABORT                                         { fun ctx -> TmAbort                                                ,() } 
    | TRUE                                          { fun ctx -> EpTrue                                                 ,() }
    | FALSE                                         { fun ctx -> EpFalse                                                ,() }
    | EUINT256                                      { fun ctx -> EpUint256 $1                                           ,() }
    | EUINT8                                        { fun ctx -> EpUint256 $1                                           ,() }
    | VALUE   LPAR  MSG  RPAR                       { fun ctx -> EpValue                                                ,() }
    | SENDER  LPAR  MSG  RPAR                       { fun ctx -> EpSender                                               ,() }
    | BALANCE LPAR  tm   RPAR                       { fun ctx -> EpBalance ($3 ctx)                                     ,() }
    | NOW     LPAR BLOCK RPAR                       { fun ctx -> EpNow                                                  ,() }
    | ADDRESS LPAR  tm   RPAR                       { fun ctx -> EpAddr ($3 ctx)                                        ,() }
    | ID                               { reserved $1; fun ctx -> TmId $1                                                ,() }
    | NEW ID  arg_list msg             { reserved $2; fun ctx -> EpNew {new_id=$2;new_args=$3 ctx; new_msg=$4 ctx}      ,() }
    |  tm  DOT DEFAULT LPAR RPAR msg                { fun ctx -> EpSend{sd_cn=$1 ctx; sd_mthd=None   ; sd_args=[]    ; sd_msg=$6 ctx},            ()  }
    |  tm  DOT ID    arg_list msg                   { fun ctx -> EpSend{sd_cn=$1 ctx; sd_mthd=Some $3; sd_args=$4 ctx; sd_msg=$5 ctx},            ()  }
    | THIS                                          { fun ctx -> EpThis,                                                           ()  }
    |  tm  LSQBR  tm  RSQBR                         { fun ctx -> EpArray{arrId=$1 ctx;arrIndex=$3 ctx},                                    ()  }
     
arg_list : 
    | LPAR RPAR                                     { fun ctx -> []                                                         }
    | LPAR args RPAR                                { fun ctx -> $2 ctx                                                     } 
args: 
    | tm                                            { fun ctx -> [$1 ctx]                                                   }
    | tm COMMA args                                 { fun ctx -> $1 ctx :: $3 ctx                                           } 
(*
%inline expr_list:
    | plist(expr)                                   { $1                                                                    }
*)
msg:
    | value_info                                    { $1                                                                    }
     
value_info:
    | (* empty *)                                   { fun ctx -> EpFalse,()                                                 }
    | WITH tm                                       { $2                                                                    }
     
lexpr:                                              (* expr '[' expr ']' *) 
    | tm LSQBR tm RSQBR                             { fun ctx -> LEpArray{arrId=$1 ctx; arrIndex=$3 ctx}                    }
     
