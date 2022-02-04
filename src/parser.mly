%{
    open Misc
    open Syntax 
    open Context
    module BS = BatString
    
let reserved x                  =   if BS.starts_with x "pre_" then err "Names 'pre_..' are reserved." 
%}

%token CONTRACT
%token <int> RIG
%token <string> ID
%token <Big_int.big_int> U8 NUM  
%token LET IN REC FIX 
%token LAM ARROW LARROW
%token BANG REF COLONEQ 
%token ADDRESS UINT256 UINT8 BYTES32
%token BOOL TRUE FALSE LT GT NOT NEQ EQ EQEQ LAND
%token PLUS MINUS MULT 
%token DARROW
%token COMMA DOT SEMI COLON
%token LPAR RPAR    LSQBR RSQBR     LBRACE RBRACE 
%token METHOD DEFAULT
%token IF THEN ELSE 
%token WITH
%token RETURN BECOME
%token ABORT 
%token NEW SELFDESTRUCT CALL 
%token SENDER MSG VALUE BALANCE
%token THIS NOW UNIT
%token EVENT LOG 
%token BLOCK
%token INDEXED
%token EOF
%token ISZERO ECDSARECOVER KECCAK

%right DARROW

%left LAND
%left NEQ EQEQ LT GT
%left PLUS MINUS
%left MULT

%start <unit Syntax.toplevel list> file 
%%

%inline plist(X):
    delimited(LPAR,separated_list(COMMA,X),RPAR)    { $1                                                                                        }

file:
    | list(cntrct) EOF                              { $1                                                                                        }

cntrct:
    | CONTRACT ID plist(arg)LBRACE list(mthd)RBRACE { reserved $2; TmCn($2,$3,$5)                                                               }
    | EVENT    ID plist(evnt_arg) SEMI              { TmEv(TyEv($2,$3))                                                                         }

mthd:
    | mthd_head LBRACE tm RBRACE                    { TmMthd($1,$3 [])                                                                          }

mthd_head:
    | DEFAULT                                       { TyDflt                                                                                 }
    | METHOD ty ID plist(arg)                       { TyMthd($3,$4,$2)                                                                          }
    | METHOD LPAR RPAR ID plist(arg)                { TyMthd($4,$5,TyUnit   )                                                                   }

arg:
    | ty ID                                         { reserved $2; TyVar($2,$1)                                                                 }

evnt_arg:
    | arg                                           { let TyVar(id,ty)=$1 in TyEvVar(id,ty,false)                                               }
    | INDEXED ty ID                                 { TyEvVar($3,$2,true)                                                                       }

ty:
    | UINT256                                       { TyU256                                                                                    }
    | UINT8                                         { TyU8                                                                                      }
    | BYTES32                                       { TyBytes32                                                                                 }
    | ADDRESS                                       { TyAddr                                                                                    }
    | BOOL                                          { TyBool                                                                                    }
    | ty DARROW ty                                  { TyMap($1,$3)                                                                              }
    | ty ARROW ty                                   { TyAbs($1,$3)                                                                              } 
    | ID                                            { TyIstc $1                                                                               }


ret: 
    |                                               { fun ctx ->    TmUnit                                                                  ,() }
    | call                                          { $1                                                                                        }
    | tm                                            { $1                                                                                        }

%inline op:
    | LT                                            { fun l r ->    TmLT(l,r)                                                                   }
    | GT                                            { fun l r ->    TmGT(l,r)                                                                   }
    | EQEQ                                          { fun l r ->    TmEQ(l,r)                                                                   }
    | NEQ                                           { fun l r ->    TmNEQ(l,r)                                                                  }
    | LAND                                          { fun l r ->    TmLAND(l,r)                                                                 } 
    | PLUS                                          { fun l r ->    TmAdd(l,r)                                                                  }
    | MULT                                          { fun l r ->    TmMul(l,r)                                                                  }
    | MINUS                                         { fun l r ->    TmSub(l,r)                                                                  }

tm: 
    | appTm                                         { $1                                                                                        } 
    (*| ty ID EQ tm SEMI             { reserved $2; fun ctx -> SmDecl($1,$2,$4 ctx)         ,ctx                                                }*)
    | LET ty ID EQ tm IN tm                         { fun ctx ->    TmApp((TmAbs($3,$2,$7(add_bruijn_idx ctx $3)),()),$5 ctx)               ,() } 
    | LET ty ID EQ tm SEMI tm                       { fun ctx ->    TmApp((TmAbs($3,$2,$7(add_bruijn_idx ctx $3)),()),$5 ctx)               ,() } 
    | LET UNIT EQ tm IN tm                          { fun ctx ->    TmApp((TmAbs("_",TyUnit,$6(add_bruijn_idx ctx "_")),()),$4 ctx)         ,() } 
    | LET UNIT EQ tm SEMI tm                        { fun ctx ->    TmApp((TmAbs("_",TyUnit,$6(add_bruijn_idx ctx "_")),()),$4 ctx)         ,() } 
    | LET REC ID ID COLON ty EQ tm IN tm            { fun ctx ->    let ctx' = add_rec_idx ctx ($3^"'")                         in 
                                                                    let ctx''= add_struct_idx ctx' $4                           in 
                                                                    let ctx  = add_bruijn_idx ctx  $3                           in 
                                                                    TmApp((TmAbs($3,$6,$10 ctx),()),(TmFix(($3^"'"),$4,$6,$8 ctx''),()))    ,() }  
    | LET REC ID ID COLON ty EQ tm SEMI tm          { fun ctx ->    let ctx' = add_rec_idx ctx ($3^"'")                         in 
                                                                    let ctx''= add_struct_idx ctx' $4                           in 
                                                                    let ctx  = add_bruijn_idx ctx  $3                           in 
                                                                    TmApp((TmAbs($3,$6,$10 ctx),()),(TmFix(($3^"'"),$4,$6,$8 ctx''),()))    ,() }  
    | LAM ID COLON ty ARROW tm                      { fun ctx ->    TmAbs($2, $4, $6(add_bruijn_idx ctx $2))                                ,() } 
    | IF tm THEN tm ELSE tm                         { fun ctx ->    TmIf($2 ctx, $4 ctx, $6 ctx)                                            ,() }
    | IF tm THEN tm SEMI tm                         { fun ctx ->    TmIf($2 ctx, $4 ctx, $6 ctx)                                            ,() }
    | tm COLONEQ tm                                 { fun ctx ->    TmAssign($1 ctx, $3 ctx)                                                ,() }
    | LOG ID  arg_list                              { fun ctx ->    TmLog($2,$3 ctx,None)                                                   ,() }
    | SELFDESTRUCT tm                               { fun ctx ->    TmSfDstr($2 ctx)                                                     ,() }
    | tm DOT DEFAULT LPAR RPAR msg                  { fun ctx ->    TmSend($1 ctx,None,[],$6 ctx)                                           ,() }
    | tm DOT ID       arg_list msg                  { fun ctx ->    TmSend($1 ctx,Some $3,$4 ctx,$5 ctx)                                    ,() }
    | tm LSQBR tm RSQBR                             { fun ctx ->    TmArr($1 ctx,$3 ctx)                                                  ,() }
    | tm op tm                                      { fun ctx ->    $2 ($1 ctx)($3 ctx)                                                     ,() }
appTm:
    | pathTm                                        { $1                                                                                        }
    | NOT   pathTm                                  { fun ctx ->    TmNOT ($2 ctx)                                                          ,() }
    | ISZERO arg_list                               { fun ctx ->    TmCall("iszero"          ,$2 ctx)                                       ,() }
    | ECDSARECOVER arg_list                         { fun ctx ->    TmCall("pre_ecdsarecover",$2 ctx)                                       ,() }
    | KECCAK arg_list                               { fun ctx ->    TmCall("keccak256"       ,$2 ctx)                                       ,() }
    | appTm pathTm                                  { fun ctx ->    TmApp($1 ctx,$2 ctx)                                                    ,() } 
pathTm:
    | aTm                                           { $1                                                                                        }
aTm:
    | LPAR tm RPAR                                  { $2                                                                                        }
    | RETURN ret THEN BECOME call                   { fun ctx ->    TmReturn($2 ctx,$5 ctx)                                                 ,() }
    | ABORT                                         { fun ctx ->    TmAbort                                                                 ,() } 
    | TRUE                                          { fun ctx ->    TmTrue                                                                  ,() }
    | FALSE                                         { fun ctx ->    TmFalse                                                                 ,() }
    | NUM                                           { fun ctx ->    TmU256 $1                                                               ,() }
    | U8                                            { fun ctx ->    TmU256 $1                                                               ,() }
    | UNIT                                          { fun ctx ->    TmUnit                                                                  ,() }
    | VALUE   LPAR  MSG  RPAR                       { fun ctx ->    EpValue                                                                 ,() }
    | SENDER  LPAR  MSG  RPAR                       { fun ctx ->    TmSender                                                                ,() }
    | BALANCE LPAR  tm   RPAR                       { fun ctx ->    Balanc ($3 ctx)                                                         ,() }
    | NOW     LPAR BLOCK RPAR                       { fun ctx ->    EpNow                                                                   ,() }
    | THIS                                          { fun ctx ->    TmThis                                                                  ,() }
    | ADDRESS LPAR  tm   RPAR                       { fun ctx ->    TmAddr ($3 ctx)                                                         ,() }
    | ID                               { reserved $1; fun ctx ->    (* #DEBUG prBds ctx;pe $1 ; *)
                                                (           try     TmI(lookup_bruijn_idx $1 ctx,len ctx)                                 ,()  
                                                with _ ->   try     TmIRec(lookup_rec_idx ($1^"'") ctx)                                   ,() 
                                                with _ ->   try     TmIStrct(lookup_struct_idx $1 ctx)                                    ,() 
                                                with _ ->           TmId $1                                                                 ,())}
    | NEW ID  arg_list msg             { reserved $2; fun ctx ->    TmNew($2,$3 ctx,$4 ctx)                                                 ,() }
call: 
    | ID arg_list                                   { fun ctx ->    TmCall($1,$2 ctx)                                                       ,() }
arg_list : 
    | LPAR RPAR                                     { fun ctx ->    []                                                                          }
    | LPAR args RPAR                                { fun ctx ->    $2 ctx                                                                      } 
args: 
    | tm                                            { fun ctx ->    [$1 ctx]                                                                    }
    | tm COMMA args                                 { fun ctx ->    $1 ctx :: $3 ctx                                                            } 

msg:
    | value_info                                    { $1                                                                                        }
     
value_info:
    | (* empty *)                                   { fun ctx ->    TmZero, ()                                                                  }
    | WITH tm                                       { $2                                                                                        }
     
