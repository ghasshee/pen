/*  Yacc grammar for the parser. */

%{
open Format
open Support
open Syntax
open Type
open Eval

let pe = print_endline 
%}


/* REPL Methods */ 
%token <string  Support.withinfo> LOAD
%token <Support.info> SHOWCONTEXT


/* Keyword tokens */
%token <Support.info> TOP 

%token <Support.info> REF 
%token <Support.info> REFTYPE

%token <Support.info> LIST
%token <Support.info> TAIL
%token <Support.info> HEAD
%token <Support.info> ISNIL
%token <Support.info> CONS
%token <Support.info> NIL

%token <Support.info> LETREC 
%token <Support.info> FIX 

%token <Support.info> STRING
%token <Support.info> FLOAT
%token <Support.info> TIMESFLOAT

%token <Support.info> CASE
%token <Support.info> OF
%token <Support.info> TAG

%token <Support.info> AS

%token <Support.info> UNIT
%token <Support.info> UNITTYPE

%token <Support.info> WHERE
%token <Support.info> IN
%token <Support.info> LET

%token <Support.info> BOOL
%token <Support.info> NAT

%token <Support.info> SUCC
%token <Support.info> PRED
%token <Support.info> ISZERO

%token <Support.info> LAMBDA
%token <Support.info> IF
%token <Support.info> THEN
%token <Support.info> ELSE
%token <Support.info> TRUE
%token <Support.info> FALSE

/* Identifier and constant value tokens */
%token <string  Support.withinfo> UCID  /* uppercase-initial */
%token <string  Support.withinfo> LCID  /* lowercase/symbolic-initial */
%token <int     Support.withinfo> INTV
%token <float   Support.withinfo> FLOATV
%token <string  Support.withinfo> STRINGV

/* Symbolic tokens */
%token <Support.info> APOSTROPHE
%token <Support.info> DQUOTE
%token <Support.info> ARROW
%token <Support.info> BANG
%token <Support.info> BARGT
%token <Support.info> BARRCURLY
%token <Support.info> BARRSQUARE
%token <Support.info> COLON
%token <Support.info> COLONCOLON
%token <Support.info> COLONEQ
%token <Support.info> COLONHASH
%token <Support.info> COMMA
%token <Support.info> DARROW
%token <Support.info> DDARROW
%token <Support.info> DOT
%token <Support.info> EOF
%token <Support.info> EQ
%token <Support.info> EQEQ
%token <Support.info> EXISTS
%token <Support.info> GT
%token <Support.info> HASH
%token <Support.info> LCURLY
%token <Support.info> LCURLYBAR
%token <Support.info> LEFTARROW
%token <Support.info> LPAREN
%token <Support.info> LSQUARE
%token <Support.info> LSQUAREBAR
%token <Support.info> LT
%token <Support.info> RCURLY
%token <Support.info> RPAREN
%token <Support.info> RSQUARE
%token <Support.info> SEMI        /* semicolon */ 
%token <Support.info> SLASH
%token <Support.info> STAR
%token <Support.info> TRIANGLE
%token <Support.info> USCORE
%token <Support.info> VBAR
%token <Support.info> NEWLINE
%token <Support.info> DOUBLESEMI
/* The returned type of a toplevel is Syntax.command list. */
%start toplevel
%start input 
%type <Syntax.context -> (Syntax.command list * Syntax.context)> input 
%type <Syntax.context -> (Syntax.command list * Syntax.context)> toplevel

%%
/************   REPL   ***************************************************************************/

input :   /* Left Recursion */
    |                                   { fun _     ->  [],emptyctx                             }
    | input LOAD                        { let file   = $2.v in 
                                          fun ctx   ->  [],ctx                                  }  
    | input SHOWCONTEXT DOUBLESEMI      { let _,ctx'    = $1 []            in pr_ctx ctx';
                                          fun _    ->  [],ctx'                                  }  
    | input DOUBLESEMI                  { fun ctx   ->  [],ctx                                  } 
    | input oneREPL                     { let _,ev_ctx      = $1 []            in   
                                          let cmds,_      = $2 ev_ctx in 
                                          let ev_ctx'      = process_commands ev_ctx   cmds in 
                                          fun _    -> [],ev_ctx'     } 
oneREPL : 
    | Command DOUBLESEMI                { fun ctx ->  let cmd,ctx'    = $1 ctx in [cmd],ctx'  } 
    | Command SEMI oneREPL              { fun ctx ->  let cmd,ctx'    = $1 ctx in 
                                                      let cmds,ctx''  = $3 ctx' in cmd::cmds,ctx''  }


/************  COMPILER  *************************************************************************/

toplevel : /* Right Recursion */                
    | EOF                               { fun ctx   ->  [],ctx                                  } 
    | Command SEMI toplevel             { fun ctx   ->  let cmd,ctx  = $1 ctx in 
                                                        let cmds,ctx = $3 ctx in cmd::cmds,ctx  } 

/************   COMMAND  *************************************************************************/

Command     :   
    | TermWrap                          { fun ctx   ->  let t = $1 ctx in Eval(tmInfo t,t),ctx  }
    | UCID TyBinder                     { fun ctx   ->  Bind($1.i,$1.v,$2 ctx),addname ctx $1.v } 
    | LCID Binder                       { fun ctx   ->  Bind($1.i,$1.v,$2 ctx),addname ctx $1.v } 
TyBinder    :
    |                                   { fun ctx   ->  BindTyVar                               }
    | EQ Type                           { fun ctx   ->  BindTyAbb($2 ctx)                       } 
Binder      : 
    | COLON Type                        { fun ctx   ->  BindTmVar($2 ctx)                         } 
    | EQ Term                           { fun ctx   ->  BindTmAbb($2 ctx,None)                  } 

/************    TYPE    *************************************************************************/

Type        : 
    | ArrowType                         { $1                                                    } 
    | REFTYPE AType                     { fun ctx   ->  TyRef($2 ctx)                           } 
ArrowType   :
    | AType ARROW ArrowType             { fun ctx   ->  TyArr($1 ctx, $3 ctx)                   }
    | AType                             { $1                                                    } 
AType       : 
    | UCID                              { fun ctx   ->  if isnamebound ctx $1.v 
                                            then    TyVar(name2index $1.i ctx $1.v, ctxlen ctx) 
                                            else    TyId($1.v)                                  } 
    | LPAREN Type RPAREN                { $2                                                    } 
    | TOP                               { fun ctx   ->  TyTop                                   } 
    | FLOAT                             { fun ctx   ->  TyFloat                                 }
    | STRING                            { fun ctx   ->  TyString                                } 
    | BOOL                              { fun ctx   ->  TyBool                                  } 
    | NAT                               { fun ctx   ->  TyNat                                   }
    | UNITTYPE                          { fun ctx   ->  TyUnit                                  } 
    | LCURLY TyFields RCURLY            { fun ctx   ->  TyRecord($2 ctx 1)                      }
    | LT TyFields GT                    { fun ctx   ->  TyVariant($2 ctx 1)                     } 
    | LIST Type                         { fun ctx   ->  TyList($2 ctx)                          }
LType       :
    | LSQUARE Type RSQUARE              { fun ctx   ->  TyList($2 ctx)                          } 
TyFields    :
    | /* Empty Type */                  { fun ctx   ->  fun i -> []                             } 
    | NETyFields                        { $1                                                    }
NETyFields  :
    | TyField                           { fun ctx   ->  fun i -> [$1 ctx i]                     }
    | TyField COMMA NETyFields          { fun ctx   ->  fun i -> ($1 ctx i)::($3 ctx (i+1))     }
TyField     : 
    | LCID COLON Type                   { fun ctx   ->  fun i -> ($1.v, $3 ctx)                 } 
    | Type                              { fun ctx   ->  fun i -> (string_of_int i, $1 ctx)      } 
/************    TERM    *************************************************************************/
TermWrap    :
    | TermWrap COMMA LCID EQ Term       { fun ctx   ->  TmLet($2,$3.v,$5 ctx,$1(addname ctx $3.v)) }
    | Term     WHERE LCID EQ Term       { fun ctx   ->  TmLet($2,$3.v,$5 ctx,$1(addname ctx $3.v)) }
    | Term                              { $1                                                    } 
Cases       :
    | Case                              { fun ctx   ->  [$1 ctx]                                }
    | Case VBAR Cases                   { fun ctx   ->  ($1 ctx)::($3 ctx)                      }
Case        :
    | LT LCID EQ LCID GT DARROW AppTerm { fun ctx   ->  ($2.v,($4.v,$7(addname ctx $4.v)))      } 
Term        :
    | AppTerm                           { $1                                                    }
    | AppTerm COLONEQ AppTerm           { fun ctx   ->  TmAssign($2,$1 ctx,$3 ctx)              } 
    | CASE Term OF Cases                { fun ctx   ->  TmCase($1,$2 ctx,$4 ctx)                }
    | Term COLON Term                   { fun ctx   ->  TmLet($2, "_", $3 ctx, $1 ctx)          } 
    | LET LCID EQ Term IN Term          { fun ctx   ->  TmLet($1,$2.v,$4 ctx,$6(addname ctx $2.v))}
    | LET USCORE EQ Term IN Term        { fun ctx   ->  TmLet($1,"_",$4 ctx,$6(addname ctx"_")) }
    | LAMBDA LCID COLON Type DOT Term   { fun ctx   ->  TmAbs($1,$2.v,$4 ctx,$6(addname ctx $2.v))}
    | IF Term THEN Term ELSE Term       { fun ctx   ->  TmIf($1,$2 ctx,$4 ctx,$6 ctx)           }
    | LETREC LCID COLON Type EQ Term IN Term
                                        { fun ctx   ->  let ctx' = addname ctx $2.v in 
                                                        TmLet($1,$2.v,TmFix($1,TmAbs($1,$2.v,$4 ctx,$6 ctx')),$8 ctx')}
    | CONS LType ATerm ATerm            { fun ctx   ->  TmCons($1,$2 ctx,$3 ctx,$4 ctx)         } 
    | HEAD LType ATerm                  { fun ctx   ->  TmHead($1,$2 ctx,$3 ctx)                } 
    | TAIL LType ATerm                  { fun ctx   ->  TmTail($1,$2 ctx,$3 ctx)                } 
    | ISNIL LType ATerm                 { fun ctx   ->  TmIsNil($1,$2 ctx,$3 ctx)               } 
AppTerm     :
    | PathTerm                          { $1                                                    }
    | PathTerm TIMESFLOAT PathTerm      { fun ctx   ->  TmTimesfloat($2,$1 ctx,$3 ctx)          } 
    | FIX     PathTerm                  { fun ctx   ->  TmFix($1, $2 ctx )                      }
    | REF     PathTerm                  { fun ctx   ->  TmRef($1, $2 ctx )                      } 
    | BANG    PathTerm                  { fun ctx   ->  TmDeref($1, $2 ctx )                    } 
    | SUCC    PathTerm                  { fun ctx   ->  TmSucc($1, $2 ctx )                     }
    | PRED    PathTerm                  { fun ctx   ->  TmPred($1, $2 ctx )                     }
    | ISZERO  PathTerm                  { fun ctx   ->  TmIsZero($1, $2 ctx)                    }
    | AppTerm PathTerm                  { fun ctx   ->  let t=$1 ctx in TmApp(tmInfo t,t,$2 ctx)}
PathTerm    : 
    | PathTerm DOT LCID                 { fun ctx   ->  TmProj($2, $1 ctx, $3.v)                }
    | PathTerm DOT INTV                 { fun ctx   ->  TmProj($2, $1 ctx, soi $3.v)            }
    | AscribeTerm                       { $1                                                    } 
AscribeTerm : 
    | ATerm AS Type                     { fun ctx   ->  TmAscribe($2,$1 ctx,$3 ctx)             }
    | ATerm                             { $1                                                    }
ATerm       :                               /* Atomic terms never require extra parentheses */
    | LPAREN TermSeq RPAREN             { $2                                                    }
    | LCURLY Fields RCURLY              { fun ctx   ->  TmRecord($1,$2 ctx 1)                   }
    | LT LCID EQ Term GT AS Type        { fun ctx   ->  TmTag($1,$2.v,$4 ctx,$7 ctx)            } 
    | LCID                              { fun ctx   ->  TmVar($1.i,name2index $1.i ctx $1.v,ctxlen ctx) } 
    | STRINGV                           { fun ctx   ->  TmString($1.i,$1.v)                     }
    | FLOATV                            { fun ctx   ->  TmFloat($1.i,$1.v)                      }
    | UNIT                              { fun ctx   ->  TmUnit($1)                              }
    | TRUE                              { fun ctx   ->  TmTrue($1)                              }
    | FALSE                             { fun ctx   ->  TmFalse($1)                             }
    | INTV                              { fun ctx   ->  let rec f = function
                                                            | 0 -> TmZero($1.i)
                                                            | n -> TmSucc($1.i,f(n-1))in f $1.v }
    | NIL LType                         { fun ctx   ->  TmNil($1,$2 ctx)                        } 
TermSeq     : 
    | Term                              { $1                                                    } 
    | Term SEMI TermSeq                 { fun ctx   ->  TmApp($2,TmAbs($2,"_",TyUnit,$3(addname ctx"_")),$1 ctx) } 

Fields      : 
    | /* empty */                       { fun ctx   ->  fun i -> []                             }
    | NEFields                          { $1                                                    }
NEFields    : 
    | Field                             { fun ctx   -> fun i -> [ $1 ctx i ]                    }
    | Field COMMA NEFields              { fun ctx   -> fun i -> ($1 ctx i)::($3 ctx(i+1))       }
Field       : 
    | LCID EQ Term                      { fun ctx   -> fun i -> ($1.v, $3 ctx)                  }
    | Term                              { fun ctx   -> fun i -> (string_of_int i, $1 ctx)       }
