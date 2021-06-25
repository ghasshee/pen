
%{
    open Syntax 
%}


%token CONTRACT
%token <string> IDENT
%token <Big_int.big_int> DECLIT256
%token <Big_int.big_int> DECLIT8
%token ADDRESS
%token UINT256 UINT8 BYTES32
%token BOOL TRUE FALSE
%token LPAR RPAR 
%token PLUS MINUS MULT 
%token RARROW
%token COMMA DOT 
%token LSQBR RSQBR
%token LBRACE RBRACE 
%token METHOD DEFAULT
%token IF THEN ELSE 
%token RETURN
%token BECOME
%token SEMI
%token LT GT NOT NEQ EQ EQEQ
%token EVENT LOG 
%token ALONG
%token ABORT
%token DEPLOY
%token SELFDESTRUCT
%token REENTRANCE
%token VALUE
%token SENDER MSG
%token THIS NOW VOID
%token LAND
%token BLOCK
%token INDEXED
%token BALANCE
%token EOF

%right RARROW

%left LAND
%left NEQ EQEQ LT GT
%left PLUS MINUS
%left MULT

%start <unit Syntax.toplevel list> file 
%%


%inline plist(X):
   xs = delimited(LPAR, separated_list(COMMA, X), RPAR) {xs}

file:
    | cs = list(cntrct);EOF;  { cs }
    ;

cntrct:
    | CONTRACT IDENT; plist(arg); LBRACE; list(case); RBRACE;
                                        { Cntrct { mthds       = $5
                                                   ; cntrct_name = $2
                                                   ; cntrct_args = $3 } }
    | EVENT IDENT plist(evnt_arg) SEMI { Event    { evnt_args = $3; evnt_name = $2 } };

case:
    | mthd_head block         {   { mthd_head    = $1
                                  ; mthd_body    = $2 }   }
    ;

block:
    | LBRACE; scs = list(stmt);RBRACE { scs }
    ;

mthd_head:
    | DEFAULT                               { Default }
    | METHOD LPAR ty IDENT plist(arg) RPAR  { Method { mthd_retTy = $3;     mthd_name = $4; mthd_args = $5 } }
    | METHOD LPAR VOID IDENT plist(arg)RPAR { Method { mthd_retTy = TyUnit; mthd_name = $4; mthd_args = $5 } };

arg:
    | ty IDENT                                 { { ty = $1; id = $2; loc = None } }
    

evnt_arg:
  | arg                                         { evnt_arg_of_arg $1 false }
  | ty INDEXED IDENT                           { { arg =    { ty  = $1 
                                                                        ; id  = $3 
                                                                        ; loc = None }; 
                                                                        indexed = true } }

ty:
  | UINT256                                     { TyUint256             }
  | UINT8                                       { TyUint8               }
  | BYTES32                                     { TyBytes32             }
  | ADDRESS                                     { TyAddr                }
  | BOOL                                        { TyBool                }
  | ty RARROW ty                                { TyMap($1,$3)          }
  | IDENT                                       { TyInstnce $1 }
  ;
%inline body:
  | stmt                                        { [$1] }
  | block                                       { $1   }
  ;
stmt:
  | ABORT SEMI                                  { SmAbort }
  | RETURN;option(expr);THEN;BECOME;expr;SEMI   { SmReturn{ret_expr=$2; ret_cont=$5} }     
  | lexpr EQ expr SEMI                          { SmAssign($1,$3) }
  | ty IDENT EQ expr SEMI                       { SmVarDecl{ varDecl_ty=$1; varDecl_id=$2; varDecl_val=$4 } }
  | VOID EQ expr SEMI                           { SmExpr $3 }
  | IF expr THEN body ELSE  body                { SmIf($2,$4,$6) }
  | IF expr THEN body                           { SmIfThen ($2, $4) }
  | LOG IDENT expr_list SEMI                    { SmLog($2,$3,None)}
  | SELFDESTRUCT expr SEMI                      { SmSelfDestruct $2 }
  ;

%inline op:
  | PLUS                                        { fun (l,r) -> EpPlus(l, r)}
  | MINUS                                       { fun (l,r) -> EpMinus(l, r)}
  | MULT                                        { fun (l,r) -> EpMult(l, r)}
  | LT                                          { fun (l,r) -> EpLt(l, r)}
  | GT                                          { fun (l,r) -> EpGt(l, r)}
  | EQEQ                                        { fun (l,r) -> EpEq(l, r)}
  | NEQ                                         { fun (l,r) -> EpNeq(l, r)}
  ;

expr:
  | expr LAND expr                              { EpLand($1,$3), () }
  | TRUE                                        { EpTrue, () }
  | FALSE                                       { EpFalse, () }
  | DECLIT256                                   { EpDecLit256 $1, ()}
  | DECLIT8                                     { EpDecLit8 $1, ()}
  | VALUE LPAR MSG RPAR                         { EpValue, () }
  | SENDER LPAR MSG RPAR                        { EpSender, () }
  | BALANCE; LPAR; expr; RPAR                   { EpBalance $3, () }
  | NOW LPAR BLOCK RPAR                         { EpNow, () }
  | expr op expr                                { ($2($1,$3)), () }
  | IDENT                                       { EpIdent $1, () }
  | LPAR;expr;RPAR                              { EpParen $2, () }
  | IDENT; expr_list                            { EpFnCall {call_head=$1; call_args=$2 }, () }
  | DEPLOY;IDENT;expr_list;msg             { EpNew{new_head=$2; new_args=$3; new_msg=$4},() }
  | expr;DOT;DEFAULT;LPAR;RPAR;msg         { EpSend{send_cntrct=$1; send_mthd=None   ; send_args=[]; send_msg=$6},() }
  | expr;DOT;IDENT  ;expr_list;msg         { EpSend{send_cntrct=$1; send_mthd=Some $3; send_args=$4; send_msg=$5},() }
  | ADDRESS;LPAR;expr;RPAR                      { EpAddr $3, () }
  | NOT; expr                                   { EpNot $2, () }
  | THIS                                        { EpThis, () }
  | lexpr;                                      { EpArray $1, () }
  ;
%inline expr_list:
  | plist(expr)                                 { $1 }
msg:
  | value_info; reentrance_info                 { {msg_value=$1; msg_reentrance=$2} }
  ;
value_info:
  | (* empty *)                                 { None }
  | ALONG; expr;                                { Some $2 }
  ;
reentrance_info:
  | REENTRANCE; block                           { $2 }
  ;
  lexpr:                                        (* expr '[' expr ']' *) 
  | expr;LSQBR;expr;RSQBR                       { LEpArray{array_name=$1; array_index=$3} }
  ;
