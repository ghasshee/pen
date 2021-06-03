
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
    | EVENT IDENT plist(event_arg) SEMI { Event    { event_args = $3; event_name = $2 } };

case:
    | mthd_head block         {   { mthd_head    = $1
                                  ; mthd_body    = $2 }   }
    ;

block:
    | LBRACE; scs = list(stmt);RBRACE { scs }
    ;

mthd_head:
    | DEFAULT                               { Default }
    | METHOD LPAR ty IDENT plist(arg) RPAR { Method { mthd_ret_ty = [$3];  mthd_name = $4; mthd_args = $5 } }
    | METHOD LPAR VOID IDENT plist(arg)RPAR { Method { mthd_ret_ty = [];    mthd_name = $4; mthd_args = $5 } };

arg:
    | ty IDENT                                 { { ty = $1; id = $2; loc = None } }
    

event_arg:
  | arg                                         { event_arg_of_arg $1 false }
  | ty INDEXED IDENT                           { { event_arg_body =    { ty  = $1 
                                                                        ; id  = $3 
                                                                        ; loc = None }; 
                                                                        event_arg_indexed = true } }

ty:
  | UINT256                                     { TyUint256             }
  | UINT8                                       { TyUint8               }
  | BYTES32                                     { TyBytes32             }
  | ADDRESS                                     { TyAddr                }
  | BOOL                                        { TyBool                }
  | ty RARROW ty                                { TyMap($1,$3)          }
  | IDENT                                       { TyCntrctInstance $1 }
  ;
%inline body:
  | stmt                                        { [$1] }
  | block                                       { $1   }
  ;
stmt:
  | ABORT SEMI                                  { AbortStmt }
  | RETURN;option(expr);THEN;BECOME;expr;SEMI   { ReturnStmt{ret_expr=$2; ret_cont=$5} }     
  | lexpr EQ expr SEMI                          { AssignStmt($1,$3) }
  | ty IDENT EQ expr SEMI                       { VarDeclStmt{ varDecl_ty=$1; varDecl_id=$2; varDecl_val=$4 } }
  | VOID EQ expr SEMI                           { ExprStmt $3 }
  | IF expr THEN body ELSE  body                { IfThenElse($2,$4,$6) }
  | IF expr THEN body                           { IfThenOnly ($2, $4) }
  | LOG IDENT expr_list SEMI                    { LogStmt($2,$3,None)}
  | SELFDESTRUCT expr SEMI                      { SelfDestructStmt $2 }
  ;

%inline op:
  | PLUS                                        { fun (l,r) -> PlusExpr(l, r)}
  | MINUS                                       { fun (l,r) -> MinusExpr(l, r)}
  | MULT                                        { fun (l,r) -> MultExpr(l, r)}
  | LT                                          { fun (l,r) -> LtExpr(l, r)}
  | GT                                          { fun (l,r) -> GtExpr(l, r)}
  | EQEQ                                        { fun (l,r) -> EqExpr(l, r)}
  | NEQ                                         { fun (l,r) -> NeqExpr(l, r)}
  ;

expr:
  | expr LAND expr                              { LandExpr($1,$3), () }
  | TRUE                                        { TrueExpr, () }
  | FALSE                                       { FalseExpr, () }
  | DECLIT256                                   { DecLit256Expr $1, ()}
  | DECLIT8                                     { DecLit8Expr $1, ()}
  | VALUE LPAR MSG RPAR                         { ValueExpr, () }
  | SENDER LPAR MSG RPAR                        { SenderExpr, () }
  | BALANCE; LPAR; expr; RPAR                   { BalanceExpr $3, () }
  | NOW LPAR BLOCK RPAR                         { NowExpr, () }
  | expr op expr                                { ($2($1,$3)), () }
  | IDENT                                       { IdentExpr $1, () }
  | LPAR;expr;RPAR                              { ParenExpr $2, () }
  | IDENT; expr_list                            { FunCallExpr {call_head=$1; call_args=$2 }, () }
  | DEPLOY;IDENT;expr_list;msg_info             { NewExpr{new_head=$2; new_args=$3; new_msg_info=$4},() }
  | expr;DOT;DEFAULT;LPAR;RPAR;msg_info         { SendExpr{send_cntrct=$1; send_mthd=None   ; send_args=[]; send_msg_info=$6},() }
  | expr;DOT;IDENT;expr_list;msg_info           { SendExpr{send_cntrct=$1; send_mthd=Some $3; send_args=$4; send_msg_info=$5},() }
  | ADDRESS;LPAR;expr;RPAR                      { AddrExpr $3, () }
  | NOT; expr                                   { NotExpr $2, () }
  | THIS                                        { ThisExpr, () }
  | lexpr;                                      { ArrayExpr $1, () }
  ;
%inline expr_list:
  | plist(expr)                                 { $1 }
msg_info:
  | value_info; reentrance_info                 { {msg_value_info=$1; msg_reentrance_info=$2} }
  ;
value_info:
  | (* empty *)                                 { None }
  | ALONG; expr;                                { Some $2 }
  ;
reentrance_info:
  | REENTRANCE; block                           { $2 }
  ;
  lexpr:                                        (* expr '[' expr ']' *) 
  | expr;LSQBR;expr;RSQBR                       { ArrayLExpr{array_name=$1; array_index=$3} }
  ;
