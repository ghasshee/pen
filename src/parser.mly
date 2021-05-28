
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
                                { Contract { mthds              = $5
                                           ; cntrct_name      = $2
                                           ; cntrct_args      = $3 } }
    | EVENT IDENT plist(event_arg) SEMI;
                                { Event    { event_args    = $3
                                           ; event_name         = $2 } };

case:
    | mthd_head block         {   { mthd_head    = $1
                                  ; mthd_body    = $2 }   }
    ;

block:
    | LBRACE; scs = list(stmt);RBRACE { scs }
    ;

mthd_head:
    | DEFAULT                               { Syntax.Default }
    | METHOD LPAR typ IDENT plist(arg) RPAR { Syntax.Method { mthd_ret_ty = [$3]; Syntax.mthd_name = $4; mthd_args = $5 } }
    | METHOD LPAR VOID IDENT plist(arg)RPAR { Syntax.Method { mthd_ret_ty = []; Syntax.mthd_name = $4; mthd_args = $5 } };

arg:
    | typ IDENT                {   { ty = $1; id = $2; loc = None    }   }
    

event_arg:
  | arg                             { Syntax.event_arg_of_arg $1 false }
  | typ INDEXED IDENT               { { Syntax.event_arg_body =
        { ty        = $1 
        ; id      = $3 
        ; loc   = None 
        }; 
        Syntax.event_arg_indexed = true }
    }

typ:
  | UINT256                                 { TyUint256             }
  | UINT8                                   { TyUint8               }
  | BYTES32                                 { TyBytes32             }
  | ADDRESS                                 { TyAddr                }
  | BOOL                                    { TyBool                }
  | typ RARROW typ                          { TyMap($1,$3)          }
  | IDENT                                   { TyContractInstance $1 }
  ;
%inline body:
  | stmt                                { [$1] }
  | block                                   { $1   }
  ;
stmt:
  | ABORT;SEMI                              { Syntax.AbortStmt }
  | RETURN;option(expr);THEN;BECOME;expr;SEMI { Syntax.ReturnStmt{Syntax.ret_expr=$2; ret_cont=$5} }     
  | lexpr; EQ; expr; SEMI                     { Syntax.AssignStmt ($1,$3) }
  | typ;IDENT;EQ;expr;SEMI                   { Syntax.VarDeclStmt{ Syntax.var_declare_type=$1; var_declare_name=$2; var_declare_value=$4 } }
  | VOID; EQ;expr; SEMI                      { Syntax.ExprStmt $3 }
  | IF; expr; THEN; body; ELSE; body         { Syntax.IfThenElse($2,$4,$6) }
  | IF; expr; THEN; body                     { Syntax.IfThenOnly ($2, $4) }
  | LOG;IDENT;expr_list; SEMI                { Syntax.LogStmt($2,$3,None)}
  | SELFDESTRUCT; expr; SEMI                 { Syntax.SelfDestructStmt $2 }
  ;

%inline op:
  | PLUS                                    { fun (l,r) -> Syntax.PlusExpr(l, r)}
  | MINUS                                   { fun (l,r) -> Syntax.MinusExpr(l, r)}
  | MULT                                    { fun (l,r) -> Syntax.MultExpr(l, r)}
  | LT                                      { fun (l,r) -> Syntax.LtExpr(l, r)}
  | GT                                      { fun (l,r) -> Syntax.GtExpr(l, r)}
  | NEQ                                     { fun (l,r) -> Syntax.NeqExpr(l, r)}
  | EQEQ                                    { fun (l,r) -> Syntax.EqExpr(l, r)}
  ;

expr:
  | lhs = expr; LAND; rhs = expr              { Syntax.LandExpr (lhs, rhs), () }
  | TRUE                                    { Syntax.TrueExpr, () }
  | FALSE                                   { Syntax.FalseExpr, () }
  | d = DECLIT256                           { Syntax.DecLit256Expr d, ()}
  | d = DECLIT8                             { Syntax.DecLit8Expr d, ()}
  | VALUE LPAR MSG RPAR                     { Syntax.ValueExpr, () }
  | SENDER LPAR MSG RPAR                    { Syntax.SenderExpr, () }
  | BALANCE; LPAR; expr; RPAR                { Syntax.BalanceExpr $3, () }
  | NOW LPAR BLOCK RPAR                     { Syntax.NowExpr, () }
  | expr;op;expr                              { ($2 ($1, $3)), () }
  | IDENT                                   { Syntax.IdentExpr $1, () }
  | LPAR;expr;RPAR                           { Syntax.ParenthExpr $2, () }
  | IDENT; expr_list                         { Syntax.FunCallExpr {Syntax.call_head=$1; call_args=$2 }, () }
  | DEPLOY;IDENT;expr_list;msg_info          { Syntax.NewExpr{Syntax.new_head=$2; new_args=$3; new_msg_info=$4},() }
  | expr;DOT;DEFAULT;LPAR;RPAR;msg_info      { Syntax.SendExpr{Syntax.send_head_cntrct=$1; send_head_method=None; send_args=[]; send_msg_info=$6},() }
  | expr;DOT;IDENT;expr_list;msg_info         { Syntax.SendExpr{Syntax.send_head_cntrct=$1; send_head_method=Some $3; send_args=$4; send_msg_info=$5},() }
  | ADDRESS;LPAR;expr;RPAR                   { Syntax.AddrExpr $3, () }
  | NOT; expr                                { Syntax.NotExpr $2, () }
  | THIS                                    { Syntax.ThisExpr, () }
  | l = lexpr;                               { Syntax.ArrayAccessExpr l, () }
  ;
%inline expr_list:
  | plist(expr)                              {$1}
msg_info:
  | value_info; reentrance_info             { {Syntax.msg_value_info=$1; msg_reentrance_info=$2} }
  ;
value_info:
  | (* empty *)                             { None }
  | ALONG; expr;                             { Some $2 }
  ;
reentrance_info:
  | REENTRANCE; block                       { $2 }
  ;
lexpr:
  | expr;LSQBR;expr;RSQBR                     { Syntax.ArrayAccessLExpr{Syntax.array_access_array=$1; array_access_index=$3} }
  ;
