type token =
  | LOAD of (string  Support.withinfo)
  | SHOWCONTEXT of (Support.info)
  | TYPE of (Support.info)
  | REC of (Support.info)
  | FOLD of (Support.info)
  | UNFOLD of (Support.info)
  | TOP of (Support.info)
  | SOURCE of (Support.info)
  | SINK of (Support.info)
  | REF of (Support.info)
  | REFTYPE of (Support.info)
  | LIST of (Support.info)
  | TAIL of (Support.info)
  | HEAD of (Support.info)
  | ISNIL of (Support.info)
  | CONS of (Support.info)
  | NIL of (Support.info)
  | LETREC of (Support.info)
  | FIX of (Support.info)
  | STRING of (Support.info)
  | FLOAT of (Support.info)
  | TIMESFLOAT of (Support.info)
  | CASE of (Support.info)
  | OF of (Support.info)
  | TAG of (Support.info)
  | AS of (Support.info)
  | UNIT of (Support.info)
  | UNITTYPE of (Support.info)
  | WHERE of (Support.info)
  | IN of (Support.info)
  | LET of (Support.info)
  | BOOL of (Support.info)
  | NAT of (Support.info)
  | SUCC of (Support.info)
  | PRED of (Support.info)
  | ISZERO of (Support.info)
  | LAM of (Support.info)
  | IF of (Support.info)
  | THEN of (Support.info)
  | ELSE of (Support.info)
  | TRUE of (Support.info)
  | FALSE of (Support.info)
  | UCID of (string  Support.withinfo)
  | LCID of (string  Support.withinfo)
  | INTV of (int     Support.withinfo)
  | FLOATV of (float   Support.withinfo)
  | STRINGV of (string  Support.withinfo)
  | APOSTROPHE of (Support.info)
  | DQUOTE of (Support.info)
  | ARROW of (Support.info)
  | BANG of (Support.info)
  | BARGT of (Support.info)
  | BARRCURLY of (Support.info)
  | BARRSQUARE of (Support.info)
  | COLON of (Support.info)
  | COLONCOLON of (Support.info)
  | COLONEQ of (Support.info)
  | COLONHASH of (Support.info)
  | COMMA of (Support.info)
  | DARROW of (Support.info)
  | DDARROW of (Support.info)
  | DOT of (Support.info)
  | EOF of (Support.info)
  | EQ of (Support.info)
  | EQEQ of (Support.info)
  | EXISTS of (Support.info)
  | GT of (Support.info)
  | HASH of (Support.info)
  | LCURLY of (Support.info)
  | LCURLYBAR of (Support.info)
  | LEFTARROW of (Support.info)
  | LPAREN of (Support.info)
  | LSQUARE of (Support.info)
  | LSQUAREBAR of (Support.info)
  | LT of (Support.info)
  | RCURLY of (Support.info)
  | RPAREN of (Support.info)
  | RSQUARE of (Support.info)
  | SEMI of (Support.info)
  | SLASH of (Support.info)
  | STAR of (Support.info)
  | TRIANGLE of (Support.info)
  | USCORE of (Support.info)
  | VBAR of (Support.info)
  | NEWLINE of (Support.info)
  | DOUBLESEMI of (Support.info)

val toplevel :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.context -> (Syntax.command list * Syntax.context)
val input :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.context -> Eval.store -> Syntax.uvargenerator -> Syntax.constr -> (Syntax.command list * Syntax.context * Eval.store * Syntax.uvargenerator * Syntax.constr)
