
(* The type of tokens. *)

type token = 
  | VOID
  | VALUE
  | UINT8
  | UINT256
  | TRUE
  | THIS
  | THEN
  | SENDER
  | SEMI
  | SELFDESTRUCT
  | RSQBR
  | RPAR
  | RETURN
  | REENTRANCE
  | RBRACE
  | RARROW
  | PLUS
  | NOW
  | NOT
  | NEQ
  | MULT
  | MSG
  | MINUS
  | METHOD
  | LT
  | LSQBR
  | LPAR
  | LOG
  | LBRACE
  | LAND
  | INDEXED
  | IF
  | IDENT of (string)
  | GT
  | FALSE
  | EVENT
  | EQEQ
  | EQ
  | EOF
  | ELSE
  | DOT
  | DEPLOY
  | DEFAULT
  | DECLIT8 of (Big_int.big_int)
  | DECLIT256 of (Big_int.big_int)
  | CONTRACT
  | COMMA
  | BYTES32
  | BOOL
  | BLOCK
  | BECOME
  | BALANCE
  | ALONG
  | ADDRESS
  | ABORT

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val file: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (unit Syntax.toplevel list)
