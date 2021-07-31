
(* The type of tokens. *)

type token = 
  | VALUE
  | UNIT
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
  | PLUS
  | NOW
  | NOT
  | NEW
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
  | ID of (string)
  | GT
  | FALSE
  | EVENT
  | EUINT8 of (Big_int.big_int)
  | EUINT256 of (Big_int.big_int)
  | EQEQ
  | EQ
  | EOF
  | ELSE
  | DOT
  | DEFAULT
  | DARROW
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
