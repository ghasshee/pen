
(* The type of tokens. *)

type token = 
  | WITH
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
  | REC
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
  | LET
  | LBRACE
  | LARROW
  | LAND
  | LAM
  | INDEXED
  | IN
  | IF
  | ID of (string)
  | GT
  | FIX
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
  | COLON
  | BYTES32
  | BOOL
  | BLOCK
  | BECOME
  | BALANCE
  | ARROW
  | ADDRESS
  | ABORT

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val file: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (unit Syntax.toplevel list)
