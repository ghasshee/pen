
(* The type of tokens. *)

type token = 
  | WITH
  | VALUE
  | UNIT
  | UINT8
  | UINT256
  | U8 of (Big_int.big_int)
  | TRUE
  | THIS
  | THEN
  | SENDER
  | SEMI
  | SELFDESTRUCT
  | RSQBR
  | RPAR
  | RIG of (int)
  | RETURN
  | REF
  | REC
  | RBRACE
  | PLUS
  | NUM of (Big_int.big_int)
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
  | KECCAK
  | ISZERO
  | INDEXED
  | IN
  | IF
  | ID of (string)
  | GT
  | FIX
  | FALSE
  | EVENT
  | EQEQ
  | EQ
  | EOF
  | ELSE
  | ECDSARECOVER
  | DOT
  | DEFAULT
  | DARROW
  | CONTRACT
  | COMMA
  | COLONEQ
  | COLON
  | CALL
  | BYTES32
  | BOOL
  | BLOCK
  | BECOME
  | BANG
  | BALANCE
  | ARROW
  | ADDRESS
  | ABORT

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val file: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (unit Syntax.toplevel list)
