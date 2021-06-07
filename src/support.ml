open Lexer 
open Lexing
open Printf 
(* The following two functions comes from
 * https://github.com/realworldocaml/examples/tree/master/code/parsing-test
 * which is under UNLICENSE *)

let pr_pos outx lexbuf =
    let pos = lexbuf.lex_curr_p in
    fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf = 
    try Parser.file Lexer.read lexbuf with
    | SyntaxError e -> fprintf stderr "%a: %s\n"           pr_pos lexbuf e;exit (-1)
    | Parser.Error  -> fprintf stderr "%a: syntax error\n" pr_pos lexbuf  ;exit (-1)
    | _             -> fprintf stderr "%a: syntax error\n" pr_pos lexbuf  ;exit (-1)
