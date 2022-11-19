open Format
open Arg 

open Support
open Syntax
open Type
open Eval



let interpreter lexbuf  = Parser.input       Lexer.token lexbuf 
let compiler    lexbuf  = Parser.toplevel   Lexer.token lexbuf

let parse' machine in_channel =   (* machine -> in_channel -> command list *) 
    let lexbuf              =   Lexing.from_channel in_channel  in
    let result,ctx          =   try     machine lexbuf emptyctx 
                                with  | Parsing.Parse_error -> error(Lexer.info lexbuf)"Parse error" 
                                      | e -> raise e 
    in
    Parsing.clear_parser(); close_in in_channel; result

(* #####################
 * #### INTERPRETER ####
 * ##################### *)

let process' ctx ()         = 
    let cmds                = parse' interpreter stdin in ()  (* REPL cannot wait returning cmds *) 

let main' ()                =   try process' emptyctx (); 0
                                with Exit x            -> x 

let _                       =   Printexc.catch main' () 


