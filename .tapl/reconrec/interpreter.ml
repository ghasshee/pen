open Format
open Arg 

open Support
open Syntax
open Type
open Eval

(*--------- REPL / Compiler -----------*)

let repl        lexbuf  =   Parser.input      Lexer.token lexbuf 
let compiler    lexbuf  =   Parser.toplevel   Lexer.token lexbuf
                                 
let parse'' machine in_channel =   (* machine -> in_channel -> command list *) 
    let lexbuf              =   Lexing.from_channel in_channel  in
    let cmds,ctx,store,uvar,constr      =   try     machine lexbuf emptyctx emptystore uvargen [] 
                                with  | Parsing.Parse_error -> error (Lexer.info lexbuf) "Parse error" 
                                      | e -> raise e   in
    Parsing.clear_parser(); close_in in_channel; cmds 

let parse' machine in_channel =   (* machine -> in_channel -> command list *) 
    let lexbuf              =   Lexing.from_channel in_channel  in
    let result,ctx          =   try     machine lexbuf emptyctx 
                                with  | Parsing.Parse_error -> error (Lexer.info lexbuf) "Parse error" 
                                      | e -> raise e   in
    Parsing.clear_parser(); close_in in_channel; result

(*-----------  FILE ---------------*) 

let path                = ref [""]
let addpath f           = path := f :: !path
let argDefs             = [ ("-I", String addpath, "Append a dir to path")]
let files               = ref (None : string option )
let getFile ()          = match !files with
    | None                  -> err "Cannot get File"
    | Some s                -> s;;
let pushFile str        = match !files with 
    | Some (_)              -> err "Specify Single File."
    | None                  -> files := Some str 
let parseArgs ()        = parse argDefs pushFile "" ;; 
let openFile f          =  (* string -> in_channel *)
    let rec trynext         = function 
        | []                    ->  err ("Could not find " ^ f)
        | (d::rest)             ->  let name = if d = "" then f else (d ^ "/" ^ f) in
                                    try open_in name    with Sys_error m -> trynext rest in 
    trynext !path

(*----------- PARSE FILE (Compiler) ----------*)

let parseFile f         =  (* string -> command list *) 
    let in_channel          =   openFile f in 
    parse' compiler in_channel 

let process_file str ctx store nextuvar constr =  (* string -> unit *)  (* print the evals of the list of commands *)  
    let cmds                =   parseFile str in
    process_commands ctx store nextuvar constr cmds 


