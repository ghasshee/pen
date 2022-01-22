open Misc
open Semiring
open Lexer
open Lexing
open Printf
open Syntax
open Codegen
open Big_int

module Psr    = BatOptParse.OptParser
module Option = BatOptParse.Opt
module StdOpt = BatOptParse.StdOpt
module LI     = Layout
module L      = List

(* The following two functions comes from
 * https://github.com/realworldocaml/examples/tree/master/code/parsing-test
 * which is under UNLICENSE *)

let e str                   = eprintf str; exit 1

let pr_pos outx lexbuf      =
    let pos = lexbuf.lex_curr_p in
    fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf = 
    try Parser.file Lexer.read lexbuf with
    | SyntaxError e -> fprintf stderr "%a: %s\n"           pr_pos lexbuf e;exit (-1)
    | Parser.Error  -> fprintf stderr "%a: syntax error\n" pr_pos lexbuf  ;exit (-1)
    | e             -> fprintf stderr "%a: Unknown syntax error\n" pr_pos lexbuf  ; raise e; exit (-1)


let enable_abi              = StdOpt.store_true () 
let enable_asm              = StdOpt.store_true ()

let optparser : Psr.t       = Psr.make 
                                ~version:"0.0.1" 
                                ~usage:"pen [options] < src.bbo" 
                                ~description:"Pen compiles input from stdin and prints EVM bytecode in stdout. " ()
                                (*
let man                     = BatOptParse.OptParser.add optparser ~long_names:["abi"] ~help:"print ABI" enable_abi
let files                   = BatOptParse.OptParser.add optparser 
let _                       = if files <> [] then (eprintf "Pass the contents to stdin.\n"; exit 1) 
let abi:bool                = (Some true = enable_abi.BatOptParse.Opt.option_get ()) 
*)
let ()      =
    Psr.add optparser ~long_names:["abi"] ~help:"print ABI" enable_abi ; 
    Psr.add optparser ~long_names:["asm"] ~help:"print Assembly Code" enable_asm ; 
    let files                       = Psr.parse_argv optparser                          in
    if files<>[] then e"Pass the contents to stdin.\n" else
    let abi       : bool            = (Some true = enable_abi.Option.option_get ())     in
    let asm       : bool            = (Some true = enable_asm.Option.option_get ())     in
    if asm then pe"----- compilation start -----" ; 
    let lexbuf                      = Lexing.from_channel stdin                             in
    if asm then pe"-------- lexing done --------" ; 
    let asts : unit toplevel list   = parse_with_error lexbuf                               in
    if asm then pe"--------- parse done --------" ; 
    let i_asts                      = to_ilist asts                                         in
    if asm then pe"-------- indexed ASTs -------" ; 
    let i_ty_asts                   = Type.addTys i_asts                                    in
    if asm then pe"--------- typed -------------" ;
    let i_ty_opt_asts               = Eval.eval i_ty_asts                                   in 
    if asm then pe"--------- evaluated ---------" ; 
    let cns                         = filter_map (function TmEv e -> None 
                                                         | cn     -> Some cn) i_ty_opt_asts in
    if asm then pe"------ extract cntrcts ------" ; 
    match cns with
    | []  ->  ()
    | _   ->   
    let crs      : creation ilist   = init_creations cns                in          
    if asm then pe"---- creation codes built ----"; 
    let cr_infos                    = cr_infos_of_crs crs               in          
    if asm then pe"---- creation infos built ----";
    let stor                        = LI.init_storage cr_infos          in          
    if asm then pe"---- storage layout built ----";
    let rn       : rntime           = compile_rntime stor cns           in          
    if asm then pe"---- contrct layout built ----";
    let tycn                        = L.hd cns                          in 
    let cn                          = fst tycn                          in 
    let bytecode : big Evm.program  = compose_bytecode crs rn cn        in 
    if asm then pe"----- bytecode composed ------"; 
    if  abi                                                                                               
        then Abi.prABI i_ty_opt_asts                                                                          
        else if asm 
            then Evm.prLn_encoded bytecode 
            else Evm.pr_encoded bytecode
(*                                                                                            *)
(*       +----------------------+                                                             *)
(*       |         IO           |                                                             *)
(*       +----------+-----------+                                                             *)
(*                  |                                                                         *)
(*           Lexing.from_channel                                                              *)
(*                  |                                                                         *)
(*                  v                                                                         *)
(*       +----------------------+                                                             *)
(*       |        TOKENS        |                                                             *)
(*       +----------+-----------+                                                             *)
(*                  |                                                                         *)
(*            parse_with_error                                                                *)
(*                  |                                                                         *)
(*                  v                                                                         *)
(*       +----------------------+                                                             *)
(*       |         AST          |                                                             *)
(*       +----------+-----------+                                                             *)
(*                  |                                                                         *)
(*               to_ilist                                                                     *)
(*                  |                                                                         *)
(*                  v                                                                         *)
(*       +----------------------+                                                             *)
(*       |    indexed AST       |                                                             *)
(*       +----------+-----------+                                                             *)
(*                  |                                                                         *)
(*               addTys                                                                       *)
(*                  |                                                                         *)
(*                  v                                                                         *)
(*       +----------------------+                                                             *)
(*       |  indexed typed AST   |                                                             *)
(*       +----------+-----------+                                                             *)
(*                  |                                                                         *)
(*         EVAL of lambda calc                 <--- to be implemented at `eval.ml`            *)
(*                  |                                                                         *)
(*                  v                                                                         *)
(*       +----------------------+                                                             *)
(*       |  indexed typed AST   |                                                             *)
(*       +----------+-----------+                                                             *)
(*                  |                                                                         *)
(*            remove events                                                                   *)
(*                  |                                                                         *)
(*                  v                                                                         *)
(*       +----------------------+                                                             *)
(*       | AST of typed cntrcts +-------------+                                               *)
(*       +----------+-----------+             |                                               *)
(*                  |                         |                                               *)
(*          compile_constrctrs                |                                               *)
(*                  |                         |                                               *)
(*                  v                         |                                               *)
(*       +----------+-----------+             |   compile_rntime     +---------------+        *)
(*       |   constructor_codes  +-------+     +--------------------->+   rntimeCode  |        *)
(*       +----------+-----------+       |     |                      +-------+-------+        *)
(*                  |                   |     |                              |                *)
(*         storage_of_cnstrctrCode      |     |                              |                *)
(*                  |                   |     |                              |                *)                  
(*                  v                   +------------------------------+-----+                *)
(*       +----------+-----------+             |                        |                      *)
(*       |    cntrct_storage    |             |                        |                      *)
(*       +----------+-----------+             |                 compose_bytecode              *)
(*                  |                         |                        |                      *)                  
(*            cnstrct_storage                 |                        v                      *)
(*                  |                         |              +---------+---------+            *)
(*                  v                         |              |      bytecode     |            *)
(*       +----------+-----------+             |              +-------------------+            *)
(*       |       storage        +-------------+                                               *)                  
(*       +----------------------+                                                             *)
(*                                                                                            *)                  
(*                                                                                            *)

(* semiring test  
let _ = pe (str_of_rig (mul (rig_of_int (168)) (N (S (S (S O))))))  
 *)
