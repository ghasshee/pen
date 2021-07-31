open Lexer
open Lexing
open Printf
open Syntax
open Codegen
open IndexList
open Big_int

module Psr    = BatOptParse.OptParser
module Option = BatOptParse.Opt
module StdOpt = BatOptParse.StdOpt
module LI     = StorLayout
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
    | _             -> fprintf stderr "%a: syntax error\n" pr_pos lexbuf  ;exit (-1)




let enable_abi              = StdOpt.store_true () 

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
    let files                               = Psr.parse_argv optparser                                   in
    if files<>[] then e"Pass the contents to stdin.\n" else
    let abi       : bool                    = (Some true = enable_abi.Option.option_get ())              in
    let lexbuf                      = Lexing.from_channel stdin                                 in
    let _ASTs : unit toplevel list  = parse_with_error lexbuf                                   in
    let idx_ASTs                    = to_idx_list _ASTs                                         in
    let idx_typed_ASTs              = Type.addTys idx_ASTs                                      in
    let idx_ty_opt_ASTs             = Eval.eval idx_typed_ASTs                                  in 
    let cns                         = filter_map (function Cntrct cn -> Some cn
                                                         | _         -> None ) idx_ty_opt_ASTs  in
    match cns with
    | []  ->  ()
    | _   ->   
    let ccs       : cnstrctrCode idx_list         = compile_cnstrctrs cns                     in          
    let cn_layts  : LI.cn_storLayout idx_list     = map storLayout_of_cnstrctrCode ccs        in          
    let layt                                      = LI.cnstrct_storLayout cn_layts            in          
    let rc        : rntimeCode                    = compile_rntime layt cns                   in          
    let bytecode  : big_int Evm.program           = compose_bytecode ccs rc (fst(L.hd cns))   in          
    if  abi                                                                                               
        then Abi.prABI idx_ty_opt_ASTs                                                                          
        else Evm.prLn_encoded bytecode 
(*                                                                                                                                 *)
(*       +----------------------+                                                                                                  *)
(*       |         IO           |                                                                                                  *)
(*       +----------+-----------+                                                                                                  *)
(*                  |                                                                                                              *)
(*           Lexing.from_channel                                                                                                   *)
(*                  |                                                                                                              *)
(*                  v                                                                                                              *)
(*       +----------------------+                                                                                                  *)
(*       |        TOKENS        |                                                                                                  *)
(*       +----------+-----------+                                                                                                  *)
(*                  |                                                                                                              *)
(*            parse_with_error                                                                                                     *)
(*                  |                                                                                                              *)
(*                  v                                                                                                              *)
(*       +----------------------+                                                                                                  *)
(*       |         AST          |                                                                                                  *)
(*       +----------+-----------+                                                                                                  *)
(*                  |                                                                                                              *)
(*             to_idx_list                                                                                                         *)
(*                  |                                                                                                              *)
(*                  v                                                                                                              *)
(*       +----------------------+                                                                                                  *)
(*       |    indexed AST       |                                                                                                  *)
(*       +----------+-----------+                                                                                                  *)
(*                  |                                                                                                              *)
(*               addTys                                                                                                            *)
(*                  |                                                                                                              *)
(*                  v                                                                                                              *)
(*       +----------------------+                                                                                                  *)
(*       |  indexed typed AST   |                                                                                                  *)
(*       +----------+-----------+                                                                                                  *)
(*                  |                                                                                                              *)
(*         EVAL of lambda calc                 <--- to be implemented at `eval.ml`                                                 *)
(*                  |                                                                                                              *)
(*                  v                                                                                                              *)
(*       +----------------------+                                                                                                  *)
(*       |  indexed typed AST   |                                                                                                  *)
(*       +----------+-----------+                                                                                                  *)
(*                  |                                                                                                              *)
(*            remove events                                                                                                        *)
(*                  |                                                                                                              *)
(*                  v                                                                                                              *)
(*       +----------------------+                                                                                                  *)
(*       | AST of typed cntrcts +-------------+                                                                                    *)
(*       +----------+-----------+             |                                                                                    *)
(*                  |                         |                                                                                    *)
(*          compile_constrctrs                |                                                                                    *)
(*                  |                         |                                                                                    *)
(*                  v                         |                                                                                    *)
(*       +----------+-----------+             |   compile_rntime     +---------------+                                             *)
(*       |   constructor_codes  +-------+     +--------------------->+   rntimeCode  |                                             *)
(*       +----------+-----------+       |     |                      +-------+-------+                                             *)
(*                  |                   |     |                              |                                                     *)
(*      storLayout_of_cnstrctrCode      |     |                              |                                                     *)
(*                  |                   |     |                              |                                                     *)                  
(*                  v                   +------------------------------+-----+                                                     *)
(*       +----------+-----------+             |                        |                                                           *)
(*       | cntrct_storLayout    |             |                        |                                                           *)
(*       +----------+-----------+             |                 compose_bytecode                                                   *)
(*                  |                         |                        |                                                           *)                  
(*         cnstrct_storLayout                 |                        v                                                           *)
(*                  |                         |              +---------+---------+                                                 *)
(*                  v                         |              |      bytecode     |                                                 *)
(*       +----------+-----------+             |              +-------------------+                                                 *)
(*       |    storLayout        +-------------+                                                                                    *)                  
(*       +----------------------+                                                                                                  *)
(*                                                                                                                                 *)                  
(*                                                                                                                                 *)
