open Misc
open Semiring
open Lexer
open Lexing
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

let e str                   = ef str; exit 1

let pr_pos outx lexbuf      =
    let pos = lexbuf.lex_curr_p in
    ff outx "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf = 
    try Parser.file Lexer.read lexbuf with
    | SyntaxError e -> ff stderr "%a: %s\n"           pr_pos lexbuf e;exit (-1)
    | Parser.Error  -> ff stderr "%a: syntax error\n" pr_pos lexbuf  ;exit (-1)
    | e             -> ff stderr "%a: Unknown syntax error\n" pr_pos lexbuf  ; raise e; exit (-1)




let enable_abi              = StdOpt.store_true () 
let enable_asm              = StdOpt.store_true ()

let optparser : Psr.t       = Psr.make 
                                ~version:"0.0.1" 
                                ~usage:"pen [options] < src.pen" 
                                ~description:"Pen compiles STDIN and prints EVM bytecode at STDOUT. " ()
                                (*
let man                     = BatOptParse.OptParser.add optparser ~long_names:["abi"] ~help:"print ABI" enable_abi
let files                   = BatOptParse.OptParser.add optparser 
*)

let ()      =
    Psr.add optparser ~long_names:["abi"] ~help:"print ABI" enable_abi ; 
    Psr.add optparser ~long_names:["asm"] ~help:"print Assembly Code" enable_asm ; 
    let files                       = Psr.parse_argv optparser                          in
    if files<>[] then e"Pass the contents to stdin.\n"; 
    let Some enableABI = enable_abi.Option.option_get ()     in
    let Some enableASM = enable_asm.Option.option_get ()     in


    if enableASM then pe"----- compilation start -----" ; 
    let lexbuf                      = Lexing.from_channel stdin                         in
    if enableASM then pe"-------- lexing done --------" ; 
    let asts : unit toplevel list   = parse_with_error lexbuf                           in
    if enableASM then pe"--------- parse done --------" ; 
    let i_asts                      = to_ilist asts                                     in
    if enableASM then pe"-------- indexed ASTs -------" ; 
    let i_ty_asts                   = Type.type_tops i_asts                             in
    if enableASM then pe"--------- typed -------------" ;
    let i_ty_opt_asts               = Eval.eval i_ty_asts                               in 
    if enableASM then pe"--------- evaluated ---------" ; 
    let cns                         = filter_map (function TmEv e -> None 
                                                         | cn     -> Some cn) i_ty_opt_asts in
    if enableASM then pe"------ extract cntrcts ------" ; 
    match cns with
    | []  ->  ()
    | _   ->   
    let crs      : creation ilist   = init_crs cns                      in          
    if enableASM then pe"---- creation codes built ----"; 
    let cr_infos                    = cr_infos_of_crs crs               in          
    if enableASM then pe"---- creation infos built ----";
    let sto                         = LI.init_storage cr_infos          in          
    if enableASM then pe"---- storage layout built ----";
    let rn       : rntime           = compile_rntime sto cns            in          
    if enableASM then pe"---- contrct layout built ----";
    let itycn                       = L.hd cns                          in 
    let idx                         = fst itycn                         in 
    let bytecode : big Evm.program  = compose_bytecode crs rn idx       in 
    if enableASM then pe"----- bytecode composed ------"; 
    if enableABI                                                                                                
        then Abi.prABI i_ty_opt_asts                                                                          
        else if enableASM 
            then Evm.prLn_encoded bytecode 
            else Evm.pr_encoded   bytecode
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
