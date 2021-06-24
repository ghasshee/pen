open Lexer
open Lexing
open Printf
open Syntax
open Codegen
open Support 
open IndexedList
open Big_int

module Parser = BatOptParse.OptParser
module Option = BatOptParse.Opt
module StdOpt = BatOptParse.StdOpt
module LI     = StorLayout
module L      = List

let enable_abi      = StdOpt.store_true () 

let optparser : Parser.t 
                    = Parser.make 
                            ~version:"0.0.02" 
                            ~usage:"bamboo [options] < src.bbo" 
                            ~description:"Bamboo compiles input from stdin and prints EVM bytecode in stdout. " ()
(*
let man         = BatOptParse.OptParser.add optparser ~long_names:["abi"] ~help:"print ABI" enable_abi
let files       = BatOptParse.OptParser.add optparser 
let _           = if files <> [] then (eprintf "Pass the contents to stdin.\n"; exit 1) 
let abi:bool    = (Some true = enable_abi.BatOptParse.Opt.option_get ()) 
let toplevels   = parse_with_error lexbuf 
Aet toplevels'  = to_idx_list toplevels
let toplevels'' = Type.assignTys toplevels'
*)
let e str   = eprintf str; exit 1

let ()      =
    Parser.add optparser 
        ~long_names:["abi"] 
        ~help:"print ABI" enable_abi ; 
    let files                              = Parser.parse_argv optparser                            in
    if files<>[] then e"Pass the contents to stdin.\n" else
    let abi       : bool                   = (Some true = enable_abi.Option.option_get ())          in
    let lexbuf                             = Lexing.from_channel stdin                              in
    let toplevels : unit toplevel list     = parse_with_error lexbuf                                in
    let toplevels                          = to_idx_list toplevels                                  in
    let toplevels : ty toplevel idx_list   = Type.assignTys toplevels                               in
    let cns                                = filter_map (function   | Cntrct cn -> Some cn
                                                                    | _         -> None ) toplevels in
    match cns with
    | []  ->  ()
    | _   ->  let ccs       : cnstrctrCode idx_list         = compile_cnstrctrs cns                     in 
              let cn_layts  : LI.cn_storLayout idx_list     = map storLayout_of_cnstrctrCode ccs     in
              let layt                                      = LI.cnstrct_storLayout cn_layts            in
              let rc        : rntimeCode                    = compile_rntime layt cns                   in
              let bytecode  : big_int Evm.program           = compose_bytecode ccs rc (fst(L.hd cns))   in
              if  abi 
                  then Abi.prABI toplevels 
                  else Evm.pr_encoded bytecode 

(*                                                                                                                                 *)
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
