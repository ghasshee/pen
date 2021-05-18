open Lexer
open Lexing
open Printf
open Syntax
open Codegen
open Support 
open ContractId
open Big_int

let enable_abi      = BatOptParse.StdOpt.store_true () 

let optparser : BatOptParse.OptParser.t 
                    = BatOptParse.OptParser.make 
                            ~version:"0.0.02" 
                            ~usage:"bamboo [options] < src.bbo" 
                            ~description:"Bamboo compiles input from stdin and prints EVM bytecode in stdout. " ()
(*
let man         = BatOptParse.OptParser.add optparser ~long_names:["abi"] ~help:"print ABI" enable_abi
let files       = BatOptParse.OptParser.add optparser 
let _           = if files <> [] then (eprintf "Pass the contents to stdin.\n"; exit 1) 
let abi:bool    = (Some true = enable_abi.BatOptParse.Opt.option_get ()) 
let toplevels   = parse_with_error lexbuf 
let toplevels'  = fold_with_cid toplevels
let toplevels'' = Type.assign_types toplevels'
*)

let () =
  let ()                = BatOptParse.OptParser.add optparser ~long_names:["abi"] ~help:"print ABI" enable_abi  in
  let files             = BatOptParse.OptParser.parse_argv optparser                                            in
  let ()                = if files<>[] then (Printf.eprintf "Pass the contents to stdin.\n"; exit 1)            in
  let abi : bool        = (Some true = enable_abi.BatOptParse.Opt.option_get ())                                in
  let lexbuf            = Lexing.from_channel stdin         in
  let toplevels : unit Syntax.toplevel list 
                        = parse_with_error lexbuf           in
  let toplevels         = fold_with_cid toplevels   in
  let toplevels : ty Syntax.toplevel with_cid 
                        = Type.assign_types toplevels       in
  let contracts         = filter_map (function 
                            | Contract c  -> Some c
                            | _           -> None     ) toplevels in
  let ()                = match contracts with
  | []              -> ()
  | _               ->
        let constructors : constructor_compiled with_cid = compile_constructors contracts in
        let contracts_stor_layout : (cid * LayoutInfo.contract_stor_layout) list =
            List.map (fun (id, const) -> (id, stor_layout_from_constructor_compiled const)) constructors in
        let layout = LayoutInfo.construct_stor_layout contracts_stor_layout in
        let runtime_compiled = compile_runtime layout contracts in
        let bytecode : big_int Evm.program =
            compose_bytecode constructors runtime_compiled (fst (List.hd contracts)) in
        let () = if abi then Abi.prABI toplevels else Evm.pr_encoded bytecode in
     () in
  ()
