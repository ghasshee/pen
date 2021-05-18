open Lexer
open Lexing
open Printf
open Syntax
open Codegen
open Support 

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
let toplevels'  = Assoc.list_to_contract_id_assoc toplevels
let toplevels'' = Type.assign_types toplevels'
*)

let () =
  let ()    = BatOptParse.OptParser.add optparser ~long_names:["abi"] ~help:"print ABI" enable_abi in
  let files = BatOptParse.OptParser.parse_argv optparser in
  let ()    = if files <> [] then(Printf.eprintf "Pass the contents to stdin.\n"; exit 1) in
  let abi : bool        = (Some true = enable_abi.BatOptParse.Opt.option_get ()) in
  let lexbuf            = Lexing.from_channel stdin in
  let toplevels : unit Syntax.toplevel list 
                        = parse_with_error lexbuf in
  let toplevels         = Assoc.list_to_contract_id_assoc toplevels in
  let toplevels : Syntax.typ Syntax.toplevel Assoc.contract_id_assoc 
                        = Type.assign_types toplevels in
  let contracts         = Assoc.filter_map (function 
                            | Contract c  -> Some c
                            | _           -> None     ) toplevels in
  let ()                = match contracts with
  | []              -> ()
  | _               ->
        let constructors : constructor_compiled Assoc.contract_id_assoc = compile_constructors contracts in
        let contracts_layout_info : (Assoc.contract_id * LayoutInfo.contract_layout_info) list =
            List.map (fun (id, const) -> (id, layout_info_from_constructor_compiled const)) constructors in
        let layout = LayoutInfo.construct_layout_info contracts_layout_info in
        let runtime_compiled = compile_runtime layout contracts in
        let bytecode : Big_int.big_int Evm.program =
            compose_bytecode constructors runtime_compiled (fst (List.hd contracts)) in
        let () = if abi then Ethereum.print_abi toplevels else Evm.print_imm_program bytecode in
     () in
  ()
