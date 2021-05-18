(* imm := the type of immediate values which is pushed to stack  *)


(* imm := the sum type of all immediate value types whose elements are pushed to stack *) 


open Big_int
open ContractId


type imm =
  | Big                                 of big_int
  | Int                                 of int
  | Label                               of Label.label
  | StoragePCIndex
  | StorageConstructorArgumentsBegin    of cid
  | StorageConstructorArgumentsSize     of cid    (* the size depends on the contract id *)
  | InitDataSize                        of cid
  | ContractOffsetInRuntimeCode         of cid    (* This index should be a JUMPDEST *)
  | CaseOffsetInRuntimeCode             of cid * Syntax.mthd_head
  | ConstructorCodeSize                 of cid
  | ConstructorInRuntimeCodeOffset      of cid
  | RuntimeCodeOffset                   of cid
  | RuntimeCodeSize
  | Minus                               of imm * imm

let rec string_of_imm           = function 
  | Big b                               -> "(Big "^(string_of_big_int b)^")"
  | Int i                               -> "(Int "^(string_of_int i)^")"
  | Label _                             -> "Label (print label here)"
  | StoragePCIndex                      -> "StoragePCIndex"
  | StorageConstructorArgumentsBegin _  -> "StorageConstructorArgumentBegin (print contract id)"
  | StorageConstructorArgumentsSize _   -> "StorageConstructorArgumentsSize (print contract id)"
  | InitDataSize cid                    -> "InitDataSize (print contract id here)"
  | ContractOffsetInRuntimeCode _       -> "ContractOffsetInRuntimeCode (print contact id)"
  | CaseOffsetInRuntimeCode(cid,header) -> "CaseOffsetInRuntimeCode (print contract id, case header)"
  | ConstructorCodeSize cid             -> "ConstructorCodeSize (print contract id)"
  | ConstructorInRuntimeCodeOffset cid  -> "ConstructorInRuntimeCodeOffset (print contract id)"
  | RuntimeCodeOffset cid               -> "RuntimeCodeOffset (print contract id)"
  | RuntimeCodeSize                     -> "RuntimeCodeSize"
  | Minus (a, b)                        -> "(- "^(string_of_imm a)^" "^(string_of_imm b)^")"

let is_constant_big (b:big_int) = function 
  | Big b'                              -> eq_big_int b b'
  | Int i                               -> eq_big_int(big_int_of_int i)b
  | _                                   -> false                        (* XXX: very rough approximation *)

let is_constant_int (i:int)     = is_constant_big (big_int_of_int i)
