(* imm := the type of immediate values which is pushed to stack  *)


(* imm := the sum type of all immediate value types whose elements are pushed to stack *) 


open Big_int
open ContractId


type imm =
  | Big                                 of big_int
  | Int                                 of int
  | Label                               of Label.label
  | StoragePCIndex
  | StorageCnstrctrArgumentsBegin    of cid
  | StorageCnstrctrArgumentsSize     of cid    (* the size depends on the cntrct id *)
  | InitDataSize                        of cid
  | ContractOffsetInRuntimeCode         of cid    (* This index should be a JUMPDEST *)
  | CaseOffsetInRuntimeCode             of cid * Syntax.mthd_head
  | CnstrctrCodeSize                 of cid
  | CnstrctrInRuntimeCodeOffset      of cid
  | RuntimeCodeOffset                   of cid
  | RuntimeCodeSize
  | Minus                               of imm * imm

let rec string_of_imm           = function 
  | Big b                               -> "(Big "^(string_of_big_int b)^")"
  | Int i                               -> "(Int "^(string_of_int i)^")"
  | Label _                             -> "Label (print label here)"
  | StoragePCIndex                      -> "StoragePCIndex"
  | StorageCnstrctrArgumentsBegin _  -> "StorageCnstrctrArgumentBegin (print cntrct id)"
  | StorageCnstrctrArgumentsSize _   -> "StorageCnstrctrArgumentsSize (print cntrct id)"
  | InitDataSize cid                    -> "InitDataSize (print cntrct id here)"
  | ContractOffsetInRuntimeCode _       -> "ContractOffsetInRuntimeCode (print contact id)"
  | CaseOffsetInRuntimeCode(cid,header) -> "CaseOffsetInRuntimeCode (print cntrct id, case header)"
  | CnstrctrCodeSize cid             -> "CnstrctrCodeSize (print cntrct id)"
  | CnstrctrInRuntimeCodeOffset cid  -> "CnstrctrInRuntimeCodeOffset (print cntrct id)"
  | RuntimeCodeOffset cid               -> "RuntimeCodeOffset (print cntrct id)"
  | RuntimeCodeSize                     -> "RuntimeCodeSize"
  | Minus (a, b)                        -> "(- "^(string_of_imm a)^" "^(string_of_imm b)^")"

let is_const_big (b:big_int) = function 
  | Big b'                              -> eq_big_int b b'
  | Int i                               -> eq_big_int(big_int_of_int i)b
  | _                                   -> false                        (* XXX: very rough approximation *)

let is_const_int (i:int)     = is_const_big (big_int_of_int i)
