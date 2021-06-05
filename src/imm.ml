(* imm := the type of immediate values which is pushed to stack  *)


(* imm := the sum type of all immediate value types whose elements are pushed to stack *) 


open Big_int
open ContractId


type imm =
  | Big                                 of big_int
  | Int                                 of int
  | Label                               of Label.label
  | StorPCIndex
  | StorCnstrctrArgsBegin    of idx
  | StorCnstrctrArgsSize     of idx    (* the size depends on the cntrct id *)
  | InitDataSize                        of idx
  | CntrctOffsetInRuntimeCode         of idx    (* This index should be a JUMPDEST *)
  | MthdAddrInRuntimeCode             of idx * Syntax.mthd_head
  | CnstrctrCodeSize                 of idx
  | CnstrctrInRuntimeCodeOffset      of idx
  | RuntimeCodeOffset                   of idx
  | RuntimeCodeSize
  | Minus                               of imm * imm

let rec string_of_imm           = function 
  | Big b                               -> "(Big "^(string_of_big_int b)^")"
  | Int i                               -> "(Int "^(string_of_int i)^")"
  | Label _                             -> "Label (print label here)"
  | StorPCIndex                      -> "StorPCIndex"
  | StorCnstrctrArgsBegin _          -> "StorCnstrctrArgBegin (print cntrct id)"
  | StorCnstrctrArgsSize _           -> "StorCnstrctrArgsSize (print cntrct id)"
  | InitDataSize idx                    -> "InitDataSize (print cntrct id here)"
  | CntrctOffsetInRuntimeCode _       -> "CntrctOffsetInRuntimeCode (print contact id)"
  | MthdAddrInRuntimeCode(idx,header) -> "MthdAddrInRuntimeCode (print cntrct id, case header)"
  | CnstrctrCodeSize idx                -> "CnstrctrCodeSize (print cntrct id)"
  | CnstrctrInRuntimeCodeOffset idx     -> "CnstrctrInRuntimeCodeOffset (print cntrct id)"
  | RuntimeCodeOffset idx               -> "RuntimeCodeOffset (print cntrct id)"
  | RuntimeCodeSize                     -> "RuntimeCodeSize"
  | Minus (a, b)                        -> "(- "^(string_of_imm a)^" "^(string_of_imm b)^")"

let is_const_big (b:big_int) = function 
  | Big b'                              -> eq_big_int b b'
  | Int i                               -> eq_big_int(big_int_of_int i)b
  | _                                   -> false                        (* XXX: very rough approximation *)

let is_const_int (i:int)     = is_const_big (big_int_of_int i)
