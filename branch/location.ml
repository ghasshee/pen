open Misc
open Hexa 
open Big_int
open IndexList

(**********************************)
(*  IMMEDIATE VALUES ON STACK     *) 
(**********************************)
type imm                        =
                                | Big                       of big_int
                                | Int                       of int
                                | Label                     of Label.label
                                | StorPCIndex
                                | StorCnstrArgsBegin        of idx
                                | StorCnstrArgsSize         of idx    (* the size depends on the cntrct id *)
                                | InitDataSize              of idx
                                | RntimeCntrctOffset        of idx    (* This index should be a JUMPDEST *)
                                | RntimeMthdLabel           of idx * Syntax.ty
                                | CnstrCodeSize             of idx
                                | RntimeCnstrOffset         of idx
                                | RntimeCodeOffset          of idx
                                | RntimeCodeSize
                                | Minus                     of imm * imm

let rec string_of_imm           =   function 
  | Big b                           -> "(Big "^(string_of_big_int b)^")"
  | Int i                           -> "(Int "^(string_of_int i)^")"
  | Label _                         -> "Label (print label here)"
  | StorPCIndex                     -> "StorPCIndex"
  | StorCnstrArgsBegin _            -> "StorCnstrArgBegin (print cntrct id)"
  | StorCnstrArgsSize _             -> "StorCnstrArgsSize (print cntrct id)"
  | InitDataSize idx                -> "InitDataSize (print cntrct id here)"
  | RntimeCntrctOffset _            -> "RntimeCntrctOffset (print contact id)"
  | RntimeMthdLabel(idx,header)     -> "RntimeMthdLabel (print cntrct id, case header)"
  | CnstrCodeSize idx               -> "CnstrCodeSize (print cntrct id)"
  | RntimeCnstrOffset idx           -> "RntimeCnstrOffset (print cntrct id)"
  | RntimeCodeOffset idx            -> "RntimeCodeOffset (print cntrct id)"
  | RntimeCodeSize                  -> "RntimeCodeSize"
  | Minus (a, b)                    -> "(- "^(string_of_imm a)^" "^(string_of_imm b)^")"

let is_const_big (b:big_int)    =   function 
  | Big b'                          -> eq_big_int b b'
  | Int i                           -> eq_big_int (big i) b
  | _                               -> false                      

let is_const_int (i:int)        =   is_const_big (big i)


(**********************************)
(*         LOCATION               *) 
(**********************************)
type 'imm code_range            =   { code_start        : 'imm  (* byte *)
                                    ; code_size         : 'imm  (* byte *)  }
                                
type 'imm stor_range            =   { stor_start        : 'imm  (* word *)
                                    ; stor_size         : 'imm  (* word *)  }
                                
type calldata_range             =   { calldata_start    : int
                                    ; calldata_size     : int               }

type location                   =   Code          of imm code_range
                                |   Stor          of imm stor_range
                                |   Calldata      of calldata_range
                                |   Stack         of int

let string_of_location          =   function 
    | Stor _                        -> "Storage ..."
    | Code _                        -> "Code ..."
    | Calldata c                    -> Printf.sprintf "Calldata offset %d, size %d" c.calldata_start c.calldata_size
    | Stack i                       -> Printf.sprintf "Stack %d" i

