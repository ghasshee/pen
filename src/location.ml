open Printf
open Big_int
open Misc
open Syntax


type 'a data                    =   
                                {   offst   : 'a
                                ;   size    : 'a
                                }
                                    
(**********************************)
(*  IMMEDIATE VALUES ON STACK     *) 
(**********************************)
type imm                        =
                                | Big                       of big_int
                                | Int                       of int
                                | Label                     of Label.label
                                | StorPCIndex
                                | StorFieldsBegin           of int
                                | StorFieldsSize            of int    (* the size depends on the cntrct id *)
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
  | StorFieldsBegin _               -> "StorFieldBegin (print cntrct id)"
  | StorFieldsSize _                -> "StorFieldsSize (print cntrct id)"
  | InitDataSize idx                -> "InitDataSize (print cntrct id here)"
  | RntimeCntrctOffset _            -> "RntimeCntrctOffset (print contact id)"
  | RntimeMthdLabel(idx,header)     -> "RntimeMthdLabel (print cntrct id, case header)"
  | CnstrCodeSize idx               -> "CnstrCodeSize (print cntrct id)"
  | RntimeCnstrOffset idx           -> "RntimeCnstrOffset (print cntrct id)"
  | RntimeCodeOffset idx            -> "RntimeCodeOffset (print cntrct id)"
  | RntimeCodeSize                  -> "RntimeCodeSize"
  | Minus (a, b)                    -> "(- "^(string_of_imm a)^" "^(string_of_imm b)^")"


let is_const_big (b:big)        =   function 
  | Big b'                          -> eq_big_int b b'
  | Int i                           -> eq_big_int (big i) b
  | _                               -> false                      

let is_const_int (i:int)        =   is_const_big (big i)


(**********************************)
(*         LOCATION               *) 
(**********************************)

type location                   =   Code          of imm data 
                                |   Stor          of imm data 
                                |   Calldata      of int data 
                                |   Mem           of int data 
                                |   Stack         of int

let calldata (o,s)              = Calldata {offst=o; size=s} 


let string_of_location          =   function 
    | Stor _                        -> sprintf "Stor[..] "
    | Mem  m                        -> sprintf "Mem[%d..%d] "      m.offst (m.offst+m.size-1) 
    | Code _                        -> sprintf "Code    ... "
    | Calldata c                    -> sprintf "CallData[%d..%d] " c.offst (c.offst+c.size-1)
    | Stack i                       -> sprintf "Stack[%d] " i


