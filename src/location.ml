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
                                | Big                   of big_int
                                | Int                   of int
                                | Label                 of Label.label
                                | StorPC
                                | StorFldBegin          of int
                                | StorFldSize           of int    (* the size depends on the cntrct id *)
                                | InitDataSize          of idx
                                | RnMthdLabel           of idx * Syntax.ty
                                | CrSize                of idx
                                | RnCrOffset            of idx
                                | RnCnOffset            of idx    (* This index should be a JUMPDEST *)
                                | RnOffset              of idx
                                | RnSize

let rec str_of_imm           =   function 
  | Big b                           -> "(Big "^(str_of_big b)^")"
  | Int i                           -> "(Int "^(str_of_int i)^")"
  | Label _                         -> "Label (print label here)"
  | StorPC                          -> "StorPC"
  | StorFldBegin _                  -> "StorFldBegin (print cntrct id)"
  | StorFldSize _                   -> "StorFldSize (print cntrct id)"
  | InitDataSize idx                -> "InitDataSize (print cntrct id here)"
  | RnCnOffset _                    -> "RnCnOffset (print contact id)"
  | RnMthdLabel(idx,header)         -> "RnMthdLabel (print cntrct id, case header)"
  | CrSize idx                      -> "CreationSize (print cntrct id)"
  | RnCrOffset idx                  -> "RnCrOffset (print cntrct id)"
  | RnOffset idx                    -> "RnOffset (print cntrct id)"
  | RnSize                          -> "RnSize"


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


let str_of_location          =   function 
    | Stor _                        -> sf "Stor[..] "
    | Mem  m                        -> sf "Mem[%d..%d] "      m.offst (m.offst+m.size-1) 
    | Code _                        -> sf "Code    ... "
    | Calldata c                    -> sf "CallData[%d..%d] " c.offst (c.offst+c.size-1)
    | Stack i                       -> sf "Stack[%d] " i


