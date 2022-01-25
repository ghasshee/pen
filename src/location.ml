open Big_int
open Misc
open Syntax

(**********************************)
(*  IMMEDIATE VALUES ON STACK     *) 
(**********************************)

type imm                        =
                                | Big                   of big_int
                                | Int                   of int
                                | Label                 of Label.label
                                | StorPC
                                | StorVarBegin          of int
                                | InitDataSize          of idx
                                | RnMthdLabel           of idx * Syntax.ty
                                | CrSize                of idx
                                | RnSize
                                | RnCrOffset            of idx 
                                | RnCnOffset            of idx    (* This index should be a JUMPDEST *)

let is_const_big (b:big)        =   function 
  | Big b'                          -> b == b'
  | Int i                           -> b == big i
  | _                               -> false                      

let is_const_int (i:int)        =   is_const_big (big i)


(**********************************)
(*         LOCATION               *) 
(**********************************)

type 'a data                    =   
                                {   offst   : 'a
                                ;   size    : 'a
                                }
                                    
type location                   =   Code          of imm data 
                                |   Stor          of imm data 
                                |   Calldata      of int data 
                                |   Mem           of int data 
                                |   Stack         of int

let calldata (o,s)              = Calldata {offst=o; size=s} 
(*
let str_of_location             =   function 
    | Stor _                        -> sf "Stor[..] "
    | Mem  m                        -> sf "Mem[%d..%d] "      m.offst (m.offst+m.size-1) 
    | Code _                        -> sf "Code    ... "
    | Calldata c                    -> sf "CallData[%d..%d] " c.offst (c.offst+c.size-1)
    | Stack i                       -> sf "Stack[%d] " i
*)


