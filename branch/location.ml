open Misc
open Big_int
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

let is_const_big (b:big_int)    =   function 
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
                                |   Stack         of int

let string_of_location          =   function 
    | Stor _                        -> "Storage ..."
    | Code _                        -> "Code ..."
    | Calldata c                    -> Printf.sprintf "Calldata offset %d, size %d" c.offst c.size
    | Stack i                       -> Printf.sprintf "Stack %d" i


(****************************************************)
(***          arg locations of mthd               ***)
(****************************************************)

let positions_of_argLens lens   =
    let rec loop ret used           =   function 
        | []                        ->  L.rev ret
        | alen::rest                ->  assert (alen>0 && alen<=32);
                                        loop (used+32-alen :: ret) (used+32) rest in 
    loop [] 4(* signature length *) lens

let argLocs_of_mthd m           =   match m.mthd_head with
    | TyDefault                 ->  []
    | TyMthd(id,args,ret)     ->  let sizes       = L.map calldata_size_of_arg args   in
                                    let positions   = positions_of_argLens sizes        in
                                    let size_pos    = L.combine positions sizes         in
                                    let locations   = L.map (fun(o,s)->Calldata{offst=o;size=s}) size_pos in
                                    let names       = L.map id_of_var args              in
                                    let argLocs     = L.combine names locations         in
                                    argLocs


