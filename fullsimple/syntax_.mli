(* module Syntax: syntax trees and associated support functions *)

open Support.Pervasive
open Support.Error

(* Data type definitions *)
type term =
    | TmVar of info * int * int
    | TmAbs of info * string * term
    | TmApp of info * term * term
    | TmTrue of info
    | TmFalse of info
    | TmIf of info * term * term * term
    | TmZero of info
    | TmSucc of info * term
    | TmPred of info * term
    | TmIsZero of info * term

type binding = 
    | NameBind

type command =
  | Eval of info * term
  | Bind of info * string * binding 

val emptycontext    : (string * binding) list
val ctxlength       : (string * binding) list -> int
val addbinding      : (string * binding) list -> string -> binding -> (string * binding) list 
val addname         : (string * binding) list -> string -> (string * binding) list
val isnamebound     : (string * binding) list -> string -> bool
val pickfreshname   : (string * binding) list -> string -> (string * binding) list * string 
(* Printing *)
val printtm: (string * binding) list -> term -> unit
val printtm_ATerm: bool -> (string * binding) list -> term -> unit


(* Misc *)
val tmInfo: term -> info

