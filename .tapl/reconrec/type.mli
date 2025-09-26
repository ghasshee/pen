open Format
open Support
open Syntax
open Subtype

val recon           : context -> uvargenerator -> term -> ty * uvargenerator * constr 
val unify           : info -> context -> string -> constr -> constr 
val apply_constr    : constr -> ty -> ty
val typeof          : context -> term -> ty
val prbindty        : context -> bind -> unit 
