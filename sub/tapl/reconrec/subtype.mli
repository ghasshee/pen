open Format
open Support
open Syntax

val tyeqv           : context -> ty -> ty -> bool
val join            : context -> ty -> ty -> ty 
val meet            : context -> ty -> ty -> ty 
val subtype         : context -> ty -> ty -> bool
val simplifyty      : context -> ty -> ty 
