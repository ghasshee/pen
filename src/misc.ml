open Printf 
open List 

module Maybe = BatOption 

(*****************************************)
(**                                     **)
(*****************************************)

(*****************************************)
(**         EXCPETIONS                  **)
(*****************************************)

exception StackUnderFlow
exception StackOverFlow

let err                     = failwith
let errc str                = err("codegen_expr: " ^ str ^ " of unexpected type")


(*****************************************)
(**      LIST OPERATERS                 **)
(*****************************************)

let rec getFstFilter f = function 
    | []        ->  None
    | x::xs     ->  begin match f x with
        | None      ->  getFstFilter f xs 
        | Some y    ->  Some y       end 

let rec changeFstFilter f = function 
    | []        ->  None
    | x::xs     ->  begin match f x with
        | None      ->  Maybe.map (cons x)(changeFstFilter f xs)
        | Some n    ->  Some (n::xs) end 


let isNil x                 = x=[]

let foldl                   = fold_left



(*****************************************)
(**        POLYMORPHIC FUNCTIONS        **)
(*****************************************)

let ($) f g x               = f (g x) 
let ($$$) a b c d           = ($)($)($) a b c d

let konst x y               = x





let word_bits               = 256

let sig_bits                = 32


let big                     = Big_int.big_int_of_int
