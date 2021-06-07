open Printf 
open List 

module Maybe = BatOption 


let rec filter_getFst f = function 
  | []      ->  None
  | x::xs   ->  begin match f x with
     | None     -> filter_getFst f xs 
     | Some y   -> Some y       end 

let rec filter_changeFst f = function 
  | []      ->  None
  | x::xs   ->  begin match f x with
     | None     -> Maybe.map (cons x)(filter_changeFst f xs)
     | Some n   -> Some (n::xs) end 


let err                     = failwith

let big                     = Big_int.big_int_of_int

let ($) f g x               = f (g x) 

let isNil x                 = x=[]

