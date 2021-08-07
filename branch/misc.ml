open Printf 

module BO       = BatOption 
module BL       = BatList
module L        = List
module S        = String 
module B        = BatBig_int
module R        = Rope 



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

let rec getFstByFilter f = function 
    | []        ->  None
    | x::xs     ->  begin match f x with
        | None      ->  getFstByFilter f xs 
        | Some y    ->  Some y       end 

let rec changeFstByFilter f = function 
    | []        ->  None
    | x::xs     ->  begin match f x with
        | None      ->  BO.map (L.cons x)(changeFstByFilter f xs)
        | Some n    ->  Some (n::xs) end 


let isNil x                 = x=[]

let foldl                   = L.fold_left
let foldr                   = L.fold_right
let rec foldn n succ zero   = if n=0 then zero else succ (foldn (n-1) succ zero) 

let rec last                = function 
    | []                    -> err "empty list cannot contain last element."
    | [x]                   -> x
    | x::xs                 -> last xs

let (++)                    = L.append
let len                     = L.length

(*****************************************)
(**        POLYMORPHIC FUNCTIONS        **)
(*****************************************)

let ($) f g x               = f (g x) 
let ($$$) a b c d           = ($)($)($) a b c d

let konst x y               = x
let subst t1 t2 x           = ( t1 x ) ( t2 x ) 

let word_bits               = 256
let sig_bits                = 32

type big                    = Big_int.big_int
let big                     = Big_int.big_int_of_int
let string_of_big           = Big_int.string_of_big_int
let big_0                   = Big_int.zero_big_int
let big_1                   = Big_int.unit_big_int



(*****************************************)
(**           HEX                       **)
(*****************************************)

type hex                                =   R.t

let empty_hex                           =   R.empty
let concat_hex                          =   R.concat2
let string_of_hex ?prefix:(prefix="")h  =   R.to_string(concat_hex(R.of_string prefix)h)

let hex_of_big_int b len                =   let s           =   B.to_string_in_hexa b         in
                                            let char_len    =   2 * len                       in (* 1 char = 1/2 byte *) 
                                            if S.length s > char_len then err "hex_of_big_int: too big" ; 
                                            let prefix      =   S.make (char_len - S.length s) '0' in
                                            concat_hex(R.of_string prefix)(R.of_string s)

let pr_hex ?prefix:(prefix="")h         =   printf"%s\n"(string_of_hex ~prefix h)
let hex_of_string s                     =   R.of_string s  (* TODO: check if the string contains only 0-9a-fA-F *)



(*****************************************)
(**     INDEXED LIST                    **)
(*****************************************)

type idx                =   int
type 'a idx_list        =   (idx * 'a) list


let to_idx_list       =   function 
    | []                    -> [] 
    | l                     -> L.combine BL.(range 0 `To (L.length l - 1)) l  

let idx_sort         l  =   L.sort (fun a b -> compare (fst a)(fst b)) l 
let map              f  =   L.map (fun(i,x)->i,f x) 
let idxmap           f  =   L.map (fun(i,x)->i,f i) 
let filter_map       f  =   BL.filter_map (fun(i,x)->BO.map(fun y->i,y)(f x))
let lookup_index     l  =   try   L.assoc l  with e-> eprintf "lookup_index: ";raise e
let pr_idx_mapping   f  =   L.iter (fun i->printf "%d ↦ %d, "i(f i))
let insert i a l        =   (i,a)::l       (* shall I sort it?  Maybe later at once. *)
let lookup_idx f l      =   let i,_ = L.find (f $ snd) l in i 
let empty               =   []
let idxs     l          =   L.map fst l
let values   l          =   L.map snd l

