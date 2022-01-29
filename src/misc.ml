open Printf 

module BO       = BatOption 
module BL       = BatList
module L        = List
module S        = String 
module B        = BatBig_int
module R        = Rope 

type str = string 
let str_of_int  = string_of_int
let str_of_bool = string_of_bool



(*****************************************)
(**         EXCPETIONS                  **)
(*****************************************)

exception StackUnderFlow
exception StackOverFlow

let err                     = failwith
let errc str                = err("codegen_tm: " ^ str ^ " of unexpected type")



(*****************************************)
(**      LIST OPERATERS                 **)
(*****************************************)

let pi  = print_int
let ps  = print_string
let pe  = print_endline
let pf  = printf
let ef  = eprintf
let ff  = fprintf
let sf  = sprintf

(*****************************************)
(**        POLYMORPHIC FUNCTIONS        **)
(*****************************************)

let ($) f g x               = f (g x) 
let ($$$) a b c d           = ($)($)($) a b c d

let konst x y               = x
let subst t1 t2 x           = ( t1 x ) ( t2 x ) 

let rec foldn n succ zero   = if n=0 then zero else succ (foldn (n-1) succ zero) 


(*****************************************)
(**      LIST OPERATERS                 **)
(*****************************************)

let rec find_by f        = function 
    | []        ->  raise Not_found 
    | x::xs     ->  try f x     with Not_found -> find_by f xs 

let rec change_fst_by_filter f  = function 
    | []        ->  raise Not_found 
    | x::xs     ->  try f x::xs with Not_found -> x :: change_fst_by_filter f xs 

let isNil x                 = x=[]

let foldl                   = L.fold_left
let foldr                   = L.fold_right

let rec last                = function 
    | []                    -> err "last: empty list cannot contain the last element."
    | [x]                   -> x
    | x::xs                 -> last xs

let (++)                    = L.append
let len                     = L.length
let lookup                  = L.assoc 
let tl                      = L.tl
let rev                     = L.rev
let zip                     = L.combine
let unzip                   = L.split 
let sum                     = foldl (+) 0

let pr_ints                 = pe $ foldl (fun x xs -> pe x;xs) "" $ L.map str_of_int 

(*****************************************)
(**                BITS                 **)
(*****************************************)

let word_bits               = 256
let sig_bits                = 32



(*****************************************)
(**             BIG INT                 **)
(*****************************************)

type big                    = Big_int.big_int
let big                     = Big_int.big_int_of_int
let int_of_big              = Big_int.int_of_big_int
let str_of_big              = Big_int.string_of_big_int
let big_of_str              = Big_int.big_int_of_string
let big_0                   = Big_int.zero_big_int
let big_1                   = Big_int.unit_big_int
let (==)                    = Big_int.eq_big_int
let (+!)                    = Big_int.add_big_int
let (-!)                    = Big_int.minus_big_int
let( *!)                    = Big_int.mult_big_int
let (/!)                    = Big_int.div_big_int
let (^!)                    = Big_int.power_big_int_positive_big_int
let (<!)                    = Big_int.lt_big_int
let (%!)                    = Big_int.mod_big_int
let print_big               = ps $ str_of_big


(*****************************************)
(**                HEX                  **)
(*****************************************)

type hex                            =   R.t

let empty_hex                       =   R.empty
let concat_hex                      =   R.concat2
let str_of_hex ?prefix:(prefix="")h =   R.to_string(concat_hex(R.of_string prefix)h)

let hex_of_big b len                =   let s           =   B.to_string_in_hexa b         in
                                        let char_len    =   2 * len                       in (* 1 char = 1/2 byte *) 
                                        if S.length s>char_len then err "hex_of_big: too big" ; 
                                        let prefix      =   S.make(char_len-S.length s)'0' in
                                        concat_hex(R.of_string prefix)(R.of_string s)

let pr_hex ?prefix:(prefix="")h     =   pf "%s\n"(str_of_hex ~prefix h)
let hex_of_str s                    =   R.of_string s  



(*****************************************)
(**     INDEXED LIST                    **)
(*****************************************)

type idx                =   int
type 'a ilist           =   (idx * 'a) list

let to_ilist            =   function 
    | []                    -> [] 
    | l                     -> L.combine BL.(range 0 `To (L.length l - 1)) l  

let idx_sort         l  =   L.sort (fun a b -> compare (fst a)(fst b)) l 
let map              f  =   L.map  (fun(i,x)-> (i,f x)) 
let imap             f  =   L.map  (fun(i,x)-> (i,f i)) 
let filter_map       f  =   BL.filter_map (fun(i,x)->BO.map(fun y->i,y)(f x))
let pr_imap          f  =   L.iter (fun i -> pf "%d ↦ %d, "i(f i))
let insert i a l        =   (i,a)::l       (* shall I sort it?  Maybe later at once. *)
let lookup_idx f l      =   let i,_ = L.find (f $ snd) l in i 
let empty               =   []
let idxs     l          =   L.map fst l
let values   l          =   L.map snd l



