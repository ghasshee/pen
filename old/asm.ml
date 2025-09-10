

open Misc
open Syntax 
open Evm
open CodegenEnv

type addr       = 
                | Var of int 
                | Arr of int * int 
                | Mem of int 
                | Sto of int
                | Frm of int * int 

type chan       = 
                | Ch  of int 


type frm        = addr -> int
type mem        = frm list 
type buf        = chan -> int list 



let ch          = ref 0
let fresh_ch () = let v = !ch  in ch  := v + 1; v

let var         = ref 0 
let fresh_var() = let v = !var in var := v + 1; v

let arr         = ref 0 
let fresh_arr() = let v = !arr in arr := v + 1; v

type var        = int ref 

type decl       = 
                | Decl      of addr  
                | DSeq      of decl * decl
                | Proc      of int list * cmd


and  cmd        = Skip
                | Bool      of bexp 
                | Assign    of addr * aexp
                | Send      of ch * aexp
                | Recv      of ch * addr 
                | If        of cg
                | Do        of gc
                | Loop      of cg
                | Seq       of cmd * cmd 
                | Call      of ( var * aexp ) list 
                | Stop 
                

and  gc         = Cond      of bexp * cmd
                | Branch    of gc * gc 

and  cg         = SendThen  of ch * decl * cmd
                | RecvThen  of ch * addr * cmd
                | Cond      of bexp * cmd
                | Branch    of cg * cg 

and  ch         = Ch        of int 

and  aexp       = Num       of int 
                | Add       of aexp * aexp 
                | Sub       of aexp * aexp 
                | Mul       of aexp * aexp
                | Div       of aexp * aexp
                | Mod       of aexp * aexp
                | Get       of config

and  config     = 
                | Address
                | Balance
                | GasPrice
                | CallValue
                | TimeStamp
                | Difficulty


and  bexp       = 
                | True 
                | False 
                | Lt        of aexp * aexp
                | Gt        of aexp * aexp 
                | Eq        of aexp * aexp 
                | IsZero    of aexp 
                | Not       of bexp 
                | And       of bexp * bexp 
                | Or        of bexp * bexp
                | Xor       of bexp * bexp 
            
;;

let check b     = Comment "if b then next go ahead else another choose another branch (Unimplemented Now) " 
let assign x a  = match x with 
    | Var(x)        -> Comment "x := a" 
    | Arr(id,i)     -> Comment "id[i] := a"

let asm_cmd     = function 
    | Skip          -> Comment "Skip Command"
    | Bool(b)       -> check b 
    | Assign(x,a)   -> assign x a 

;;






                
