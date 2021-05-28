(* CE := Codegen Environment *)

open Syntax
open ContractId
open Evm 

type ce             =
                    { stack_size     : int
                    ; program        : Imm.imm program
                    ; cid_lookup     : string -> cid
                    ; cntrcts      : ty cntrct with_cid }

let extract_program ce              =   ce.program
let cid_lookup      ce              =   ce.cid_lookup

let empty_ce cid_lookup cntrcts   =
    { stack_size     = 0
    ; program        = empty_program
    ; cid_lookup     = cid_lookup
    ; cntrcts      = cntrcts  }

let code_length    ce               =   size_of_program ce.program
let stack_size     ce               =   ce.stack_size
let set_stack_size ce i             =   { ce with stack_size = i }

let append_opcode ce opcode         =
    if ce.stack_size < stack_popped opcode then failwith "stack underflow"
    else    (match opcode with
                | JUMPDEST l        ->  (try ignore (Label.lookup_value l)
                                        with Not_found -> Label.register_value l (code_length ce) )
                | _                 ->  ()   ); 
            let new_stack_size = ce.stack_size - stack_popped opcode + stack_pushed opcode in
            if new_stack_size > 1024 then failwith "stack overflow"
            else    { stack_size = new_stack_size
                    ; program    = opcode :: ce.program 
                    ; cid_lookup = ce.cid_lookup
                    ; cntrcts  = ce.cntrcts  }


let (>>>) op ce = append_opcode ce op  


let cntrct_lookup ce cid          =   try choose_cntrct cid ce.cntrcts
                                        with e ->   (Printf.eprintf "cntrct_lookup failed on %d\n%!" cid; 
                                                    (pr_cid_mapping(fun x->x)(cids ce.cntrcts)); 
                                                    raise e)
