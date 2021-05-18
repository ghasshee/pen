(* codegenEnv remembers the current stack size,
   initial storage assumtion, and
   accumulated opcodes. *)
type t

val empty_env : (string -> Assoc.contract_id) -> (Syntax.typ Syntax.contract Assoc.contract_id_assoc) -> t

val ce_program : t -> PseudoImm.pseudo_imm Evm.program
val code_length : t -> int

val stack_size : t -> int
val set_stack_size : t -> int -> t

(* for each opcode,
 * create an interface function.
 * This allows keeping track of stack size...
 *)

val append_opcode :
  t -> PseudoImm.pseudo_imm Evm.opcode -> t

val cid_lookup : t -> string -> Assoc.contract_id

val contract_lookup : t -> Assoc.contract_id -> Syntax.typ Syntax.contract
