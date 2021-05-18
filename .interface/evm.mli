type 'imm opcode =
  | PUSH1 of 'imm
  | PUSH4 of 'imm
  | PUSH32 of 'imm
  | NOT
  | TIMESTAMP
  | EQ
  | ISZERO
  | LT
  | GT
  | BALANCE
  | STOP
  | ADD
  | MUL
  | SUB
  | DIV
  | SDIV
  | MOD
  | SMOD
  | ADDMOD
  | MULMOD
  | EXP
  | SIGNEXTEND
  | SHA3
  | ADDRESS
  | ORIGIN
  | CALLER
  | CALLVALUE
  | CALLDATALOAD
  | CALLDATASIZE
  | CALLDATACOPY
  | CODESIZE
  | CODECOPY
  | GASPRICE
  | EXTCODESIZE
  | EXTCODECOPY
  | POP
  | MLOAD
  | MSTORE
  | MSTORE8
  | SLOAD
  | SSTORE
  | JUMP
  | JUMPI
  | PC
  | MSIZE
  | GAS
  | JUMPDEST of Label.label
  | LOG0
  | LOG1
  | LOG2
  | LOG3
  | LOG4
  | CREATE
  | CALL
  | CALLCODE
  | RETURN
  | DELEGATECALL
  | SUICIDE
  | SWAP1
  | SWAP2
  | SWAP3
  | SWAP4
  | SWAP5
  | SWAP6
  | DUP1
  | DUP2
  | DUP3
  | DUP4
  | DUP5
  | DUP6
  | DUP7

val log : int -> 'a opcode


val dup_suc_n : int -> 'imm opcode

(** ['imm program] is a sequence of EVM opcodes
 * where immediate values are expressed with type 'imm *)
type 'imm program = 'imm opcode list
val empty_program : 'imm program

val num_opcodes : 'imm program -> int

val append_inst : 'imm program -> 'imm opcode -> 'imm program

val stack_popped : 'imm opcode -> int
val stack_pushed : 'imm opcode -> int

val string_of_opcode : PseudoImm.pseudo_imm opcode -> string

val encode_program : PseudoImm.pseudo_imm program -> string
val pr_encoded : PseudoImm.pseudo_imm program -> unit

val hex_of_opcode : Big_int.big_int opcode -> Hexa.hex
val hex_of_program : Big_int.big_int program -> Hexa.hex

val encode_program : Big_int.big_int program -> string
val pr_encoded : Big_int.big_int program -> unit

val size_of_opcode : 'exp opcode -> int
val size_of_program : 'exp program -> int

(*
Commented out till we need it.

val string_of_real_opcode :
  Big_int.big_int opcode -> string
val string_of_real_program : Big_int.big_int program -> unit
*)
