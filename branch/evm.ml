open Hexa 
open Big_int
open Misc

type 'imm opcode = 
    | STOP      | ADD       | MUL       | SUB       | DIV       | SDIV              (*   0s *)
    | MOD       | SMOD      | ADDMOD    | MULMOD    | EXP       | SIGNEXTEND
    | LT        | GT        | SLT       | SGT       | EQ        | ISZERO            (*  10s *) 
    | AND       | OR        | XOR       | NOT       | BYTE
    | SHA3                                                                          (*  20s *)
    | ADDRESS   | BALANCE   | ORIGIN    | CALLER                                    (*  30s *) 
    | CALLVALUE | CALLDATALOAD          | CALLDATASIZE    | CALLDATACOPY
    | CODESIZE  | CODECOPY  | GASPRICE  | EXTCODESIZE           | EXTCODECOPY           
    | RETURNDATASIZE        | RETUENDATACOPY
    | BLOCKHASH | COINBASE  | TIMESTAMP | NUMBER    | DIFFICULTY| GASLIMIT          (*  40s *)
    | POP       | MLOAD     | MSTORE    | MSTORE8   | SLOAD     | SSTORE            (*  50s *)
    | JUMP      | JUMPI     | PC        | MSIZE     | GAS       | JUMPDEST of Label.label
    | PUSH1 of 'imm         | PUSH4 of 'imm         | PUSH32 of 'imm                (*  60s *)
    | DUP1      | DUP2      | DUP3      | DUP4      | DUP5      | DUP6      | DUP7  (*  80s *)
    | SWAP1     | SWAP2     | SWAP3     | SWAP4     | SWAP5     | SWAP6             (*  90s *)
    | LOG0      | LOG1      | LOG2      | LOG3      | LOG4                          (*  A0s *)
    | CREATE    | CALL      | CALLCODE  | RETURN    | DELEGATECALL                  (*  F0s *) 
    | CREATE2   | STATICCALL| REVERT    | INVALID   | SELFDESTRUCT

type 'imm program       = 'imm opcode list

let num_opcodes         = List.length

let empty_program       = []

(** The program is stored in the reverse order *)
(* let append_inst orig i = i :: orig *) 

let to_list (p : 'imm program) =
  List.rev p

let stack_popped = function
  | PUSH1 _|PUSH4 _|PUSH32 _            -> 0
  | NOT                                 -> 1
  | TIMESTAMP                           -> 0
  | ISZERO                              -> 1
  | LT|GT|EQ                            -> 2
  | BALANCE                             -> 1
  | STOP                                -> 0
  | ADD|SUB|MUL|DIV|SDIV|MOD|SMOD|EXP   -> 2 
  | ADDMOD|MULMOD                       -> 3
  | SIGNEXTEND                          -> 2
  | SHA3                                -> 2
  | ADDRESS|ORIGIN|CALLER|CALLVALUE     -> 0
  | CALLDATALOAD                        -> 1
  | CALLDATASIZE                        -> 0
  | CALLDATACOPY                        -> 3
  | CODESIZE                            -> 0
  | CODECOPY                            -> 3
  | EXTCODESIZE                         -> 1
  | EXTCODECOPY                         -> 4
  | POP                                 -> 1
  | MLOAD                               -> 1
  | MSTORE|MSTORE8                      -> 2
  | SLOAD                               -> 1
  | SSTORE                              -> 2
  | JUMP                                -> 1
  | JUMPI                               -> 2
  | PC|MSIZE|GAS|GASPRICE               -> 0
  | JUMPDEST _                          -> 0
  | SWAP1                               -> 2
  | SWAP2                               -> 3
  | SWAP3                               -> 4
  | SWAP4                               -> 5
  | SWAP5                               -> 6
  | SWAP6                               -> 7
  | LOG0                                -> 2
  | LOG1                                -> 3
  | LOG2                                -> 4
  | LOG3                                -> 5
  | LOG4                                -> 6
  | CREATE                              -> 3
  | CALL                                -> 7
  | CALLCODE                            -> 7
  | RETURN                              -> 2
  | DELEGATECALL                        -> 7
  | SELFDESTRUCT                        -> 1
  | DUP1                                -> 1
  | DUP2                                -> 2
  | DUP3                                -> 3
  | DUP4                                -> 4
  | DUP5                                -> 5
  | DUP6                                -> 6
  | DUP7                                -> 7


let stack_pushed = function
  | PUSH1 _|PUSH4 _|PUSH32 _            -> 1
  | TIMESTAMP                           -> 1
  | ISZERO                              -> 1
  | EQ | LT | GT                        -> 1
  | NOT                                 -> 1
  | BALANCE                             -> 1
  | STOP                                -> 0
  | ADD|MUL|SUB|DIV|SDIV|EXP|MOD|SMOD   -> 1
  | ADDMOD|MULMOD                       -> 1
  | SIGNEXTEND                          -> 1
  | SHA3                                -> 1
  | ADDRESS                             -> 1
  | ORIGIN                              -> 1
  | CALLER                              -> 1
  | CALLVALUE                           -> 1
  | CALLDATALOAD                        -> 1
  | CALLDATASIZE                        -> 1
  | CALLDATACOPY                        -> 0
  | CODESIZE                            -> 1
  | CODECOPY                            -> 0
  | GASPRICE                            -> 1
  | EXTCODESIZE                         -> 1
  | EXTCODECOPY                         -> 0
  | POP                                 -> 0
  | MLOAD                               -> 1
  | MSTORE                              -> 0
  | MSTORE8                             -> 0
  | SLOAD                               -> 1
  | SSTORE                              -> 0
  | JUMP|JUMPI                          -> 0
  | PC|MSIZE|GAS                        -> 1
  | JUMPDEST _                          -> 0
  | SWAP1                               -> 2
  | SWAP2                               -> 3
  | SWAP3                               -> 4
  | SWAP4                               -> 5
  | SWAP5                               -> 6
  | SWAP6                               -> 7
  | DUP1                                -> 2
  | DUP2                                -> 3
  | DUP3                                -> 4
  | DUP4                                -> 5
  | DUP5                                -> 6
  | DUP6                                -> 7
  | DUP7                                -> 8
  | LOG0|LOG1|LOG2|LOG3|LOG4            -> 0
  | CREATE                              -> 1
  | CALL                                -> 1
  | CALLCODE                            -> 1
  | RETURN                              -> 0
  | DELEGATECALL                        -> 1
  | SELFDESTRUCT                        -> 0

let string_of_opcode string_of_push = function 
  | NOT             -> "NOT"
  | TIMESTAMP       -> "TIMESTAMP"
  | ISZERO          -> "ISZERO"
  | EQ              -> "EQ"
  | LT              -> "LT"
  | GT              -> "GT"
  | BALANCE         -> "BALANCE"
  | STOP            -> "STOP"
  | ADD             -> "ADD"
  | MUL             -> "MUL"
  | SUB             -> "SUB"
  | DIV             -> "DIV"
  | SDIV            -> "SDIV"
  | EXP             -> "EXP"
  | MOD             -> "MOD"
  | SMOD            -> "SMOD"
  | ADDMOD          -> "ADDMOD"
  | MULMOD          -> "MULMOD"
  | SIGNEXTEND      -> "SIGNEXTEND"
  | SHA3            -> "SHA3"
  | ADDRESS         -> "ADDRESS"
  | ORIGIN          -> "ORIGIN"
  | CALLER          -> "CALLER"
  | CALLVALUE       -> "CALLVALUE"
  | CALLDATALOAD    -> "CALLDATALOAD"
  | CALLDATASIZE    -> "CALLDATASIZE"
  | CALLDATACOPY    -> "CALLDATACOPY"
  | CODESIZE        -> "CODESIZE"
  | CODECOPY        -> "CODECOPY"
  | GASPRICE        -> "GASPRICE"
  | EXTCODESIZE     -> "EXTCODESIZE"
  | EXTCODECOPY     -> "EXTCODECOPY"
  | POP             -> "POP"
  | MLOAD           -> "MLOAD"
  | MSTORE          -> "MSTORE"
  | MSTORE8         -> "MSTORE8"
  | SLOAD           -> "SLOAD"
  | SSTORE          -> "SSTORE"
  | JUMP            -> "JUMP"
  | JUMPI           -> "JUMPI"
  | PC              -> "PC"
  | MSIZE           -> "MSIZE"
  | GAS             -> "GAS"
  | JUMPDEST l      -> "JUMPDEST (print label)"
  | SWAP1           -> "SWAP1"
  | SWAP2           -> "SWAP2"
  | SWAP3           -> "SWAP3"
  | SWAP4           -> "SWAP5"
  | SWAP5           -> "SWAP5"
  | SWAP6           -> "SWAP6"
  | DUP1            -> "DUP1"
  | DUP2            -> "DUP2"
  | DUP3            -> "DUP3"
  | DUP4            -> "DUP4"
  | DUP5            -> "DUP5"
  | DUP6            -> "DUP6"
  | DUP7            -> "DUP7"
  | LOG0            -> "LOG0"
  | LOG1            -> "LOG1"
  | LOG2            -> "LOG2"
  | LOG3            -> "LOG3"
  | LOG4            -> "LOG4"
  | CREATE          -> "CREATE"
  | CALL            -> "CALL"
  | CALLCODE        -> "CALLCODE"
  | RETURN          -> "RETURN"
  | DELEGATECALL    -> "DELEGATECALL"
  | SELFDESTRUCT    -> "SELFDESTRUCT"
  | opcode          -> string_of_push opcode

let string_of_push_big = function
  | PUSH1 v         -> "PUSH1 " ^(string_of_hex (hex_of_big_int v  1))(*(Location.string_of_imm v)*)
  | PUSH4 v         -> "PUSH4 " ^(string_of_hex (hex_of_big_int v  4))(*(Location.string_of_imm v)*)
  | PUSH32 v        -> "PUSH32 "^(string_of_hex (hex_of_big_int v 32))(*(Location.string_of_imm v)*)

let string_of_opcode_big = string_of_opcode string_of_push_big

let string_of_push_imm = function
  | PUSH1 v         -> "PUSH1 " ^ Location.string_of_imm v
  | PUSH4 v         -> "PUSH4 " ^ Location.string_of_imm v
  | PUSH32 v        -> "PUSH32 "^ Location.string_of_imm v

let string_of_opcode_imm = string_of_opcode string_of_push_imm



let string_of_opcodes_big p = String.concat""  (List.map(fun op->string_of_opcode_big op^"\n")(to_list p))
let pr_opcodes_big        p = Printf.printf"%s"(string_of_opcodes_big p) 

let hex_of_opcode = 
  let h = hex_of_string in function 
  | PUSH1 i         -> concat_hex (h "60") (hex_of_big_int i  1)
  | PUSH4 i         -> concat_hex (h "63") (hex_of_big_int i  4)
  | PUSH32 i        -> concat_hex (h "7f") (hex_of_big_int i 32)
  | NOT             -> h "19"
  | TIMESTAMP       -> h "42"
  | EQ              -> h "14"
  | ISZERO          -> h "15"
  | LT              -> h "10"
  | GT              -> h "11"
  | BALANCE         -> h "31"
  | STOP            -> h "00"
  | ADD             -> h "01"
  | MUL             -> h "02"
  | SUB             -> h "03"
  | DIV             -> h "04"
  | SDIV            -> h "05"
  | MOD             -> h "06"
  | SMOD            -> h "07"
  | ADDMOD          -> h "08"
  | MULMOD          -> h "09"
  | EXP             -> h "0a"
  | SIGNEXTEND      -> h "0b"
  | SHA3            -> h "20"
  | ADDRESS         -> h "30"
  | ORIGIN          -> h "32"
  | CALLER          -> h "33"
  | CALLVALUE       -> h "34"
  | CALLDATALOAD    -> h "35"
  | CALLDATASIZE    -> h "36"
  | CALLDATACOPY    -> h "37"
  | CODESIZE        -> h "38"
  | CODECOPY        -> h "39"
  | GASPRICE        -> h "3a"
  | EXTCODESIZE     -> h "3b"
  | EXTCODECOPY     -> h "3c"
  | POP             -> h "50"
  | MLOAD           -> h "51"
  | MSTORE          -> h "52"
  | MSTORE8         -> h "53"
  | SLOAD           -> h "54"
  | SSTORE          -> h "55"
  | JUMP            -> h "56"
  | JUMPI           -> h "57"
  | PC              -> h "58"
  | MSIZE           -> h "59"
  | GAS             -> h "5a"
  | JUMPDEST _      -> h "5b"
  | DUP1            -> h "80"
  | DUP2            -> h "81"
  | DUP3            -> h "82"
  | DUP4            -> h "83"
  | DUP5            -> h "84"
  | DUP6            -> h "85"
  | DUP7            -> h "86"
  | SWAP1           -> h "90"
  | SWAP2           -> h "91"
  | SWAP3           -> h "92"
  | SWAP4           -> h "93"
  | SWAP5           -> h "94"
  | SWAP6           -> h "95"
  | LOG0            -> h "a0"
  | LOG1            -> h "a1"
  | LOG2            -> h "a2"
  | LOG3            -> h "a3"
  | LOG4            -> h "a4"
  | CREATE          -> h "f0"
  | CALL            -> h "f1"
  | CALLCODE        -> h "f2"
  | RETURN          -> h "f3"
  | DELEGATECALL    -> h "f4"
  | SELFDESTRUCT    -> h "ff"

let log = function 
  | 0               -> LOG0
  | 1               -> LOG1
  | 2               -> LOG2
  | 3               -> LOG3
  | 4               -> LOG4
  | _               -> failwith "too many indexed args for an evnt"

let endline h = hex_of_string ( string_of_hex h ^ "\n")
(*let rev_append_op (h:hex)(i:big_int opcode) = concat_hex (hex_of_opcode i) h *)
let hex_of_program      (p : big program) = foldl (fun h i->concat_hex(hex_of_opcode          i )h) empty_hex p
let hex_of_program_ln   (p : big program) = foldl (fun h i->concat_hex(endline (hex_of_opcode i))h) empty_hex p
let string_of_program_ln(p : big program) = foldl (fun h i-> hex_of_string ((^) (string_of_opcode_big i^"\n")(string_of_hex h))) empty_hex p 
let pr_encoded          (p : big program) = pr_hex        ~prefix:"0x" (hex_of_program p) 
let prLn_encoded        (p : big program) = pr_hex        ~prefix:"0x" (string_of_program_ln p) 
let encode_program      (p : big program) = string_of_hex ~prefix:"0x" (hex_of_program p) 

let size_of_opcode  = function 
  | PUSH1 _         -> 2
  | PUSH4 _         -> 5
  | PUSH32 _        -> 33
  | _               -> 1

let size_of_program p = List.fold_left (fun a i -> a + size_of_opcode i) 0 p 

let dup_succ       = function 
  | 0               -> DUP1
  | 1               -> DUP2
  | 2               -> DUP3
  | 3               -> DUP4
  | 4               -> DUP5
  | 5               -> DUP6
  | 6               -> DUP7
  | n               -> print_int n; failwith "more DUP opcodes needed"
