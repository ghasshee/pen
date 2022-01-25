open Printf 
open Big_int
open Misc

module L = List
module S = String 

type 'imm opcode = 
    | STOP      | ADD       | MUL       | SUB       | DIV       | SDIV              (*   0s *)
    | MOD       | SMOD      | ADDMOD    | MULMOD    | EXP       | SIGNEXTEND
    | LT        | GT        | SLT       | SGT       | EQ        | ISZERO            (*  10s *) 
    | AND       | OR        | XOR       | NOT       | BYTE
    | SHR       | SHL
    | SHA3                                                                          (*  20s *)
    | ADDRESS   | BALANCE   | ORIGIN    | CALLER                                    (*  30s *) 
    | CALLVALUE | CALLDATALOAD          | CALLDATASIZE          | CALLDATACOPY
    | CODESIZE  | CODECOPY  | GASPRICE  | EXTCODESIZE           | EXTCODECOPY           
    | RETURNDATASIZE        | RETUENDATACOPY
    | BLOCKHASH | COINBASE  | TIMESTAMP | NUMBER    | DIFFICULTY| GASLIMIT          (*  40s *)
    | POP       | MLOAD     | MSTORE    | MSTORE8   | SLOAD     | SSTORE            (*  50s *)
    | JUMP      | JUMPI     | PC        | MSIZE     | GAS       | JUMPDEST of Label.label
    | PUSH  of 'imm
    | PUSH1 of 'imm   | PUSH4 of 'imm   | PUSH5 of 'imm         | PUSH8 of 'imm     (*  60s *)
    | PUSH16 of 'imm        | PUSH20 of 'imm        | PUSH32 of 'imm                (*      *)
    | DUP1      | DUP2      | DUP3      | DUP4      | DUP5      | DUP6      | DUP7  (*  80s *)
    | SWAP1     | SWAP2     | SWAP3     | SWAP4     | SWAP5     | SWAP6             (*  90s *)
    | LOG0      | LOG1      | LOG2      | LOG3      | LOG4                          (*  A0s *)
    | CREATE    | CALL      | CALLCODE  | RETURN    | DELEGATECALL                  (*  F0s *) 
    | CREATE2   | STATICCALL| REVERT    | INVALID   | SELFDESTRUCT
    | Comment of str 

type 'imm program       = 'imm opcode list

let num_opcodes         = L.length

let empty_program       = []

(** The program is stored in the reverse order *)
(* let append_inst orig i = i :: orig *) 

let to_list (p : 'imm program) = L.rev p

let stack_popped = function
  | PUSH _                                                          -> 0
  | NOT | ISZERO                                                    -> 1
  | ADDMOD|MULMOD                                                   -> 3
  | ADD|SUB|MUL|DIV|SDIV|MOD|SMOD|EXP|SHA3|LT|GT|EQ|SHL|SHR         -> 2
  | STOP|ADDRESS|ORIGIN|CALLER|CALLVALUE|TIMESTAMP                  -> 0
  | CODESIZE|CALLDATASIZE                                           -> 0
  | PC|MSIZE|GAS|GASPRICE                                           -> 0
  | JUMPDEST _                                                      -> 0
  | SIGNEXTEND                                                      -> 2
  | BALANCE                                                         -> 1
  | CALLDATALOAD                                                    -> 1
  | CALLDATACOPY | CODECOPY                                         -> 3
  | EXTCODESIZE                                                     -> 1
  | EXTCODECOPY                                                     -> 4
  | MSTORE|MSTORE8|SSTORE|RETURN                                    -> 2
  | MLOAD|SLOAD|POP                                                 -> 1
  | JUMP                                                            -> 1
  | JUMPI                                                           -> 2
  | SWAP1                                                           -> 2
  | SWAP2                                                           -> 3
  | SWAP3                                                           -> 4
  | SWAP4                                                           -> 5
  | SWAP5                                                           -> 6
  | SWAP6                                                           -> 7
  | LOG0                                                            -> 2
  | LOG1                                                            -> 3
  | LOG2                                                            -> 4
  | LOG3                                                            -> 5
  | LOG4                                                            -> 6
  | CREATE                                                          -> 3
  | CALL | CALLCODE | DELEGATECALL                                  -> 7
  | SELFDESTRUCT                                                    -> 1
  | DUP1                                                            -> 1
  | DUP2                                                            -> 2
  | DUP3                                                            -> 3
  | DUP4                                                            -> 4
  | DUP5                                                            -> 5
  | DUP6                                                            -> 6
  | DUP7                                                            -> 7
  | Comment _ -> 0 

let stack_pushed = function
  | PUSH _                                                          -> 1 
  | TIMESTAMP                                                       -> 1
  | ISZERO                                                          -> 1
  | ADD|MUL|SUB|DIV|SDIV|EXP|MOD|SMOD|EQ|LT|GT|SHL|SHR              -> 1
  | ADDMOD|MULMOD                                                   -> 1
  | NOT                                                             -> 1
  | BALANCE                                                         -> 1
  | STOP                                                            -> 0
  | SIGNEXTEND                                                      -> 1
  | SHA3                                                            -> 1
  | ADDRESS                                                         -> 1
  | ORIGIN                                                          -> 1
  | CALLER                                                          -> 1
  | CALLVALUE| CALLDATALOAD| CALLDATASIZE                           -> 1
  | CALLDATACOPY                                                    -> 0
  | CODESIZE                                                        -> 1
  | CODECOPY                                                        -> 0
  | GASPRICE                                                        -> 1
  | EXTCODESIZE                                                     -> 1
  | EXTCODECOPY                                                     -> 0
  | POP                                                             -> 0
  | MLOAD                                                           -> 1
  | MSTORE                                                          -> 0
  | MSTORE8                                                         -> 0
  | SLOAD                                                           -> 1
  | SSTORE                                                          -> 0
  | JUMP|JUMPI                                                      -> 0
  | PC|MSIZE|GAS                                                    -> 1
  | JUMPDEST _                                                      -> 0
  | SWAP1                                                           -> 2
  | SWAP2                                                           -> 3
  | SWAP3                                                           -> 4
  | SWAP4                                                           -> 5
  | SWAP5                                                           -> 6
  | SWAP6                                                           -> 7
  | DUP1                                                            -> 2
  | DUP2                                                            -> 3
  | DUP3                                                            -> 4
  | DUP4                                                            -> 5
  | DUP5                                                            -> 6
  | DUP6                                                            -> 7
  | DUP7                                                            -> 8
  | LOG0|LOG1|LOG2|LOG3|LOG4                                        -> 0
  | CREATE                                                          -> 1
  | CALL                                                            -> 1
  | CALLCODE                                                        -> 1
  | RETURN                                                          -> 0
  | DELEGATECALL                                                    -> 1
  | SELFDESTRUCT                                                    -> 0
  | Comment _ -> 0

let str_of_opcode str_of_push = function 
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
  | JUMPDEST l      -> sprintf "JUMPDEST (label %x)" (Label.lookup_label l)
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
  | SHR             -> "SHR"
  | SHL             -> "SHL"
  | Comment s       -> "// " ^ s   
  | opcode          -> str_of_push opcode

let str_of_push_big = function
  | PUSH1  v        -> "PUSH1 " ^ str_of_hex (hex_of_big v  1)
  | PUSH4  v        -> "PUSH4 " ^ str_of_hex (hex_of_big v  4)
  | PUSH5  v        -> "PUSH5 " ^ str_of_hex (hex_of_big v  5)
  | PUSH8  v        -> "PUSH8 " ^ str_of_hex (hex_of_big v  8)
  | PUSH16 v        -> "PUSH16 "^ str_of_hex (hex_of_big v 16)
  | PUSH20 v        -> "PUSH20 "^ str_of_hex (hex_of_big v 20)
  | PUSH32 v        -> "PUSH32 "^ str_of_hex (hex_of_big v 32)

let str_of_opcode_big = str_of_opcode str_of_push_big

(*
let str_of_push_imm = function
  | PUSH imm        -> "PUSH " ^ Location.str_of_imm imm
let str_of_opcode_imm = str_of_opcode str_of_push_imm
*)

let str_of_opcodes_big p    = S.concat""(L.map(fun op->str_of_opcode_big op^"\n")(to_list p))
let pr_opcodes_big        p = pf "%s"(str_of_opcodes_big p) 

let hex_of_opcode = 
  let h = hex_of_str in function 
  | PUSH1 i         -> concat_hex (h "60") (hex_of_big i  1)
  | PUSH4 i         -> concat_hex (h "63") (hex_of_big i  4)
  | PUSH5 i         -> concat_hex (h "64") (hex_of_big i  5)
  | PUSH8 i         -> concat_hex (h "67") (hex_of_big i  8)
  | PUSH16 i        -> concat_hex (h "6f") (hex_of_big i 16)
  | PUSH20 i        -> concat_hex (h "73") (hex_of_big i 20)
  | PUSH32 i        -> concat_hex (h "7f") (hex_of_big i 32)
  | SHL             -> h "1b"
  | SHR             -> h "1c"
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
  | Comment s       -> h "" 

let log = function 
  | 0               -> LOG0
  | 1               -> LOG1
  | 2               -> LOG2
  | 3               -> LOG3
  | 4               -> LOG4
  | _               -> failwith "too many indexed args for an evnt"

let endline h               = hex_of_str ( str_of_hex h ^ "\n")
let hex_of_program       p  = foldl (fun h i->concat_hex(hex_of_opcode          i )h) empty_hex p
let hex_of_program_ln    p  = foldl (fun h i->concat_hex(endline (hex_of_opcode i))h) empty_hex p
let str_of_program_ln    p  = foldl (fun h i->hex_of_str(str_of_opcode_big i^"\n"^str_of_hex h)) empty_hex p 
let pr_encoded           p  = pr_hex        ~prefix:"0x" (hex_of_program p) 
let prLn_encoded         p  = pr_hex        ~prefix:"0x" (str_of_program_ln p) 
let encode_program       p  = str_of_hex ~prefix:"0x" (hex_of_program p) 

let size_of_opcode  = function 
  | Comment _       -> 0 
  | PUSH1 _         -> 1 + 1 
  | PUSH4 _         -> 1 + 4 
  | PUSH5 _         -> 1 + 5 
  | PUSH8 _         -> 1 + 8 
  | PUSH16 _        -> 1 + 16 
  | PUSH20 _        -> 1 + 20 
  | PUSH32 _        -> 1 + 32 
  | PUSH _          -> 1 + 16 (* err "size_of_opcode: abstract PUSH opcode CANNOT DETERMINE THE SIZE" *)
  | _               -> 1

let size_of_prog p = foldl (fun a i -> size_of_opcode i + a) 0 p 

let dup_succ       = function 
  | 0               -> DUP1
  | 1               -> DUP2
  | 2               -> DUP3
  | 3               -> DUP4
  | 4               -> DUP5
  | 5               -> DUP6
  | 6               -> DUP7
  | n               -> ef "EVM: DUP%d does not exists\n" (n+1); err ""
