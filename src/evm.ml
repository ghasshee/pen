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
    | PUSH1 of 'imm | PUSH2 of 'imm | PUSH4 of 'imm | PUSH5 of 'imm | PUSH8 of 'imm (*  60s *)
    | PUSH16 of 'imm        | PUSH20 of 'imm        | PUSH32 of 'imm                (*      *)
    | DUP1      | DUP2      | DUP3      | DUP4      | DUP5      | DUP6      | DUP7  (*  80s *)
    | SWAP1     | SWAP2     | SWAP3     | SWAP4     | SWAP5     | SWAP6             (*  90s *)
    | LOG0      | LOG1      | LOG2      | LOG3      | LOG4                          (*  A0s *)
    | CREATE    | CALL      | CALLCODE  | RETURN    | DELEGATECALL                  (*  F0s *) 
    | CREATE2   | STATICCALL| REVERT    | INVALID   | SELFDESTRUCT
    | Comment of str 

type 'imm program       = 'imm opcode list

let empty_prog          = []

let stack_popped = function
  | INVALID                                                         -> 0 
  | PUSH _                                                          -> 0
  | STOP|ADDRESS|ORIGIN|CALLER|CALLVALUE|TIMESTAMP                  -> 0
  | CODESIZE|CALLDATASIZE|PC|MSIZE|GAS|GASPRICE                     -> 0
  | Comment _ | JUMPDEST _                                          -> 0
  | MLOAD|SLOAD|POP|NOT|ISZERO|JUMP                                 -> 1
  | BALANCE|EXTCODESIZE|CALLDATALOAD|SELFDESTRUCT                   -> 1
  | ADD|SUB|MUL|DIV|SDIV|MOD|SMOD|EXP|SHA3|LT|GT|EQ|SHL|SHR         -> 2
  | MSTORE|MSTORE8|SSTORE|RETURN|JUMPI|SIGNEXTEND                   -> 2
  | ADDMOD|MULMOD                                                   -> 3
  | CALLDATACOPY|CODECOPY|CREATE                                    -> 3
  | EXTCODECOPY                                                     -> 4
  | CALL | CALLCODE | DELEGATECALL                                  -> 7
  | DUP1  -> 1
  | DUP2  -> 2   | SWAP1 -> 2   | LOG0  -> 2
  | DUP3  -> 3   | SWAP2 -> 3   | LOG1  -> 3
  | DUP4  -> 4   | SWAP3 -> 4   | LOG2  -> 4
  | DUP5  -> 5   | SWAP4 -> 5   | LOG3  -> 5
  | DUP6  -> 6   | SWAP5 -> 6   | LOG4  -> 6
  | DUP7  -> 7   | SWAP6 -> 7

let stack_pushed = function
  | INVALID                                                         -> 0 
  | STOP | RETURN | SELFDESTRUCT                                    -> 0
  | POP                                                             -> 0
  | CALLDATACOPY|CODECOPY|EXTCODECOPY                               -> 0
  | MSTORE|MSTORE8|SSTORE                                           -> 0
  | JUMP|JUMPI                                                      -> 0
  | JUMPDEST _                                                      -> 0
  | PUSH _                                                          -> 1 
  | ISZERO|NOT|BALANCE|SIGNEXTEND|SHA3                              -> 1
  | ADD|MUL|SUB|DIV|SDIV|EXP|MOD|SMOD|EQ|LT|GT|SHL|SHR              -> 1
  | ADDMOD|MULMOD                                                   -> 1
  | ADDRESS|ORIGIN|CALLER|CALLVALUE|CALLDATALOAD|CALLDATASIZE       -> 1
  | CODESIZE|GASPRICE|EXTCODESIZE|TIMESTAMP|PC|MSIZE|GAS            -> 1
  | MLOAD|SLOAD                                                     -> 1
  | CREATE | CALL | CALLCODE | DELEGATECALL                         -> 1
  | DUP1  -> 2 | SWAP1 -> 2 | LOG0  -> 0 
  | DUP2  -> 3 | SWAP2 -> 3 | LOG1  -> 0 
  | DUP3  -> 4 | SWAP3 -> 4 | LOG2  -> 0 
  | DUP4  -> 5 | SWAP4 -> 5 | LOG3  -> 0 
  | DUP5  -> 6 | SWAP5 -> 6 | LOG4  -> 0
  | DUP6  -> 7 | SWAP6 -> 7
  | DUP7  -> 8
  | Comment _ -> 0

let str_of_opcode str_of_push = function 
  | NOT             -> "NOT"               | SWAP1           -> "SWAP1"
  | TIMESTAMP       -> "TIMESTAMP"         | SWAP2           -> "SWAP2"
  | ISZERO          -> "ISZERO"            | SWAP3           -> "SWAP3"
  | EQ              -> "EQ"                | SWAP4           -> "SWAP5"
  | LT              -> "LT"                | SWAP5           -> "SWAP5"
  | GT              -> "GT"                | SWAP6           -> "SWAP6"
  | BALANCE         -> "BALANCE"           | DUP1            -> "DUP1"
  | STOP            -> "STOP"              | DUP2            -> "DUP2"
  | ADD             -> "ADD"               | DUP3            -> "DUP3"
  | MUL             -> "MUL"               | DUP4            -> "DUP4"
  | SUB             -> "SUB"               | DUP5            -> "DUP5"
  | DIV             -> "DIV"               | DUP6            -> "DUP6"
  | SDIV            -> "SDIV"              | DUP7            -> "DUP7"
  | EXP             -> "EXP"               | LOG0            -> "LOG0"
  | MOD             -> "MOD"               | LOG1            -> "LOG1"
  | SMOD            -> "SMOD"              | LOG2            -> "LOG2"
  | ADDMOD          -> "ADDMOD"            | LOG3            -> "LOG3"
  | MULMOD          -> "MULMOD"            | LOG4            -> "LOG4"
  | SIGNEXTEND      -> "SIGNEXTEND"        | CREATE          -> "CREATE"
  | SHA3            -> "SHA3"              | CALL            -> "CALL"
  | ADDRESS         -> "ADDRESS"           | ORIGIN          -> "ORIGIN"        
  | POP             -> "POP"               | CALLER          -> "CALLER"      
  | MLOAD           -> "MLOAD"             | CALLVALUE       -> "CALLVALUE"   
  | MSTORE          -> "MSTORE"            | CALLDATALOAD    -> "CALLDATALOAD"
  | MSTORE8         -> "MSTORE8"           | CALLDATASIZE    -> "CALLDATASIZE"
  | SLOAD           -> "SLOAD"             | CALLDATACOPY    -> "CALLDATACOPY"
  | SSTORE          -> "SSTORE"            | CODESIZE        -> "CODESIZE"
  | JUMP            -> "JUMP"              | CODECOPY        -> "CODECOPY"
  | JUMPI           -> "JUMPI"             | GASPRICE        -> "GASPRICE"
  | PC              -> "PC"                | EXTCODESIZE     -> "EXTCODESIZE"
  | MSIZE           -> "MSIZE"             | EXTCODECOPY     -> "EXTCODECOPY"
  | GAS             -> "GAS"               | CALLCODE        -> "CALLCODE"
  | SHR             -> "SHR"               | RETURN          -> "RETURN"
  | SHL             -> "SHL"               | DELEGATECALL    -> "DELEGATECALL"
  | Comment s       -> "// " ^ s           | SELFDESTRUCT    -> "SELFDESTRUCT"
  | JUMPDEST l      -> sprintf "JUMPDEST (label %x)" (Label.lookup_label l)
  | opcode          -> str_of_push opcode


  

let str_of_push_big = function
  | PUSH1  v        -> "PUSH1 " ^ str_of_hex (hex_of_big v  1)
  | PUSH2  v        -> "PUSH2 " ^ str_of_hex (hex_of_big v  2)
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

let str_of_opcodes_big p    = S.concat""(L.map(fun op->str_of_opcode_big op^"\n")(rev p))
let pr_opcodes_big        p = pf "%s"(str_of_opcodes_big p) 

let hex_of_opcode = 
  let h = hex_of_str in function 
  | PUSH1 i         -> concat_hex (h "60") (hex_of_big i  1)
  | PUSH2 i         -> concat_hex (h "61") (hex_of_big i  2)
  | PUSH4 i         -> concat_hex (h "63") (hex_of_big i  4)
  | PUSH5 i         -> concat_hex (h "64") (hex_of_big i  5)
  | PUSH8 i         -> concat_hex (h "67") (hex_of_big i  8)
  | PUSH16 i        -> concat_hex (h "6f") (hex_of_big i 16)
  | PUSH20 i        -> concat_hex (h "73") (hex_of_big i 20)
  | PUSH32 i        -> concat_hex (h "7f") (hex_of_big i 32)
  | STOP            -> h "00"    | POP             -> h "50"
  | ADD             -> h "01"    | MLOAD           -> h "51"
  | MUL             -> h "02"    | MSTORE          -> h "52"
  | SUB             -> h "03"    | MSTORE8         -> h "53"
  | DIV             -> h "04"    | SLOAD           -> h "54"
  | SDIV            -> h "05"    | SSTORE          -> h "55"
  | MOD             -> h "06"    | JUMP            -> h "56"
  | SMOD            -> h "07"    | JUMPI           -> h "57"
  | ADDMOD          -> h "08"    | PC              -> h "58"
  | MULMOD          -> h "09"    | MSIZE           -> h "59"
  | EXP             -> h "0a"    | GAS             -> h "5a"
  | SIGNEXTEND      -> h "0b"    | JUMPDEST _      -> h "5b"
  | LT              -> h "10"    | DUP1            -> h "80"
  | GT              -> h "11"    | DUP2            -> h "81"
  | EQ              -> h "14"    | DUP3            -> h "82"
  | ISZERO          -> h "15"    | DUP4            -> h "83"
  | NOT             -> h "19"    | DUP5            -> h "84"
  | SHL             -> h "1b"    | DUP6            -> h "85"
  | SHR             -> h "1c"    | DUP7            -> h "86"
  | SHA3            -> h "20"    | SWAP1           -> h "90"
  | ADDRESS         -> h "30"    | SWAP2           -> h "91"
  | BALANCE         -> h "31"    | SWAP3           -> h "92"
  | ORIGIN          -> h "32"    | SWAP4           -> h "93"
  | CALLER          -> h "33"    | SWAP5           -> h "94"
  | CALLVALUE       -> h "34"    | SWAP6           -> h "95"
  | CALLDATALOAD    -> h "35"    | LOG0            -> h "a0"
  | CALLDATASIZE    -> h "36"    | LOG1            -> h "a1"
  | CALLDATACOPY    -> h "37"    | LOG2            -> h "a2"
  | CODESIZE        -> h "38"    | LOG3            -> h "a3"
  | CODECOPY        -> h "39"    | LOG4            -> h "a4"
  | GASPRICE        -> h "3a"    | CREATE          -> h "f0"
  | EXTCODESIZE     -> h "3b"    | CALL            -> h "f1"
  | EXTCODECOPY     -> h "3c"    | CALLCODE        -> h "f2"
  | TIMESTAMP       -> h "42"    | RETURN          -> h "f3"
  | Comment s       -> h ""      | DELEGATECALL    -> h "f4"
  | INVALID         -> h "fe"    | SELFDESTRUCT    -> h "ff"
  
  
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
  | PUSH2 _         -> 1 + 2
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
