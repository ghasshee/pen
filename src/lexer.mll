(* Some code in this file comes from
 * https://github.com/realworldocaml/examples/tree/master/code/parsing-test
 * which is under UNLICENSE
 *)
{
  open Lexing
  open Parser
  exception SyntaxError of string
}

let white   = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let digit   = ['0'-'9']
let id      = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let comment = "//" (_ # ['\r' '\n'])* newline

rule read = parse
  | white           { read lexbuf }
  | comment         { new_line lexbuf; read lexbuf }
  | newline         { new_line lexbuf; read lexbuf }
  | "contract"      { CONTRACT                      }
  | "default"       { DEFAULT                       }
  | "method"        { METHOD                        }
  | "abort"         { ABORT                         }
  | "u8"            { UINT8                         }
  | "u256"          { UINT256                       }
  | "bytes32"       { BYTES32                       }
  | "address"       { ADDRESS                       }
  | "bool"          { BOOL                          }
  | "["             { LSQBR                         }
  | "]"             { RSQBR                         }
  | "if"            { IF                            }
  | "else"          { ELSE                          }
  | "true"          { TRUE                          }
  | "false"         { FALSE                         }
  | "then"          { THEN                          }
  | "become"        { BECOME                        }
  | "return"        { RETURN                        }
  | ";"             { SEMI                          }
  | "("             { LPAR                          }
  | ")"             { RPAR                          }
  | "{"             { LBRACE                        }
  | "}"             { RBRACE                        }
  | ","             { COMMA                         }
  | "=="            { EQEQ                          }
  | "!="            { NEQ                           }
  | "<"             { LT                            }
  | ">"             { GT                            }
  | "="             { EQ                            }
  | "new"           { NEW                           }
  | "with"          { ALONG                         }
  | "reentrance"    { REENTRANCE                    }
  | "selfdestruct"  { SELFDESTRUCT                  }
  | "."             { DOT                           }
  | "not"           { NOT                           }
  | "msg"           { MSG                           }
  | "value"         { VALUE                         }  (* transferred value (wei) *)
  | "sender"        { SENDER                        }
  | "this"          { THIS                          }
  | "balance"       { BALANCE                       }
  | "now"           { NOW                           }
  | "block"         { BLOCK                         }
  | "unit"          { UNIT                          }
  | "&&"            { LAND                          }
  | "=>"            { DARROW                        }
  | "+"             { PLUS                          }
  | "-"             { MINUS                         }
  | "*"             { MULT                          }
  | "event"         { EVENT                         }
  | "log"           { LOG                           }
  | "visible"       { INDEXED                       }
  | digit+ as i     { EUINT256 (Big_int.big_int_of_string i) }
  (* uint8 has at most three digits *)
  | digit digit? digit? "u8" as i {
      let last = String.length i - 2 in
      EUINT8 (Big_int.big_int_of_string (String.sub i 0 last)) }
  | id              { ID (lexeme lexbuf) }
  | eof             { EOF           }
