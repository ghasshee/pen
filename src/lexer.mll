(* Some code in this file comes from
 * https://github.com/realworldocaml/examples/tree/master/code/parsing-test
 * which is under UNLICENSE
 *)
{
  open Misc
  open Lexing
  open Parser
  exception SyntaxError of string
}

let white   = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let digit   = ['0'-'9']
let id      = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
let comment = "//" (_ # ['\r' '\n'])* newline

rule read = parse
  | white                           {   read lexbuf                             }
  | comment                         {   new_line lexbuf; read lexbuf            }
  | newline                         {   new_line lexbuf; read lexbuf            }
  | "!"                             {   BANG                                    } 
  | ":="                            {   COLONEQ                                 } 
  | "ref"                           {   REF                                     } 
  | "->"                            {   ARROW                                   }
  | "<-"                            {   LARROW                                  } 
  | "fn"                            {   LAM                                     } 
  | "let"                           {   LET                                     }
  | "rec"                           {   REC                                     } 
  | "fix"                           {   FIX                                     } 
  | "in"                            {   IN                                      }
  | "iszero"                        {   ISZERO                                  }
  | "pre_ecdsarecover"              {   ECDSARECOVER                            }
  | "keccak256"                     {   KECCAK                                  }
  | "contract"                      {   CONTRACT                                }
  | "default"                       {   DEFAULT                                 }
  | "method"                        {   METHOD                                  }
  | "call"                          {   CALL                                    } 
  | "abort"                         {   ABORT                                   }
  | "u8"                            {   UINT8                                   }
  | "u256"                          {   UINT256                                 }
  | "bytes32"                       {   BYTES32                                 }
  | "address"                       {   ADDRESS                                 }
  | "bool"                          {   BOOL                                    }
  | "["                             {   LSQBR                                   }
  | "]"                             {   RSQBR                                   }
  | "if"                            {   IF                                      }
  | "else"                          {   ELSE                                    }
  | "true"                          {   TRUE                                    }
  | "false"                         {   FALSE                                   }
  | "then"                          {   THEN                                    }
  | "become"                        {   BECOME                                  }
  | "return"                        {   RETURN                                  }
  | ";"                             {   SEMI                                    }
  | "("                             {   LPAR                                    }
  | ")"                             {   RPAR                                    }
  | "{"                             {   LBRACE                                  }
  | "}"                             {   RBRACE                                  }
  | ","                             {   COMMA                                   }
  | "=="                            {   EQEQ                                    }
  | "!="                            {   NEQ                                     }
  | "<"                             {   LT                                      }
  | ">"                             {   GT                                      }
  | "="                             {   EQ                                      }
  | "new"                           {   NEW                                     }
  | "with"                          {   WITH                                    }
  | "reentrance"                    {   REENTRANCE                              }
  | "selfdestruct"                  {   SELFDESTRUCT                            }
  | "."                             {   DOT                                     }
  | ":"                             {   COLON                                   }
  | "not"                           {   NOT                                     }
  | "msg"                           {   MSG                                     }
  | "value"                         {   VALUE                                   }  (* transferred value (wei) *)
  | "sender"                        {   SENDER                                  }
  | "this"                          {   THIS                                    }
  | "balance"                       {   BALANCE                                 }
  | "now"                           {   NOW                                     }
  | "block"                         {   BLOCK                                   }
  | "unit"                          {   UNIT                                    }
  | "&&"                            {   LAND                                    }
  | "=>"                            {   DARROW                                  }
  | "+"                             {   PLUS                                    }
  | "-"                             {   MINUS                                   }
  | "*"                             {   MULT                                    }
  | "event"                         {   EVENT                                   }
  | "log"                           {   LOG                                     }
  | "visible"                       {   INDEXED                                 }
  | id                              {   ID (lexeme lexbuf)                      }
  | eof                             {   EOF                                     }
  | digit+ as i                     {   NUM (big_of_str i)                      }
  | digit digit? digit? "u8" as i   {   let last = String.length i-2 in
                                        U8(big_of_str(String.sub i 0 last))     } (* uint8 has at most three digits *)
