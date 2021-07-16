{
open Support

let reservedWords = [
  (* Keywords *)
    ("Top",     fun i -> Parser.TOP i);
    ("ref",     fun i -> Parser.REF i);
    ("Ref",     fun i -> Parser.REFTYPE i);
    (*
    ("List",    fun i -> Parser.LIST i);
    ("tail",    fun i -> Parser.TAIL i);
    ("head",    fun i -> Parser.HEAD i);
    ("isnil",   fun i -> Parser.ISNIL i);
    ("cons",    fun i -> Parser.CONS i);
    ("nil",     fun i -> Parser.NIL i);
    *)
    ("letrec",  fun i -> Parser.LETREC i);
    ("fix",     fun i -> Parser.FIX i);
    ("Float",   fun i -> Parser.FLOAT i);
    ("*.",      fun i -> Parser.TIMESFLOAT i);
    ("String",  fun i -> Parser.STRING i);
    ("case",    fun i -> Parser.CASE i);
    ("of",      fun i -> Parser.OF i);
    ("as",      fun i -> Parser.AS i);
    ("unit",    fun i -> Parser.UNIT i);
    ("Unit",    fun i -> Parser.UNITTYPE i);
    ("where",   fun i -> Parser.WHERE i);
    ("in",      fun i -> Parser.IN i);
    ("let",     fun i -> Parser.LET i);
    ("Bool",    fun i -> Parser.BOOL i);
    ("Nat",     fun i -> Parser.NAT i);
    ("\\",      fun i -> Parser.LAMBDA i);
    ("if",      fun i -> Parser.IF i);
    ("then",    fun i -> Parser.THEN i);
    ("else",    fun i -> Parser.ELSE i);
    ("true",    fun i -> Parser.TRUE i);
    ("false",   fun i -> Parser.FALSE i);
    ("succ",    fun i -> Parser.SUCC i);
    ("pred",    fun i -> Parser.PRED i);
    ("iszero",  fun i -> Parser.ISZERO i);
  
  (* Symbols *)
    ("_",       fun i -> Parser.USCORE i);
    ("'",       fun i -> Parser.APOSTROPHE i);
    ("\"",      fun i -> Parser.DQUOTE i);
    ("!",       fun i -> Parser.BANG i);
    ("#",       fun i -> Parser.HASH i);
    ("$",       fun i -> Parser.TRIANGLE i);
    ("*",       fun i -> Parser.STAR i);
    ("|",       fun i -> Parser.VBAR i);
    (".",       fun i -> Parser.DOT i);
    (";",       fun i -> Parser.SEMI i);
    (",",       fun i -> Parser.COMMA i);
    ("/",       fun i -> Parser.SLASH i);
    (":",       fun i -> Parser.COLON i);
    ("::",      fun i -> Parser.COLONCOLON i);
    ("=",       fun i -> Parser.EQ i);
    ("==",      fun i -> Parser.EQEQ i);
    ("[",       fun i -> Parser.LSQUARE i); 
    ("<",       fun i -> Parser.LT i);
    ("{",       fun i -> Parser.LCURLY i); 
    ("(",       fun i -> Parser.LPAREN i); 
    ("<-",      fun i -> Parser.LEFTARROW i); 
    ("{|",      fun i -> Parser.LCURLYBAR i); 
    ("[|",      fun i -> Parser.LSQUAREBAR i); 
    ("}",       fun i -> Parser.RCURLY i);
    (")",       fun i -> Parser.RPAREN i);
    ("]",       fun i -> Parser.RSQUARE i);
    (">",       fun i -> Parser.GT i);
    ("|}",      fun i -> Parser.BARRCURLY i);
    ("|>",      fun i -> Parser.BARGT i);
    ("|]",      fun i -> Parser.BARRSQUARE i);
    ("\n",      fun i -> Parser.NEWLINE i); 
    (";;",      fun i -> Parser.DOUBLESEMI i); 

  (* Special compound symbols: *)
    (":=",      fun i -> Parser.COLONEQ i);
    ("->",      fun i -> Parser.ARROW i);
    ("=>",      fun i -> Parser.DARROW i);
    ("==>",     fun i -> Parser.DDARROW i);
]

(* Support functions *)

type buildfun               =   info -> Parser.token
type hoge                   =   (string,buildfun) Hashtbl.t
let  symbolTable:hoge       =   Hashtbl.create 1024
let _                       =   List.iter (fun(str,f)->Hashtbl.add symbolTable str f) reservedWords
let fos                     =   float_of_string
let ios                     =   int_of_string 
let initCapital str         =   let s=String.get str 0 in s>='A'&&s<='Z'  

let createID i str          =   (* info -> string -> token *)
  try   Hashtbl.find symbolTable str i
  with _ -> if initCapital str then Parser.UCID {i=i;v=str} else Parser.LCID {i=i;v=str}

let lineno                  =   ref 1
and depth                   =   ref 0
and start                   =   ref 0
and filename                =   ref ""
and startLex                =   ref dummyinfo
let create inFile stream    =   if not(Filename.is_implicit inFile) 
                                    then filename   := inFile
                                    else filename   := Filename.concat (Sys.getcwd()) inFile;
                                lineno := 1; start := 0; Lexing.from_channel stream
let newline lexbuf          =   incr lineno; start := (Lexing.lexeme_start lexbuf)
let info    lexbuf          =   createInfo (!filename) (!lineno) (Lexing.lexeme_start lexbuf - !start)
let text                    =   Lexing.lexeme
let stringBuffer            =   ref (String.create 2048)
let stringEnd               =   ref 0
let resetStr ()             =   stringEnd := 0
let addStr ch               =
    let x                       =   !stringEnd in
    let buffer                  =   !stringBuffer in
    if x=String.length buffer 
    then begin
        let newBuffer   = String.create (x*2) in
        String.blit buffer 0 newBuffer 0 x;
        String.set newBuffer x ch;
        stringBuffer    := newBuffer;
        stringEnd       := x+1
    end else begin
        String.set buffer x ch;
        stringEnd       := x+1
    end
let getStr ()                   = String.sub (!stringBuffer) 0 (!stringEnd)
let extractLineno yytxt offset  = ios(String.sub yytxt offset(String.length yytxt-offset))
let out_of_char x fi            = if x>255 then error fi"Illegal Char" else Char.chr x 
}


(* The main body of the lexical analyzer *)

let digit                   = ['0'-'9'] 
let init                    = ['a'-'z' 'A'-'Z' '_' ]
let tail                    = ['a'-'z' 'A'-'Z' '_' '0'-'9' '\'']
let tabs                    = [' ' '\009' '\012']
let op                      = ['~' '%' '\\' '+' '-' '&' '|' ':' '`' '$']
let symbol                  = ['*' '#' '/' '!' '?' '^' '(' ')' '{' '}' '[' ']' '<' '>' '.' ';' '_' ',' '=' '\'']
let nl                      = tabs*("\r")?"\n"
let comment                 = "/*" 
let comment_end             = "*/"  

rule token              = parse
| "#show"                   { show lexbuf                                           }   
| "#load"                   { load lexbuf                                           }   
| tabs+                     { token lexbuf                                          }
| "()"                      { Parser.UNIT(info lexbuf)                              }
| "[]"                      { Parser.NIL(info lexbuf)                               } 
| nl                        { newline lexbuf; token lexbuf                          }
| digit+                    { Parser.INTV{i=info lexbuf;v=ios(text lexbuf)}         }
| "*."                      { Parser.TIMESFLOAT(info lexbuf)                        }  
| digit+ '.' digit+         { Parser.FLOATV{i=info lexbuf;v=fos(text lexbuf)}       }
| init tail*                { createID (info lexbuf) (text lexbuf)                  }
| ":=" | "<:" | "<-" | "->" | "=>" | "==>" | "{|" | "|}" | "<|" | "|>" 
| "[|" | "|]" | "=="        { createID (info lexbuf) (text lexbuf)                  }
| op+                       { createID (info lexbuf) (text lexbuf)                  }
| symbol                    { createID (info lexbuf) (text lexbuf)                  }
| "\""                      { resetStr(); startLex:=info lexbuf; string lexbuf      } 
| ";;" nl                   { Parser.DOUBLESEMI(info lexbuf)                        }
| eof                       { Parser.EOF(info lexbuf)                               }
| comment_end               { error (info lexbuf) "Unmatched end of comment"        } 
| comment                   { depth:=1;startLex:=info lexbuf;comment lexbuf;token lexbuf } 
| _                         { error (info lexbuf) "Illegal character"               }

and show                = parse
| "context"                 { Parser.SHOWCONTEXT(info lexbuf)                       }
| _                         { show lexbuf   }
and load                = parse
| '"'                       { Parser.LOAD{i = !startLex; v=getStr()}                  }
| eof                       { error(!startLex)"String not terminated"               } 
| '\\'                      { addStr(escaped lexbuf)              ; load lexbuf     } 
| '\n'                      { addStr('\n') ; newline lexbuf       ; load lexbuf     } 
| _                         { addStr(Lexing.lexeme_char lexbuf 0) ; load lexbuf     } 
| "\""                      { resetStr();startLex:=info lexbuf    ; load lexbuf     } 

and comment             = parse
| comment                   { depth:=succ !depth; comment lexbuf                    } 
| comment_end               { depth:=pred !depth; if !depth>0 then comment lexbuf   } 
| eof                       { error (!startLex) "Comment not terminated"            } 
| [^ '\n']                  { comment lexbuf                                        }
| "\n"                      { newline lexbuf; comment lexbuf                        } 

and string              = parse
| '"'                       { Parser.STRINGV{i= !startLex; v=getStr()}              }
| eof                       { error(!startLex)"String not terminated"               } 
| '\\'                      { addStr(escaped lexbuf)              ; string lexbuf   } 
| '\n'                      { addStr('\n') ; newline lexbuf       ; string lexbuf   } 
| _                         { addStr(Lexing.lexeme_char lexbuf 0) ; string lexbuf   } 
and escaped             = parse
| 'n'	                    { '\n'                                                  }
| 't'	                    { '\t'                                                  }
| '\\'	                    { '\\'                                                  }
| '"'                       { '\034'                                                }
| '\''	                    { '\''                                                  }
| digit digit digit         { out_of_char (ios(text lexbuf))(info lexbuf)           }
| [^ '"' '\\' 't' 'n' '\''] { error (info lexbuf) "Illegal character constant"      }




