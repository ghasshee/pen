%{ 
open Printf
open Lexing

let var_table = Hashtbl.create 16
%}

%token NEWLINE
%token LPAREN RPAREN
%token <float> NUM
%token PLUS MINUS MUL DIV POW 
%token EQ
%token <string> VAR
%token <float->float> FUNC

%left   PLUS MINUS 
%left   MUL DIV
%left   NEG
%right  POW
%right  EQ

%start input
%type <unit> input

%%


input:                          { }
    | input line                { }             
    ;

line: NEWLINE                   { } 
    | expr NEWLINE              { printf "\t%.10g\n"  $1; flush stdout } 
    | error NEWLINE             { printf "parse error\n"; flush stdout }   
    ;       /* OCamlYacc has the reverved words 'error' */ 

expr: NUM                       { $1 }
    | VAR                       { 
        try Hashtbl.find var_table $1
        with Not_found      -> printf "no such variable '%s'\n" $1; 0.0 }
    | VAR EQ expr               { Hashtbl.replace var_table $1 $3; $3 }
    | FUNC LPAREN expr RPAREN   { $1 $3 }
    | expr PLUS expr            { $1 +. $3 } 
    | expr MINUS expr           { $1 -. $3 } 
    | expr MUL expr             { $1 *. $3 }
    | expr DIV expr             { 
        if $3 <> 0.0  
            then $1 /. $3 
            else (
                let start_pos   = Parsing.rhs_start_pos 3 in 
                let end_pos     = Parsing.rhs_end_pos 3 in
                printf "%d.%d-%d.%d: division by zero"
                    start_pos.pos_lnum (start_pos.pos_cnum - start_pos.pos_bol)
                    end_pos.pos_lnum (end_pos.pos_cnum - end_pos.pos_bol); 
                1.0 ) }
    | expr POW expr             { $1 ** $3 }
    | MINUS expr %prec NEG      { -. $2 }
    | LPAREN expr RPAREN        { $2 } 
    ;

%%


