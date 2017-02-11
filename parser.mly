/* ---------------------------------------------------------------------------*/
/* HEADER SECTION */
/* ---------------------------------------------------------------------------*/
%{
    open Printf
    open Lexing

    (* Called by the parser function on error *)
    let parse_error s = 
        print_endline s;
        flush stdout;;
%}


/* ---------------------------------------------------------------------------*/
/* OCAMLYACC DECLARATIONS */
/* ---------------------------------------------------------------------------*/
%token <int> INT_VALUE
%token <string> STR_VALUE
%token <string> IDENTIFIER
%token PLUS MINUS STAR SLASH LT LEQ EQ
%token LPAREN RPAREN LBRACET RBRACET
%token PERIOD COMMA SEMICOLON UNDERSCORE
%token IF ELIF ELSE VAR FUNCTION
%token EOF

%left PLUS MINUS
%left STAR SLAH

%start program
%type <Exp.exp list> program


/* ---------------------------------------------------------------------------*/
/* GRAMMAR RULES (Rules and actions) */
/* ---------------------------------------------------------------------------*/
%%
program:
    EOF {[]}
    | body EOF{$1}
    | error EOF {print_endline "Error in program"; []}
;

body:
    block{$1}
    | variable {[$1]}
    | error {print_endline "Error in body";[]}
;

block:
    LBRACET RBRACET {[]}
    | LBRACET body RBRACET {$2}
;

variable:
    VAR IDENTIFIER SEMICOLON {
            let loc = ("todo-filename", 1, 1) in
            let id = (loc, $2) in
            Exp.Var id
        }
;






/*
TODO



body_list:
    body {[$1]}
    | body_list body {$1 @ [$2]}
;


declaration:
    FUNCTION IDENTIFIER LPAREN list_args RPAREN {}
    | VAR IDENTIFIER EQ expression {}
;

expression:
    FUNCTION LPAREN list_args RPAREN expression {}
    | expression LPAREN list_expressions RPAREN {}
;

list_args:
    INT_VALUE COMMA {}
;

list_expressions:
    expression COMMA {}
;

number:
    INT_VALUE {Exp.Num $1}
;



exp:    INT_VALUE {$1}
        | exp PLUS exp {$1 + $3}
        | exp MINUS exp {$1 - $3}
        | exp STAR exp {$1 * $3}
        | exp SLASH exp {
            if $3 <> 0 then $1 / $3
            else (
                let pos_start = Parsing.rhs_start_pos 3 in
                printf "Division by zero: %d.%d : "
                    pos_start.pos_lnum
                    (pos_start.pos_cnum - pos_start.pos_bol);
                1
            )
         }
        | MINUS exp {-$2}
        | LPAREN exp RPAREN {$2}
;
*/
        /*
        TODO
        | exp LEQ exp {$1 <= $3}
        | exp LT exp {$1 < $3}
        | exp EQ exp {$1 = $3}
        */


/* ---------------------------------------------------------------------------*/
/* TRAILER */
/* ---------------------------------------------------------------------------*/
%%




