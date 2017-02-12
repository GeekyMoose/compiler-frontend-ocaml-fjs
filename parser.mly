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


/* -------------------------------------------------------------------------- */

program:
    EOF {[]}
    | body EOF{$1}
    | error EOF {print_endline "Error in program"; []}
;

body:
    block {$1}
    | function_declaration function_call{[$1]@[$2]}
    | error {print_endline "Error in body"; []}
;

block:
    LBRACET RBRACET {[]}
    | LBRACET body RBRACET {$2}
;


/* -------------------------------------------------------------------------- */
expression:
    number {$1}
;


/* -------------------------------------------------------------------------- */
function_declaration:
    FUNCTION identifier LPAREN list_args_option RPAREN expression {
            let ids = [$2]@$4 in
            Exp.Function (ids, $6)
        }
;

list_args_option:
    /* No args */ {[]}
    | list_args {$1}
;

list_args:
    identifier {[$1]}
    | list_args COMMA identifier {$1@[$3]}
;

/* -------------------------------------------------------------------------- */
function_call:
    identifier LPAREN list_parameters_option RPAREN {
        let loc = ("todo-filename", 1, 1) in
        let f = Exp.Var $1 in
        Exp.Call (loc, f, $3)
    }
;

list_parameters_option:
    /* No params */ {[]}
    | list_parameters {$1}
;

list_parameters:
    expression {[$1]}
    | list_parameters COMMA expression {$1@[$3]}
;


/* -------------------------------------------------------------------------- */

identifier:
    IDENTIFIER {
        let loc = ("todo-filename", 1, 1) in
        let id = (loc, $1) in
        id
    }
;

variable:
    VAR identifier {Exp.Var $2}
;

number:
    INT_VALUE {Exp.Num $1}
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
list_expressions:
    expression COMMA {}
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


