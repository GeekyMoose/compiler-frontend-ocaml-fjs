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
%token PLUS MINUS STAR SLASH LT LEQ EQ2 EQ
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
    | body EOF {$1}
    | error EOF {print_endline "Error in program"; []}
;

body:
    | /* Empty body */ {[]}
    | statement_list {$1}
;

block:
    | LBRACET body RBRACET {$2}
    | LBRACET error RBRACET {print_endline "Error in block"; []}
;


/* -------------------------------------------------------------------------- */
statement_list:
    | block {$1}
    | statement {[$1]}
    | statement_list statement {$1@[$2]}
;

statement:
    | declaration SEMICOLON {$1}
    | expression SEMICOLON {$1}
;

declaration:
    | function_declaration {$1}
    | variable {$1}
    | string {$1}
;

expression:
    | number {$1}
    | function_call {$1}
;

/* -------------------------------------------------------------------------- */
identifier:
    IDENTIFIER {
        let loc = ("TODO-filename", 1, 1) in
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

string:
    STR_VALUE{Exp.Str $1}
;

/* -------------------------------------------------------------------------- */
function_declaration:
    FUNCTION identifier LPAREN list_args_option RPAREN expression {
            Exp.Function ($4, $6)
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
    expression LPAREN list_parameters_option RPAREN {
        let loc = ("TODO-filename", 1, 1) in
        let f = $1 in
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


/* ---------------------------------------------------------------------------*/
/* TRAILER */
/* ---------------------------------------------------------------------------*/
%%


