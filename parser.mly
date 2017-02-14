/*
 * Since:   Feb 9, 2017
 * Author:  Constantin
 *
 * Parser for the UdeM fjs language grammar (Functional language)
 */

/* ---------------------------------------------------------------------------*/
/* HEADER SECTION */
/* ---------------------------------------------------------------------------*/
%{
    open Printf
    open Lexing

    let print_location (fname, lineno, charpos) = 
        print_string fname;
        print_string " / ";
        print_string (string_of_int lineno);
        print_string " / ";
        print_string (string_of_int charpos);
        ;;

    (* DEPRECATED - Return the current location *)
    let current_loc =
        let pos     = Parsing.symbol_start_pos() in
        let fname   = pos.pos_fname in
        let lineno  = pos.pos_lnum in
        let charpos = pos.pos_cnum - pos.pos_bol in
        (fname, lineno, charpos);;

    (* Called by the parser function on error *)
    let parse_error msg = 
        flush stdout;;

    (* Print error *)
    let print_error loc msg = 
        print_string "[ERR] ";
        print_location loc;
        print_string ": Parse error: ";
        print_endline msg;;
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
    /*| error EOF {print_error current_loc "Invalid program"; []}*/
;

body:
    | /* Empty body */ {[]}
    | statement_list {$1}
;

block:
    | LBRACET body RBRACET {$2}
;


/* -------------------------------------------------------------------------- */
statement_list:
    | block {$1} /* Must be moved. here error if 2 blocks */
    | statement {[$1]}
    | statement_list statement {$1@[$2]}
;

statement:
    | declaration SEMICOLON {$1}
    | expression SEMICOLON {$1}
;

declaration:
    | function_declaration {$1}
    | variable_declaration {$1}
    | string_value {$1} /* TODO: Debug. Should be moved */
;

expression:
    | function_call {$1}
    | if_statement {$1}
    | expression_number {$1}
    | variable_get {$1}
;

expression_number:
    | number {$1}
    | binop {$1}
;

/* -------------------------------------------------------------------------- */
if_statement:
    | IF LPAREN unary_test RPAREN expression if_follow {
            Exp.If (current_loc, $3, $5, $6)
        }
;

if_follow:
    | ELSE expression {$2}
;

/* -------------------------------------------------------------------------- */
unary_test:
    /* TODO: resolve conflicts + type */
    | expression LEQ expression {Exp.PrimOp(current_loc, Exp.Leq, $1::$3::[])}
    | expression LT expression {Exp.PrimOp(current_loc, Exp.Lt, $1::$3::[])}
    | expression EQ expression {Exp.PrimOp(current_loc, Exp.Eq, $1::$3::[])}
;

binop:
    /* TODO: resolve shitf/reduce conflicts + type */
    | expression PLUS expression {Exp.PrimOp(current_loc, Exp.Add, $1::$3::[])}
    | expression MINUS expression {Exp.PrimOp(current_loc, Exp.Sub, $1::$3::[])}
    | expression STAR expression {Exp.PrimOp(current_loc, Exp.Mul, $1::$3::[])}
    | expression SLASH expression {Exp.PrimOp(current_loc, Exp.Div, $1::$3::[])}
    | MINUS expression {Exp.PrimOp(current_loc, Exp.Neg, [$2])}
;

/* -------------------------------------------------------------------------- */
identifier:
    | IDENTIFIER {
        let id = (current_loc, $1) in
        id
    }
;

variable_declaration:
    /* TODO: Change Var by Let. Here just to compile but doesn't make sence*/
    VAR identifier EQ expression {Exp.Var $2}
;

variable_get:
    identifier {Exp.Var $1}
;

number:
    INT_VALUE {Exp.Num $1}
;

string_value:
    STR_VALUE{Exp.Str $1}
;

/* -------------------------------------------------------------------------- */
function_declaration:
    | FUNCTION identifier LPAREN list_args_option RPAREN expression {
            Exp.Function ($4, $6)
        }
;

list_args_option:
    | /* No args */ {[]}
    | list_args {$1}
;

list_args:
    | identifier {[$1]}
    | list_args COMMA identifier {$1@[$3]}
;

/* -------------------------------------------------------------------------- */
function_call:
    | expression LPAREN list_parameters_option RPAREN {
            Exp.Call (current_loc, $1, $3)
        }
;

list_parameters_option:
    | /* No params */ {[]}
    | list_parameters {$1}
;

list_parameters:
    | expression {[$1]}
    | list_parameters COMMA expression {$1@[$3]}
;


/* ---------------------------------------------------------------------------*/
/* TRAILER */
/* ---------------------------------------------------------------------------*/
%%


