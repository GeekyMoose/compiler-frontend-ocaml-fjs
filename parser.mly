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
%token ADD SUB MUL DIV NEG LEQ LT EQ
%token LPAREN RPAREN
%token NEWLINE

%left ADD SUB
%left MUL DIV
%left NEG

%start input
%type <unit> input


/* ---------------------------------------------------------------------------*/
/* GRAMMAR RULES (Rules and actions) */
/* ---------------------------------------------------------------------------*/
%%
input:  /* empty */ {}
        | input line {}
;

line:   NEWLINE {}
        | exp NEWLINE { printf "%d\n" $1; flush stdout}
        | error NEWLINE {}
;

exp:    INT_VALUE {$1}
        | exp ADD exp {$1 + $3}
        | exp SUB exp {$1 - $3}
        | exp MUL exp {$1 * $3}
        | exp DIV exp {
            if $3 <> 0 then $1 / $3
            else (
                let pos_start = Parsing.rhs_start_pos 3 in
                printf "Division by zero: %d.%d : "
                    pos_start.pos_lnum
                    (pos_start.pos_cnum - pos_start.pos_bol);
                1
            )
         }
        | NEG exp {-$2}
        | LPAREN exp RPAREN {$2}
        /*
        TODO
        | exp LEQ exp {$1 <= $3}
        | exp LT exp {$1 < $3}
        | exp EQ exp {$1 = $3}
        */
;


/* ---------------------------------------------------------------------------*/
/* TRAILER */
/* ---------------------------------------------------------------------------*/
%%



