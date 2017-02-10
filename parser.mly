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
%token PLUS MINUS MULTIPLY DIVIDE LT LEQ EQ
%token LPAREN RPAREN LBRACET RBRACET
%token NEWLINE PERIOD
%token IF ELIF ELSE VAR FUNCTION

%left ADD SUB
%left MUL DIV
%left NEG

%start body
%type <unit> body


/* ---------------------------------------------------------------------------*/
/* GRAMMAR RULES (Rules and actions) */
/* ---------------------------------------------------------------------------*/
%%
body:  /* empty */ {}
        | body line {}
;

line:   NEWLINE {}
        | exp NEWLINE { printf "%d\n" $1; flush stdout}
        | error NEWLINE {}
;

exp:    INT_VALUE {$1}
        | exp PLUS exp {$1 + $3}
        | exp MINUS exp {$1 - $3}
        | exp MULTIPLY exp {$1 * $3}
        | exp DIVIDE exp {
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

