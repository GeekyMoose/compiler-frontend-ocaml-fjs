/* ---------------------------------------------------------------------------*/
/* HEADER SECTION */
/* ---------------------------------------------------------------------------*/
%{
    open Printf

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
%token NEWLINE

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
        | exp NEWLINE {}
;

exp:    ADD exp exp {$2 + $3}
        | SUB exp exp {$2 - $3}
        | MUL exp exp {$2 * $3}
        | DIV exp exp {$2 / $3}
        | NEG exp {-$2}
        /*
        | LEQ exp exp {$2 <= $3}
        | LT exp exp {$2 < $3}
        | EQ exp exp {$2 = $3}
        */
;

/* ---------------------------------------------------------------------------*/
/* TRAILER */
/* ---------------------------------------------------------------------------*/
%%
