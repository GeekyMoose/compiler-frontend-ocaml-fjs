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
        | exp NEWLINE { printf "%d\n" $1; flush stdout}
;

exp:    INT_VALUE {$1}
        | exp ADD exp {$1 + $3}
        | exp SUB exp {$1 - $3}
        | exp MUL exp {$1 * $3}
        | exp DIV exp {$1 / $3}
        | NEG exp {-$2}
        /*
        | exp LEQ exp {$1 <= $3}
        | exp LT exp {$1 < $3}
        | exp EQ exp {$1 = $3}
        */
;

/* ---------------------------------------------------------------------------*/
/* TRAILER */
/* ---------------------------------------------------------------------------*/
%%
