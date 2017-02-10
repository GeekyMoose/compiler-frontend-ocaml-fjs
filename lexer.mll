(*
 * Since: Feb 9, 2017
 *
 * Lexer for the grammar defined for Parser
 *)


(* ---------------------------------------------------------------------------*)
(* HEADER SECTION *)
(* ---------------------------------------------------------------------------*)
{
    open Parser
}


(* ---------------------------------------------------------------------------*)
(* DEFINITIONS SECTION *)
(* ---------------------------------------------------------------------------*)
let digit       = ['0'-'9']
let alpha       = ['a'-'z''A'-'Z']
let str         = "\"[^\"]*\""

let newline     = "\r\n" | "\n\r" | '\n' | '\r'
let whitespace  = ' '
let tabulation  = '\t'
let blank       = tabulation | whitespace



(* ---------------------------------------------------------------------------*)
(* RULES SECTION *)
(* ---------------------------------------------------------------------------*)
rule token = parse

    (* operators *)
    | '+' {ADD}
    | '-' {SUB}
    | '*' {MUL}
    | '/' {DIV}
    | '=' {EQ}
    | '-' {NEG}
    | '<' {LT}
    | "<=" {LEQ}

    (* values *)
    | digit+ as value {INT_VALUE (int_of_string value)}

    (* Special elements / Unrecognized elements*)
    | newline {NEWLINE}
    | blank {token lexbuf}
    | _ {token lexbuf}
    | eof {raise End_of_file}



(* ---------------------------------------------------------------------------*)
(* TRAILER SECTION *)
(* ---------------------------------------------------------------------------*)
{}

