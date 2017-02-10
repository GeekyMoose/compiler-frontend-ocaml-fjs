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
    open Lexing

    (* Update the current lexbuf position *)
    let incr_lineno lexbuf =
        let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- {
            pos with
            pos_lnum = pos.pos_lnum + 1;
            pos_bol = pos.pos_cnum;
        }
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

    (* Elements *)
    | '(' {LPAREN}
    | ')' {RPAREN}

    (* values *)
    | digit+ as value {INT_VALUE (int_of_string value)}

    (* Special elements / Unrecognized elements*)
    | newline {incr_lineno lexbuf; NEWLINE}
    | blank {token lexbuf}
    | _ {token lexbuf}
    | eof {raise End_of_file}



(* ---------------------------------------------------------------------------*)
(* TRAILER SECTION *)
(* ---------------------------------------------------------------------------*)
{}

