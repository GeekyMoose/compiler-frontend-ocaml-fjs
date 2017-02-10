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
    | '+' {PLUS}
    | '-' {MINUS}
    | '*' {MULTIPLY}
    | '/' {DIVIDE}
    | '<' {LT}
    | "<=" {LEQ}
    | "==" {EQ}

    (* Elements *)
    | '(' {LPAREN}
    | ')' {RPAREN}
    | '{' {LBRACET}
    | '}' {RBRACET}

    (* keywords *)
    | "if" {IF}
    | "else if" {ELIF}
    | "else" {ELSE}
    | "function" {FUNCTION}
    | "var" {VAR}

    (* values *)
    | digit+ as value {INT_VALUE (int_of_string value)}
    | str as value {STR_VALUE (value)}

    (* Special elements / Unrecognized elements*)
    | '.' {PERIOD}
    | newline {incr_lineno lexbuf; NEWLINE}
    | blank {token lexbuf}
    | _ {token lexbuf}
    | eof {raise End_of_file}



(* ---------------------------------------------------------------------------*)
(* TRAILER SECTION *)
(* ---------------------------------------------------------------------------*)
{
    let rec debug_iter_tokens lexbuf =
        let tok = token lexbuf in
        match tok with
        | PLUS -> print_string "PLUS"; debug_iter_tokens lexbuf
        | INT_VALUE x -> print_string "INT_VALUE(";print_int x;print_string ")";debug_iter_tokens lexbuf
        | _ -> print_string "X";;
}



