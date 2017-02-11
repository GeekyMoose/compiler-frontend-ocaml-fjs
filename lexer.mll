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
    let current_filename lexbuf =
        lexbuf.lex_curr_p.pos_fname
    let current_lnum lexbuf = 
        lexbuf.lex_curr_p.pos_lnum
    let current_bol lexbuf =
        lexbuf.lex_curr_p.pos_bol
}


(* ---------------------------------------------------------------------------*)
(* DEFINITIONS SECTION *)
(* ---------------------------------------------------------------------------*)
let digit       = ['0'-'9']
let alpha       = ['a'-'z''A'-'Z']
let str         = "\"[^\"]*\""
let var_name    = alpha(alpha | digit | '-' | '_')*

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

    (* elements / punctuation *)
    | '(' {LPAREN}
    | ')' {RPAREN}
    | '{' {LBRACET}
    | '}' {RBRACET}
    | '.' {PERIOD}
    | ',' {COMMA}
    | ';' {SEMICOLON}
    | '-' {HYPHEN}
    | '_' {UNDERSCORE}

    (* keywords *)
    | "if" {IF}
    | "else" {ELSE}
    | "else if" {ELIF}
    | "function" {FUNCTION}
    | "var" {VAR}

    (* values *)
    | var_name as value {VAR_NAME (value)}
    | digit+ as value {INT_VALUE (int_of_string value)}
    | str as value {STR_VALUE (value)}

    (* Special elements / Unrecognized elements*)
    | newline {incr_lineno lexbuf; NEWLINE}
    | blank {token lexbuf}
    | _ {token lexbuf}
    | eof {EOF}



(* ---------------------------------------------------------------------------*)
(* TRAILER SECTION *)
(* ---------------------------------------------------------------------------*)
{
    (* DEBUG FUNCTION - Iterate through the tokens and display flow of tokens *)
    let rec debug_iter_tokens lexbuf =
        let tok = token lexbuf in
        let print_token t = print_string t;print_string " " in
        match tok with
        | PLUS -> print_token "PLUS"; debug_iter_tokens lexbuf
        | MINUS -> print_token "MINUS"; debug_iter_tokens lexbuf
        | MULTIPLY -> print_token "MULTIPLY"; debug_iter_tokens lexbuf
        | DIVIDE -> print_token "DIVIDE"; debug_iter_tokens lexbuf
        | LT -> print_token "LT"; debug_iter_tokens lexbuf
        | LEQ -> print_token "LEQ"; debug_iter_tokens lexbuf
        | EQ -> print_token "EQ"; debug_iter_tokens lexbuf

        | LPAREN -> print_token "LPAREN"; debug_iter_tokens lexbuf
        | RPAREN -> print_token "RPAREN"; debug_iter_tokens lexbuf
        | LBRACET -> print_token "LBRACET"; debug_iter_tokens lexbuf
        | RBRACET -> print_token "RBRACET"; debug_iter_tokens lexbuf
        | PERIOD -> print_token "PERIOD"; debug_iter_tokens lexbuf
        | COMMA -> print_token "COMMA"; debug_iter_tokens lexbuf
        | SEMICOLON -> print_token "SEMICOLON"; debug_iter_tokens lexbuf
        | HYPHEN -> print_token "HYPHEN"; debug_iter_tokens lexbuf
        | UNDERSCORE -> print_token "UNDERSCORE"; debug_iter_tokens lexbuf

        | IF -> print_token "IF"; debug_iter_tokens lexbuf
        | ELSE -> print_token "ELSE"; debug_iter_tokens lexbuf
        | ELIF -> print_token "ELIF"; debug_iter_tokens lexbuf
        | FUNCTION -> print_token "FUNCTION"; debug_iter_tokens lexbuf
        | VAR -> print_token "VAR"; debug_iter_tokens lexbuf

        | VAR_NAME x ->     print_string "VAR_NAME(";
                            print_string x;
                            print_token ")";
                            debug_iter_tokens lexbuf
        | INT_VALUE x ->    print_string "INT_VALUE(";
                            print_int x;
                            print_token ")";
                            debug_iter_tokens lexbuf
        | STR_VALUE x ->    print_string "STR_VALUE(";
                            print_string x;
                            print_token ")";
                            debug_iter_tokens lexbuf

        | NEWLINE -> print_endline "NEWLINE"; debug_iter_tokens lexbuf
        | EOF -> print_endline "EOF"
        | _ -> print_token "UNKNOWN"; debug_iter_tokens lexbuf
    ;;
}



