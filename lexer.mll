(*
 * Since:   Feb 9, 2017
 * Author:  Constantin
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


    (* Error exception *)
    exception Error of string

    (* Error print function *)
    let parse_error s = print_endline s

    let error lexbuf msg = 
        raise (Error msg)
}


(* ---------------------------------------------------------------------------*)
(* DEFINITIONS SECTION *)
(* ---------------------------------------------------------------------------*)
let digit       = ['0'-'9']
let alpha_upper = ['A'-'Z']
let alpha_lower = ['a'-'z']
let alpha       = alpha_upper | alpha_lower
let alphanum    = digit | alpha

let identifier  = alpha (alphanum | '-' | '_')*
let str         = "\"[^\"]*\""

let newline     = "\r\n" | "\n\r" | '\n' | '\r'
let whitespace  = ' '
let tabulation  = '\t'
let blank       = tabulation | whitespace



(* ---------------------------------------------------------------------------*)
(* RULES SECTION *)
(* ---------------------------------------------------------------------------*)
rule token = parse
    (* layout *)
    | newline       {incr_lineno lexbuf; NEWLINE}
    | blank+        {token lexbuf}

    (* operators *)
    | '+'           {PLUS}
    | '-'           {MINUS}
    | '*'           {STAR}
    | '/'           {SLASH}
    | '<'           {LT}
    | "<="          {LEQ}
    | "=="          {EQ}

    (* elements / punctuation *)
    | '('           {LPAREN}
    | ')'           {RPAREN}
    | '{'           {LBRACET}
    | '}'           {RBRACET}
    | '.'           {PERIOD}
    | ','           {COMMA}
    | ';'           {SEMICOLON}
    | '_'           {UNDERSCORE}

    (* keywords *)
    | "if"          {IF}
    | "else"        {ELSE}
    | "else if"     {ELIF}
    | "function"    {FUNCTION}
    | "var"         {VAR}

    (* special keywords *)
    | "/*"          {comments 0 lexbuf}

    (* values *)
    | identifier as value   {IDENTIFIER (value)}
    | digit+ as value       {INT_VALUE (int_of_string value)}
    | str as value          {STR_VALUE (value)}

    (* Special elements / Unrecognized elements*)
    | eof           {EOF}
    | _             {error lexbuf "Unknown character."}


    (* comments section. Allow nested comments (No line restriction) *)
    and comments level = parse
    | "*/"          {if level = 0
                        then token lexbuf
                        else comments (level-1) lexbuf
                    }
    | "/*"          {comments (level+1) lexbuf}
    | _             {comments level lexbuf}
    | eof           {error lexbuf "Comment started but no */ find."}



(* ---------------------------------------------------------------------------*)
(* TRAILER SECTION *)
(* ---------------------------------------------------------------------------*)
{
    (* DEBUG FUNCTION - Iterate through the tokens and display flow of tokens *)
    let rec debug_iter_tokens lexbuf =
        let tok = token lexbuf in
        let print_token t = print_string t;print_string " " in
        match tok with
        | PLUS          -> print_token "PLUS"; debug_iter_tokens lexbuf
        | MINUS         -> print_token "MINUS"; debug_iter_tokens lexbuf
        | STAR          -> print_token "STAR"; debug_iter_tokens lexbuf
        | SLASH         -> print_token "SLASH"; debug_iter_tokens lexbuf
        | LT            -> print_token "LT"; debug_iter_tokens lexbuf
        | LEQ           -> print_token "LEQ"; debug_iter_tokens lexbuf
        | EQ            -> print_token "EQ"; debug_iter_tokens lexbuf

        | LPAREN        -> print_token "LPAREN"; debug_iter_tokens lexbuf
        | RPAREN        -> print_token "RPAREN"; debug_iter_tokens lexbuf
        | LBRACET       -> print_token "LBRACET"; debug_iter_tokens lexbuf
        | RBRACET       -> print_token "RBRACET"; debug_iter_tokens lexbuf
        | PERIOD        -> print_token "PERIOD"; debug_iter_tokens lexbuf
        | COMMA         -> print_token "COMMA"; debug_iter_tokens lexbuf
        | SEMICOLON     -> print_token "SEMICOLON"; debug_iter_tokens lexbuf
        | UNDERSCORE    -> print_token "UNDERSCORE"; debug_iter_tokens lexbuf

        | IF            -> print_token "IF"; debug_iter_tokens lexbuf
        | ELSE          -> print_token "ELSE"; debug_iter_tokens lexbuf
        | ELIF          -> print_token "ELIF"; debug_iter_tokens lexbuf
        | FUNCTION      -> print_token "FUNCTION"; debug_iter_tokens lexbuf
        | VAR           -> print_token "VAR"; debug_iter_tokens lexbuf

        | IDENTIFIER x  ->  print_string "VAR_NAME(";
                            print_string x;
                            print_token ")";
                            debug_iter_tokens lexbuf
        | INT_VALUE x   ->  print_string "INT_VALUE(";
                            print_int x;
                            print_token ")";
                            debug_iter_tokens lexbuf
        | STR_VALUE x   ->  print_string "STR_VALUE(";
                            print_string x;
                            print_token ")";
                            debug_iter_tokens lexbuf

        | NEWLINE       -> print_endline "NEWLINE"; debug_iter_tokens lexbuf
        | EOF           -> print_endline "EOF"
    ;;
}



