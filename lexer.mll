(* ---------------------------------------------------------------------------*)
(* HEADER SECTION *)
(* ---------------------------------------------------------------------------*)
{
    open Printf
}


(* ---------------------------------------------------------------------------*)
(* DEFINITIONS SECTION *)
(* ---------------------------------------------------------------------------*)
let ADD         = '+'
let SUB         = '-'
let MUL         = '*'
let DIV         = '/'

let NEG         = "<>"
let LEQ         = "<="
let LT          = '<'
let EQ          = "=="

let DIGIT       = ['0'-'9']+
let ALPHA       = ['a'-'z''A'-'Z']+
let STR         = "\"[^\"]*\""

let linereturn  = "\r\n" | "\n\r" | '\n' | '\r'
let whitespace  = ' '
let tabulation  = '\t'
let blank       = tabulation | whitespace



(* ---------------------------------------------------------------------------*)
(* RULES SECTION *)
(* ---------------------------------------------------------------------------*)
rule udem_lang = parse 

    (* oper *)
    | ADD
    | SUB
    | MUL
    | DIV as operator
        {
            printf "oper(%c)\n" operator;
            udem_lang lexbuf
        }

    (* Unknown char *)
    | _ as c
        {
            printf "Unrecognized character: %c\n" c;
            udem_lang lexbuf
        }

    (* end of file *)
    | eof {}



(* ---------------------------------------------------------------------------*)
(* TRAILER SECTION *)
(* ---------------------------------------------------------------------------*)
{
    (* Temporary debug *)
    let main() =
        let cin = 
            if Array.length Sys.argv > 1
                then open_in Sys.argv.(1)
            else stdin
        in
        let lexbuf = Lexing.from_channel cin in
        udem_lang lexbuf
        let _ = Printexc.print main()
}

