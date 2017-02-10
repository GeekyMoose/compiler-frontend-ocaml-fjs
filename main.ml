open Printf
open Parser
open Lexer

let main() = 
    try
        let lexbuf = Lexing.from_channel stdin in
        Lexer.debug_iter_tokens lexbuf
        (*
        while true do
            Parser.input Lexer.token lexbuf
        done
        *)
    with End_of_file -> exit 0
let _ = Printexc.print main()
