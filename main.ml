open Printf
open Parser
open Lexer
module E = Exp

let input_file = 
    try open_in Sys.argv.(1)
    with Invalid_argument a
        -> print_string "No file name given!\n"; exit 1

let main() = 
    let lexbuf = Lexing.from_channel input_file in
    Lexer.debug_iter_tokens lexbuf
    (*
    while true do
        Parser.input Lexer.token lexbuf
    done
    *)
let _ = Printexc.print main()
