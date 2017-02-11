open Printf
module P = Parser

let input_file = 
    try open_in Sys.argv.(1)
    with Invalid_argument a
        -> print_string "No file name given!\n"; exit 1

let main() = 
        let lexbuf = Lexing.from_channel input_file in
        Lexer.debug_iter_tokens lexbuf
let _ = Printexc.print main()
