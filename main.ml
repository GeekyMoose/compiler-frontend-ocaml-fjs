open Printf
module P = Parser
module E = Exp

let input_file = 
    try open_in Sys.argv.(1)
    with Invalid_argument a
        -> print_string "No file name given!\n"; exit 1

let parser_mode() =
    print_endline "\n----- PARSER -----";
    print_endline " - Start parsing...";
    let lexbuf  = Lexing.from_channel input_file in
    let ast     = Parser.program Lexer.token lexbuf in
    let size    = List.length ast in
    print_string " + [SUCCESS] Parsing successfully done! (Number elt: ";
    print_int size;
    print_endline ")";
    print_endline "----- ----- -----";
    ;;



let debug_mode() =
    print_endline "\n----- DEBUG PARSE -----";
    let lexbuf = Lexing.from_channel input_file in
    Lexer.debug_iter_tokens lexbuf;
    print_endline "----- ----- -----";
    ;;

let main() = 
    print_endline "\n----- START -----";
    (*debug_mode();*)
    parser_mode();
    print_endline "\n----- STOP -----\n";
    ;;



let _ = Printexc.print main()
