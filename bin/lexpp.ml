open Lib
module Lexer = Lexer
module Fm = File_manager

(*for pretty-printing tokens read from BDDL spec*)

let pp_lex domain_file problem_file =
    let bddl_spec = Fm.read_file domain_file ^ Fm.read_file problem_file ^ "\n" in
    Token_printer.lexpp bddl_spec 

let _ =
    try
        pp_lex (Array.get Sys.argv 1) (Array.get Sys.argv 2)
    with
        Invalid_argument _ -> Printf.printf "No filepath given \n"
