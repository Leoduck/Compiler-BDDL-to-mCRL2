module Lexer = Lib.Lexer
module Fm = Lib.File_manager
open Lib


let pp_lex domain_file problem_file =
    (* let file_in = open_in file in *)
    let bddl_spec = Fm.read_file domain_file ^ Fm.read_file problem_file ^ "\n" in
    Token_printer.lexpp bddl_spec 

let _ =
    try
        pp_lex (Array.get Sys.argv 1) (Array.get Sys.argv 2)
    with
        Invalid_argument _ -> Printf.printf "No filepath given \n"
