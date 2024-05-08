module Parser = Lib.Parser
module Lexer = Lib.Lexer
module Pp = Lib.Pretty
module Fm = Lib.File_manager
open Lib

let pp_parse domain_file problem_file =
    (* let file_in = open_in file in *)
    let bddl_spec = Fm.read_file domain_file ^ Fm.read_file problem_file ^ "\n" in
    (*Printf.printf "%s" bddl_spec;*)
    let lex_string = Lexing.from_string bddl_spec in
    let prg = Parser.program Lexer.token lex_string in
    print_endline (Pp.str_of_spec prg) 
    

let _ =
    try
        pp_parse (Array.get Sys.argv 1) (Array.get Sys.argv 2)
    with
        Invalid_argument _ -> Printf.printf "No filepath given \n"
        
