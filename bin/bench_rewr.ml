module Parser = Lib.Parser
module Lexer = Lib.Lexer
module Pp = Lib.Pretty
module Fm = Lib.File_manager
open Lib

let rewr_dir source_dir dest_dir =
  let file_names = Sys.readdir source_dir in
  let domian_file = source_dir ^ "domain.ig" in
  Array.iter (fun filename ->
    if filename = "domain.ig" 
    then ()
    else 
      let filepath = Filename.concat source_dir filename in
      let name_hint = (List.hd (String.split_on_char '.' filename)) in
      (*parse and print with new keyword to dest*)
      let bddl_spec = Fm.read_file domian_file ^ Fm.read_file  filepath ^ "\n" in
      let lex_string = Lexing.from_string bddl_spec in
      let prg = Parser.program Lexer.token lex_string in
      let rewritten = Pp.str_of_spec prg in
      (*let _ = print_endline source_dir in*)
      let fixed = (if source_dir = "benchmarks/GDDL_models/evader_pursuer/" 
        then (print_endline "found evader!"; rewritten ^ "breaker \n")
        else rewritten) in
      Fm.write_to_file (dest_dir ^ name_hint ^ ".ig") fixed;
      ) file_names 

let _ = 
  (*writing all new specs*)
  ignore (Sys.command ("rm -rf test_bench && mkdir test_bench"));
  Array.iter (fun fn ->
        let dir = "test_bench/" ^ fn in
        ignore (Sys.command ("mkdir " ^ dir));
        rewr_dir ("benchmarks/GDDL_models/" ^ fn ^ "/") (dir ^ "/"))
        (Sys.readdir "benchmarks/GDDL_models");
  
  (*testing that they can still be parsed*)
  let bddl_spec = Fm.read_file "test_bench/evader_pursuer/3x3_3_e-3-3_p-2-2.ig" in
  let lex_buf = Lexing.from_string bddl_spec in
  let spec = Parser.program Lexer.token lex_buf in
  let content1, _ = Fm.spec_to_string spec in
  Fm.write_to_file ("fixed" ^ ".mcrl2") content1

