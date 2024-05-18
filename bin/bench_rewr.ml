open Lib
module Pp = Pretty_bddl
module Fm = File_manager

(*file used for rewriting BDDL specifications for benchmarking to include breakers*)

(** Function to find the line in a string and insert a string on the next line *)
let add_line file line to_insert =
  let lines = String.split_on_char '\n' file in
  let rec aux acc = function
    | [] -> List.rev acc
    | hd :: tl ->
      if hd = line then
        aux (to_insert :: hd :: acc) tl
      else
        aux (hd :: acc) tl
  in
  String.concat "\n" (aux [] lines)

(** Adds the breaker keyword to the *)
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
      (*adding breaker keywords where necessary*)
      let rewritten = Pp.str_of_spec prg in
      (*special handling of evader-pursuer and breakthrough*)
      let fixed = (
          if (source_dir = "benchmarks/GDDL_models/evader_pursuer/" ) 
            then add_line rewritten "#whitegoal" "breaker"
          else if  source_dir = "benchmarks/GDDL_models/breakthrough/" || source_dir = "benchmarks/GDDL_models/breakthrough-second-player/" 
            then add_line (add_line rewritten "#blackgoal" "breaker") "#whitegoal" "breaker"
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
  (*let bddl_spec = Fm.read_file "test_bench/evader_pursuer/3x3_3_e-3-3_p-2-2.ig" in
  let lex_buf = Lexing.from_string bddl_spec in
  let spec = Parser.program Lexer.token lex_buf in
  let content1, _ = Fm.spec_to_string spec in
  Fm.write_to_file ("fixed" ^ ".mcrl2") content1*)

