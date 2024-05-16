open Lib
module Pp = Pretty_bddl
module Fm = File_manager

(** domain file, problem file, white breaker flag, black breaker flag
    rewrites BDDL specification to add new breaker keywords where necessary*)
let _ = 
  try (
    (*input handling*)
    let domain_file = (Array.get Sys.argv 1) in
    let problem_file = (Array.get Sys.argv 2) in
    let white_breaker = bool_of_string  (Array.get Sys.argv 3) in (*flag for "invisible" white breaker*)
    let black_breaker = bool_of_string  (Array.get Sys.argv 4) in (*flag for "invisible" black breaker*)

    (*rewrite with breaker keyword*)
    let bddl_spec = Fm.read_file domain_file ^ Fm.read_file  problem_file ^ "\n" in
    let lex_string = Lexing.from_string bddl_spec in
    let prg = Parser.program Lexer.token lex_string in
    let rewritten = Pp.str_of_spec prg in     (*standard rewrite - replacess empty and false winning conditions*)

    (*handling ambiguous breakers using flags*)
    let rewritten' = if black_breaker 
      then 
        (let reg = Str.regexp_string "#whitegoal" in
        let str_list = Str.split reg rewritten in
        (List.hd str_list) ^ "breaker \n" ^ "#whitegoal" ^List.hd (List.tl str_list))
      else rewritten in
    let rewritten'' = if white_breaker then (rewritten' ^ "breaker \n") else rewritten' in
    print_endline rewritten'';

    (*splitting specification up again and writing to seperate files*)
    let reg = Str.regexp_string "#boardsize" in
    let str_list = Str.split reg rewritten'' in
    Fm.write_to_file "new_domain.ig" (List.hd str_list);
    Fm.write_to_file "new_problem.ig" ("#boardsize" ^ List.hd (List.tl str_list));
  )
with Invalid_argument _ -> Printf.printf "Wrong number of arguments given \n"