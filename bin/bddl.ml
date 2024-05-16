open Lib
module Fm = File_manager
module Rtb = RuntimeBindings
module Ast = Ast

let find_boardsize spec =
    match spec with |Ast.Spec {problem=Problem{boardsize= x,y;_};_} -> Int64.to_int x , Int64.to_int y

let gen_and_return_mcfs col1 col2 =
  let _ = Fm.write_to_file (col1 ^ "_starting.mcf") (Rtb.starting_mu_calc col1) in
  let _ = Fm.write_to_file (col2 ^ "_second.mcf") (Rtb.second_mu_calc col2) in
  (col1 ^ "_starting.mcf") , (col2 ^ "_second.mcf")

(** domain -> problem -> target : compile and solve game instance specified in given BDDL specification*)
let _ =
  (*NOTICE - translation will not work if breaker keyword is not used where necessary in the specification*)
  try (
    (*handling arguments*)
    let domain_file = (Array.get Sys.argv 1) in
    let problem_file = (Array.get Sys.argv 2) in
    let target_name = (Array.get Sys.argv 3) in
    
    (*compiling specification*)
    let conc_bddl = Fm.read_file domain_file ^ Fm.read_file  problem_file ^ "\n" in
    let lex_buf = Lexing.from_string conc_bddl in
    let spec = Parser.program Lexer.token lex_buf in
    let tspec, starting = Codegen.trans_spec spec "Game" in

      (*finding specifics*)
      let xmax, ymax = find_boardsize spec in 
      let mcf1, mcf2 = if starting = "black"
        then gen_and_return_mcfs "black" "white"
        else gen_and_return_mcfs "white" "black"
      in
    Fm.write_to_file target_name (Pretty_mcrl.string_of_spec tspec);
    print_endline ("specification has been compiled to " ^ target_name);

    (*solving*)
    let command mcrl2 mcf = Printf.sprintf "mcrl22lps %s | lpsparunfold -sBoard -n%i | lpsparunfold -sRow -n%i | lpssuminst | lpsconstelm -s -t | " mcrl2 xmax ymax ^
                            Printf.sprintf "lps2pbes -m -s -f%s | pbessolve -s2 " mcf in

    print_endline "checking winning strategy for starting player...";
    let out = Unix.open_process_in (command (target_name) (mcf1)) in
    print_endline (input_line out);

    print_endline "checking winning strategy for second player...";
    let out = Unix.open_process_in (command (target_name) (mcf2)) in
    print_endline (input_line out);

    (*cleanup*)
    ignore (Sys.command ("rm " ^ mcf1));
    ignore (Sys.command ("rm " ^ mcf2));
    )

  with Invalid_argument s -> print_endline s