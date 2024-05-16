(** Function to read the contents of a file into a string *)
let read_file filename =
  let channel = open_in filename in
  let size = in_channel_length channel in
  let content = really_input_string channel size in
  close_in channel;
  content

(** path -> content - writes content to the file at path *)
let write_to_file (path : string) (content : string) =
  let oc = open_out path in
  Printf.fprintf oc "%s" content;
  close_out oc

(*translate spec into 2 copies of mcrl2 spec, with comments for the necessary mcf files*)
let spec_to_string spec =
  let tspec, starting = Codegen.trans_spec spec "Game" in
  let mcf1, mcf2 = if starting = "black"
    then "%black_starting \n" , "%white_second \n" 
    else "%white_starting \n" , "%black_second \n" 
  in
  mcf1 ^ (Pretty_mcrl.string_of_spec tspec), mcf2 ^ (Pretty_mcrl.string_of_spec tspec)
  
(** domain -> problem -> fname - compiles the domain and problem to a file*)
let compile_specification filepath fname =
  let bddl_spec = read_file filepath in
  let lex_buf = Lexing.from_string bddl_spec in
  let spec = Parser.program Lexer.token lex_buf in
  let content1, content2 = spec_to_string spec in
  write_to_file (fname ^ "_1" ^ ".mcrl2") content1;
  write_to_file (fname ^ "_2" ^ ".mcrl2") content2
