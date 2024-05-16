open Lib

(*file used to compile all BDDL specifications for benchmarking*)

let print_file filepath =
  Printf.printf "Processing file: %s\n" filepath

(* Function to apply the process_file function to all files in a directory *)
let compile_directory directory_path place_path =
  let files = Sys.readdir directory_path in
  Array.iter (fun filename ->
        let filepath = Filename.concat directory_path filename in
        let fn = (List.hd (String.split_on_char '.' filename)) in
        print_file filepath;
        File_manager.compile_specification filepath (place_path ^ fn)
  ) files

let _ = 
    ignore (Sys.command ("rm -rf translated && mkdir translated"));
    Array.iter (fun fn ->
        let dir = "translated/" ^ fn in
        ignore (Sys.command ("mkdir " ^ dir));
        compile_directory ("test_bench/" ^ fn ^ "/") (dir ^ "/") 
    ) (Sys.readdir "test_bench");
    (*generating mcf's*)
    File_manager.write_to_file  "translated/black_starting.mcf" (RuntimeBindings.starting_mu_calc "black");
    File_manager.write_to_file  "translated/white_starting.mcf" (RuntimeBindings.starting_mu_calc "white");
    File_manager.write_to_file  "translated/black_second.mcf" (RuntimeBindings.second_mu_calc "black");
    File_manager.write_to_file  "translated/white_second.mcf" (RuntimeBindings.second_mu_calc "white")
