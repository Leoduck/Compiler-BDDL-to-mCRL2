(**test file for verifying that the translation yields correct results for simple game instances*)

let add_path path list = List.map (fun (filename, mcf, result) -> (path ^ filename, path ^ mcf, result)) list
let tests = add_path "translated/"
    [(*
    ("breakthrough-second-player/2x4_8_1.mcrl2", "white_starting.mcf", "false");
     ("breakthrough-second-player/2x4_8_2.mcrl2", "black_second.mcf", "true");
     *)
     ("breakthrough/2x4_13_1.mcrl2", "black_starting.mcf", "false");
     ("breakthrough/2x4_13_2.mcrl2", "white_second.mcf", "true");
     ("connect-c/2x2_3_connect2_1.mcrl2", "black_starting.mcf", "true");
     ("connect-c/2x2_3_connect2_2.mcrl2", "white_second.mcf", "false");
     ("domineering/2x2_2_1.mcrl2", "black_starting.mcf", "true");
     ("domineering/2x2_2_2.mcrl2", "white_second.mcf", "false");
     ("domineering/3x3_4_2.mcrl2", "black_starting.mcf", "true");
     ("domineering/3x3_4_2.mcrl2", "white_second.mcf", "false");
     (*winning on only field*)
     ("evader_pursuer/3x3_3_e-3-3_p-2-2_1.mcrl2", "black_starting.mcf", "false");
     ("evader_pursuer/3x3_3_e-3-3_p-2-2_2.mcrl2", "white_second.mcf", "true");
     (*winning on capture*)
     ("evader_pursuer/4x4_3_e-4-1_p-2-3_1.mcrl2", "black_starting.mcf", "true");
     ("evader_pursuer/4x4_3_e-4-1_p-2-3_2.mcrl2", "white_second.mcf", "false");

     (*test big 
     ("evader_pursuer/8x8_11_e-8-1_p-2-3_1.mcrl2", "black_starting.mcf", "true");
     ("evader_pursuer/8x8_11_e-8-1_p-2-3_2.mcrl2", "white_second.mcf", "false");
     *)
     
     (*dual winnign on capture*)
     ("evader_pursuer_dual/4x4_2_e-4-1_p-1-2_1.mcrl2", "black_second.mcf", "true");
     ("evader_pursuer_dual/4x4_2_e-4-1_p-1-2_2.mcrl2", "white_starting.mcf", "false");
     ("hex/hein_04_3x3-03_1.mcrl2", "black_starting.mcf", "false");
     ("hex/hein_04_3x3-03_2.mcrl2", "white_second.mcf", "true");
     ("httt/3x3_3_domino_1.mcrl2", "black_starting.mcf", "true");
     ("httt/3x3_3_domino_2.mcrl2", "white_second.mcf", "false");
     (*adding case where neither player has winning strategy*)
     ("httt/3x3_9_fatty_1.mcrl2", "black_starting.mcf", "false");
     ("httt/3x3_9_fatty_2.mcrl2", "white_second.mcf", "false");
    ]

let check_spec spec_path mcf_path ex_res  = 
    Printf.printf "Checking %s...%!" spec_path;
    let command = Printf.sprintf "mcrl22lps %s | lps2pbes -m -s -f%s | pbessolve --threads=4 -s2 -zbreadth-first" spec_path mcf_path in
    let chan = Unix.open_process_in command in
    let out = input_line chan in 
    if out = ex_res then (Printf.printf "\x1b[32mok\x1b[0m\n"; true)
    else (Printf.printf "\x1b[31mfailed\x1b[0m\n"; false)


let run_tests () = 
  Printf.printf "\nNow running tests...\n\n";
  let results = List.map (fun (mcrl,mcf,e) -> check_spec mcrl mcf e) tests in
  let res = List.for_all (fun x -> x) results in
  Printf.printf "\nTests have run\n";
  Printf.printf "%s \n" (if res then "Tests passed \x1b[32msuccessfully\x1b[0m\n" else "Some tests \x1b[31mfailed\x1b[0m")

let _ = 
    run_tests ()

