module Mcrl = Lib.Mcrl2
module Run = Lib.RuntimeBindings

let spec:Mcrl.specification = 
    let open Mcrl in
    (*let sorts = {sorts = []} in
    let m = { maps = []
            ; vars = []
            ; eqns = []
            } in *)
    let proc = Action ("move", []) in
    let p = { acts = []
            ; proc_id = "Game", []
            ; proc = proc  
            } in
    let i = { proc_id = "Game"
            ; init_board = "init_board"
            ; starting_p = "b"
            } in 
    {sorts = Run.static_sorts 
    ; maps = Run.static_maps
    ; proc = p 
    ; init = i
    }

let write_to_file (path: string)(content: string) =
    let oc = open_out path in
    Printf.fprintf oc "%s" content;
    close_out oc

let _ =
    write_to_file "test.mcrl2" (Mcrl.string_of_spec spec)
