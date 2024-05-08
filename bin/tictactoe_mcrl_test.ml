module Mcr = Lib.Mcrl2


let sorts = [ ("Piece", Mcr.Struct ["empty";"naught";"cross"])
        ; ("Row", Mcr.List (Mcr.NamedSort "Piece"))
        ; ("Board", Mcr.List (Mcr.NamedSort "Row"))] 

let mps = [(["empty_board"] , Mcr.NamedSort "Board")
          ;(["other"] , Mcr.Arrow (Mcr.NamedSort "Piece", Mcr.NamedSort "Piece"))
          ;(["get"] , Mcr.Arrow (Mcr.Hash (Mcr.Hash (Mcr.Nat , Mcr.Nat), Mcr.NamedSort "Piece") , Mcr.NamedSort "Piece"))
          ;(["get"] , Mcr.Arrow (Mcr.Hash (Mcr.Nat , Mcr.NamedSort "Row"), Mcr.NamedSort "Piece"))
          ;(["set"] , Mcr.Arrow (Mcr.Hash (Mcr.Hash (Mcr.Nat , Mcr.Nat), Mcr.Hash (Mcr.NamedSort "Piece", Mcr.NamedSort "Board")), Mcr.NamedSort "Board"))
          ;(["set"], Mcr.Arrow (Mcr.Hash (Mcr.Hash (Mcr.Nat , Mcr.NamedSort "Piece"), Mcr.NamedSort "Row"), Mcr.NamedSort "Row"))
          ;(["did_win"], Mcr.Arrow (Mcr.Hash (Mcr.NamedSort "Piece", Mcr.NamedSort "Board"), Mcr.Bool))
          ]
let vs = [(["b"],Mcr.NamedSort "Board")
         ;(["r"],Mcr.NamedSort "Row")
         ;(["p";"p'"],Mcr.NamedSort "Piece")
         ;(["i";"j"],Mcr.Nat)
         ]
let es = [(None, Mcr.Id ("empty_board") , Mcr.ListExpr ([Mcr.ListExpr ([Mcr.Id "empty";Mcr.Id "empty";Mcr.Id "empty"]); 
                                                Mcr.ListExpr ([Mcr.Id "empty";Mcr.Id "empty";Mcr.Id "empty"]); 
                                                Mcr.ListExpr ([Mcr.Id "empty";Mcr.Id "empty";Mcr.Id "empty"])]))

         ;(None , Mcr.FunExpr (Mcr.Id "other", [Mcr.Id "naught"]), Mcr.Id "cross")

         ;(None , Mcr.FunExpr (Mcr.Id "other", [Mcr.Id "cross"]), Mcr.Id "naught")

         ;(None, Mcr.FunExpr (Mcr.Id "get", [Mcr.Id "i"; Mcr.Id "j"; Mcr.Id "b"]),
           Mcr.FunExpr (Mcr.Id "get", [Mcr.Id "i"; Mcr.DataBinop (Mcr.Dot, Mcr.Id "b", Mcr.Id "j")]))

         ;(None, Mcr.FunExpr (Mcr.Id "get", [Mcr.Id "i"; Mcr.Id "r"]), Mcr.DataBinop (Mcr.Dot , Mcr.Id "r" , Mcr.Id "i"))

         ;(Some (Mcr.DataBinop (Mcr.Eq , Mcr.Id "j", Mcr.Number 0L)),
           Mcr.FunExpr (Mcr.Id "set", [Mcr.Id "i"; Mcr.Id "j"; Mcr.Id "p"; Mcr.DataBinop (Mcr.Cons , Mcr.Id "r", Mcr.Id "b") ]),
           Mcr.DataBinop (Mcr.Cons, Mcr.FunExpr (Mcr.Id "set", [Mcr.Id "i"; Mcr.Id "p"; Mcr.Id "r"]), Mcr.Id "b"))
         
         ;(Some (Mcr.DataBinop (Mcr.Gt , Mcr.Id "j", Mcr.Number 0L)),
         Mcr.FunExpr (Mcr.Id "set", [Mcr.Id "i"; Mcr.Id "j"; Mcr.Id "p"; Mcr.DataBinop (Mcr.Cons , Mcr.Id "r", Mcr.Id "b") ]),
         Mcr.DataBinop (Mcr.Cons, Mcr.Id "r", Mcr.FunExpr (Mcr.Id "set", [Mcr.Id "i"; 
                                                                          Mcr.FunExpr (Mcr.Id "max", [Mcr.Number 0L; Mcr.DataBinop (Mcr.Sub, Mcr.Id "j", Mcr.Number 1L)]); 
                                                                          Mcr.Id "p"; Mcr.Id "b"])))

         ;(Some (Mcr.DataBinop (Mcr.Eq, Mcr.Id "i", Mcr.Number 0L)),
           Mcr.FunExpr (Mcr.Id "set", [Mcr.Id "i"; Mcr.Id "p"; Mcr.DataBinop (Mcr.Cons , Mcr.Id "p'", Mcr.Id "r")]),
           Mcr.DataBinop (Mcr.Cons, Mcr.Id "p", Mcr.Id "r"))

         ;(Some (Mcr.DataBinop (Mcr.Gt , Mcr.Id "i", Mcr.Number 0L)),
           Mcr.FunExpr (Mcr.Id "set", [Mcr.Id "i"; Mcr.Id "p"; Mcr.DataBinop (Mcr.Cons , Mcr.Id "p'", Mcr.Id "r")]),
           Mcr.DataBinop (Mcr.Cons, Mcr.Id "p'", Mcr.FunExpr (Mcr.Id "set", [Mcr.FunExpr (Mcr.Id "max", [Mcr.Number 0L; Mcr.DataBinop (Mcr.Sub, Mcr.Id "i", Mcr.Number 1L)]);
                                                                            Mcr.Id "p"; Mcr.Id "r"])))

         ;(None, Mcr.FunExpr (Mcr.Id "did_win", [Mcr.Id "p"; Mcr.Id "b"]),
           Mcr.DataBinop (Mcr.Or , 
                Mcr.DataBinop (Mcr.Or , 
                                (*(exists i:Nat.(i<3 && get(i,0,b)==p && get(i,1,b)==p && get(i,2,b)==p))*)
                                Mcr.QuantExpr (Mcr.Exists, [(["i"],Mcr.Nat)], 
                                        Mcr.DataBinop (Mcr.And , Mcr.DataBinop (Mcr.Lt , Mcr.Id "i", Mcr.Number 3L), 
                                            Mcr.DataBinop (Mcr.And, Mcr.DataBinop (Mcr.Eq, Mcr.FunExpr (Mcr.Id "get", [Mcr.Id "i"; Mcr.Number 0L;  Mcr.Id "b"]), Mcr.Id "p"),
                                                Mcr.DataBinop (Mcr.And, Mcr.DataBinop (Mcr.Eq, Mcr.FunExpr (Mcr.Id "get", [Mcr.Id "i"; Mcr.Number 1L; Mcr.Id "b"]), Mcr.Id "p") , 
                                                                        Mcr.DataBinop (Mcr.Eq, Mcr.FunExpr (Mcr.Id "get", [Mcr.Id "i"; Mcr.Number 2L; Mcr.Id "b"]), Mcr.Id "p"))))),
                                (*(exists j:Nat.(j<3 && get(0,j,b)==p && get(1,j,b)==p && get(2,j,b)==p))*)
                                Mcr.QuantExpr (Mcr.Exists, [(["j"],Mcr.Nat)], 
                                        Mcr.DataBinop (Mcr.And , Mcr.DataBinop (Mcr.Lt , Mcr.Id "j", Mcr.Number 3L), 
                                            Mcr.DataBinop (Mcr.And, Mcr.DataBinop (Mcr.Eq, Mcr.FunExpr (Mcr.Id "get", [Mcr.Number 0L; Mcr.Id "j"; Mcr.Id "b"]), Mcr.Id "p"),
                                                Mcr.DataBinop (Mcr.And, Mcr.DataBinop (Mcr.Eq, Mcr.FunExpr (Mcr.Id "get", [Mcr.Number 1L; Mcr.Id "j"; Mcr.Id "b"]), Mcr.Id "p") , 
                                                                        Mcr.DataBinop (Mcr.Eq, Mcr.FunExpr (Mcr.Id "get", [Mcr.Number 2L; Mcr.Id "j"; Mcr.Id "b"]), Mcr.Id "p")))))) , 
                                (*(get(0,0,b)==p && get(1,1,b)==p && get(2,2,b)==p)*)
                Mcr.DataBinop (Mcr.Or , Mcr.DataBinop (Mcr.And , 
                                                        Mcr.DataBinop (Mcr.Eq , Mcr.FunExpr(Mcr.Id "get", [Mcr.Number 0L ; Mcr.Number 0L ; Mcr.Id "b"]) , Mcr.Id "p"), 
                                                        Mcr.DataBinop (Mcr.And, 
                                                                Mcr.DataBinop (Mcr.Eq , Mcr.FunExpr(Mcr.Id "get", [Mcr.Number 1L ; Mcr.Number 1L ; Mcr.Id "b"]) , Mcr.Id "p") , 
                                                                Mcr.DataBinop (Mcr.Eq , Mcr.FunExpr(Mcr.Id "get", [Mcr.Number 2L ; Mcr.Number 2L ; Mcr.Id "b"]) , Mcr.Id "p"))),
                                (*(get(0,2,b)==p && get(1,1,b)==p && get(2,0,b)==p)*)
                                        Mcr.DataBinop (Mcr.And , 
                                        Mcr.DataBinop (Mcr.Eq , Mcr.FunExpr(Mcr.Id "get", [Mcr.Number 0L ; Mcr.Number 2L ; Mcr.Id "b"]) , Mcr.Id "p"), 
                                        Mcr.DataBinop (Mcr.And, 
                                                Mcr.DataBinop (Mcr.Eq , Mcr.FunExpr(Mcr.Id "get", [Mcr.Number 1L ; Mcr.Number 1L ; Mcr.Id "b"]) , Mcr.Id "p") , 
                                                Mcr.DataBinop (Mcr.Eq , Mcr.FunExpr(Mcr.Id "get", [Mcr.Number 2L ; Mcr.Number 0L ; Mcr.Id "b"]) , Mcr.Id "p"))))))                                                                 
         ]

let a = [(["win"],Mcr.NamedSort "Piece"); 
         (["put"],Mcr.Hash (Mcr.NamedSort "Piece", Mcr.Hash (Mcr.Nat, Mcr.Nat)))]

let pid = ("TicTacToe",[(["board"], Mcr.NamedSort "Board");(["player"], Mcr.NamedSort "Piece")])

let p_test = Mcr.ITE (Mcr.DataBinop (Mcr.And , Mcr.DataBinop (Mcr.Lt , Mcr.Id "i", Mcr.Number 3L),
                      Mcr.DataBinop (Mcr.And , Mcr.DataBinop (Mcr.Lt , Mcr.Id "j", Mcr.Number 3L),
                                               Mcr.DataBinop (Mcr.Eq , Mcr.FunExpr (Mcr.Id "get" ,[Mcr.Id "i"; Mcr.Id "j"; Mcr.Id "board"]) , Mcr.Id "empty"))) , 
                      Mcr.PBinop (Mcr.Dot , Mcr.Action ("put", [Mcr.Id "player"; Mcr.Id "i"; Mcr.Id "j"]),
                                            Mcr.Action ("TicTacToe", [Mcr.FunExpr (Mcr.Id "set", [Mcr.Id "i"; Mcr.Id "j"; Mcr.Id "player"; Mcr.Id "board"]) ; 
                                                                      Mcr.FunExpr (Mcr.Id "other", [Mcr.Id "player"])])) , 
                      None)

let pexp = Mcr.ITE (Mcr.FunExpr (Mcr.Id "did_win", [Mcr.FunExpr (Mcr.Id "other", [Mcr.Id "player"]); Mcr.Id "board"]),
                    Mcr.PBinop  (Mcr.Dot, Mcr.Action ("win", [Mcr.FunExpr (Mcr.Id "other", [Mcr.Id "player"])]) , Mcr.Delta),
                    Some(Mcr.SumExpr ([(["i";"j"], Mcr.Nat)], p_test)))

let s : Mcr.sort_spec = {sorts = sorts}
let m : Mcr.map_spec = {maps = mps ;vars = vs ; eqns= es}
let p : Mcr.proc_spec = {acts = a ; proc_id = pid ; proc = pexp}
let i : Mcr.init_spec = {proc_id = "TicTacToe" ; init_board = "empty_board" ; starting_p = "cross"}

let spec : Mcr.specification = {sorts = s ; maps = m ; proc = p ; init = i}

let write_to_file (path: string)(content: string) =
  let oc = open_out path in
  Printf.fprintf oc "%s" content;
  close_out oc

let _ =
  write_to_file "test.mcrl2" (Mcr.string_of_spec spec)
