open Mcrl2_ast

(** static definitions for translation*)

let static_sorts:sort_spec = 
    { sorts = [ ("Piece", Struct ["black";"white";"open"])
              ; ("Row"  , List (NamedSort "Piece"))
              ; ("Board", List (NamedSort "Row"))
              ]
    }   

let static_maps:map_spec =
    { maps = [ (["other"], Arrow (NamedSort "Piece", NamedSort "Piece"))
             ; (["get"]  , Arrow (Hash (Hash (Nat,Nat), (NamedSort "Board")), (NamedSort "Piece")))
             ; (["get"]  , Arrow (Hash (Nat,(NamedSort "Row")), NamedSort ("Piece")))
             ; (["set"]  , Arrow (Hash (Hash (Hash (Nat,Nat),(NamedSort "Piece")),(NamedSort "Board")),(NamedSort "Board")))
             ; (["set"]  , Arrow (Hash (Hash (Nat, NamedSort "Piece"),(NamedSort "Row")),(NamedSort "Row")))
             ]
    ; vars = [ (["b"], (NamedSort "Board"))
             ; (["r"], (NamedSort "Row"))
             ; (["p";"p'"], (NamedSort "Piece"))
             ; (["x";"y"], Nat)
             ; (["a"], (NamedSort "Action_enum"))
             ] 
    ; eqns = [ (None , FunExpr (Id("other"), [Id "white"]), (Id "black"))
             ; (None , FunExpr (Id("other"), [Id "black"]), (Id "white"))
             ; (None , FunExpr (Id "get", [Id "x";Id "y"; Id "b"]), (FunExpr (Id "get", [Id "x"; DataBinop (Dot,Id "b",Id "y")])))
             ; (None , FunExpr (Id "get", [Id "x";Id "r"]), DataBinop (Dot,Id "r",Id "x"))
             ; ( Some (DataBinop (Eq,Id "y", Number 0L))
               , FunExpr (Id "set", [Id "x";Id "y";Id "p"; DataBinop (Cons ,Id "r", Id "b")])
               , DataBinop (Cons,FunExpr (Id "set", [Id "x"; Id "p"; Id "r"]),Id "b")
               )
             ; ( Some (DataBinop (Gt,Id "y", Number 0L))
               , FunExpr (Id "set", [Id "x"; Id "y"; Id "p"; DataBinop (Cons ,Id "r", Id "b")])
               , DataBinop (Cons, Id "r",FunExpr (Id "set", [Id "x"; FunExpr (Id "max", [Number 0L; DataBinop (Sub, Id "y",Number 1L)]); Id "p"; Id "b"]))
               )
             ; (Some (DataBinop (Eq, Id "x", Number 0L)),
               FunExpr (Id "set", [Id "x"; Id "p"; DataBinop (Cons , Id "p'", Id "r")]),
               DataBinop (Cons, Id "p", Id "r"))

             ;(Some (DataBinop (Gt , Id "x", Number 0L)),
               FunExpr (Id "set", [Id "x"; Id "p"; DataBinop (Cons , Id "p'", Id "r")]),
               DataBinop (Cons, Id "p'", FunExpr (Id "set", [FunExpr (Id "max", [Number 0L; DataBinop (Sub, Id "x", Number 1L)]);
                                                                            Id "p"; Id "r"])))
             ]
    }
let static_process (gamename : string) : proc_spec =
  { acts = [(["move"], Hash (Hash (Nat, Nat), NamedSort "Action_enum"));
            (["win"],NamedSort "Piece")]; 
      proc_id = (gamename, [(["board"],NamedSort "Board");
                            (["player"],NamedSort "Piece")]); 
      proc = ITE (FunExpr (Id "did_win",[FunExpr (Id "other",[Id "player"]);Id "board"]),
                  PBinop (Dot, Action ("win", [FunExpr (Id "other",[Id "player"])]), 
                               Action (gamename,[Id "board"; Id "player"])),
                    Some (SumExpr ([(["d"],NamedSort "Action_enum");(["x";"y"],Nat)],
                                 ITE (
                                        DataBinop (And, DataBinop (Lt, Id "x", Id "xmax"),
                                          DataBinop (And, DataBinop (Lt, Id "y", Id "ymax"),
                                          FunExpr (Id "isLegal", [Id "x"; Id "y"; Id "d"; Id "player"; Id "board" ]))),
                                          PBinop (Dot, Action ("move", [Id "x";Id "y"; Id "d"])
                                                   , Action (gamename , [FunExpr (Id "move", [Id "x"; Id "y"; Id "d"; Id "player" ; Id "board"]);
                                                                         FunExpr (Id "other", [Id "player"])])),
                                        None
                                 ) ))) 
  }

let starting_mu_calc col = 
  Printf.sprintf "mu X.exists d: Action_enum , x,y:Nat . val(x<xmax && y<ymax) && 
  (<move(x,y,d)>
      (<win(%s)> true ||
          (<true>true && forall d: Action_enum, i,j: Nat . 
              (val(i<xmax && j<ymax) => [move(i,j,d)]X
              )
          )
      )
  )" col
let second_mu_calc col = 
  Printf.sprintf "mu X.(<true>true && (
    forall d: Action_enum, x,y: Nat .(
        val(x<xmax && y<ymax) => (
            [move(x,y,d)](
                [win(other(%s))] false && exists p: Action_enum, i,j: Nat . val(i<xmax && j<ymax) &&(
                    <move(i,j,p)>(<win(%s)> true || X)
                )
            )
        )
    )
)
)" col col
