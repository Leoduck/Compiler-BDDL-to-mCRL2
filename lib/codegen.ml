open Ast
open Mcrl2

type env = {
  action_enum : id list ref;
  mappings : idDecl list ref;
  eqns : eqnDecl list ref;
  starting_player : string ref
}

(** Convert pred to mcr id *)
let pred_to_id (p : pred) =
  match p with
  | Black -> Id "black"
  | White -> Id "white"
  | Open -> Id "open"

(** Translate pos expression to eqiv data expr *)
let trans_int_exp (var : string) (e : int_exp) : dataExpr =
  let exp op n = FunExpr (Id "max" , [Number 0L; DataBinop (op, Id var, Number n)]) in
  match e with
  | PlusExp { int = n } -> exp Add n
  | MinusExp { int = n } -> exp Sub n

(** Translate pos arguments to tuple of equiv data expr *)
let trans_pos (e1 : e1) (e2 : e2) =
  let me1 =
    match e1 with
    | Xmin -> Number 0L
    | Xmax -> FunExpr (Id "max" ,[Number 0L; DataBinop (Sub, Id "xmax", Number 1L)]) 
    | Integer { int = n } -> Number (Int64.of_int((Int64.to_int n)-1))
    | XExp iexp -> (
        match iexp with Some ex -> trans_int_exp "x" ex | None -> Id "x")
  in
  let me2 =
    match e2 with
    | Ymin -> Number 0L
    | Ymax -> FunExpr (Id "max" ,[Number 0L; DataBinop (Sub, Id "ymax", Number 1L)])
    | Integer { int = n } -> Number (Int64.of_int(Int64.to_int(n)-1))
    | YExp iexp -> (
        match iexp with Some ex -> trans_int_exp "y" ex | None -> Id "y")
  in
  (me1, me2)

(** translate condition to expr like get(x,y) == pred *)
let trans_condition (cond : Ast.cond) =
  let exp op e1 e2 pred = DataBinop (op, FunExpr (Id "get", [ e1; e2 ; Id "b"]), pred_to_id pred) in
  match cond with
  | Condition { pred; place = e1, e2 } -> let me1, me2 = trans_pos e1 e2 in exp Eq me1 me2 pred
  | NotCondition { pred; place = e1, e2 } -> let me1, me2 = trans_pos e1 e2 in exp Neq me1 me2 pred

(** Translate condition to expr set(x,y,p)*)
let rec trans_condition_set conds =
  let trans_cond_params cond =
    match cond with
    | Condition { pred; place = e1, e2 } | NotCondition { pred; place = e1, e2 }
      -> (trans_pos e1 e2, pred_to_id pred)
  in
  match conds with
  | [] -> Id "b"
  | hd :: tl ->
      let params = trans_cond_params hd in
      FunExpr( Id "set",[fst (fst params); snd (fst params); snd params;trans_condition_set tl;] )

  (*Takes in a cond and returns potential add or minus operations -> 0L if none *)
  let int_in_place (cond:Ast.cond) = 
    let (Condition { pred = _; place } | NotCondition { pred = _; place }) = cond in
    let get_int (exp:int_exp):int =
        match exp with
        | PlusExp {int = n} -> (Int64.to_int n) 
        | MinusExp {int = n} -> -(Int64.to_int n)
    in
    let x = match (fst place) with
    | XExp e -> (match e with Some e -> get_int e | None -> 0) 
    | _ -> 0
    in
    let y = match (snd place) with
    | YExp e -> (match e with Some e -> get_int e | None -> 0)
    | _ -> 0
    in
    (x,y) 


(** Makes bounding expressions from a list of conditions *)
let make_bounds (conds:Ast.cond list):dataExpr list =
    (*find minimum and maximum value for x and y in list - for use in making bounds*) 
    let find_min_max lst =
        let min_fst, max_fst, min_snd, max_snd =
            List.fold_left
            (fun (min_fst, max_fst, min_snd, max_snd) (x,y) ->
                (min min_fst x, max max_fst x, min min_snd y, max max_snd y))
            (0, 0, 0, 0) lst
            in
        ((min_fst, max_fst),(min_snd, max_snd))
    in
    (*given min max bound for a variable - generate conditions to avoid oob*)
    let make_bound (min_max:int * int) (cord:string) =
        let minimum = fst min_max in
        let maximum = snd min_max in
        let min_bound =  
            if minimum = 0 then [] 
            else  [DataBinop (Gte, Id cord, Number (Int64.of_int (-minimum)))]
        in
        let max_bound = 
            if maximum = 0 then [] 
            else  [DataBinop (Lt, Id cord, DataBinop(Sub, Id (cord ^ "max"), Number (Int64.of_int maximum)))]
        in
        min_bound @ max_bound
    in
    let ops_cond = List.map int_in_place conds in 
    let min_max = find_min_max ops_cond in
    let x_bounds = make_bound (fst min_max) "x" in
    let y_bounds = make_bound (snd min_max) "y" in
    x_bounds @ y_bounds 

(*helper functions for concatinating many dataexps / sortexps using some seperator*)
let build_dataexpr op conds = 
  List.fold_left (fun acc elem -> DataBinop (op, acc, elem)) (List.hd conds) (List.tl conds)

let hash_sortexpr sorts =
  List.fold_left (fun acc elem -> Hash (acc, elem)) (List.hd sorts) (List.tl sorts)

(*translate list of goal conditions into expr like tcond1 && tcond2 && ... *)
let trans_goal (goal : Ast.goal) (col : string) (quant_bounds)=
  match goal with
  | Goal {conditions} ->
    (*maker-maker goal*)
    let gbounds = make_bounds conditions in
    let tconds = List.map trans_condition conditions in
    build_dataexpr And (DataBinop (Eq, Id "p", Id col) :: tconds @ gbounds @ quant_bounds)
  | NoGoal ->
    (*single breaker goal*)
    let other string = if string = "white" then "black" else "white" in
    let bounds = [DataBinop(Lt,Id "x",Id "xmax"); DataBinop(Lt,Id "y",Id"ymax")] in                       
    let data = build_dataexpr And (FunExpr(Id "isLegal",[Id "x";Id "y";Id "a";Id (other col);Id "b"]) :: bounds) in
    let ex_black = NotUnop (QuantExpr (Exists,[(["a"],(NamedSort "Action_enum"));(["x"; "y"],Nat)],data)) in
    DataBinop (And, ex_black , DataBinop (Eq,Id "p", Id col))

let make_breaker color = 
    (*double breaker goal*)
    let other string = if string = "white" then "black" else "white" in
    let data = build_dataexpr And [DataBinop(Lt,Id "x",Id "xmax"); 
                                   DataBinop(Lt,Id "y",Id"ymax"); 
                                   FunExpr(Id "isLegal",[Id "x";Id "y";Id "a";Id (other color);Id "b"]); 
                                   ]
    in
    DataBinop(And, NotUnop(QuantExpr(Exists,[(["a"],(NamedSort "Action_enum"));(["x"; "y"],Nat)], data)), DataBinop (Eq,Id "p", Id color))

let make_breaker_breaker =
    let bwin = make_breaker "black" in
    let wwin = make_breaker "white" in
    build_dataexpr Or (bwin :: [wwin])

let make_with_maker bgoals wgoals =
  (*figure out if exists needed*)
  let all_cond_goals = List.flatten 
                            (List.map 
                                (fun goal -> match goal with |Goal {conditions} -> conditions ) 
                            (List.filter (fun goal -> match goal with |Goal _ -> true | NoGoal -> false) (wgoals @ bgoals))) in
  let free = List.fold_left (
    fun acc elem -> 
      match elem with
      | Condition { place = e1, e2; _ } | NotCondition { place = e1, e2; _ }-> (
      match (e1, e2) with
      | XExp _, YExp _ -> (true, true)
      | XExp _, _ -> (true, snd acc)
      | _, YExp _ -> (snd acc, true)
      | _ -> acc))
    (false, false) all_cond_goals in
  let bound_exprs, vars = match free with
    | (true,true)   -> [DataBinop (Lt, Id "x", Id "xmax"); 
                        DataBinop (Lt, Id "y", Id "ymax")], [(["x";"y"],Nat)]
    | (true,false)  -> [DataBinop (Lt, Id "x", Id "xmax")], [(["x"],Nat)]
    | (false, true) -> [DataBinop (Lt, Id "y", Id "ymax")], [(["y"],Nat)]
    | _ ->  [], []  
  in 
  (*translate all goals to big and expressions / breaker exps*)
  let tbgs = List.fold_left (fun acc elem -> trans_goal elem "white" bound_exprs :: acc) [] wgoals in
  let twgs = List.fold_left (fun acc elem -> trans_goal elem "black" bound_exprs :: acc) [] bgoals in
  let all_tgs = tbgs @ twgs in
  (*concat all with or*)
  let big_or_expr = build_dataexpr Or all_tgs in
  if List.length bound_exprs <> 0 
    then QuantExpr (Exists , vars, big_or_expr)
    else big_or_expr

let make_board init xmax ymax = 
  (*translate integer position to integers *)
  let place_to_int (place : e1 * e2) =
    match place with
    | Integer { int = x }, Integer { int = y } -> (Int64.to_int x-1, Int64.to_int y-1)
    | _ -> (0, 0)
  in

  (*translate conditions to a tuples ((x,y),pred)*)
  let trans_cond cond  =
    let (Condition { pred; place } | NotCondition { pred; place }) = cond in
    (place_to_int place, pred_to_id pred)
  in
  let coord_col = List.map trans_cond init in

  (*building row -ind- of the board *)
  let build_row ind cond_triples xmax =
    List.rev (List.fold_left
         (fun acc arg ->
           let elem_opt = List.find_opt (fun (co, _) -> co = (arg, ind)) cond_triples in
           match elem_opt with
           | None -> Id "open" :: acc
           | Some (_, col) -> col :: acc)
      [] (List.init xmax (fun i -> i)))
  in  
    List.rev (List.fold_left
    (fun acc arg -> let row = build_row arg coord_col (Int64.to_int xmax) in
      ListExpr row :: acc)
    [] (List.init (Int64.to_int ymax) (fun i -> i)))

let trans_prob (prob : Ast.problem) env =
  let (Problem { boardsize = xmax, ymax; init; bgoals; wgoals ; bfirst}) = prob in
  (*boardsize*)
  let bs_mappings = [ (["xmax"; "ymax" ], Nat) ] in
  let bs_eqns = [(None, Id "xmax", Number xmax);
                 (None, Id "ymax", Number ymax);]
  in

  (*building board + adding mappings*)
  let mboard = ListExpr (make_board init xmax ymax) in
  let board_mapping = ([ "init_board" ], NamedSort "Board") in
  let board_eqn : eqnDecl = (None, Id "init_board", mboard) in

  (*goals*)
  let win_mapping =([ "did_win" ],Arrow
        ( Hash (NamedSort "Piece", NamedSort "Board"),
          NamedSort "Bool" ) )
  in
  (*diffenrentiating maker-maker, maker-breaker*)
  let noWhite = List.mem NoGoal wgoals in
  let noBlack = List.mem NoGoal bgoals in
  (*if both players breaker (noGoal in list) -> breaker breaker case*)
  let goal_exp = if noWhite && noBlack
    then  make_breaker_breaker
    else make_with_maker bgoals wgoals
  in 
  let goal_eqn = (None, FunExpr (Id "did_win", [Id "p"; Id "b"]),goal_exp) in 

  (*updating environment*)
  let n_mappings = (win_mapping :: board_mapping :: bs_mappings) @ !(env.mappings) in
  let n_eqns = (goal_eqn :: board_eqn :: bs_eqns) @ !(env.eqns) in

  env.mappings := n_mappings;
  env.eqns := n_eqns;
  env.starting_player := (if not bfirst then "white" else "black");
  env

let sanitize_name name =
  String.map (fun c -> if c <> '-' then c else '_') name

(** Translate an action into isLegal and move for player*)
let trans_action (act : Ast.action) player env =
  let (Action {name; pre; eff }) = act in

  (*Adding the action to the enumarator*)
  let current_actions = !(env.action_enum) in
  let sanitized_name = sanitize_name name in
  if not (List.mem sanitized_name current_actions) then env.action_enum := sanitized_name :: current_actions;
  let args p : dataExpr list = [ Id "x"; Id "y"; Id sanitized_name; Id p; Id "b" ] in
  let args_move : dataExpr list = [ Id "x"; Id "y"; Id sanitized_name; Id "b" ] in

  (* Make isLegal def from pre*)
  let pre_conds = List.map trans_condition pre in
  let bounds = make_bounds (pre @ eff) in
  let isLegal_def : eqnDecl = (None, FunExpr (Id "isLegal", args player), build_dataexpr And (bounds @ pre_conds))
  in
  (* Make move def from eff*)
  let move_def : eqnDecl = (None, FunExpr (Id ("move" ^ player), args_move), trans_condition_set eff)
  in
  let current_eqn = !(env.eqns) in
  env.eqns := move_def :: isLegal_def :: current_eqn;
  env

(**making additional isLegal cases for non-mirrored moves*)
let make_extra_cases bacts wacts = 
  let to_names acts = List.fold_left (fun acc act -> match act with |Ast.Action {name ;_} -> 
    sanitize_name name :: acc ) [] acts in
  
  let to_eqns ids opp = List.fold_left (fun acc name -> 
    (None, FunExpr(Id "isLegal", [Id "x"; Id "y"; Id name ; Id opp ; Id "b"]), Id "false") :: acc) [] ids in
  
  let b_act_names = to_names bacts in
  let w_act_names = to_names wacts in
  let only_black = List.fold_left (fun acc name -> if List.mem name w_act_names then acc else name :: acc) [] b_act_names in
  let only_white = List.fold_left (fun acc name -> if List.mem name b_act_names then acc else name :: acc) [] w_act_names in
  (to_eqns only_black "white" , to_eqns only_white "black")

(** Translate domain into isLegal and move mappings and eqns*)
let trans_domain (domain : Ast.domain) env =
  (*for making extra isLegal cases*)

  let (Domain { bactions; wactions }) = domain in
  let not_black_eqns , not_white_eqns = make_extra_cases bactions  wactions in

  (*regular translations*)
  let env = List.fold_left (fun acc elem -> trans_action elem "black" acc) env bactions in
  let env = List.fold_left (fun acc elem -> trans_action elem "white" acc) env wactions in
  let args_move : dataExpr list = [ Id "x"; Id "y"; Id "a"; Id "b" ] in
  let move_eqn : eqnDecl =
    ( None,
      FunExpr (Id "move", [ Id "x"; Id "y"; Id "a"; Id "p"; Id "b" ]),
      IfExpr
        ( DataBinop (Eq, Id "p", Id "white"),
          FunExpr (Id "movewhite", args_move),
          FunExpr (Id "moveblack", args_move) ) )
  in
  let current_maps = !(env.mappings) in
  let current_eqns = !(env.eqns) in
  let hashes = hash_sortexpr [Nat ; Nat ; NamedSort "Action_enum"; NamedSort "Piece" ; NamedSort "Board"] in
  let isLegal_map = (["isLegal"],Arrow (hashes,NamedSort "Bool")) in
  let move_map = (["move"], Arrow (hashes,NamedSort "Board")) in
  let movewb_map =(["movewhite"; "moveblack" ], 
                   Arrow (hash_sortexpr [Nat; Nat; NamedSort "Action_enum"; NamedSort "Board"],NamedSort "Board")) in
  env.mappings := movewb_map :: move_map :: isLegal_map :: current_maps;
  env.eqns := move_eqn :: current_eqns @ not_white_eqns @ not_black_eqns;
  env

(**translate given specification, returns mCRL2 AST and the starting player*)
let trans_spec (spec : Ast.specification)(gamename : string) : specification * string =
  let (Spec { domain; problem }) = spec in
  let empty_env : env =
    {
      action_enum = ref [];
      mappings = ref RuntimeBindings.static_maps.maps;
      eqns = ref RuntimeBindings.static_maps.eqns;
      starting_player = ref "black"
    }
  in
  let dom_env = trans_domain domain empty_env in
  let final_env = trans_prob problem dom_env in
  let final_maps : map_spec =
    { maps = !(final_env.mappings);
      vars = RuntimeBindings.static_maps.vars;
      eqns = !(final_env.eqns);}
  in

  let action_enum_sort : projDecl = ("Action_enum", Struct !(final_env.action_enum)) in
  let s : sort_spec = { sorts = action_enum_sort :: RuntimeBindings.static_sorts.sorts } in
  let p : proc_spec = RuntimeBindings.static_process (gamename) in
  let i : init_spec = { proc_id = gamename; init_board = "init_board"; starting_p = !(final_env.starting_player)}
  in
  {sorts = s; maps = final_maps; proc = p; init = i} , (!(final_env.starting_player))
