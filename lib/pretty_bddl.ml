open Bddl_ast
(** functionality for generating string of BDDL AST*)

let mapcat sep func li = (String.concat sep) (List.map func li)

let print_pred pred =
    match pred with
    | Black -> Printf.sprintf "black"
    | White -> Printf.sprintf "white"
    | Open -> Printf.sprintf "open"

let string_of_exp (exp: int_exp option):string=
    match exp with
    | Some PlusExp {int} -> Printf.sprintf "+ %s" (Int64.to_string int)
    | Some MinusExp {int} -> Printf.sprintf "- %s" (Int64.to_string int)
    | None -> ""

let print_ex e1 =
    match e1 with
    | Xmin -> Printf.sprintf "(xmin,"
    | Xmax -> Printf.sprintf "(xmax,"
    | Integer {int} -> Printf.sprintf "(%s," (Int64.to_string int)
    | XExp exp -> Printf.sprintf "(?x %s," (string_of_exp exp) 

let print_ey e2 =
    match e2 with
    | Ymin -> Printf.sprintf "ymin) "
    | Ymax -> Printf.sprintf "ymax) "
    | Integer {int} -> Printf.sprintf "%s) " (Int64.to_string int)
    | YExp exp -> Printf.sprintf "?y %s) " (string_of_exp exp)
  
let print_cond cond =
    match cond with
    | Condition {pred; place} -> 
            print_pred pred ^ print_ex (fst place) ^ print_ey (snd place)
    | NotCondition {pred; place} -> 
            "NOT(" ^ print_pred pred ^ print_ex (fst place) ^ print_ey (snd place) ^  ") "

let print_action act =
    match act with
    | Action {name; pre; eff} ->
            Printf.sprintf ":action %s\n" name ^
            Printf.sprintf ":parameters %s\n" "(?x,?y)" ^
            ":precondition (" ^ (mapcat "" print_cond pre) ^ ")" ^
            "\n:effect (" ^ (mapcat "" print_cond eff) ^ ")"


let print_domain domain = 
    match domain with
    | Domain {bactions; wactions} -> 
            "#blackactions\n" ^ mapcat "\n" print_action bactions ^
            "\n#whiteactions\n" ^ mapcat "\n" print_action wactions

let print_goal goals =
    match goals with
    | Goal {conditions} -> mapcat "" print_cond conditions
    | NoGoal -> "breaker"

let print_problem problem =
    match problem with
    | Problem {boardsize; init; bgoals; wgoals; bfirst} -> 
            Printf.sprintf "#boardsize\n %s %s\n" (Int64.to_string (fst boardsize)) (Int64.to_string (snd boardsize)) ^
            "#init \n" ^  mapcat "" print_cond init ^
            (*generating breaker keyword...*)
            "\n#blackgoal\n" ^ 
            (if List.length bgoals = 0 
                then  "breaker"
                else mapcat "\n" print_goal bgoals) ^
            "\n#whitegoal\n" ^
            (if List.length wgoals = 0 
                then "breaker"
                else mapcat "\n" print_goal wgoals) ^
            (if not bfirst then "\n#blackturn \n second" else "")

        
let print_prog prog = 
    match prog with
    | Spec {domain; problem} -> 
            print_domain domain ^ "\n" ^
            print_problem problem ^
            "\n"

let str_of_spec spec =
    print_prog spec 
