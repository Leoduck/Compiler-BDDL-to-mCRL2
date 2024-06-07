open Mcrl2_ast

(** functionality for generating string of mCRL2 AST*)

let mapcat sep func li = (String.concat sep) (List.map func li)

(* #TODO better parentheses*)
let rec string_of_sortexpr (expr:sortExpr):string =
    match expr with 
          Bool -> "Bool"
        | Pos  -> "Pos"
        | Nat  -> "Nat"
        | Int  -> "Int"
        | Real -> "Real"
        | List  typ -> "List(" ^  (string_of_sortexpr typ) ^ ")"
        | Struct ids  -> Printf.sprintf "struct %s" (String.concat " | " ids)
        | Arrow (left, right) -> 
            Printf.sprintf "%s -> %s" (string_of_sortexpr left) (string_of_sortexpr right)
        | Hash (left, right)  ->
            Printf.sprintf "%s # %s" (string_of_sortexpr left) (string_of_sortexpr right)
        | NamedSort id -> id

let string_of_projDecl pd =
    Printf.sprintf "%s = %s ;" (fst pd) (string_of_sortexpr (snd pd))
let string_of_vardecl (decl:varDecl):string =
    Printf.sprintf "%s:%s" (String.concat "," (fst decl)) (string_of_sortexpr (snd decl))

let string_of_vardecls (decls:varDecl list)(sep: string):string =
    (mapcat sep string_of_vardecl decls)

let string_of_quant (q:quant):string =
    match q with 
          ForAll -> "forall"
        | Exists -> "exists"
        | Lambda -> "lambda"

let string_of_bop (op:bop):string =
    match op with
          And  -> "&&"
        | Or   -> "||\n"
        | Eq   -> "=="
        | Neq  -> "!="
        | Gt -> ">"
        | Lt -> "<"
        | Gte -> ">="
        | Lte -> "<="
        | Add  -> "+"
        | Sub  -> "-"
        | Mul  -> "*"
        | Div  -> "/"
        | Cons -> "|>"
        | Snoc -> "<|"
        | Dot  -> "."

let string_of_procop (op:procOp):string =
    match op with
          Plus -> "+"
        | Dot  -> "."

let rec string_of_dataexpr (expr:dataExpr):string =
    match expr with
          Id s                              -> s
        | Number n                          -> Int64.to_string n
        | QuantExpr (quant, vars, dataexpr) -> 
                Printf.sprintf "%s %s.(%s)" (string_of_quant quant)
                (string_of_vardecls vars ", ") (string_of_dataexpr dataexpr)
        | DataBinop  (op, left, right)      -> 
                let left_str = match left with
                | Id _ | Number _ |FunExpr _ -> Printf.sprintf "%s"(string_of_dataexpr left) 
                | _ -> Printf.sprintf "(%s)" (string_of_dataexpr left)
                in
                let right_str = match right with
                | Id _ | Number _ -> Printf.sprintf "%s" (string_of_dataexpr right) 
                | _ -> Printf.sprintf "(%s)" (string_of_dataexpr right) 
                in
                Printf.sprintf "%s %s %s" (left_str)
                (string_of_bop op) right_str
        |NotUnop (dataexpr) -> Printf.sprintf "!(%s)" (string_of_dataexpr dataexpr)
        | ListExpr explist -> Printf.sprintf "[%s]" (string_of_dataexps explist)
        | IfExpr (cond, th, el) ->
                Printf.sprintf "if(%s, %s, %s)"
                (string_of_dataexpr cond) (string_of_dataexpr th) (string_of_dataexpr el) 
        | FunExpr (id, explist) -> Printf.sprintf "%s(%s)" (string_of_dataexpr id) (string_of_dataexps explist)
and string_of_dataexps (exps : dataExpr list) : string = 
    mapcat ", " string_of_dataexpr exps

let string_of_eqndecl ((f,s,t): dataExpr option * dataExpr * dataExpr):string =
    match f with
    |None -> Printf.sprintf "%s = %s;" (string_of_dataexpr s) (string_of_dataexpr t)
    |Some e -> Printf.sprintf "%s -> %s = %s;" (string_of_dataexpr e) (string_of_dataexpr s) (string_of_dataexpr t)
    
let string_of_eqndecls ((decls):eqnDecl list):string =
    mapcat "\n\t " string_of_eqndecl decls

let string_of_iddecl (decl:idDecl):string =
    Printf.sprintf "%s: %s;" (String.concat "," (fst decl)) (string_of_sortexpr (snd decl))

let string_of_iddecls (decls:idDecl list):string =
    mapcat "\n\t " string_of_iddecl decls
    
let string_of_pid (pid : id * varDecl list) =
    Printf.sprintf "%s(%s)" (fst pid) (string_of_vardecls (snd pid) ", ")
    
let rec string_of_procexpr (expr:procExpr):string = 
    match expr with
      Action (id, data) -> Printf.sprintf "%s(%s)" id (mapcat "," string_of_dataexpr data)
    | SumExpr (vli, pexp) -> Printf.sprintf "sum %s.%s" (string_of_vardecls vli ",") (string_of_procexpr pexp)
    | ITE (cond, th, el) -> 
            let st_el = (match el with
            |Some el -> Printf.sprintf "\n <> (\n%s)" (string_of_procexpr el)
            |None -> "") in
            Printf.sprintf "(%s) -> (%s)" (string_of_dataexpr cond) 
            (string_of_procexpr th) ^ st_el
    | PBinop (op, left, right) ->
            Printf.sprintf "%s %s %s" (string_of_procexpr left) 
            (string_of_procop op) (string_of_procexpr right)
    | Delta   -> "delta"

let string_of_sorts (sorts:sort_spec):string =
    Printf.sprintf "sort %s" (mapcat "\n\t " string_of_projDecl sorts.sorts)

let string_of_maps (maps:map_spec):string =
    Printf.sprintf "map  %s\n" (string_of_iddecls maps.maps)  ^
    Printf.sprintf "var  %s;\n" (string_of_vardecls maps.vars ";\n\t ") ^
    Printf.sprintf "eqn  %s"   (string_of_eqndecls maps.eqns) 

let string_of_procs (proc:proc_spec):string =
    Printf.sprintf "act %s \n" (string_of_iddecls proc.acts) ^
    Printf.sprintf "proc %s = \n %s;" 
        (string_of_pid proc.proc_id) (string_of_procexpr proc.proc)

let string_of_init (init:init_spec):string =
    Printf.sprintf "init %s(%s, %s);" 
        init.proc_id init.init_board init.starting_p

let string_of_spec (spec:specification):string =
   (string_of_sorts spec.sorts) ^ "\n\n" ^
   (string_of_maps spec.maps) ^ "\n\n" ^
   (string_of_procs spec.proc) ^ "\n\n" ^
   (string_of_init spec.init)
