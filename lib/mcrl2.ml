(**Ast representing the mCRL2 specification*)
type id = string 

type sortExpr
    = Bool | Pos | Nat | Int | Real 
    | List of sortExpr
    | Struct of id list
    | Arrow of sortExpr * sortExpr
    | Hash of sortExpr * sortExpr
    | NamedSort of id

type projDecl = id * sortExpr
type varDecl = id list * sortExpr 
type idDecl = id list * sortExpr

type quant =
    | ForAll
    | Exists
    | Lambda

type bop =
    |And | Or
    | Eq | Neq | Gt | Lt | Gte | Lte
    | Add | Sub | Mul | Div
    | Cons | Snoc
    | Dot

type procOp  = Plus | Dot

type dataExpr =
    | Id of id
    | Number of int64
    | QuantExpr of quant * varDecl list * dataExpr
    | DataBinop of bop * dataExpr * dataExpr
    | NotUnop of dataExpr
    | ListExpr of dataExpr list
    | IfExpr of dataExpr * dataExpr * dataExpr
    | FunExpr of dataExpr * dataExpr list
    
type eqnDecl = dataExpr option * dataExpr * dataExpr

type procExpr =
    | Action of id * dataExpr list 
    | SumExpr of varDecl list * procExpr
    | ITE of dataExpr * procExpr * procExpr option  (*Data exp should be limited*)
    | PBinop of procOp * procExpr * procExpr
    | Delta

type sort_spec = {sorts: projDecl list}

type map_spec =  
    { maps: idDecl list 
    ; vars: varDecl list
    ; eqns: eqnDecl list
    }

type proc_spec = 
      { acts: idDecl list 
      ; proc_id: id * varDecl list
      ; proc: procExpr
      }

type init_spec = 
    { proc_id: id
    ; init_board: id
    ; starting_p: id
    }

type specification = 
    { sorts: sort_spec
    ; maps: map_spec
    ; proc: proc_spec
    ; init: init_spec
    }