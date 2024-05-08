
type pred = | Black | White | Open

type int_exp = 
    |PlusExp of {int:int64}
    |MinusExp of {int:int64}


type e1 = | Xmin | Xmax | Integer of {int:int64} | XExp of int_exp option
type e2 = | Ymin | Ymax | Integer of {int:int64} | YExp of int_exp option


type cond = | Condition of {pred: pred ; place : e1 * e2}
            | NotCondition of {pred: pred ; place : e1 * e2}

type action = Action of {name: string; pre : cond list; eff : cond list}

type goal = 
| Goal of {conditions : cond list}
| NoGoal 

type domain = Domain of {bactions : action list; wactions: action list}

(*The init is not right...*)
type problem = Problem of {boardsize: int64*int64; init: cond list; bgoals : goal list; wgoals : goal list; bfirst: bool}

type specification = Spec of {domain : domain; problem : problem}
