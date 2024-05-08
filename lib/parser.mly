%{
    open Ast
%}


%token COLON COMMA
%token BACTIONS WACTIONS
%token ACTION PARAMS PRECOND EFFECT
%token LPAREN RPAREN
%token BLACK WHITE OPEN
%token NOT
%token <string> IDENT
%token POSX POSY
%token XMAX XMIN YMAX YMIN
%token PLUS MINUS
%token <int64> INT
%token BOARDSIZE
%token INIT
%token DEPTH
%token BGOALS WGOALS
%token NL
%token BTURN SECOND FALSE BREAKER
%token EOF

%start <specification> program

%%
pred:
|BLACK {Black}
|WHITE {White}
|OPEN {Open}

action:
| COLON ACTION name = IDENT NL 
  COLON PARAMS LPAREN POSX COMMA POSY RPAREN NL
  COLON PRECOND pre = condition NL
  COLON EFFECT eff = condition NL {Action{name = name; pre = pre; eff=eff}}

condition:
| LPAREN c = sub_conditions RPAREN {c}

sub_conditions:
|s = single_condition rest = sub_conditions {[s] @ rest}
|s = single_condition {[s]}

single_condition:
|p = pred pa = params {Condition{pred = p; place = pa}}
|NOT LPAREN p= pred pa = params RPAREN {NotCondition{pred=p; place = pa}}
|LPAREN NOT LPAREN p= pred pa = params RPAREN RPAREN {NotCondition{pred=p; place = pa}}
params:
| LPAREN x = e1 COMMA y = e2 RPAREN {x,y}

e1:
| POSX e=int_exp? {XExp e}
| XMIN {Xmin}
| XMAX {Xmax}
| i = INT {Integer{int = i}}

e2:
| POSY e=int_exp? {YExp e}
| YMIN {Ymin}
| YMAX {Ymax}
| i = INT {Integer{int= i}}

int_exp:
| PLUS i = INT {PlusExp{int=i}}
| MINUS i = INT {MinusExp{int=i}}

position:
| p=pred LPAREN x=INT COMMA y=INT RPAREN {Condition{pred=p; place = (Integer{int=x},Integer{int=y})}}

domain:
|BACTIONS NL ab=action*  
 WACTIONS NL aw=action* {Domain{bactions=ab; wactions=aw}}

goal: 
| conds = sub_conditions NL {Goal{conditions = conds}}
| BREAKER NL {NoGoal}

turn_decl:
| BTURN NL SECOND {false}
problem:
 BOARDSIZE NL i=INT j=INT NL
 INIT NL pos=position* NL*
 BGOALS NL cb=goal* 
 WGOALS NL cw=goal* 
 t = turn_decl? {Problem{boardsize = (i,j); init = pos; bgoals = cb ; wgoals = cw ; bfirst = match t with |None -> true |Some b -> b }}


program:
    d=domain p=problem NL* EOF   { Spec{domain= d; problem= p} }
    (*d = domain NL* EOF {Spec{domain= d; problem= Problem{boardsize=(0L,0L);init=[];bgoals=[];wgoals=[]; bfirst=true}}}*)
