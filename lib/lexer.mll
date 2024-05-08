{
    open Parser
        exception UnknownInput of string
}

let digit = ['0'-'9']
let digits = digit+
let letters = (['a'-'z'] | ['A' - 'Z'])
let ident = (('_' |letters) ('_' |'-' |letters | digits)*) 

rule token = parse
| "#blackactions" {BACTIONS}
| "#whiteactions" {WACTIONS}
| ':' {COLON} | ',' {COMMA}
| "action" {ACTION}
| "parameters" {PARAMS}
| "precondition" {PRECOND}
| "effect" {EFFECT}
| '(' {LPAREN} | ')' {RPAREN}
| "black" {BLACK} | "white" {WHITE} | "open" {OPEN}
| "NOT" {NOT}
| "?x" {POSX} |"?y" {POSY}
| "xmin" {XMIN} | "xmax" {XMAX} 
| "ymin" {YMIN} | "ymax" {YMAX}
| '+' {PLUS} | '-' {MINUS}
| digits as i {INT (Int64.of_string i)}
(* for the problem file *)
| "#boardsize" {BOARDSIZE}
| "#init" {INIT}
| "#depth" {depth_decl lexbuf}
| "#blackgoals" | "#blackgoal" {BGOALS}
| "#whitegoal" | "#whitegoals" {WGOALS}
| "#blackturn" {BTURN}
| "second" {SECOND}
| "False" | "false" {BREAKER} (*should no longer be necessary with rewrites*)
| "breaker" {BREAKER}
(* empty space... *)
| " " | "\t" {token lexbuf}
| "\n" | "\r\n" {Lexing.new_line lexbuf; NL}
| eof { EOF }
|"%" {one_line_comment lexbuf} (*ignore everything till linebreak...*)
| ident as y {IDENT y}
| _ as x { raise (UnknownInput (String.make 1 x))}

and one_line_comment = parse
|'\n' {token lexbuf}
| _ {one_line_comment lexbuf}

and depth_decl = parse
| digits {token lexbuf}
| "#blackgoal" {BGOALS}
| "#blackgoals" {BGOALS}
| _ {depth_decl lexbuf}
