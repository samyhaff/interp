{
    open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+

rule read =
    parse
    | white { read lexbuf }
    | "+" { PLUS }
    | "*" { TIMES }
    | "(" { LPAREN }
    | ")" { RPAREN }
    | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | eof { EOF }
