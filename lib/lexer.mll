{
    open Parser
}

let digit = ['0'-'9']
let int = '-'? digit+

rule read =
    parse
    | "+" { PLUS }
    | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | eof { EOF }
