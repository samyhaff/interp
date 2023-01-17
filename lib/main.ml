open Ast

let parse s =
    let lexbuf = Lexing.from_string s in
    let ast = Parser.prog Lexer.read lexbuf in
    ast

let string_of_val e =
    match e with
    | Int i -> string_of_int i
    | Binop _ -> "precondition violated"

let is_value = function
    | Int _ -> true
    | Binop _ -> false

let rec step = function
    | Int _ -> failwith "Does not step"
    | Binop (bop, e1, e2) when is_value e1 && is_value e2 ->
            step_binop bop e1 e2
    | Binop (bop, e1, e2) when is_value e1 -> Binop(bop, e1, step e2)
    | Binop (bop, e1, e2) -> Binop(bop, step e1, e2)
and step_binop bop v1 v2 = match bop, v1, v2 with
    | Add, Int a, Int b -> Int (a + b)
    | _ -> failwith "precondition violated"

let rec eval e =
    if is_value e then e else
        e |> step |> eval

let interp s =
    s |> parse |> eval |> string_of_val
