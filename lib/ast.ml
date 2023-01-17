type bop =
    | Add

type expr =
    | Int of int
    | Binop of bop * expr * expr
