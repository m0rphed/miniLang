namespace MiniML

type Ast = 
    | Let of string * Ast * Ast
    | Letrec of string * Ast * Ast
    | Var of string
    | Integer of int
    | Lambda of string * Ast
    | Apply of Ast * Ast
    | IfThenElse of Ast * Ast * Ast