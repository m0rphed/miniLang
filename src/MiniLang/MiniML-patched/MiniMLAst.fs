module MiniML.Compiler.Ast

type Name = Name of string
    with
        override n.ToString() =
            match n with
            | Name v -> v

type ty =
    | TyInt
    | TyFloat
    | TyBool
    | TyFun of ty * ty (* function type t1 -> t2 *)
    override ty.ToString() =
        let rec toStr precedence ty =
            let n, str =
                match ty with
                | TyInt -> 2, "int"
                | TyFloat -> 2, "float"
                | TyBool -> 2, "bool"
                | TyFun(ty1, ty2) -> 1, toStr 1 ty1 + " -> " + toStr 0 ty2

            if n > precedence then str else "(" + str + ")"
        toStr -1 ty

type Expr =
    | Var of Name
    | Int of int
    | Float of float
    | Bool of bool
    | Times of Expr * Expr
    | Plus of Expr * Expr
    | Minus of Expr * Expr
    | Divide of Expr * Expr
    | Equal of Expr * Expr
    | Less of Expr * Expr
    | Cond of Expr * Expr * Expr
    | LetIn of Name * Expr * Expr
    | Fun of Name * Name * ty * ty * Expr
    | Apply of Expr * Expr
    override expr.ToString() =
        let rec toStr precedence e =
            let m, str =
                match e with
                | Int n -> 7, string n
                | Float f -> 7, string f
                | Bool b -> 7, string b
                | Var x -> 7, string x
                | Apply (e1, e2) -> 6, toStr 5 e1 + " " + toStr 6 e2
                | Times (e1, e2) -> 5, toStr 4 e1 + " * " + toStr 5 e2
                | Divide (e1, e2) -> 5, toStr 4 e1 + " / " + toStr 5 e2
                | Plus (e1, e2) -> 4, toStr 3 e1 + " + " + toStr 4 e2
                | Minus (e1, e2) -> 4, toStr 3 e1 + " - " + toStr 4 e2
                | Equal (e1, e2) -> 3, toStr 3 e1 + " = " + toStr 3 e2
                | Less (e1, e2) -> 3, toStr 3 e1 + " < " + toStr 3 e2
                | Cond (e1, e2, e3) -> 2, $"if %s{toStr 2 e1} then %s{toStr 2 e2} else %s{toStr 2 e3}"
                | Fun (f, x, ty1, ty2, e) ->
                    (1, $"fun %s{string f} (%s{string x} : %s{string ty1}) : %s{string ty2} is %s{toStr 0 e}")
                | LetIn (name, e1, e2) -> 8, $"let {name} = %s{toStr 7 e1} in %s{toStr 8 e2}"
            if m > precedence then str else "(" + str + ")"
        toStr -1 expr

type toplevel_decl =
    | Expr of Expr
    | LetBinding of Name * Expr
