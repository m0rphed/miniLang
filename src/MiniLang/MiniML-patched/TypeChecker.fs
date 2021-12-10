module MiniML.Compiler.TypeChecker

open Ast
open TAst
open MiniML.Utils

exception TypeError of string

let typeError msg = raise (TypeError msg)

let inline randName () =
    Name <| System.Guid.NewGuid().ToString()

let rec check ctx ty e =
    let ty': TypedExpr = typify ctx e

    if ty'.Type <> ty then
        typeError $"%s{string e} has type {ty'} but is used as if it has type {ty}"

    ty'

and checkBinOp ctx e1 e2 types =
    match (typify ctx e1, typify ctx e2) with
    | (texpr1: TypedExpr, texpr2: TypedExpr) when texpr1.Type = texpr2.Type ->
        if List.exists ((=) texpr1.Type) types then
            texpr1, texpr2, texpr1.Type
        else
            typeError $"Operator not defined for type: {texpr1.Type}"
    | ty1, ty2 -> typeError $"Operator can't be applied for different types %s{string ty1.Type} and %s{string ty2.Type}"

and typify ctx =
    function
    | Var x ->
        match List.assoc x ctx with
        | Some ty -> TVar(x, ty)
        | _ -> typeError $"Undefined variable: {x}"
    | Int i -> TInt i
    | Float f -> TFloat f
    | Bool b -> TBool b
    | Times (e1, e2) -> TTimes(checkBinOp ctx e1 e2 [ TyInt; TyFloat ])
    | Plus (e1, e2) -> TPlus(checkBinOp ctx e1 e2 [ TyInt; TyFloat ])
    | Minus (e1, e2) -> TMinus(checkBinOp ctx e1 e2 [ TyInt; TyFloat ])
    | Divide (e1, e2) -> TDivide(checkBinOp ctx e1 e2 [ TyInt; TyFloat ])

    | Equal (e1, e2) ->
        let te1, te2, _ =
            checkBinOp ctx e1 e2 [ TyInt; TyFloat; TyBool ]

        TEqual(te1, te2)

    | Less (e1, e2) ->
        let te1, te2, _ = checkBinOp ctx e1 e2 [ TyInt; TyFloat ]
        TLess(te1, te2)

    | Cond (e1, e2, e3) ->
        let e1tast = check ctx TyBool e1
        let e2tast = typify ctx e2
        let e3tast = check ctx e2tast.Type e3
        TCond(e1tast, e2tast, e3tast, e2tast.Type)

    | Fun (f, x, ty1, ty2, e) ->
        let ctx = (x, ty1) :: (f, TyFun(ty1, ty2)) :: ctx
        let texpr = check ctx ty2 e
        TFun(f, x, ty1, ty2, texpr, TyFun(ty1, ty2))

    | Apply (e1, e2) ->
        let e1tast = typify ctx e1

        match e1tast.Type with
        | TyFun (ty1, ty2) ->
            check ctx ty1 e2 |> ignore
            TApply(e1tast, typify ctx e2, ty2)
        | ty -> typeError $"%s{string e1} has type %s{string ty} which is not a function and can't be applied"

    | LetIn (name, e1, e2) ->
        let e1texpr = typify ctx e1
        let e2texpr = typify ((name, e1texpr.Type) :: ctx) e2
        TLetIn(name, e1texpr, e2texpr, e2texpr.Type)

let cleanName (Name n as name) = if n = "_" then randName () else name

let rec transform context =
    function
    | TLetIn (x, texpr1, texpr2, ty) ->
        let name = randName ()

        let ctx =
            (name, TyFun(texpr1.Type, texpr2.Type)) :: context

        let lambda =
            TFun(name, x, texpr1.Type, texpr2.Type, transform ctx texpr2, TyFun(texpr1.Type, texpr2.Type))

        TApply(lambda, texpr1, texpr2.Type)
    | TVar _
    | TInt _
    | TFloat _
    | TBool _ as e -> e
    | TTimes (e1, e2, ty) -> TTimes(transform context e1, transform context e2, ty)
    | TPlus (e1, e2, ty) -> TPlus(transform context e1, transform context e2, ty)
    | TMinus (e1, e2, ty) -> TMinus(transform context e1, transform context e2, ty)
    | TDivide (e1, e2, ty) -> TDivide(transform context e1, transform context e2, ty)
    | TEqual (e1, e2) -> TEqual(transform context e1, transform context e2)
    | TLess (e1, e2) -> TLess(transform context e1, transform context e2)
    | TCond (e1, e2, e3, ty) -> TCond(transform context e1, transform context e2, transform context e3, ty)
    | TFun (f, x, ty1, ty2, e, ty) -> TFun(cleanName f, x, ty1, ty2, transform context e, ty)
    | TApply (e1, e2, ty) -> TApply(transform context e1, transform context e2, ty)
    | _ -> failwith "Not implemented yet"