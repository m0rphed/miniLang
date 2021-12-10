module MiniML.Compiler.TAst
open MiniML.Compiler.Ast

type TypedExpr =
    | TVar of Name * ty
    | TInt of int
    | TFloat of float
    | TBool of bool
    | TTimes of TypedExpr * TypedExpr * ty
    | TPlus of TypedExpr * TypedExpr * ty
    | TMinus of TypedExpr * TypedExpr * ty
    | TDivide of TypedExpr * TypedExpr * ty
    | TEqual of TypedExpr * TypedExpr 
    | TLess of TypedExpr * TypedExpr
    | TCond of TypedExpr * TypedExpr * TypedExpr * ty
    | TFun of Name * Name * ty * ty * TypedExpr * ty
    | TApply of TypedExpr * TypedExpr * ty
    | TLetIn of Name * TypedExpr * TypedExpr * ty
    
    member this.Type =
        match this with
        | TVar (_, ty) | TTimes (_, _, ty) | TPlus (_, _, ty) | TMinus (_, _, ty)
        | TDivide (_, _, ty) | TCond (_, _, _, ty) | TFun (_, _, _, _, _, ty) 
        | TApply (_, _, ty) | TLetIn (_, _, _, ty) -> ty
        | TInt _ -> TyInt 
        | TFloat _ -> TyFloat 
        | TBool _ | TEqual _ | TLess _ -> TyBool


type TToplevelDecl =
    | TExpr of TypedExpr
    | TLetBinding of Name * TypedExpr



