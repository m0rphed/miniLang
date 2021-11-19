namespace MiniML

exception TypeInferenceException of Ast * string

module Types = 
    open System

    type TypeVarName = string

    type Type =
        | TypeVar of TypeVarName
        | Function of Type * Type
        | TypeConstructor of string * Type list
        override this.ToString() =
            match this with
            | TypeVar(name) -> name
            | Function(arg, res) -> $"({arg} -> {res})"
            | TypeConstructor(name, args) -> 
                if List.isEmpty args 
                then name
                else sprintf "%s[%s]" name (String.Join(", ", args |> Seq.map string))
    
    exception UnificationException of Type * Type
    
    type TypeVarGenerator() = 
        let n = ref 0
        member this.New() = 
            incr n
            TypeVar $"T%d{!n}"
    
    [<AbstractClass>]
    type Substitution private() = 
        static let empty = {new Substitution() with override this.Lookup(tv) = TypeVar(tv) }
        static member Empty = empty

        abstract Lookup : TypeVarName -> Type
        member this.Run(t) = 
            match t with
            | TypeVar tv ->
                let substituted = this.Lookup(tv)
                if t = substituted then substituted
                else this.Run(substituted)    
            | Function(a, r) -> 
                Function(this.Run(a), this.Run(r))
            | TypeConstructor(name, tyArgs) ->
                TypeConstructor(name, tyArgs |> List.map this.Run)

        member this.Extend(v : TypeVarName, t : Type) = 
            { new Substitution() with override __.Lookup(tv) = if v = tv then t else this.Lookup(tv) }
    
    type TypeScheme(tyVars : Set<_>, t, gen : TypeVarGenerator) = 
        let subst = (Substitution.Empty, tyVars) ||> Seq.fold(fun s tt -> s.Extend(tt, gen.New() ))
        let instance = subst.Run(t)
        member this.Instance = instance
        member this.TypeVariables = tyVars
        member this.Type = t

    module Env = 
        type Environment = Map<string, TypeScheme>

        let rec typeVarsOfType = function
            | TypeVar(tv)  -> Set.singleton tv
            | Function(a, r) -> typeVarsOfType(a) + typeVarsOfType(r)
            | TypeConstructor(_, tyArgs) -> (Set.empty, tyArgs) ||> List.fold (fun acc ty -> acc + typeVarsOfType ty)
    
        let typeVarsOfScheme(s : TypeScheme) = 
            (typeVarsOfType s.Type) - s.TypeVariables
    
        let typeVarsOfEnv(e : Environment) = 
            let schemes = e |> Map.toSeq |> Seq.map snd
            (Set.empty, schemes) ||> Seq.fold (fun acc s -> acc + (typeVarsOfScheme s))

        let typeToScheme (env : Environment, t : Type, gen) = 
            TypeScheme((typeVarsOfType t) - (typeVarsOfEnv env), t, gen)
    
    type Inferencer() =
        let nameGenerator = TypeVarGenerator() 

        let rec unify(a : Type, b : Type, s : Substitution) =
            match(s.Run(a), s.Run(b)) with
            | TypeVar(ta), TypeVar(tb) when ta = tb -> 
                s
            | TypeVar(ta), _ when not <| Env.typeVarsOfType(b).Contains(ta) -> 
                s.Extend(ta, b)
            | _, TypeVar _ -> 
                unify(b, a, s) 
            | Function(a1, b1), Function(a2, b2) -> 
                unify(a1, a2, unify(b1, b2, s))
            | TypeConstructor(name1, args1), TypeConstructor(name2, args2) when name1 = name2 ->
                (s, args1, args2) |||> List.fold2 (fun subst t1 t2 -> unify(t1, t2, subst))
            | x,y -> UnificationException(x,y) |> raise

        let newTypeVar = nameGenerator.New()
        let newTypeScheme t = Env.typeToScheme(Map.empty, t, nameGenerator)

        let boolean = TypeConstructor("bool", [])
        let integer = TypeConstructor("int", [])
        let list t = TypeConstructor("list", [t])
         
        let builtins = 
            [
            "true", newTypeScheme(boolean)
            "false", newTypeScheme(boolean)
            "nil", newTypeScheme(list(newTypeVar))
            "cons", newTypeScheme(Function(newTypeVar, Function(list(newTypeVar), list(newTypeVar))))
            "null", newTypeScheme(Function(list(newTypeVar), boolean))
            "eq", newTypeScheme(Function(newTypeVar, Function(newTypeVar, boolean)))
            "add", newTypeScheme(Function(integer, Function(integer, integer)))
            "mul", newTypeScheme(Function(integer, Function(integer, integer)))
            "sub", newTypeScheme(Function(integer, Function(integer, integer)))
            "tail", newTypeScheme(Function(list(newTypeVar), list(newTypeVar)))
            "head", newTypeScheme(Function(list(newTypeVar), newTypeVar))
            ] |> Map.ofList

        let rec analyze (e : Env.Environment, ast, baseType : Type, s : Substitution) = 
            try
                match ast with
                | Integer(v) ->
                    unify(integer, baseType, s)
                | Var(name) ->
                    if not (e.ContainsKey name) 
                    then failwithf $"Name %s{name} no found"

                    let schema = e.[name]
                    unify(schema.Instance, baseType, s)
                | Lambda(arg, body) ->
                    let a = nameGenerator.New()
                    let b = nameGenerator.New()

                    let s1 = unify(baseType, Function(a, b), s)
                    let newEnv = e.Add(arg, TypeScheme(Set.empty, a, nameGenerator))
                    analyze(newEnv, body, b, s1)
                | Apply(f, arg) ->
                    let a = nameGenerator.New()
                    let s1 = analyze(e, f, Function(a, baseType), s)
                    analyze(e, arg, a, s1)
                | Let(name, inV, body) ->
                    let a = nameGenerator.New()
                    let s1 = analyze(e, inV, a, s)
                    analyze(e.Add(name, Env.typeToScheme(e, s1.Run(a), nameGenerator)),body, baseType, s1)
                | Letrec(name, inV, body) ->
                    let t = nameGenerator.New()
                    let newEnv = e.Add(name, TypeScheme(Set.empty, t, nameGenerator))
                    let s1 = analyze(newEnv, inV, t, s)
                    analyze(e.Add(name, Env.typeToScheme(e, s1.Run(t), nameGenerator)), body, baseType, s1)               

                | IfThenElse(cond, ifTrue, ifFalse) ->
                    let s1 = analyze(e, cond, boolean, s)
                    let s2 = analyze(e, ifTrue, baseType, s1)
                    analyze(e, ifFalse, baseType, s2)             
            with
                UnificationException(t1, t2) -> TypeInferenceException(ast, $"Cannot unify {t1} and {t2}") |> raise

        let alpha t = 
            let l = ref 'A'
            let map = Collections.Generic.Dictionary<_, _>()
            let rec run = function
                | TypeVar(name) ->
                    if not <| map.ContainsKey(name) then
                        let newName = string (!l)
                        l := Convert.ToChar(int !l + 1)
                        map.Add(name, newName)
                    TypeVar(map.[name])
                | Function(arg, res) -> Function(run arg, run res)
                | TypeConstructor(name, typeArgs) -> TypeConstructor(name, List.map run typeArgs)
            run t

        member this.TypeOf(ast) =
            let a = nameGenerator.New()
            analyze(builtins, ast, a, Substitution.Empty).Run(a) |> alpha