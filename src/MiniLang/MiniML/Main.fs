module MiniML.Main

open MiniML.MiniMLParser

let sources =
    [ @"if (1) then true else false", "incorrect condition type"
      @"if true then 1 else false", "different types for conditional branches"
      @"letrec fold =
        \f -> \s -> \l -> if (null l) then s else (fold f (f s (head l)) (tail l)) in
        fold",
      "generic fold"
      @"letrec fold =
        \f -> \s -> \l -> if (null l) then s else (fold f (f s (head l)) (tail l)) in
        fold add",
      "partial application"
      @"letrec length = \l -> if (null l) then 0 else (add 1 (length (tail l))) in length", "recursive definition1"
      @"letrec fact = \x -> if (eq x 1) then 1 else (mul x (fact (add x -1))) in fact", "factorial"
      @"let f = \x -> \y -> (cons (x y) y) in f", "" ]

let rec astToString =
    function
    | Let (name, v, body) ->
        $"let %s{name} =
        %s{astToString v} in
        %s{astToString body}"
    | Letrec (name, v, body) ->
        $"letrec %s{name} =
        %s{astToString v} in
        %s{astToString body}"
    | Apply (arg, r) -> $"(%s{astToString arg} %s{astToString r})"
    | IfThenElse (cond, ifTrue, ifFalse) ->
        $"if %s{astToString cond}
        then %s{astToString ifTrue}
        else %s{astToString ifFalse}"
    | Integer v -> string v
    | Lambda (name, body) -> $"\%s{name} -> %s{astToString body}"
    | Var name -> name

let parse (s, comment) =
    printfn $"===%s{comment}"
    let inferencer = Types.Inferencer()

    match parse s with
    | HomeGrownParsec.Success (r, []) ->
        printfn $"%s{astToString r}"
        let text =
            try
                inferencer.TypeOf(r).ToString()
            with
            | TypeInferenceException (ast, text) -> $"%s{astToString ast}: %s{text}"

        printfn $"Type: %s{text}"
    | _ -> printfn $"Parsing failed for %s{s}"

let parseAndInfer (str: string) =
    let inferencer = Types.Inferencer()
    match MiniMLParser.parse str with
    | HomeGrownParsec.Success (res, []) ->
        try
            inferencer.TypeOf(res).ToString() |> Ok
        with
        | TypeInferenceException (_ast, remText) as exp ->
            (exp, remText) |> Error
    | _ -> Error (ParserException str, str.ToString())

[<EntryPoint>]
let main _argv =
    printfn "Running..."

    for src in sources do
        parse src

    0 // return an integer exit code