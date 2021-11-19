namespace MiniML

module MiniMLParser =
    exception ParserException of string
    
    open System
    open HomeGrownParsec

    let text (str: string) =
        Parser
            (fun chars ->
                let rec loop l p =
                    if p >= str.Length then
                        Success((), l)
                    else
                        match l with
                        | h :: tl when h = str.[p] -> loop tl (p + 1)
                        | _ -> Failed

                loop chars 0)

    let ws0Plus p = zeroOrManyU ws <&> p
    let ws1Plus = oneOrManyU ws
    let ws0Text s = ws0Plus (text s)
    let ws1Text s = ws1Plus <&> (text s)
    let ws0Char c = ws0Plus (charU c)

    let keywords =
        set [ "let"
              "letrec"
              "in"
              "if"
              "then"
              "else" ]

    let integer =
        parser {
            let! sign = ws0Plus ((char '-' <&> one -1) <|> (one 1))
            let! value = 0 |> oneOrMany digit (fun acc v -> acc * 10 + (int v - int '0'))

            return sign * value
        }

    let identifier =
        parser {
            let! f = ws0Plus letter
            let sb = (Text.StringBuilder()).Append(f)

            let! r =
                sb
                |> zeroOrMany letterOrDigit (fun acc v -> acc.Append(v))

            let text = r.ToString()

            if not <| keywords.Contains(text) then
                return text
        }

    let rec expr =
        let parseIdentifier = map identifier Var
        let parseNumber = map integer Integer

        let parseLet, parseLetRec =
            let p keyword f =
                parser {
                    do! ws0Text keyword <&> ws
                    let! id = identifier
                    do! ws0Char '='
                    let! value = expr
                    do! ws1Text "in" <&> ws
                    let! body = expr
                    return f (id, value, body)
                }

            p "let" Let, p "letrec" Letrec

        let parseLambda =
            parser {
                do! ws0Char '\\'
                let! id = identifier
                do! ws0Text "->"
                let! body = expr
                return Lambda(id, body)
            }

        let parseBracketed =
            parser {
                do! ws0Char '('
                let! e = expr
                do! ws0Char ')'
                return e
            }

        let parseApply =
            parser {
                let! f = parseIdentifier <|> parseBracketed

                return!
                    f
                    |> oneOrMany (parseIdentifier <|> parseNumber <|> parseBracketed) (fun acc e -> Apply(acc, e))
            }

        let parseIfThenElse =
            parser {
                let body =
                    parseIdentifier <|> parseBracketed <|> parseNumber

                do! ws0Text "if" <&> ws
                let! cond = parseIdentifier <|> parseBracketed
                do! ws1Text "then" <&> ws
                let! ifTrue = body
                do! ws1Text "else" <&> ws
                let! ifFalse = body
                return IfThenElse(cond, ifTrue, ifFalse)

            }

        parseLet
        <|> parseLetRec
        <|> parseLambda
        <|> parseIfThenElse
        <|> parseApply
        <|> parseBracketed
        <|> parseIdentifier
        <|> parseNumber

    let parse chars = run expr chars
