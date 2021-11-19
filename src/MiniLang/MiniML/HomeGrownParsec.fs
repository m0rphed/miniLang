namespace MiniML

/// Tiny parser combinators implemented from scratch
module HomeGrownParsec =
    open System

    type ParseResult<'T> =
        | Success of 'T * list<char>
        | Failed

    type Parser<'T> = Parser of (list<char> -> ParseResult<'T>)

    let apply (Parser p) s = p s
    let run p l = apply p (Seq.toList l)
    let one v = Parser(fun cs -> Success(v, cs))
    let failed () = Parser(fun _ -> Failed)

    // binds subsequent parsers
    let bind p f =
        Parser
            (fun cs ->
                match apply p cs with
                | Success (r, cs2) -> apply (f r) cs2
                | Failed -> Failed)
    // (OR combinator)
    let (<|>) p1 p2 =
        Parser
            (fun cs ->
                match apply p1 cs with
                | Failed -> apply p2 cs
                | ok -> ok)
    // (AND combinator)
    let (<&>) p1 p2 =
        Parser
            (fun cs ->
                match apply p1 cs with
                | Success (_, cs2) -> apply p2 cs2
                | Failed -> Failed)
    // applies given predicate to first symbol in the stream
    let choose f p =
        Parser
            (fun cs ->
                match cs with
                | c :: cs2 when f c -> Success(p c, cs2)
                | _ -> Failed)
    let digit = choose Char.IsDigit id
    let digitU = choose Char.IsDigit ignore
    let letter = choose Char.IsLetter id
    let letterU = choose Char.IsLetter ignore
    let letterOrDigit = choose Char.IsLetterOrDigit id
    let letterOrDigitU = choose Char.IsLetterOrDigit ignore
    let char c = choose ((=) c) id
    let charU c = choose ((=) c) ignore
    let ws = choose Char.IsWhiteSpace ignore

    // To enable computation expresion syntax
    type ParserBuilder() =
        member this.Return(v) = one v
        member this.Bind(p, f) = bind p f
        member this.ReturnFrom(p) = p
        member this.Zero() = failed ()

    let parser = ParserBuilder()
    let rec zeroOrMany p f v0 =
        parser { return! oneOrMany p f v0 <|> one v0 }

    and oneOrMany p f v0 =
        parser {
            let! v1 = p
            return! zeroOrMany p f (f v0 v1)
        }

    let zeroOrManyU p = zeroOrMany p (fun _ _ -> ()) ()

    let oneOrManyU p = oneOrMany p (fun _ _ -> ()) ()

    let map p f =
        parser {
            let! v = p
            return f v
        }
