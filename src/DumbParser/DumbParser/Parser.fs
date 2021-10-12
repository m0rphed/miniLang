module DumbParser.Parser

open DumbParser.SimpleLogger
open FParsec

[<AutoOpen>]
module AST =
    type Name = string

    type Value =
        | Int of int
        | Str of Name
        | Bool of bool

    type Operator =
        | Add // Arithmetic operators
        | Sub
        | Mult
        | Div
        | Mod
        | Gt // Comparison operators
        | Lt
        | Gte
        | Lte
        | Eq // Equality operators
        | Neq
        | And // Boolean operators
        | Or
        | Sconcat // String concatenation
        | Not

    type Expr =
        | Literal of Value
        | Variable of name: Name
        // The Expr type is recursive, as operations
        // can consist of expressions
        | Operation of (Expr * Operator * Expr)

    type Statement =
        | Print of Expr
        | Set of name: Name * value: Expr
        | If of condition: Expr * body: Block * Else: Block option
        | While of condition: Expr * body: Block

    and Block = Statement list



[<AutoOpen>]
module Parsing =
    let pword s = pstring s .>> spaces

    // A combinator that transforms a parser by requiring that it
    // be wrapped in parentheses
    let parens p = between (pword "(") (pword ")") p

    let pbool: Parser<Value, Unit> =
        pword "true" <|> pword "false"
        |>> function
            | "true" -> Bool true
            | "false" -> Bool false
            | _ -> failwith "Expected 'true' or 'false' boolean literal"

    // FParsec defines the pint32 parser.
    // We simply cast its result to an int
    // then construct an Integer Value from it
    let pint: Parser<Value, Unit> = pint32 |>> int |>> Int

    let pstringliteral: Parser<Value, Unit> =
        // This line returns a list of chars, which we have to
        // turn into a string before turning into a Str Value
        pchar '\"' >>. manyCharsTill anyChar (pchar '\"')
        |>> string
        |>> Str
        // Discard the spaces at the end
        .>> spaces

    let pvalue: Parser<Value, Unit> = choice [ pint; pstringliteral; pbool ]

    let test parser strInput =
        match run parser strInput with
        // Assuming your parser returns something
        // that can be printed. For our purposes,
        // %O is usually enough.
        | Success (result, _, _) -> printfn $"{result}"
        | Failure (error, _, _) -> printfn $"%s{error}"

    let pliteral: Parser<Expr, Unit> = pvalue |>> Literal

    let pidentifier: Parser<string, Unit> =
        many1Satisfy2 System.Char.IsLetter System.Char.IsLetterOrDigit
        .>> spaces

    let pvariable = pidentifier |>> Variable

    let intOperatorParser =
        OperatorPrecedenceParser<Expr, Unit, Unit>()

    let intExpr = intOperatorParser.ExpressionParser

    let intTerm =
        choice [ pint .>> spaces |>> Literal <|> pvariable
                 parens intExpr ]

    // Assign the term parser we designed to the
    // OperatorPrecedenceParser instance
    do intOperatorParser.TermParser <- intTerm

    let createOperation op x y = Operation(x, op, y)

    type OperatorDetails =
        { Symbol: string
          Precedence: int
          Operator: Operator }

    let intOperators =
        [
          { Symbol = ">"
            Precedence = 1
            Operator = Gt }
          { Symbol = "<"
            Precedence = 1
            Operator = Lt }
          { Symbol = ">="
            Precedence = 1
            Operator = Gte }
          { Symbol = "<="
            Precedence = 1
            Operator = Lte }
          { Symbol = "=="
            Precedence = 1
            Operator = Eq }
          { Symbol = "!="
            Precedence = 1
            Operator = Neq }
          { Symbol = "+"
            Precedence = 2
            Operator = Add }
          { Symbol = "-"
            Precedence = 2
            Operator = Sub }
          { Symbol = "*"
            Precedence = 3
            Operator = Mult }
          { Symbol = "/"
            Precedence = 3
            Operator = Div }
          { Symbol = "%"
            Precedence = 3
            Operator = Mod } ]

    let addOperators (precedenceParser: OperatorPrecedenceParser<_, _, _>) operatorTable =
        operatorTable
        |> List.iter
            (fun details ->
                let operator =
                    InfixOperator(
                        details.Symbol,
                        spaces,
                        details.Precedence,
                        Associativity.Left,
                        createOperation details.Operator
                    )

                precedenceParser.AddOperator(operator))


    do addOperators intOperatorParser intOperators
    // Define similar structures for booleans and strings
    let boolOperatorParser =
        OperatorPrecedenceParser<Expr, Unit, Unit>()

    let boolExpr = boolOperatorParser.ExpressionParser

    let pbool': Parser<bool, Unit> =
        pstring "true" <|> pstring "false"
        |>> System.Boolean.Parse

    let pboolval = pbool' |>> Bool

    let boolTerm =
        choice [ pboolval .>> spaces |>> Literal
                 pvariable
                 parens boolExpr ]

    boolOperatorParser.TermParser <- boolTerm

    let strOperatorParser =
        OperatorPrecedenceParser<Expr, Unit, Unit>()

    let strExpr = strOperatorParser.ExpressionParser
    // We want to make sure we can concatenate
    // non-string values with strings, so we
    // accept any literal or variable
    let strTerm =
        choice [ pliteral
                 pvariable
                 intExpr
                 boolExpr
                 parens strExpr ]

    do strOperatorParser.TermParser <- strTerm

    let boolOperators =
        [ { Symbol = "and"
            Precedence = 2
            Operator = And }
          { Symbol = "or"
            Precedence = 1
            Operator = Or } ]

    let stringOperators =
        [ { Symbol = "++"
            Precedence = 1
            Operator = Sconcat } ]

    do addOperators boolOperatorParser boolOperators
    do addOperators strOperatorParser stringOperators

    let poperation = choice [ intExpr; boolExpr; strExpr ]

    let pexpression =
        choice [ poperation
                 pliteral
                 pvariable ]

    let pstatement, pstatementref =
        createParserForwardedToRef<Statement, Unit> ()

    let pprint: Parser<Statement, Unit> =
        pword "print" >>. parens pexpression |>> Print

    let pset: Parser<Statement, Unit> =
        let identifier =
            many1Satisfy2 System.Char.IsLetter System.Char.IsLetterOrDigit
            .>> pword "="

        identifier .>>. pexpression |>> Set

    let pblock: Parser<Statement list, Unit> =
        between (pword "{") (pword "}") (many pstatement)

    let pif: Parser<Statement, Unit> =
        let condition = pword "if" >>. pexpression
        let inner = pblock
        let elseBlock = pword "else" >>. pblock |> opt

        pipe3 condition inner elseBlock (fun condition inner elseBlock -> If(condition, inner, elseBlock))

    let pwhile: Parser<Statement, Unit> =
        let condition = pword "while" >>. pexpression

        condition .>>. pblock |>> While

    do pstatementref := choice [ pprint; pif; pwhile; pset ]

    let parseSourceFile fullPath =
        match runParserOnFile (many pstatement) () fullPath System.Text.Encoding.UTF8 with
        | Success (result, _, _) -> ConsoleLogger.complete $"Result %A{result}"
        | Failure (error, _, _) -> ConsoleLogger.error $"%s{error}"
