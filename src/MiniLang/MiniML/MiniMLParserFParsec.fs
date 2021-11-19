namespace MiniML

open System

module MiniMLParserFParsec =
    open System.Numerics
    open FParsec
    let private bigInt: Parser<_, Unit> =
        many1Satisfy isDigit
            |>> BigInteger.Parse
            .>> spaces 

    // checks if parsed text matches given string
    let text str = pstring str
    
    // defines parser that consumes
    // 0+ whitespaces + specified parser
    let ws0Plus p = spaces >>. p
    
    // defines parser that consumes
    // 1+ whitespaces + specified parser
    let ws1Plus p = spaces1 >>. p
    
    // matches 0+ whitespaces + specifies string
    let ws0Text str = ws0Plus (text str)
    let ws1Text str = ws1Plus (text str)
    
    // selects given char and returns unit
    let charU c = skipChar c
    
    // matches 0+ whitespaces + specified char
    let ws0Char c = ws0Plus (charU c)
    
    // list of language keywords
    let keywords: Parser<string, Unit> =
        choice [ text "let"
                 text "letrec"
                 text "in"
                 text "if"
                 text "then"
                 text "else" ]
    
    let integer = bigInt 
    let isId =
        many1Satisfy2
            Char.IsLetter
            Char.IsLetterOrDigit
    
    // zero or more whitespaces
    // + letter + zero or more letters or digits
    let identifier: Parser<string, Unit> = 
        isId
        .>> spaces
        
    // TODO: THIS IS NOT READY YET
    raise (NotImplementedException())