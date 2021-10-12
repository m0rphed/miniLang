module DumbParser.SimpleLogger

module ConsoleLogger =

    open System

    let log =
        let lockObj = obj()
        fun color message ->
            lock lockObj (fun _ ->
                Console.ForegroundColor <- color
                printfn $"%s{message}"
                Console.ResetColor())

    let complete = log ConsoleColor.Magenta
    let ok = log ConsoleColor.Green
    let info = log ConsoleColor.Cyan
    let warn = log ConsoleColor.Yellow
    let error = log ConsoleColor.Red