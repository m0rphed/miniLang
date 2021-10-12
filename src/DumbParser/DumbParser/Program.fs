module DumbParser.Cli

open DumbParser.Parser
open System.IO
open DumbParser.SimpleLogger

let private (+/) path1 path2 = Path.Combine(path1, path2)

[<EntryPoint>]
let main _argv =
    let fileExt = "dumbLang"

    let rootDir =
        __SOURCE_DIRECTORY__
        |> Directory.GetParent
        |> string
        |> Directory.GetParent
        |> string
        |> Directory.GetParent
        |> string

    let dirPath = rootDir +/ "data"

    ConsoleLogger.info $"Searching for files in: {dirPath}"

    let mutable counter = 0

    for file in Directory.GetFiles(dirPath) do
        let splitName = file.Split(".")

        if (Array.last splitName) = fileExt then
            counter <- counter + 1
            let fullPath = (dirPath +/ file)
            ConsoleLogger.warn $"Parsing %s{fullPath} ......."
            parseSourceFile fullPath

    ConsoleLogger.complete $"Processed %d{counter} files."
    0 // return an integer exit code