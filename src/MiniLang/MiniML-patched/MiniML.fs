module MiniML.Program

open MiniML.Compiler.Ast
open MiniML.Compiler.TAst
open MiniML.Compiler;
open FSharp.Text.Lexing
open Argu

type CliArguments =
    | [<AltCommandLine("-n")>] NonInteractive 
    | DumpDecl
    | DumpVMCode
    | DumpTAst
    | DumpCtx
    | DumpEnv
    | [<Mandatory>] File of string
    interface IArgParserTemplate with
        member args.Usage =
            match args with
            | NonInteractive -> "Non interactive run"
            | DumpDecl -> "Dump declarations"
            | DumpVMCode -> "Dump virtual machine code" 
            | DumpTAst -> "Dump typed AST"
            | DumpCtx -> "Dump context (Attention: printing may cycle when object graph has cycles)"
            | DumpEnv -> "Dump environment"
            | File _ -> "Specify file with source code"

open System.IO

type Context = (Name * ty) list

type Settings = { DumpDeclarations : bool
                  DumpVMCode : bool
                  DumpTAst : bool
                  DumpContext : bool
                  DumpEnv : bool }

let inline dumpVmCode (s : Settings) (frm : VirtualMachine.Frame) =
    if s.DumpVMCode then
        printfn "\ndump VM code:"
        printfn $"%s{VirtualMachine.frame2string frm}"
    frm

let inline dumpTAst (s : Settings) comment (texpr : TypedExpr) = 
    if s.DumpTAst then
        printfn $"\n%s{comment}:"
        printfn $"%A{texpr}"
    texpr

let inline dumpContext (s : Settings) (ctx : Context) =
    if s.DumpContext then
        printfn "\ndump context:"
        printfn $"%A{ctx}"
    ctx

let inline dumpEnv (s : Settings) (env : VirtualMachine.Env) =
    if s.DumpEnv then
        printfn "\ndump environment:"
        printfn $"%A{env}"
    env

let execCmd (s : Settings) (ctx, env) expr = 
    let name, e = match expr with Expr e -> Name "it", e | LetBinding (x, e) -> x, e
    let tast' = TypeChecker.typify ctx e |> dumpTAst s "dump typed abstract syntax tree"
    let tast = TypeChecker.transform ctx tast' |> dumpTAst s "dump typed and transforment abstract syntax tree"
    let frm = Emitter.emit tast |> dumpVmCode s
    let v = VirtualMachine.run frm env
    ((name, tast.Type) :: ctx, (name, ref v) :: env), $"val {name} : %s{string tast.Type} = %s{string v}"


let execCmds (s : Settings) ce cmds =
    List.fold (fun ce cmd -> let ce', msg = execCmd s ce cmd in printfn $"%s{msg}"; ce') ce cmds

let dumpDeclarations (settings : Settings) (decls : toplevel_decl list) =
    if settings.DumpDeclarations then
        printfn "\ndeclarations dump:"
        for decl in decls do 
            printfn $"%A{decl}"
    decls


let interactive (settings : Settings) ctx env =
    printfn "Welcome to MiniML"

    let globalCtx = ref ctx
    let globalEnv = ref env

    try
        while true do
            try
                printf "MiniML> "
                let str = System.Console.ReadLine()
                let decls = Parser.toplevel Lexer.token (LexBuffer<_>.FromString str) |> dumpDeclarations settings
                let (ctx : Context, env : VirtualMachine.Env) = execCmds settings (!globalCtx, !globalEnv) decls
                globalCtx := ctx |> dumpContext settings
                globalEnv := env |> dumpEnv settings
                ()
            with
                | TypeChecker.TypeError msg -> printfn $"Type error: %s{msg}"
                | VirtualMachine.RuntimeError msg -> printfn $"Runtime error: %s{msg}"
                | e -> printfn $"Error: %A{e}"

    with
        _ -> printfn "Exiting..."

[<EntryPoint>]
let main argv =
    let nonInteractive = ref false
    let dumpDecl = ref false
    let dumpVmCode = ref false
    let dumpTAst = ref false
    let dumpContext = ref false
    let dumpEnv = ref false
    let files = ref []
    try
        // build the argument parser
        let parser =  ArgumentParser.Create<CliArguments>(programName = "miniML")    
        let res = parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)
        
        let setConfigParameter cliParameter =
            match cliParameter with
            | NonInteractive -> nonInteractive := true
            | DumpDecl -> dumpDecl := true
            | DumpVMCode -> dumpVmCode := true
            | DumpTAst -> dumpTAst := true
            | DumpCtx -> dumpContext := true
            | DumpEnv -> dumpEnv := true
            | File fileName -> files := files.contents @ [fileName]

        let cliParams = res.GetAllResults()
        for prm in cliParams do
            setConfigParameter prm
            
        try
            let settings = { DumpDeclarations = !dumpDecl
                             DumpVMCode = !dumpVmCode; 
                             DumpTAst = !dumpTAst
                             DumpContext = !dumpContext
                             DumpEnv = !dumpEnv }
            
            let (ctx : Context), env =
                files.Value
                |> List.fold
                    (fun ce f ->
                        let text = File.ReadAllText f
                        let lexbuf = LexBuffer<_>.FromString text
                        let cmds = Parser.toplevel Lexer.token lexbuf
                        execCmds settings ce cmds)
                            MiniML.Language.Basics.initialContextAndEnv
            
            if not !nonInteractive
            then interactive settings ctx env
            0
        with
            | TypeChecker.TypeError msg -> printfn $"Type error: %s{msg}"; 1
            | VirtualMachine.RuntimeError msg -> printfn $"Virtual machine error: %s{msg}"; 1
            | e -> printfn $"Error: %A{e}"; 1
    with
    | :? ArguParseException as ex ->
        printfn $"CLI Arguments parsing error: %s{ex.Message}"
        1
