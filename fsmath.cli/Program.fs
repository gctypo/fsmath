module fsmath.cli.Program

open System
open System.Reflection
open fsmath.core

let helpInfo =
    "Usage:
  fsmath <expression>
  fsmath --version
  fsmath --help

Parses a mathematical expression. Result is printed to stdout.

Supported binary operators:
  +  Addition
  -  Subtraction
  *  Multiplication
  x  Multiplication (alt)
  /  Division
  ^  Exponentiation

Supported unary operators:
  -  Negation
  +  Identity
  -/ Square Root

Traditional order of operations supported. Use parentheses to force grouping.
    "
let printHelp () =
    helpInfo |> printfn "%s"
    0

let printVersionInfo () =
    let ver = Assembly.GetExecutingAssembly().GetName().Version
    (ver.Major, ver.Minor, ver.Build)
    |||> printfn "fsmath version %i.%i.%i"
    0

let evalString (expr: string) =
    try
        let eval = expr |> Evaluator.evaluateString
        eval.ToString() |> printfn "%s"
        0
    with
    | :? FormatException as ex ->
        ex.Message |> eprintfn "Parse error: %s"
        2
    | ex ->
        ex.ToString() |> eprintfn "ERROR: %s"
        1

[<EntryPoint>]
let main argv =
    match (argv |> Array.toList) with
    | [] ->
        eprintfn "Usage: fsmath <expression>\n   or: fsmath --help"
        1
    | "--version"::_ -> printVersionInfo ()
    | "--help"::_ -> printHelp ()
    | arr -> arr |> String.concat " " |> evalString
