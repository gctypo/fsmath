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
  + Addition
  - Subtraction
  * Multiplication
  x Multiplication (alt)
  / Division
  ^ Exponentiation

Supported unary operators:
  - Negation

Traditional order of operations supported. Use parentheses to force grouping.
    "

[<EntryPoint>]
let main argv =
    match (argv |> Array.toList) with
    | [] ->
        eprintfn "Usage: fsmath <expression>\n   or: fsmath --help"
        1
    | "--version"::_ ->
        let ver = Assembly.GetExecutingAssembly().GetName().Version
        (ver.Major, ver.Minor, ver.Build)
        |||> printfn "fsmath version %i.%i.%i"
        0
    | "--help"::_ ->
        helpInfo |> printfn "%s"
        0
    | arr ->
        try
            let eval = arr |> String.concat " " |> Evaluator.evaluateString
            eval.ToString() |> printfn "%s"
            0
        with
        | :? FormatException as ex ->
            ex.Message |> eprintfn "Parse error: %s"
            2
        | ex ->
            ex.ToString() |> eprintfn "ERROR: %s"
            1
