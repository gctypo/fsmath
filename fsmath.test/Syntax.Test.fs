module fsmath.test.Syntax_Test

open System
open NUnit.Framework
open FsUnit

open fsmath.core
open fsmath.core.Syntax

let makeNumber (num: string) =
    if (num.Contains '.') then
        let sp = num.Split('.')
        Number(sp[0], sp[1])
    else
        Number(num, "")


let makeToken (shorthand: string) =
    match shorthand with
    | "" -> raise <| FormatException("Empty token!")
    | "(" -> ParenOpen
    | ")" -> ParenClose
    | a when a[0] |> Tokenizer.isOperator [] -> Operator(a)
    | d when d[0] = '.' -> Number("", d.Substring(1))
    | d when d[0] |> Tokenizer.isDigit -> makeNumber d
    | _ -> raise <| FormatException($"Invalid token: {shorthand}")

let private TEST_TOKENS =
    [ Number("123","234");
        Operator("*");
        ParenOpen;
        Operator("-");
        Number("","456");
        ParenClose;
        Operator("^");
        Number("2", "") ]

[<Test>]
let makeToken_Test () =
    [ "123.234"; "*"; "("; "-"; ".456"; ")"; "^"; "2" ]
    |> List.map makeToken
    |> should equivalent TEST_TOKENS

[<Test>]
let toksToString_Test_General () =
    TEST_TOKENS
    |> toksToString
    |> should equal "123.234 * ( - .456 ) ^ 2"

[<Test>]
[<TestCase([|"("; "-"; "1.22"; ")"; "^"; "2"|], "- 1.22", "^ 2")>]
let syntaxParen_Test (tokens: string[], expBody: string, expRem: string) =
    let (rem, grp) =
        tokens |> Array.toList |> List.map makeToken
        |> syntaxParen
    grp |> nodeToString |> should equal expBody
    rem |> toksToString |> should equal expRem
