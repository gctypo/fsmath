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

// FsTest constraint for throw message with substring
let throwWithPartialMessage (expected: string) (t: Type) =
    Throws.TypeOf(t).And.Message.Contains(expected)

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
[<TestCase([|"("; "-"; "1.22"; ")"; "^"; "2"|], "((- 1.22) ^ 2)")>]
[<TestCase([|"(";")";"*";"(";"(";")";")"|], "(() * (()))")>]
let syntaxParen_Test (tokens: string[], expBody: string) =
    tokens |> Array.toList |> List.map makeToken
    |> syntaxParen
    |> nodeToString |> should equal expBody

[<Test>]
[<TestCase([|"(";"1.22"|], "1.22")>]
[<TestCase([|"-";"(";"-";"(";"1.22"|], "1.22")>]
[<TestCase([|"(";"1.22";")";")"|], "(1.22)")>]
let syntaxParen_Test_Fail (tokens: string[], expMsg: string) =
    let inp = tokens |> Array.toList |> List.map makeToken
    (fun () -> syntaxParen inp |> ignore)
    |> should (throwWithPartialMessage $": {expMsg}") typeof<FormatException>

[<Test>]
[<TestCase([|"-";"100"|], "([-100])")>]
[<TestCase([|"3";"-";"100"|], "(3 - 100)")>]
[<TestCase([|"-";"100";"*";"-";"100"|], "([-100] * [-100])")>]
let groupUnary_Test (tokens: string[], expStr: string) =
    let inp = tokens |> Array.toList |> List.map (makeToken >> TokenWrapper)
    groupUnary inp []
    |> UnparsedGroup |> nodeToString
    |> should equal expStr

[<Test>]
[<TestCase([|"-";"100";"*";"(";"3";"-";"100";")"|], "([-100] * (3 - 100))")>]
[<TestCase([|"100";"*";"-";"(";"3";"-";"100";")"|], "(100 * [-(3 - 100)])")>]
[<TestCase([|"-";"100";"*";"(";"3";"*";"-";"100";")"|], "([-100] * (3 * [-100]))")>]
let groupUnary_Test_Paren (tokens: string[], expStr: string) =
    let inp = tokens |> Array.toList |> List.map makeToken
    let par = syntaxParen inp
    let body = match par with | UnparsedGroup(l) -> l | a -> [a]
    groupUnary body []
    |> UnparsedGroup |> nodeToString
    |> should equal expStr
