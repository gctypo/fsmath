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

let arrayToWrappedTokens (tokens: string[]) =
    tokens |> Array.toList |> List.map (makeToken >> TokenWrapper)

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
[<TestCase([|"-";"100";"*";"-";"100"|], "- '100' * - '100'")>]
let parseLiterals_Test (tokens: string[], expr: string) =
    tokens |> Array.toList |> List.map makeToken
    |> syntaxLiterals
    |> List.map nodeToString |> String.concat " "
    |> should equal expr

[<Test>]
[<TestCase([|"("; "-"; "1.22"; ")"; "^"; "2"|], "{- 1.22} ^ 2")>]
[<TestCase([|"(";")";"*";"(";"(";")";")"|], "{} * {{}}")>]
let syntaxParen_Test (tokens: string[], expBody: string) =
    tokens |> arrayToWrappedTokens
    |> syntaxParen
    |> nodesToString
    |> should equal expBody

[<Test>]
[<TestCase([|"(";"1.22"|], "1.22")>]
[<TestCase([|"-";"(";"-";"(";"1.22"|], "1.22")>]
[<TestCase([|"(";"1.22";")";")"|], "{1.22}")>]
let syntaxParen_Test_Fail (tokens: string[], expMsg: string) =
    let inp = tokens |> arrayToWrappedTokens
    (fun () -> syntaxParen inp |> ignore)
    |> should (throwWithPartialMessage $": {expMsg}") typeof<FormatException>

[<Test>]
[<TestCase([|"-";"100"|], "(-100)")>]
[<TestCase([|"3";"-";"100"|], "3 - 100")>]
[<TestCase([|"-";"100";"*";"-";"100"|], "(-100) * (-100)")>]
let groupUnary_Test (tokens: string[], expr: string) =
    let inp = tokens |> arrayToWrappedTokens
    groupUnary inp []
    |> nodesToString
    |> should equal expr

[<Test>]
[<TestCase([|"-";"100";"*";"(";"3";"-";"100";")"|], "(-100) * {3 - 100}")>]
[<TestCase([|"100";"*";"-";"(";"3";"-";"100";")"|], "100 * (-{3 - 100})")>]
[<TestCase([|"-";"100";"*";"(";"3";"*";"-";"100";")"|], "(-100) * {3 * (-100)}")>]
[<TestCase([|"-";"(";"3";"+";"4";")"|], "(-{3 + 4})")>]
[<TestCase([|"3";"*";"(";"-";"100";")"|], "3 * {(-100)}")>]
let groupUnary_Test_Paren (tokens: string[], expr: string) =
    let body = tokens |> arrayToWrappedTokens |> syntaxParen
    groupUnary body []
    |> nodesToString
    |> should equal expr

[<Test>]
[<TestCase([|"3";"+";"2"|], "(3+2)")>]
[<TestCase([|"3";"+";"4";"+";"2";"+";"1"|], "(((3+4)+2)+1)")>]
[<TestCase([|"(";"3";"+";"4";")";"+";"(";"2";"+";"1";")"|], "((3+4)+(2+1))")>]
let groupBinary_Test (tokens: string[], expr: string) =
    let body = tokens |> arrayToWrappedTokens |> syntaxParen
    groupBinary body
    |> nodeToString
    |> should equal expr

// This is about to get complicated
[<Test>]
let groupBinary_Test_AroundUnary () =
    let body =
        [ UnparsedGroup([UnaryExpression("-", LiteralValue("100"))]);
            TokenWrapper(Operator("*"));
            UnaryExpression("-", LiteralValue("100")); ]
    groupBinary body
    |> nodeToString
    |> should equal "((-'100')*(-'100'))"

[<Test>]
let groupBinary_Test_WithinUnary () =
    let body =
        [ UnaryExpression("-",
            UnparsedGroup([
                LiteralValue("3");
                TokenWrapper(Operator("+"));
                LiteralValue("4");
            ]));
            TokenWrapper(Operator("*"));
            LiteralValue("100")
        ]
    groupBinary body
    |> nodeToString
    |> should equal "((-('3'+'4'))*'100')"

[<Test>]
[<TestCase([|"3";"+";"4";"+";"2";"+";"1"|], "((('3'+'4')+'2')+'1')")>]
[<TestCase([|"(";"3";"+";"4";")";"+";"(";"2";"+";"1";")"|], "(('3'+'4')+('2'+'1'))")>]
[<TestCase([|"-";"100";"*";"-";"100"|], "((-'100')*(-'100'))")>]
[<TestCase([|"-";"(";"3";"+";"4";")";"*";"100"|], "((-('3'+'4'))*'100')")>]
let parseToTree_Test (tokens: string[], expr: string) =
    let body = tokens |> Array.toList |> List.map makeToken
    parseToTree body
    |> nodeToString
    |> should equal expr
