module fsmath.test.Tokenizer_Test

open NUnit.Framework
open FsUnit

open fsmath.core
open fsmath.core.Tokenizer


[<Test>]
[<TestCase('0', true)>]
[<TestCase('9', true)>]
[<TestCase('/', false)>]
[<TestCase(':', false)>]
let isDigit_Test (c: char, exp: bool) =
    isDigit c
    |> should equal exp

[<Test>]
[<TestCase("123", "123", "")>]
[<TestCase("123+4", "123", "+4")>]
[<TestCase("123.", "123", ".")>]
[<TestCase("+4", "", "+4")>]
let tokenizeDigitSequence_Test (inp: string, expNum: string, expRem: string) =
    let (rem, num) = tokenizeDigitSequence (toList inp) []
    num |> should equal expNum
    rem |> should equal expRem

[<Test>]
[<TestCase("123", "123", "", "")>]
[<TestCase("123.4", "123", "4", "")>]
[<TestCase(".456", "", "456", "")>]
[<TestCase("123.", "123", "", "")>]
[<TestCase("+7", "", "", "+7")>]
[<TestCase("123 4", "123", "", " 4")>]
let tokenizeNumber_Test (inp: string, expWhole: string, expFrac: string, expRem: string) =
    let (rem, num) = tokenizeNumber (toList inp)
    if (expWhole = "" && expFrac = "") then
        num |> should equal None
    else
        match num.Value with
        | Number(w, f) -> (w, f) |> should equal (expWhole, expFrac)
        | x -> Assert.Fail $"Wrong token {x}"
    rem |> lstToString |> should equal expRem

[<Test>]
[<TestCase("+7", "+", "7")>]
[<TestCase("78+", "", "78+")>]
[<TestCase("++", "+", "+")>]
[<TestCase("/^", "/", "^")>]
let tokenizeOperator_Test (inp: string, expOp: string, expRem: string) =
    let (rem, op) = inp |> toList |> tokenizeOperator
    if expOp = "" then
        op |> should equal None
    else
        match op.Value with
        | Operator o -> o |> should equal expOp
        | x -> Assert.Fail $"Wrong token {x}"
    rem |> lstToString |> should equal expRem

[<Test>]
[<TestCase("(123", "(", "123")>]
[<TestCase(")+123", ")", "+123")>]
[<TestCase("12(", "", "12(")>]
let tokenizeParen_Test (inp: string, expPar: string, expRem: string) =
    let (rem, par: TokenType Option) = inp |> toList |> tokenizeParen
    if expPar = "" then
        par |> should equal None
    else
        match par.Value with
        | ParenOpen -> "(" |> should equal expPar
        | ParenClose -> ")" |> should equal expPar
        | x -> Assert.Fail $"Wrong token {x}"
    rem |> lstToString |> should equal expRem

[<Test>]
[<TestCase("   ", "")>]
[<TestCase(" 33", "33")>]
[<TestCase("\t.33", ".33")>]
[<TestCase(".33 ", ".33")>]
let tokenizeIgnore_Test (inp: string, expRem: string) =
    let (rem, ws: TokenType Option) = inp |> toList |> tokenizeIgnored
    if ws.IsSome then
        match ws.Value with
        | Whitespace -> Assert.Pass "Cleared whitespace"
        | x -> Assert.Fail $"Wrong token {x}"
    rem |> lstToString |> should equal expRem

[<Test>]
[<TestCase("123", "IsNum")>]
[<TestCase(".123", "IsNum")>]
[<TestCase(".123", "IsNum")>]
[<TestCase(".ayy", "NoMatch")>]
[<TestCase("(", "IsParen")>]
[<TestCase(")", "IsParen")>]
[<TestCase("^", "IsOper")>]
[<TestCase("   \t  ", "IsIgnore")>]
let tokenMatch_Test (inp: string, expMatch: string) =
    expMatch |> should equal <|
    match inp |> toList with
    | IsNum -> "IsNum"
    | IsOper -> "IsOper"
    | IsParen -> "IsParen"
    | IsIgnore -> "IsIgnore"
    | NoMatch -> "NoMatch"

[<Test>]
[<TestCase("3+4", [| "3"; "+"; "4" |])>]
[<TestCase(" 3 + 4 ", [| "3"; "+"; "4" |])>]
[<TestCase("-43.4", [| "-"; "43.4" |])>]
[<TestCase("3*(8+2.4)", [| "3"; "*"; "("; "8"; "+"; "2.4"; ")" |])>]
let tokenize_Test_Pass (inp: string, exp: string[]) =
    inp |> toList |> tokenize
    |> List.map tokToString |> List.toArray
    |> should equivalent exp
