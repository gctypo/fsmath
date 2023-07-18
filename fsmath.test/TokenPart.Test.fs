module fsmath.test

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
        rem |> toString |> should equal expRem
    else
        match num.Value with
        | Number(w, f) ->
            (w, f) |> should equal (expWhole, expFrac)
        | o -> Assert.Fail $"Wrong token {o}"
        rem |> toString |> should equal expRem

[<Test>]
[<TestCase("+7", "+", "7")>]
[<TestCase("78+", "", "78+")>]
[<TestCase("++", "++", "")>]
let tokenizeOperator_Test (inp: string, expOp: string, expRem: string) =
    let (rem, op) = inp |> toList |> tokenizeOperator
    if expOp = "" then
        op |> should equal None
    else
        match op.Value with
        | Operator o -> o |> should equal expOp
        | x -> Assert.Fail $"Wrong token {x}"
    rem |> toString |> should equal expRem
