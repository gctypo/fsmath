module fsmath.test.Evaluator_Test

open NUnit.Framework
open FsUnit

open fsmath.core
open fsmath.core.Evaluator

[<Test>]
[<TestCase("1.22", 1.22)>]
[<TestCase(".22", 0.22)>]
[<TestCase("10", 10.0)>]
let evaluateLiteral_Test (literal: string, exp: decimal) =
    LiteralValue(literal)
    |> evaluateNode
    |> should equal exp
