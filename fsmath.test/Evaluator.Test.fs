module fsmath.test.Evaluator_Test

open Microsoft.VisualStudio.TestPlatform.ObjectModel
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

[<Test>]
[<TestCase("-","100", -100)>]
let evaluateUnary_Test (op: string, rhs: string, exp: decimal) =
    UnaryExpression(op, LiteralValue(rhs))
    |> evaluateNode
    |> should equal exp

[<Test>]
[<TestCase("3","+","4", 7)>]
[<TestCase("3","-","4", -1)>]
[<TestCase("3","*","4", 12)>]
[<TestCase("3","/","4", 0.75)>]
[<TestCase("3","^","4", 81)>]
let evaluateBinary_Test (lhs: string, op: string, rhs: string, exp: decimal) =
    BinaryExpression(LiteralValue(lhs), op, LiteralValue(rhs))
    |> evaluateNode
    |> should equal exp
