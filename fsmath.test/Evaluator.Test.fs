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

[<Test>]
let evaluateNode_Test_Nested () =
    let expr =
        BinaryExpression(
            UnaryExpression("-",
                BinaryExpression(LiteralValue("3"),
                    "+", LiteralValue("4") )),
            "*", LiteralValue("10"))
    expr |> Syntax.nodeToString |> should equal "((-(3+4))*10)"
    expr |> evaluateNode
    |> should equal -70

[<Test>]
[<TestCase("3+4", 7)>]
[<TestCase("3 + 4", 7)>]
[<TestCase("3 + (4)", 7)>]
[<TestCase("-10 * -10", 100)>]
[<TestCase("3 + 4 + 2 + 1", 10)>]
[<TestCase("3 * 4 + 2 * 1", 14)>]
[<TestCase("3 + 4 * 2 + 1", 12)>]
[<TestCase("3 * (4 + 2) * 1", 18)>]
[<TestCase("-(3 + 4) * 100", -700)>]
[<TestCase("-(-7 + 4) * 100", 300)>]
[<TestCase("-(3 + -7) * 100", 400)>]
let evaluateString_Test (input: string, exp: decimal) =
    evaluateString input
    |> should equal exp
