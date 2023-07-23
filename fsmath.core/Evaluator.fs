module fsmath.core.Evaluator

open System

let rec evaluateNode (node: SyntaxNode) =
    match node with
    | LiteralValue v -> Decimal.Parse(v)
    | BinaryExpression(lhs, oper, rhs) ->
        (evaluateNode lhs, evaluateNode rhs)
        ||> OperatorTokens.getBinaryOper oper
    | UnaryExpression(oper, rhs) ->
        evaluateNode rhs
        |> OperatorTokens.getUnaryOper oper
    | _ -> invalidArg (nameof node) $"Cannot evaluate incomplete syntax: {node}"
