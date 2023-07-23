module fsmath.core.Evaluator

open System

let rec evaluateNode (node: SyntaxNode) =
    match node with
    | LiteralValue v -> Decimal.Parse(v)
    | _ -> invalidArg (nameof node) $"Cannot evaluate incomplete syntax: {node}"
