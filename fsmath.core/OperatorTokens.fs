module fsmath.core.OperatorTokens

open System

let Exponential = ["^"]
let Multiplicative = ["*";"/"]
let Additive = ["+";"-"]

let All = Exponential @ Multiplicative @ Additive

let getUnaryOper (oper: string) =
    match oper with
    | "-" -> (fun (v:decimal) -> -v)
    | _ -> raise <| FormatException $"Unrecognized unary operator '{oper}'"

let getBinaryOper (oper: string) =
    match oper with
    | "^" -> (fun l (r:decimal) -> (float l) ** (float r) |> decimal)
    | "*" -> (fun l r -> l * r)
    | "/" -> (fun l r -> l / r)
    | "+" -> (fun l r -> l + r)
    | "-" -> (fun l r -> l - r)
    | _ -> raise <| FormatException $"Unrecognized binary operator '{oper}'"
