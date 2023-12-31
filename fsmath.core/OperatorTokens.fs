module fsmath.core.OperatorTokens

open System

let Exponential = ["^"]
let Multiplicative = ["*";"/";"x"]
let Additive = ["+";"-"]
let Unary = ["-";"+";"-/"]

let All = Unary @ Exponential @ Multiplicative @ Additive

let getUnaryOper (oper: string) =
    match oper with
    | "-" -> (fun (v:decimal) -> -v)
    | "+" -> id
    | "-/" -> (float >> Math.Sqrt >> decimal)
    | _ -> raise <| FormatException $"Unrecognized unary operator '{oper}'"

let getBinaryOper (oper: string) =
    match oper with
    | "^" -> (fun l (r:decimal) -> (float l) ** (float r) |> decimal)
    | "*" | "x" -> (fun l r -> l * r)
    | "/" -> (fun l r -> l / r)
    | "+" -> (fun l r -> l + r)
    | "-" -> (fun l r -> l - r)
    | _ -> raise <| FormatException $"Unrecognized binary operator '{oper}'"
