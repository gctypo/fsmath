module fsmath.core.Syntax

open System

type SyntaxNode =
    | TokenGroup of unparsed: TokenType list

let toksToString (tokens: TokenType list) =
    tokens |> List.map Tokenizer.tokToString
    |> String.concat " "

let nodeToString (node: SyntaxNode) =
    match node with
    | TokenGroup u -> u |> toksToString

let rec groupParen (tokens: TokenType list) (within: TokenType list) =
    match tokens with
    | ParenClose::tail -> (tail, within)
    | ParenOpen::tok::tail -> within @ [tok] |> groupParen tail
    | tok::tail -> within @ [tok] |> groupParen tail
    | [] -> raise <| FormatException($"Unmatched parentheses around: {within |> toksToString}")

let syntaxParen (tokens: TokenType list) =
    let (rem, body) = groupParen tokens []
    (rem, TokenGroup(body))
