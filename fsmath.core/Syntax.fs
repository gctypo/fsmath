namespace fsmath.core

open System

type SyntaxNode =
    | TokenGroup of unparsed: TokenType list
    | UnparsedGroup of group: SyntaxNode list
    | TokenWrapper of token: TokenType

module Syntax =

    let toksToString (tokens: TokenType list) =
        tokens |> List.map Tokenizer.tokToString
        |> String.concat " "

    let rec nodeToString (node: SyntaxNode) =
        match node with
        | TokenGroup u -> u |> toksToString
        | UnparsedGroup n ->
            "(" + (n |> List.map nodeToString |> String.concat " ") + ")"
        | TokenWrapper n -> n |> Tokenizer.tokToString

    let rec groupParen (tokens: TokenType list) (parenBody: SyntaxNode list) =
        match tokens with
        | ParenClose::tail -> (tail, parenBody)
        | ParenOpen::tail ->
            let (rem, inner) = groupParen tail []
            parenBody @ [UnparsedGroup(inner)]
            |> groupParen rem
        | tok::tail -> parenBody @ [TokenWrapper(tok)] |> groupParen tail
        | [] -> ([], parenBody)

    let syntaxParen (tokens: TokenType list) =
        let (rem, body) = groupParen tokens []
        if rem <> [] then
            let err = body |> List.map nodeToString
            raise <| FormatException $"Unmatched closing parenthesis around: {err}"
        else UnparsedGroup body
