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

    let rec groupParen (tokens: TokenType list) (depth: int) (parenBody: SyntaxNode list) =
        match tokens with
        | ParenClose::tail ->
            if depth = 0 then
                let err = parenBody |> List.map nodeToString |> String.concat " "
                raise <| FormatException $"Unexpected closing parenthesis after: {err}"
            else (tail, parenBody)
        | ParenOpen::tail ->
            let (rem, inner) = groupParen tail (depth + 1) []
            parenBody @ [UnparsedGroup(inner)]
            |> groupParen rem depth
        | tok::tail ->
            parenBody @ [TokenWrapper(tok)]
            |> groupParen tail depth
        | [] ->
            if depth = 0 then ([], parenBody)
            else
                let err = parenBody |> List.map nodeToString |> String.concat " "
                raise <| FormatException $"Unclosed parenthesis around: {err}"

    let syntaxParen (tokens: TokenType list) =
        let (rem, body) = groupParen tokens 0 []
        if rem = [] then UnparsedGroup body
        else
            let err = body |> List.map nodeToString
            raise <| FormatException $"Unexpected closing parenthesis after: {err}"
