namespace fsmath.core

open System

type SyntaxNode =
    | UnparsedGroup of group: SyntaxNode list
    | TokenWrapper of token: TokenType
    | BinaryExpression of lhs: SyntaxNode * oper: string * rhs: SyntaxNode
    | UnaryExpression of oper: string * rhs: SyntaxNode


module Syntax =

    let toksToString (tokens: TokenType list) =
        tokens |> List.map Tokenizer.tokToString
        |> String.concat " "

    let rec nodeToString (node: SyntaxNode) =
        match node with
        | UnparsedGroup n ->
            "(" + (n |> List.map nodeToString |> String.concat " ") + ")"
        | TokenWrapper n -> n |> Tokenizer.tokToString
        | BinaryExpression(l, o, r) ->
            "[" + (nodeToString l) + o + (nodeToString r) + "]"
        | UnaryExpression(o, r) ->
            "[" + o + (nodeToString r) + "]"

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

    let (|OperNode|EvalNode|OtherNode|) (node: SyntaxNode) =
        match node with
        | TokenWrapper tok ->
            match tok with
            | Number _ -> EvalNode node
            | Operator o -> OperNode o
            | _ -> OtherNode node
        | UnparsedGroup l -> EvalNode node
        | BinaryExpression _ -> EvalNode node
        | UnaryExpression _ -> EvalNode node

    let rec groupUnary (nodes: SyntaxNode list) (accum: SyntaxNode list) =
        match nodes with
        | OperNode(o)::EvalNode(r)::tail ->
            accum @ [UnaryExpression(o, r)]
            |> groupUnary tail
        | n::tail -> accum @ [n] |> groupUnary tail
        | [] -> accum
