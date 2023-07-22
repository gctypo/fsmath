namespace fsmath.core

open System
open System.Linq.Expressions

type SyntaxNode =
    | UnparsedGroup of group: SyntaxNode list
    | TokenWrapper of token: TokenType
    | BinaryExpression of lhs: SyntaxNode * oper: string * rhs: SyntaxNode
    | UnaryExpression of oper: string * rhs: SyntaxNode
    | LiteralValue of value: string

    override this.ToString() =
        match this with
        | TokenWrapper n -> n |> Tokenizer.tokToString
        | UnparsedGroup l ->
            "(" + (l |> List.map (fun n -> n.ToString())
                |> String.concat " ") + ")"
        | BinaryExpression(l, o, r) ->
            "[" + l.ToString() + o + r.ToString() + "]"
        | UnaryExpression(o, r) ->
            "[" + o + r.ToString() + "]"
        | LiteralValue v -> "'" + v + "'"


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
        | LiteralValue v -> "'" + v + "'"

    let (|IsLiteral|NotLiteral|) (node: SyntaxNode) =
        match node with
        | TokenWrapper tok ->
            match tok with
            | Number(w, f) -> IsLiteral(w, f)
            | _ -> NotLiteral node
        | _ -> NotLiteral node

    let rec parseLiterals (nodes: SyntaxNode list) (accum: SyntaxNode list) =
        match nodes with
        | IsLiteral(w,f)::tail ->
            let litStr = if f = "" then w else $"{w}.{f}"
            accum @ [LiteralValue(litStr)] |> parseLiterals tail
        | NotLiteral(n)::tail -> accum @ [n] |> parseLiterals tail
        | [] -> accum

    let syntaxLiterals (tokens: TokenType list) =
        let inp = tokens |> List.map TokenWrapper
        parseLiterals inp []

    let (|ParenCloseNode|ParenOpenNode|OtherByParen|) (node: SyntaxNode) =
        match node with
        | TokenWrapper tok ->
            match tok with
            | ParenOpen -> ParenOpenNode
            | ParenClose -> ParenCloseNode
            | _ -> OtherByParen node
        | _ -> OtherByParen node

    let rec groupParen (tokens: SyntaxNode list) (depth: int) (parenBody: SyntaxNode list) =
        match tokens with
        | ParenCloseNode::tail ->
            if depth = 0 then
                let err = parenBody |> List.map nodeToString |> String.concat " "
                raise <| FormatException $"Unexpected closing parenthesis after: {err}"
            else (tail, parenBody)
        | ParenOpenNode::tail ->
            let (rem, inner) = groupParen tail (depth + 1) []
            parenBody @ [UnparsedGroup(inner)]
            |> groupParen rem depth
        | OtherByParen(node)::tail -> parenBody @ [node] |> groupParen tail depth
        | [] ->
            if depth = 0 then ([], parenBody)
            else
                let err = parenBody |> List.map nodeToString |> String.concat " "
                raise <| FormatException $"Unclosed parenthesis around: {err}"

    let syntaxParen (tokens: SyntaxNode list) =
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
        | LiteralValue _ -> EvalNode node

    let rec groupUnary (nodes: SyntaxNode list) (accum: SyntaxNode list) =
        match nodes with
        | OperNode(o)::EvalNode(r)::tail when accum = [] ->
            accum @ [UnaryExpression(o, r)]
            |> groupUnary tail
        | OperNode(lop)::OperNode(un)::EvalNode(r)::tail ->
            let lopNode = Operator lop |> TokenWrapper
            accum @ [lopNode] @ [UnaryExpression(un, r)]
            |> groupUnary tail
        | UnparsedGroup(inner)::tail ->
            let parBody = groupUnary inner [] |> UnparsedGroup
            accum @ [parBody] |> groupUnary tail
        | n::tail -> accum @ [n] |> groupUnary tail
        | [] -> accum

    let rec groupBinary (nodes: SyntaxNode list) =
        match nodes with
        | EvalNode(ln)::OperNode(o)::EvalNode(rn)::tail ->
            let lhs = match ln with | UnparsedGroup grp -> groupBinary grp | a -> a
            let rhs = match rn with | UnparsedGroup grp -> groupBinary grp | a -> a
            BinaryExpression(lhs, o, rhs)::tail
            |> groupBinary
        | [n] -> n
        | _ -> raise <| FormatException $"Cannot group binary operators without nodes"

