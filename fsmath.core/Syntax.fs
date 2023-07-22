namespace fsmath.core

open System

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
            "{" + (l |> List.map (fun n -> n.ToString())
                |> String.concat " ") + "}"
        | BinaryExpression(l, o, r) ->
            "(" + l.ToString() + o + r.ToString() + ")"
        | UnaryExpression(o, r) ->
            "(" + o + r.ToString() + ")"
        | LiteralValue v -> v


module Syntax =

    let toksToString (tokens: TokenType list) =
        tokens |> List.map Tokenizer.tokToString
        |> String.concat " "

    let nodeToString (node: SyntaxNode) = node.ToString()

    let nodesToString (nodes: SyntaxNode list) =
        nodes |> List.map nodeToString |> String.concat " "

    let unpackNode (node: SyntaxNode) =
        match node with
        | UnparsedGroup grp -> grp
        | a -> [a]

    let packNodes (nodes: SyntaxNode list) =
        match nodes with
        | [n] -> n
        | _ -> UnparsedGroup nodes

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

    let rec groupParen (unparsed: SyntaxNode list) (depth: int) (parenBody: SyntaxNode list) =
        match unparsed with
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

    let syntaxParen (unparsed: SyntaxNode list) =
        let (rem, body) = groupParen unparsed 0 []
        if rem = [] then body
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

    let rec groupUnary (unparsed: SyntaxNode list) (parsed: SyntaxNode list) =
        match unparsed with
        // Ex: {- 100} -> {(-100)}
        | OperNode(op)::EvalNode(rhs)::tail when parsed = [] ->
            parsed @ [UnaryExpression(op, rhs)]
            |> groupUnary tail
        // Ex: {* - 100} -> {* (-100)}
        | OperNode(seekHead)::OperNode(op)::EvalNode(rhs)::tail ->
            let rewrappedSeek = Operator seekHead |> TokenWrapper
            parsed @ [rewrappedSeek] @ [UnaryExpression(op, rhs)]
            |> groupUnary tail
        // Ex: {{- 100} * - 100} -> {{(-100)} * (-100)}
        | UnparsedGroup(inner)::tail ->
            let parBody = groupUnary inner [] |> UnparsedGroup
            parsed @ [parBody] |> groupUnary tail
        | n::tail -> parsed @ [n] |> groupUnary tail
        | [] -> parsed

    let syntaxUnary (unparsed: SyntaxNode list) = groupUnary unparsed []

    // Here be dragons
    let rec syntaxBinaryOrd (ops: string[]) (unparsed: SyntaxNode list) =
        // Repeat syntaxBinaryOrd recursively on all child nodes
        let rec descend (node: SyntaxNode) =
            match node with
            | UnparsedGroup grp -> grp |> syntaxBinaryOrd ops
            | UnaryExpression(o, r) ->
                (o, descend r) |> UnaryExpression
            | BinaryExpression(l, o, r) ->
                (descend l, o, descend r) |> BinaryExpression
            | LiteralValue _ | TokenWrapper _ -> node
        // binds binop pairs while filtering nodes
        let rec groupBinaryOrd (ops: string[]) (unparsed: SyntaxNode list) (parsed: SyntaxNode list) =
            match unparsed with
            // {}3 * 4 * 2 -> {}(3*4) * 2   Does NOT add new expression to parsed list
            | EvalNode(lhs)::OperNode(op)::EvalNode(rhs)::tail when ops |> Array.contains op ->
                let newTail = BinaryExpression(descend lhs, op, descend rhs)::tail
                groupBinaryOrd ops newTail parsed
            // {}3 + 4 * 2 -> {3 + 4} * 2
            | n::tail ->
                parsed @ [descend n]
                |> groupBinaryOrd ops tail
            // {({3 + 4}*2)} -> done
            | [] -> parsed
        groupBinaryOrd ops unparsed [] |> packNodes

    let BIN_OPS_POW = [|"^"|]
    let BIN_OPS_MULT = [|"*";"/"|]
    let BIN_OPS_ADD = [|"+";"-"|]

    let syntaxBinary (unparsed: SyntaxNode list) =
        unparsed
        |> syntaxBinaryOrd BIN_OPS_POW |> unpackNode
        |> syntaxBinaryOrd BIN_OPS_MULT |> unpackNode
        |> syntaxBinaryOrd BIN_OPS_ADD

    let parseToTree (tokens: TokenType list) =
        tokens
        |> syntaxLiterals
        |> syntaxParen
        |> syntaxUnary
        |> syntaxBinary
