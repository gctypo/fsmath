namespace fsmath.core

open System
open System.Text


type TokenType =
    | Number of whole: string * frac: string
    | Operator of oper: string
    | ParenOpen
    | ParenClose
    | Whitespace


module Tokenizer =

    let toList (str: string) =
        str.ToCharArray() |> Array.toList

    let lstToString (lst: char list) =
        let sb = (StringBuilder(), lst)
                ||> List.fold (fun (s:StringBuilder) (c:char) -> s.Append c)
        sb.ToString()

    let tokToString (tok: TokenType) =
        match tok with
        | Number (w, f) when f = "" -> $"{w}"
        | Number (w, f) -> $"{w}.{f}"
        | Operator o -> o
        | ParenOpen -> "("
        | ParenClose -> ")"
        | Whitespace -> " "

    let isDigit (c: char) =
        c >= '0' && c <= '9'

    let rec tokenizeDigitSequence (feed: char list) (num: char list) =
        match feed with
        | c::tail when isDigit c ->
            num @ [c] |> tokenizeDigitSequence tail
        | _ -> (feed, num)

    let rec parseNumber (feed: char list) (tok: char list) =
        match feed with
        | c::tail when isDigit c ->
            let (rem, n) = tokenizeDigitSequence tail [c]
            parseNumber rem n
        | '.'::c::tail when isDigit c ->
            let (rem, n) = tokenizeDigitSequence tail [c]
            (tok, n, rem)
        | '.'::tail -> (tok, [], tail)
        | _ -> (tok, [], feed)

    let tokenizeNumber (feed: char list) =
        let (w, p, rem) = parseNumber feed []
        if (w = [] && p = []) then (rem, None)
        else (rem, Number(lstToString w, lstToString p) |> Some)

    let isPartOfOperator (op: char list) (c: char) =
        let str = (op @ [c]) |> lstToString
        OperatorTokens.All
        |> List.exists (fun o -> o.StartsWith str)

    let rec parseOperator (feed: char list) (op: char list) =
        match feed with
        | c::tail when isPartOfOperator op c ->
            op @ [c] |> parseOperator tail
        | _ -> (op, feed)

    let tokenizeOperator (feed: char list) =
        let (op, rem) = parseOperator feed []
        if op = [] then (rem, None)
        else (rem, op |> lstToString |> Operator |> Some)

    let tokenizeParen (feed: char list) =
        match feed with
        | '('::tail -> (tail, ParenOpen |> Some)
        | ')'::tail -> (tail, ParenClose |> Some)
        | _ -> (feed, None)

    let rec tokenizeIgnored (feed: char list) =
        match feed with
        | ' '::tail -> tokenizeIgnored tail
        | '\t'::tail -> tokenizeIgnored tail
        | _ -> (feed, Whitespace |> Some)

    let (|IsParen|IsOper|IsNum|IsIgnore|NoMatch|) (feed: char list) =
        match feed with
        | '('::_
        | ')'::_ -> IsParen
        | o::_ when o |> isPartOfOperator [] -> IsOper
        | '.'::d::_ when d |> isDigit -> IsNum
        | d::_ when d |> isDigit -> IsNum
        | ' '::_ -> IsIgnore
        | _ -> NoMatch

    let rec tokenizePart (feed: char list) (toks: TokenType list) =
        if feed = [] then
            (feed, toks)
        else
            let (tail, res) =
                match feed with
                | IsParen -> tokenizeParen feed
                | IsOper -> tokenizeOperator feed
                | IsNum -> tokenizeNumber feed
                | IsIgnore -> tokenizeIgnored feed
                | NoMatch -> (feed, None)
            if res |> Option.isSome then
                if res.Value = Whitespace then toks
                else toks @ [res.Value]
                |> tokenizePart tail
            else
                FormatException($"Unrecognized token at {feed}") |> raise

    let tokenize (feed: char list) =
        let (tail, res) = tokenizePart feed []
        if tail = [] then res
        else FormatException($"Incomplete parsing. Remainder: {tail}") |> raise
