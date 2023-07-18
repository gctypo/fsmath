namespace fsmath.core

open System.Text

type TokenType =
    | Number of whole: string * frac: string
    | Operator of oper: string
    | ParenOpen
    | ParenClose


module Tokenizer =

    let toList (str: string) =
        str.ToCharArray() |> Array.toList

    let toString (lst: char list) =
        let sb = (StringBuilder(), lst)
                ||> List.fold (fun (s:StringBuilder) (c:char) -> s.Append c)
        sb.ToString()

    let isDigit (c: char) =
        c >= '0' && c <= '9'

    let rec tokenizeDigitSequence (feed: char list) (num: char list) =
        match feed with
        | c::tail when isDigit c ->
            num @ [c] |> tokenizeDigitSequence tail
        | _ -> (feed, num)

    let rec parseNumber (feed: char list) (tok) =
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
        else (rem, Number(toString w, toString p) |> Some)

    let isOperator (op: char list) (c: char) =
        let str = (op @ [c]) |> toString
        [| "+"; "-"; "*"; "/"; "^" |]
        |> Array.exists (fun o -> o.StartsWith str)

    let rec parseOperator (feed: char list) (op: char list) =
        match feed with
        | c::tail when isOperator op c ->
            op @ [c] |> parseOperator tail
        | _ -> (op, feed)

    let tokenizeOperator (feed: char list) =
        let (op, rem) = parseOperator feed []
        if op = [] then (rem, None)
        else (rem, op |> toString |> Operator |> Some)

    let tokenizeParen (feed: char list) =
        match feed with
        | '('::tail -> (tail, ParenOpen |> Some)
        | ')'::tail -> (tail, ParenClose |> Some)
        | _ -> (feed, None)
