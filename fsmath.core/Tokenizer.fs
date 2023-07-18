namespace fsmath.core

open System.Text

type TokenType =
    | Number of whole: string * frac: string
    | Operator of oper: string
    | ParenOpen
    | ParenClose

type TokenizerResult =
    | Complete of TokenType
    | Partial of partial: string
    | Empty

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
        //(rem, Number(toString w, toString p) |> Some)
