[<AutoOpen>]
module Prelude

open System


/// Small inline version of if.
let iif cond a b =
    if cond then a else b


/// Flips the first two parameters of a function.
let flip f a b = f b a


/// Create a function that ignores the input and always returns the provided value.
let always a = fun _ -> a


module Bool =
    /// Converts a bool to an ordinal value (1 for true, 0 for false).
    let ord b = iif b 1 0


module String =
    /// List version of String.Join
    let join (sep: string) (list: List<string>) =
        String.Join (sep, (List.toArray list))


module Option =
    /// Get the value from an Option, or throw the provided exception.
    let getOrThrow (ex: Lazy<Exception>) option =
        match option with
        | Some value -> value
        | _ -> raise ex.Value


module List =
    /// Prefix cons operator.
    let cons x xs = x :: xs

    /// Convert a list of characters to a string.
    let toString = Array.ofList >> System.String

    /// List.choose which allows for an index parameter.
    let choosei fn list =
        list |> List.mapi fn |> List.choose id


module Int =
    /// Attempts to parse an integer from a string. Returns Some if it succeeds, None otherwise.
    let tryParse (str: string) =
        match Int32.TryParse str with
        | true, result -> Some result
        | _ -> None


module Long =
    /// Attempts to parse a long from a string. Returns Some if it succeeds, None otherwise.
    let tryParse (str: string) =
        match Int64.TryParse str with
        | true, result -> Some result
        | _ -> None


module Char =
    /// Checks whether a character is a regular ASCII alphabetical letter. Different from Char.IsLetter, which
    /// also recognizes letters with accents etc as a letter.
    let isAlpha char =
        let code = int char
        let isLower = 0x61 <= code && code <= 0x7A
        let isUpper = code <= 0x5A && 0x41 <= code
        isLower || isUpper

    /// Convenience alias for Char.IsDigit.
    let isNum = Char.IsDigit

    /// Checks whether a character is alphabetical or numerical.
    let isAlphaNum char = Char.IsDigit char || isAlpha char