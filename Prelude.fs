[<AutoOpen>]
module Prelude


open System


/// Small inline version of if.
let iif cond a b =
    if cond then a else b

/// Flips the first two parameters of a function.
let flip f a b = f b a


module Bool =
    /// Converts a bool to an ordinal value (1 for true, 0 for false).
    let ord b = if b then 1 else 0


module String =
    /// List version of String.Join
    let join (sep: string) (list: List<string>) =
        String.Join (sep, (List.toArray list))