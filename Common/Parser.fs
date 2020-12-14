module Parser


/// A parser can take a string as input, and if it succeeds, returns the parsed element and the remaining string.
type Parser<'a> = Parser of (string -> Option<'a * string>)


/// Run a parser, returning its raw output.
let run (Parser p) (input: string) =
    p input


/// Run a parser, returning the resulting value as an Option.
let parse p = run p >> Option.map fst


/// Run a parser, returning its resulting value or failing with the provided exception.
let parseOrFail p ex = parse p >> Option.getOrThrow ex


/// Basic parsers.
module Basic =
    /// Returns the complete input without consuming it.
    let look = Parser (fun s -> Some (s, s))

    /// Returns a parser that doesn't consume input and always succeeds with the provided value.
    let succeed result = Parser (fun s -> Some (result, s ))

    /// Returns a parser that always fails.
    let fail _ = Parser (always None)

    /// Returns a parser that succeeds if the provided Option is Some, and fails otherwise.
    let fromOption option =
        match option with
        | Some r -> succeed r
        | _ -> fail ()


/// Standard parser combinators.
module Combinators =
    /// A parser that checks the provided parser, and fails if it succeeds. If the specified parser fails, this parser
    /// succeeds but does not consume any input.
    ///
    /// This parser can be used for example to peek ahead and check that a certain sequence does not occur further down.
    let not (Parser p) : Parser<unit> = Parser (fun s -> match p s with | None -> Some ( (), s) | _ -> None)

    /// Returns a parser that peeks ahead and checks that a sequence occurs further down the input. Does not consume
    /// input, but fails when the input doesn't occur.
    let peek p : Parser<unit> = not p |> not

    /// Returns a parser which is equivalent to applying two given parsers after each other and combining their result
    /// using the provided function.
    let sequence combine p1 p2 =
        Parser (fun s1 ->
            match run p1 s1 with
            | None -> None
            | Some (r1, s2) ->
                match run p2 s2 with
                | None -> None
                | Some (r2, s3) -> Some (combine r1 r2, s3))

    /// Similar to `sequence` except the order in which the parsers are invoked is inverted. This function can be used
    /// when piping the result of an earlier parser into a new parser, where with sequence this would result in the
    /// parsers working backwards, this function performs them in the expected order.
    let andThen combine = flip (sequence combine)

    /// Returns a parser which is equivalent to applying two given parsers after each other and discarding the result
    /// of the second.
    let left p1 p2 = sequence (fun l _ -> l) p1 p2

    /// Returns a parser which is equivalent to applying two given parsers after each other and discarding the result
    /// of the first.
    let right p1 p2 = sequence (fun _ r -> r) p1 p2

    /// Returns a parser which can be used to pipe the reuslt of a previous parser into a new parser that will be
    /// ignored.
    let skip p1 p2 = andThen (fun a _ -> a) p1 p2

    /// Returns a parser which can be used to pipe the reuslt of a previous parser into a new parser, where only the
    /// result of the second parser is kept.
    let discard p1 p2 = andThen (fun _ b -> b) p1 p2

    /// Returns a parser which applies the result of the first parser to the result of the second parser.
    let apply pf = sequence (fun f a -> f a) pf

    /// A flipped version of `apply` which can be used when chaining parsers with one parser per constructor argument
    /// of the type being parsed.
    let andMap (p: Parser<'a>) (pf : (Parser<'a -> 'b>)) = (flip apply) p pf

    /// Sequentially combines two parsers where the second can depend on the outcome of the first.
    let bind f (Parser p) = Parser (fun s1 -> Option.bind (fun ( r1, s2 ) -> run (f r1) s2) (p s1))

    /// Returns a parser which returns the transformed result of the given parser.
    let map f = bind (f >> Basic.succeed)

    /// Returns a parser which is equivalent to the first parser, if it succeeds, and otherwise equivalent to the second
    /// parser.
    let alt (Parser p1) (Parser p2) = Parser (fun s -> Option.orElseWith (fun _ -> p2 s) (p1 s))

    /// Returns a parser that tries all parsers in the list in sequence, where the result will be equivalent to that of
    /// the first parser that succeeds. If all parsers fail, or the list is empty, the parser fails.
    let rec oneOf parsers =
        match parsers with
        | [] -> Basic.fail ()
        | [ p ] -> p
        | p :: ps -> alt p (oneOf ps)

    /// Returns a parser which applies a predicate to the outcome of a given parser. The resulting parser succeeds iff the
    /// original parser succeeds and the predicate holds for its results.
    let check f p =
        bind (fun s -> iif (f s) (Basic.succeed s) (Basic.fail ())) p


/// Parsers that perform similar operations as regex operators.
module Regex =
    /// Returns a parser which represents application of the given parser zero or more times. The result is returned in a list
    /// that has 0 or more elements.
    let rec star p = Combinators.alt (Combinators.bind (fun r -> Combinators.map (List.cons r) (star p)) p) (Basic.succeed [])

    /// Returns a parser which represents application of the given parser one or more times. The result is returned in a list
    /// that has 1 or more elements.
    let plus p = Combinators.sequence List.cons p (star p)

    /// Returns a parser which represents application of the given parser exactly the given number of times.
    let rec times n p =
        if n <= 0 then
            Basic.succeed []
        else
            Combinators.sequence List.cons p (times (n - 1) p)

    /// Returns a parser that applies the parser once and if it fails pretends that the parser was never applied.
    let optional p = Combinators.alt (Combinators.map Some p) (Basic.succeed None)


open System


/// Parsers for characters.
module Char =
    /// A parser which returns the first character of the remaining input. Fails if the end of the input was reached.
    let char = Parser (fun s -> match Seq.toList s with | x :: xs -> Some ( x, List.toString xs ) | _ -> None)

    /// A parser that parses the first character, and succeeds only if it is a numerical character.
    let num = Combinators.check Char.isNum char

    /// A parser that parses the first character, and succeeds only if it is an alphabetical character.
    let alpha = Combinators.check Char.isAlpha char

    /// A parser that parses the first character, and succeeds only if it is an alphanumerical character.
    let alphaNum = Combinators.check Char.isAlphaNum char

    /// A parser that parses the first character, and succeeds only if it is a letter. Using the .NET definition of
    /// Char.IsLetter. Note that this is not compatible with Elm's Char.isAlpha.
    let letter = Combinators.check Char.IsLetter char

    /// A parser that parses the first character, and succeeds only if it is a letter or a digit. Using the .NET definition
    /// of Char.IsLetterOrDigit. Note that this is not compatible with Elm's Char.isAlphaNum.
    let letterOrDigit = Combinators.check Char.IsLetterOrDigit char

    /// Returns a parser which returns the given character or fails if the input does not start with this character.
    let literalChar c = Combinators.check ((=) c) char


/// Parsers for strings.
module String =
    /// Returns a parser which parses a specific literal. Fails if the input does not start with this literal.
    let literal (prefix: string) =
        Parser (fun s -> iif (s.StartsWith prefix) (Some ( prefix, s.[prefix.Length..] )) None)

    /// A parser that matches exactly when input is empty.
    let eof = Basic.look |> Combinators.check ((=) "") |> Combinators.map (fun _  -> ())

    /// Returns a parser that succeeds if the provided parser succeeds, and leaves a remaining string that is empty.
    let entire p = Combinators.left p eof

    /// Returns a parser that applies a parser one or more times, but separated by the specified parser every time.
    let separated separator p = Combinators.sequence List.cons p (Regex.star (Combinators.right separator p))

    /// The same as `separated`, however rather than a parser, it accepts a string as the first parameter which is the
    /// literal separator.
    let separated' separator p = separated (literal separator) p

    /// Returns a parser that finds the index of the specified needle in the input string. If it is found, the entire
    /// string, and the position of the first occurrence are returned. Otherwise, the parser fails.
    let indexOf (needle: string) =
        Basic.look |> Combinators.bind
            (fun s ->
                match s.IndexOf needle with
                | -1 -> Basic.fail ()
                | i -> Basic.succeed ( s, i )
            )

    /// Returns a parser that finds and returns the first occurrence of the given string in the input. Everything prior to
    /// the string and the string itself are consumed.
    let find s = indexOf s |> Combinators.bind (fun ( input, pos ) -> Parser (fun _ -> Some ( s, input.[pos + s.Length..] )))

    /// Returns a parser which succeeds when the given string occurs in the input. Everything prior to the first occurrence
    /// of the string and the string itself are consumed.
    let occurs s = Combinators.right (find s) (Basic.succeed ())

    /// Returns a parser which returns everything in the input after the first occurrence of the given string. Everything
    /// prior to the string and the string itself are consumed.
    let after s = Combinators.right (occurs s) Basic.look

    /// Returns a parser which returns everything in the input before the first occurrence of the given string. The string
    /// itself is not consumed.
    let before s = indexOf s |> Combinators.bind (fun ( input, pos ) -> Parser (fun _ -> Some ( input.[..pos - 1] , input.[pos..] )))

    /// Returns a parser that yields the longest prefix in which every character (as a string) satisfies the given
    /// predicate.
    let takeWhile condition =
        Combinators.map
            (String.concat "")
            (Regex.star (Combinators.map (List.singleton >> List.toString) (Combinators.check condition Char.char)))

    /// Given a parser that parses a single character, returns a parser that applies this parser one or more times,
    /// until it no longer succeeds, and returns the result as a string.
    let stringOf p = Regex.plus p |> Combinators.map List.toString

    /// Given a parser that parses a single character, returns a parser that applies this parser n times, and returns the
    /// result as a string. If the provided parser fails earlier, the resulting parser fails.
    let stringOfLength p n = Regex.times n p |> Combinators.map List.toString


/// Integer parsers.
module Int =
    /// Returns a parser that succeeds when the specified string is an integer, and returns this integer without
    /// consuming input. Otherwise, the parser fails.
    let parseInt str = Int.tryParse str |> Basic.fromOption

    /// Returns a parser that succeeds when the specified string is a long, and returns this long without
    /// consuming input. Otherwise, the parser fails.
    let parseLong str = Long.tryParse str |> Basic.fromOption

    /// A parser that returns a single digit as an integer.
    let digit = Char.num |> Combinators.map (List.singleton >> List.toString) |> Combinators.bind parseInt

    /// A parser that returns a sequence of digits as a positive integer.
    let positiveInt = String.stringOf Char.num |> Combinators.bind parseInt

    /// A parser that returns a sequence of digits as a positive long.
    let positiveLong = String.stringOf Char.num |> Combinators.bind parseLong

    /// A parser that returns a sequence of digits, optionally prefixed by a '-' or '+' as an integer.
    let int =
        Regex.optional
            (Combinators.alt
                (String.literal "-")
                (String.literal "+" |> Combinators.map (always "")))
        |> Combinators.map (Option.defaultValue "")
        |> Combinators.andThen (+) (String.stringOf Char.num)
        |> Combinators.bind parseInt

    /// A parser that returns a sequence of digits, optionally prefixed by a '-' or '+' as a long.
    let long =
        Regex.optional
            (Combinators.alt
                (String.literal "-")
                (String.literal "+" |> Combinators.map (always "")))
        |> Combinators.map (Option.defaultValue "")
        |> Combinators.andThen (+) (String.stringOf Char.num)
        |> Combinators.bind parseLong