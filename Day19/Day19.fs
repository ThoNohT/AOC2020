module Day19

open System
open System.IO
open Problem

module P = Parser
module PB = Parser.Basic
module PC = Parser.Combinators
module PCh = Parser.Char
module PI = Parser.Int
module PR = Parser.Regex
module PS = Parser.String


type Rule =
    | Literal of char
    | Combination of List<int>
    | Alternative of Rule * Rule
with
    static member private LiteralParser =
        PCh.literalChar '"'
        |> PC.discard (lazy PCh.alpha)
        |> PC.skip (lazy PCh.literalChar '"')
        |> PC.map Literal

    static member private CombinationParser =
        PR.plus (PI.positiveInt |> PC.skip (lazy PS.whitespace))
        |> PC.map Combination

    static member private AlternativeParser =
        PB.succeed (fun a b -> a, b)
        |> PC.andMap (lazy (Rule.CombinationParser |> PC.skip (lazy PS.literal "| ")))
        |> PC.andMap (lazy Rule.CombinationParser)
        |> PC.map Alternative

    static member Parser =
        let possibleParsers = [ Rule.LiteralParser ; Rule.CombinationParser ; Rule.AlternativeParser ]
        PB.succeed (fun a b -> a, b)
        |> PC.andMap (lazy (PI.positiveInt |> PC.skip (lazy PS.literal ": ")))
        |> PC.andMap (lazy (possibleParsers |> List.map PS.entire |> PC.oneOf))

    static member ToParser allRules = function
        | Literal ch -> PCh.literalChar ch |> PC.map (Seq.singleton >> String.fromCharSeq)
        | Combination ids ->
            ids
            |> List.map (flip Map.find allRules)
            |> List.fold (fun parser rule ->
                PC.andThen (+) (lazy (Rule.ToParser allRules rule )) parser) (PB.succeed "")
        | Alternative (r1, r2) -> PC.alt (Rule.ToParser allRules r1) (Rule.ToParser allRules r2)


let ruleToFullLineParser rule allRules =
    Rule.ToParser allRules rule |> PS.entire


let parseRule line = P.parseOrFail Rule.Parser (lazy Exception "Failed to parse rule") line


let parseRules lines =
    lines
    |> Seq.map parseRule
    |> Map.ofSeq


let (rules, lines) =
    File.ReadAllText "Day19/input.txt"
    |> fun s -> s.Split "\r\n\r\n"
    |> fun s -> parseRules (s.[0].Split "\r\n") , List.ofSeq (s.[1].Split "\r\n")


type Day19 () =
    interface IProblem with
        member _.Number = "19"

        /// How many messages completely match rule 0?
        member _.Part1 () =
            let rule0 = ruleToFullLineParser rules.[0] rules

            lines
            |> List.filter (P.parse rule0 >> Option.isSome)
            |> List.length
            |> sprintf "%i"

        /// After updating rules 8 and 11, how many messages completely match rule 0?
        member _.Part2 () =
            // Dua Lipa.
            let newRules =
                rules
                    |> (uncurry Map.add) (parseRule "8: 42 | 42 8")
                    |> (uncurry Map.add) (parseRule "11: 42 31 | 42 11 31")

            let rule0 = ruleToFullLineParser newRules.[0] newRules

            lines
            |> List.choose (P.parse rule0)
            |> sprintf "%A"
