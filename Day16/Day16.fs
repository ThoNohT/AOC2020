module Day16

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


type Ticket = Ticket of List<int>
with
    static member Parser =
        PS.separated' "," PI.positiveInt |> PC.skip PS.newline |> PC.map Ticket

    member this.List =
        let (Ticket list) = this
        list

type Range = Range of int * int
with
    static member Make low high = Range (low, high)

    static member Parser =
        PB.succeed Range.Make
        |> PC.andMap (PI.positiveInt |> PC.skip (PCh.literalChar '-' ))
        |> PC.andMap PI.positiveInt

    member this.ValidateValue value =
        let (Range (low, high)) = this
        low <= value && high >= value


type Field = {
    name: string
    firstRange: Range
    secondRange : Range
} with
    static member Make name firstRange secondRange =
        { name = name ; firstRange = firstRange ; secondRange = secondRange }

    static member Parser =
        PB.succeed Field.Make
        |> PC.andMap (PS.takeWhile ((<>) ':') |> PC.skip (PS.literal ": "))
        |> PC.andMap (Range.Parser |> PC.skip (PS.literal " or "))
        |> PC.andMap (Range.Parser |> PC.skip PS.newline)

    member this.ValidateValue value = this.firstRange.ValidateValue value || this.secondRange.ValidateValue value


type Document = {
    fields: List<Field>
    myTicket: Ticket
    nearbyTickets : List<Ticket>
} with
    static member Make fields myTicket nearbyTickets =
        { fields = fields ; myTicket = myTicket ; nearbyTickets = nearbyTickets }

    static member Parser =
        PB.succeed Document.Make
        |> PC.andMap (PR.plus Field.Parser |> PC.skip PS.whitespace)
        |> PC.andMap (PS.literal "your ticket:" |> (PC.discard PS.newline) |> (PC.discard Ticket.Parser) |> PC.skip PS.whitespace)
        |> PC.andMap (PS.literal "nearby tickets:" |> (PC.discard PS.newline) |> (PC.discard (PR.plus Ticket.Parser)) |> PC.skip PS.whitespace)
        |> PS.entire


let input =
    File.ReadAllText "Day16/input.txt"
    |> P.parseOrFail Document.Parser (lazy Exception "Parsing input failed.")


type Day16 () =
    let allRanges = List.collect (fun r -> [ r.firstRange ; r.secondRange ]) input.fields

    let invalidValues =
            input.nearbyTickets
            |> List.collect (fun ticket -> ticket.List)
            |> List.filter (fun v -> not <| List.exists (fun (r: Range) -> r.ValidateValue v) allRanges)
            |> Set.ofList

    interface IProblem with
        member _.Number = "16"

        /// What is your ticket scanning error rate?
        member _.Part1 () =
            invalidValues
            |> Set.fold (+) 0
            |> sprintf "%i"

        /// Once you work out which field is which, look for the six fields on your ticket that start with the word
        /// departure. What do you get if you multiply those six values together?
        member _.Part2 () =
            let validTickets =
                input.nearbyTickets
                |> List.filter (fun ticket -> Set.ofList ticket.List |> Set.intersect invalidValues |> Set.isEmpty)

            let validTicketValuesAtIndex index = List.map (fun (t: Ticket) -> t.List.[index]) validTickets

            let validFieldsPerIndex =
                seq { 0 .. List.length input.myTicket.List - 1}
                |> Seq.map validTicketValuesAtIndex
                |> Seq.mapi (fun i values -> List.filter (fun (f: Field) -> List.forall f.ValidateValue values) input.fields |> Set.ofList, i )
                |> Seq.sortBy (fst >> Set.count)

            let removeAlreadyTakenFields (acc, taken) (current, i) =
                let remaining = Set.difference current taken |> Set.toList |> List.head
                (remaining, i) :: acc, Set.add remaining taken

            validFieldsPerIndex
            |> Seq.fold removeAlreadyTakenFields ([], Set.empty)
            |> fst
            |> List.filter (fun (field, _) -> field.name.StartsWith "departure")
            |> List.map snd
            |> List.map (fun idx -> int64 input.myTicket.List.[idx])
            |> List.fold (*) 1L
            |> sprintf "%i"
