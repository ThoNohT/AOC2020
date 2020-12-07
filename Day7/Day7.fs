module Day7

open System
open System.Text.RegularExpressions
open System.IO
open Common
open Problem


type ContainingBag = ContainingBag of int * string
let (|ContainingBag|) (str: string) =
    let result = Regex.Match (str.Trim ' ', "\A(\d+) (\w+ \w+) bags?\z")
    if result.Success then ContainingBag (Int32.Parse result.Groups.[1].Value, result.Groups.[2].Value)
    else failwith "Unable to parse bag definition"


type BagDefinition = BagDefinition of string * Map<string, int>
with
    static member Make (str: string) =
        let split = str.Split " bags contain "
        let containingBags = split.[1].TrimEnd('.').Split(',') |> List.ofSeq
        match containingBags with
        | [ "no other bags" ] -> BagDefinition (split.[0], Map.empty)
        | _ ->
            let quantities =
                 containingBags
                 |> List.map (fun (ContainingBag (qty, cb)) -> cb, qty)
                 |> Map.ofList
            BagDefinition (split.[0], quantities)

    member this.CanContain color allDefs =
        let (BagDefinition (_, thisContains)) = this
        // This bag can contain the color.
        if Map.containsKey color thisContains then true
        else
            // A bag contained in this bag can contain the color.
            let containedColors = thisContains |> Map.toSeq |> Seq.map fst |> Set.ofSeq
            allDefs
            |> List.filter (fun (BagDefinition (name, _)) -> Set.contains name containedColors)
            |> List.filter (fun bd -> bd.CanContain color allDefs)
            |> List.isEmpty
            |> not

    member this.NumberOfContainedBags allDefs : int =
        let (BagDefinition (_, thisContains)) = this
        let containedDefs =
            thisContains
            |> Map.toList
            |> List.map (fun (n, count) -> (List.find (fun (BagDefinition (n', _)) -> n' = n) allDefs, count))

        let directlyContained = thisContains |> Map.toSeq |> Seq.map snd |> Seq.sum
        let indirectlyContained = List.sumBy (fun ((bd: BagDefinition), count) -> bd.NumberOfContainedBags allDefs * count) containedDefs
        directlyContained + indirectlyContained


let input =
    File.ReadLines "Day7/input.txt"
    |> Seq.map BagDefinition.Make
    |> Seq.toList


type Day7 () =
    interface IProblem with
        member _.Number = "7"

        /// How many bag colors can eventually contain at least one shiny gold bag?
        member _.Part1 () =
            input
            |> List.filter (fun bd -> bd.CanContain "shiny gold" input)
            |> List.length
            |> sprintf "%i"

        /// How many individual bags are required inside your single shiny gold bag?
        member _.Part2 () =
            let bd = List.find (fun (BagDefinition (n, _)) -> n = "shiny gold") input
            sprintf "%i" <| bd.NumberOfContainedBags input
