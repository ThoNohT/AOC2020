module Day6

open System
open System.IO
open Common
open Problem

let input =
    File.ReadLines "Day6/input.txt"
    |> Seq.toList
    |> List.batchWhile (fun l -> l <> "") false


type Part1 () =
    static member Problem = new Part1 () :> IProblem

    interface IProblem with
        /// For each group, count the number of questions to which anyone answered "yes". What is the sum of those counts?
        member _.Solve () =
            input
            |> List.map (Seq.concat >> Set.ofSeq >> Set.count)
            |> List.sum
            |> Console.WriteLine


type Part2 () =
    static member Problem = new Part2 () :> IProblem

    interface IProblem with
        /// For each group, count the number of questions to which everyone answered "yes". What is the sum of those counts?
        member _.Solve () =
            input
            |> List.map (List.map Set.ofSeq >> Set.intersectMany >> Set.count)
            |> List.sum
            |> Console.WriteLine
