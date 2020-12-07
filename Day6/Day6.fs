module Day6

open System.IO
open Common
open Problem


let input =
    File.ReadLines "Day6/input.txt"
    |> Seq.toList
    |> List.batchWhile (fun l -> l <> "") false


type Day6 () =
    interface IProblem with
        member _.Number = "6"

        /// For each group, count the number of questions to which anyone answered "yes". What is the sum of those counts?
        member _.Part1 () =
            input
            |> List.map (Seq.concat >> Set.ofSeq >> Set.count)
            |> List.sum
            |> sprintf "%i"

        /// For each group, count the number of questions to which everyone answered "yes". What is the sum of those counts?
        member _.Part2 () =
            input
            |> List.map (List.map Set.ofSeq >> Set.intersectMany >> Set.count)
            |> List.sum
            |> sprintf "%i"
