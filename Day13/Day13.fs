module Day13

open System
open System.IO
open Problem


let input =
    File.ReadLines "Day13/input.txt"
    |> Seq.toList


let arrival = Int64.Parse input.[0]


let busNumbers =
    input.[1].Split ','
    |> Seq.map Long.tryParse
    |> List.ofSeq


type Day13 () =
    interface IProblem with
        member _.Number = "13"

        /// What is the ID of the earliest bus you can take to the airport multiplied by the number of minutes you'll
        /// need to wait for that bus?
        member _.Part1 () =
            busNumbers
            |> List.choose id
            |> List.map (fun n -> ((arrival / n) + 1L) * n - arrival , n)
            |> List.minBy fst
            |> fun (waitTime, lineNumber) -> waitTime * lineNumber
            |> sprintf "%i"

        /// What is the earliest timestamp such that all of the listed bus IDs depart at offsets matching their
        /// positions in the list?
        member _.Part2 () =
            let busOccurrences =
                busNumbers
                |> List.choosei (fun i n -> Option.map (fun busNumber -> (int64 i, int64 busNumber)) n)

            let rec findNextOccurrence (time, step) (offset, busNumber) =
                if (time + offset) % busNumber = 0L then
                    time, step * busNumber
                else
                    findNextOccurrence (time + step, step) (offset, busNumber)

            List.fold findNextOccurrence (0L, 1L) busOccurrences
            |> fst
            |> sprintf "%i"
