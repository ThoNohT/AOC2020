module Day10

open System
open System.IO
open Problem


let input =
    File.ReadLines "Day10/input.txt"
    |> Seq.map Int64.Parse
    |> Seq.sort
    |> Seq.toList


type Day8 () =
    interface IProblem with
        member _.Number = "10"

        /// What is the number of 1-jolt differences multiplied by the number of 3-jolt differences?
        member _.Part1 () =
            let addDifference (diff1Count, diff3Count, lastElem) nextElem =
                if nextElem = lastElem + 1L then
                    diff1Count + 1L, diff3Count, nextElem
                elif nextElem = lastElem + 3L then
                    diff1Count, diff3Count + 1L, nextElem
                else
                    diff1Count, diff3Count, nextElem

            input
            // Start from 0 in outlet.
            |> List.fold addDifference (0L, 0L, 0L)
            // Consider phone a 3 diff.
            |> (fun (diff1Count, diff3Count, _) -> diff1Count * (diff3Count + 1L))
            |> sprintf "%i"

        /// What is the total number of distinct ways you can arrange the adapters to connect the charging outlet to your device?
        member _.Part2 () =
            let countWays (knownWays: List<int64 * int64>) adapter =
                let fromStart = adapter < 4L |> Bool.ord |> int64
                let fromOtherAdapters =
                    knownWays
                    |> List.filter (fun knownWay -> fst knownWay < adapter && fst knownWay > adapter - 4L)
                    |> List.map snd
                    |> List.sum

                (adapter, fromStart + fromOtherAdapters) :: knownWays

            // Observation: To the phone adapter can only be done from the last adapter, so doesn't add any way.
            input
            |> List.fold countWays List.empty
            |> List.head
            |> snd
            |> sprintf "%i"
