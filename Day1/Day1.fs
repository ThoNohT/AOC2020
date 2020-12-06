module Day1

open System
open System.IO
open Problem


let input = File.ReadLines "Day1/input.txt" |> Seq.map Int32.Parse |> List.ofSeq


type Day1 () =
    interface IProblem with
        member _.Number = "1"

        /// Find the two entries that sum to 2020; what do you get if you multiply them together?
        member _.Part1 () =
            input
            |> Common.List.findPairSym (fun a b -> a + b = 2020)
            |> Option.map (fun (a, b) -> a * b)
            |> Option.get
            |> sprintf "%i"

        /// In your expense report, what is the product of the three entries that sum to 2020?
        member _.Part2 () =
            input
            |> Common.List.findThreeSym (fun a b c -> a + b + c = 2020)
            |> Option.map (fun (a, b, c) -> a * b * c)
            |> Option.get
            |> sprintf "%i"


(* ---------- Alternate  solution ---------- *)


type Day1Alternate () =
    interface IProblem with
        member _.Number = "1b"

        /// Find the two entries that sum to 2020; what do you get if you multiply them together?
        member _.Part1 () =
            input
            |> Common.List.findNWithSum 2 2020
            |> Option.map (fun lst -> List.fold (*) 1 lst)
            |> Option.get
            |> sprintf "%i"

        /// In your expense report, what is the product of the three entries that sum to 2020?
        member _.Part2 () =
            input
            |> Common.List.findNWithSum 3 2020
            |> Option.map (fun lst -> List.fold (*) 1 lst)
            |> Option.get
            |> sprintf "%i"
