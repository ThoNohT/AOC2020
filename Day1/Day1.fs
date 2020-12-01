module Day1

open System
open System.IO
open Problem


let input = File.ReadLines "Day1/input.txt" |> Seq.map Int32.Parse |> List.ofSeq


type Part1 () =
    static member Problem = new Part1 () :> IProblem

    interface IProblem with
        /// Find the two entries that sum to 2020; what do you get if you multiply them together?
        member _.Solve () =
            input
            |> Common.List.findPairSym (fun a b -> a + b = 2020)
            |> Option.map (fun (a, b) -> Console.WriteLine (sprintf "Found: %i, %i. Their product is %i." a b (a *b)))
            |> Option.defaultWith (fun _ -> Console.WriteLine "No result found.")


type Part2 () =
    static member Problem = new Part2 () :> IProblem

    interface IProblem with
        /// In your expense report, what is the product of the three entries that sum to 2020?
        member _.Solve () =
            input
            |> Common.List.findThreeSym (fun a b c -> a + b + c = 2020)
            |> Option.map (fun (a, b, c) -> Console.WriteLine (sprintf "Found: %i, %i, %i. Their product is %i." a b c (a * b * c)))
            |> Option.defaultWith (fun _ -> Console.WriteLine "No result found.")
