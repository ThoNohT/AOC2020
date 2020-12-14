module Day9

open System
open System.IO
open Problem


let input =
    File.ReadLines "Day9/input.txt"
    |> Seq.map Int64.Parse
    |> Seq.toList


let hasSumOf sum (elems: List<int64>) =
    List.exists (fun e1 -> List.exists (fun e2 -> e1 + e2 = sum && e1 <> e2) elems) elems


let rec trySum sum acc elems =
    match elems with
    // End of list, nothing found.
    | [] -> None
    // Next element would make sum too big.
    | x :: _ when x >= sum || (x + List.sum acc) > sum -> None
    // Next element makes the currect sum, return.
    | x :: _ when (x + List.sum acc) = sum -> Some <| x :: acc
    // Next element still fits.
    | x :: xs ->
        match acc, trySum sum (x :: acc) xs with
        // Result found deeper on, return it.
        | _, Some result -> Some result
        // No result found, but acc is empty, so we can just move on to the next starting position.
        | [], None -> trySum sum acc xs
        // No result found, acc is not empty, so return the result further up.
        | _, _ -> None


type Day9 () =
    interface IProblem with
        member _.Number = "9"

        /// What is the first number that does not have this property?
        member _.Part1 () =
            input
            |> List.mapi (fun i n -> i > 24 && not (hasSumOf n input.[i-25..i-1]), n)
            |> List.find fst
            |> snd
            |> sprintf "%i"

        /// Find a contiguous set of at least two numbers in your list which sum to the invalid number from step 1.
        member _.Part2 () =
            let sequence = input |> trySum 556543474L [] |> Option.get |> List.sort
            let low, high = (List.head sequence, List.last sequence)
            sprintf "%i" (low + high)
