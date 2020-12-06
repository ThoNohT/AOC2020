module Day5

open System
open System.IO
open Common
open Problem


let ord ch =
    match ch with
    | 'L' | 'F' -> 0
    | 'R' | 'B' -> 1
    | _ -> failwith "Invalid position indicator."


type BoardingPass = BoardingPass of int * int
with
    static member Make (str: string) =
        let row =
            str.Substring (0, 7)
            |> Seq.mapi (fun i e -> pown 2 (6 - i) * (ord e))
            |> Seq.sum
        let column =
            str.Substring (7, 3)
            |> Seq.mapi (fun i e -> pown 2 (2 - i) * (ord e))
            |> Seq.sum

        BoardingPass (row, column)

    member this.SeatId =
        let (BoardingPass (row, column)) = this
        (int64 row * 8L) + int64 column


let input =
    File.ReadLines "Day5/input.txt"
    |> Seq.map BoardingPass.Make
    |> Seq.toList


type Day5 () =
    interface IProblem with
        member _.Number = "5"

        /// What is the highest seat ID on a boarding pass?
        member _.Part1 () =
            input
            |> List.map (fun pass -> pass.SeatId)
            |> List.max
            |> sprintf "%i"


        /// What is the ID of your seat?
        member _.Part2 () =
            let sorted = List.sortBy (fun (p: BoardingPass) -> p.SeatId) input

            sorted
            |> List.take (List.length sorted - 1)
            |> List.zip (List.tail sorted)
            |> List.map (fun (a, b) -> a.SeatId, b.SeatId)
            |> List.filter (fun (a, b) -> a > b + 1L)
            |> List.map (fun (a, _) -> a - 1L)
            |> List.head
            |> sprintf "%i"
