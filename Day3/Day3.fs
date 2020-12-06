module Day3

open System
open System.IO
open Problem


let input =
    File.ReadLines "Day3/input.txt"
    |> Seq.map (Seq.map (fun c -> c = '#') >> Seq.toList)
    |> Seq.toList


let rec traverse (x, y) (stepX, stepY) (forest: List<List<bool>>) =
    let forestHeight = List.length forest
    let forestWidth = List.length forest.[0] // Note that the forest wraps around after the width.

    if y >= forestHeight then 0
    else Bool.ord forest.[y].[x] + traverse ((x + stepX) % forestWidth, y + stepY) (stepX, stepY) forest


type Day3 () =
    interface IProblem with
        member _.Number = "3"

        /// Starting at the top-left corner of your map and following a slope of right 3 and down 1, how many trees
        /// would you encounter?
        member _.Part1 () = (traverse (0, 0) (3, 1) input) |> sprintf "%i"

        /// What do you get if you multiply together the number of trees encountered on each of the listed slopes?
        member _.Part2 () =
            [ 1, 1 ; 3 , 1 ; 5 , 1 ; 7 , 1 ; 1 , 2 ]
            |> List.map (fun slope -> int64 <| traverse (0, 0) slope input)
            |> List.fold (*) 1L
            |> sprintf "%i"
