module Day3

open System
open System.IO
open Problem


let input =
    File.ReadLines "Day3/input.txt"
    |> Seq.map (Seq.map (fun c -> c = '#') >> Seq.toList)
    |> Seq.toList


let forestHeight = List.length input
let forestWidth = List.length input.[0] // Note that the forest wraps around after the width.


let rec traverse (x,y) (stepX, stepY) (input: List<List<bool>>) =
    if y >= forestHeight then 0
    else (if input.[y].[x] then 1 else 0) + traverse ((x + stepX) % forestWidth, y + stepY) (stepX, stepY) input


type Part1 () =
    static member Problem = new Part1 () :> IProblem

    interface IProblem with
        /// Starting at the top-left corner of your map and following a slope of right 3 and down 1, how many trees
        /// would you encounter?
        member _.Solve () = Console.WriteLine (traverse (0, 0) (3, 1) input)


type Part2 () =
    static member Problem = new Part2 () :> IProblem

    interface IProblem with
        /// What do you get if you multiply together the number of trees encountered on each of the listed slopes?
        member _.Solve () =
            [ 1, 1 ; 3 , 1 ; 5 , 1 ; 7 , 1 ; 1 , 2 ]
            |> List.map (fun slope -> int64 <| traverse (0, 0) slope input)
            |> List.fold (*) 1L
            |> Console.WriteLine
