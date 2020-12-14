module Day12

open System
open System.IO
open Problem

type Ship = { position: int * int ; waypoint :  int * int }
with
    member this.MoveWaypointLeft amount =
        let wx, wy = this.waypoint
        match amount % 360 with
        | 0 -> this.waypoint
        | 90 -> -wy , wx
        | 180 -> -wx , -wy
        | 270 -> wy, -wx
        | _ -> failwith "Invalid amount"

    // Positive position = North, East, negative = South, West
    static member Init1 = { position = 0, 0 ; waypoint = 1 , 0 }
    static member Init2 = { position = 0 , 0 ; waypoint = 10 , 1 }

    static member PerformInstruction1 this (instruction: string) =
        let x, y = this.position
        let wx, wy = this.waypoint
        match instruction.[0], Int32.Parse (instruction.Substring 1) with
        | 'N', amount -> { this with position = x, y + amount }
        | 'S', amount -> { this with position = x, y - amount }
        | 'E', amount -> { this with position = x + amount, y }
        | 'W', amount -> { this with position = x - amount, y }
        | 'L', amount -> { this with waypoint = this.MoveWaypointLeft amount }
        | 'R', amount -> { this with waypoint = this.MoveWaypointLeft (360 - amount) }
        | 'F', amount -> { this with position = x + wx * amount , y + wy * amount}
        | _ -> failwith "Invalid instruction."

    static member PerformInstruction2 this (instruction: string) =
        let wx, wy = this.waypoint
        let px, py = this.position
        match instruction.[0], Int32.Parse (instruction.Substring 1) with
        | 'N', amount -> { this with waypoint = wx, wy + amount }
        | 'S', amount -> { this with waypoint = wx, wy - amount }
        | 'E', amount -> { this with waypoint = wx + amount, wy }
        | 'W', amount -> { this with waypoint = wx - amount, wy }
        | 'L', amount -> { this with waypoint = this.MoveWaypointLeft amount }
        | 'R', amount -> { this with waypoint = this.MoveWaypointLeft (360 - amount) }
        | 'F', amount -> { this with position = px + wx * amount , py + wy * amount }
        | _ -> failwith "Invalid instruction."


let input =
    File.ReadLines "Day12/input.txt"
    |> Seq.toList


type Day12 () =
    interface IProblem with
        member _.Number = "12"

        /// What is the Manhattan distance between that location and the ship's starting position?
        member _.Part1 () =
            input
            |> List.fold Ship.PerformInstruction1 Ship.Init1
            |> (fun ship -> (ship.position |> fst |> Math.Abs) + (ship.position |> snd |> Math.Abs))
            |> sprintf "%i"

        /// Figure out where the navigation instructions actually lead. What is the Manhattan distance between that
        /// location and the ship's starting position?
        member _.Part2 () =
            input
            |> List.fold Ship.PerformInstruction2 Ship.Init2
            |> (fun ship -> (ship.position |> fst |> Math.Abs) + (ship.position |> snd |> Math.Abs))
            |> sprintf "%i"
