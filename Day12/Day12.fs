module Day12

open System
open System.IO
open Problem

type Heading = North | East | South | West
with
    static member Order = [ North ; West ; South ; East ]

    member this.MoveLeft amount =
        let newIndex =
            Heading.Order
            |> List.findIndex ((=) this)
            |> (fun idx -> (idx + (amount / 90)) % 4)

        Heading.Order.[newIndex]

    member this.MoveRight amount =
        let newIndex =
            Heading.Order
            |> List.rev
            |> List.findIndex ((=) this)
            |> (fun idx -> (idx + (amount / 90)) % 4)

        (List.rev Heading.Order).[newIndex]


type Ship = { position: int * int ; heading : Heading }
with
    // Positive position = North, East, negative = South, West
    static member Init = { position = 0, 0 ; heading = East }

    static member PerformInstruction this (instruction: string) =
        let x, y = this.position
        match instruction.[0], Int32.Parse (instruction.Substring 1) with
        | 'N', amount -> { this with position = x, y + amount }
        | 'S', amount -> { this with position = x, y - amount }
        | 'E', amount -> { this with position = x + amount, y }
        | 'W', amount -> { this with position = x - amount, y }
        | 'L', amount -> { this with heading = this.heading.MoveLeft amount }
        | 'R', amount -> { this with heading = this.heading.MoveRight amount }
        | 'F', amount when this.heading = North -> { this with position = x, y + amount }
        | 'F', amount when this.heading = South -> { this with position = x, y - amount }
        | 'F', amount when this.heading = East -> { this with position = x + amount, y }
        | 'F', amount when this.heading = West -> { this with position = x - amount, y }
        | _ -> failwith "Invalid instruction."


type Ship2 = { position : int * int ; waypoint : int * int }
with
    member this.MoveWaypointLeft amount =
        let wx, wy = this.waypoint
        match amount % 360 with
        | 0 -> this.waypoint
        | 90 -> -wy , wx
        | 180 -> -wx , -wy
        | 270 -> wy, -wx
        | _ -> failwith "Invalid amount"

    static member Init = { position = 0 , 0 ; waypoint = 10 , 1 }

    static member PerformInstruction this (instruction: string) =
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


type Day8 () =
    interface IProblem with
        member _.Number = "12"

        /// What is the Manhattan distance between that location and the ship's starting position?
        member _.Part1 () =
            input
            |> List.fold Ship.PerformInstruction Ship.Init
            |> (fun ship -> (ship.position |> fst |> Math.Abs) + (ship.position |> snd |> Math.Abs))
            |> sprintf "%A"

        /// Figure out where the navigation instructions actually lead. What is the Manhattan distance between that
        /// location and the ship's starting position?
        member _.Part2 () =
            input
            |> List.fold Ship2.PerformInstruction Ship2.Init
            |> (fun ship -> (ship.position |> fst |> Math.Abs) + (ship.position |> snd |> Math.Abs))
            |> sprintf "%A"
