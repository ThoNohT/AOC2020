module Day11

open System.IO
open Problem


type SpotState = Floor | EmptySeat | OccupiedSeat | OutOfBounds
with
    static member FromChar =
        function
        | '.' -> Floor
        | 'L' -> EmptySeat
        | '#' -> OccupiedSeat
        | _ -> failwith "Invalid Spot type."


type Spot = { position : int * int ; state : SpotState }
with
    static member GetState (area: List<List<Spot>>) (x, y) =
        if x < 0 || x >= List.length area.[0] || y < 0 || y >= List.length area then OutOfBounds
        else area.[y].[x].state

    static member Update area this = async {
        if this.state = Floor then return this
        else
            let x, y = this.position
            let neighbors =
                [ x - 1, y - 1 ; x, y - 1 ;  x + 1,  y - 1
                  x - 1, y                ;  x + 1,  y
                  x - 1, y + 1 ; x, y + 1 ;  x + 1,  y + 1
                ]
            let occupiedCount = List.filter (Spot.GetState area >> ((=) OccupiedSeat)) neighbors |> List.length

            return
                match this.state with
                | EmptySeat when occupiedCount = 0 -> { this with state = OccupiedSeat }
                | OccupiedSeat when occupiedCount >= 4 -> { this with state = EmptySeat }
                | _ -> this
    }

    static member Update2 area this = async {
        if this.state = Floor then return this
        else
            let x, y = this.position
            let directions =
                [ -1, -1 ; 0, -1 ; 1, -1
                  -1,  0 ;         1,  0
                  -1,  1 ; 0,  1 ; 1,  1
                ]

            let rec findSeat area (x, y) (dx, dy) =
                match Spot.GetState area (x + dx, y + dy) with
                | Floor -> findSeat area (x + dx, y + dy) (dx, dy)
                | other -> other

            let occupiedCount = List.filter (findSeat area (x, y) >> ((=) OccupiedSeat)) directions |> List.length

            return
                match this.state with
                | EmptySeat when occupiedCount = 0 -> { this with state = OccupiedSeat }
                | OccupiedSeat when occupiedCount >= 5 -> { this with state = EmptySeat }
                | _ -> this
    }


let rec step update (area: List<List<Spot>>) =
    let updateRow row = async {
        let! newRow = Async.Parallel <| List.map (update area) row
        return List.ofSeq newRow
    }

    let updateArea area = async {
        let! newArea = Async.Parallel (List.map updateRow area)
        return List.ofSeq newArea
    }

    let newArea = Async.RunSynchronously (updateArea area)
    if List.forall2 (fun l1 l2 -> List.forall2 (fun s1 s2 -> s1.state = s2.state) l1 l2) area newArea then newArea
    else step update newArea


let input =
    File.ReadLines "Day11/input.txt"
    |> Seq.mapi (fun y str -> Seq.mapi (fun x ch -> { position = x, y ; state = SpotState.FromChar ch }) str |> Seq.toList)
    |> Seq.toList


type Day8 () =
    interface IProblem with
        member _.Number = "11"

        /// Simulate your seating area by applying the seating rules repeatedly until no seats change state. How many
        /// seats end up occupied?
        member _.Part1 () =
            input
            |> step Spot.Update
            |> List.map (List.filter (fun s -> s.state = OccupiedSeat) >> List.length)
            |> List.sum
            |> sprintf "%A"

        /// Given the new visibility method and the rule change for occupied seats becoming empty, once equilibrium is
        /// reached, how many seats end up occupied?
        member _.Part2 () =
            input
            |> step Spot.Update2
            |> List.map (List.filter (fun s -> s.state = OccupiedSeat) >> List.length)
            |> List.sum
            |> sprintf "%A"
