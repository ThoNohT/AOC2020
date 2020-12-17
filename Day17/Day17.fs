module Day17

open System.IO
open Problem


/// Cube is defined in terms of its position. Inactive cube is represented by absence of a Cube.
type Cube = Cube of List<int>
with
    static member Make dims positions =
        let inputDims = List.length positions
        if dims < inputDims then failwith "Too many dimensions provided."
        Cube <| positions @ (List.replicate (dims - inputDims) 0)

    member this.Coords =
        let (Cube c) = this
        c

    member this.Neighbors =
        let buildNeighbors acc x =
            match acc with
            | [] -> [ [ x - 1 ] ; [ x ] ; [ x + 1 ] ]
            | _ -> List.collect (fun r -> [ x - 1 :: r ; x :: r ; x + 1 :: r ] ) acc

        List.fold buildNeighbors [] this.Coords
        |> List.map (List.rev >> Cube)
        |> Set.ofList
        |> Set.remove this

    /// Call this on active cubes.
    member this.RemainActive dimension =
        let activeNeighbors =
            this.Neighbors
            |> Set.filter (flip Set.contains dimension)
            |> Set.count
        activeNeighbors = 2 || activeNeighbors = 3

    /// Call this on inactive cubes.
    member this.BecomeActive dimension =
        let activeNeighbors =
            this.Neighbors
            |> Set.filter (flip Set.contains dimension)
            |> Set.count
        activeNeighbors = 3


/// Result is a new set, where cubes that became inactive are removed, and cubes that became active are added.
let step dimension =
    let becomeInactive = Set.filter (fun (c: Cube) -> not <| c.RemainActive dimension) dimension
    let becomeActive =
        dimension
        |> Set.map (fun (c: Cube) -> c.Neighbors) |> Set.unionMany
        |> flip Set.difference dimension
        |> Set.filter (fun c -> c.BecomeActive dimension)

    Set.difference dimension becomeInactive |> Set.union becomeActive


let rec simulate steps dimension =
    if steps < 0 then failwith "Invalid number of steps."
    if steps = 0 then dimension
    else simulate (steps - 1) (step dimension)


let input =
    File.ReadAllLines "Day17/input.txt"
    |> Seq.mapi (fun y r -> r |> Seq.mapi (fun x e -> [ x ; y ], e = '#') |> Seq.filter snd |> Seq.map fst)
    |> Seq.concat
    |> Set.ofSeq


type Day17 () =
    interface IProblem with
        member _.Number = "17"

        /// How many cubes are left in the active state after the sixth cycle?
        member _.Part1 () =
            input
            |> Set.map (Cube.Make 3)
            |> simulate 6
            |> Set.count
            |> sprintf "%i"

        /// Starting with your given initial configuration, simulate six cycles in a 4-dimensional space. How many
        /// cubes are left in the active state after the sixth cycle?
        member _.Part2 () =
            input
            |> Set.map (Cube.Make 4)
            |> simulate 6
            |> Set.count
            |> sprintf "%i"
