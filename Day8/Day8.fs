module Day8

open System
open System.IO
open Problem

module P = Parser
module PC = Parser.Combinators


type Instruction =
    | Acc of int
    | Jmp of int
    | Nop of int // This int does nothing.
with
    static member Parse str =
        let parseAcc = PC.right (P.String.literal "acc ") (P.Int.int) |> P.String.entire |> PC.map Acc
        let parseJmp = PC.right (P.String.literal "jmp ") (P.Int.int) |> P.String.entire |> PC.map Jmp
        let parseNop = PC.right (P.String.literal "nop ") (P.Int.int) |> P.String.entire |> PC.map Nop
        let parseInstruction = PC.oneOf [ parseAcc ; parseJmp ; parseNop ]

        P.parseOrFail parseInstruction (lazy Exception (sprintf "Failed to parse instruction: '%s'" str)) str


type Machine = { instructionPointer : int ; accumulator : int64 ; seenLocations : Set<int>; finished : bool }
with
    static member Init = { instructionPointer = 0 ; accumulator = 0L ; seenLocations = Set.empty; finished = false }
    static member Run machine (instructions: List<Instruction>) =
        if machine.seenLocations.Contains machine.instructionPointer then
            // Loop, just stop.
            machine
        elif List.length instructions <= machine.instructionPointer then
            // Pas the end, we're done.
            { machine with finished = true }
        else
            // Add pointer to seen locations.
            let newSeenLocations = Set.add machine.instructionPointer machine.seenLocations

            // Update accumulator and move pointer.
            let newAcc, newPointer =
                match instructions.[machine.instructionPointer] with
                | Acc value ->
                    (machine.accumulator + int64 value, machine.instructionPointer + 1)
                | Jmp value ->
                    (machine.accumulator, machine.instructionPointer + value)
                | Nop _ ->
                    (machine.accumulator, machine.instructionPointer + 1)

            Machine.Run
                { machine with
                    instructionPointer = newPointer
                    accumulator = newAcc
                    seenLocations = newSeenLocations }
                instructions


let input =
    File.ReadLines "Day8/input.txt"
    |> Seq.map Instruction.Parse
    |> Seq.toList


type Day8 () =
    interface IProblem with
        member _.Number = "8"

        /// Immediately before any instruction is executed a second time, what value is in the accumulator?
        member _.Part1 () =
            input
            |> Machine.Run Machine.Init
            |> sprintf "%A"

        /// What is the value of the accumulator after the program terminates?
        member _.Part2 () =
            let possibleChanges =
                input
                |> List.mapi (fun i e -> match e with | Jmp _ -> Some i | Nop _ -> Some i | _ -> None)
                |> List.choose id

            let flipInstruction index instructions =
                List.mapi (fun i e ->
                    match e with
                    | Jmp n when i = index -> Nop n
                    | Nop n when i = index -> Jmp n
                    | _ -> e) instructions

            possibleChanges
            |> List.map (fun i -> input |> flipInstruction i |> Machine.Run Machine.Init )
            |> List.filter (fun r -> r.finished)
            |> List.head
            |> sprintf "%A"
