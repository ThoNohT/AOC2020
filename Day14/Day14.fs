module Day14

open System
open System.IO
open Problem
module P = Parser
module PI = Parser.Int
module PS = Parser.String
module PC = Parser.Combinators
module PCh = Parser.Char


module Bitmask =
    let private longToString (value: int64) =
        let converted = Convert.ToString (value, 2)
        converted.PadLeft (36, '0')

    let private stringToLong (value: string) = Convert.ToInt64 (value, 2)

    let private replaceMaskCharacters mask valueStr =
        Seq.map2 (fun maskCh valueCh -> if maskCh = 'X' then valueCh else maskCh) mask valueStr
        |> String.fromCharSeq

    let private setMaskCharacters mask valueStr =
        Seq.map2
            (fun maskCh valueCh ->
                match maskCh with
                | '0' -> valueCh
                | '1' -> '1'
                | 'X' -> 'X'
                | _ -> failwith "invalid char") mask valueStr
        |> String.fromCharSeq

    let private maskedPermutations (valueStr: string) =
        let rec iterate = function
            | [ ch ] ->
                match ch with
                | 'X' -> [ [ '1' ] ; [ '0' ] ]
                | _ -> [ [ ch ] ]
            | ch :: rest ->
                let permutedRest = iterate rest
                match ch with
                | 'X' ->
                    List.append
                        (List.map (fun r -> '0' :: r) permutedRest)
                        (List.map (fun r -> '1' :: r) permutedRest)
                | _ -> List.map (fun r -> ch :: r) permutedRest
            | [] -> []

        valueStr |> List.ofSeq |> iterate |> List.map String.fromCharSeq

    let applyMask mask value =
        value
        |> longToString
        |> replaceMaskCharacters mask
        |> stringToLong

    let maskAddress mask address =
        address
        |> longToString
        |> setMaskCharacters mask
        |> maskedPermutations
        |> List.map stringToLong


type Instruction =
    | SetMask of string
    | Write of int64 * int64
with
    static member Parser =
        let maskCharParser = PC.alt (PCh.literalChar 'X') (PCh.num)
        let maskParser =
            PS.literal "mask = "
            |> PC.discard (lazy PS.stringOfLength (maskCharParser) 36)
            |> PS.entire
            |> PC.map SetMask

        let writeParser =
            PS.literal "mem["
            |> PC.discard (lazy  PI.positiveLong)
            |> PC.skip (lazy PS.literal "] = ")
            |> PC.andThen (fun addr value -> Write (addr, value)) (lazy PI.positiveLong)
            |> PS.entire

        PC.alt maskParser writeParser


type Computer = { memory : Map<int64, int64> ; mask: string }
with
    static member Init = { memory = Map.empty ; mask = Seq.replicate 36 'X' |> string}

    static member ExecuteInstruction comp = function
        | SetMask newMask -> { comp with mask = newMask }
        | Write (address, value) ->
            let maskedValue = Bitmask.applyMask comp.mask value
            { comp with memory = Map.add address maskedValue comp.memory}

    static member ExecuteInstruction2 comp = function
        | SetMask newMask -> { comp with mask = newMask }
        | Write (address, value) ->
            let maskedAddresses = Bitmask.maskAddress comp.mask address
            { comp with
                memory = List.fold (fun mem addr -> Map.add addr value mem) comp.memory maskedAddresses
            }


let input =
    File.ReadLines "Day14/input.txt"
    |> Seq.map (fun x -> P.parseOrFail Instruction.Parser (lazy Exception (sprintf "Invalid instruction: %s" x)) x)
    |> Seq.toList


type Day14 () =
    interface IProblem with
        member _.Number = "14"

        /// What is the sum of all values left in memory after it completes?
        member _.Part1 () =
            input
            |> List.fold Computer.ExecuteInstruction Computer.Init
            |> (fun c -> c.memory |> Map.toList |> List.map snd |> List.sum)
            |> sprintf "%i"

        /// Execute the initialization program using an emulator for a version 2 decoder chip. What is the sum of all
        /// values left in memory after it completes?
        member _.Part2 () =
            input
            |> List.fold Computer.ExecuteInstruction2 Computer.Init
            |> (fun c -> c.memory |> Map.toList |> List.map snd |> List.sum)
            |> sprintf "%i"
