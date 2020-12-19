module Day19

open System
open System.IO
open Problem


let parseRule rule =
        let split = String.split ": " rule
        Int32.Parse split.[0], split.[1]


let (rules, lines) =
    File.ReadAllText "Day19/input.txt"
    |> String.split "\r\n\r\n"
    |> fun s -> List.map parseRule (String.split "\r\n" s.[0]) |> Map.ofSeq , String.split "\r\n" s.[1]


let rec isMatch (allRules: Map<int, string>) (rulesToGo: List<int>) (line: string) =
    match line, rulesToGo with
    | "", _ -> List.isEmpty rulesToGo
    | _, [] -> false
    | _ ->
        let nextRule = Map.find rulesToGo.[0] allRules
        if Char.isAlpha nextRule.[1] then
            line.StartsWith nextRule.[1] && isMatch allRules (List.tail rulesToGo) (line.Substring 1)
        else
            nextRule
            |> String.split " | "
            |> List.exists (fun sr -> isMatch allRules ((String.split " " sr |> List.map Int32.Parse) @ (List.tail rulesToGo)) line)


type Day19 () =
    interface IProblem with
        member _.Number = "19"

        /// How many messages completely match rule 0?
        member _.Part1 () =
            lines
            |> List.filter (isMatch rules [ 0 ])
            |> List.length
            |> sprintf "%i"

        /// After updating rules 8 and 11, how many messages completely match rule 0?
        member _.Part2 () =
            // Dua Lipa.
            let newRules =
                rules
                    |> (uncurry Map.add) (parseRule "8: 42 | 42 8")
                    |> (uncurry Map.add) (parseRule "11: 42 31 | 42 11 31")

            lines
            |> List.filter (isMatch newRules [ 0 ])
            |> List.length
            |> sprintf "%i"
