module Day4

open System
open System.IO
open System.Text.RegularExpressions
open Common
open Problem


/// Checks whether a string is an integer, and if so also checks a predicate on it.
let stringIsIntWith (str: string) predicate =
    match Int32.TryParse str with
    | true, intVal -> predicate intVal str
    | _ -> false


let (|ValidLength|InvalidLength|) input =
    let result = Regex.Match (input, "\A(\d+)(cm|in)\z")
    if result.Success then
        match result.Groups.[2].Value, Int32.Parse result.Groups.[1].Value with
        | "cm", length when length >= 150 && length <= 193 -> ValidLength
        | "in", length when length >= 59 && length <= 76 -> ValidLength
        | _ -> InvalidLength
    else InvalidLength


type PassportField = {
    id: string
    value: string
} with
    static member FromString (str: string) =
        let split = str.Split (':')
        { id = split.[0]
          value = split.[1]
        }

    static member Validate field =
        match field.id with
        | "byr" -> stringIsIntWith field.value (fun i _ -> i >= 1920 && i <= 2002)
        | "iyr" -> stringIsIntWith field.value (fun i _ -> i >= 2010 && i <= 2020)
        | "eyr" -> stringIsIntWith field.value (fun i _ -> i >= 2020 && i <= 2030)
        | "hgt" -> match field.value with
                   | ValidLength -> true
                   | InvalidLength -> false
        | "hcl" -> Regex.IsMatch (field.value, "\A#[0-9a-f]{6}\z")
        | "ecl" -> ["amb" ; "blu" ; "brn" ; "gry" ; "grn" ; "hzl" ; "oth"] |> List.contains field.value
        | "pid" -> stringIsIntWith field.value (fun _ str -> String.length str = 9)
        | "cid" -> true
        | _ -> true


type Passport = Passport of List<PassportField>
with
    static member FromString (str: string) =
        str.Split (' ')
        |> List.ofSeq
        |> List.map PassportField.FromString
        |> Passport

    /// Checks that the passport has at least the required fields.
    member this.HasRequiredFields =
        let (Passport fields) = this
        let requiredFields = [ "byr" ; "iyr" ; "eyr" ; "hgt" ; "hcl" ; "ecl" ; "pid" ] |> Set.ofList
        fields
        |> List.map (fun f -> f.id)
        |> Set.ofList
        |> flip Set.isSuperset requiredFields

    /// Checks that all fields are valid.
    member this.AllFieldsValid =
        let (Passport fields) = this
        fields
        |> List.map PassportField.Validate
        |> List.contains false
        |> not


let input =
    File.ReadLines "Day4/input.txt"
    |> Seq.toList
    |> List.batchWhile (fun l -> l <> "") false
    |> List.map (String.join " ")
    |> List.map Passport.FromString


type Day4 () =
    interface IProblem with
        member _.Number = "4"

        /// Treat cid as optional. In your batch file, how many passports are valid?
        member _.Part1 () =
            input
            |> List.filter (fun p -> p.HasRequiredFields)
            |> List.length
            |> Console.WriteLine

        /// Continue to treat cid as optional. In your batch file, how many passports are valid?
        member _.Part2 () =
            input
            |> List.filter (fun p -> p.HasRequiredFields && p.AllFieldsValid)
            |> List.length
            |> Console.WriteLine
