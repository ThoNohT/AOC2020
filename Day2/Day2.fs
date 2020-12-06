module Day2

open System
open System.IO
open Problem


type PasswordWithPolicy = {
    number1: int
    number2 : int
    restrictedLetter : char
    password : string }
with
    static member FromString (input: string) =
        let split = input.Split(' ')
        let occurrenceSplit = split.[0].Split('-')
        { number1 = Int32.Parse (occurrenceSplit.[0])
          number2 = Int32.Parse (occurrenceSplit.[1])
          restrictedLetter = split.[1].[0]
          password = split.[2]
        }

    /// Restricted letter must occur between number1 and number2 times.
    static member IsValidPostal pwp =
        let restrictedLetterOccurences = pwp.password |> Seq.filter (fun c -> c = pwp.restrictedLetter) |> Seq.length
        restrictedLetterOccurences >= pwp.number1 && restrictedLetterOccurences <= pwp.number2

    /// Either (1-based) index number1 or index number2 must contain restricted letter, not both.
    static member IsValidShop pwp =
        (pwp.password.[pwp.number1 - 1] = pwp.restrictedLetter) <> (pwp.password.[pwp.number2 - 1] = pwp.restrictedLetter)


let input =
    File.ReadLines "Day2/input.txt"
    |> Seq.map (PasswordWithPolicy.FromString)
    |> Seq.toList


type Day2 () =
    interface IProblem with
        member _.Number = "2"

        /// How many passwords are valid according to their policies?
        member _.Part1 () =
            input
            |> List.filter (PasswordWithPolicy.IsValidPostal)
            |> List.length
            |> Console.WriteLine

        /// How many passwords are valid according to the new interpretation of the policies?
        member _.Part2 () =
            input
            |> List.filter (PasswordWithPolicy.IsValidShop)
            |> List.length
            |> Console.WriteLine
