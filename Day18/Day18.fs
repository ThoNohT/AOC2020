module Day18

open System
open System.IO
open Problem

module P = Parser
module PB = Parser.Basic
module PC = Parser.Combinators
module PCh = Parser.Char
module PI = Parser.Int
module PR = Parser.Regex
module PS = Parser.String


type Expression =
    | Number of int64
    | Addition of Expression * Expression
    | Multiplication of Expression * Expression
    | Grouping of Expression
with
    static member ToString = function
        | Number n -> sprintf "%i" n
        | Addition (a, b) -> sprintf "(%s + %s)" (Expression.ToString a) (Expression.ToString b)
        | Multiplication (a, b) -> sprintf "(%s * %s)" (Expression.ToString a) (Expression.ToString b)
        | Grouping g -> Expression.ToString g

    static member ParseNumber =
        PS.whitespace
        |> PC.discard (lazy PI.positiveLong)
        |> PC.skip (lazy PS.whitespace)
        |> PC.map Number

    static member ParseAddition =
        PB.succeed (curry Addition |> flip)
        |> PC.skip (lazy PS.whitespace)
        |> PC.andMap (lazy (PC.alt Expression.ParseGrouping Expression.ParseNumber |> PC.skip (lazy PS.whitespace) |> PC.skip (lazy PCh.literalChar '+') |> PC.skip (lazy PS.whitespace)))
        |> PC.andMap (lazy Expression.Parser)
        |> PC.skip (lazy PS.whitespace)

    static member ParseMultiplication =
        PB.succeed (curry Multiplication |> flip)
        |> PC.skip (lazy PS.whitespace)
        |> PC.andMap (lazy (PC.alt Expression.ParseGrouping Expression.ParseNumber |> PC.skip (lazy PS.whitespace) |> PC.skip (lazy PCh.literalChar '*') |> PC.skip (lazy PS.whitespace)))
        |> PC.andMap (lazy Expression.Parser)
        |> PC.skip (lazy PS.whitespace)

    static member ParseGrouping =
        PCh.literalChar ')'
        |> PC.discard (lazy PS.whitespace)
        |> PC.discard (lazy Expression.Parser)
        |> PC.skip (lazy PS.whitespace)
        |> PC.skip (lazy PCh.literalChar '(')
        |> PC.skip (lazy PS.whitespace)
        |> PC.map Grouping

    static member Parser =
        PC.oneOf [ Expression.ParseAddition ; Expression.ParseMultiplication ; Expression.ParseGrouping ; Expression.ParseNumber ]

    static member Solve = function
        | Number n -> n
        | Addition (a, b) -> Expression.Solve a + Expression.Solve b
        | Multiplication (a, b) -> Expression.Solve a * Expression.Solve b
        | Grouping g -> Expression.Solve g

    static member Transform expression =
        let transform' = function
            | Number n -> Number n
            | Addition (a, b) ->
                // Addition has higher precedence, so pull stuff in if it is multiplication outside.
                match a, b with
                | Multiplication (a1, a2), Multiplication (b1, b2) ->
                    Multiplication (
                        Expression.Transform a1,
                        Multiplication (
                            Expression.Transform <| Addition ( Expression.Transform a2, Expression.Transform b1),
                            Expression.Transform b2))

                | Multiplication (a1, a2), _ ->
                    Multiplication (
                        Expression.Transform a1,
                        Expression.Transform <| Addition (Expression.Transform a2, Expression.Transform b))

                | _, Multiplication (b1, b2) ->
                    Multiplication (
                        Addition (Expression.Transform a, Expression.Transform b1),
                        Expression.Transform b2)

                | _ -> Addition (Expression.Transform a, Expression.Transform b)

            | Multiplication (a, b) -> Multiplication (Expression.Transform a, Expression.Transform b)
            | Grouping g -> Grouping <| Expression.Transform g

        let transformed = transform' expression
        if expression = transformed then transformed else Expression.Transform transformed


let input =
    File.ReadAllLines "Day18/input.txt"
    |> Seq.map (Seq.rev >> String.fromCharSeq)
    |> List.ofSeq


type Day18 () =
    interface IProblem with
        member _.Number = "18"

        /// Evaluate the expression on each line of the homework; what is the sum of the resulting values?
        member _.Part1 () =
            input
            |> List.map (P.parseOrFail (PS.entire Expression.Parser) (lazy Exception "Failed parsing the expression"))
            |> List.map Expression.Solve
            |> List.sum
            |> sprintf "%i"

        /// What do you get if you add up the results of evaluating the homework problems using these new rules?
        member _.Part2 () =
            input
            |> List.map (P.parseOrFail (PS.entire Expression.Parser) (lazy Exception "Failed parsing the expression"))
            |> List.map Expression.Transform
            |> List.map Expression.Solve
            |> List.sum
            |> sprintf "%i"
