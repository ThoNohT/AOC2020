module Day18

open System
open System.IO
open Problem


let input =
    File.ReadAllLines "Day18/input.txt"
    |> Seq.map (fun s -> s.Replace (" " , ""))
    |> List.ofSeq


let isOp = function | '*' -> true | '+' -> true | _ -> false
let precedence1 = always 0
let precedence2 = function | '+' -> 1 | _ -> 0


let shuntingYard precedence (input: string) =
    let shunt (opStack, outQueue) char =
        if Char.isNum char then
            (opStack, Queue.push char outQueue)
        elif char = '(' then
            (Stack.push char opStack, outQueue)
        elif char = ')' then
            let mutable newStack = opStack
            let mutable newQueue = outQueue
            while Option.unwrap false (fun op -> op <> '(') (Stack.top newStack) do
                let (op, ns) = Stack.pop newStack
                newStack <- ns
                newQueue <- Queue.push op newQueue

            // Now there is a '('
            let (_, ns) = Stack.pop newStack
            (ns, newQueue)
        else // Operator
            let mutable newStack = opStack
            let mutable newQueue = outQueue
            while Option.unwrap false (fun op -> op <> '(' && precedence op >= precedence char) (Stack.top newStack) do
                let (op, ns) = Stack.pop newStack
                newStack <- ns
                newQueue <- Queue.push op newQueue

            (Stack.push char newStack, newQueue)

    // First go over all characters.
    let (opStack, outQueue ) = Seq.fold shunt (Stack.empty, Queue.empty) input

    // After all characters are processed, pop all operators onto the output queue.
    let mutable newStack = opStack
    let mutable newQueue = outQueue
    while Option.isSome (Stack.top newStack) do
        let (op, ns) = Stack.pop newStack
        newStack <- ns
        newQueue <- Queue.push op newQueue

    newQueue


let evaluatePostfix queue =
    let charToString ch = sprintf "%c" ch

    let evaluateOp n1 n2 = function
        | "+" -> (Int64.Parse n1) + (Int64.Parse n2) |> fun i -> i.ToString ()
        | "*" -> (Int64.Parse n1) * (Int64.Parse n2) |> fun i -> i.ToString ()
        |_ -> failwith "Invalid operator."

    let evaluate stack elem =
        if Option.isSome (Long.tryParse elem) then
            Stack.push elem stack
        else
            let n1, ns1 = Stack.pop stack
            let n2, ns2 = Stack.pop ns1
            Stack.push (evaluateOp n2 n1 elem) ns2

    let rec clearStack stack =
        match Stack.pop stack with
        | elem, _ when Option.isSome (Long.tryParse elem ) -> Int64.Parse elem
        | elem, ns ->
            let n1, ns1 = Stack.pop ns
            let n2, ns2 = Stack.pop ns1
            clearStack (Stack.push (evaluateOp n2 n1 elem) ns2)

    List.fold evaluate (Stack.empty) (Queue.toList queue |> List.map charToString)
    |> clearStack


type Day18 () =
    interface IProblem with
        member _.Number = "18"

        /// Evaluate the expression on each line of the homework; what is the sum of the resulting values?
        member _.Part1 () =
            input
            |> List.map (shuntingYard precedence1)
            |> List.sumBy evaluatePostfix
            |> sprintf "%i"

        /// What do you get if you add up the results of evaluating the homework problems using these new rules?
        member _.Part2 () =
            input
            |> List.map (shuntingYard precedence2)
            |> List.sumBy evaluatePostfix
            |> sprintf "%i"
