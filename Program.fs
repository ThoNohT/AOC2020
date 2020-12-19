module Program

open System
open System.Diagnostics
open Problem


/// Lists the available problems to solve.
let problems =
    AppDomain.CurrentDomain.GetAssemblies ()
    |> Seq.collect (fun assy -> assy.GetTypes ())
    |> Seq.filter (fun t -> typedefof<IProblem>.IsAssignableFrom t && not t.IsInterface)
    |> Seq.map (fun t -> (Activator.CreateInstance t) :?> IProblem)
    |> Seq.collect (fun (problem : IProblem) ->
        [ (problem.Number + ".1", (fun _ -> problem.Part1 ()))
          (problem.Number + ".2", (fun _ -> problem.Part2 ()))
        ])
    |> Map.ofSeq


let runAndTime p =
    let sw = Stopwatch.StartNew ()
    let result = p ()
    result, sw.Elapsed.Ticks / (TimeSpan.TicksPerMillisecond / 1000L)


let showTime time =
    if time < 1000.0 then
        sprintf "%f μs" time
    else
        sprintf "%f ms" (time / 1000.0)


let profileProblem p =
    let (result, us) = runAndTime p

    Console.WriteLine (sprintf "Answer: %s" result)

    if us < 100_000L then
        // The problem was solved quickly (less than 100 ms), so run multiple times to get a more accurate average.
        let average = List.map (fun _ -> runAndTime p |> snd |> double) [0..99] |> List.average
        Console.WriteLine (sprintf "Average runtime over 100 runs: %s." (showTime average))
    else
        Console.WriteLine (sprintf "Runtime: %i ms." (us / 1000L))


/// If Some, this problem will be ran, and then the program will exit immediately. Otherwise, an input loop is started.
let debugProblem = None

[<EntryPoint>]
let rec main _ =
    debugProblem
    |> Option.map (sprintf "Runing problem %s")
    |> Option.defaultValue "Please choose a problem to solve."
    |> printfn "%s"

    let input =
        debugProblem
        |> Option.defaultWith (fun _ -> (Console.ReadLine ()).ToLowerInvariant ())

    if input <> "" && ("quit".StartsWith input || "exit".StartsWith input) then 0
    else
        // Solve a problem if a valid one was provided.
        (Map.tryFind input problems)
        |> Option.map profileProblem
        |> Option.defaultWith (fun _ -> printfn "Problem not found.")

        if Option.isSome debugProblem then 0 else main [||]
