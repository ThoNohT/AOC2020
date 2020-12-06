module Program

open System
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


[<EntryPoint>]
let rec main _ =
    Console.WriteLine ("Please choose a problem to solve.")
    let input = (Console.ReadLine ()).ToLowerInvariant ()

    if "quit".StartsWith input || "exit".StartsWith input then 0
    else
        // Solve a problem if a valid one was provided.
        (Map.tryFind input problems)
        |> Option.map (fun p -> p ())
        |> Option.defaultWith (fun _ -> Console.WriteLine "Problem not found.")

        main [||]
