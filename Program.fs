module Program

open Problem


/// Lists the available problems to solve.
let problems : Map<string, IProblem> =
    [ ("1.1", Day1.Part1.Problem)
      ("1.2", Day1.Part2.Problem)
    ]
    |> Map.ofList


open System


[<EntryPoint>]
let rec main _ =
    Console.WriteLine ("Please choose a problem to solve.")
    let input = (Console.ReadLine ()).ToLowerInvariant ()

    if input = "quit" || input = "exit" then 0
    else
        // Solve a problem if a valid one was provided.
        (Map.tryFind input problems)
        |> Option.map (fun p -> p.Solve ())
        |> Option.defaultWith (fun _ -> Console.WriteLine "Problem not found.")

        main [||]