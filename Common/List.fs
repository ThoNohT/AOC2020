module Common.List


/// Requirement: predicate matches symmetrically.
let rec findPairSym (predicate : 'a -> 'a -> bool) (list : List<'a>) : Option<'a * 'a> =
    match list with
    | x :: xs ->
        // Find matching element in the rest of the list. Note that we only need to search to the right, since the pair matches symmetrically.
        List.tryFind (fun y -> predicate x y) xs
        |> Option.map (fun y -> (x, y))
        |> Option.orElseWith (fun _ -> findPairSym predicate xs)
    | [] -> None


/// Requirement: predicate matches symmetrically.
let rec findThreeSym (predicate : 'a -> 'a -> 'a -> bool) (list : List<'a>) : Option<'a * 'a * 'a> =
    match list with
    | x :: xs ->
        // Find matching element in the rest of the list. Note that we only need to search to the right, since the pair matches symmetrically.
        findPairSym (fun y z -> predicate x y z) xs
        |> Option.map (fun (y, z) -> (x, y, z))
        |> Option.orElseWith (fun _ -> findThreeSym predicate xs)
    | [] -> None


// Generalize on the number of elements, rather than the predicate.
let rec findNWithSum (n: int) (sum: int) (list : List<int>) : Option<List<int>> =
    match list with
    | x :: xs when n > 1 ->
        findNWithSum (n - 1) (sum - x) xs
        |> Option.map (fun r -> x :: r)
        |> Option.orElseWith (fun _ -> findNWithSum n sum xs)
    | x :: xs when n = 1 ->
        if x = sum then
            Some [x]
        else
            findNWithSum n sum xs
    | _ -> None