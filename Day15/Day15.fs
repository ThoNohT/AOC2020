module Day15

open Problem


let input = [ 0 ; 6 ; 1 ; 7 ; 2 ; 19 ; 20 ]


/// Note that never isn't actually used in the game, but simply a convenience value to start building up the fold.
type LastOccurrences = Never | Once of int | MoreThanOnce of int * int
with
    static member Init next = Once next

    static member Add next this =
        match this with
        | Never _ -> Once next
        | Once first -> MoreThanOnce (first, next)
        | MoreThanOnce (_, second) -> MoreThanOnce (second, next)

    /// Call this on the last produced element to get the next number.
    member this.DetermineNextNumber =
        match this with
        | Never -> 0
        | Once _ -> 0
        | MoreThanOnce (first, second) -> second - first


module Game =
    let init list =
        let addStartingNumber (round, acc, _) elem =
            let lo = LastOccurrences.Init round
            round + 1, Map.add elem lo acc, lo
        List.fold addStartingNumber (1, Map.empty, Never) list

    let play totalRounds list  =
        let rec playRound round acc (lo: LastOccurrences) =
            let nextNumber = lo.DetermineNextNumber
            let occurrence =
                Map.tryFind nextNumber acc
                |> Option.defaultValue Never
                |> LastOccurrences.Add round

            if round = totalRounds then nextNumber
            else playRound (round + 1) (Map.add nextNumber occurrence acc) occurrence

        let (round, acc, lastOccurrence) = init list
        playRound round acc lastOccurrence


type Day15 () =
    interface IProblem with
        member _.Number = "15"

        /// Given your starting numbers, what will be the 2020th number spoken?
        member _.Part1 () =
            input
            |> Game.play 2020
            |> sprintf "%A"

        /// Given your starting numbers, what will be the 30000000th number spoken?
        member _.Part2 () =
            input
            |> Game.play 30000000
            |> sprintf "%A"
