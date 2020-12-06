module Problem


/// Interface for a problem that can be solved.
type IProblem =
    // The problem number, used for choosing it in the command line.
    abstract member Number : string

    /// Solve part 1 of the problem.
    abstract member Part1 : unit -> unit

    // Solve part 2 of the problem.
    abstract member Part2 : unit -> unit
