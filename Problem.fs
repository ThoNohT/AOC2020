module Problem


/// Interface for a problem that can be solved.
type IProblem =
    // The problem number, used for choosing it in the command line.
    abstract member Number : string

    /// Solve part 1 of the problem. Return the result as a string.
    abstract member Part1 : unit -> string

    // Solve part 2 of the problem. Return the result as a string.
    abstract member Part2 : unit -> string
