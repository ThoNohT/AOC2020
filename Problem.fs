module Problem


/// Interface for a problem that can be solved.
type IProblem =
    /// Solve the problem.
    abstract member Solve : unit -> unit