[<AutoOpen>]
module Prelude


module Bool =
    /// Converts a bool to an ordinal value (1 for true, 0 for false).
    let ord b = if b then 1 else 0
