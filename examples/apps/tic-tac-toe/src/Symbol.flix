///
/// Represents the different symbols used in the game.
///
pub enum Symbol with Eq {
    case X,
    case O,
    case Empty
}

mod Symbol {

    ///
    /// Returns the opposite symbol, i.e., if the input is `X`, it returns `O`, and vice versa.
    ///
    pub def complement(symbol: Symbol): Symbol  = match symbol {
        case Symbol.X       => Symbol.O
        case Symbol.O       => Symbol.X
        case Symbol.Empty   => Symbol.Empty
    }

    ///
    /// Returns a string representation of the symbol.
    ///
    pub def toString(s: Symbol): String = match s {
        case Symbol.X => "X"
        case Symbol.O => "O"
        case Symbol.Empty => " "
    }
}
