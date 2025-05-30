pub enum Symbol with Eq {
    case X,
    case O,
    case Empty
}

mod Symbol {

    pub def complement(symbol: Symbol): Symbol  = match symbol {
        case Symbol.X       => Symbol.O
        case Symbol.O       => Symbol.X
        case Symbol.Empty   => Symbol.Empty
    }

    pub def toString(s: Symbol): String = match s {
        case Symbol.X => "X"
        case Symbol.O => "O"
        case Symbol.Empty => " "
    }
}
