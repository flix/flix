///
/// An implementation of the reduced product of the Parity and Sign domains.
///
mod Domain.ParitySign {

    ///
    /// The elements of the reduced product of the Parity and Sign lattices.
    ///
    /// NB: This figure is slightly misleading, because ParitySign.ONeg !<= ParitySign.ENeg and ParitySign.OPos !<= ParitySign.EPos.
    ///
    // TODO: Consider how to re-implement this.
    enum ParitySign with Eq {
                              case Top,
            // TODO: Introduce Odd/Even and Pos/Neg?
                       case ENeg,    case EPos,

        case ONeg,            case Zer,            case OPos,

                              case Bot
    }

    ///
    /// Returns `true` iff `e1` is equal to `e2`.
    ///
    pub def equ(e1: ParitySign, e2: ParitySign): Bool = e1 == e2

    ///
    /// Returns `true` iff `e1` is less than or equal to `e2`.
    ///
    pub def leq(x: ParitySign, y: ParitySign): Bool = match (x, y) {
        case (ParitySign.Bot, _)                => true
        case (ParitySign.ONeg, ParitySign.ONeg) => true
        case (ParitySign.Zer, ParitySign.Zer)   => true
        case (ParitySign.Zer, ParitySign.ENeg)  => true
        case (ParitySign.Zer, ParitySign.EPos)  => true
        case (ParitySign.OPos, ParitySign.OPos) => true
        case (ParitySign.ENeg, ParitySign.ENeg) => true
        case (ParitySign.EPos, ParitySign.EPos) => true
        case (_, ParitySign.Top)                => true
        case _                                  => false
    }

    ///
    /// Returns the least upper bound of `e1` and `e2`.
    ///
    pub def lub(e1: ParitySign, e2: ParitySign): ParitySign = match (e1, e2) {
        case (ParitySign.Bot, x)                => x
        case (x, ParitySign.Bot)                => x
        case (ParitySign.ONeg, ParitySign.ONeg) => ParitySign.ONeg
        case (ParitySign.Zer,  ParitySign.Zer)  => ParitySign.Zer
        case (ParitySign.Zer,  ParitySign.ENeg) => ParitySign.ENeg
        case (ParitySign.Zer,  ParitySign.EPos) => ParitySign.EPos
        case (ParitySign.OPos, ParitySign.OPos) => ParitySign.OPos
        case (ParitySign.ENeg, ParitySign.ENeg) => ParitySign.ENeg
        case (ParitySign.ENeg,  ParitySign.Zer) => ParitySign.ENeg
        case (ParitySign.EPos, ParitySign.EPos) => ParitySign.EPos
        case (ParitySign.EPos,  ParitySign.Zer) => ParitySign.EPos
        case _                                  => ParitySign.Top
    }

    ///
    /// Returns the greatest lower bound of `e1` and `e2`.
    ///
    pub def glb(e1: ParitySign, e2: ParitySign): ParitySign = match (e1, e2) {
        case (ParitySign.Top, x)                => x
        case (x, ParitySign.Top)                => x
        case (ParitySign.ONeg, ParitySign.ONeg) => ParitySign.ONeg
        case (ParitySign.Zer,  ParitySign.Zer)  => ParitySign.Zer
        case (ParitySign.Zer,  ParitySign.ENeg) => ParitySign.Zer
        case (ParitySign.Zer,  ParitySign.EPos) => ParitySign.Zer
        case (ParitySign.OPos, ParitySign.OPos) => ParitySign.OPos
        case (ParitySign.ENeg, ParitySign.ENeg) => ParitySign.ENeg
        case (ParitySign.ENeg,  ParitySign.Zer) => ParitySign.Zer
        case (ParitySign.EPos, ParitySign.EPos) => ParitySign.EPos
        case (ParitySign.EPos,  ParitySign.Zer) => ParitySign.Zer
        case (ParitySign.ENeg, ParitySign.EPos) => ParitySign.Zer
        case (ParitySign.EPos, ParitySign.ENeg) => ParitySign.Zer
        case _                                  => ParitySign.Bot
    }

    ///
    /// Abstracts a concrete integer into the Parity domain.
    ///
    pub def alpha(i: Int32): ParitySign = {
        if      (i == 0)    ParitySign.Zer
        else if (i <= 0)    if (i `Int32.remainder` 2 != 0) ParitySign.ONeg else ParitySign.ENeg
        else                if (i `Int32.remainder` 2 != 0) ParitySign.OPos else ParitySign.EPos
    }

    ///
    /// Over-approximates integer `increment`.
    ///
    pub def inc(e: ParitySign): ParitySign = match e {
        case ParitySign.Bot  => ParitySign.Bot
        case ParitySign.ONeg => ParitySign.ENeg
        case ParitySign.Zer  => ParitySign.OPos
        case ParitySign.OPos => ParitySign.EPos
        case ParitySign.ENeg => ParitySign.Top
        case ParitySign.EPos => ParitySign.OPos
        case _               => ParitySign.Top
    }

    ///
    /// Over-approximates integer `decrement`.
    ///
    pub def dec(e: ParitySign): ParitySign = match e {
        case ParitySign.Bot  => ParitySign.Bot
        case ParitySign.ONeg => ParitySign.ENeg
        case ParitySign.Zer  => ParitySign.ONeg
        case ParitySign.OPos => ParitySign.EPos
        case ParitySign.ENeg => ParitySign.ONeg
        case ParitySign.EPos => ParitySign.Top
        case _               => ParitySign.Top
    }

    ///
    /// Over-approximates integer `addition`.
    ///
    pub def plus(e1: ParitySign, e2: ParitySign): ParitySign = match (e1, e2) {
        case (ParitySign.Bot, _) => ParitySign.Bot
        case (_, ParitySign.Bot) => ParitySign.Bot
        case _                   => ParitySign.Top
        // case (ParitySign.ONeg, ParitySign.ONeg) => Even // TODO: Need Even and Odd
    }

}
