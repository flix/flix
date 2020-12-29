/**
 * An implementation of the reduced product of the Parity and Sign domains.
 */
namespace Domain/ParitySign {

    ///
    /// The elements of the reduced product of the Parity and Sign lattices.
    ///
    /// NB: This figure is slightly misleading, because ONeg !<= ENeg and OPos !<= EPos.
    ///
    // TODO: Consider how to re-implement this.
    enum ParitySign {
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
        case (Bot, _)       => true
        case (ONeg, ONeg)   => true
        case (Zer, Zer)     => true
        case (Zer, ENeg)    => true
        case (Zer, EPos)    => true
        case (OPos, OPos)   => true
        case (ENeg, ENeg)   => true
        case (EPos, EPos)   => true
        case (_, Top)       => true
        case _              => false
    }

    ///
    /// Returns the least upper bound of `e1` and `e2`.
    ///
    pub def lub(e1: ParitySign, e2: ParitySign): ParitySign = match (e1, e2) {
        case (Bot, x)       => x
        case (x, Bot)       => x
        case (ONeg, ONeg)   => ONeg
        case (Zer,  Zer)    => Zer
        case (Zer,  ENeg)   => ENeg
        case (Zer,  EPos)   => EPos
        case (OPos, OPos)   => OPos
        case (ENeg, ENeg)   => ENeg
        case (ENeg,  Zer)   => ENeg
        case (EPos, EPos)   => EPos
        case (EPos,  Zer)   => EPos
        case _              => Top
    }

    ///
    /// Returns the greatest lower bound of `e1` and `e2`.
    ///
    pub def glb(e1: ParitySign, e2: ParitySign): ParitySign = match (e1, e2) {
        case (Top, x)       => x
        case (x, Top)       => x
        case (ONeg, ONeg)   => ONeg
        case (Zer,  Zer)    => Zer
        case (Zer,  ENeg)   => Zer
        case (Zer,  EPos)   => Zer
        case (OPos, OPos)   => OPos
        case (ENeg, ENeg)   => ENeg
        case (ENeg,  Zer)   => Zer
        case (EPos, EPos)   => EPos
        case (EPos,  Zer)   => Zer
        case (ENeg, EPos)   => Zer
        case (EPos, ENeg)   => Zer
        case _              => Bot
    }

    ///
    /// Abstracts a concrete integer into the Parity domain.
    ///
    pub def alpha(i: Int): ParitySign = {
        if      (i == 0)    Zer
        else if (i <= 0)    if (i % 2 != 0) ONeg else ENeg
        else                if (i % 2 != 0) OPos else EPos
    }

    ///
    /// Over-approximates integer `increment`.
    ///
    pub def inc(e: ParitySign): ParitySign = match e {
        case Bot    => Bot
        case ONeg   => ENeg
        case Zer    => OPos
        case OPos   => EPos
        case ENeg   => Top
        case EPos   => OPos
        case _      => Top
    }

    ///
    /// Over-approximates integer `decrement`.
    ///
    pub def dec(e: ParitySign): ParitySign = match e {
        case Bot    => Bot
        case ONeg   => ENeg
        case Zer    => ONeg
        case OPos   => EPos
        case ENeg   => ONeg
        case EPos   => Top
        case _      => Top
    }

    ///
    /// Over-approximates integer `addition`.
    ///
    pub def plus(e1: ParitySign, e2: ParitySign): ParitySign = match (e1, e2) {
        case (Bot, _)   => Bot
        case (_, Bot)   => Bot
        // case (ONeg, ONeg) => Even // TODO: Need Even and Odd
        case _                                  => Top
    }

}