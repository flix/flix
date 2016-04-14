/**
 * An implementation of the reduced product of the Parity and Sign domains.
 */
namespace ParitySign {

    /**
     * The elements of the reduced product of the Parity and Sign lattices.
     */
    enum ParitySign {
                         case Top,

        case ONeg, case ENeg, case OPos, case EPos,

                         case Zer,

                         case Bot
    }

    /**
     * Returns `true` iff `e1` is less than or equal to `e2`.
     */
    def leq(x: ParitySign, y: ParitySign): Bool = match (x, y) with {
        case (ParitySign.Bot, _)                => true
        case (ParitySign.Zer, ParitySign.Zer)   => true
        case (ParitySign.Zer, ParitySign.ONeg)  => true
        case (ParitySign.Zer, ParitySign.ENeg)  => true
        case (ParitySign.Zer, ParitySign.OPos)  => true
        case (ParitySign.Zer, ParitySign.EPos)  => true
        case (ParitySign.ONeg, ParitySign.ONeg) => true
        case (ParitySign.ENeg, ParitySign.ENeg) => true
        case (ParitySign.OPos, ParitySign.OPos) => true
        case (ParitySign.EPos, ParitySign.EPos) => true
        case (_, ParitySign.Top)                => true
        case _                                  => false
    }

    /**
     * Returns the least upper bound of `e1` and `e2`.
     */
    @commutative @associative
    def lub(e1: ParitySign, e2: ParitySign): ParitySign = match (e1, e2) with {
        case (ParitySign.Bot, x)                => x
        case (x, ParitySign.Bot)                => x
        case (ParitySign.Zer,  ParitySign.Zer)  => ParitySign.Zer
        case (ParitySign.Zer,  ParitySign.ONeg) => ParitySign.ONeg
        case (ParitySign.Zer,  ParitySign.ENeg) => ParitySign.ENeg
        case (ParitySign.Zer,  ParitySign.OPos) => ParitySign.OPos
        case (ParitySign.Zer,  ParitySign.EPos) => ParitySign.EPos
        case (ParitySign.ONeg, ParitySign.ONeg) => ParitySign.ONeg
        case (ParitySign.ONeg, ParitySign.Zer)  => ParitySign.ONeg
        case (ParitySign.ENeg, ParitySign.ENeg) => ParitySign.ENeg
        case (ParitySign.ENeg, ParitySign.Zer)  => ParitySign.ENeg
        case (ParitySign.OPos, ParitySign.OPos) => ParitySign.OPos
        case (ParitySign.OPos, ParitySign.Zer)  => ParitySign.OPos
        case (ParitySign.EPos, ParitySign.EPos) => ParitySign.EPos
        case (ParitySign.EPos, ParitySign.Zer)  => ParitySign.EPos
        case _ => ParitySign.Top
    }

    /**
     * Returns the greatest lower bound of `e1` and `e2`.
     */
    @commutative @associative
    def glb(e1: ParitySign, e2: ParitySign): ParitySign = match (e1, e2) with {
        case (ParitySign.Top, x)                => x
        case (x, ParitySign.Top)                => x
        case (ParitySign.Zer,  ParitySign.Zer)  => ParitySign.Zer
        case (ParitySign.Zer,  ParitySign.ONeg) => ParitySign.Zer
        case (ParitySign.Zer,  ParitySign.ENeg) => ParitySign.Zer
        case (ParitySign.Zer,  ParitySign.OPos) => ParitySign.Zer
        case (ParitySign.Zer,  ParitySign.EPos) => ParitySign.Zer
        case (ParitySign.ONeg, ParitySign.ONeg) => ParitySign.ONeg
        case (ParitySign.ONeg, ParitySign.ONeg) => ParitySign.ONeg
        case (ParitySign.ENeg, ParitySign.ENeg) => ParitySign.ENeg
        case (ParitySign.OPos, ParitySign.OPos) => ParitySign.OPos
        case (ParitySign.EPos, ParitySign.EPos) => ParitySign.EPos
        case (ParitySign.ONeg, ParitySign.ENeg) => ParitySign.Zer
        case (ParitySign.ONeg, ParitySign.OPos) => ParitySign.Zer
        case (ParitySign.ONeg, ParitySign.EPos) => ParitySign.Zer
        case (ParitySign.ENeg, ParitySign.ONeg) => ParitySign.Zer
        case (ParitySign.ENeg, ParitySign.OPos) => ParitySign.Zer
        case (ParitySign.ENeg, ParitySign.EPos) => ParitySign.Zer
        case (ParitySign.OPos, ParitySign.ONeg) => ParitySign.Zer
        case (ParitySign.OPos, ParitySign.ENeg) => ParitySign.Zer
        case (ParitySign.OPos, ParitySign.EPos) => ParitySign.Zer
        case (ParitySign.EPos, ParitySign.ONeg) => ParitySign.Zer
        case (ParitySign.EPos, ParitySign.ENeg) => ParitySign.Zer
        case (ParitySign.EPos, ParitySign.OPos) => ParitySign.Zer
        case (ParitySign.ONeg,  ParitySign.Zer) => ParitySign.Zer
        case (ParitySign.ENeg,  ParitySign.Zer) => ParitySign.Zer
        case (ParitySign.OPos,  ParitySign.Zer) => ParitySign.Zer
        case (ParitySign.EPos,  ParitySign.Zer) => ParitySign.Zer
        case _ => ParitySign.Bot
    }

    let ParitySign<> = (ParitySign.Bot, ParitySign.Top, leq, lub, glb)

}