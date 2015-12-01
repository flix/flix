/**
 * An implementation of the sign abstract domain.
 */
namespace Sign {

    /**
     * The elements of the sign lattice.
     */
    enum Sign {
                  case Top,

        case Neg, case Zer, case Pos,

                  case Bot
    }

    /**
     * Returns `true` iff `e1` is less than or equal to `e2`.
     */
    fn leq(e1: Sign, e2: Sign): Bool = match (e1, e2) with {
        case (Sign.Bot, _)        => true
        case (Sign.Neg, Sign.Neg) => true
        case (Sign.Zer, Sign.Zer) => true
        case (Sign.Pos, Sign.Pos) => true
        case (_, Sign.Top)        => true
        case _                    => false
    }

    /**
     * Returns the least upper bound of `e1` and `e2`.
     */
    fn lub(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Sign.Bot, x)        => x
        case (x, Sign.Bot)        => x
        case (Sign.Neg, Sign.Neg) => Sign.Neg
        case (Sign.Zer, Sign.Zer) => Sign.Zer
        case (Sign.Pos, Sign.Pos) => Sign.Pos
        case _                    => Sign.Top
    }

    /**
     * Returns the greatest lower bound of `e1` and `e2`.
     */
    fn glb(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Sign.Top, x)        => x
        case (x, Sign.Top)        => x
        case (Sign.Neg, Sign.Neg) => Sign.Neg
        case (Sign.Zer, Sign.Zer) => Sign.Zer
        case (Sign.Pos, Sign.Pos) => Sign.Pos
        case _                    => Sign.Bot
    }

    /**
     * Over-approximates integer `addition`.
     */
    @strict @mono
    fn plus(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Sign.Bot, _)        => Sign.Bot
        case (_, Sign.Bot)        => Sign.Bot
        case (Sign.Neg, Sign.Neg) => Sign.Neg
        case (Sign.Neg, Sign.Zer) => Sign.Neg
        case (Sign.Neg, Sign.Pos) => Sign.Top
        case (Sign.Zer, Sign.Neg) => Sign.Neg
        case (Sign.Zer, Sign.Zer) => Sign.Zer
        case (Sign.Zer, Sign.Pos) => Sign.Pos
        case (Sign.Pos, Sign.Neg) => Sign.Top
        case (Sign.Pos, Sign.Zer) => Sign.Pos
        case (Sign.Pos, Sign.Pos) => Sign.Pos
        case _                    => Sign.Top
    }

    /**
     * Over-approximates integer `subtraction`.
     */
    @strict @mono
    fn times(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Sign.Bot, _)        => Sign.Bot
        case (_, Sign.Bot)        => Sign.Bot
        case (Sign.Neg, Sign.Neg) => Sign.Pos
        case (Sign.Neg, Sign.Zer) => Sign.Zer
        case (Sign.Neg, Sign.Pos) => Sign.Neg
        case (Sign.Zer, Sign.Neg) => Sign.Zer
        case (Sign.Zer, Sign.Zer) => Sign.Zer
        case (Sign.Zer, Sign.Pos) => Sign.Zer
        case (Sign.Pos, Sign.Neg) => Sign.Neg
        case (Sign.Pos, Sign.Zer) => Sign.Zer
        case (Sign.Pos, Sign.Pos) => Sign.Pos
        case _                    => Sign.Top
    }

    /**
     * Over-approximates integer `multiplication`.
     */


    /**
     * Over-approximates integer `division`.
     */


    // + - * /
     // TODO: more operations:
     // modulo
     // bitwise and, or, xor
     // less than, etc.
     // eq ==, !=
     // left shift <<
     // right shift >>
     //

};



