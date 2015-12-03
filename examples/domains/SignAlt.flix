/**
 * An alternative implementation of the sign abstract domain
 * where zero is considered to be both positive and negative.
 *
 * See Cousot & Cousot '77 for details.
 */
namespace SignAlt {

    /**
     * The elements of the (alternative) sign lattice.
     *
     * Notice that Neg and Pos implicitely includes zero.
     */
    enum Sign {
                  case Top,

        case Neg,          case Pos,

                  case Zer,

                  case Bot
    }

    /**
     * Returns `true` iff `e1` is less than or equal to `e2`.
     */
    fn leq(e1: Sign, e2: Sign): Bool = match (e1, e2) with {
        case (Sign.Bot, _)          => true
        case (Sign.Zer, Sign.Zer)   => true
        case (Sign.Zer, Sign.Neg)   => true
        case (Sign.Zer, Sign.Pos)   => true
        case (Sign.Neg, Sign.Neg)   => true
        case (Sign.Pos, Sign.Pos)   => true
        case (_, Sign.Top)          => true
        case _                      => false
    }

    /**
     * Returns the least upper bound of `e1` and `e2`.
     */
    @commutative @associative
    fn lub(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Sign.Bot, x)          => x
        case (x, Sign.Bot)          => x
        case (Sign.Zer, Sign.Zer)   => Sign.Zer
        case (Sign.Zer, Sign.Neg)   => Sign.Neg
        case (Sign.Neg, Sign.Zer)   => Sign.Neg
        case (Sign.Zer, Sign.Pos)   => Sign.Pos
        case (Sign.Pos, Sign.Zer)   => Sign.Pos
        case (Sign.Neg, Sign.Neg)   => Sign.Neg
        case (Sign.Pos, Sign.Pos)   => Sign.Pos
        case _                      => Sign.Top
    }

    /**
     * Returns the greatest lower bound of `e1` and `e2`.
     */
    @commutative @associative
    fn glb(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Sign.Top, x)          => x
        case (x, Sign.Top)          => x
        case (Sign.Zer, Sign.Zer)   => Sign.Zer
        case (Sign.Neg, Sign.Neg)   => Sign.Neg
        case (Sign.Pos, Sign.Pos)   => Sign.Pos
        case (Sign.Zer, Sign.Neg)   => Sign.Zer
        case (Sign.Neg, Sign.Zer)   => Sign.Zer
        case (Sign.Zer, Sign.Pos)   => Sign.Zer
        case (Sign.Pos, Sign.Zer)   => Sign.Zer
        case _                      => Sign.Bot
    }


}
