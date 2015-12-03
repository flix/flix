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

    // TODO ...

}
