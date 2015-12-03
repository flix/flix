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

    /**
     * Lifts a regular number into the (alternative) sign domain.
     */
    fn lift(i: Int): Sign =
        if (i == 0) Sign.Zer else
            if (i < 0) Sign.Neg else
                Sign.Pos

    /**
     * Over-approximates integer `addition`.
     */
    @strict @monotone @commutative @associative
    fn plus(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Sign.Bot, _)          => Sign.Bot
        case (_, Sign.Bot)          => Sign.Bot
        case (Sign.Neg, Sign.Neg)   => Sign.Neg
        case (Sign.Neg, Sign.Zer)   => Sign.Neg
        case (Sign.Neg, Sign.Pos)   => Sign.Top
        case (Sign.Zer, Sign.Neg)   => Sign.Neg
        case (Sign.Zer, Sign.Zer)   => Sign.Zer
        case (Sign.Zer, Sign.Pos)   => Sign.Pos
        case (Sign.Pos, Sign.Neg)   => Sign.Top
        case (Sign.Pos, Sign.Zer)   => Sign.Pos
        case (Sign.Pos, Sign.Pos)   => Sign.Pos
        case  _                     => Sign.Top
    }

    /**
     * Over-approximates integer `subtraction`.
     */
    @strict @monotone
    fn minus(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Sign.Bot, _)          => Sign.Bot
        case (_, Sign.Bot)          => Sign.Bot
        case (Sign.Neg, Sign.Neg)   => Sign.Top
        case (Sign.Neg, Sign.Zer)   => Sign.Neg
        case (Sign.Neg, Sign.Pos)   => Sign.Neg
        case (Sign.Zer, Sign.Neg)   => Sign.Pos
        case (Sign.Zer, Sign.Zer)   => Sign.Zer
        case (Sign.Zer, Sign.Pos)   => Sign.Neg
        case (Sign.Pos, Sign.Neg)   => Sign.Pos
        case (Sign.Pos, Sign.Zer)   => Sign.Pos
        case (Sign.Pos, Sign.Pos)   => Sign.Top
        case  _                     => Sign.Top
    }

    /**
     * Over-approximates integer `multiplication`.
     */
    @strict @monotone
    fn times(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Sign.Bot, _)          => Sign.Bot
        case (_, Sign.Bot)          => Sign.Bot
        case (Sign.Neg, Sign.Neg)   => Sign.Pos
        case (Sign.Neg, Sign.Zer)   => Sign.Zer
        case (Sign.Neg, Sign.Pos)   => Sign.Neg
        case (Sign.Zer, Sign.Neg)   => Sign.Zer
        case (Sign.Zer, Sign.Zer)   => Sign.Zer
        case (Sign.Zer, Sign.Pos)   => Sign.Zer
        case (Sign.Pos, Sign.Neg)   => Sign.Neg
        case (Sign.Pos, Sign.Zer)   => Sign.Zer
        case (Sign.Pos, Sign.Pos)   => Sign.Pos
        case  _                     => Sign.Top
    }

    /**
     * Over-approximates integer `division`.
     */
    @strict @monotone
    fn divide(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Sign.Bot, _)          => Sign.Bot
        case (_, Sign.Bot)          => Sign.Bot
        case (Sign.Neg, Sign.Neg)   => Sign.Pos
        case (Sign.Neg, Sign.Zer)   => Sign.Bot
        case (Sign.Neg, Sign.Pos)   => Sign.Neg
        case (Sign.Zer, Sign.Neg)   => Sign.Zer
        case (Sign.Zer, Sign.Zer)   => Sign.Bot
        case (Sign.Zer, Sign.Pos)   => Sign.Zer
        case (Sign.Pos, Sign.Neg)   => Sign.Neg
        case (Sign.Pos, Sign.Zer)   => Sign.Bot
        case (Sign.Pos, Sign.Pos)   => Sign.Pos
        case  _                     => Sign.Top
    }

    /**
     * Over-approximates integer `modulus`.
     */
    @strict @monotone
    fn divide(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Sign.Bot, _)          => Sign.Bot
        case (_, Sign.Bot)          => Sign.Bot
        case (Sign.Neg, Sign.Neg)   => Sign.Bot
        case (Sign.Neg, Sign.Zer)   => Sign.Bot
        case (Sign.Neg, Sign.Pos)   => Sign.Top
        case (Sign.Zer, Sign.Neg)   => Sign.Bot
        case (Sign.Zer, Sign.Zer)   => Sign.Bot
        case (Sign.Zer, Sign.Pos)   => Sign.Pos
        case (Sign.Pos, Sign.Neg)   => Sign.Top
        case (Sign.Pos, Sign.Zer)   => Sign.Bot
        case (Sign.Pos, Sign.Pos)   => Sign.Top
        case  _                     => Sign.Top
    }

    /**
     * Over-approximates `bitwise negation`.
     */
    @strict @monotone
    fn negate(e: Sign): Sign = match e with {
        case Sign.Bot => Sign.Bot
        case Sign.Zer => Sign.Neg
        case Sign.Neg => Sign.Top
        case Sign.Pos => Sign.Top
        case Sign.Top => Sign.Top
    }

    /**
      * Over-approximates `bitwise or`.
      */
    @strict @monotone @commutative @associative
    fn or(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Sign.Bot, _)          => Sign.Bot
        case (_, Sign.Bot)          => Sign.Bot
        case (Sign.Neg, Sign.Neg)   => Sign.Neg
        case (Sign.Neg, Sign.Zer)   => Sign.Neg
        case (Sign.Neg, Sign.Pos)   => Sign.Neg
        case (Sign.Zer, Sign.Neg)   => Sign.Zer
        case (Sign.Zer, Sign.Zer)   => Sign.Zer
        case (Sign.Zer, Sign.Pos)   => Sign.Zer
        case (Sign.Pos, Sign.Neg)   => Sign.Neg
        case (Sign.Pos, Sign.Zer)   => Sign.Pos
        case (Sign.Pos, Sign.Pos)   => Sign.Pos
        case  _                     => Sign.Top
    }

    /**
      * Over-approximates `bitwise and`.
      */
    @strict @monotone @commutative @associative
    fn and(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Sign.Bot, _)          => Sign.Bot
        case (_, Sign.Bot)          => Sign.Bot
        case (Sign.Neg, Sign.Neg)   => Sign.Neg
        case (Sign.Neg, Sign.Zer)   => Sign.Zer
        case (Sign.Neg, Sign.Pos)   => Sign.Pos
        case (Sign.Zer, Sign.Neg)   => Sign.Zer
        case (Sign.Zer, Sign.Zer)   => Sign.Zer
        case (Sign.Zer, Sign.Pos)   => Sign.Zer
        case (Sign.Pos, Sign.Neg)   => Sign.Pos
        case (Sign.Pos, Sign.Zer)   => Sign.Zer
        case (Sign.Pos, Sign.Pos)   => Sign.Pos
        case  _                     => Sign.Top
    }

    /**
      * Over-approximates `bitwise xor`.
      */
    @strict @monotone @commutative @associative
    fn xor(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Sign.Bot, _)          => Sign.Bot
        case (_, Sign.Bot)          => Sign.Bot
        case (Sign.Neg, Sign.Neg)   => Sign.Pos
        case (Sign.Neg, Sign.Zer)   => Sign.Neg
        case (Sign.Neg, Sign.Pos)   => Sign.Neg
        case (Sign.Zer, Sign.Neg)   => Sign.Neg
        case (Sign.Zer, Sign.Zer)   => Sign.Zer
        case (Sign.Zer, Sign.Pos)   => Sign.Pos
        case (Sign.Pos, Sign.Neg)   => Sign.Neg
        case (Sign.Pos, Sign.Zer)   => Sign.Pos
        case (Sign.Pos, Sign.Pos)   => Sign.Pos
        case  _                     => Sign.Top
    }



}
