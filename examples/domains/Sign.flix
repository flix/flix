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
        case (Sign.Bot, _)          => true
        case (Sign.Neg, Sign.Neg)   => true
        case (Sign.Zer, Sign.Zer)   => true
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
        case (Sign.Neg, Sign.Neg)   => Sign.Neg
        case (Sign.Zer, Sign.Zer)   => Sign.Zer
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
        case (Sign.Neg, Sign.Neg)   => Sign.Neg
        case (Sign.Zer, Sign.Zer)   => Sign.Zer
        case (Sign.Pos, Sign.Pos)   => Sign.Pos
        case _                      => Sign.Bot
    }

    /**
     * Lifts a regular integer into the Sign domain.
     */
    fn lift(i: Int): Sign =
        if (i < 0) Sign.Neg else
            if (i == 0) Sign.Zer else
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
        case _                      => Sign.Top
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
        case _                      => Sign.Top
    }

    /**
     * Over-approximates integer `multiplication`.
     */
    @strict @monotone @commutative @associative
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
        case _                      => Sign.Top
    }

    /**
     * Over-approximates integer `division`.
     */
    @strict @monotone
    fn divide(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Sign.Bot, _)          => Sign.Bot
        case (_, Sign.Bot)          => Sign.Bot
        case (Sign.Neg, Sign.Neg)   => Sign.Top
        case (Sign.Neg, Sign.Zer)   => Sign.Bot
        case (Sign.Neg, Sign.Pos)   => Sign.Top
        case (Sign.Zer, Sign.Neg)   => Sign.Zer
        case (Sign.Zer, Sign.Zer)   => Sign.Zer
        case (Sign.Zer, Sign.Pos)   => Sign.Zer
        case (Sign.Pos, Sign.Neg)   => Sign.Top
        case (Sign.Pos, Sign.Zer)   => Sign.Bot
        case (Sign.Pos, Sign.Pos)   => Sign.Top
        case _                      => Sign.Top
    }

    /**
     * Over-approximates integer `modulus`.
     */
    @strict @monotone
    fn modulo(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Sign.Bot, _)          => Sign.Bot
        case (_, Sign.Bot)          => Sign.Bot
        case (Sign.Neg, Sign.Neg)   => Sign.Bot
        case (Sign.Neg, Sign.Zer)   => Sign.Bot
        case (Sign.Neg, Sign.Pos)   => Sign.Top
        case (Sign.Zer, Sign.Neg)   => Sign.Bot
        case (Sign.Zer, Sign.Zer)   => Sign.Bot
        case (Sign.Zer, Sign.Pos)   => Sign.Zer
        case (Sign.Pos, Sign.Neg)   => Sign.Bot
        case (Sign.Pos, Sign.Zer)   => Sign.Bot
        case (Sign.Pos, Sign.Pos)   => Sign.Top
        case _                      => Sign.Top
    }

    /**
     * Over-approximates `bitwise negation`.
     */
    @strict @monotone
    fn negate(e: Sign): Sign = match e with {
        case Sign.Bot => Sign.Bot
        case Sign.Neg => Sign.Pos
        case Sign.Zer => Sign.Zer
        case Sign.Pos => Sign.Neg
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
        case (Sign.Zer, Sign.Neg)   => Sign.Neg
        case (Sign.Zer, Sign.Zer)   => Sign.Zer
        case (Sign.Zer, Sign.Pos)   => Sign.Pos
        case (Sign.Pos, Sign.Neg)   => Sign.Neg
        case (Sign.Pos, Sign.Zer)   => Sign.Pos
        case (Sign.Pos, Sign.Pos)   => Sign.Pos
        case _                      => Sign.Top
    }

    /**
      * Over-approximates `bitwise and`.
      */
    @strict @monotone @commutative @associative
    fn and(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Sign.Bot, _)          => Sign.Bot
        case (_, Sign.Bot)          => Sign.Bot
        case (Sign.Neg, Sign.Neg)   => Sign.Top
        case (Sign.Neg, Sign.Zer)   => Sign.Zer
        case (Sign.Neg, Sign.Pos)   => Sign.Top
        case (Sign.Zer, Sign.Neg)   => Sign.Zer
        case (Sign.Zer, Sign.Zer)   => Sign.Zer
        case (Sign.Zer, Sign.Pos)   => Sign.Zer
        case (Sign.Pos, Sign.Neg)   => Sign.Top
        case (Sign.Pos, Sign.Zer)   => Sign.Zer
        case (Sign.Pos, Sign.Pos)   => Sign.Top
        case _                      => Sign.Top
    }

    /**
      * Over-approximates `bitwise xor`.
      */
    @strict @monotone @commutative @associative
    fn xor(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Sign.Bot, _)          => Sign.Bot
        case (_, Sign.Bot)          => Sign.Bot
        case (Sign.Neg, Sign.Neg)   => Sign.Top
        case (Sign.Neg, Sign.Zer)   => Sign.Neg
        case (Sign.Neg, Sign.Pos)   => Sign.Neg
        case (Sign.Zer, Sign.Neg)   => Sign.Neg
        case (Sign.Zer, Sign.Zer)   => Sign.Zer
        case (Sign.Zer, Sign.Pos)   => Sign.Pos
        case (Sign.Pos, Sign.Neg)   => Sign.Neg
        case (Sign.Pos, Sign.Zer)   => Sign.Pos
        case (Sign.Pos, Sign.Pos)   => Sign.Top
        case _                      => Sign.Top
    }

    /**
      * Over-approximates `bitwise left shift`.
      */
    @strict @monotone
    fn leftShift(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Sign.Bot, _)          => Sign.Bot
        case (_, Sign.Bot)          => Sign.Bot
        case (Sign.Neg, Sign.Neg)   => Sign.Bot
        case (Sign.Neg, Sign.Zer)   => Sign.Neg
        case (Sign.Neg, Sign.Pos)   => Sign.Top
        case (Sign.Zer, Sign.Neg)   => Sign.Bot
        case (Sign.Zer, Sign.Zer)   => Sign.Zer
        case (Sign.Zer, Sign.Pos)   => Sign.Zer
        case (Sign.Pos, Sign.Neg)   => Sign.Bot
        case (Sign.Pos, Sign.Zer)   => Sign.Pos
        case (Sign.Pos, Sign.Pos)   => Sign.Top
        case _                      => Sign.Top
    }

    /**
      * Over-approximates `bitwise right shift`.
      */
    @strict @monotone
    fn rightShift(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Sign.Bot, _)          => Sign.Bot
        case (_, Sign.Bot)          => Sign.Bot
        case (Sign.Neg, Sign.Neg)   => Sign.Bot
        case (Sign.Neg, Sign.Zer)   => Sign.Neg
        case (Sign.Neg, Sign.Pos)   => Sign.Pos
        case (Sign.Zer, Sign.Neg)   => Sign.Bot
        case (Sign.Zer, Sign.Zer)   => Sign.Zer
        case (Sign.Zer, Sign.Pos)   => Sign.Zer
        case (Sign.Pos, Sign.Neg)   => Sign.Bot
        case (Sign.Pos, Sign.Zer)   => Sign.Pos
        case (Sign.Pos, Sign.Pos)   => Sign.Top
        case _                      => Sign.Top
    }

    /**
      * Over-approximates `equal`.
      */
    @strict @monotone @commutative
    fn eq(e1: Sign, e2: Sign): Belnap::Belnap = match (e1, e2) with {
        case (Sign.Bot, _)          => Belnap::Belnap.Bot
        case (_, Sign.Bot)          => Belnap::Belnap.Bot
        case (Sign.Neg, Sign.Neg)   => Belnap::Belnap.Top
        case (Sign.Neg, Sign.Zer)   => Belnap::Belnap.False
        case (Sign.Neg, Sign.Pos)   => Belnap::Belnap.False
        case (Sign.Zer, Sign.Neg)   => Belnap::Belnap.False
        case (Sign.Zer, Sign.Zer)   => Belnap::Belnap.True
        case (Sign.Zer, Sign.Pos)   => Belnap::Belnap.False
        case (Sign.Pos, Sign.Neg)   => Belnap::Belnap.False
        case (Sign.Pos, Sign.Zer)   => Belnap::Belnap.False
        case (Sign.Pos, Sign.Pos)   => Belnap::Belnap.Top
        case _                      => Belnap::Belnap.Top
    }

    /**
      * Over-approximates `not equal`.
      */
    @strict @monotone @commutative
    fn neq(e1: Sign, e2: Sign): Belnap::Belnap = Belnap::not(eq(e1, e2))

    /**
      * Over-approximates `less than`.
      */
    @strict @monotone
    fn less(e1: Sign, e2: Sign): Belnap::Belnap = match (e1, e2) with {
        case (Sign.Bot, _)          => Belnap::Belnap.Bot
        case (_, Sign.Bot)          => Belnap::Belnap.Bot
        case (Sign.Neg, Sign.Zer)   => Belnap::Belnap.True
        case (Sign.Neg, Sign.Pos)   => Belnap::Belnap.True
        case (Sign.Zer, Sign.Pos)   => Belnap::Belnap.True
        case (Sign.Pos, Sign.Neg)   => Belnap::Belnap.False
        case (Sign.Pos, Sign.Zer)   => Belnap::Belnap.False
        case (Sign.Zer, Sign.Neg)   => Belnap::Belnap.False
        case _                      => Belnap::Belnap.Top
    }

    /**
      * Over-approximates `less than or equal`.
      */
    @strict @monotone
    fn lessEqual(e1: Sign, e2: Sign): Belnap::Belnap = Belnap::or(e1 `less` e2, e1 `eq` e2)

}
