/**
 * An implementation of the interval domain.
 *
 * This implementation uses an explicit bottom and top element.
 *
 * See Cousot & Cousot '77 for details.
 */
namespace Interval {

    /**
     * The elements of the interval lattice.
     */
    enum Interval {
            case Top,

        case Range(Int, Int)/*TODO: refinement type [x > y]*/,

            case Bot
    }

    /**
     * Returns `true` iff `e1` is less than or equal to `e2`.
     */
    fn leq(e1: Interval, e2: Interval): Bool = match (e1, e2) with {
        case (Interval.Bot, _)                                  => true
        case (Interval.Range(b1, e1), Interval.Range(b2, e2))   =>
            b2 <= b1 && e1 <= e2
        case (_, Interval.Top)                                  => true
        case _                                                  => false
    }

    /**
     * Returns the least upper bound of `e1` and `e2`.
     */
    @commutative @associative
    fn lub(e1: Interval, e2: Interval): Interval = match (e1, e2) with {
        case (Interval.Bot, _)                                  => Interval.Bot
        case (_, Interval.Bot)                                  => Interval.Bot
        case (Interval.Range(b1, e1), Interval.Range(b2, e2))   =>
            Interval.Range(Math/min(b1, b2), Math/max(e1, e2))
        case _                                                  => Interval.Top
    }

    // TODO: Widen
    // TODO: Narrow

    /**
     * Returns the greatest lower bound of `e1` and `e2`.
     */
    @commutative @associative
    fn glb(e1: Interval, e2: Interval): Interval = match (e1, e2) with {
        case (Interval.Top, x)                                  => x
        case (x, Interval.Top)                                  => x
        case (Interval.Range(b1, e1), Interval.Range(b2, e2))   =>
            let b = Math/max(b1, b2) in
            let e = Math/min(e1, e2) in
                if (b <= e)
                    Interval.Range(b, e)
                else
                    Interval.Bot
        case _                                                  => Interval.Bot
    }

    /**
     * Abstracts a concrete integer into the interval domain.
     */
    fn alpha(i: Int): Interval = Interval.Range(i, i)

    /**
     * Over-approximates integer `addition`.
     */
    @strict @monotone @commutative @associative
    fn plus(e1: Interval, e2: Interval): Interval = match (e1, e2) with {
        case (Interval.Bot, _)                                  => Interval.Bot
        case (_, Interval.Bot)                                  => Interval.Bot
        case (Interval.Range(b1, e1), Interval.Range(b2, e2))   =>
            Interval.Range(b1 + b2, e1 + e2)
        case _                                                  => Interval.Top
    }

    /**
     * Over-approximates integer `subtraction`.
     */
    @strict @monotone
    fn minus(e1: Interval, e2: Interval): Interval = match (e1, e2) with {
        case (Interval.Bot, _)                                  => Interval.Bot
        case (_, Interval.Bot)                                  => Interval.Bot
        case (Interval.Range(b1, e1), Interval.Range(b2, e2))   =>
            Interval.Range(b1 - e2, e1 - b2)
        case _                                                  => Interval.Top
    }

    /**
     * Over-approximates integer `multiplication`.
     */
    @strict @monotone @commutative @associative
    fn times(e1: Interval, e2: Interval): Interval = match (e1, e2) with {
        case (Interval.Bot, _)                                  => Interval.Bot
        case (_, Interval.Bot)                                  => Interval.Bot
        case (Interval.Range(b1, e1), Interval.Range(b2, e2))   =>
            Interval.Range(b1 * b2, e1 * e2)
        case _                                                  => Interval.Top
    }

    /**
     * Over-approximates integer `division`.
     */
    @strict @monotone @commutative @associative
    fn divide(e1: Interval, e2: Interval): Interval = match (e1, e2) with {
        case (Interval.Bot, _)                                  => Interval.Bot
        case (_, Interval.Bot)                                  => Interval.Bot
        case (_, Interval.Range(0, 0))                          => Interval.Bot
        case (Interval.Range(b1, e1), Interval.Range(b2, e2))   =>
            if (b2 <= 0 && 0 <= e2)
                Interval.Top
            else
                Interval.Range(b1 / e2, e1 / b2)
        case _                                                  => Interval.Top
    }

//    /**
//     * Over-approximates `bitwise negation`.
//     */
//    @strict @monotone
//    fn negate(e: Interval): Interval = match e with {
//        case Interval.Bot           => Interval.Bot
//        case Interval.Range(b, e)   =>
//            ???: Interval // TODO
//        case Interval.Top           => Interval.Top
//    }

    // TODO, or, and, xor, leftShift, rightShift.

    /**
      * Over-approximates `equal`.
      */
    @strict @monotone @commutative
    fn eq(e1: Interval, e2: Interval): Belnap/Belnap = match (e1, e2) with {
        case (Interval.Bot, _)                                  => Belnap/Belnap.Bot
        case (_, Interval.Bot)                                  => Belnap/Belnap.Bot
        case (Interval.Range(b1, e1), Interval.Range(b2, e2))   => switch {
            case (b1 == e1) && ((e1 == b2) && (b2 == e2))       => Belnap/Belnap.True
            case (e1 < b2) || (e2 < b1)                         => Belnap/Belnap.False
            case true                                           => Belnap/Belnap.Top
        }
        case _                                                  => Belnap/Belnap.Top
    }

    // TODO: Consider moving neq and lessEq.

    /**
      * Over-approximates `not equal`.
      */
    @strict @monotone @commutative
    fn neq(e1: Interval, e2: Interval): Belnap/Belnap = Belnap/not(eq(e1, e2))

    /**
      * Over-approximates `less than`.
      */
    @strict @monotone
    fn less(e1: Interval, e2: Interval): Belnap/Belnap = match (e1, e2) with {
        case (Interval.Bot, _)                                  => Belnap/Belnap.Bot
        case (_, Interval.Bot)                                  => Belnap/Belnap.Bot
        case (Interval.Range(b1, e1), Interval.Range(b2, e2))   =>
            if (e1 < b2)
                Belnap/Belnap.True
            else
                if (e2 < b1)
                    Belnap/Belnap.False
                else
                    Belnap/Belnap.Top

        case _                                                  => Belnap/Belnap.Top
    }

    /**
      * Over-approximates `less than or equal`.
      */
    @strict @monotone
    fn lessEqual(e1: Interval, e2: Interval): Belnap/Belnap = Belnap/or(e1 `less` e2, e1 `eq` e2)

    let Interval<> = (Interval.Bot, Interval.Top, leq, lub, glb)

}

// TODO: Import

// TODO
namespace Math {

    /**
     * Returns the smallest of `i1` and `i2`.
     */
    fn min(i1: Int, i2: Int): Int = if (i1 <= i2) i1 else i2

    /**
     * Returns the greatest of `i1` and `i2`.
     */
    fn max(i1: Int, i2: Int): Int = if (i1 >= i2) i1 else i2

}