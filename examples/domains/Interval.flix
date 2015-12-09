/**
 * An implementation of the interval domain.
 *
 * This implementation uses an explicit bottom and top element.
 *
 * See Cousot & Cousot '77 for details.
 */
namespace Interval {

    /**
     * The elements of the boolean lattice.
     */
    enum Interval {
            case Top,

        case Range(Int, Int),

            case Bot
    }

    /**
     * Returns `true` iff `e1` is less than or equal to `e2`.
     */
    fn leq(e1: Interval, e2: Interval): Bool = match (e1, e2) with {
        case (Interval.Bot, _)                                  => true
        case (Interval.Range(b1, e1), Interval.Range(b2, e2))   =>
            b1 <= b2 && e1 <= e2
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
            Interval.Range(Math::min(b1, b2), Math::max(e1, e2))
        case _                                                  => Interval.Top
    }

    /**
     * Returns the greatest lower bound of `e1` and `e2`.
     */
    @commutative @associative
    fn glb(e1: Interval, e2: Interval): Interval = match (e1, e2) with {
        case (Interval.Top, x)                                  => x
        case (x, Interval.Top)                                  => x
        case (Interval.Range(b1, e1), Interval.Range(b2, e2))   =>
            let b = Math::max(b1, b2) in
            let e = Math::min(e1, e2) in
                if (b <= e)
                    Interval.Range(b, e)
                else
                    Interval.Bot
        case _                                                  => Interval.Bot
    }

    /**
     * Lifts a regular integer into the interval domain.
     */
    fn lift(i: Int): Interval = Interval.Range(i, i)


}

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