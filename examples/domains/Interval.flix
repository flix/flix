/**
 * An implementation of the interval domain.
 *
 * See Cousot & Cousot '77 for details.
 */
namespace Interval {

    /**
     * The elements of the interval lattice.
     */
    enum Interval {
                 case Top,

        case Range(BigInt, BigInt)
    }

    /**
     * Returns the normalized element corresponding to the given interval `i`.
     *
     * Effectively turns the pre-order into a partial order.
     *
     * This function must be manually called until we support pre-orders natively.
     */
    def norm(i: Interval): Interval = match i with {
        case Interval.Range(b, e)   => if (b > e) Interval.Range(1ii, -1ii) else Interval.Range(b, e)
        case Interval.Top           => Interval.Top
    }

    /**
     * Returns `true` iff `e1` is less than or equal to `e2`.
     */
    def leq(e1: Interval, e2: Interval): Bool = match (e1, e2) with {
        case (Interval.Range(1ii, -1ii), _)                     => true
        case (Interval.Range(b1, e1), Interval.Range(b2, e2))   => b2 <= b1 && e1 <= e2
        case (_, Interval.Top)                                  => true
        case _                                                  => false
    }

    /**
     * Returns the least upper bound of `e1` and `e2`.
     */
    @commutative @associative
    def lub(e1: Interval, e2: Interval): Interval = match (e1, e2) with {
        case (Interval.Range(1ii, -1ii), x)                     => x
        case (x, Interval.Range(1ii, -1ii))                     => x
        case (Interval.Range(b1, e1), Interval.Range(b2, e2))   => Interval.Range(Math/min(b1, b2), Math/max(e1, e2))
        case _                                                  => Interval.Top
    }

    /**
     * Returns the greatest lower bound of `e1` and `e2`.
     */
    @commutative @associative
    def glb(e1: Interval, e2: Interval): Interval = match (e1, e2) with {
        case (Interval.Top, x)                                  => x
        case (x, Interval.Top)                                  => x
        case (Interval.Range(b1, e1), Interval.Range(b2, e2))   =>
            let b = Math/max(b1, b2) in
            let e = Math/min(e1, e2) in
                if (b <= e)
                    Interval.Range(b, e)
                else
                    Interval.Range(1ii, -1ii)
        case _                                                  => Interval.Range(1ii, -1ii)
    }

    let Interval<> = (Interval.Range(1ii, -1ii), Interval.Top, leq, lub, glb)

}

namespace Math {
    /**
     * Returns the smallest of `i1` and `i2`.
     */
    fn min(i1: BigInt, i2: BigInt): BigInt = if (i1 <= i2) i1 else i2

    /**
     * Returns the greatest of `i1` and `i2`.
     */
    fn max(i1: BigInt, i2: BigInt): BigInt = if (i1 >= i2) i1 else i2
}