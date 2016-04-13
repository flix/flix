/**
 * An example of a lattice with an infinite number of elements, but no ascending/descending chains.
 *
 * Figure 2.9, page 51.
 */
namespace InfNoAccDcc {

    enum L {
                  case Top,

              case Mid(Int, Int), // TODO: Should refine to only allow elements where y <= x.

                  case Bot
    }

    // Mid(x, y) identifies an element such that `x` is the
    // number of the "column" from the left and `y` is the
    // number of elements from the "bottom".

    /**
     * Returns `true` iff `e1` is less than or equal to `e2`.
     */
    def leq(e1: L, e2: L): Bool = match (e1, e2) with {
        case (L.Bot, _)                     => true
        case (L.Mid(x1, y1), L.Mid(x2, y2)) => x1 == x2 && y1 <= y2
        case (_, L.Top)                     => true
        case _                              => false
    }

    /**
     * Returns the least upper bound of `e1` and `e2`.
     */
    @commutative @associative
    def lub(e1: L, e2: L): L = match (e1, e2) with {
        case (L.Bot, x)                     => x
        case (x, L.Bot)                     => x
        case (L.Mid(x1, y1), L.Mid(x2, y2)) => switch {
            case (x1 == x2) => L.Mid(x1, Math/max(y1, y2))
            case true => L.Top
        }
        case _                              => L.Top
    }

    /**
     * Returns the greatest lower bound of `e1` and `e2`.
     */
    @commutative @associative
    def glb(e1: L, e2: L): L = match (e1, e2) with {
        case (L.Top, x)                     => x
        case (x, L.Top)                     => x
        case (L.Mid(x1, y1), L.Mid(x2, y2)) => switch {
            case (x1 == x2) => L.Mid(x1, Math/min(y1, y2))
            case true => L.Bot
        }
        case _                          => L.Bot
    }

    let L<> = (L.Bot, L.Top, leq, lub, glb)

}

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