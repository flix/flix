/**
 * An implementation of the constant propagation lattice.
 */
namespace Constant {

    /**
     * The elements of the constant propagation lattice.
     */
    enum Constant {
          case Top,

        case Cst(Int),

          case Bot
    }

    /**
     * Returns `true` iff `e1` is less than or equal to `e2`.
     */
    fn leq(e1: Constant, e2: Constant): Bool = match (e1, e2) with {
        case (Constant.Bot, _)                      => true
        case (Constant.Cst(n1), Constant.Cst(n2))   => n1 == n2
        case (_, Constant.Top)                      => true
        case _                                      => false
    }

    /**
     * Returns the least upper bound of `e1` and `e2`.
     */
    @commutative @associative
    fn lub(e1: Constant, e2: Constant): Constant = match (e1, e2) with {
        case (Constant.Bot, x)                      => x;
        case (x, Constant.Bot)                      => x;
        case (Constant.Cst(n1), Constant.Cst(n2))   => if (n1 == n2) e1 else Constant.Top
        case _                                      => Constant.Top;
    }

    /**
     * Returns the greatest lower bound of `e1` and `e2`.
     */
    @commutative @associative
    fn glb(e1: Constant, e2: Constant): Constant = match (e1, e2) with {
        case (Constant.Top, x)                      => x
        case (x, Constant.Top)                      => x
        case (Constant.Cst(n1), Constant.Cst(n2))   => if (n1 == n2) e1 else Constant.Bot
        case _                                      => Constant.Bot
    }

}