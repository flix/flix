/**
 * An implementation of the (alternative) sign domain
 * lifted over the constant propagation domain.
 */
namespace ConstantSign {

    /**
     * The elements of the constant-sign lattice.
     */
    enum ConstSign {
                  case Top,

        case Neg,          case Pos,

                case Cst(Int),

                  case Bot
    }

    /**
     * Returns `true` iff `e1` is less than or equal to `e2`.
     */
    fn leq(e1: ConstSign, e2: ConstSign): Bool = match (e1, e2) with {
        case (ConstSign.Bot, _)                         => true
        case (ConstSign.Cst(n1), ConstSign.Cst(n2))     => n1 == n2
        case (ConstSign.Cst(n), ConstSign.Neg)          => n < 0
        case (ConstSign.Cst(n), ConstSign.Pos)          => n > 0
        case (ConstSign.Neg, ConstSign.Neg)             => true
        case (ConstSign.Pos, ConstSign.Pos)             => true
        case (_, ConstSign.Top)                         => true
        case _                                          => false
    }

    /**
     * Returns the least upper bound of `e1` and `e2`.
     */
    @commutative @associative
    fn lub(e1: ConstSign, e2: ConstSign): ConstSign = match (e1, e2) with {
        case (ConstSign.Bot, x)                         => x
        case (x, ConstSign.Bot)                         => x
        case (ConstSign.Cst(n1), ConstSign.Cst(n2)) =>
            if (n1 == n2)
                ConstSign.Cst(n1)
            else
               if (n1 > 0 && n2 > 0)
                 ConstSign.Pos
               else
                 if (n1 < 0 && n2 < 0)
                    ConstSign.Neg
                 else
                    ConstSign.Top

        case (ConstSign.Cst(n), ConstSign.Neg) =>
            if (n < 0) ConstSign.Neg else ConstSign.Top

        case (ConstSign.Neg, ConstSign.Cst(n)) =>
            if (n < 0) ConstSign.Neg else ConstSign.Top

        case (ConstSign.Cst(n), ConstSign.Pos) =>
            if (n > 0) ConstSign.Pos else ConstSign.Top

        case (ConstSign.Pos, ConstSign.Cst(n)) =>
            if (n > 0) ConstSign.Pos else ConstSign.Top

        case (ConstSign.Neg, ConstSign.Neg)            => ConstSign.Neg
        case (ConstSign.Neg, ConstSign.Pos)            => ConstSign.Top
        case (ConstSign.Pos, ConstSign.Neg)            => ConstSign.Top
        case (ConstSign.Pos, ConstSign.Pos)            => ConstSign.Pos

        case _                                          => ConstSign.Top
    }

    // TODO ...

}