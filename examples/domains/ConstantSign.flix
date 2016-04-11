/**
 * An implementation of the sign domain lifted over the constant propagation domain.
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
        case (ConstSign.Cst(n), ConstSign.Neg)          => n <= 0
        case (ConstSign.Cst(n), ConstSign.Pos)          => n >= 0
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
        case (ConstSign.Bot, x)                 => x
        case (x, ConstSign.Bot)                 => x

        case (ConstSign.Cst(n1), ConstSign.Cst(n2)) => switch {
            case n1 == n2                       => ConstSign.Cst(n1)
            case n1 >= 0 && n2 >= 0             => ConstSign.Pos
            case n1 <= 0 && n2 <= 0             => ConstSign.Neg
            case true                           => ConstSign.Top
        }

        case (ConstSign.Cst(n), ConstSign.Neg) => if (n <= 0) ConstSign.Neg else ConstSign.Top
        case (ConstSign.Neg, ConstSign.Cst(n)) => if (n <= 0) ConstSign.Neg else ConstSign.Top
        case (ConstSign.Cst(n), ConstSign.Pos) => if (n >= 0) ConstSign.Pos else ConstSign.Top
        case (ConstSign.Pos, ConstSign.Cst(n)) => if (n >= 0) ConstSign.Pos else ConstSign.Top

        case (ConstSign.Neg, ConstSign.Neg)    => ConstSign.Neg
        case (ConstSign.Neg, ConstSign.Pos)    => ConstSign.Top
        case (ConstSign.Pos, ConstSign.Neg)    => ConstSign.Top
        case (ConstSign.Pos, ConstSign.Pos)    => ConstSign.Pos

        case _                                 => ConstSign.Top
    }

    /**
     * Returns the greatest lower bound of `e1` and `e2`.
     */
    @commutative @associative
    fn glb(e1: ConstSign, e2: ConstSign): ConstSign = match (e1, e2) with {
        case (ConstSign.Top, x)                         => x
        case (x, ConstSign.Top)                         => x

        case (ConstSign.Neg, ConstSign.Neg)             => ConstSign.Neg
        case (ConstSign.Neg, ConstSign.Cst(n))          => if (n <= 0) ConstSign.Cst(n) else ConstSign.Bot
        case (ConstSign.Cst(n), ConstSign.Neg)          => if (n <= 0) ConstSign.Cst(n) else ConstSign.Bot

        case (ConstSign.Pos, ConstSign.Pos)             => ConstSign.Pos
        case (ConstSign.Pos, ConstSign.Cst(n))          => if (n >= 0) ConstSign.Cst(n) else ConstSign.Bot
        case (ConstSign.Cst(n), ConstSign.Pos)          => if (n >= 0) ConstSign.Cst(n) else ConstSign.Bot

        case (ConstSign.Neg, ConstSign.Pos)             => ConstSign.Cst(0)
        case (ConstSign.Pos, ConstSign.Neg)             => ConstSign.Cst(0)

        case (ConstSign.Cst(n1), ConstSign.Cst(n2)) =>
            if (n1 == n2) ConstSign.Cst(n1) else ConstSign.Bot

        case _ => ConstSign.Bot
    }

    // TODO: Inc/dec



    // TODO: Issues with overflow?

//    /**
//     * Over-approximates integer `addition`.
//     */
//    @strict @monotone @commutative @associative
//    fn plus(e1: ConstSign, e2: ConstSign): ConstSign = match (e1, e2) with {
//        case (ConstSign.Bot, _)                         => ConstSign.Bot
//        case (_, ConstSign.Bot)                         => ConstSign.Bot
//        case (ConstSign.Cst(n1), ConstSign.Cst(n2))     => ConstSign.Cst(n1 + n2)
//        case (ConstSign.Cst(n1), ConstSign.Neg)         => if (n1 <= 0) ConstSign.Neg else ConstSign.Top
//        case (ConstSign.Neg, ConstSign.Cst(n1))         => if (n1 <= 0) ConstSign.Neg else ConstSign.Top
//        case (ConstSign.Cst(n1), ConstSign.Pos)         => if (n1 >= 0) ConstSign.Pos else ConstSign.Top
//        case (ConstSign.Pos, ConstSign.Cst(n1))         => if (n1 >= 0) ConstSign.Pos else ConstSign.Top
//        case (ConstSign.Neg, ConstSign.Neg)             => ConstSign.Neg
//        case (ConstSign.Pos, ConstSign.Pos)             => ConstSign.Pos
//        case _                                          => ConstSign.Top
//    }

//    /**
//     * Over-approximates integer `subtraction`.
//     */
//    @strict @monotone
//    fn minus(e1: ConstSign, e2: ConstSign): ConstSign = match (e1, e2) with {
//        case (ConstSign.Bot, _)                         => ConstSign.Bot
//        case (_, ConstSign.Bot)                         => ConstSign.Bot
//        case (ConstSign.Cst(n1), ConstSign.Cst(n2))     => ConstSign.Cst(n1 - n2)
//        case (ConstSign.Cst(n1), ConstSign.Neg)         => if (n1 >= 0) ConstSign.Pos else ConstSign.Top
//        case (ConstSign.Neg, ConstSign.Cst(n1))         => if (n1 >= 0) ConstSign.Neg else ConstSign.Top
//        case (ConstSign.Cst(n1), ConstSign.Pos)         => if (n1 <= 0) ConstSign.Neg else ConstSign.Top
//        case (ConstSign.Pos, ConstSign.Cst(n1))         => if (n1 <= 0) ConstSign.Pos else ConstSign.Top
//        case (ConstSign.Pos, ConstSign.Neg)             => ConstSign.Pos
//        case (ConstSign.Neg, ConstSign.Pos)             => ConstSign.Neg
//        case _                                          => ConstSign.Top
//    }
//
//    /**
//     * Over-approximates integer `multiplication`.
//     */
//    @strict @monotone @commutative @associative
//    fn times(e1: ConstSign, e2: ConstSign): ConstSign = match (e1, e2) with {
//        case (ConstSign.Bot, _)                         => ConstSign.Bot
//        case (_, ConstSign.Bot)                         => ConstSign.Bot
//        case (ConstSign.Cst(n1), ConstSign.Cst(n2))     => ConstSign.Cst(n1 * n2)
//        case (ConstSign.Cst(0), _)                      => ConstSign.Cst(0)
//        case (_, ConstSign.Cst(0))                      => ConstSign.Cst(0)
//        case (ConstSign.Cst(n1), ConstSign.Neg)         => if (n1 > 0) ConstSign.Neg else ConstSign.Pos
//        case (ConstSign.Neg, ConstSign.Cst(n1))         => if (n1 > 0) ConstSign.Neg else ConstSign.Pos
//        case (ConstSign.Cst(n1), ConstSign.Pos)         => if (n1 < 0) ConstSign.Neg else ConstSign.Pos
//        case (ConstSign.Pos, ConstSign.Cst(n1))         => if (n1 < 0) ConstSign.Neg else ConstSign.Pos
//        case (ConstSign.Neg, ConstSign.Neg)             => ConstSign.Pos
//        case (ConstSign.Pos, ConstSign.Neg)             => ConstSign.Neg
//        case (ConstSign.Neg, ConstSign.Pos)             => ConstSign.Neg
//        case (ConstSign.Pos, ConstSign.Pos)             => ConstSign.Pos
//        case _                                          => ConstSign.Top
//    }
//
//    /**
//     * Over-approximates integer `division`.
//     */
//    @strict @monotone
//    fn divide(e1: ConstSign, e2: ConstSign): ConstSign = match (e1, e2) with {
//        case (ConstSign.Bot, _)                         => ConstSign.Bot
//        case (_, ConstSign.Bot)                         => ConstSign.Bot
//        case (_, ConstSign.Cst(0))                      => ConstSign.Bot
//        case (ConstSign.Cst(n1), ConstSign.Cst(n2))     => ConstSign.Cst(n1 / n2)
//        case (ConstSign.Cst(n1), ConstSign.Neg)         => if (n1 > 0) ConstSign.Neg else ConstSign.Pos
//        case (ConstSign.Neg, ConstSign.Cst(n1))         => if (n1 > 0) ConstSign.Neg else ConstSign.Pos
//        case (ConstSign.Cst(n1), ConstSign.Pos)         => if (n1 < 0) ConstSign.Neg else ConstSign.Pos
//        case (ConstSign.Pos, ConstSign.Cst(n1))         => if (n1 < 0) ConstSign.Neg else ConstSign.Pos
//        case (ConstSign.Neg, ConstSign.Neg)             => ConstSign.Pos
//        case (ConstSign.Pos, ConstSign.Neg)             => ConstSign.Neg
//        case (ConstSign.Neg, ConstSign.Pos)             => ConstSign.Neg
//        case (ConstSign.Pos, ConstSign.Pos)             => ConstSign.Pos
//        case _                                          => ConstSign.Top
//    }
//
//    /**
//     * Over-approximates integer `modulus`.
//     */
//    @strict @monotone
//    fn modulo(e1: ConstSign, e2: ConstSign): ConstSign = match (e1, e2) with {
//        case (ConstSign.Bot, _)                         => ConstSign.Bot
//        case (_, ConstSign.Bot)                         => ConstSign.Bot
//        case (_, ConstSign.Neg)                         => ConstSign.Bot
//        case (_, ConstSign.Cst(0))                      => ConstSign.Bot
//        case (ConstSign.Cst(n1), ConstSign.Cst(n2))     => ConstSign.Cst(n1 % n2)
//        case (ConstSign.Neg, ConstSign.Cst(n1))         => if (n1 > 0) ConstSign.Neg else ConstSign.Bot
//        case (ConstSign.Cst(n1), ConstSign.Pos)         => if (n1 < 0) ConstSign.Neg else ConstSign.Top
//        case (ConstSign.Pos, ConstSign.Cst(n1))         => if (n1 < 0) ConstSign.Neg else ConstSign.Top
//        case (ConstSign.Neg, ConstSign.Pos)             => ConstSign.Top
//        case (ConstSign.Pos, ConstSign.Pos)             => ConstSign.Top
//        case _                                          => ConstSign.Top
//    }
//
//    /**
//     * Over-approximates `bitwise negation`.
//     */
//    @strict @monotone
//    fn negate(e: ConstSign): ConstSign = match e with {
//        case ConstSign.Bot      => ConstSign.Bot
//        case ConstSign.Cst(n)   => ConstSign.Cst(~ n)
//        case ConstSign.Neg      => ConstSign.Pos
//        case ConstSign.Pos      => ConstSign.Neg
//        case ConstSign.Top      => ConstSign.Top
//    }
//
//    /**
//      * Over-approximates `bitwise or`.
//      */
//    @strict @monotone @commutative @associative
//    fn or(e1: ConstSign, e2: ConstSign): ConstSign = match (e1, e2) with {
//        case (ConstSign.Bot, _)                         => ConstSign.Bot
//        case (_, ConstSign.Bot)                         => ConstSign.Bot
//        case (ConstSign.Cst(n1), ConstSign.Cst(n2))     => ConstSign.Cst(n1 | n2)
//        case (ConstSign.Cst(n1), ConstSign.Neg)         => ConstSign.Neg
//        case (ConstSign.Neg, ConstSign.Cst(n1))         => ConstSign.Neg
//        case (ConstSign.Cst(n1), ConstSign.Pos)         => if (n1 < 0) ConstSign.Neg else ConstSign.Pos
//        case (ConstSign.Pos, ConstSign.Cst(n1))         => if (n1 < 0) ConstSign.Neg else ConstSign.Pos
//        case (ConstSign.Neg, ConstSign.Neg)             => ConstSign.Neg
//        case (ConstSign.Neg, ConstSign.Pos)             => ConstSign.Neg
//        case (ConstSign.Pos, ConstSign.Neg)             => ConstSign.Neg
//        case (ConstSign.Pos, ConstSign.Pos)             => ConstSign.Pos
//        case _                                          => ConstSign.Top
//    }
//
//    /**
//      * Over-approximates `bitwise and`.
//      */
//    @strict @monotone @commutative @associative
//    fn and(e1: ConstSign, e2: ConstSign): ConstSign = match (e1, e2) with {
//        case (ConstSign.Bot, _)                         => ConstSign.Bot
//        case (_, ConstSign.Bot)                         => ConstSign.Bot
//        case (ConstSign.Cst(n1), ConstSign.Cst(n2))     => ConstSign.Cst(n1 & n2)
//        case (x, ConstSign.Pos)                         => ConstSign.Pos
//        case (ConstSign.Pos, x)                         => ConstSign.Pos
//        case (ConstSign.Cst(n1), ConstSign.Neg)         => if (n1 < 0) ConstSign.Neg else ConstSign.Pos
//        case (ConstSign.Neg, ConstSign.Cst(n1))         => if (n1 < 0) ConstSign.Neg else ConstSign.Pos
//        case (ConstSign.Neg, ConstSign.Neg)             => ConstSign.Neg
//        case (ConstSign.Neg, ConstSign.Pos)             => ConstSign.Pos
//        case (ConstSign.Pos, ConstSign.Neg)             => ConstSign.Pos
//        case (ConstSign.Pos, ConstSign.Pos)             => ConstSign.Pos
//        case _                                          => ConstSign.Top
//    }
//
//    /**
//      * Over-approximates `bitwise xor`.
//      */
//    @strict @monotone @commutative @associative
//    fn xor(e1: ConstSign, e2: ConstSign): ConstSign = match (e1, e2) with {
//        case (ConstSign.Bot, _)                         => ConstSign.Bot
//        case (_, ConstSign.Bot)                         => ConstSign.Bot
//        case (ConstSign.Cst(n1), ConstSign.Cst(n2))     => ConstSign.Cst(n1 ^ n2)
//        case (ConstSign.Cst(n1), ConstSign.Neg)         => if (n1 >= 0) ConstSign.Neg else ConstSign.Pos
//        case (ConstSign.Neg, ConstSign.Cst(n1))         => if (n1 >= 0) ConstSign.Neg else ConstSign.Pos
//        case (ConstSign.Cst(n1), ConstSign.Pos)         => if (n1 < 0) ConstSign.Neg else ConstSign.Pos
//        case (ConstSign.Pos, ConstSign.Cst(n1))         => if (n1 < 0) ConstSign.Neg else ConstSign.Pos
//        case (ConstSign.Neg, ConstSign.Neg)             => ConstSign.Pos
//        case (ConstSign.Neg, ConstSign.Pos)             => ConstSign.Neg
//        case (ConstSign.Pos, ConstSign.Neg)             => ConstSign.Neg
//        case (ConstSign.Pos, ConstSign.Pos)             => ConstSign.Pos
//        case _                                          => ConstSign.Top
//    }
//
//    /**
//      * Over-approximates `bitwise left shift`.
//      */
//    @strict @monotone
//    fn leftShift(e1: ConstSign, e2: ConstSign): ConstSign = match (e1, e2) with {
//        case (ConstSign.Bot, _)                         => ConstSign.Bot
//        case (_, ConstSign.Bot)                         => ConstSign.Bot
//        case (_, ConstSign.Neg)                         => ConstSign.Bot
//        case (ConstSign.Cst(0), _)                      => ConstSign.Cst(0)
//        case (x, ConstSign.Cst(0))                      => x
//        case (ConstSign.Cst(n1), ConstSign.Cst(n2))     => ConstSign.Cst(n1 << n2)
//        case _                                          => ConstSign.Top
//    }
//
//    /**
//      * Over-approximates `bitwise right shift`.
//      */
//    @strict @monotone
//    fn rightShift(e1: ConstSign, e2: ConstSign): ConstSign = match (e1, e2) with {
//        case (ConstSign.Bot, _)                         => ConstSign.Bot
//        case (_, ConstSign.Bot)                         => ConstSign.Bot
//        case (_, ConstSign.Neg)                         => ConstSign.Bot
//        case (ConstSign.Cst(0), _)                      => ConstSign.Cst(0)
//        case (x, ConstSign.Cst(0))                      => x
//        case (ConstSign.Cst(n1), ConstSign.Cst(n2))     => ConstSign.Cst(n1 >> n2)
//        case (ConstSign.Neg, ConstSign.Cst(n1))         => if (n1 > 0) ConstSign.Pos else ConstSign.Bot
//        case (ConstSign.Pos, _)                         => ConstSign.Pos
//        case _                                          => ConstSign.Top
//    }
//
//    /**
//      * Over-approximates `equal`.
//      */
//    @strict @monotone @commutative
//    fn eq(e1: ConstSign, e2: ConstSign): Belnap/Belnap = match (e1, e2) with {
//        case (ConstSign.Bot, _)                         => Belnap/Belnap.Bot
//        case (_, ConstSign.Bot)                         => Belnap/Belnap.Bot
//        case (ConstSign.Cst(n1), ConstSign.Cst(n2))     => Belnap/alpha(n1 == n2)
//        case (ConstSign.Cst(n1), ConstSign.Neg)         => if (n1 > 0) Belnap/Belnap.False else Belnap/Belnap.Top
//        case (ConstSign.Neg, ConstSign.Cst(n1))         => if (n1 > 0) Belnap/Belnap.False else Belnap/Belnap.Top
//        case (ConstSign.Cst(n1), ConstSign.Pos)         => if (n1 < 0) Belnap/Belnap.False else Belnap/Belnap.Top
//        case (ConstSign.Pos, ConstSign.Cst(n1))         => if (n1 < 0) Belnap/Belnap.False else Belnap/Belnap.Top
//        case _                                          => Belnap/Belnap.Top
//    }
//
//    /**
//      * Over-approximates `not equal`.
//      */
//    @strict @monotone @commutative
//    fn neq(e1: ConstSign, e2: ConstSign): Belnap/Belnap = Belnap/not(eq(e1, e2))
//
//    /**
//      * Over-approximates `less than`.
//      */
//    @strict @monotone
//    fn less(e1: ConstSign, e2: ConstSign): Belnap/Belnap = match (e1, e2) with {
//        case (ConstSign.Bot, _)                         => Belnap/Belnap.Bot
//        case (_, ConstSign.Bot)                         => Belnap/Belnap.Bot
//        case (ConstSign.Cst(n1), ConstSign.Cst(n2))     => Belnap/alpha(n1 < n2)
//        case (ConstSign.Cst(n1), ConstSign.Neg)         => if (n1 > 0) Belnap/Belnap.False else Belnap/Belnap.Top
//        case (ConstSign.Neg, ConstSign.Cst(n1))         => if (n1 > 0) Belnap/Belnap.True else Belnap/Belnap.Top
//        case (ConstSign.Cst(n1), ConstSign.Pos)         => if (n1 < 0) Belnap/Belnap.True else Belnap/Belnap.Top
//        case (ConstSign.Pos, ConstSign.Cst(n1))         => if (n1 < 0) Belnap/Belnap.False else Belnap/Belnap.Top
//        case _                                          => Belnap/Belnap.Top
//    }
//
//    /**
//      * Over-approximates `less than or equal`.
//      */
//    @strict @monotone
//    fn lessEqual(e1: ConstSign, e2: ConstSign): Belnap/Belnap = Belnap/or(e1 `less` e2, e1 `eq` e2)
//
    let ConstSign<> = (ConstSign.Bot, ConstSign.Top, leq, lub, glb)

}