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

                case Cst(BigInt),

                  case Bot
    }

    /**
     * Returns `true` iff `e1` is less than or equal to `e2`.
     */
    def leq(e1: ConstSign, e2: ConstSign): Bool = match (e1, e2) with {
        case (ConstSign.Bot, _)                         => true
        case (ConstSign.Cst(n1), ConstSign.Cst(n2))     => n1 == n2
        case (ConstSign.Cst(n), ConstSign.Neg)          => n <= 0ii
        case (ConstSign.Cst(n), ConstSign.Pos)          => n >= 0ii
        case (ConstSign.Neg, ConstSign.Neg)             => true
        case (ConstSign.Pos, ConstSign.Pos)             => true
        case (_, ConstSign.Top)                         => true
        case _                                          => false
    }

    /**
     * Returns the least upper bound of `e1` and `e2`.
     */
    @commutative @associative
    def lub(e1: ConstSign, e2: ConstSign): ConstSign = match (e1, e2) with {
        case (ConstSign.Bot, x)                 => x
        case (x, ConstSign.Bot)                 => x
        case (ConstSign.Cst(n1), ConstSign.Cst(n2)) => switch {
            case n1 == n2                       => ConstSign.Cst(n1)
            case n1 >= 0ii && n2 >= 0ii         => ConstSign.Pos
            case n1 <= 0ii && n2 <= 0ii         => ConstSign.Neg
            case true                           => ConstSign.Top
        }
        case (ConstSign.Cst(n), ConstSign.Neg) => if (n <= 0ii) ConstSign.Neg else ConstSign.Top
        case (ConstSign.Neg, ConstSign.Cst(n)) => if (n <= 0ii) ConstSign.Neg else ConstSign.Top
        case (ConstSign.Cst(n), ConstSign.Pos) => if (n >= 0ii) ConstSign.Pos else ConstSign.Top
        case (ConstSign.Pos, ConstSign.Cst(n)) => if (n >= 0ii) ConstSign.Pos else ConstSign.Top
        case (ConstSign.Neg, ConstSign.Neg)    => ConstSign.Neg
        case (ConstSign.Pos, ConstSign.Pos)    => ConstSign.Pos
        case _                                 => ConstSign.Top
    }

    /**
     * Returns the greatest lower bound of `e1` and `e2`.
     */
    @commutative @associative
    def glb(e1: ConstSign, e2: ConstSign): ConstSign = match (e1, e2) with {
        case (ConstSign.Top, x)                         => x
        case (x, ConstSign.Top)                         => x
        case (ConstSign.Cst(n1), ConstSign.Cst(n2))     => if (n1 == n2) ConstSign.Cst(n1) else ConstSign.Bot
        case (ConstSign.Neg, ConstSign.Cst(n))          => if (n <= 0ii) ConstSign.Cst(n) else ConstSign.Bot
        case (ConstSign.Cst(n), ConstSign.Neg)          => if (n <= 0ii) ConstSign.Cst(n) else ConstSign.Bot
        case (ConstSign.Pos, ConstSign.Cst(n))          => if (n >= 0ii) ConstSign.Cst(n) else ConstSign.Bot
        case (ConstSign.Cst(n), ConstSign.Pos)          => if (n >= 0ii) ConstSign.Cst(n) else ConstSign.Bot
        case (ConstSign.Neg, ConstSign.Neg)             => ConstSign.Neg
        case (ConstSign.Pos, ConstSign.Pos)             => ConstSign.Pos
        case (ConstSign.Neg, ConstSign.Pos)             => ConstSign.Cst(0ii)
        case (ConstSign.Pos, ConstSign.Neg)             => ConstSign.Cst(0ii)
        case _ => ConstSign.Bot
    }

    /**
     * Over-approximates integer `increment`.
     */
    @strict @monotone
    def inc(e: ConstSign): ConstSign = match e with {
        case ConstSign.Bot      => ConstSign.Bot
        case ConstSign.Cst(n)   => ConstSign.Cst(n + 1ii)
        case ConstSign.Neg      => ConstSign.Top
        case ConstSign.Pos      => ConstSign.Pos
        case ConstSign.Top      => ConstSign.Top
    }

    /**
     * Over-approximates integer `decrement`.
     */
    @strict @monotone
    def dec(e: ConstSign): ConstSign = match e with {
        case ConstSign.Bot      => ConstSign.Bot
        case ConstSign.Cst(n)   => ConstSign.Cst(n - 1ii)
        case ConstSign.Neg      => ConstSign.Neg
        case ConstSign.Pos      => ConstSign.Top
        case ConstSign.Top      => ConstSign.Top
    }

    /**
     * Over-approximates integer `addition`.
     */
    @strict @monotone @commutative
    def plus(e1: ConstSign, e2: ConstSign): ConstSign = match (e1, e2) with {
        case (ConstSign.Bot, _)                         => ConstSign.Bot
        case (_, ConstSign.Bot)                         => ConstSign.Bot
        case (ConstSign.Cst(n1), ConstSign.Cst(n2))     => ConstSign.Cst(n1 + n2 : BigInt)
        case (ConstSign.Cst(n1), ConstSign.Neg)         => if (n1 <= 0ii) ConstSign.Neg else ConstSign.Top
        case (ConstSign.Neg, ConstSign.Cst(n1))         => if (n1 <= 0ii) ConstSign.Neg else ConstSign.Top
        case (ConstSign.Cst(n1), ConstSign.Pos)         => if (n1 >= 0ii) ConstSign.Pos else ConstSign.Top
        case (ConstSign.Pos, ConstSign.Cst(n1))         => if (n1 >= 0ii) ConstSign.Pos else ConstSign.Top
        case (ConstSign.Neg, ConstSign.Neg)             => ConstSign.Neg
        case (ConstSign.Pos, ConstSign.Pos)             => ConstSign.Pos
        case _                                          => ConstSign.Top
    }

    /**
     * Over-approximates integer `subtraction`.
     */
    @strict @monotone
    def minus(e1: ConstSign, e2: ConstSign): ConstSign = match (e1, e2) with {
        case (ConstSign.Bot, _)                         => ConstSign.Bot
        case (_, ConstSign.Bot)                         => ConstSign.Bot
        case (ConstSign.Cst(n1), ConstSign.Cst(n2))     => ConstSign.Cst(n1 - n2 : BigInt)
        case (ConstSign.Cst(n1), ConstSign.Neg)         => if (n1 >= 0ii) ConstSign.Pos else ConstSign.Top
        case (ConstSign.Neg, ConstSign.Cst(n1))         => if (n1 >= 0ii) ConstSign.Neg else ConstSign.Top
        case (ConstSign.Cst(n1), ConstSign.Pos)         => if (n1 <= 0ii) ConstSign.Neg else ConstSign.Top
        case (ConstSign.Pos, ConstSign.Cst(n1))         => if (n1 <= 0ii) ConstSign.Pos else ConstSign.Top
        case (ConstSign.Pos, ConstSign.Neg)             => ConstSign.Pos
        case (ConstSign.Neg, ConstSign.Pos)             => ConstSign.Neg
        case _                                          => ConstSign.Top
    }

    /**
     * Over-approximates integer `multiplication`.
     */
    @strict @monotone @commutative @associative
    def times(e1: ConstSign, e2: ConstSign): ConstSign = match (e1, e2) with {
        case (ConstSign.Bot, _)                         => ConstSign.Bot
        case (_, ConstSign.Bot)                         => ConstSign.Bot
        case (ConstSign.Cst(n1), ConstSign.Cst(n2))     => ConstSign.Cst(n1 * n2 : BigInt)
        case (ConstSign.Cst(0ii), _)                    => ConstSign.Cst(0ii)
        case (_, ConstSign.Cst(0ii))                    => ConstSign.Cst(0ii)
        case (ConstSign.Cst(n1), ConstSign.Neg)         => if (n1 > 0ii) ConstSign.Neg else ConstSign.Pos
        case (ConstSign.Neg, ConstSign.Cst(n1))         => if (n1 > 0ii) ConstSign.Neg else ConstSign.Pos
        case (ConstSign.Cst(n1), ConstSign.Pos)         => if (n1 < 0ii) ConstSign.Neg else ConstSign.Pos
        case (ConstSign.Pos, ConstSign.Cst(n1))         => if (n1 < 0ii) ConstSign.Neg else ConstSign.Pos
        case (ConstSign.Neg, ConstSign.Neg)             => ConstSign.Pos
        case (ConstSign.Pos, ConstSign.Neg)             => ConstSign.Neg
        case (ConstSign.Neg, ConstSign.Pos)             => ConstSign.Neg
        case (ConstSign.Pos, ConstSign.Pos)             => ConstSign.Pos
        case _                                          => ConstSign.Top
    }

    /**
     * Over-approximates integer `division`.
     */
    @strict @monotone
    def divide(e1: ConstSign, e2: ConstSign): ConstSign = match (e1, e2) with {
        case (ConstSign.Bot, _)                         => ConstSign.Bot
        case (_, ConstSign.Bot)                         => ConstSign.Bot
        case (_, ConstSign.Cst(0ii))                    => ConstSign.Bot
        case (ConstSign.Cst(0ii), _)                    => ConstSign.Cst(0ii)
        case (ConstSign.Cst(n1), ConstSign.Cst(n2))     => ConstSign.Cst(n1 / n2 : BigInt)
        case (ConstSign.Cst(n1), ConstSign.Neg)         => if (n1 > 0ii) ConstSign.Neg else ConstSign.Pos
        case (ConstSign.Neg, ConstSign.Cst(n1))         => if (n1 > 0ii) ConstSign.Neg else ConstSign.Pos
        case (ConstSign.Cst(n1), ConstSign.Pos)         => if (n1 < 0ii) ConstSign.Neg else ConstSign.Pos
        case (ConstSign.Pos, ConstSign.Cst(n1))         => if (n1 < 0ii) ConstSign.Neg else ConstSign.Pos
        case (ConstSign.Neg, ConstSign.Neg)             => ConstSign.Pos
        case (ConstSign.Pos, ConstSign.Neg)             => ConstSign.Neg
        case (ConstSign.Neg, ConstSign.Pos)             => ConstSign.Neg
        case (ConstSign.Pos, ConstSign.Pos)             => ConstSign.Pos
        case _                                          => ConstSign.Top
    }

    /**
     * Over-approximates integer `modulus`.
     */
    @strict @monotone
    def modulo(e1: ConstSign, e2: ConstSign): ConstSign = match (e1, e2) with {
        case (ConstSign.Bot, _)                         => ConstSign.Bot
        case (_, ConstSign.Bot)                         => ConstSign.Bot
        case (_, ConstSign.Cst(0ii))                    => ConstSign.Bot
        case (ConstSign.Cst(n1), ConstSign.Cst(n2))     => ConstSign.Cst(n1 % n2 : BigInt)
        case _                                          => ConstSign.Top
    }

    /**
      * Over-approximates `equal`.
      */
    @strict @monotone @commutative
    def eq(e1: ConstSign, e2: ConstSign): Belnap/Belnap = match (e1, e2) with {
        case (ConstSign.Bot, _)                         => Belnap/Belnap.Bot
        case (_, ConstSign.Bot)                         => Belnap/Belnap.Bot
        case (ConstSign.Cst(n1), ConstSign.Cst(n2))     => Belnap/alpha(n1 == n2)
        case (ConstSign.Cst(n1), ConstSign.Neg)         => if (n1 > 0ii) Belnap/Belnap.False else Belnap/Belnap.Top
        case (ConstSign.Neg, ConstSign.Cst(n1))         => if (n1 > 0ii) Belnap/Belnap.False else Belnap/Belnap.Top
        case (ConstSign.Cst(n1), ConstSign.Pos)         => if (n1 < 0ii) Belnap/Belnap.False else Belnap/Belnap.Top
        case (ConstSign.Pos, ConstSign.Cst(n1))         => if (n1 < 0ii) Belnap/Belnap.False else Belnap/Belnap.Top
        case _                                          => Belnap/Belnap.Top
    }

    /**
      * Over-approximates `not equal`.
      */
    @strict @monotone @commutative
    def neq(e1: ConstSign, e2: ConstSign): Belnap/Belnap = Belnap/not(eq(e1, e2))

    /**
      * Over-approximates `less than`.
      */
    @strict @monotone
    def less(e1: ConstSign, e2: ConstSign): Belnap/Belnap = match (e1, e2) with {
        case (ConstSign.Bot, _)                         => Belnap/Belnap.Bot
        case (_, ConstSign.Bot)                         => Belnap/Belnap.Bot
        case (ConstSign.Cst(n1), ConstSign.Cst(n2))     => Belnap/alpha(n1 < n2)
        case (ConstSign.Cst(n1), ConstSign.Neg)         => if (n1 > 0ii) Belnap/Belnap.False else Belnap/Belnap.Top
        case (ConstSign.Neg, ConstSign.Cst(n1))         => if (n1 > 0ii) Belnap/Belnap.True else Belnap/Belnap.Top
        case (ConstSign.Cst(n1), ConstSign.Pos)         => if (n1 < 0ii) Belnap/Belnap.True else Belnap/Belnap.Top
        case (ConstSign.Pos, ConstSign.Cst(n1))         => if (n1 < 0ii) Belnap/Belnap.False else Belnap/Belnap.Top
        case _                                          => Belnap/Belnap.Top
    }

    /**
      * Over-approximates `less than or equal`.
      */
    @strict @monotone
    def lessEqual(e1: ConstSign, e2: ConstSign): Belnap/Belnap = Belnap/or(e1 `less` e2, e1 `eq` e2)

    let ConstSign<> = (ConstSign.Bot, ConstSign.Top, leq, lub, glb)

}