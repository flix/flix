/**
 * An implementation of the parity domain lifted over the constant propagation domain.
 */
namespace ConstantParity {

    /**
     * The elements of the constant-parity lattice.
     */
    enum ConstParity {
                  case Top,

        case Odd,          case Even,

                case Cst(BigInt),

                  case Bot
    }

    /**
     * Returns `true` iff `e1` is less than or equal to `e2`.
     */
    def leq(e1: ConstParity, e2: ConstParity): Bool = match (e1, e2) with {
        case (ConstParity.Bot, _)                       => true
        case (ConstParity.Cst(n1), ConstParity.Cst(n2)) => n1 == n2
        case (ConstParity.Cst(n), ConstParity.Odd)      => isOdd(n)
        case (ConstParity.Cst(n), ConstParity.Even)     => isEven(n)
        case (ConstParity.Odd, ConstParity.Odd)         => true
        case (ConstParity.Even, ConstParity.Even)       => true
        case (_, ConstParity.Top)                       => true
        case _                                          => false
    }

    /**
     * Returns the least upper bound of `e1` and `e2`.
     */
    @commutative @associative
    def lub(e1: ConstParity, e2: ConstParity): ConstParity = match (e1, e2) with {
        case (ConstParity.Bot, x)                       => x
        case (x, ConstParity.Bot)                       => x
        case (ConstParity.Cst(n1), ConstParity.Cst(n2)) => switch {
            case (n1 == n2)                 => ConstParity.Cst(n1)
            case (isOdd(n1) && isOdd(n2))   => ConstParity.Odd
            case (isEven(n1) && isEven(n2)) => ConstParity.Even
            case true                       => ConstParity.Top
        }
        case (ConstParity.Cst(n), ConstParity.Odd)      => if (isOdd(n))  ConstParity.Odd else ConstParity.Top
        case (ConstParity.Odd, ConstParity.Cst(n))      => if (isOdd(n))  ConstParity.Odd else ConstParity.Top
        case (ConstParity.Cst(n), ConstParity.Even)     => if (isEven(n)) ConstParity.Even else ConstParity.Top
        case (ConstParity.Even, ConstParity.Cst(n))     => if (isEven(n)) ConstParity.Even else ConstParity.Top
        case (ConstParity.Odd, ConstParity.Odd)         => ConstParity.Odd
        case (ConstParity.Even, ConstParity.Even)       => ConstParity.Even
        case _                                          => ConstParity.Top
    }

    /**
     * Returns the greatest lower bound of `e1` and `e2`.
     */
    @commutative @associative
    def glb(e1: ConstParity, e2: ConstParity): ConstParity = match (e1, e2) with {
        case (ConstParity.Top, x)                         => x
        case (x, ConstParity.Top)                         => x
        case (ConstParity.Cst(n1), ConstParity.Cst(n2)) => if (n1 == n2)  ConstParity.Cst(n1) else ConstParity.Bot
        case (ConstParity.Cst(n), ConstParity.Odd)      => if (isOdd(n))  ConstParity.Cst(n) else ConstParity.Bot
        case (ConstParity.Odd, ConstParity.Cst(n))      => if (isOdd(n))  ConstParity.Cst(n) else ConstParity.Bot
        case (ConstParity.Cst(n), ConstParity.Even)     => if (isEven(n)) ConstParity.Cst(n) else ConstParity.Bot
        case (ConstParity.Even, ConstParity.Cst(n))     => if (isEven(n)) ConstParity.Cst(n) else ConstParity.Bot
        case (ConstParity.Odd, ConstParity.Odd)         => ConstParity.Odd
        case (ConstParity.Even, ConstParity.Even)       => ConstParity.Even
        case _ => ConstParity.Bot
    }

    /**
     * Over-approximates integer `increment`.
     */
    @strict @monotone
    def inc(e: ConstParity): ConstParity = match e with {
        case ConstParity.Bot    => ConstParity.Bot
        case ConstParity.Cst(n) => ConstParity.Cst(n + 1ii)
        case ConstParity.Even   => ConstParity.Odd
        case ConstParity.Odd    => ConstParity.Even
        case ConstParity.Top    => ConstParity.Top
    }

    /**
     * Over-approximates integer `decrement`.
     */
    @strict @monotone
    def dec(e: ConstParity): ConstParity = match e with {
        case ConstParity.Bot    => ConstParity.Bot
        case ConstParity.Cst(n) => ConstParity.Cst(n - 1ii)
        case ConstParity.Even   => ConstParity.Odd
        case ConstParity.Odd    => ConstParity.Even
        case ConstParity.Top    => ConstParity.Top
    }

    /**
     * Over-approximates integer `addition`.
     */
    @strict @monotone @commutative
    def plus(e1: ConstParity, e2: ConstParity): ConstParity = match (e1, e2) with {
        case (ConstParity.Bot, _)                       => ConstParity.Bot
        case (_, ConstParity.Bot)                       => ConstParity.Bot
        case (ConstParity.Cst(n1), ConstParity.Cst(n2)) => ConstParity.Cst((n1: BigInt) + n2) // TODO: Added ascription to help type system
        case (ConstParity.Cst(n), ConstParity.Odd)      => if (isOdd(n))  ConstParity.Even else ConstParity.Odd
        case (ConstParity.Odd, ConstParity.Cst(n))      => if (isOdd(n))  ConstParity.Even else ConstParity.Odd
        case (ConstParity.Cst(n), ConstParity.Even)     => if (isEven(n)) ConstParity.Even else ConstParity.Odd
        case (ConstParity.Even, ConstParity.Cst(n))     => if (isEven(n)) ConstParity.Even else ConstParity.Odd
        case (ConstParity.Even, ConstParity.Even)       => ConstParity.Even
        case (ConstParity.Even, ConstParity.Odd)        => ConstParity.Odd
        case (ConstParity.Odd, ConstParity.Even)        => ConstParity.Odd
        case (ConstParity.Odd, ConstParity.Odd)         => ConstParity.Even
        case _                                          => ConstParity.Top
    }

    /**
     * Over-approximates integer `subtraction`.
     */
    @strict @monotone
    def minus(e1: ConstParity, e2: ConstParity): ConstParity = match (e1, e2) with {
        case (ConstParity.Bot, _)                       => ConstParity.Bot
        case (_, ConstParity.Bot)                       => ConstParity.Bot
        case (ConstParity.Cst(n1), ConstParity.Cst(n2)) => ConstParity.Cst((n1: BigInt) - n2) // TODO: Added ascription to help type system
        case (ConstParity.Cst(n), ConstParity.Odd)      => if (isOdd(n))  ConstParity.Even else ConstParity.Odd
        case (ConstParity.Odd, ConstParity.Cst(n))      => if (isOdd(n))  ConstParity.Even else ConstParity.Odd
        case (ConstParity.Cst(n), ConstParity.Even)     => if (isEven(n)) ConstParity.Even else ConstParity.Odd
        case (ConstParity.Even, ConstParity.Cst(n))     => if (isEven(n)) ConstParity.Even else ConstParity.Odd
        case (ConstParity.Even, ConstParity.Even)       => ConstParity.Even
        case (ConstParity.Even, ConstParity.Odd)        => ConstParity.Odd
        case (ConstParity.Odd, ConstParity.Even)        => ConstParity.Odd
        case (ConstParity.Odd, ConstParity.Odd)         => ConstParity.Even
        case _                                          => ConstParity.Top
    }

    /**
     * Over-approximates integer `multiplication`.
     */
    // NB: @monotone annotation removed since Z3 reports unknown.
    @strict
    def times(e1: ConstParity, e2: ConstParity): ConstParity = match (e1, e2) with {
        case (ConstParity.Bot, _)                       => ConstParity.Bot
        case (_, ConstParity.Bot)                       => ConstParity.Bot
        case (ConstParity.Cst(n1), ConstParity.Cst(n2)) => ConstParity.Cst((n1: BigInt) * n2) // TODO: Added ascription to help type system
        case (ConstParity.Cst(n), ConstParity.Odd)      => if (isOdd(n)) ConstParity.Odd else ConstParity.Even
        case (ConstParity.Odd, ConstParity.Cst(n))      => if (isOdd(n)) ConstParity.Odd else ConstParity.Even
        case (ConstParity.Cst(_), ConstParity.Even)     => ConstParity.Even
        case (ConstParity.Even, ConstParity.Cst(_))     => ConstParity.Even
        case (ConstParity.Even, ConstParity.Even)       => ConstParity.Even
        case (ConstParity.Even, ConstParity.Odd)        => ConstParity.Even
        case (ConstParity.Odd, ConstParity.Even)        => ConstParity.Even
        case (ConstParity.Odd, ConstParity.Odd)         => ConstParity.Odd
        case _                                          => ConstParity.Top
    }

    /**
     * Over-approximates integer `division`.
     */
    @strict @monotone
    def divide(e1: ConstParity, e2: ConstParity): ConstParity = match (e1, e2) with {
        case (ConstParity.Bot, _)                       => ConstParity.Bot
        case (_, ConstParity.Bot)                       => ConstParity.Bot
        case (_, ConstParity.Cst(0ii))                  => ConstParity.Bot
        case (ConstParity.Cst(n1), ConstParity.Cst(n2)) => ConstParity.Cst((n1: BigInt) / n2) // TODO: Added ascription to help type system
        case _                                          => ConstParity.Top
    }

    /**
     * Over-approximates integer `modulus`.
     */
    // NB: @monotone annotation removed since Z3 reports unknown.
    @strict
    def modulo(e1: ConstParity, e2: ConstParity): ConstParity = match (e1, e2) with {
        case (ConstParity.Bot, _)                       => ConstParity.Bot
        case (_, ConstParity.Bot)                       => ConstParity.Bot
        case (_, ConstParity.Cst(0ii))                  => ConstParity.Bot
        case (ConstParity.Cst(n1), ConstParity.Cst(n2)) => ConstParity.Cst((n1: BigInt) % n2) // TODO: Added ascription to help type system
        case (ConstParity.Cst(n), ConstParity.Even)     => if (isOdd(n)) ConstParity.Odd else ConstParity.Even
        case (ConstParity.Odd, ConstParity.Cst(n))      => if (isEven(n)) ConstParity.Odd else ConstParity.Top
        case (ConstParity.Even, ConstParity.Cst(n))     => if (isEven(n)) ConstParity.Even else ConstParity.Top
        case (ConstParity.Odd, ConstParity.Even)        => ConstParity.Odd
        case (ConstParity.Even, ConstParity.Even)       => ConstParity.Even
        case _                                          => ConstParity.Top
    }

    /**
      * Over-approximates `equal`.
      */
    @strict @monotone @commutative
    def eq(e1: ConstParity, e2: ConstParity): Belnap/Belnap = match (e1, e2) with {
        case (ConstParity.Bot, _)                       => Belnap/Belnap.Bot
        case (_, ConstParity.Bot)                       => Belnap/Belnap.Bot
        case (ConstParity.Cst(n1), ConstParity.Cst(n2)) => Belnap/alpha(n1 == n2)
        case (ConstParity.Cst(n), ConstParity.Even)     => if (isOdd(n))  Belnap/Belnap.False else Belnap/Belnap.Top
        case (ConstParity.Even, ConstParity.Cst(n))     => if (isOdd(n))  Belnap/Belnap.False else Belnap/Belnap.Top
        case (ConstParity.Cst(n), ConstParity.Odd)      => if (isEven(n)) Belnap/Belnap.False else Belnap/Belnap.Top
        case (ConstParity.Odd, ConstParity.Cst(n))      => if (isEven(n)) Belnap/Belnap.False else Belnap/Belnap.Top
        case _                                          => Belnap/Belnap.Top
    }

    /**
      * Over-approximates `not equal`.
      */
    @strict @monotone @commutative
    def neq(e1: ConstParity, e2: ConstParity): Belnap/Belnap = Belnap/not(eq(e1, e2))

    /**
      * Over-approximates `less than`.
      */
    @strict @monotone
    def less(e1: ConstParity, e2: ConstParity): Belnap/Belnap = match (e1, e2) with {
        case (ConstParity.Bot, _)                       => Belnap/Belnap.Bot
        case (_, ConstParity.Bot)                       => Belnap/Belnap.Bot
        case (ConstParity.Cst(n1), ConstParity.Cst(n2)) => Belnap/alpha(n1 < n2)
        case _                                          => Belnap/Belnap.Top
    }

    /**
      * Over-approximates `less than or equal`.
      */
    @strict @monotone
    def lessEqual(e1: ConstParity, e2: ConstParity): Belnap/Belnap = Belnap/or(e1 `less` e2, e1 `eq` e2)


    /**
     * Returns `true` iff the given number `n` is odd.
     */
    def isOdd(n: BigInt): Bool = (n % 2ii) != 0ii

    /**
     * Returns `true` iff the given number `n` is even.
     */
    def isEven(n: BigInt): Bool = !isOdd(n)

    let ConstParity<> = (ConstParity.Bot, ConstParity.Top, leq, lub, glb)

}