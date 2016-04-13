/**
 * An implementation of the parity domain lifted over the constant propagation domain.
 */
// TODO: Is this the reduced product in disguise?
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
    fn leq(e1: ConstParity, e2: ConstParity): Bool = match (e1, e2) with {
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
    fn lub(e1: ConstParity, e2: ConstParity): ConstParity = match (e1, e2) with {
        case (ConstParity.Bot, x)                       => x
        case (x, ConstParity.Bot)                       => x
        case (ConstParity.Cst(n1), ConstParity.Cst(n2)) => switch {
            case (n1 == n2)                 => ConstParity.Cst(n1)
            case (isOdd(n1) && isOdd(n2))   => ConstParity.Odd
            case (isEven(n1) && isEven(n2)) => ConstParity.Even
            case true                       => ConstParity.Top
        }
        case (ConstParity.Cst(n), ConstParity.Odd)      => if (isOdd(n)) ConstParity.Odd else ConstParity.Top
        case (ConstParity.Odd, ConstParity.Cst(n))      => if (isOdd(n)) ConstParity.Odd else ConstParity.Top
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
    fn glb(e1: ConstParity, e2: ConstParity): ConstParity = match (e1, e2) with {
        case (ConstParity.Top, x)                         => x
        case (x, ConstParity.Top)                         => x
        case (ConstParity.Cst(n1), ConstParity.Cst(n2)) => if (n1 == n2) ConstParity.Cst(n1) else ConstParity.Bot
        case (ConstParity.Cst(n), ConstParity.Odd)      => if (isOdd(n)) ConstParity.Cst(n) else ConstParity.Bot
        case (ConstParity.Odd, ConstParity.Cst(n))      => if (isOdd(n)) ConstParity.Cst(n) else ConstParity.Bot
        case (ConstParity.Cst(n), ConstParity.Even)     => if (isEven(n)) ConstParity.Cst(n) else ConstParity.Bot
        case (ConstParity.Even, ConstParity.Cst(n))     => if (isEven(n)) ConstParity.Cst(n) else ConstParity.Bot
        case (ConstParity.Odd, ConstParity.Odd)         => ConstParity.Odd
        case (ConstParity.Even, ConstParity.Even)       => ConstParity.Even
        case _ => ConstParity.Bot
    }

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