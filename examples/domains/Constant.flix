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

    /**
     * Abstracts a concrete integer into the Sign domain.
     */
    fn alpha(i: Int): Constant = Constant.Cst(i)

    /**
     * Over-approximates integer `addition`.
     */
    @strict @monotone @commutative @associative
    fn plus(e1: Constant, e2: Constant): Constant = match (e1, e2) with {
        case (Constant.Bot, _)                      => Constant.Bot
        case (_, Constant.Bot)                      => Constant.Bot
        case (Constant.Cst(n1), Constant.Cst(n2))   => Constant.Cst(n1 + n2)
        case _                                      => Constant.Top
    }

    /**
     * Over-approximates integer `subtraction`.
     */
    @strict @monotone
    fn minus(e1: Constant, e2: Constant): Constant = match (e1, e2) with {
        case (Constant.Bot, _)                      => Constant.Bot
        case (_, Constant.Bot)                      => Constant.Bot
        case (Constant.Cst(n1), Constant.Cst(n2))   => Constant.Cst(n1 - n2)
        case _                                      => Constant.Top
    }

    /**
     * Over-approximates integer `multiplication`.
     */
    @strict @monotone @commutative @associative
    fn times(e1: Constant, e2: Constant): Constant = match (e1, e2) with {
        case (Constant.Bot, _)                      => Constant.Bot
        case (_, Constant.Bot)                      => Constant.Bot
        case (Constant.Cst(n1), Constant.Cst(n2))   => Constant.Cst(n1 * n2)
        case _                                      => Constant.Top
    }

    /**
     * Over-approximates integer `division`.
     */
    @strict @monotone
    fn divide(e1: Constant, e2: Constant): Constant = match (e1, e2) with {
        case (Constant.Bot, _)                      => Constant.Bot
        case (_, Constant.Bot)                      => Constant.Bot
        case (Constant.Cst(n1), Constant.Cst(n2))   => Constant.Cst(n1 / n2)
        case _                                      => Constant.Top
    }

    /**
     * Over-approximates integer `modulus`.
     */
    @strict @monotone
    fn modulo(e1: Constant, e2: Constant): Constant = match (e1, e2) with {
        case (Constant.Bot, _)                      => Constant.Bot
        case (_, Constant.Bot)                      => Constant.Bot
        case (Constant.Cst(n1), Constant.Cst(n2))   => Constant.Cst(n1 % n2)
        case _                                      => Constant.Top
    }

    /**
     * Over-approximates `bitwise negation`.
     */
    @strict @monotone
    fn negate(e: Constant): Constant = match e with {
        case Constant.Bot       => Constant.Bot
        case Constant.Cst(n)    => Constant.Cst(~ n)
        case Constant.Top       => Constant.Top
    }

    /**
      * Over-approximates `bitwise or`.
      */
    @strict @monotone @commutative @associative
    fn or(e1: Constant, e2: Constant): Constant = match (e1, e2) with {
        case (Constant.Bot, _)                      => Constant.Bot
        case (_, Constant.Bot)                      => Constant.Bot
        case (Constant.Cst(n1), Constant.Cst(n2))   => Constant.Cst(n1 | n2)
        case _                                      => Constant.Top
    }

    /**
      * Over-approximates `bitwise and`.
      */
    @strict @monotone @commutative @associative
    fn and(e1: Constant, e2: Constant): Constant = match (e1, e2) with {
        case (Constant.Bot, _)                      => Constant.Bot
        case (_, Constant.Bot)                      => Constant.Bot
        case (Constant.Cst(n1), Constant.Cst(n2))   => Constant.Cst(n1 & n2)
        case _                                      => Constant.Top
    }

    /**
      * Over-approximates `bitwise xor`.
      */
    @strict @monotone @commutative @associative
    fn xor(e1: Constant, e2: Constant): Constant = match (e1, e2) with {
        case (Constant.Bot, _)                      => Constant.Bot
        case (_, Constant.Bot)                      => Constant.Bot
        case (Constant.Cst(n1), Constant.Cst(n2))   => Constant.Cst(n1 ^ n2)
        case _                                      => Constant.Top
    }

    /**
      * Over-approximates `bitwise left shift`.
      */
    @strict @monotone
    fn leftShift(e1: Constant, e2: Constant): Constant = match (e1, e2) with {
        case (Constant.Bot, _)                      => Constant.Bot
        case (_, Constant.Bot)                      => Constant.Bot
        case (Constant.Cst(n1), Constant.Cst(n2))   => Constant.Cst(n1 << n2)
        case _                                      => Constant.Top
    }

    /**
      * Over-approximates `bitwise right shift`.
      */
    @strict @monotone
    fn rightShift(e1: Constant, e2: Constant): Constant = match (e1, e2) with {
        case (Constant.Bot, _)                      => Constant.Bot
        case (_, Constant.Bot)                      => Constant.Bot
        case (Constant.Cst(n1), Constant.Cst(n2))   => Constant.Cst(n1 >> n2)
        case _                                      => Constant.Top
    }

    /**
      * Over-approximates `equal`.
      */
    @strict @monotone @commutative
    fn eq(e1: Constant, e2: Constant): Belnap::Belnap = match (e1, e2) with {
        case (Constant.Bot, _)                      => Belnap::Belnap.Bot
        case (_, Constant.Bot)                      => Belnap::Belnap.Bot
        case (Constant.Cst(n1), Constant.Cst(n2))   => Belnap::alpha(n1 == n2)
        case _                                      => Belnap::Belnap.Top
    }

    /**
      * Over-approximates `not equal`.
      */
    @strict @monotone @commutative
    fn neq(e1: Constant, e2: Constant): Belnap::Belnap = Belnap::not(eq(e1, e2))

    /**
      * Over-approximates `less than`.
      */
    @strict @monotone
    fn less(e1: Constant, e2: Constant): Belnap::Belnap = match (e1, e2) with {
        case (Constant.Bot, _)                      => Belnap::Belnap.Bot
        case (_, Constant.Bot)                      => Belnap::Belnap.Bot
        case (Constant.Cst(n1), Constant.Cst(n2))   => Belnap::alpha(n1 < n2)
        case _                                      => Belnap::Belnap.Top
    }

    /**
      * Over-approximates `less than or equal`.
      */
    @strict @monotone
    fn lessEqual(e1: Constant, e2: Constant): Belnap::Belnap = Belnap::or(e1 `less` e2, e1 `eq` e2)
}