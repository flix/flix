/**
 * An implementation of the four-valued boolean lattice named for Nuel Belnap.
 */
namespace Belnap {

    /**
     * The elements of the boolean lattice.
     */
    enum Belnap {
                  case Top,

        case True,         case False,

                  case Bot
    }

    /**
     * Returns `true` iff `e1` is less than or equal to `e2`.
     */
    @commutative @associative
    fn leq(e1: Belnap, e2: Belnap): Bool = match (e1, e2) with {
        case (Belnap.Bot, _)                => true
        case (Belnap.True, Belnap.True)     => true
        case (Belnap.False, Belnap.False)   => true
        case (_, Belnap.Top)                => true
        case _                              => false
    }

    /**
     * Returns the least upper bound of `e1` and `e2`.
     */
    @commutative @associative
    fn lub(e1: Belnap, e2: Belnap): Belnap = match (e1, e2) with {
        case (Belnap.Bot, x)                => x
        case (x, Belnap.Bot)                => x
        case (Belnap.True, Belnap.True)     => Belnap.True
        case (Belnap.False, Belnap.False)   => Belnap.False
        case _                              => Belnap.Top
    }

    /**
     * Returns the greatest lower bound of `e1` and `e2`.
     */
    @commutative @associative
    fn glb(e1: Belnap, e2: Belnap): Belnap = match (e1, e2) with {
        case (Belnap.Top, x)                => x
        case (x, Belnap.Top)                => x
        case (Belnap.True, Belnap.True)     => Belnap.True
        case (Belnap.False, Belnap.False)   => Belnap.False
        case _                              => Belnap.Bot
    }

    /**
     * Over-approximates the logical `not` operator.
     */
    @strict @monotone
    fn not(e: Belnap): Belnap = match e with {
        case Belnap.Bot     => Belnap.Bot
        case Belnap.True    => Belnap.False
        case Belnap.False   => Belnap.True
        case Belnap.Top     => Belnap.Top
    }

    /**
     * Over-approximates the logical `and` operator.
     */
    @strict @monotone @commutative @associative
    fn and(e1: Belnap, e2: Belnap): Belnap = match (e1, e2) with {
        case (Belnap.Bot, _)                => Belnap.Bot
        case (_, Belnap.Bot)                => Belnap.Bot
        case (Belnap.True, Belnap.True)     => Belnap.True
        case (Belnap.False, Belnap.True)    => Belnap.False
        case (Belnap.True, Belnap.False)    => Belnap.False
        case (Belnap.False, Belnap.False)   => Belnap.False
        case _                              => Belnap.Top
    }

    /**
     * Over-approximates the logical `or` operator.
     */
    @strict @monotone @commutative @associative
    fn or(e1: Belnap, e2: Belnap): Belnap = match (e1, e2) with {
        case (Belnap.Bot, _)                => Belnap.Bot
        case (_, Belnap.Bot)                => Belnap.Bot
        case (Belnap.True, Belnap.True)     => Belnap.True
        case (Belnap.True, Belnap.False)    => Belnap.True
        case (Belnap.False, Belnap.True)    => Belnap.True
        case (Belnap.False, Belnap.False)   => Belnap.False
        case _                              => Belnap.Top
    }

    /**
     * Over-approximates the logical `xor` operator.
     */
    @strict @monotone @commutative @associative
    fn xor(e1: Belnap, e2: Belnap): Belnap = match (e1, e2) with {
        case (Belnap.Bot, _)                => Belnap.Bot
        case (_, Belnap.Bot)                => Belnap.Bot
        case (Belnap.True, Belnap.False)    => Belnap.True
        case (Belnap.False, Belnap.True)    => Belnap.True
        case (Belnap.True, Belnap.True)     => Belnap.False
        case (Belnap.False, Belnap.False)   => Belnap.False
        case _                              => Belnap.Top
    }

}
