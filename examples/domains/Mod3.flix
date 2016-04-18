/**
 * An implementation of the modulo three abstract domain.
 *
 * Note: The Parity domain (e.g. even and odd) can be viewed as the Mod2 domain.
 */
namespace Mod3 {

    /**
     * The elements of the modulo three lattice.
     */
    enum Mod3 {
                   case Top,

        case Zer,  case One,  case Two,

                   case Bot
    }

    /**
     * Returns `true` iff `e1` is less than or equal to `e2`.
     */
    def leq(e1: Mod3, e2: Mod3): Bool = match (e1, e2) with {
        case (Mod3.Bot, _)          => true
        case (Mod3.Zer, Mod3.Zer)   => true
        case (Mod3.One, Mod3.One)   => true
        case (Mod3.Two, Mod3.Two)   => true
        case (_, Mod3.Top)          => true
        case _                      => false
    }

    /**
     * Returns the least upper bound of `e1` and `e2`.
     */
    @commutative @associative
    def lub(e1: Mod3, e2: Mod3): Mod3 = match (e1, e2) with {
        case (Mod3.Bot, x)          => x
        case (x, Mod3.Bot)          => x
        case (Mod3.Zer, Mod3.Zer)   => Mod3.Zer
        case (Mod3.One, Mod3.One)   => Mod3.One
        case (Mod3.Two, Mod3.Two)   => Mod3.Two
        case _                      => Mod3.Top
    }

    /**
     * Returns the greatest lower bound of `e1` and `e2`.
     */
    @commutative @associative
    def glb(e1: Mod3, e2: Mod3): Mod3 = match (e1, e2) with {
        case (Mod3.Top, x)          => x
        case (x, Mod3.Top)          => x
        case (Mod3.Zer, Mod3.Zer)   => Mod3.Zer
        case (Mod3.One, Mod3.One)   => Mod3.One
        case (Mod3.Two, Mod3.Two)   => Mod3.Two
        case _                      => Mod3.Bot
    }

    /**
      * Abstracts a concrete integer into the Parity domain.
      */
    def alpha(i: Int): Mod3 = switch {
        case i % 3 == 0 => Mod3.Zer
        case i % 3 == 1 => Mod3.One
        case i % 3 == 2 => Mod3.Two
    }

    /**
     * Over-approximates integer `increment`.
     */
    @strict @monotone
    def inc(e: Mod3): Mod3 = match e with {
       case Mod3.Bot    => Mod3.Bot
       case Mod3.Zer    => Mod3.One
       case Mod3.One    => Mod3.Two
       case Mod3.Two    => Mod3.Zer
       case Mod3.Top    => Mod3.Top
    }

    /**
     * Over-approximates integer `decrement`.
     */
    @strict @monotone
    def dec(e: Mod3): Mod3 = match e with {
       case Mod3.Bot    => Mod3.Bot
       case Mod3.Zer    => Mod3.Two
       case Mod3.One    => Mod3.Zer
       case Mod3.Two    => Mod3.One
       case Mod3.Top    => Mod3.Top
    }

    /**
     * Over-approximates integer `addition`.
     */
    @strict @monotone @commutative @associative
    def plus(e1: Mod3, e2: Mod3): Mod3 = match (e1, e2) with {
       case (_, Mod3.Bot)          => Mod3.Bot
       case (Mod3.Bot, _)          => Mod3.Bot
       case (Mod3.Zer, x)          => x
       case (x, Mod3.Zer)          => x
       case (Mod3.One, Mod3.One)   => Mod3.Two
       case (Mod3.One, Mod3.Two)   => Mod3.Zer
       case (Mod3.Two, Mod3.One)   => Mod3.Zer
       case (Mod3.Two, Mod3.Two)   => Mod3.One
       case _                      => Mod3.Top
    }

    /**
     * Over-approximates integer `subtraction`.
     */
    @strict @monotone
    def minus(e1: Mod3, e2: Mod3): Mod3 = match (e1, e2) with {
       case (_, Mod3.Bot)          => Mod3.Bot
       case (Mod3.Bot, _)          => Mod3.Bot
       case (Mod3.Zer, x)          => x
       case (x, Mod3.Zer)          => x
       case (Mod3.One, Mod3.One)   => Mod3.Zer
       case (Mod3.One, Mod3.Two)   => Mod3.One
       case (Mod3.Two, Mod3.One)   => Mod3.One
       case (Mod3.Two, Mod3.Two)   => Mod3.Zer
       case _                      => Mod3.Top
    }

    /**
     * Over-approximates integer `multiplication`.
     */
    @strict @monotone @commutative @associative
    def times(e1: Mod3, e2: Mod3): Mod3 = match (e1, e2) with {
        case (_, Mod3.Bot)          => Mod3.Bot
        case (Mod3.Bot, _)          => Mod3.Bot
        // TODO: rest
        case _                      => Mod3.Top
    }

    /**
     * Over-approximates integer `division`.
     */
    @strict @monotone
    def divide(e1: Mod3, e2: Mod3): Mod3 = match (e1, e2) with {
        case (_, Mod3.Bot)          => Mod3.Bot
        case (Mod3.Bot, _)          => Mod3.Bot
        // TODO: rest
        case _                      => Mod3.Top
    }

    /**
     * Over-approximates integer `modulus`.
     */
    @strict @monotone
    def modulo(e1: Mod3, e2: Mod3): Mod3 = match (e1, e2) with {
        case (_, Mod3.Bot)          => Mod3.Bot
        case (Mod3.Bot, _)          => Mod3.Bot
        // TODO: rest
        case _                      => Mod3.Top
    }

    // TODO: Rest

    /**
      * Over-approximates `equal`.
      */
    @strict @monotone @commutative
    def eq(e1: Mod3, e2: Mod3): Belnap/Belnap = match (e1, e2) with {
        case (Mod3.Bot, _)          => Belnap/Belnap.Bot
        case (_, Mod3.Bot)          => Belnap/Belnap.Bot
        case (Mod3.Zer, Mod3.One)   => Belnap/Belnap.False
        case (Mod3.Zer, Mod3.Two)   => Belnap/Belnap.False
        case (Mod3.One, Mod3.Zer)   => Belnap/Belnap.False
        case (Mod3.One, Mod3.Two)   => Belnap/Belnap.False
        case (Mod3.Two, Mod3.Zer)   => Belnap/Belnap.False
        case (Mod3.Two, Mod3.One)   => Belnap/Belnap.False
        case _                      => Belnap/Belnap.Top
    }

    /**
      * Over-approximates `not equal`.
      */
    @strict @monotone @commutative
    def neq(e1: Mod3, e2: Mod3): Belnap/Belnap = Belnap/not(eq(e1, e2))

    /**
      * Over-approximates `less than`.
      */
    @strict @monotone
    def less(e1: Mod3, e2: Mod3): Belnap/Belnap = match (e1, e2) with {
        case (Mod3.Bot, _)            => Belnap/Belnap.Bot
        case (_, Mod3.Bot)            => Belnap/Belnap.Bot
        case _                          => Belnap/Belnap.Top
    }

    /**
      * Over-approximates `less than or equal`.
      */
    @strict @monotone
    def lessEqual(e1: Mod3, e2: Mod3): Belnap/Belnap = Belnap/or(e1 `less` e2, e1 `eq` e2)

    let Mod3<> = (Mod3.Bot, Mod3.Top, leq, lub, glb)

}