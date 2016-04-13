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
     * Over-approximates integer `multiplication`.
     */
    @strict @monotone @commutative @associative
    def times(e1: Mod3, e2: Mod3): Mod3 = match (e1, e2) with {
        case (_, Mod3.Bot)          => Mod3.Bot
        case (Mod3.Bot, _)          => Mod3.Bot
        case (Mod3.Zer, _)          => Mod3.Zer
        case (_, Mod3.Zer)          => Mod3.Zer
        case (Mod3.One, x)          => x
        case (x, Mod3.One)          => x
        case (Mod3.Two, Mod3.Two)   => Mod3.One
        case _                      => Mod3.Top
    }

    // TODO: Implement the rest

    let Mod3<> = (Mod3.Bot, Mod3.Top, leq, lub, glb)

}