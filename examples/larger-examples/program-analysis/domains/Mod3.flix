///
/// An implementation of the modulo three abstract domain.
///
/// Note: The Parity domain (e.g. even and odd) can be viewed as the Mod2 domain.
///
mod Domain.Mod3 {

    ///
    /// The elements of the modulo three lattice.
    ///
    enum Mod3 with Eq {
                   case Top,

        case Zer,  case One,  case Two,

                   case Bot
    }

    ///
    /// Returns `true` iff `e1` is less than or equal to `e2`.
    ///
//    #reflexive #antiSymmetric #transitive #leastElement(Mod3.Bot) #greatestElement(Mod3.Top)
    pub def leq(e1: Mod3, e2: Mod3): Bool = match (e1, e2) {
        case (Mod3.Bot, _)        => true
        case (Mod3.Zer, Mod3.Zer) => true
        case (Mod3.One, Mod3.One) => true
        case (Mod3.Two, Mod3.Two) => true
        case (_, Mod3.Top)        => true
        case _                    => false
    }

    ///
    /// Returns the least upper bound of `e1` and `e2`.
    ///
//    #upperBound #leastUpperBound #commutative #associative
    pub def lub(e1: Mod3, e2: Mod3): Mod3 = match (e1, e2) {
        case (Mod3.Bot, x)        => x
        case (x, Mod3.Bot)        => x
        case (Mod3.Zer, Mod3.Zer) => Mod3.Zer
        case (Mod3.One, Mod3.One) => Mod3.One
        case (Mod3.Two, Mod3.Two) => Mod3.Two
        case _                    => Mod3.Top
    }

    ///
    /// Returns the greatest lower bound of `e1` and `e2`.
    ///
//    #lowerBound #greatestLowerBound #commutative #associative
    pub def glb(e1: Mod3, e2: Mod3): Mod3 = match (e1, e2) {
        case (Mod3.Top, x)        => x
        case (x, Mod3.Top)        => x
        case (Mod3.Zer, Mod3.Zer) => Mod3.Zer
        case (Mod3.One, Mod3.One) => Mod3.One
        case (Mod3.Two, Mod3.Two) => Mod3.Two
        case _                    => Mod3.Bot
    }

    ///
    /// The lattice height function.
    ///
//    #nonNegative
    pub def height(e: Mod3): BigInt = match e {
        case Mod3.Top => 0ii
        case Mod3.Zer => 1ii
        case Mod3.One => 1ii
        case Mod3.Two => 1ii
        case Mod3.Bot => 2ii
    }

    ///
    /// Abstracts a concrete integer into the Parity domain.
    ///
    pub def alpha(i: BigInt): Mod3 = {
    // TODO: The verifier must handle errors so it never enters a Switch/MatchError.
        if      ((i `BigInt.remainder` 3ii == 1ii) or (i `BigInt.remainder` 3ii == -1ii)) Mod3.One
        else if ((i `BigInt.remainder` 3ii == 2ii) or (i `BigInt.remainder` 3ii == -2ii)) Mod3.Two
        else                                                                              Mod3.Zer
    }

    ///
    /// Over-approximates integer `increment`.
    ///
//    #safe1(x -> x + 1ii)
//    #strict1 #monotone1
    pub def inc(e: Mod3): Mod3 = match e {
       case Mod3.Bot => Mod3.Bot
       case Mod3.Zer => Mod3.One
       case Mod3.One => Mod3.Two
       case Mod3.Two => Mod3.Zer
       case Mod3.Top => Mod3.Top
    }

    ///
    /// Over-approximates integer `decrement`.
    ///
//    #safe1(x -> x - 1ii)
//    #strict1 #monotone1
    pub def dec(e: Mod3): Mod3 = match e {
       case Mod3.Bot => Mod3.Bot
       case Mod3.Zer => Mod3.Two
       case Mod3.One => Mod3.Zer
       case Mod3.Two => Mod3.One
       case Mod3.Top => Mod3.Top
    }

    ///
    /// Over-approximates integer `addition`.
    ///
//    #safe2((x, y) -> x + y)
//    #strict2 #monotone2 #commutative #associative
    pub def plus(e1: Mod3, e2: Mod3): Mod3 = match (e1, e2) {
       case (_, Mod3.Bot)        => Mod3.Bot
       case (Mod3.Bot, _)        => Mod3.Bot
       case (Mod3.Zer, x)        => x
       case (x, Mod3.Zer)        => x
       case (Mod3.One, Mod3.One) => Mod3.Two
       case (Mod3.One, Mod3.Two) => Mod3.Zer
       case (Mod3.Two, Mod3.One) => Mod3.Zer
       case (Mod3.Two, Mod3.Two) => Mod3.One
       case _                    => Mod3.Top
    }

    ///
    /// Over-approximates integer `subtraction`.
    ///
//    #safe2((x, y) -> x - y)
//    #strict2 #monotone2
    pub def minus(e1: Mod3, e2: Mod3): Mod3 = match (e1, e2) {
       case (_, Mod3.Bot)        => Mod3.Bot
       case (Mod3.Bot, _)        => Mod3.Bot
       case (Mod3.Zer, Mod3.Zer) => Mod3.Zer
       case (Mod3.Zer, Mod3.One) => Mod3.Two
       case (Mod3.Zer, Mod3.Two) => Mod3.One
       case (Mod3.One, Mod3.Zer) => Mod3.One
       case (Mod3.One, Mod3.One) => Mod3.Zer
       case (Mod3.One, Mod3.Two) => Mod3.Two
       case (Mod3.Two, Mod3.Zer) => Mod3.Two
       case (Mod3.Two, Mod3.One) => Mod3.One
       case (Mod3.Two, Mod3.Two) => Mod3.Zer
       case _                    => Mod3.Top
    }

    ///
    /// Over-approximates integer `multiplication`.
    ///
    /* unknown #safe2((x, y) -> x * y) */
//    #strict2 #monotone2 #commutative #associative
    pub def times(e1: Mod3, e2: Mod3): Mod3 = match (e1, e2) {
        case (_, Mod3.Bot) => Mod3.Bot
        case (Mod3.Bot, _) => Mod3.Bot
        case (Mod3.Zer, _) => Mod3.Zer
        case (_, Mod3.Zer) => Mod3.Zer
        case _             => Mod3.Top
    }

    //
    // ## Specialized Laws
    //

//    law upperBound(lub: (Mod3, Mod3) -> Mod3): Bool = JoinLattice.leastUpperBound1(leq, lub)

//    law leastUpperBound(lub: (Mod3, Mod3) -> Mod3): Bool = JoinLattice.leastUpperBound2(leq, lub)

//    law lowerBound(glb: (Mod3, Mod3) -> Mod3): Bool = MeetLattice.greatestLowerBound1(leq, glb)

//    law greatestLowerBound(glb: (Mod3, Mod3) -> Mod3): Bool = MeetLattice.greatestLowerBound2(leq, glb)

//    law leastElement(leq: (Mod3, Mod3) -> Bool, bot: Mod3): Bool = Bounded.leastElement(bot, leq)

//    law greatestElement(leq: (Mod3, Mod3) -> Bool, top: Mod3): Bool = Bounded.greatestElement(top, leq)

//    law strict1(f: Mod3 -> Mod3): Bool = Bounded.strict1(f, Mod3.Bot, Mod3.Bot)

//    law strict2(f: (Mod3, Mod3) -> Mod3): Bool = Bounded.strict2(f, Mod3.Bot, Mod3.Bot, Mod3.Bot)

//    law monotone1(f: Mod3 -> Mod3): Bool = PartialOrder.monotone1(f, leq, leq)

//    law monotone2(f: (Mod3, Mod3) -> Mod3): Bool = PartialOrder.monotone2(f, leq, leq, leq)

//    law safe1(fa: Mod3 -> Mod3, fc: BigInt -> BigInt): Bool = PartialOrder.safe1(fa, fc, alpha, alpha, leq)

//    law safe2(fa: (Mod3, Mod3) -> Mod3, fc: (BigInt, BigInt) -> BigInt): Bool = PartialOrder.safe2(fa, fc, alpha, alpha, leq)

}
