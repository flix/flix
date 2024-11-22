///
/// An implementation of the sign domain.
///
/// See Cousot & Cousot '77 for details.
///
mod Domain.Sign {

    ///
    /// The elements of the sign lattice.
    ///
    /// Notice that Sign.Neg and Sign.Pos implicitly includes zero.
    ///
    enum Sign with Eq {
                  case Top,

        case Neg,          case Pos,

                  case Zer,

                  case Bot
    }

    ///
    /// Returns `true` iff `e1` is less than or equal to `e2`.
    ///
//    #reflexive #antiSymmetric #transitive #leastElement(Sign.Bot) #greatestElement(Sign.Top)
    pub def leq(e1: Sign, e2: Sign): Bool = match (e1, e2) {
        case (Sign.Bot, _)        => true
        case (Sign.Zer, Sign.Zer) => true
        case (Sign.Zer, Sign.Neg) => true
        case (Sign.Zer, Sign.Pos) => true
        case (Sign.Neg, Sign.Neg) => true
        case (Sign.Pos, Sign.Pos) => true
        case (_, Sign.Top)        => true
        case _                    => false
    }

    ///
    /// Returns the least upper bound of `e1` and `e2`.
    ///
//    #upperBound #leastUpperBound #commutative #associative
    pub def lub(e1: Sign, e2: Sign): Sign = match (e1, e2) {
        case (Sign.Bot, x)        => x
        case (x, Sign.Bot)        => x
        case (Sign.Zer, Sign.Zer) => Sign.Zer
        case (Sign.Zer, Sign.Neg) => Sign.Neg
        case (Sign.Neg, Sign.Zer) => Sign.Neg
        case (Sign.Zer, Sign.Pos) => Sign.Pos
        case (Sign.Pos, Sign.Zer) => Sign.Pos
        case (Sign.Neg, Sign.Neg) => Sign.Neg
        case (Sign.Pos, Sign.Pos) => Sign.Pos
        case _                    => Sign.Top
    }

    ///
    /// Returns the greatest lower bound of `e1` and `e2`.
    ///
//    #lowerBound #greatestLowerBound #commutative #associative
    pub def glb(e1: Sign, e2: Sign): Sign = match (e1, e2) {
        case (Sign.Top, x)        => x
        case (x, Sign.Top)        => x
        case (Sign.Zer, Sign.Zer) => Sign.Zer
        case (Sign.Neg, Sign.Neg) => Sign.Neg
        case (Sign.Pos, Sign.Pos) => Sign.Pos
        case (Sign.Zer, Sign.Neg) => Sign.Zer
        case (Sign.Neg, Sign.Zer) => Sign.Zer
        case (Sign.Zer, Sign.Pos) => Sign.Zer
        case (Sign.Pos, Sign.Zer) => Sign.Zer
        case (Sign.Neg, Sign.Pos) => Sign.Zer
        case (Sign.Pos, Sign.Neg) => Sign.Zer
        case (x, Sign.Top)        => x
        case (Sign.Top, x)        => x
        case _                    => Sign.Bot
    }

    ///
    /// The lattice height function.
    ///
//    #nonSign.Negative
    pub def height(e: Sign): BigInt = match e {
        case Sign.Top => 0ii
        case Sign.Pos => 1ii
        case Sign.Neg => 1ii
        case Sign.Zer => 2ii
        case Sign.Bot => 3ii
    }

    ///
    /// Abstracts a concrete number into the sign domain.
    ///
    pub def alpha(i: BigInt): Sign = {
        if      (i < 0ii) Sign.Neg
        else if (i > 0ii) Sign.Pos
        else              Sign.Zer
    }

    ///
    /// Over-approximates integer `increment`.
    ///
//    #safe1(x -> x + 1ii)
//    #strict1 #monotone1
    pub def inc(e: Sign): Sign = match e {
        case Sign.Bot => Sign.Bot
        case Sign.Neg => Sign.Top
        case Sign.Zer => Sign.Pos
        case Sign.Pos => Sign.Pos
        case Sign.Top => Sign.Top
    }

    ///
    /// Over-approximates integer `decrement`.
    ///
//    #safe1(x -> x - 1ii)
//    #strict1 #monotone1
    pub def dec(e: Sign): Sign = match e {
        case Sign.Bot => Sign.Bot
        case Sign.Neg => Sign.Neg
        case Sign.Zer => Sign.Neg
        case Sign.Pos => Sign.Top
        case Sign.Top => Sign.Top
    }

    ///
    /// Over-approximates integer `addition`.
    ///
//    #safe2((x, y) -> x + y)
//    #strict2 #monotone2 #commutative #associative
    pub def plus(e1: Sign, e2: Sign): Sign = match (e1, e2) {
        case (Sign.Bot, _)        => Sign.Bot
        case (_, Sign.Bot)        => Sign.Bot
        case (Sign.Neg, Sign.Neg) => Sign.Neg
        case (Sign.Neg, Sign.Zer) => Sign.Neg
        case (Sign.Neg, Sign.Pos) => Sign.Top
        case (Sign.Zer, Sign.Neg) => Sign.Neg
        case (Sign.Zer, Sign.Zer) => Sign.Zer
        case (Sign.Zer, Sign.Pos) => Sign.Pos
        case (Sign.Pos, Sign.Neg) => Sign.Top
        case (Sign.Pos, Sign.Zer) => Sign.Pos
        case (Sign.Pos, Sign.Pos) => Sign.Pos
        case  _                   => Sign.Top
    }

    ///
    /// Over-approximates integer `subtraction`.
    ///
//    #safe2((x, y) -> x - y)
//    #strict2 #monotone2
    pub def minus(e1: Sign, e2: Sign): Sign = match (e1, e2) {
        case (Sign.Bot, _)        => Sign.Bot
        case (_, Sign.Bot)        => Sign.Bot
        case (Sign.Neg, Sign.Neg) => Sign.Top
        case (Sign.Neg, Sign.Zer) => Sign.Neg
        case (Sign.Neg, Sign.Pos) => Sign.Neg
        case (Sign.Zer, Sign.Neg) => Sign.Pos
        case (Sign.Zer, Sign.Zer) => Sign.Zer
        case (Sign.Zer, Sign.Pos) => Sign.Neg
        case (Sign.Pos, Sign.Neg) => Sign.Pos
        case (Sign.Pos, Sign.Zer) => Sign.Pos
        case (Sign.Pos, Sign.Pos) => Sign.Top
        case  _                   => Sign.Top
    }

    ///
    /// Over-approximates integer `multiplication`.
    ///
//    #safe2((x, y) -> x * y)
//    #strict2 #monotone2
    pub def times(e1: Sign, e2: Sign): Sign = match (e1, e2) {
        case (Sign.Bot, _)        => Sign.Bot
        case (_, Sign.Bot)        => Sign.Bot
        case (Sign.Neg, Sign.Neg) => Sign.Pos
        case (Sign.Neg, Sign.Zer) => Sign.Zer
        case (Sign.Neg, Sign.Pos) => Sign.Neg
        case (Sign.Zer, Sign.Neg) => Sign.Zer
        case (Sign.Zer, Sign.Zer) => Sign.Zer
        case (Sign.Zer, Sign.Pos) => Sign.Zer
        case (Sign.Pos, Sign.Neg) => Sign.Neg
        case (Sign.Pos, Sign.Zer) => Sign.Zer
        case (Sign.Pos, Sign.Pos) => Sign.Pos
        case  _                   => Sign.Top
    }

    //
    // ## Specialized Laws
    //

//    law upperBound(lub: (Sign, Sign) -> Sign): Bool = JoinLattice.leastUpperBound1(leq, lub)

//    law leastUpperBound(lub: (Sign, Sign) -> Sign): Bool = JoinLattice.leastUpperBound2(leq, lub)

//    law lowerBound(glb: (Sign, Sign) -> Sign): Bool = MeetLattice.greatestLowerBound1(leq, glb)

//    law greatestLowerBound(glb: (Sign, Sign) -> Sign): Bool = MeetLattice.greatestLowerBound2(leq, glb)

//    law leastElement(leq: (Sign, Sign) -> Bool, bot: Sign): Bool = Bounded.leastElement(bot, leq)

//    law greatestElement(leq: (Sign, Sign) -> Bool, top: Sign): Bool = Bounded.greatestElement(top, leq)

//    law strict1(f: Sign -> Sign): Bool = Bounded.strict1(f, Sign.Bot, Sign.Bot)

//    law strict2(f: (Sign, Sign) -> Sign): Bool = Bounded.strict2(f, Sign.Bot, Sign.Bot, Sign.Bot)

//    law monotone1(f: Sign -> Sign): Bool = PartialOrder.monotone1(f, leq, leq)

//    law monotone2(f: (Sign, Sign) -> Sign): Bool = PartialOrder.monotone2(f, leq, leq, leq)

//    law safe1(fa: Sign -> Sign, fc: BigInt -> BigInt): Bool = PartialOrder.safe1(fa, fc, alpha, alpha, leq)

//    law safe2(fa: (Sign, Sign) -> Sign, fc: (BigInt, BigInt) -> BigInt): Bool = PartialOrder.safe2(fa, fc, alpha, alpha, leq)

}
