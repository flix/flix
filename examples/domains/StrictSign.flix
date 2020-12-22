/**
 * An implementation of the strict sign domain.
 */
namespace Domain/StrictSign {

    ///
    /// The elements of the strict sign lattice.
    ///
    enum Sign {
                  case Top,

        case Neg, case Zer, case Pos,

                  case Bot
    }

    ///
    /// Returns `true` iff `e1` is equal to `e2`.
    ///
    pub def equ(e1: Sign, e2: Sign): Bool = e1 == e2

    ///
    /// Returns `true` iff `e1` is less than or equal to `e2`.
    ///
    #reflexive #antiSymmetric #transitive #leastElement(Bot) #greatestElement(Top)
    def leq(e1: Sign, e2: Sign): Bool = match (e1, e2) {
        case (Bot, _)   => true
        case (Neg, Neg) => true
        case (Zer, Zer) => true
        case (Pos, Pos) => true
        case (_, Top)   => true
        case _          => false
    }

    ///
    /// Returns the least upper bound of `e1` and `e2`.
    ///
    #upperBound #leastUpperBound #commutative #associative
    pub def lub(e1: Sign, e2: Sign): Sign = match (e1, e2) {
        case (Bot, x)   => x
        case (x, Bot)   => x
        case (Neg, Neg) => Neg
        case (Zer, Zer) => Zer
        case (Pos, Pos) => Pos
        case _          => Top
    }

    ///
    /// Returns the greatest lower bound of `e1` and `e2`.
    ///
    #lowerBound #greatestLowerBound #commutative #associative
    pub def glb(e1: Sign, e2: Sign): Sign = match (e1, e2) {
        case (Top, x)   => x
        case (x, Top)   => x
        case (Neg, Neg) => Neg
        case (Zer, Zer) => Zer
        case (Pos, Pos) => Pos
        case _          => Bot
    }

    ///
    /// The lattice height function.
    ///
    #nonNegative #decreasing(equ, leq)
    pub def height(e: Sign): BigInt = match e {
        case Top    => 0ii
        case Neg    => 1ii
        case Zer    => 1ii
        case Pos    => 1ii
        case Bot    => 2ii
    }

    ///
    /// Abstracts a concrete integer into the Sign domain.
    ///
    pub def alpha(i: BigInt): Sign = {
        if      (i < 0ii)   Neg
        else if (i > 0ii)   Pos
        else                Zer
    }

    ///
    /// Over-approximates integer `increment`.
    ///
    #safe1(x -> x + 1ii)
    #strict1 #monotone1
    pub def inc(e: Sign): Sign = match e {
        case Bot => Bot
        case Neg => Top
        case Zer => Pos
        case Pos => Pos
        case Top => Top
    }

    ///
    /// Over-approximates integer `decrement`.
    ///
    #safe1(x -> x - 1ii)
    #strict1 #monotone1
    pub def dec(e: Sign): Sign = match e {
        case Bot => Bot
        case Neg => Neg
        case Zer => Neg
        case Pos => Top
        case Top => Top
    }

    ///
    /// Over-approximates integer `addition`.
    ///
    #safe2((x, y) -> x + y)
    #strict2 #monotone2 #commutative #associative
    pub def plus(e1: Sign, e2: Sign): Sign = match (e1, e2) {
        case (Bot, _)   => Bot
        case (_, Bot)   => Bot
        case (Neg, Neg) => Neg
        case (Neg, Zer) => Neg
        case (Neg, Pos) => Top
        case (Zer, Neg) => Neg
        case (Zer, Zer) => Zer
        case (Zer, Pos) => Pos
        case (Pos, Neg) => Top
        case (Pos, Zer) => Pos
        case (Pos, Pos) => Pos
        case _          => Top
    }

    ///
    /// Over-approximates integer `subtraction`.
    ///
    #safe2((x, y) -> x - y)
    #strict2 #monotone2
    pub def minus(e1: Sign, e2: Sign): Sign = match (e1, e2) {
        case (Bot, _)   => Bot
        case (_, Bot)   => Bot
        case (Neg, Neg) => Top
        case (Neg, Zer) => Neg
        case (Neg, Pos) => Neg
        case (Zer, Neg) => Pos
        case (Zer, Zer) => Zer
        case (Zer, Pos) => Neg
        case (Pos, Neg) => Pos
        case (Pos, Zer) => Pos
        case (Pos, Pos) => Top
        case _          => Top
    }

    ///
    /// Over-approximates integer `multiplication`.
    ///
    #safe2((x, y) -> x * y)
    #strict2 #monotone2 #commutative #associative
    pub def times(e1: Sign, e2: Sign): Sign = match (e1, e2) {
        case (Bot, _)   => Bot
        case (_, Bot)   => Bot
        case (Neg, Neg) => Pos
        case (Neg, Zer) => Zer
        case (Neg, Pos) => Neg
        case (Zer, Neg) => Zer
        case (Zer, Zer) => Zer
        case (Zer, Pos) => Zer
        case (Pos, Neg) => Neg
        case (Pos, Zer) => Zer
        case (Pos, Pos) => Pos
        case _          => Top
    }

    ///
    /// Over-approximates `equal`.
    ///
    #PartialOrder.safe2((x, y) -> x == y, alpha, Belnap.alpha, Belnap.leq)
    #Bounded.strict2(Bot, Bot, Belnap/Belnap.Bot)
    #PartialOrder.monotone2(leq, leq, Belnap.leq)
    #commutative
    pub def eq(e1: Sign, e2: Sign): Belnap.Belnap = match (e1, e2) {
        case (Bot, _)   => Belnap/Belnap.Bot
        case (_, Bot)   => Belnap/Belnap.Bot
        case (Neg, Neg) => Belnap/Belnap.Top
        case (Neg, Zer) => Belnap/Belnap.False
        case (Neg, Pos) => Belnap/Belnap.False
        case (Zer, Neg) => Belnap/Belnap.False
        case (Zer, Zer) => Belnap/Belnap.True
        case (Zer, Pos) => Belnap/Belnap.False
        case (Pos, Neg) => Belnap/Belnap.False
        case (Pos, Zer) => Belnap/Belnap.False
        case (Pos, Pos) => Belnap/Belnap.Top
        case _          => Belnap/Belnap.Top
    }

    ///
    /// Over-approximates `not equal`.
    ///
    #PartialOrder.safe2((x, y) -> x != y, alpha, Belnap.alpha, Belnap.leq)
    #Bounded.strict2(Bot, Bot, Belnap/Belnap.Bot)
    #PartialOrder.monotone2(leq, leq, Belnap.leq)
    #commutative
    pub def neq(e1: Sign, e2: Sign): Belnap.Belnap = Belnap.not(eq(e1, e2))

    ///
    /// Over-approximates `less than`.
    ///
    #PartialOrder.safe2((x, y) -> x < y, alpha, Belnap.alpha, Belnap.leq)
    #Bounded.strict2(Bot, Bot, Belnap/Belnap.Bot)
    #PartialOrder.monotone2(leq, leq, Belnap.leq)
    pub def less(e1: Sign, e2: Sign): Belnap.Belnap = match (e1, e2) {
        case (Bot, _)   => Belnap/Belnap.Bot
        case (_, Bot)   => Belnap/Belnap.Bot
        case (Neg, Zer) => Belnap/Belnap.True
        case (Neg, Pos) => Belnap/Belnap.True
        case (Zer, Pos) => Belnap/Belnap.True
        case (Pos, Neg) => Belnap/Belnap.False
        case (Pos, Zer) => Belnap/Belnap.False
        case (Zer, Neg) => Belnap/Belnap.False
        case _          => Belnap/Belnap.Top
    }

    ///
    /// Over-approximates `less than or equal`.
    ///
    #PartialOrder.safe2((x, y) -> x <= y, alpha, Belnap.alpha, Belnap.leq)
    #Bounded.strict2(Bot, Bot, Belnap/Belnap.Bot)
    #PartialOrder.monotone2(leq, leq, Belnap.leq)
    pub def lessEqual(e1: Sign, e2: Sign): Belnap.Belnap = Belnap.or(e1 `less` e2, e1 `eq` e2)

    //
    // ## Specialized Laws
    //

    law upperBound(lub: (Sign, Sign) -> Sign): Bool = JoinLattice.upperBound(leq, lub)

    law leastUpperBound(lub: (Sign, Sign) -> Sign): Bool = JoinLattice.leastUpperBound(leq, lub)

    law lowerBound(glb: (Sign, Sign) -> Sign): Bool = MeetLattice.lowerBound(leq, glb)

    law greatestLowerBound(glb: (Sign, Sign) -> Sign): Bool = MeetLattice.greatestLowerBound(leq, glb)

    law leastElement(leq: (Sign, Sign) -> Bool, bot: Sign): Bool = Bounded.leastElement(bot, leq)

    law greatestElement(leq: (Sign, Sign) -> Bool, top: Sign): Bool = Bounded.greatestElement(top, leq)

    law strict1(f: Sign -> Sign): Bool = Bounded.strict1(f, Bot, Bot)

    law strict2(f: (Sign, Sign) -> Sign): Bool = Bounded.strict2(f, Bot, Bot, Bot)

    law monotone1(f: Sign -> Sign): Bool = PartialOrder.monotone1(f, leq, leq)

    law monotone2(f: (Sign, Sign) -> Sign): Bool = PartialOrder.monotone2(f, leq, leq, leq)

    law safe1(fa: Sign -> Sign, fc: BigInt -> BigInt): Bool = PartialOrder.safe1(fa, fc, alpha, alpha, leq)

    law safe2(fa: (Sign, Sign) -> Sign, fc: (BigInt, BigInt) -> BigInt): Bool = PartialOrder.safe2(fa, fc, alpha, alpha, leq)

}
