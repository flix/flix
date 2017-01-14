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
    /// Returns `true` iff `e1` is less than or equal to `e2`.
    ///
    #reflexive #antiSymmetric #transitive #leastElement(Bot) #greatestElement(Top)
    def leq(e1: Sign, e2: Sign): Bool = match (e1, e2) with {
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
    def lub(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
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
    def glb(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Top, x)   => x
        case (x, Top)   => x
        case (Neg, Neg) => Neg
        case (Zer, Zer) => Zer
        case (Pos, Pos) => Pos
        case _          => Bot
    }

    ///
    /// Abstracts a concrete integer into the Sign domain.
    ///
    def alpha(i: BigInt): Sign = switch {
        case i < 0ii    => Neg
        case i > 0ii    => Pos
        case true       => Zer
    }

    ///
    /// Over-approximates integer `increment`.
    ///
    #safe1(x -> x + 1ii)
    #strict1 #monotone1
    def inc(e: Sign): Sign = match e with {
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
    def dec(e: Sign): Sign = match e with {
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
    def plus(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
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
    def minus(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
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
    def times(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
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
    /// Over-approximates integer `division`.
    ///
    /// NB: We cannot prove safety directly because division is not a total function.
    #strict2 #monotone2
    def divide(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Bot, _)   => Bot
        case (_, Bot)   => Bot
        case (Neg, Neg) => Top
        case (Neg, Zer) => Bot
        case (Neg, Pos) => Top
        case (Zer, Neg) => Zer
        case (Zer, Zer) => Zer
        case (Zer, Pos) => Zer
        case (Pos, Neg) => Top
        case (Pos, Zer) => Bot
        case (Pos, Pos) => Top
        case _          => Top
    }

    ///
    /// Over-approximates integer `modulus`.
    ///
    // #safe2((x, y) -> x % y) // TODO: Causes timeout ...
    #strict2 #monotone2
    def modulo(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Bot, _)   => Bot
        case (_, Bot)   => Bot
        case (Neg, Neg) => Bot
        case (Neg, Zer) => Bot
        case (Neg, Pos) => Top
        case (Zer, Neg) => Bot
        case (Zer, Zer) => Bot
        case (Zer, Pos) => Zer
        case (Pos, Neg) => Bot
        case (Pos, Zer) => Bot
        case (Pos, Pos) => Top
        case _          => Top
    }

    ///
    /// Over-approximates `bitwise negation`.
    ///
    // NB: We cannot verify safety since Z3 does not support bitwise ops on ints.
    #strict1 #monotone1
    def negate(e: Sign): Sign = match e with {
        case Bot => Bot
        case Neg => Pos
        case Zer => Neg
        case Pos => Neg
        case Top => Top
    }

    ///
    /// Over-approximates `bitwise or`.
    ///
    // NB: We cannot verify safety since Z3 does not support bitwise ops on ints.
    #strict2 #monotone2 #commutative #associative
    def or(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Bot, _)   => Bot
        case (_, Bot)   => Bot
        case (Neg, Neg) => Neg
        case (Neg, Zer) => Neg
        case (Neg, Pos) => Neg
        case (Zer, Neg) => Neg
        case (Zer, Zer) => Zer
        case (Zer, Pos) => Pos
        case (Pos, Neg) => Neg
        case (Pos, Zer) => Pos
        case (Pos, Pos) => Pos
        case _          => Top
    }

    ///
    /// Over-approximates `bitwise and`.
    ///
    // NB: We cannot verify safety since Z3 does not support bitwise ops on ints.
    #strict2 #monotone2 #commutative #associative
    def and(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Bot, _)   => Bot
        case (_, Bot)   => Bot
        case (Neg, Neg) => Neg
        case (Neg, Zer) => Zer
        case (Neg, Pos) => Top
        case (Zer, Neg) => Zer
        case (Zer, Zer) => Zer
        case (Zer, Pos) => Zer
        case (Pos, Neg) => Top
        case (Pos, Zer) => Zer
        case (Pos, Pos) => Top
        case (Zer, Top) => Zer
        case (Top, Zer) => Zer
        case _          => Top
    }

    ///
    /// Over-approximates `bitwise xor`.
    ///
    // NB: We cannot verify safety since Z3 does not support bitwise ops on ints.
    #strict2 #monotone2 #commutative #associative
    def xor(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Bot, _)   => Bot
        case (_, Bot)   => Bot
        case (Zer, Zer) => Zer
        case _          => Top
    }

    ///
    /// Over-approximates `bitwise left shift`.
    ///
    // NB: We cannot verify safety since Z3 does not support bitwise ops on ints.
    #strict2 #monotone2
    def leftShift(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Bot, _)   => Bot
        case (_, Bot)   => Bot
        case (Neg, Neg) => Bot
        case (Neg, Zer) => Neg
        case (Neg, Pos) => Top
        case (Zer, Neg) => Bot
        case (Zer, Zer) => Zer
        case (Zer, Pos) => Zer
        case (Pos, Neg) => Bot
        case (Pos, Zer) => Pos
        case (Pos, Pos) => Top
        case _          => Top
    }

    ///
    /// Over-approximates `bitwise right shift`.
    ///
    // NB: We cannot verify safety since Z3 does not support bitwise ops on ints.
    #strict2 #monotone2
    def rightShift(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Bot, _)   => Bot
        case (_, Bot)   => Bot
        case (Neg, Neg) => Bot
        case (Neg, Zer) => Neg
        case (Neg, Pos) => Pos
        case (Zer, Neg) => Bot
        case (Zer, Zer) => Zer
        case (Zer, Pos) => Zer
        case (Pos, Neg) => Bot
        case (Pos, Zer) => Pos
        case (Pos, Pos) => Top
        case _          => Top
    }

    ///
    /// Over-approximates `equal`.
    ///
    #PartialOrder.safe2((x, y) -> x == y, alpha, Belnap.alpha, Belnap.leq)
    #Bounded.strict2(Bot, Bot, Belnap/Belnap.Bot)
    #PartialOrder.monotone2(leq, leq, Belnap.leq)
    #commutative
    def eq(e1: Sign, e2: Sign): Belnap.Belnap = match (e1, e2) with {
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
    def neq(e1: Sign, e2: Sign): Belnap.Belnap = Belnap.not(eq(e1, e2))

    ///
    /// Over-approximates `less than`.
    ///
    #PartialOrder.safe2((x, y) -> x < y, alpha, Belnap.alpha, Belnap.leq)
    #Bounded.strict2(Bot, Bot, Belnap/Belnap.Bot)
    #PartialOrder.monotone2(leq, leq, Belnap.leq)
    def less(e1: Sign, e2: Sign): Belnap.Belnap = match (e1, e2) with {
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
    def lessEqual(e1: Sign, e2: Sign): Belnap.Belnap = Belnap.or(e1 `less` e2, e1 `eq` e2)

    let Sign<> = (Bot, Top, leq, lub, glb)

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
