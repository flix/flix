/**
 * An implementation of the sign domain.
 *
 * See Cousot & Cousot '77 for details.
 */
namespace Domain.Sign {

    ///
    /// The elements of the sign lattice.
    ///
    /// Notice that Neg and Pos implicitly includes zero.
    ///
    enum Sign {
                  case Top,

        case Neg,          case Pos,

                  case Zer,

                  case Bot
    }

    ///
    /// Returns `true` iff `e1` is less than or equal to `e2`.
    ///
    #reflexive #antiSymmetric #transitive #leastElement(Bot) #greatestElement(Top)
    def leq(e1: Sign, e2: Sign): Bool = match (e1, e2) with {
        case (Bot, _)   => true
        case (Zer, Zer) => true
        case (Zer, Neg) => true
        case (Zer, Pos) => true
        case (Neg, Neg) => true
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
        case (Zer, Zer) => Zer
        case (Zer, Neg) => Neg
        case (Neg, Zer) => Neg
        case (Zer, Pos) => Pos
        case (Pos, Zer) => Pos
        case (Neg, Neg) => Neg
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
        case (Zer, Zer) => Zer
        case (Neg, Neg) => Neg
        case (Pos, Pos) => Pos
        case (Zer, Neg) => Zer
        case (Neg, Zer) => Zer
        case (Zer, Pos) => Zer
        case (Pos, Zer) => Zer
        case (Neg, Pos) => Zer
        case (Pos, Neg) => Zer
        case (x, Top)   => x
        case (Top, x)   => x
        case _          => Bot
    }

    ///
    /// Abstracts a concrete number into the sign domain.
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
        case (Bot, _) => Bot
        case (_, Bot) => Bot
        case (Neg, Neg) => Neg
        case (Neg, Zer) => Neg
        case (Neg, Pos) => Top
        case (Zer, Neg) => Neg
        case (Zer, Zer) => Zer
        case (Zer, Pos) => Pos
        case (Pos, Neg) => Top
        case (Pos, Zer) => Pos
        case (Pos, Pos) => Pos
        case  _         => Top
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
        case  _         => Top
    }

    ///
    /// Over-approximates integer `multiplication`.
    ///
    #safe2((x, y) -> x * y)
    #strict2 #monotone2
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
        case  _         => Top
    }

    ///
    /// Over-approximates integer `division`.
    ///
    /// NB: We cannot prove safety directly because division is not a total function.
    #strict2 #monotone2
    def divide(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Bot, _)   => Bot
        case (_, Bot)   => Bot
        case (Neg, Neg) => Pos
        case (Neg, Zer) => Bot
        case (Neg, Pos) => Neg
        case (Zer, Neg) => Zer
        case (Zer, Zer) => Bot
        case (Zer, Pos) => Zer
        case (Pos, Neg) => Neg
        case (Pos, Zer) => Bot
        case (Pos, Pos) => Pos
        case  _         => Top
    }

    ///
    /// Over-approximates integer `modulus`.
    ///
    /// NB: We cannot prove safety directly because modulus is not a total function.
    #strict2 #monotone2
    def modulo(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Bot, _)   => Bot
        case (_, Bot)   => Bot
        case (Neg, Neg) => Bot
        case (Neg, Zer) => Bot
        case (Neg, Pos) => Top
        case (Zer, Neg) => Bot
        case (Zer, Zer) => Bot
        case (Zer, Pos) => Pos
        case (Pos, Neg) => Top
        case (Pos, Zer) => Bot
        case (Pos, Pos) => Top
        case  _         => Top
    }

    ///
    /// Over-approximates `bitwise negation`.
    ///
    // NB: We cannot verify safety since Z3 does not support bitwise ops on ints.
    #strict1 #monotone1
    def negate(e: Sign): Sign = match e with {
        case Bot => Bot
        case Zer => Neg
        case Neg => Top
        case Pos => Top
        case Top => Top
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
        case (Neg, Pos) => Pos
        case (Zer, Neg) => Zer
        case (Zer, Zer) => Zer
        case (Zer, Pos) => Zer
        case (Pos, Neg) => Pos
        case (Pos, Zer) => Zer
        case (Pos, Pos) => Pos
        case  _         => Top
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
        case (Neg, Pos) => Top
        case (Zer, Neg) => Neg
        case (Zer, Zer) => Zer
        case (Zer, Pos) => Pos
        case (Pos, Neg) => Top
        case (Pos, Zer) => Pos
        case (Pos, Pos) => Pos
        case  _         => Top
    }

    ///
    /// Over-approximates `bitwise xor`.
    ///
    // NB: We cannot verify safety since Z3 does not support bitwise ops on ints.
    #strict2 #monotone2 #commutative #associative
    def xor(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Bot, _)   => Bot
        case (_, Bot)   => Bot
        case (Neg, Neg) => Top
        case (Neg, Zer) => Neg
        case (Neg, Pos) => Top
        case (Zer, Neg) => Neg
        case (Zer, Zer) => Zer
        case (Zer, Pos) => Pos
        case (Pos, Neg) => Top
        case (Pos, Zer) => Pos
        case (Pos, Pos) => Top
        case  _         => Top
    }

    ///
    /// Over-approximates `bitwise left shift`.
    ///
    // NB: We cannot verify safety since Z3 does not support bitwise ops on ints.
    //#safe2((x, y) -> x <<< y)
    #strict2 #monotone2
    def leftShift(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
       case (Bot, _)   => Bot
       case (_, Bot)   => Bot
       case (Neg, Neg) => Top
       case (Neg, Zer) => Neg
       case (Neg, Pos) => Top
       case (Zer, Neg) => Top
       case (Zer, Zer) => Zer
       case (Zer, Pos) => Zer
       case (Pos, Neg) => Top
       case (Pos, Zer) => Pos
       case (Pos, Pos) => Top
       case  _         => Top
   }

    ///
    /// Over-approximates `bitwise right shift`.
    ///
    // NB: We cannot verify safety since Z3 does not support bitwise ops on ints.
    #strict2 #monotone2
    def rightShift(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Bot, _)   => Bot
        case (_, Bot)   => Bot
        case (Neg, Neg) => Top
        case (Neg, Zer) => Neg
        case (Neg, Pos) => Top
        case (Zer, Neg) => Top
        case (Zer, Zer) => Zer
        case (Zer, Pos) => Zer
        case (Pos, Neg) => Top
        case (Pos, Zer) => Pos
        case (Pos, Pos) => Pos
        case  _         => Top
    }

    ///
    /// Over-approximates `equal`.
    ///
    #PartialOrder/safe2((x, y) -> x == y, alpha, Belnap/alpha, Belnap/leq)
    #Bounded/strict2(Bot, Bot, Belnap/Belnap.Bot)
    #PartialOrder/monotone2(leq, leq, Belnap/leq)
    #commutative
    def eq(e1: Sign, e2: Sign): Belnap/Belnap = match (e1, e2) with {
        case (Bot, _)   => Belnap/Belnap.Bot
        case (_, Bot)   => Belnap/Belnap.Bot
        case (Zer, Zer) => Belnap/Belnap.True
        case _          => Belnap/Belnap.Top
    }

    ///
    /// Over-approximates `not equal`.
    ///
    #PartialOrder/safe2((x, y) -> x != y, alpha, Belnap/alpha, Belnap/leq)
    #Bounded/strict2(Bot, Bot, Belnap/Belnap.Bot)
    #PartialOrder/monotone2(leq, leq, Belnap/leq)
    #commutative
    def neq(e1: Sign, e2: Sign): Belnap/Belnap = Belnap/not(eq(e1, e2))

    ///
    /// Over-approximates `less than`.
    ///
    #PartialOrder/safe2((x, y) -> x < y, alpha, Belnap/alpha, Belnap/leq)
    #Bounded/strict2(Bot, Bot, Belnap/Belnap.Bot)
    #PartialOrder/monotone2(leq, leq, Belnap/leq)
    def less(e1: Sign, e2: Sign): Belnap/Belnap = match (e1, e2) with {
        case (Bot, _)   => Belnap/Belnap.Bot
        case (_, Bot)   => Belnap/Belnap.Bot
        case (Zer, Zer) => Belnap/Belnap.False
        case _          => Belnap/Belnap.Top
    }

    ///
    /// Over-approximates `less than or equal`.
    ///
    #PartialOrder/safe2((x, y) -> x <= y, alpha, Belnap/alpha, Belnap/leq)
    #Bounded/strict2(Bot, Bot, Belnap/Belnap.Bot)
    #PartialOrder/monotone2(leq, leq, Belnap/leq)
    def lessEqual(e1: Sign, e2: Sign): Belnap/Belnap = Belnap/or(e1 `less` e2, e1 `eq` e2)

    let Sign<> = (Bot, Top, leq, lub, glb)

    //
    // ## Specialized Laws
    //

    law upperBound(lub: (Sign, Sign) -> Sign): Bool = JoinLattice/upperBound(leq, lub)

    law leastUpperBound(lub: (Sign, Sign) -> Sign): Bool = JoinLattice/leastUpperBound(leq, lub)

    law lowerBound(glb: (Sign, Sign) -> Sign): Bool = MeetLattice/lowerBound(leq, glb)

    law greatestLowerBound(glb: (Sign, Sign) -> Sign): Bool = MeetLattice/greatestLowerBound(leq, glb)

    law leastElement(leq: (Sign, Sign) -> Bool, bot: Sign): Bool = Bounded/leastElement(bot, leq)

    law greatestElement(leq: (Sign, Sign) -> Bool, top: Sign): Bool = Bounded/greatestElement(top, leq)

    law strict1(f: Sign -> Sign): Bool = Bounded/strict1(f, Bot, Bot)

    law strict2(f: (Sign, Sign) -> Sign): Bool = Bounded/strict2(f, Bot, Bot, Bot)

    law monotone1(f: Sign -> Sign): Bool = PartialOrder/monotone1(f, leq, leq)

    law monotone2(f: (Sign, Sign) -> Sign): Bool = PartialOrder/monotone2(f, leq, leq, leq)

    law safe1(fa: Sign -> Sign, fc: BigInt -> BigInt): Bool = PartialOrder/safe1(fa, fc, alpha, alpha, leq)

    law safe2(fa: (Sign, Sign) -> Sign, fc: (BigInt, BigInt) -> BigInt): Bool = PartialOrder/safe2(fa, fc, alpha, alpha, leq)

}
