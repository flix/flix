/**
 * An implementation of the modulo three abstract domain.
 *
 * Note: The Parity domain (e.g. even and odd) can be viewed as the Mod2 domain.
 */
namespace Domain.Mod3 {

    ///
    /// The elements of the modulo three lattice.
    ///
    enum Mod3 {
                   case Top,

        case Zer,  case One,  case Two,

                   case Bot
    }

    ///
    /// Returns `true` iff `e1` is less than or equal to `e2`.
    ///
    #reflexive #antiSymmetric #transitive #leastElement(Bot) #greatestElement(Top)
    def leq(e1: Mod3, e2: Mod3): Bool = match (e1, e2) with {
        case (Bot, _)   => true
        case (Zer, Zer) => true
        case (One, One) => true
        case (Two, Two) => true
        case (_, Top)   => true
        case _          => false
    }

    ///
    /// Returns the least upper bound of `e1` and `e2`.
    ///
    #upperBound #leastUpperBound #commutative #associative
    def lub(e1: Mod3, e2: Mod3): Mod3 = match (e1, e2) with {
        case (Bot, x)   => x
        case (x, Bot)   => x
        case (Zer, Zer) => Zer
        case (One, One) => One
        case (Two, Two) => Two
        case _          => Top
    }

    ///
    /// Returns the greatest lower bound of `e1` and `e2`.
    ///
    #lowerBound #greatestLowerBound #commutative #associative
    def glb(e1: Mod3, e2: Mod3): Mod3 = match (e1, e2) with {
        case (Top, x)   => x
        case (x, Top)   => x
        case (Zer, Zer) => Zer
        case (One, One) => One
        case (Two, Two) => Two
        case _          => Bot
    }

    ///
    /// Abstracts a concrete integer into the Parity domain.
    ///
    def alpha(i: BigInt): Mod3 = switch {
        case i % 3ii == 0ii => Zer
        case i % 3ii == 1ii => One
        case i % 3ii == 2ii => Two
        case true => Bot // TODO: Why is this required? Related to negative numbers???
    }

    ///
    /// Over-approximates integer `increment`.
    ///
    #safe1(x -> x + 1ii)
    #strict1 #monotone1
    def inc(e: Mod3): Mod3 = match e with {
       case Bot => Bot
       case Zer => One
       case One => Two
       case Two => Zer
       case Top => Top
    }

    ///
    /// Over-approximates integer `decrement`.
    ///
    #safe1(x -> x - 1ii)
    #strict1 #monotone1
    def dec(e: Mod3): Mod3 = match e with {
       case Bot => Bot
       case Zer => Two
       case One => Zer
       case Two => One
       case Top => Top
    }

    ///
    /// Over-approximates integer `addition`.
    ///
    #safe2((x, y) -> x + y)
    #strict2 #monotone2 #commutative #associative
    def plus(e1: Mod3, e2: Mod3): Mod3 = match (e1, e2) with {
       case (_, Bot)   => Bot
       case (Bot, _)   => Bot
       case (Zer, x)   => x
       case (x, Zer)   => x
       case (One, One) => Two
       case (One, Two) => Zer
       case (Two, One) => Zer
       case (Two, Two) => One
       case _          => Top
    }

    ///
    /// Over-approximates integer `subtraction`.
    ///
    #safe2((x, y) -> x - y) // TODO: Check if there is a bug here..
    #strict2 #monotone2
    def minus(e1: Mod3, e2: Mod3): Mod3 = match (e1, e2) with {
       case (_, Bot)   => Bot
       case (Bot, _)   => Bot
       case (Zer, x)   => x
       case (x, Zer)   => x
       case (One, One) => Zer
       case (One, Two) => One
       case (Two, One) => One
       case (Two, Two) => Zer
       case _          => Top
    }

    ///
    /// Over-approximates integer `multiplication`.
    ///
    #safe2((x, y) -> x * y)
    #strict2 #monotone2 #commutative #associative
    def times(e1: Mod3, e2: Mod3): Mod3 = match (e1, e2) with {
        case (_, Bot) => Bot
        case (Bot, _) => Bot
        // TODO: rest
        case _        => Top
    }

    ///
    /// Over-approximates integer `division`.
    ///
    /// NB: We cannot prove safety directly because division is not a total function.
    #strict2 #monotone2
    def divide(e1: Mod3, e2: Mod3): Mod3 = match (e1, e2) with {
        case (_, Bot) => Bot
        case (Bot, _) => Bot
        // TODO: rest
        case _        => Top
    }

    ///
    /// Over-approximates integer `modulus`.
    ///
    /// NB: We cannot prove safety directly because division is not a total function.
    #strict2 #monotone2
    def modulo(e1: Mod3, e2: Mod3): Mod3 = match (e1, e2) with {
        case (_, Bot) => Bot
        case (Bot, _) => Bot
        // TODO: rest
        case _        => Top
    }

    ///
    /// Over-approximates `equal`.
    ///
    // TODO: Safety property
    #Bounded/strict2(Bot, Bot, Belnap/Belnap.Bot)
    #PartialOrder/monotone2(leq, leq, Belnap/leq)
    #commutative
    def eq(e1: Mod3, e2: Mod3): Belnap/Belnap = match (e1, e2) with {
        case (Bot, _)   => Belnap/Belnap.Bot
        case (_, Bot)   => Belnap/Belnap.Bot
        case (Zer, One) => Belnap/Belnap.False
        case (Zer, Two) => Belnap/Belnap.False
        case (One, Zer) => Belnap/Belnap.False
        case (One, Two) => Belnap/Belnap.False
        case (Two, Zer) => Belnap/Belnap.False
        case (Two, One) => Belnap/Belnap.False
        case _          => Belnap/Belnap.Top
    }

    ///
    /// Over-approximates `not equal`.
    // TODO: Safety property
    #Bounded/strict2(Bot, Bot, Belnap/Belnap.Bot)
    #PartialOrder/monotone2(leq, leq, Belnap/leq)
    #commutative
    def neq(e1: Mod3, e2: Mod3): Belnap/Belnap = Belnap/not(eq(e1, e2))

    ///
    /// Over-approximates `less than`.
    ///
    // TODO: Safety property
    #Bounded/strict2(Bot, Bot, Belnap/Belnap.Bot)
    #PartialOrder/monotone2(leq, leq, Belnap/leq)
    def less(e1: Mod3, e2: Mod3): Belnap/Belnap = match (e1, e2) with {
        case (Bot, _) => Belnap/Belnap.Bot
        case (_, Bot) => Belnap/Belnap.Bot
        case _        => Belnap/Belnap.Top
    }

    ///
    /// Over-approximates `less than or equal`.
    ///
    // TODO: Safety property
    #Bounded/strict2(Bot, Bot, Belnap/Belnap.Bot)
    #PartialOrder/monotone2(leq, leq, Belnap/leq)
    def lessEqual(e1: Mod3, e2: Mod3): Belnap/Belnap = Belnap/or(e1 `less` e2, e1 `eq` e2)

    let Mod3<> = (Bot, Top, leq, lub, glb)

    //
    // ## Specialized Laws
    //

    law upperBound(lub: (Mod3, Mod3) -> Mod3): Bool = JoinLattice/upperBound(leq, lub)

    law leastUpperBound(lub: (Mod3, Mod3) -> Mod3): Bool = JoinLattice/leastUpperBound(leq, lub)

    law lowerBound(glb: (Mod3, Mod3) -> Mod3): Bool = MeetLattice/lowerBound(leq, glb)

    law greatestLowerBound(glb: (Mod3, Mod3) -> Mod3): Bool = MeetLattice/greatestLowerBound(leq, glb)

    law leastElement(leq: (Mod3, Mod3) -> Bool, bot: Mod3): Bool = Bounded/leastElement(bot, leq)

    law greatestElement(leq: (Mod3, Mod3) -> Bool, top: Mod3): Bool = Bounded/greatestElement(top, leq)

    law strict1(f: Mod3 -> Mod3): Bool = Bounded/strict1(f, Bot, Bot)

    law strict2(f: (Mod3, Mod3) -> Mod3): Bool = Bounded/strict2(f, Bot, Bot, Bot)
    
    law monotone1(f: Mod3 -> Mod3): Bool = PartialOrder/monotone1(f, leq, leq)

    law monotone2(f: (Mod3, Mod3) -> Mod3): Bool = PartialOrder/monotone2(f, leq, leq, leq)

    law safe1(fa: Mod3 -> Mod3, fc: BigInt -> BigInt): Bool = PartialOrder/safe1(fa, fc, alpha, alpha, leq)

    law safe2(fa: (Mod3, Mod3) -> Mod3, fc: (BigInt, BigInt) -> BigInt): Bool = PartialOrder/safe2(fa, fc, alpha, alpha, leq)

}