/**
 * An implementation of Prefix-Suffix domain.
 */
namespace Domain/PrefixSuffix {

    ///
    /// The elements of the prefix-suffix lattice.
    ///
    enum PrefixSuffix {
                   case Top,

        case Pre(Int), case Suf(Int),

              case PreSuf(Int, Int),

                   case Bot
    }

    ///
    /// Returns `true` iff `e1` is equal to `e2`.
    ///
    pub def equ(e1: PrefixSuffix, e2: PrefixSuffix): Bool = e1 == e2

    instance Eq[PrefixSuffix] {
        pub def eq(e1: PrefixSuffix, e2: PrefixSuffix): Bool = equ(e1, e2)
    }

    ///
    /// Returns `true` iff `e1` is less than or equal to `e2`.
    ///
//    #reflexive #antiSymmetric #transitive #leastElement(Bot) #greatestElement(Top)
    pub def leq(e1: PrefixSuffix, e2: PrefixSuffix): Bool = match (e1, e2) {
        case (Bot, _)                           => true
        case (PreSuf(p1, s1), PreSuf(p2, s2))   => p1 == p2 and s1 == s2
        case (PreSuf(p1, _), Pre(p2))           => p1 == p2
        case (PreSuf(_, s1), Suf(s2))           => s1 == s2
        case (Pre(p1), Pre(p2))                 => p1 == p2
        case (Suf(s1), Suf(s2))                 => s1 == s2
        case (_, Top)                           => true
        case _                                  => false
    }

    ///
    /// Returns the least upper bound of `e1` and `e2`.
    ///
//    #upperBound #leastUpperBound #commutative #associative
    pub def lub(e1: PrefixSuffix, e2: PrefixSuffix): PrefixSuffix = match (e1, e2) {
        case (Bot, x)                           => x
        case (x, Bot)                           => x
        case (PreSuf(p1, s1), PreSuf(p2, s2))   => {
            if      ((p1 == p2) and (s1 == s2))  PreSuf(p1, s1)
            else if ((p1 == p2) and (s1 != s2))  Pre(p1)
            else if ((p1 != p2) and (s1 == s2))  Suf(s1)
            else                                Top
        }
        case (PreSuf(p1, _), Pre(p2))           => if (p1 == p2) Pre(p1) else Top
        case (Pre(p1), PreSuf(p2, _))           => if (p1 == p2) Pre(p1) else Top
        case (PreSuf(_, s1), Suf(s2))           => if (s1 == s2) Suf(s1) else Top
        case (Suf(s1), PreSuf(_, s2))           => if (s1 == s2) Suf(s1) else Top
        case (Pre(p1), Pre(p2))                 => if (p1 == p2) Pre(p1) else Top
        case (Suf(s1), Suf(s2))                 => if (s1 == s2) Suf(s1) else Top
        case _                                  => Top
    }

    ///
    /// Returns the greatest lower bound of `e1` and `e2`.
    ///
//    #lowerBound #greatestLowerBound #commutative #associative
    pub def glb(e1: PrefixSuffix, e2: PrefixSuffix): PrefixSuffix = match (e1, e2) {
        case (Top, x)                           => x
        case (x, Top)                           => x
        case (PreSuf(p1, s1), PreSuf(p2, s2))   => if (p1 == p2 and s1 == s2) PreSuf(p1, s1) else Bot
        case (PreSuf(p1, s1), Pre(p2))          => if (p1 == p2) PreSuf(p1, s1) else Bot
        case (Pre(p1), PreSuf(p2, s2))          => if (p1 == p2) PreSuf(p2, s2) else Bot
        case (PreSuf(p1, s1), Suf(s2))          => if (s1 == s2) PreSuf(p1, s1) else Bot
        case (Suf(s1), PreSuf(p2, s2))          => if (s1 == s2) PreSuf(p2, s2) else Bot
        case (Pre(p1), Pre(p2))                 => if (p1 == p2) Pre(p1) else Bot
        case (Suf(s1), Suf(s2))                 => if (s1 == s2) Suf(s1) else Bot
        case (Pre(p), Suf(s))                   => PreSuf(p, s)
        case (Suf(s), Pre(p))                   => PreSuf(p, s)
        case _                                  => Bot
    }

    ///
    /// The lattice height function.
    ///
//    #nonNegative #decreasing(equ, leq)
    pub def height(e: PrefixSuffix): BigInt = match e {
        case Top            => 0ii
        case Pre(_)         => 1ii
        case Suf(_)         => 1ii
        case PreSuf(_, _)   => 2ii
        case Bot            => 3ii
    }

    ///
    /// Over-approximates `concatenate`.
    ///
//    #strict2 #monotone2 #associative
    pub def concatenate(e1: PrefixSuffix, e2: PrefixSuffix): PrefixSuffix = match (e1, e2) {
        case (Bot, _)                       => Bot
        case (_, Bot)                       => Bot
        case (PreSuf(p1, _), PreSuf(_, s2)) => PreSuf(p1, s2)
        case (PreSuf(p1, _), Suf(s2))       => PreSuf(p1, s2)
        case (Pre(p1), PreSuf(_, s2))       => PreSuf(p1, s2)
        case (Pre(p1), Suf(s2))             => PreSuf(p1, s2)
        case (PreSuf(p1, _), Pre(_))        => Pre(p1)
        case (Pre(p1), Pre(_))              => Pre(p1)
        case (Suf(_), PreSuf(_, s2))        => Suf(s2)
        case (Suf(_), Suf(s2))              => Suf(s2)
        case (Suf(_), Pre(_))               => Top
        case (Pre(p1), Top)                 => Pre(p1)
        case (PreSuf(p1, _), Top)           => Pre(p1)
        case (Top, Suf(s2))                 => Suf(s2)
        case (Top, PreSuf(_, s2))           => Suf(s2)
        case _                              => Top
    }

    ///
    /// Over-approximates `equal`.
    ///
//    #Bounded.strict2(Bot, Bot, Belnap/Belnap.Bot)
//    #PartialOrder.monotone2(leq, leq, Belnap.leq)
//    #commutative
    pub def eq(e1: PrefixSuffix, e2: PrefixSuffix): Belnap.Belnap = match (e1, e2) {
        case (Bot, _)            => Belnap/Belnap.Bot
        case (_, Bot)            => Belnap/Belnap.Bot
        case (PreSuf(p1, s1), PreSuf(p2, s2)) => {
            if      (p1 != p2)      Belnap/Belnap.False
            else if (s1 != s2)      Belnap/Belnap.False
            else                    Belnap/Belnap.Top
        }
        case (Pre(p1), Pre(p2))           => if (p1 != p2) Belnap/Belnap.False else Belnap/Belnap.Top
        case (Suf(s1), Suf(s2))           => if (s1 != s2) Belnap/Belnap.False else Belnap/Belnap.Top
        case (Pre(p1), PreSuf(p2, _))     => if (p1 != p2) Belnap/Belnap.False else Belnap/Belnap.Top
        case (PreSuf(p1, _), Pre(p2))     => if (p1 != p2) Belnap/Belnap.False else Belnap/Belnap.Top
        case (Suf(s1), PreSuf(_, s2))     => if (s1 != s2) Belnap/Belnap.False else Belnap/Belnap.Top
        case (PreSuf(_, s1), Suf(s2))     => if (s1 != s2) Belnap/Belnap.False else Belnap/Belnap.Top
        case _                                  => Belnap/Belnap.Top
    }

    ///
    /// Over-approximates `not equal`.
    ///
//    #Bounded.strict2(Bot, Bot, Belnap/Belnap.Bot)
//    #PartialOrder.monotone2(leq, leq, Belnap.leq)
//    #commutative
    pub def neq(e1: PrefixSuffix, e2: PrefixSuffix): Belnap.Belnap = Belnap.not(eq(e1, e2))

    //
    // ## Specialized Laws
    //

//    law upperBound(lub: (PrefixSuffix, PrefixSuffix) -> PrefixSuffix): Bool = JoinLattice.leastUpperBound1(leq, lub)

//    law leastUpperBound(lub: (PrefixSuffix, PrefixSuffix) -> PrefixSuffix): Bool = JoinLattice.leastUpperBound2(leq, lub)

//    law lowerBound(glb: (PrefixSuffix, PrefixSuffix) -> PrefixSuffix): Bool = MeetLattice.greatestLowerBound1(leq, glb)

//    law greatestLowerBound(glb: (PrefixSuffix, PrefixSuffix) -> PrefixSuffix): Bool = MeetLattice.greatestLowerBound2(leq, glb)

//    law leastElement(leq: (PrefixSuffix, PrefixSuffix) -> Bool, bot: PrefixSuffix): Bool = Bounded.leastElement(bot, leq)

//    law greatestElement(leq: (PrefixSuffix, PrefixSuffix) -> Bool, top: PrefixSuffix): Bool = Bounded.greatestElement(top, leq)

//    law strict1(f: PrefixSuffix -> PrefixSuffix): Bool = Bounded.strict1(f, Bot, Bot)

//    law strict2(f: (PrefixSuffix, PrefixSuffix) -> PrefixSuffix): Bool = Bounded.strict2(f, Bot, Bot, Bot)

//    law monotone1(f: PrefixSuffix -> PrefixSuffix): Bool = PartialOrder.monotone1(f, leq, leq)

//    law monotone2(f: (PrefixSuffix, PrefixSuffix) -> PrefixSuffix): Bool = PartialOrder.monotone2(f, leq, leq, leq)

}