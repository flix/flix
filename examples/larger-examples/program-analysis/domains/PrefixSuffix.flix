///
/// An implementation of Prefix-Suffix domain.
///
mod Domain.PrefixSuffix {

    ///
    /// The elements of the prefix-suffix lattice.
    ///
    enum PrefixSuffix with Eq {
                   case Top,

        case Pre(Int32), case Suf(Int32),

              case PreSuf(Int32, Int32),

                   case Bot
    }

    ///
    /// Returns `true` iff `e1` is equal to `e2`.
    ///
    pub def equ(e1: PrefixSuffix, e2: PrefixSuffix): Bool = e1 == e2

    ///
    /// Returns `true` iff `e1` is less than or equal to `e2`.
    ///
//    #reflexive #antiSymmetric #transitive #leastElement(Bot) #greatestElement(Top)
    pub def leq(e1: PrefixSuffix, e2: PrefixSuffix): Bool = match (e1, e2) {
        case (PrefixSuffix.Bot, _)                                      => true
        case (PrefixSuffix.PreSuf(p1, s1), PrefixSuffix.PreSuf(p2, s2)) => p1 == p2 and s1 == s2
        case (PrefixSuffix.PreSuf(p1, _), PrefixSuffix.Pre(p2))         => p1 == p2
        case (PrefixSuffix.PreSuf(_, s1), PrefixSuffix.Suf(s2))         => s1 == s2
        case (PrefixSuffix.Pre(p1), PrefixSuffix.Pre(p2))               => p1 == p2
        case (PrefixSuffix.Suf(s1), PrefixSuffix.Suf(s2))               => s1 == s2
        case (_, PrefixSuffix.Top)                                      => true
        case _                                                          => false
    }

    ///
    /// Returns the least upper bound of `e1` and `e2`.
    ///
//    #upperBound #leastUpperBound #commutative #associative
    pub def lub(e1: PrefixSuffix, e2: PrefixSuffix): PrefixSuffix = match (e1, e2) {
        case (PrefixSuffix.Bot, x)                                      => x
        case (x, PrefixSuffix.Bot)                                      => x
        case (PrefixSuffix.PreSuf(p1, s1), PrefixSuffix.PreSuf(p2, s2)) =>
            if      ((p1 == p2) and (s1 == s2))  PrefixSuffix.PreSuf(p1, s1)
            else if ((p1 == p2) and (s1 != s2))  PrefixSuffix.Pre(p1)
            else if ((p1 != p2) and (s1 == s2))  PrefixSuffix.Suf(s1)
            else                                 PrefixSuffix.Top
        case (PrefixSuffix.PreSuf(p1, _), PrefixSuffix.Pre(p2))         => if (p1 == p2) PrefixSuffix.Pre(p1) else PrefixSuffix.Top
        case (PrefixSuffix.Pre(p1), PrefixSuffix.PreSuf(p2, _))         => if (p1 == p2) PrefixSuffix.Pre(p1) else PrefixSuffix.Top
        case (PrefixSuffix.PreSuf(_, s1), PrefixSuffix.Suf(s2))         => if (s1 == s2) PrefixSuffix.Suf(s1) else PrefixSuffix.Top
        case (PrefixSuffix.Suf(s1), PrefixSuffix.PreSuf(_, s2))         => if (s1 == s2) PrefixSuffix.Suf(s1) else PrefixSuffix.Top
        case (PrefixSuffix.Pre(p1), PrefixSuffix.Pre(p2))               => if (p1 == p2) PrefixSuffix.Pre(p1) else PrefixSuffix.Top
        case (PrefixSuffix.Suf(s1), PrefixSuffix.Suf(s2))               => if (s1 == s2) PrefixSuffix.Suf(s1) else PrefixSuffix.Top
        case _                                                          => PrefixSuffix.Top
    }

    ///
    /// Returns the greatest lower bound of `e1` and `e2`.
    ///
//    #lowerBound #greatestLowerBound #commutative #associative
    pub def glb(e1: PrefixSuffix, e2: PrefixSuffix): PrefixSuffix = match (e1, e2) {
        case (PrefixSuffix.Top, x)                                      => x
        case (x, PrefixSuffix.Top)                                      => x
        case (PrefixSuffix.PreSuf(p1, s1), PrefixSuffix.PreSuf(p2, s2)) => if (p1 == p2 and s1 == s2) PrefixSuffix.PreSuf(p1, s1) else PrefixSuffix.Bot
        case (PrefixSuffix.PreSuf(p1, s1), PrefixSuffix.Pre(p2))        => if (p1 == p2) PrefixSuffix.PreSuf(p1, s1) else PrefixSuffix.Bot
        case (PrefixSuffix.Pre(p1), PrefixSuffix.PreSuf(p2, s2))        => if (p1 == p2) PrefixSuffix.PreSuf(p2, s2) else PrefixSuffix.Bot
        case (PrefixSuffix.PreSuf(p1, s1), PrefixSuffix.Suf(s2))        => if (s1 == s2) PrefixSuffix.PreSuf(p1, s1) else PrefixSuffix.Bot
        case (PrefixSuffix.Suf(s1), PrefixSuffix.PreSuf(p2, s2))        => if (s1 == s2) PrefixSuffix.PreSuf(p2, s2) else PrefixSuffix.Bot
        case (PrefixSuffix.Pre(p1), PrefixSuffix.Pre(p2))               => if (p1 == p2) PrefixSuffix.Pre(p1) else PrefixSuffix.Bot
        case (PrefixSuffix.Suf(s1), PrefixSuffix.Suf(s2))               => if (s1 == s2) PrefixSuffix.Suf(s1) else PrefixSuffix.Bot
        case (PrefixSuffix.Pre(p), PrefixSuffix.Suf(s))                 => PrefixSuffix.PreSuf(p, s)
        case (PrefixSuffix.Suf(s), PrefixSuffix.Pre(p))                 => PrefixSuffix.PreSuf(p, s)
        case _                                                          => PrefixSuffix.Bot
    }

    ///
    /// The lattice height function.
    ///
//    #nonNegative #decreasing(equ, leq)
    pub def height(e: PrefixSuffix): BigInt = match e {
        case PrefixSuffix.Top          => 0ii
        case PrefixSuffix.Pre(_)       => 1ii
        case PrefixSuffix.Suf(_)       => 1ii
        case PrefixSuffix.PreSuf(_, _) => 2ii
        case PrefixSuffix.Bot          => 3ii
    }

    ///
    /// Over-approximates `concatenate`.
    ///
//    #strict2 #monotone2 #associative
    pub def concatenate(e1: PrefixSuffix, e2: PrefixSuffix): PrefixSuffix = match (e1, e2) {
        case (PrefixSuffix.Bot, _)                                    => PrefixSuffix.Bot
        case (_, PrefixSuffix.Bot)                                    => PrefixSuffix.Bot
        case (PrefixSuffix.PreSuf(p1, _), PrefixSuffix.PreSuf(_, s2)) => PrefixSuffix.PreSuf(p1, s2)
        case (PrefixSuffix.PreSuf(p1, _), PrefixSuffix.Suf(s2))       => PrefixSuffix.PreSuf(p1, s2)
        case (PrefixSuffix.Pre(p1), PrefixSuffix.PreSuf(_, s2))       => PrefixSuffix.PreSuf(p1, s2)
        case (PrefixSuffix.Pre(p1), PrefixSuffix.Suf(s2))             => PrefixSuffix.PreSuf(p1, s2)
        case (PrefixSuffix.PreSuf(p1, _), PrefixSuffix.Pre(_))        => PrefixSuffix.Pre(p1)
        case (PrefixSuffix.Pre(p1), PrefixSuffix.Pre(_))              => PrefixSuffix.Pre(p1)
        case (PrefixSuffix.Suf(_), PrefixSuffix.PreSuf(_, s2))        => PrefixSuffix.Suf(s2)
        case (PrefixSuffix.Suf(_), PrefixSuffix.Suf(s2))              => PrefixSuffix.Suf(s2)
        case (PrefixSuffix.Suf(_), PrefixSuffix.Pre(_))               => PrefixSuffix.Top
        case (PrefixSuffix.Pre(p1), PrefixSuffix.Top)                 => PrefixSuffix.Pre(p1)
        case (PrefixSuffix.PreSuf(p1, _), PrefixSuffix.Top)           => PrefixSuffix.Pre(p1)
        case (PrefixSuffix.Top, PrefixSuffix.Suf(s2))                 => PrefixSuffix.Suf(s2)
        case (PrefixSuffix.Top, PrefixSuffix.PreSuf(_, s2))           => PrefixSuffix.Suf(s2)
        case _                                                        => PrefixSuffix.Top
    }

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
