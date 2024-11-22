///
/// An implementation of the four-valued boolean lattice named for Nuel Belnap.
///
mod Belnap {

    ///
    /// The elements of the boolean lattice.
    ///
    pub enum Belnap with Eq {
                  case Top,

        case True,         case False,

                  case Bot
    }

    ///
    /// Returns `true` iff `e1` is less than or equal to `e2`.
    ///
//    #reflexive #antiSymmetric #transitive #leastElement(Bot) #greatestElement(Top)
    pub def leq(e1: Belnap, e2: Belnap): Bool =
        use Belnap.{Top, True, False, Bot};
        match (e1, e2) {
            case (Bot, _)       => true
            case (True, True)   => true
            case (False, False) => true
            case (_, Top)       => true
            case _              => false
        }

    ///
    /// Returns the least upper bound of `e1` and `e2`.
    ///
//    #upperBound #leastUpperBound #commutative #associative
    pub def lub(e1: Belnap, e2: Belnap): Belnap =
        use Belnap.{Top, True, False, Bot};
        match (e1, e2) {
            case (Bot, x)       => x
            case (x, Bot)       => x
            case (True, True)   => True
            case (False, False) => False
            case _              => Top
        }

    ///
    /// Returns the greatest lower bound of `e1` and `e2`.
    ///
//    #lowerBound #greatestLowerBound #commutative #associative
    pub def glb(e1: Belnap, e2: Belnap): Belnap =
        use Belnap.{Top, True, False, Bot};
        match (e1, e2) {
            case (Top, x)       => x
            case (x, Top)       => x
            case (True, True)   => True
            case (False, False) => False
            case _              => Bot
        }

    ///
    /// The lattice height function.
    ///
//    #nonNegative
    pub def height(e: Belnap): BigInt =
        use Belnap.{Top, True, False, Bot};
        match e {
            case Top    => 0ii
            case True   => 1ii
            case False  => 1ii
            case Bot    => 2ii
        }

    ///
    /// Abstracts a concrete boolean into the Belnap domain.
    ///
    pub def alpha(b: Bool): Belnap = if (b) Belnap.True else Belnap.False

    ///
    /// Over-approximates logical negation.
    ///
//    #safe1(x -> neg x)
//    #strict1 #monotone1
    pub def neg(e: Belnap): Belnap =
        use Belnap.{Top, True, False, Bot};
        match e {
            case Bot    => Bot
            case True   => False
            case False  => True
            case Top    => Top
        }

    ///
    /// Over-approximates the logical `and` operator.
    ///
//    #safe2((x, y) -> x and y)
//    #strict2 #monotone2 #commutative #associative
    pub def band(e1: Belnap, e2: Belnap): Belnap =
        use Belnap.{Top, True, False, Bot};
        match (e1, e2) {
            case (Bot, _)       => Bot
            case (_, Bot)       => Bot
            case (True, True)   => True
            case (True, False)  => False
            case (False, True)  => False
            case (False, False) => False
            case _              => Top
        }

    ///
    /// Over-approximates the logical `or` operator.
    ///
//    #safe2((x, y) -> x or y)
//    #strict2 #monotone2 #commutative #associative
    pub def bor(e1: Belnap, e2: Belnap): Belnap =
        use Belnap.{Top, True, False, Bot};
        match (e1, e2) {
            case (Bot, _)       => Bot
            case (_, Bot)       => Bot
            case (True, True)   => True
            case (True, False)  => True
            case (False, True)  => True
            case (False, False) => False
            case _              => Top
        }

    ///
    /// Over-approximates the logical `xor` operator.
    ///
//    #safe2((x, y) -> x ⊕ y)
//    #strict2 #monotone2 #commutative #associative
    pub def bxor(e1: Belnap, e2: Belnap): Belnap =
        use Belnap.{Top, True, False, Bot};
        match (e1, e2) {
            case (Bot, _)       => Bot
            case (_, Bot)       => Bot
            case (True, True)   => False
            case (True, False)  => True
            case (False, True)  => True
            case (False, False) => False
            case _              => Top
        }

    ///
    /// Over-approximates the logical `implies` operator.
    ///
//    #safe2((x, y) -> x → y)
//    #strict2 #monotone2
    pub def implies(e1: Belnap, e2: Belnap): Belnap =
        use Belnap.{Top, True, False, Bot};
        match (e1, e2) {
            case (Bot, _)         => Bot
            case (_, Bot)         => Bot
            case (True, True)     => True
            case (True, False)    => False
            case (False, True)    => True
            case (False, False)   => True
            case _                => Top
        }

    ///
    /// Over-approximates the logical `bicondition` operator.
    ///
//    #safe2((x, y) -> x ↔ y)
//    #strict2 #monotone2 #commutative #associative
    pub def bicondition(e1: Belnap, e2: Belnap): Belnap =
        use Belnap.{Top, True, False, Bot};
        match (e1, e2) {
            case (Bot, _)       => Bot
            case (_, Bot)       => Bot
            case (True, True)   => True
            case (True, False)  => False
            case (False, True)  => False
            case (False, False) => True
            case _              => Top
        }

    //
    // ## Specialized Laws
    //

//    law upperBound(lub: (Belnap, Belnap) -> Belnap): Bool = JoinLattice.leastUpperBound1(leq, lub)

//    law leastUpperBound(lub: (Belnap, Belnap) -> Belnap): Bool = JoinLattice.leastUpperBound2(leq, lub)

//    law lowerBound(glb: (Belnap, Belnap) -> Belnap): Bool = MeetLattice.greatestLowerBound1(leq, glb)

//    law greatestLowerBound(glb: (Belnap, Belnap) -> Belnap): Bool = MeetLattice.greatestLowerBound2(leq, glb)

//    law leastElement(leq: (Belnap, Belnap) -> Bool, bot: Belnap): Bool = Bounded.leastElement(bot, leq)

//    law greatestElement(leq: (Belnap, Belnap) -> Bool, top: Belnap): Bool = Bounded.greatestElement(top, leq)

//    law strict1(f: Belnap -> Belnap): Bool = Bounded.strict1(f, Bot, Bot)

//    law strict2(f: (Belnap, Belnap) -> Belnap): Bool = Bounded.strict2(f, Bot, Bot, Bot)

//    law monotone1(f: Belnap -> Belnap): Bool =  PartialOrder.monotone1(f, leq, leq)

//    law monotone2(f: (Belnap, Belnap) -> Belnap): Bool =  PartialOrder.monotone2(f, leq, leq, leq)

//    law safe1(fa: Belnap -> Belnap, fc: Bool -> Bool): Bool = PartialOrder.safe1(fa, fc, alpha, alpha, leq)

//    law safe2(fa: (Belnap, Belnap) -> Belnap, fc: (Bool, Bool) -> Bool): Bool = PartialOrder.safe2(fa, fc, alpha, alpha, leq)

}
