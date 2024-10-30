///
/// An implementation of the sign domain lifted over the constant propagation domain.
///
mod Domain.ConstantSign {

    ///
    /// The elements of the constant-sign lattice.
    ///
    enum ConstantSign with Eq {
                  case Top,

        case Neg,          case Pos,

                case Cst(BigInt),

                  case Bot
    }

    ///
    /// Returns `true` iff `e1` is less than or equal to `e2`.
    ///
//    #reflexive #antiSymmetric #transitive #leastElement(ConstantSign.Bot) #greatestElement(ConstantSign.Top)
    pub def leq(e1: ConstantSign, e2: ConstantSign): Bool = match (e1, e2) {
        case (ConstantSign.Bot, _)                        => true
        case (ConstantSign.Cst(n1), ConstantSign.Cst(n2)) => n1 == n2
        case (ConstantSign.Cst(n), ConstantSign.Neg)      => n <= 0ii
        case (ConstantSign.Cst(n), ConstantSign.Pos)      => n >= 0ii
        case (ConstantSign.Neg, ConstantSign.Neg)         => true
        case (ConstantSign.Pos, ConstantSign.Pos)         => true
        case (_, ConstantSign.Top)                        => true
        case _                                            => false
    }

    ///
    /// Returns the least upper bound of `e1` and `e2`.
    ///
//    #upperBound #leastUpperBound #commutative #associative
    pub def lub(e1: ConstantSign, e2: ConstantSign): ConstantSign = match (e1, e2) {
        case (ConstantSign.Bot, x)                        => x
        case (x, ConstantSign.Bot)                        => x
        case (ConstantSign.Cst(n1), ConstantSign.Cst(n2)) =>
            if      (n1 == n2)                ConstantSign.Cst(n1)
            else if (n1 >= 0ii and n2 >= 0ii) ConstantSign.Pos
            else if (n1 <= 0ii and n2 <= 0ii) ConstantSign.Neg
            else                              ConstantSign.Top
        case (ConstantSign.Cst(n), ConstantSign.Neg)      => if (n <= 0ii) ConstantSign.Neg else ConstantSign.Top
        case (ConstantSign.Neg, ConstantSign.Cst(n))      => if (n <= 0ii) ConstantSign.Neg else ConstantSign.Top
        case (ConstantSign.Cst(n), ConstantSign.Pos)      => if (n >= 0ii) ConstantSign.Pos else ConstantSign.Top
        case (ConstantSign.Pos, ConstantSign.Cst(n))      => if (n >= 0ii) ConstantSign.Pos else ConstantSign.Top
        case (ConstantSign.Neg, ConstantSign.Neg)         => ConstantSign.Neg
        case (ConstantSign.Pos, ConstantSign.Pos)         => ConstantSign.Pos
        case _                                            => ConstantSign.Top
    }

    ///
    /// Returns the greatest lower bound of `e1` and `e2`.
    ///
//    #lowerBound #greatestLowerBound #commutative #associative
    pub def glb(e1: ConstantSign, e2: ConstantSign): ConstantSign = match (e1, e2) {
        case (ConstantSign.Top, x)                        => x
        case (x, ConstantSign.Top)                        => x
        case (ConstantSign.Cst(n1), ConstantSign.Cst(n2)) => if (n1 == n2) ConstantSign.Cst(n1) else ConstantSign.Bot
        case (ConstantSign.Neg, ConstantSign.Cst(n))      => if (n <= 0ii) ConstantSign.Cst(n) else ConstantSign.Bot
        case (ConstantSign.Cst(n), ConstantSign.Neg)      => if (n <= 0ii) ConstantSign.Cst(n) else ConstantSign.Bot
        case (ConstantSign.Pos, ConstantSign.Cst(n))      => if (n >= 0ii) ConstantSign.Cst(n) else ConstantSign.Bot
        case (ConstantSign.Cst(n), ConstantSign.Pos)      => if (n >= 0ii) ConstantSign.Cst(n) else ConstantSign.Bot
        case (ConstantSign.Neg, ConstantSign.Neg)         => ConstantSign.Neg
        case (ConstantSign.Pos, ConstantSign.Pos)         => ConstantSign.Pos
        case (ConstantSign.Neg, ConstantSign.Pos)         => ConstantSign.Cst(0ii)
        case (ConstantSign.Pos, ConstantSign.Neg)         => ConstantSign.Cst(0ii)
        case _                                            => ConstantSign.Bot
    }

    ///
    /// The lattice height function.
    ///
//    #nonConstantSign.Negative
    pub def height(e: ConstantSign): BigInt = match e {
        case ConstantSign.Top    => 0ii
        case ConstantSign.Neg    => 1ii
        case ConstantSign.Pos    => 1ii
        case ConstantSign.Cst(_) => 2ii
        case ConstantSign.Bot    => 3ii
    }

    ///
    /// Abstracts a concrete number into the domain.
    ///
    pub def alpha(i: BigInt): ConstantSign = ConstantSign.Cst(i)

    ///
    /// Over-approximates integer `increment`.
    ///
//    #safe1(x -> x + 1ii)
//    #strict1 #monotone1
    pub def inc(e: ConstantSign): ConstantSign = match e {
        case ConstantSign.Bot    => ConstantSign.Bot
        case ConstantSign.Cst(n) => ConstantSign.Cst(n + 1ii)
        case ConstantSign.Neg    => ConstantSign.Top
        case ConstantSign.Pos    => ConstantSign.Pos
        case ConstantSign.Top    => ConstantSign.Top
    }

    ///
    /// Over-approximates integer `decrement`.
    ///
//    #safe1(x -> x - 1ii)
//    #strict1 #monotone1
    pub def dec(e: ConstantSign): ConstantSign = match e {
        case ConstantSign.Bot    => ConstantSign.Bot
        case ConstantSign.Cst(n) => ConstantSign.Cst(n - 1ii)
        case ConstantSign.Neg    => ConstantSign.Neg
        case ConstantSign.Pos    => ConstantSign.Top
        case ConstantSign.Top    => ConstantSign.Top
    }

    ///
    /// Over-approximates integer `addition`.
    ///
//    #safe2((x, y) -> x + y)
//    #strict2 #monotone2 #commutative
    pub def plus(e1: ConstantSign, e2: ConstantSign): ConstantSign = match (e1, e2) {
        case (ConstantSign.Bot, _)                        => ConstantSign.Bot
        case (_, ConstantSign.Bot)                        => ConstantSign.Bot
        case (ConstantSign.Cst(n1), ConstantSign.Cst(n2)) => ConstantSign.Cst(n1 + n2)
        case (ConstantSign.Cst(n1), ConstantSign.Neg)     => if (n1 <= 0ii) ConstantSign.Neg else ConstantSign.Top
        case (ConstantSign.Neg, ConstantSign.Cst(n1))     => if (n1 <= 0ii) ConstantSign.Neg else ConstantSign.Top
        case (ConstantSign.Cst(n1), ConstantSign.Pos)     => if (n1 >= 0ii) ConstantSign.Pos else ConstantSign.Top
        case (ConstantSign.Pos, ConstantSign.Cst(n1))     => if (n1 >= 0ii) ConstantSign.Pos else ConstantSign.Top
        case (ConstantSign.Neg, ConstantSign.Neg)         => ConstantSign.Neg
        case (ConstantSign.Pos, ConstantSign.Pos)         => ConstantSign.Pos
        case _                                            => ConstantSign.Top
    }

    ///
    /// Over-approximates integer `subtraction`.
    ///
//    #safe2((x, y) -> x - y)
//    #strict2 #monotone2
    pub def minus(e1: ConstantSign, e2: ConstantSign): ConstantSign = match (e1, e2) {
        case (ConstantSign.Bot, _)                        => ConstantSign.Bot
        case (_, ConstantSign.Bot)                        => ConstantSign.Bot
        case (ConstantSign.Cst(n1), ConstantSign.Cst(n2)) => ConstantSign.Cst(n1 - n2)
        case (ConstantSign.Cst(n1), ConstantSign.Neg)     => if (n1 >= 0ii) ConstantSign.Pos else ConstantSign.Top
        case (ConstantSign.Neg, ConstantSign.Cst(n1))     => if (n1 >= 0ii) ConstantSign.Neg else ConstantSign.Top
        case (ConstantSign.Cst(n1), ConstantSign.Pos)     => if (n1 <= 0ii) ConstantSign.Neg else ConstantSign.Top
        case (ConstantSign.Pos, ConstantSign.Cst(n1))     => if (n1 <= 0ii) ConstantSign.Pos else ConstantSign.Top
        case (ConstantSign.Pos, ConstantSign.Neg)         => ConstantSign.Pos
        case (ConstantSign.Neg, ConstantSign.Pos)         => ConstantSign.Neg
        case _                                            => ConstantSign.Top
    }

    ///
    /// Over-approximates integer `multiplication`.
    ///
//    #safe2((x, y) -> x * y)
//    #strict2 #monotone2 #commutative #associative
    pub def times(e1: ConstantSign, e2: ConstantSign): ConstantSign = match (e1, e2) {
        case (ConstantSign.Bot, _)                        => ConstantSign.Bot
        case (_, ConstantSign.Bot)                        => ConstantSign.Bot
        case (ConstantSign.Cst(n1), ConstantSign.Cst(n2)) => ConstantSign.Cst(n1 * n2)
        case (ConstantSign.Cst(0ii), _)                   => ConstantSign.Cst(0ii)
        case (_, ConstantSign.Cst(0ii))                   => ConstantSign.Cst(0ii)
        case (ConstantSign.Cst(n1), ConstantSign.Neg)     => if (n1 > 0ii) ConstantSign.Neg else ConstantSign.Pos
        case (ConstantSign.Neg, ConstantSign.Cst(n1))     => if (n1 > 0ii) ConstantSign.Neg else ConstantSign.Pos
        case (ConstantSign.Cst(n1), ConstantSign.Pos)     => if (n1 < 0ii) ConstantSign.Neg else ConstantSign.Pos
        case (ConstantSign.Pos, ConstantSign.Cst(n1))     => if (n1 < 0ii) ConstantSign.Neg else ConstantSign.Pos
        case (ConstantSign.Neg, ConstantSign.Neg)         => ConstantSign.Pos
        case (ConstantSign.Pos, ConstantSign.Neg)         => ConstantSign.Neg
        case (ConstantSign.Neg, ConstantSign.Pos)         => ConstantSign.Neg
        case (ConstantSign.Pos, ConstantSign.Pos)         => ConstantSign.Pos
        case _                                            => ConstantSign.Top
    }

    //
    // ## Specialized Laws
    //

//    law upperBound(lub: (ConstantSign, ConstantSign) -> ConstantSign): Bool = JoinLattice.leastUpperBound1(leq, lub)

//    law leastUpperBound(lub: (ConstantSign, ConstantSign) -> ConstantSign): Bool = JoinLattice.leastUpperBound2(leq, lub)

//    law lowerBound(glb: (ConstantSign, ConstantSign) -> ConstantSign): Bool = MeetLattice.greatestLowerBound1(leq, glb)

//    law greatestLowerBound(glb: (ConstantSign, ConstantSign) -> ConstantSign): Bool = MeetLattice.greatestLowerBound2(leq, glb)

//    law leastElement(leq: (ConstantSign, ConstantSign) -> Bool, ConstantSign.Bot: ConstantSign): Bool = Bounded.leastElement(ConstantSign.Bot, leq)

//    law greatestElement(leq: (ConstantSign, ConstantSign) -> Bool, ConstantSign.Top: ConstantSign): Bool = Bounded.greatestElement(ConstantSign.Top, leq)

//    law strict1(f: ConstantSign -> ConstantSign): Bool = Bounded.strict1(f, ConstantSign.Bot, ConstantSign.Bot)

//    law strict2(f: (ConstantSign, ConstantSign) -> ConstantSign): Bool = Bounded.strict2(f, ConstantSign.Bot, ConstantSign.Bot, ConstantSign.Bot)

//    law monotone1(f: ConstantSign -> ConstantSign): Bool = PartialOrder.monotone1(f, leq, leq)

//    law monotone2(f: (ConstantSign, ConstantSign) -> ConstantSign): Bool = PartialOrder.monotone2(f, leq, leq, leq)

//    law safe1(fa: ConstantSign -> ConstantSign, fc: BigInt -> BigInt): Bool = PartialOrder.safe1(fa, fc, alpha, alpha, leq)

//    law safe2(fa: (ConstantSign, ConstantSign) -> ConstantSign, fc: (BigInt, BigInt) -> BigInt): Bool = PartialOrder.safe2(fa, fc, alpha, alpha, leq)

}
