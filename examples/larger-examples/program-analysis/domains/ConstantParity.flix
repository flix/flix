///
/// An implementation of the parity domain lifted over the constant propagation domain.
///
mod Domain.ConstantParity {

    ///
    /// The elements of the constant-parity lattice.
    ///
    enum ConstantParity with Eq {
                  case Top,

        case Odd,          case Even,

                case Cst(BigInt),

                  case Bot
    }

    ///
    /// Returns `true` iff `e1` is less than or equal to `e2`.
    ///
//    #reflexive #antiSymmetric #transitive #leastElement(ConstantParity.Bot) #greatestElement(ConstantParity.Top)
    pub def leq(e1: ConstantParity, e2: ConstantParity): Bool = match (e1, e2) {
        case (ConstantParity.Bot, _)                    => true
        case (ConstantParity.Cst(n1), ConstantParity.Cst(n2)) => n1 == n2
        case (ConstantParity.Cst(n), ConstantParity.Odd)      => isOdd(n)
        case (ConstantParity.Cst(n), ConstantParity.Even)     => isEven(n)
        case (ConstantParity.Odd, ConstantParity.Odd)         => true
        case (ConstantParity.Even, ConstantParity.Even)       => true
        case (_, ConstantParity.Top)                          => true
        case _                                                => false
    }

    ///
    /// Returns the least upper bound of `e1` and `e2`.
    ///
//    #upperBound #leastUpperBound #commutative #associative
    pub def lub(e1: ConstantParity, e2: ConstantParity): ConstantParity = match (e1, e2) {
        case (ConstantParity.Bot, x)                          => x
        case (x, ConstantParity.Bot)                          => x
        case (ConstantParity.Cst(n1), ConstantParity.Cst(n2)) =>
            if      (n1 == n2)                  ConstantParity.Cst(n1)
            else if (isOdd(n1)  and isOdd(n2))  ConstantParity.Odd
            else if (isEven(n1) and isEven(n2)) ConstantParity.Even
            else                                ConstantParity.Top
        case (ConstantParity.Cst(n), ConstantParity.Odd)      => if (isOdd(n))  ConstantParity.Odd else ConstantParity.Top
        case (ConstantParity.Odd, ConstantParity.Cst(n))      => if (isOdd(n))  ConstantParity.Odd else ConstantParity.Top
        case (ConstantParity.Cst(n), ConstantParity.Even)     => if (isEven(n)) ConstantParity.Even else ConstantParity.Top
        case (ConstantParity.Even, ConstantParity.Cst(n))     => if (isEven(n)) ConstantParity.Even else ConstantParity.Top
        case (ConstantParity.Odd, ConstantParity.Odd)         => ConstantParity.Odd
        case (ConstantParity.Even, ConstantParity.Even)       => ConstantParity.Even
        case _                                                => ConstantParity.Top
    }

    ///
    /// Returns the greatest lower bound of `e1` and `e2`.
    ///
//    #lowerBound #greatestLowerBound #commutative #associative
    pub def glb(e1: ConstantParity, e2: ConstantParity): ConstantParity = match (e1, e2) {
        case (ConstantParity.Top, x)                          => x
        case (x, ConstantParity.Top)                          => x
        case (ConstantParity.Cst(n1), ConstantParity.Cst(n2)) => if (n1 == n2)  ConstantParity.Cst(n1) else ConstantParity.Bot
        case (ConstantParity.Cst(n), ConstantParity.Odd)      => if (isOdd(n))  ConstantParity.Cst(n) else ConstantParity.Bot
        case (ConstantParity.Odd, ConstantParity.Cst(n))      => if (isOdd(n))  ConstantParity.Cst(n) else ConstantParity.Bot
        case (ConstantParity.Cst(n), ConstantParity.Even)     => if (isEven(n)) ConstantParity.Cst(n) else ConstantParity.Bot
        case (ConstantParity.Even, ConstantParity.Cst(n))     => if (isEven(n)) ConstantParity.Cst(n) else ConstantParity.Bot
        case (ConstantParity.Odd, ConstantParity.Odd)         => ConstantParity.Odd
        case (ConstantParity.Even, ConstantParity.Even)       => ConstantParity.Even
        case _                                                => ConstantParity.Bot
    }

    ///
    /// The lattice height function.
    ///
//    #nonNegative
    pub def height(e: ConstantParity): BigInt = match e {
        case ConstantParity.Top    => 0ii
        case ConstantParity.Odd    => 1ii
        case ConstantParity.Even   => 1ii
        case ConstantParity.Cst(_) => 2ii
        case ConstantParity.Bot    => 3ii
    }

    ///
    /// Abstracts a concrete number into the constant parity domain.
    ///
    pub def alpha(i: BigInt): ConstantParity = ConstantParity.Cst(i)

    ///
    /// Over-approximates integer `increment`.
    ///
//    #safe1(x -> x + 1ii)
//    #strict1 #monotone1
    pub def inc(e: ConstantParity): ConstantParity = match e {
        case ConstantParity.Bot    => ConstantParity.Bot
        case ConstantParity.Cst(n) => ConstantParity.Cst(n + 1ii)
        case ConstantParity.Even   => ConstantParity.Odd
        case ConstantParity.Odd    => ConstantParity.Even
        case ConstantParity.Top    => ConstantParity.Top
    }

    ///
    /// Over-approximates integer `decrement`.
    ///
//    #safe1(x -> x - 1ii)
//    #strict1 #monotone1
    pub def dec(e: ConstantParity): ConstantParity = match e {
        case ConstantParity.Bot    => ConstantParity.Bot
        case ConstantParity.Cst(n) => ConstantParity.Cst(n - 1ii)
        case ConstantParity.Even   => ConstantParity.Odd
        case ConstantParity.Odd    => ConstantParity.Even
        case ConstantParity.Top    => ConstantParity.Top
    }

    ///
    /// Over-approximates integer `addition`.
    ///
//    #safe2((x, y) -> x + y)
//    #strict2 #monotone2 #commutative
    pub def plus(e1: ConstantParity, e2: ConstantParity): ConstantParity = match (e1, e2) {
        case (ConstantParity.Bot, _)                          => ConstantParity.Bot
        case (_, ConstantParity.Bot)                          => ConstantParity.Bot
        case (ConstantParity.Cst(n1), ConstantParity.Cst(n2)) => ConstantParity.Cst(n1 + n2)
        case (ConstantParity.Cst(n), ConstantParity.Odd)      => if (isOdd(n))  ConstantParity.Even else ConstantParity.Odd
        case (ConstantParity.Odd, ConstantParity.Cst(n))      => if (isOdd(n))  ConstantParity.Even else ConstantParity.Odd
        case (ConstantParity.Cst(n), ConstantParity.Even)     => if (isEven(n)) ConstantParity.Even else ConstantParity.Odd
        case (ConstantParity.Even, ConstantParity.Cst(n))     => if (isEven(n)) ConstantParity.Even else ConstantParity.Odd
        case (ConstantParity.Even, ConstantParity.Even)       => ConstantParity.Even
        case (ConstantParity.Even, ConstantParity.Odd)        => ConstantParity.Odd
        case (ConstantParity.Odd, ConstantParity.Even)        => ConstantParity.Odd
        case (ConstantParity.Odd, ConstantParity.Odd)         => ConstantParity.Even
        case _                                                => ConstantParity.Top
    }

    ///
    /// Over-approximates integer `subtraction`.
    ///
//    #safe2((x, y) -> x - y)
//    #strict2 #monotone2
    pub def minus(e1: ConstantParity, e2: ConstantParity): ConstantParity = match (e1, e2) {
        case (ConstantParity.Bot, _)                          => ConstantParity.Bot
        case (_, ConstantParity.Bot)                          => ConstantParity.Bot
        case (ConstantParity.Cst(n1), ConstantParity.Cst(n2)) => ConstantParity.Cst(n1 - n2)
        case (ConstantParity.Cst(n), ConstantParity.Odd)      => if (isOdd(n))  ConstantParity.Even else ConstantParity.Odd
        case (ConstantParity.Odd, ConstantParity.Cst(n))      => if (isOdd(n))  ConstantParity.Even else ConstantParity.Odd
        case (ConstantParity.Cst(n), ConstantParity.Even)     => if (isEven(n)) ConstantParity.Even else ConstantParity.Odd
        case (ConstantParity.Even, ConstantParity.Cst(n))     => if (isEven(n)) ConstantParity.Even else ConstantParity.Odd
        case (ConstantParity.Even, ConstantParity.Even)       => ConstantParity.Even
        case (ConstantParity.Even, ConstantParity.Odd)        => ConstantParity.Odd
        case (ConstantParity.Odd, ConstantParity.Even)        => ConstantParity.Odd
        case (ConstantParity.Odd, ConstantParity.Odd)         => ConstantParity.Even
        case _                                                => ConstantParity.Top
    }

    ///
    /// Over-approximates integer `multiplication`.
    ///
//    #safe2((x, y) -> x * y)
//    #strict2  // NB: @monotone annotation removed since Z3 reports unknown.
    pub def times(e1: ConstantParity, e2: ConstantParity): ConstantParity = match (e1, e2) {
        case (ConstantParity.Bot, _)                          => ConstantParity.Bot
        case (_, ConstantParity.Bot)                          => ConstantParity.Bot
        case (ConstantParity.Cst(n1), ConstantParity.Cst(n2)) => ConstantParity.Cst(n1 * n2)
        case (ConstantParity.Cst(n), ConstantParity.Odd)      => if (isOdd(n)) ConstantParity.Odd else ConstantParity.Even
        case (ConstantParity.Odd, ConstantParity.Cst(n))      => if (isOdd(n)) ConstantParity.Odd else ConstantParity.Even
        case (ConstantParity.Cst(_), ConstantParity.Even)     => ConstantParity.Even
        case (ConstantParity.Even, ConstantParity.Cst(_))     => ConstantParity.Even
        case (ConstantParity.Even, ConstantParity.Even)       => ConstantParity.Even
        case (ConstantParity.Even, ConstantParity.Odd)        => ConstantParity.Even
        case (ConstantParity.Odd, ConstantParity.Even)        => ConstantParity.Even
        case (ConstantParity.Odd, ConstantParity.Odd)         => ConstantParity.Odd
        case _                                                => ConstantParity.Top
    }

    ///
    /// Returns `true` iff the given number `n` is ConstantParity.Odd.
    ///
    pub def isOdd(n: BigInt): Bool = (n `BigInt.remainder` 2ii) != 0ii

    ///
    /// Returns `true` iff the given number `n` is ConstantParity.Even.
    ///
    pub def isEven(n: BigInt): Bool = not isOdd(n)

    //
    // ## Specialized Laws
    //

//    law upperBound(lub: (ConstantParity, ConstantParity) -> ConstantParity): Bool = JoinLattice.leastUpperBound1(leq, lub)

//    law leastUpperBound(lub: (ConstantParity, ConstantParity) -> ConstantParity): Bool = JoinLattice.leastUpperBound2(leq, lub)

//    law lowerBound(glb: (ConstantParity, ConstantParity) -> ConstantParity): Bool = MeetLattice.greatestLowerBound1(leq, glb)

//    law greatestLowerBound(glb: (ConstantParity, ConstantParity) -> ConstantParity): Bool = MeetLattice.greatestLowerBound2(leq, glb)

//    law leastElement(leq: (ConstantParity, ConstantParity) -> Bool, ConstantParity.Bot: ConstantParity): Bool = Bounded.leastElement(ConstantParity.Bot, leq)

//    law greatestElement(leq: (ConstantParity, ConstantParity) -> Bool, ConstantParity.Top: ConstantParity): Bool = Bounded.greatestElement(ConstantParity.Top, leq)

//    law strict1(f: ConstantParity -> ConstantParity): Bool = Bounded.strict1(f, ConstantParity.Bot, ConstantParity.Bot)

//    law strict2(f: (ConstantParity, ConstantParity) -> ConstantParity): Bool = Bounded.strict2(f, ConstantParity.Bot, ConstantParity.Bot, ConstantParity.Bot)

//    law monotone1(f: ConstantParity -> ConstantParity): Bool = PartialOrder.monotone1(f, leq, leq)

//    law monotone2(f: (ConstantParity, ConstantParity) -> ConstantParity): Bool = PartialOrder.monotone2(f, leq, leq, leq)

//    law safe1(fa: ConstantParity -> ConstantParity, fc: BigInt -> BigInt): Bool = PartialOrder.safe1(fa, fc, alpha, alpha, leq)

//    law safe2(fa: (ConstantParity, ConstantParity) -> ConstantParity, fc: (BigInt, BigInt) -> BigInt): Bool = PartialOrder.safe2(fa, fc, alpha, alpha, leq)

}
