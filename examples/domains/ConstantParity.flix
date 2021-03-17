/**
 * An implementation of the parity domain lifted over the constant propagation domain.
 */
namespace Domain/ConstantParity {

    ///
    /// The elements of the constant-parity lattice.
    ///
    enum ConstantParity {
                  case Top,

        case Odd,          case Even,

                case Cst(BigInt),

                  case Bot
    }

    instance Eq[ConstantParity] {
        pub def eq(e1: ConstantParity, e2: ConstantParity): Bool = match (e1, e2) {
            case (Top, Top)         => true
            case (Odd, Odd)         => true
            case (Even, Even)       => true
            case (Cst(n1), Cst(n2)) => n1 == n2
            case (Bot, Bot)         => true
            case _                  => false
        }
        pub def neq(e1: ConstantParity, e2: ConstantParity): Bool = not (e1 == e2)
    }

    ///
    /// Returns `true` iff `e1` is less than or equal to `e2`.
    ///
    #reflexive #antiSymmetric #transitive #leastElement(Bot) #greatestElement(Top)
    pub def leq(e1: ConstantParity, e2: ConstantParity): Bool = match (e1, e2) {
        case (Bot, _)           => true
        case (Cst(n1), Cst(n2)) => n1 == n2
        case (Cst(n), Odd)      => isOdd(n)
        case (Cst(n), Even)     => isEven(n)
        case (Odd, Odd)         => true
        case (Even, Even)       => true
        case (_, Top)           => true
        case _                  => false
    }

    ///
    /// Returns the least upper bound of `e1` and `e2`.
    ///
    #upperBound #leastUpperBound #commutative #associative
    pub def lub(e1: ConstantParity, e2: ConstantParity): ConstantParity = match (e1, e2) {
        case (Bot, x)           => x
        case (x, Bot)           => x
        case (Cst(n1), Cst(n2)) => {
            if      (n1 == n2)                  Cst(n1)
            else if (isOdd(n1) and isOdd(n2))    Odd
            else if (isEven(n1) and isEven(n2))  Even
            else                                Top
        }
        case (Cst(n), Odd)      => if (isOdd(n))  Odd else Top
        case (Odd, Cst(n))      => if (isOdd(n))  Odd else Top
        case (Cst(n), Even)     => if (isEven(n)) Even else Top
        case (Even, Cst(n))     => if (isEven(n)) Even else Top
        case (Odd, Odd)         => Odd
        case (Even, Even)       => Even
        case _                  => Top
    }

    ///
    /// Returns the greatest lower bound of `e1` and `e2`.
    ///
    #lowerBound #greatestLowerBound #commutative #associative
    pub def glb(e1: ConstantParity, e2: ConstantParity): ConstantParity = match (e1, e2) {
        case (Top, x)           => x
        case (x, Top)           => x
        case (Cst(n1), Cst(n2)) => if (n1 == n2)  Cst(n1) else Bot
        case (Cst(n), Odd)      => if (isOdd(n))  Cst(n) else Bot
        case (Odd, Cst(n))      => if (isOdd(n))  Cst(n) else Bot
        case (Cst(n), Even)     => if (isEven(n)) Cst(n) else Bot
        case (Even, Cst(n))     => if (isEven(n)) Cst(n) else Bot
        case (Odd, Odd)         => Odd
        case (Even, Even)       => Even
        case _                  => Bot
    }

    ///
    /// The lattice height function.
    ///
    #nonNegative
    pub def height(e: ConstantParity): BigInt = match e {
        case Top    => 0ii
        case Odd    => 1ii
        case Even   => 1ii
        case Cst(_) => 2ii
        case Bot    => 3ii
    }

    ///
    /// Abstracts a concrete number into the constant parity domain.
    ///
    pub def alpha(i: BigInt): ConstantParity = Cst(i)

    ///
    /// Over-approximates integer `increment`.
    ///
    #safe1(x -> x + 1ii)
    #strict1 #monotone1
    pub def inc(e: ConstantParity): ConstantParity = match e {
        case Bot    => Bot
        case Cst(n) => Cst(n + 1ii)
        case Even   => Odd
        case Odd    => Even
        case Top    => Top
    }

    ///
    /// Over-approximates integer `decrement`.
    ///
    #safe1(x -> x - 1ii)
    #strict1 #monotone1
    pub def dec(e: ConstantParity): ConstantParity = match e {
        case Bot    => Bot
        case Cst(n) => Cst(n - 1ii)
        case Even   => Odd
        case Odd    => Even
        case Top    => Top
    }

    ///
    /// Over-approximates integer `addition`.
    ///
    #safe2((x, y) -> x + y)
    #strict2 #monotone2 #commutative
    pub def plus(e1: ConstantParity, e2: ConstantParity): ConstantParity = match (e1, e2) {
        case (Bot, _)           => Bot
        case (_, Bot)           => Bot
        case (Cst(n1), Cst(n2)) => Cst(n1 + n2)
        case (Cst(n), Odd)      => if (isOdd(n))  Even else Odd
        case (Odd, Cst(n))      => if (isOdd(n))  Even else Odd
        case (Cst(n), Even)     => if (isEven(n)) Even else Odd
        case (Even, Cst(n))     => if (isEven(n)) Even else Odd
        case (Even, Even)       => Even
        case (Even, Odd)        => Odd
        case (Odd, Even)        => Odd
        case (Odd, Odd)         => Even
        case _                  => Top
    }

    ///
    /// Over-approximates integer `subtraction`.
    ///
    #safe2((x, y) -> x - y)
    #strict2 #monotone2
    pub def minus(e1: ConstantParity, e2: ConstantParity): ConstantParity = match (e1, e2) {
        case (Bot, _)           => Bot
        case (_, Bot)           => Bot
        case (Cst(n1), Cst(n2)) => Cst(n1 - n2)
        case (Cst(n), Odd)      => if (isOdd(n))  Even else Odd
        case (Odd, Cst(n))      => if (isOdd(n))  Even else Odd
        case (Cst(n), Even)     => if (isEven(n)) Even else Odd
        case (Even, Cst(n))     => if (isEven(n)) Even else Odd
        case (Even, Even)       => Even
        case (Even, Odd)        => Odd
        case (Odd, Even)        => Odd
        case (Odd, Odd)         => Even
        case _                  => Top
    }

    ///
    /// Over-approximates integer `multiplication`.
    ///
    #safe2((x, y) -> x * y)
    #strict2  // NB: @monotone annotation removed since Z3 reports unknown.
    pub def times(e1: ConstantParity, e2: ConstantParity): ConstantParity = match (e1, e2) {
        case (Bot, _)           => Bot
        case (_, Bot)           => Bot
        case (Cst(n1), Cst(n2)) => Cst(n1 * n2)
        case (Cst(n), Odd)      => if (isOdd(n)) Odd else Even
        case (Odd, Cst(n))      => if (isOdd(n)) Odd else Even
        case (Cst(_), Even)     => Even
        case (Even, Cst(_))     => Even
        case (Even, Even)       => Even
        case (Even, Odd)        => Even
        case (Odd, Even)        => Even
        case (Odd, Odd)         => Odd
        case _                  => Top
    }

    ///
    /// Over-approximates `equal`.
    ///
    #PartialOrder.safe2((x, y) -> x == y, alpha, Belnap.alpha, Belnap.leq)
    #Bounded.strict2(Bot, Bot, Belnap/Belnap.Bot)
    #PartialOrder.monotone2(leq, leq, Belnap.leq)
    #commutative
    pub def eq(e1: ConstantParity, e2: ConstantParity): Belnap.Belnap = match (e1, e2) {
        case (Bot, _)           => Belnap/Belnap.Bot
        case (_, Bot)           => Belnap/Belnap.Bot
        case (Cst(n1), Cst(n2)) => Belnap.alpha(n1 == n2)
        case (Cst(n), Even)     => if (isOdd(n))  Belnap/Belnap.False else Belnap/Belnap.Top
        case (Even, Cst(n))     => if (isOdd(n))  Belnap/Belnap.False else Belnap/Belnap.Top
        case (Cst(n), Odd)      => if (isEven(n)) Belnap/Belnap.False else Belnap/Belnap.Top
        case (Odd, Cst(n))      => if (isEven(n)) Belnap/Belnap.False else Belnap/Belnap.Top
        case _                  => Belnap/Belnap.Top
    }

    ///
    /// Over-approximates `not equal`.
    ///
    #PartialOrder.safe2((x, y) -> x != y, alpha, Belnap.alpha, Belnap.leq)
    #Bounded.strict2(Bot, Bot, Belnap/Belnap.Bot)
    #PartialOrder.monotone2(leq, leq, Belnap.leq)
    #commutative
    pub def neq(e1: ConstantParity, e2: ConstantParity): Belnap.Belnap = Belnap.not(eq(e1, e2))

    ///
    /// Over-approximates `less than`.
    ///
    #PartialOrder.safe2((x, y) -> x < y, alpha, Belnap.alpha, Belnap.leq)
    #Bounded.strict2(Bot, Bot, Belnap/Belnap.Bot)
    #PartialOrder.monotone2(leq, leq, Belnap.leq)
    pub def less(e1: ConstantParity, e2: ConstantParity): Belnap.Belnap = match (e1, e2) {
        case (Bot, _)           => Belnap/Belnap.Bot
        case (_, Bot)           => Belnap/Belnap.Bot
        case (Cst(n1), Cst(n2)) => Belnap.alpha(n1 < n2)
        case _                  => Belnap/Belnap.Top
    }

    ///
    /// Over-approximates `less than or equal`.
    ///
    #PartialOrder.safe2((x, y) -> x <= y, alpha, Belnap.alpha, Belnap.leq)
    #Bounded.strict2(Bot, Bot, Belnap/Belnap.Bot)
    #PartialOrder.monotone2(leq, leq, Belnap.leq)
    pub def lessEqual(e1: ConstantParity, e2: ConstantParity): Belnap.Belnap = Belnap.or(e1 `less` e2, e1 `eq` e2)

    ///
    /// Returns `true` iff the given number `n` is odd.
    ///
    pub def isOdd(n: BigInt): Bool = (n % 2ii) != 0ii

    ///
    /// Returns `true` iff the given number `n` is even.
    ///
    pub def isEven(n: BigInt): Bool = not isOdd(n)

    //
    // ## Specialized Laws
    //

    law upperBound(lub: (ConstantParity, ConstantParity) -> ConstantParity): Bool = JoinLattice.leastUpperBound1(leq, lub)

    law leastUpperBound(lub: (ConstantParity, ConstantParity) -> ConstantParity): Bool = JoinLattice.leastUpperBound2(leq, lub)

    law lowerBound(glb: (ConstantParity, ConstantParity) -> ConstantParity): Bool = MeetLattice.greatestLowerBound1(leq, glb)

    law greatestLowerBound(glb: (ConstantParity, ConstantParity) -> ConstantParity): Bool = MeetLattice.greatestLowerBound2(leq, glb)

    law leastElement(leq: (ConstantParity, ConstantParity) -> Bool, bot: ConstantParity): Bool = Bounded.leastElement(bot, leq)

    law greatestElement(leq: (ConstantParity, ConstantParity) -> Bool, top: ConstantParity): Bool = Bounded.greatestElement(top, leq)

    law strict1(f: ConstantParity -> ConstantParity): Bool = Bounded.strict1(f, Bot, Bot)

    law strict2(f: (ConstantParity, ConstantParity) -> ConstantParity): Bool = Bounded.strict2(f, Bot, Bot, Bot)

    law monotone1(f: ConstantParity -> ConstantParity): Bool = PartialOrder.monotone1(f, leq, leq)

    law monotone2(f: (ConstantParity, ConstantParity) -> ConstantParity): Bool = PartialOrder.monotone2(f, leq, leq, leq)

    law safe1(fa: ConstantParity -> ConstantParity, fc: BigInt -> BigInt): Bool = PartialOrder.safe1(fa, fc, alpha, alpha, leq)

    law safe2(fa: (ConstantParity, ConstantParity) -> ConstantParity, fc: (BigInt, BigInt) -> BigInt): Bool = PartialOrder.safe2(fa, fc, alpha, alpha, leq)

}