/**
 * An implementation of the sign domain lifted over the constant propagation domain.
 */
namespace Domain/ConstantSign {

    ///
    /// The elements of the constant-sign lattice.
    ///
    enum ConstantSign {
                  case Top,

        case Neg,          case Pos,

                case Cst(BigInt),

                  case Bot
    }

    ///
    /// Returns `true` iff `e1` is equal to `e2`.
    ///
    pub def equ(e1: ConstantSign, e2: ConstantSign): Bool = match (e1, e2) {
        case (Top, Top) => true
        case (Neg, Neg) => true
        case (Pos, Pos) => true
        case (Cst(x), Cst(y)) => x == y
        case (Bot, Bot) => true
        case _ => false
    }

    instance PreOrder[ConstantSign] {
        def equ(e1: ConstantSign, e2: ConstantSign): Bool = equ(e1, e2)
    }

    instance Eq[ConstantSign] {
        def eq(e1: ConstantSign, e2: ConstantSign): Bool = equ(e1, e2)
        def neq(e1: ConstantSign, e2: ConstantSign): Bool = not equ(e1, e2)
    }

    ///
    /// Returns `true` iff `e1` is less than or equal to `e2`.
    ///
    #reflexive #antiSymmetric #transitive #leastElement(Bot) #greatestElement(Top)
    pub def leq(e1: ConstantSign, e2: ConstantSign): Bool = match (e1, e2) {
        case (Bot, _)           => true
        case (Cst(n1), Cst(n2)) => n1 == n2
        case (Cst(n), Neg)      => n <= 0ii
        case (Cst(n), Pos)      => n >= 0ii
        case (Neg, Neg)         => true
        case (Pos, Pos)         => true
        case (_, Top)           => true
        case _                  => false
    }

    ///
    /// Returns the least upper bound of `e1` and `e2`.
    ///
    #upperBound #leastUpperBound #commutative #associative
    pub def lub(e1: ConstantSign, e2: ConstantSign): ConstantSign = match (e1, e2) {
        case (Bot, x)           => x
        case (x, Bot)           => x
        case (Cst(n1), Cst(n2)) => {
            if      (n1 == n2)                  Cst(n1)
            else if (n1 >= 0ii and n2 >= 0ii)    Pos
            else if (n1 <= 0ii and n2 <= 0ii)    Neg
            else                                Top
        }
        case (Cst(n), Neg)      => if (n <= 0ii) Neg else Top
        case (Neg, Cst(n))      => if (n <= 0ii) Neg else Top
        case (Cst(n), Pos)      => if (n >= 0ii) Pos else Top
        case (Pos, Cst(n))      => if (n >= 0ii) Pos else Top
        case (Neg, Neg)         => Neg
        case (Pos, Pos)         => Pos
        case _                  => Top
    }

    ///
    /// Returns the greatest lower bound of `e1` and `e2`.
    ///
    #lowerBound #greatestLowerBound #commutative #associative
    pub def glb(e1: ConstantSign, e2: ConstantSign): ConstantSign = match (e1, e2) {
        case (Top, x)           => x
        case (x, Top)           => x
        case (Cst(n1), Cst(n2)) => if (n1 == n2) Cst(n1) else Bot
        case (Neg, Cst(n))      => if (n <= 0ii) Cst(n) else Bot
        case (Cst(n), Neg)      => if (n <= 0ii) Cst(n) else Bot
        case (Pos, Cst(n))      => if (n >= 0ii) Cst(n) else Bot
        case (Cst(n), Pos)      => if (n >= 0ii) Cst(n) else Bot
        case (Neg, Neg)         => Neg
        case (Pos, Pos)         => Pos
        case (Neg, Pos)         => Cst(0ii)
        case (Pos, Neg)         => Cst(0ii)
        case _                  => Bot
    }

    ///
    /// The lattice height function.
    ///
    #nonNegative #decreasing(equ, leq)
    pub def height(e: ConstantSign): BigInt = match e {
        case Top    => 0ii
        case Neg    => 1ii
        case Pos    => 1ii
        case Cst(_) => 2ii
        case Bot    => 3ii
    }

    ///
    /// Abstracts a concrete number into the domain.
    ///
    pub def alpha(i: BigInt): ConstantSign = Cst(i)

    ///
    /// Over-approximates integer `increment`.
    ///
    #safe1(x -> x + 1ii)
    #strict1 #monotone1
    pub def inc(e: ConstantSign): ConstantSign = match e {
        case Bot    => Bot
        case Cst(n) => Cst(n + 1ii)
        case Neg    => Top
        case Pos    => Pos
        case Top    => Top
    }

    ///
    /// Over-approximates integer `decrement`.
    ///
    #safe1(x -> x - 1ii)
    #strict1 #monotone1
    pub def dec(e: ConstantSign): ConstantSign = match e {
        case Bot    => Bot
        case Cst(n) => Cst(n - 1ii)
        case Neg    => Neg
        case Pos    => Top
        case Top    => Top
    }

    ///
    /// Over-approximates integer `addition`.
    ///
    #safe2((x, y) -> x + y)
    #strict2 #monotone2 #commutative
    pub def plus(e1: ConstantSign, e2: ConstantSign): ConstantSign = match (e1, e2) {
        case (Bot, _)           => Bot
        case (_, Bot)           => Bot
        case (Cst(n1), Cst(n2)) => Cst(n1 + n2)
        case (Cst(n1), Neg)     => if (n1 <= 0ii) Neg else Top
        case (Neg, Cst(n1))     => if (n1 <= 0ii) Neg else Top
        case (Cst(n1), Pos)     => if (n1 >= 0ii) Pos else Top
        case (Pos, Cst(n1))     => if (n1 >= 0ii) Pos else Top
        case (Neg, Neg)         => Neg
        case (Pos, Pos)         => Pos
        case _                  => Top
    }

    ///
    /// Over-approximates integer `subtraction`.
    ///
    #safe2((x, y) -> x - y)
    #strict2 #monotone2
    pub def minus(e1: ConstantSign, e2: ConstantSign): ConstantSign = match (e1, e2) {
        case (Bot, _)           => Bot
        case (_, Bot)           => Bot
        case (Cst(n1), Cst(n2)) => Cst(n1 - n2)
        case (Cst(n1), Neg)     => if (n1 >= 0ii) Pos else Top
        case (Neg, Cst(n1))     => if (n1 >= 0ii) Neg else Top
        case (Cst(n1), Pos)     => if (n1 <= 0ii) Neg else Top
        case (Pos, Cst(n1))     => if (n1 <= 0ii) Pos else Top
        case (Pos, Neg)         => Pos
        case (Neg, Pos)         => Neg
        case _                  => Top
    }

    ///
    /// Over-approximates integer `multiplication`.
    ///
    #safe2((x, y) -> x * y)
    #strict2 #monotone2 #commutative #associative
    pub def times(e1: ConstantSign, e2: ConstantSign): ConstantSign = match (e1, e2) {
        case (Bot, _)           => Bot
        case (_, Bot)           => Bot
        case (Cst(n1), Cst(n2)) => Cst(n1 * n2)
        case (Cst(0ii), _)      => Cst(0ii)
        case (_, Cst(0ii))      => Cst(0ii)
        case (Cst(n1), Neg)     => if (n1 > 0ii) Neg else Pos
        case (Neg, Cst(n1))     => if (n1 > 0ii) Neg else Pos
        case (Cst(n1), Pos)     => if (n1 < 0ii) Neg else Pos
        case (Pos, Cst(n1))     => if (n1 < 0ii) Neg else Pos
        case (Neg, Neg)         => Pos
        case (Pos, Neg)         => Neg
        case (Neg, Pos)         => Neg
        case (Pos, Pos)         => Pos
        case _                  => Top
    }

    ///
    /// Over-approximates `equal`.
    ///
    #PartialOrder.safe2((x, y) -> x == y, alpha, Belnap.alpha, Belnap.leq)
    #Bounded.strict2(Bot, Bot, Belnap/Belnap.Bot)
    #PartialOrder.monotone2(leq, leq, Belnap.leq)
    #commutative
    pub def eq(e1: ConstantSign, e2: ConstantSign): Belnap.Belnap = match (e1, e2) {
        case (Bot, _)           => Belnap/Belnap.Bot
        case (_, Bot)           => Belnap/Belnap.Bot
        case (Cst(n1), Cst(n2)) => Belnap.alpha(n1 == n2)
        case (Cst(n1), Neg)     => if (n1 > 0ii) Belnap/Belnap.False else Belnap/Belnap.Top
        case (Neg, Cst(n1))     => if (n1 > 0ii) Belnap/Belnap.False else Belnap/Belnap.Top
        case (Cst(n1), Pos)     => if (n1 < 0ii) Belnap/Belnap.False else Belnap/Belnap.Top
        case (Pos, Cst(n1))     => if (n1 < 0ii) Belnap/Belnap.False else Belnap/Belnap.Top
        case _                  => Belnap/Belnap.Top
    }

    ///
    /// Over-approximates `not equal`.
    ///
    #PartialOrder.safe2((x, y) -> x != y, alpha, Belnap.alpha, Belnap.leq)
    #Bounded.strict2(Bot, Bot, Belnap/Belnap.Bot)
    #PartialOrder.monotone2(leq, leq, Belnap.leq)
    #commutative
    pub def neq(e1: ConstantSign, e2: ConstantSign): Belnap.Belnap = Belnap.not(eq(e1, e2))

    ///
    /// Over-approximates `less than`.
    ///
    #PartialOrder.safe2((x, y) -> x < y, alpha, Belnap.alpha, Belnap.leq)
    #Bounded.strict2(Bot, Bot, Belnap/Belnap.Bot)
    #PartialOrder.monotone2(leq, leq, Belnap.leq)
    pub def less(e1: ConstantSign, e2: ConstantSign): Belnap.Belnap = match (e1, e2) {
        case (Bot, _)           => Belnap/Belnap.Bot
        case (_, Bot)           => Belnap/Belnap.Bot
        case (Cst(n1), Cst(n2)) => Belnap.alpha(n1 < n2)
        case (Cst(n1), Neg)     => if (n1 > 0ii) Belnap/Belnap.False else Belnap/Belnap.Top
        case (Neg, Cst(n1))     => if (n1 > 0ii) Belnap/Belnap.True else Belnap/Belnap.Top
        case (Cst(n1), Pos)     => if (n1 < 0ii) Belnap/Belnap.True else Belnap/Belnap.Top
        case (Pos, Cst(n1))     => if (n1 < 0ii) Belnap/Belnap.False else Belnap/Belnap.Top
        case _                  => Belnap/Belnap.Top
    }

    ///
    /// Over-approximates `less than or equal`.
    ///
    #PartialOrder.safe2((x, y) -> x <= y, alpha, Belnap.alpha, Belnap.leq)
    #Bounded.strict2(Bot, Bot, Belnap/Belnap.Bot)
    #PartialOrder.monotone2(leq, leq, Belnap.leq)
    pub def lessEqual(e1: ConstantSign, e2: ConstantSign): Belnap.Belnap = Belnap.or(e1 `less` e2, e1 `eq` e2)

    //
    // ## Specialized Laws
    //

    law upperBound(lub: (ConstantSign, ConstantSign) -> ConstantSign): Bool = JoinLattice.leastUpperBound1(leq, lub)

    law leastUpperBound(lub: (ConstantSign, ConstantSign) -> ConstantSign): Bool = JoinLattice.leastUpperBound2(leq, lub)

    law lowerBound(glb: (ConstantSign, ConstantSign) -> ConstantSign): Bool = MeetLattice.greatestLowerBound1(leq, glb)

    law greatestLowerBound(glb: (ConstantSign, ConstantSign) -> ConstantSign): Bool = MeetLattice.greatestLowerBound2(leq, glb)

    law leastElement(leq: (ConstantSign, ConstantSign) -> Bool, bot: ConstantSign): Bool = Bounded.leastElement(bot, leq)

    law greatestElement(leq: (ConstantSign, ConstantSign) -> Bool, top: ConstantSign): Bool = Bounded.greatestElement(top, leq)

    law strict1(f: ConstantSign -> ConstantSign): Bool = Bounded.strict1(f, Bot, Bot)

    law strict2(f: (ConstantSign, ConstantSign) -> ConstantSign): Bool = Bounded.strict2(f, Bot, Bot, Bot)

    law monotone1(f: ConstantSign -> ConstantSign): Bool = PartialOrder.monotone1(f, leq, leq)

    law monotone2(f: (ConstantSign, ConstantSign) -> ConstantSign): Bool = PartialOrder.monotone2(f, leq, leq, leq)

    law safe1(fa: ConstantSign -> ConstantSign, fc: BigInt -> BigInt): Bool = PartialOrder.safe1(fa, fc, alpha, alpha, leq)

    law safe2(fa: (ConstantSign, ConstantSign) -> ConstantSign, fc: (BigInt, BigInt) -> BigInt): Bool = PartialOrder.safe2(fa, fc, alpha, alpha, leq)

}