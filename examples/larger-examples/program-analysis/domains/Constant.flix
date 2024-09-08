///
/// An implementation of the constant propagation lattice.
///
mod Domain.Constant {

    ///
    /// The elements of the constant propagation lattice.
    ///
    enum Constant with Eq, ToString {
          case Top,

        case Cst(Int32),

          case Bot
    }

    ///
    /// Returns `true` iff `e1` is less than or equal to `e2`.
    ///
//    #reflexive #antiSymmetric #transitive #leastElement(Constant.Bot) #greatestElement(Constant.Top)
    pub def leq(e1: Constant, e2: Constant): Bool =
        match (e1, e2) {
            case (Constant.Bot, _)                    => true
            case (Constant.Cst(n1), Constant.Cst(n2)) => n1 == n2
            case (_, Constant.Top)                    => true
            case _                                    => false
        }

    ///
    /// Returns the least upper bound of `e1` and `e2`.
    ///
//    #upperBound #leastUpperBound #commutative #associative
    pub def lub(e1: Constant, e2: Constant): Constant =
        match (e1, e2) {
            case (Constant.Bot, x)                    => x
            case (x, Constant.Bot)                    => x
            case (Constant.Cst(n1), Constant.Cst(n2)) => if (n1 == n2) e1 else Constant.Top
            case _                                    => Constant.Top
        }

    ///
    /// Returns the greatest lower bound of `e1` and `e2`.
    ///
//    #lowerBound #greatestLowerBound #commutative #associative
    pub def glb(e1: Constant, e2: Constant): Constant =
        match (e1, e2) {
            case (Constant.Top, x)                    => x
            case (x, Constant.Top)                    => x
            case (Constant.Cst(n1), Constant.Cst(n2)) => if (n1 == n2) e1 else Constant.Bot
            case _                                    => Constant.Bot
        }

    ///
    /// The lattice height function.
    ///
//    #nonNegative
    pub def height(e: Constant): BigInt =
        match e {
            case Constant.Top    => 0ii
            case Constant.Cst(_) => 1ii
            case Constant.Bot    => 2ii
        }

    ///
    /// Abstracts a concrete integer into the Sign domain.
    ///
    pub def alpha(i: Int32): Constant = Constant.Cst(i)

    ///
    /// Over-approximates integer `increment`.
    ///
//    #safe1(x -> x + 1)
//    #strict1 #monotone1
    pub def inc(e: Constant): Constant =
        match e {
            case Constant.Bot    => Constant.Bot
            case Constant.Cst(n) => Constant.Cst(n + 1)
            case Constant.Top    => Constant.Top
        }

    ///
    /// Over-approximates integer `decrement`.
    ///
//    #safe1(x -> x - 1)
//    #strict1 #monotone1
    pub def dec(e: Constant): Constant =
        match e {
            case Constant.Bot    => Constant.Bot
            case Constant.Cst(n) => Constant.Cst(n - 1)
            case Constant.Top    => Constant.Top
        }

    ///
    /// Over-approximates integer `addition`.
    ///
//    #safe2((x, y) -> x + y)
//    #strict2 #monotone2 #commutative #associative
    pub def plus(e1: Constant, e2: Constant): Constant =
        match (e1, e2) {
            case (Constant.Bot, _)                    => Constant.Bot
            case (_, Constant.Bot)                    => Constant.Bot
            case (Constant.Cst(n1), Constant.Cst(n2)) => Constant.Cst(n1 + n2)
            case _                                    => Constant.Top
        }

    ///
    /// Over-approximates integer `subtraction`.
    ///
//    #safe2((x, y) -> x - y)
//    #strict2 #monotone2
    pub def minus(e1: Constant, e2: Constant): Constant =
        match (e1, e2) {
            case (Constant.Bot, _)                    => Constant.Bot
            case (_, Constant.Bot)                    => Constant.Bot
            case (Constant.Cst(n1), Constant.Cst(n2)) => Constant.Cst(n1 - n2)
            case _                                    => Constant.Top
        }

    ///
    /// Over-approximates integer `multiplication`.
    ///
//    #safe2((x, y) -> x * y)
//    #strict2 #monotone2 #commutative #associative
    pub def times(e1: Constant, e2: Constant): Constant =
        match (e1, e2) {
            case (Constant.Bot, _)                    => Constant.Bot
            case (_, Constant.Bot)                    => Constant.Bot
            case (Constant.Cst(n1), Constant.Cst(n2)) => Constant.Cst(n1 * n2)
            case _                                    => Constant.Top
        }

    ///
    /// Over-approximates integer `division`.
    ///
//    #safe2((x, y) -> x / y)
//    #strict2 #monotone2
    pub def divide(e1: Constant, e2: Constant): Constant =
        match (e1, e2) {
            case (Constant.Bot, _)                    => Constant.Bot
            case (_, Constant.Bot)                    => Constant.Bot
            case (Constant.Cst(n1), Constant.Cst(n2)) => Constant.Cst(n1 / n2)
            case _                                    => Constant.Top
        }

    ///
    /// Over-approximates integer `modulus`.
    ///
//    #safe2((x, y) -> x `remainder` y)
//    #strict2 #monotone2
    pub def modulo(e1: Constant, e2: Constant): Constant =
        match (e1, e2) {
            case (Constant.Bot, _)                    => Constant.Bot
            case (_, Constant.Bot)                    => Constant.Bot
            case (Constant.Cst(n1), Constant.Cst(n2)) => Constant.Cst(n1 `Int32.remainder` n2)
            case _                                    => Constant.Top
        }

    ///
    /// Over-approximates `bitwise negation`.
    ///
//    #safe1(x -> ~~~x)
//    #strict1 #monotone1
    pub def negate(e: Constant): Constant =
        match e {
            case Constant.Bot    => Constant.Bot
            case Constant.Cst(n) => Constant.Cst(Int32.bitwiseNot(n))
            case Constant.Top    => Constant.Top
        }

    ///
    /// Over-approximates `bitwise and`.
    ///
//    #safe2((x, y) -> x &&& y)
//    #strict2 #monotone2 #commutative #associative
    pub def band(e1: Constant, e2: Constant): Constant =
        match (e1, e2) {
            case (Constant.Bot, _)                    => Constant.Bot
            case (_, Constant.Bot)                    => Constant.Bot
            case (Constant.Cst(n1), Constant.Cst(n2)) => Constant.Cst(n1 `Int32.bitwiseAnd` n2)
            case _                                    => Constant.Top
        }

    ///
    /// Over-approximates `bitwise or`.
    ///
//    #safe2((x, y) -> x ||| y)
//    #strict2 #monotone2 #commutative #associative
    pub def bor(e1: Constant, e2: Constant): Constant =
        match (e1, e2) {
            case (Constant.Bot, _)                    => Constant.Bot
            case (_, Constant.Bot)                    => Constant.Bot
            case (Constant.Cst(n1), Constant.Cst(n2)) => Constant.Cst(n1 `Int32.bitwiseOr` n2)
            case _                                    => Constant.Top
        }

    ///
    /// Over-approximates `bitwise xor`.
    ///
//    #safe2((x, y) -> x ^^^ y)
//    #strict2 #monotone2 #commutative #associative
    pub def bxor(e1: Constant, e2: Constant): Constant =
        match (e1, e2) {
            case (Constant.Bot, _)                    => Constant.Bot
            case (_, Constant.Bot)                    => Constant.Bot
            case (Constant.Cst(n1), Constant.Cst(n2)) => Constant.Cst(n1 `Int32.bitwiseXor` n2)
            case _                                    => Constant.Top
        }

    ///
    /// Over-approximates `bitwise left shift`.
    ///
//    #safe2((x, y) -> Int32.shiftLeft(x, y))
//    #strict2 #monotone2
    pub def leftShift(e1: Constant, e2: Constant): Constant =
        match (e1, e2) {
            case (Constant.Bot, _)                    => Constant.Bot
            case (_, Constant.Bot)                    => Constant.Bot
            case (Constant.Cst(n1), Constant.Cst(n2)) => Constant.Cst(Int32.leftShift(n1, n2))
            case _                                    => Constant.Top
        }

    ///
    /// Over-approximates `bitwise right shift`.
    ///
    ///
//    #safe2((x, y) -> Int32.shiftRight(x, y))
//    #strict2 #monotone2
    pub def rightShift(e1: Constant, e2: Constant): Constant =
        match (e1, e2) {
            case (Constant.Bot, _)                    => Constant.Bot
            case (_, Constant.Bot)                    => Constant.Bot
            case (Constant.Cst(n1), Constant.Cst(n2)) => Constant.Cst(Int32.rightShift(n1, n2))
            case _                                    => Constant.Top
        }

    instance LowerBound[Constant] {
        pub def minValue(): Constant = Constant.Bot
    }

    instance PartialOrder[Constant] {
        pub def lessEqual(x: Constant, y: Constant): Bool = leq(x, y)
    }

    instance JoinLattice[Constant] {
        pub def leastUpperBound(x: Constant, y: Constant): Constant = lub(x, y)
    }

    instance MeetLattice[Constant] {
        pub def greatestLowerBound(x: Constant, y: Constant): Constant = glb(x, y)
    }

    //
    // ## Specialized Laws
    //

//    law upperBound(lub: (Constant, Constant) -> Constant): Bool = JoinLattice.leastUpperBound1(leq, lub)

//    law leastUpperBound(lub: (Constant, Constant) -> Constant): Bool = JoinLattice.leastUpperBound2(leq, lub)

//    law lowerBound(glb: (Constant, Constant) -> Constant): Bool = MeetLattice.greatestLowerBound1(leq, glb)

//    law greatestLowerBound(glb: (Constant, Constant) -> Constant): Bool = MeetLattice.greatestLowerBound2(leq, glb)

//    law leastElement(leq: (Constant, Constant) -> Bool, bot: Constant): Bool = Bounded.leastElement(bot, leq)

//    law greatestElement(leq: (Constant, Constant) -> Bool, top: Constant): Bool = Bounded.greatestElement(top, leq)

//    law strict1(f: Constant -> Constant): Bool = Bounded.strict1(f, Constant.Bot, Constant.Bot)

//    law strict2(f: (Constant, Constant) -> Constant): Bool = Bounded.strict2(f, Constant.Bot, Constant.Bot, Constant.Bot)

//    law monotone1(f: Constant -> Constant): Bool = PartialOrder.monotone1(f, leq, leq)

//    law monotone2(f: (Constant, Constant) -> Constant): Bool = PartialOrder.monotone2(f, leq, leq, leq)

//    law safe1(fa: Constant -> Constant, fc: Int32 -> Int32): Bool = PartialOrder.safe1(fa, fc, alpha, alpha, leq)

//    law safe2(fa: (Constant, Constant) -> Constant, fc: (Int32, Int32) -> Int32): Bool = PartialOrder.safe2(fa, fc, alpha, alpha, leq)

}
