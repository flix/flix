/**
 * An implementation of the constant propagation lattice.
 */
namespace Domain/Constant {

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
//    #reflexive #antiSymmetric #transitive #leastElement(Bot) #greatestElement(Top)
    pub def leq(e1: Constant, e2: Constant): Bool = match (e1, e2) {
        case (Bot, _)           => true
        case (Cst(n1), Cst(n2)) => n1 == n2
        case (_, Top)           => true
        case _                  => false
    }

    ///
    /// Returns the least upper bound of `e1` and `e2`.
    ///
//    #upperBound #leastUpperBound #commutative #associative
    pub def lub(e1: Constant, e2: Constant): Constant = match (e1, e2) {
        case (Bot, x)           => x
        case (x, Bot)           => x
        case (Cst(n1), Cst(n2)) => if (n1 == n2) e1 else Top
        case _                  => Top
    }

    ///
    /// Returns the greatest lower bound of `e1` and `e2`.
    ///
//    #lowerBound #greatestLowerBound #commutative #associative
    pub def glb(e1: Constant, e2: Constant): Constant = match (e1, e2) {
        case (Top, x)           => x
        case (x, Top)           => x
        case (Cst(n1), Cst(n2)) => if (n1 == n2) e1 else Bot
        case _                  => Bot
    }

    ///
    /// The lattice height function.
    ///
//    #nonNegative
    pub def height(e: Constant): BigInt = match e {
        case Top    => 0ii
        case Cst(_) => 1ii
        case Bot    => 2ii
    }

    ///
    /// Abstracts a concrete integer into the Sign domain.
    ///
    pub def alpha(i: Int32): Constant = Cst(i)

    /**
     * Over-approximates integer `increment`.
     */
//    #safe1(x -> x + 1)
//    #strict1 #monotone1
    pub def inc(e: Constant): Constant = match e {
        case Bot    => Bot
        case Cst(n) => Cst(n + 1)
        case Top    => Top
    }

    ///
    /// Over-approximates integer `decrement`.
    ///
//    #safe1(x -> x - 1)
//    #strict1 #monotone1
    pub def dec(e: Constant): Constant = match e {
        case Bot    => Bot
        case Cst(n) => Cst(n - 1)
        case Top    => Top
    }

    ///
    /// Over-approximates integer `addition`.
    ///
//    #safe2((x, y) -> x + y)
//    #strict2 #monotone2 #commutative #associative
    pub def plus(e1: Constant, e2: Constant): Constant = match (e1, e2) {
        case (Bot, _)           => Bot
        case (_, Bot)           => Bot
        case (Cst(n1), Cst(n2)) => Cst(n1 + n2)
        case _                  => Top
    }

    ///
    /// Over-approximates integer `subtraction`.
    ///
//    #safe2((x, y) -> x - y)
//    #strict2 #monotone2
    pub def minus(e1: Constant, e2: Constant): Constant = match (e1, e2) {
        case (Bot, _)           => Bot
        case (_, Bot)           => Bot
        case (Cst(n1), Cst(n2)) => Cst(n1 - n2)
        case _                  => Top
    }

    ///
    /// Over-approximates integer `multiplication`.
    ///
//    #safe2((x, y) -> x * y)
//    #strict2 #monotone2 #commutative #associative
    pub def times(e1: Constant, e2: Constant): Constant = match (e1, e2) {
        case (Bot, _)           => Bot
        case (_, Bot)           => Bot
        case (Cst(n1), Cst(n2)) => Cst(n1 * n2)
        case _                  => Top
    }

    ///
    /// Over-approximates integer `division`.
    ///
//    #safe2((x, y) -> x / y)
//    #strict2 #monotone2
    pub def divide(e1: Constant, e2: Constant): Constant = match (e1, e2) {
        case (Bot, _)           => Bot
        case (_, Bot)           => Bot
        case (Cst(n1), Cst(n2)) => Cst(n1 / n2)
        case _                  => Top
    }

    ///
    /// Over-approximates integer `modulus`.
    ///
//    #safe2((x, y) -> x `rem` y)
//    #strict2 #monotone2
    pub def modulo(e1: Constant, e2: Constant): Constant = match (e1, e2) {
        case (Bot, _)           => Bot
        case (_, Bot)           => Bot
        case (Cst(n1), Cst(n2)) => Cst(n1 `rem` n2)
        case _                  => Top
    }

    ///
    /// Over-approximates `bitwise negation`.
    ///
//    #safe1(x -> ~~~x)
//    #strict1 #monotone1
    pub def negate(e: Constant): Constant = match e {
        case Bot       => Bot
        case Cst(n)    => Cst(~~~ n)
        case Top       => Top
    }

    ///
    /// Over-approximates `bitwise and`.
    ///
//    #safe2((x, y) -> x &&& y)
//    #strict2 #monotone2 #commutative #associative
    pub def and(e1: Constant, e2: Constant): Constant = match (e1, e2) {
        case (Bot, _)           => Bot
        case (_, Bot)           => Bot
        case (Cst(n1), Cst(n2)) => Cst(n1 &&& n2)
        case _                  => Top
    }

    ///
    /// Over-approximates `bitwise or`.
    ///
//    #safe2((x, y) -> x ||| y)
//    #strict2 #monotone2 #commutative #associative
    pub def or(e1: Constant, e2: Constant): Constant = match (e1, e2) {
        case (Bot, _)           => Bot
        case (_, Bot)           => Bot
        case (Cst(n1), Cst(n2)) => Cst(n1 ||| n2)
        case _                  => Top
    }

    ///
    /// Over-approximates `bitwise xor`.
    ///
//    #safe2((x, y) -> x ^^^ y)
//    #strict2 #monotone2 #commutative #associative
    pub def xor(e1: Constant, e2: Constant): Constant = match (e1, e2) {
        case (Bot, _)           => Bot
        case (_, Bot)           => Bot
        case (Cst(n1), Cst(n2)) => Cst(n1 ^^^ n2)
        case _                  => Top
    }

    ///
    /// Over-approximates `bitwise left shift`.
    ///
//    #safe2((x, y) -> Int32.shiftLeft(x, y))
//    #strict2 #monotone2
    pub def leftShift(e1: Constant, e2: Constant): Constant = match (e1, e2) {
        case (Bot, _)           => Bot
        case (_, Bot)           => Bot
        case (Cst(n1), Cst(n2)) => Cst(Int32.shiftLeft(n1, n2))
        case _                  => Top
    }

    ///
    /// Over-approximates `bitwise right shift`.
    ///
    ///
//    #safe2((x, y) -> Int32.shiftRight(x, y))
//    #strict2 #monotone2
    pub def rightShift(e1: Constant, e2: Constant): Constant = match (e1, e2) {
        case (Bot, _)           => Bot
        case (_, Bot)           => Bot
        case (Cst(n1), Cst(n2)) => Cst(Int32.shiftRight(n1, n2))
        case _                  => Top
    }

    ///
    /// Over-approximates `equal`.
    ///
//    #PartialOrder.safe2((x, y) -> x == y, alpha, Belnap.alpha, Belnap.leq)
//    #Bounded.strict2(Bot, Bot, Belnap/Belnap.Bot)
//    #PartialOrder.monotone2(leq, leq, Belnap.leq)
//    #commutative
    pub def eq(e1: Constant, e2: Constant): Belnap.Belnap = match (e1, e2) {
        case (Bot, _)           => Belnap/Belnap.Bot
        case (_, Bot)           => Belnap/Belnap.Bot
        case (Cst(n1), Cst(n2)) => Belnap.alpha(n1 == n2)
        case _                  => Belnap/Belnap.Top
    }

    ///
    /// Over-approximates `not equal`.
    ///
//    #PartialOrder.safe2((x, y) -> x != y, alpha, Belnap.alpha, Belnap.leq)
//    #Bounded.strict2(Bot, Bot, Belnap/Belnap.Bot)
//    #PartialOrder.monotone2(leq, leq, Belnap.leq)
//    #commutative
    pub def neq(e1: Constant, e2: Constant): Belnap.Belnap = Belnap.not(eq(e1, e2))

    ///
    /// Over-approximates `less than`.
    ///
//    #PartialOrder.safe2((x, y) -> x < y, alpha, Belnap.alpha, Belnap.leq)
//    #Bounded.strict2(Bot, Bot, Belnap/Belnap.Bot)
//    #PartialOrder.monotone2(leq, leq, Belnap.leq)
    pub def less(e1: Constant, e2: Constant): Belnap.Belnap = match (e1, e2) {
        case (Bot, _)           => Belnap/Belnap.Bot
        case (_, Bot)           => Belnap/Belnap.Bot
        case (Cst(n1), Cst(n2)) => Belnap.alpha(n1 < n2)
        case _                  => Belnap/Belnap.Top
    }

    ///
    /// Over-approximates `less than or equal`.
    ///
//    #PartialOrder.safe2((x, y) -> x <= y, alpha, Belnap.alpha, Belnap.leq)
//    #Bounded.strict2(Bot, Bot, Belnap/Belnap.Bot)
//    #PartialOrder.monotone2(leq, leq, Belnap.leq)
    pub def lessEqual(e1: Constant, e2: Constant): Belnap.Belnap = Belnap.or(e1 `less` e2, e1 `eq` e2)

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

//    law strict1(f: Constant -> Constant): Bool = Bounded.strict1(f, Bot, Bot)

//    law strict2(f: (Constant, Constant) -> Constant): Bool = Bounded.strict2(f, Bot, Bot, Bot)

//    law monotone1(f: Constant -> Constant): Bool = PartialOrder.monotone1(f, leq, leq)

//    law monotone2(f: (Constant, Constant) -> Constant): Bool = PartialOrder.monotone2(f, leq, leq, leq)

//    law safe1(fa: Constant -> Constant, fc: Int32 -> Int32): Bool = PartialOrder.safe1(fa, fc, alpha, alpha, leq)

//    law safe2(fa: (Constant, Constant) -> Constant, fc: (Int32, Int32) -> Int32): Bool = PartialOrder.safe2(fa, fc, alpha, alpha, leq)

}
