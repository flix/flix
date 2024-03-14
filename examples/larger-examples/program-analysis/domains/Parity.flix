//
// An implementation of the parity domain.
//
mod Domain.Parity {

    ///
    /// The elements of the parity lattice.
    ///
    enum Parity with Eq {
                  case Top,

        case Odd,          case Even,

                  case Bot
    }

    ///
    /// Returns `true` iff `e1` is less than or equal to `e2`.
    ///
//    #reflexive #antiSymmetric #transitive #leastElement(Parity.Bot) #greatestElement(Parity.Top)
    pub def leq(e1: Parity, e2: Parity): Bool = match (e1, e2) {
        case (Parity.Bot, _)            => true
        case (Parity.Odd, Parity.Odd)   => true
        case (Parity.Even, Parity.Even) => true
        case (_, Parity.Top)            => true
        case _                          => false
    }

    ///
    /// Returns the least upper bound of `e1` and `e2`.
    ///
//    #commutative #associative #upperBound #leastUpperBound
    pub def lub(e1: Parity, e2: Parity): Parity = match (e1, e2) {
        case (Parity.Bot, x)            => x
        case (x, Parity.Bot)            => x
        case (Parity.Odd, Parity.Odd)   => Parity.Odd
        case (Parity.Even, Parity.Even) => Parity.Even
        case _                          => Parity.Top
    }

    ///
    /// Returns the greatest lower bound of `e1` and `e2`.
    ///
//    #commutative #associative #lowerBound #greatestLowerBound
    pub def glb(e1: Parity, e2: Parity): Parity = match (e1, e2) {
        case (Parity.Top, x)            => x
        case (x, Parity.Top)            => x
        case (Parity.Odd, Parity.Odd)   => Parity.Odd
        case (Parity.Even, Parity.Even) => Parity.Even
        case _                          => Parity.Bot
    }

    ///
    /// The lattice height function.
    ///
//    #nonNegative
    pub def height(e: Parity): BigInt = match e {
        case Parity.Top  => 0ii
        case Parity.Odd  => 1ii
        case Parity.Even => 1ii
        case Parity.Bot  => 2ii
    }

    ///
    /// Abstracts a concrete integer into the Parity domain.
    ///
    pub def alpha(i: Int32): Parity = if (i `Int32.remainder` 2 != 0) Parity.Odd else Parity.Even

    ///
    /// Over-approximates integer `increment`.
    ///
//    #safe1(x -> x + 1)
//    #strict1 #monotone1
    pub def inc(e: Parity): Parity = match e {
        case Parity.Bot  => Parity.Bot
        case Parity.Odd  => Parity.Even
        case Parity.Even => Parity.Odd
        case Parity.Top  => Parity.Top
    }

    ///
    /// Over-approximates integer `decrement`.
    ///
//    #safe1(x -> x - 1)
//    #strict1 #monotone1
    pub def dec(e: Parity): Parity = match e {
        case Parity.Bot  => Parity.Bot
        case Parity.Odd  => Parity.Even
        case Parity.Even => Parity.Odd
        case Parity.Top  => Parity.Top
    }

    ///
    /// Over-approximates integer `addition`.
    ///
//    #safe2((x, y) -> x + y)
//    #strict2 #monotone2 #commutative #associative
    pub def plus(e1: Parity, e2: Parity): Parity = match (e1, e2) {
        case (_, Parity.Bot)            => Parity.Bot
        case (Parity.Bot, _)            => Parity.Bot
        case (Parity.Odd, Parity.Odd)   => Parity.Even
        case (Parity.Odd, Parity.Even)  => Parity.Odd
        case (Parity.Even, Parity.Odd)  => Parity.Odd
        case (Parity.Even, Parity.Even) => Parity.Even
        case _                          => Parity.Top
    }

    ///
    /// Over-approximates integer `subtraction`.
    ///
//    #safe2((x, y) -> x - y)
//    #strict2 #monotone2
    pub def minus(e1: Parity, e2: Parity): Parity = match (e1, e2) {
        case (_, Parity.Bot)            => Parity.Bot
        case (Parity.Bot, _)            => Parity.Bot
        case (Parity.Odd, Parity.Odd)   => Parity.Even
        case (Parity.Odd, Parity.Even)  => Parity.Odd
        case (Parity.Even, Parity.Odd)  => Parity.Odd
        case (Parity.Even, Parity.Even) => Parity.Even
        case _                          => Parity.Top
    }

    ///
    /// Over-approximates integer `multiplication`.
    ///
//    #safe2((x, y) -> x * y)
//    #strict2 #monotone2 #commutative #associative
    pub def times(e1: Parity, e2: Parity): Parity = match (e1, e2) {
        case (_, Parity.Bot)            => Parity.Bot
        case (Parity.Bot, _)            => Parity.Bot
        case (Parity.Odd, Parity.Odd)   => Parity.Odd
        case (Parity.Odd, Parity.Even)  => Parity.Even
        case (Parity.Even, Parity.Odd)  => Parity.Even
        case (Parity.Even, Parity.Even) => Parity.Even
        case _                          => Parity.Top
    }

    ///
    /// Over-approximates integer `division`.
    ///
//    #safe2((x, y) -> x / y)
//    #strict2 #monotone2
    pub def divide(e1: Parity, e2: Parity): Parity = match (e1, e2) {
       case (_, Parity.Bot) => Parity.Bot
       case (Parity.Bot, _) => Parity.Bot
       case _               => Parity.Top
    }

    ///
    /// Over-approximates integer `modulus`.
    ///
//    #safe2((x, y) -> x `BigInt.rem` y)
//    #strict2 #monotone2
    pub def modulo(e1: Parity, e2: Parity): Parity = match (e1, e2) {
       case (_, Parity.Bot)            => Parity.Bot
       case (Parity.Bot, _)            => Parity.Bot
       case (Parity.Odd, Parity.Odd)   => Parity.Top
       case (Parity.Even, Parity.Odd)  => Parity.Top
       case (Parity.Odd, Parity.Even)  => Parity.Odd
       case (Parity.Even, Parity.Even) => Parity.Even
       case _                          => Parity.Top
    }

    ///
    /// Over-approximates `bitwise negation`.
    ///
//    #safe1(x -> ~~~x)
//    #strict1 #monotone1
    pub def negate(e: Parity): Parity = match e {
        case Parity.Bot  => Parity.Bot
        case Parity.Odd  => Parity.Even
        case Parity.Even => Parity.Odd
        case Parity.Top  => Parity.Top
    }

    ///
    /// Over-approximates `bitwise and`.
    ///
//    #safe2((x, y) -> x &&& y)
//    #strict2 #monotone2 #commutative #associative
    pub def band(e1: Parity, e2: Parity): Parity = match (e1, e2) {
       case (_, Parity.Bot)            => Parity.Bot
       case (Parity.Bot, _)            => Parity.Bot
       case (Parity.Odd, Parity.Odd)   => Parity.Odd
       case (Parity.Even, Parity.Odd)  => Parity.Even
       case (Parity.Odd, Parity.Even)  => Parity.Even
       case (Parity.Even, Parity.Even) => Parity.Even
       case _                          => Parity.Top
    }

    ///
    /// Over-approximates `bitwise or`.
    ///
//    #safe2((x, y) -> x ||| y)
//    #strict2 #monotone2 #commutative #associative
    pub def bor(e1: Parity, e2: Parity): Parity = match (e1, e2) {
      case (_, Parity.Bot)            => Parity.Bot
      case (Parity.Bot, _)            => Parity.Bot
      case (Parity.Odd, Parity.Odd)   => Parity.Odd
      case (Parity.Even, Parity.Odd)  => Parity.Odd
      case (Parity.Odd, Parity.Even)  => Parity.Odd
      case (Parity.Even, Parity.Even) => Parity.Even
      case _                          => Parity.Top
    }

    ///
    /// Over-approximates `bitwise xor`.
    ///
//    #safe2((x, y) -> x ^^^ y)
//    #strict2 #monotone2 #commutative #associative
    pub def bxor(e1: Parity, e2: Parity): Parity = match (e1, e2) {
      case (_, Parity.Bot)            => Parity.Bot
      case (Parity.Bot, _)            => Parity.Bot
      case (Parity.Odd, Parity.Odd)   => Parity.Even
      case (Parity.Even, Parity.Odd)  => Parity.Odd
      case (Parity.Odd, Parity.Even)  => Parity.Odd
      case (Parity.Even, Parity.Even) => Parity.Even
      case _                          => Parity.Top
    }

    ///
    /// Over-approximates `bitwise left shift`.
    ///
//    #safe2((x, y) -> Int32.leftShift(x, y))
//    #strict2 #monotone2
    pub def leftShift(e1: Parity, e2: Parity): Parity = match (e1, e2) {
      case (_, Parity.Bot)            => Parity.Bot
      case (Parity.Bot, _)            => Parity.Bot
      case (Parity.Odd, Parity.Odd)   => Parity.Even
      case (Parity.Even, Parity.Odd)  => Parity.Even
      case (Parity.Odd, Parity.Even)  => Parity.Top
      case (Parity.Even, Parity.Even) => Parity.Even
      case _                          => Parity.Top
    }

    ///
    /// Over-approximates `bitwise right shift`.
    ///
//    #safe2((x, y) -> Int32.rightShift(x, y))
//    #strict2 #monotone2
    pub def rightShift(e1: Parity, e2: Parity): Parity = match (e1, e2) {
      case (_, Parity.Bot) => Parity.Bot
      case (Parity.Bot, _) => Parity.Bot
      case _               => Parity.Top
    }

    //
    // ## Specialized Laws
    //

//    law upperBound(lub: (Parity, Parity) -> Parity): Bool = JoinLattice.leastUpperBound1(leq, lub)

//    law leastUpperBound(lub: (Parity, Parity) -> Parity): Bool = JoinLattice.leastUpperBound2(leq, lub)

//    law lowerBound(glb: (Parity, Parity) -> Parity): Bool = MeetLattice.greatestLowerBound1(leq, glb)

//    law greatestLowerBound(glb: (Parity, Parity) -> Parity): Bool = MeetLattice.greatestLowerBound2(leq, glb)

//    law leastElement(leq: (Parity, Parity) -> Bool, bot: Parity): Bool = Bounded.leastElement(bot, leq)

//    law greatestElement(leq: (Parity, Parity) -> Bool, top: Parity): Bool = Bounded.greatestElement(top, leq)

//    law strict1(f: Parity -> Parity): Bool = Bounded.strict1(f, Parity.Bot, Parity.Bot)

//    law strict2(f: (Parity, Parity) -> Parity): Bool = Bounded.strict2(f, Parity.Bot, Parity.Bot, Parity.Bot)

//    law monotone1(f: Parity -> Parity): Bool = PartialOrder.monotone1(f, leq, leq)

//    law monotone2(f: (Parity, Parity) -> Parity): Bool = PartialOrder.monotone2(f, leq, leq, leq)

//    law safe1(fa: Parity -> Parity, fc: Int32 -> Int32): Bool = PartialOrder.safe1(fa, fc, alpha, alpha, leq)

//    law safe2(fa: (Parity, Parity) -> Parity, fc: (Int32, Int32) -> Int32): Bool = PartialOrder.safe2(fa, fc, alpha, alpha, leq)

}
