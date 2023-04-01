//
// An implementation of the parity domain.
//
namespace Domain/Parity {

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
//    #reflexive #antiSymmetric #transitive #leastElement(Bot) #greatestElement(Top)
    pub def leq(e1: Parity, e2: Parity): Bool = match (e1, e2) {
        case (Bot, _)       => true
        case (Odd, Odd)     => true
        case (Even, Even)   => true
        case (_, Top)       => true
        case _              => false
    }

    ///
    /// Returns the least upper bound of `e1` and `e2`.
    ///
//    #commutative #associative #upperBound #leastUpperBound
    pub def lub(e1: Parity, e2: Parity): Parity = match (e1, e2) {
        case (Bot, x)       => x
        case (x, Bot)       => x
        case (Odd, Odd)     => Odd
        case (Even, Even)   => Even
        case _              => Top
    }

    ///
    /// Returns the greatest lower bound of `e1` and `e2`.
    ///
//    #commutative #associative #lowerBound #greatestLowerBound
    pub def glb(e1: Parity, e2: Parity): Parity = match (e1, e2) {
        case (Top, x)       => x
        case (x, Top)       => x
        case (Odd, Odd)     => Odd
        case (Even, Even)   => Even
        case _              => Bot
    }

    ///
    /// The lattice height function.
    ///
//    #nonNegative
    pub def height(e: Parity): BigInt = match e {
        case Top    => 0ii
        case Odd    => 1ii
        case Even   => 1ii
        case Bot    => 2ii
    }

    ///
    /// Abstracts a concrete integer into the Parity domain.
    ///
    pub def alpha(i: Int32): Parity = if (i `rem` 2 != 0) Odd else Even

    ///
    /// Over-approximates integer `increment`.
    ///
//    #safe1(x -> x + 1)
//    #strict1 #monotone1
    pub def inc(e: Parity): Parity = match e {
        case Bot  => Bot
        case Odd  => Even
        case Even => Odd
        case Top  => Top
    }

    ///
    /// Over-approximates integer `decrement`.
    ///
//    #safe1(x -> x - 1)
//    #strict1 #monotone1
    pub def dec(e: Parity): Parity = match e {
        case Bot  => Bot
        case Odd  => Even
        case Even => Odd
        case Top  => Top
    }

    ///
    /// Over-approximates integer `addition`.
    ///
//    #safe2((x, y) -> x + y)
//    #strict2 #monotone2 #commutative #associative
    pub def plus(e1: Parity, e2: Parity): Parity = match (e1, e2) {
        case (_, Bot)       => Bot
        case (Bot, _)       => Bot
        case (Odd, Odd)     => Even
        case (Odd, Even)    => Odd
        case (Even, Odd)    => Odd
        case (Even, Even)   => Even
        case _              => Top
    }

    ///
    /// Over-approximates integer `subtraction`.
    ///
//    #safe2((x, y) -> x - y)
//    #strict2 #monotone2
    pub def minus(e1: Parity, e2: Parity): Parity = match (e1, e2) {
        case (_, Bot)       => Bot
        case (Bot, _)       => Bot
        case (Odd, Odd)     => Even
        case (Odd, Even)    => Odd
        case (Even, Odd)    => Odd
        case (Even, Even)   => Even
        case _              => Top
    }

    ///
    /// Over-approximates integer `multiplication`.
    ///
//    #safe2((x, y) -> x * y)
//    #strict2 #monotone2 #commutative #associative
    pub def times(e1: Parity, e2: Parity): Parity = match (e1, e2) {
        case (_, Bot)       => Bot
        case (Bot, _)       => Bot
        case (Odd, Odd)     => Odd
        case (Odd, Even)    => Even
        case (Even, Odd)    => Even
        case (Even, Even)   => Even
        case _              => Top
    }

    ///
    /// Over-approximates integer `division`.
    ///
//    #safe2((x, y) -> x / y)
//    #strict2 #monotone2
    pub def divide(e1: Parity, e2: Parity): Parity = match (e1, e2) {
       case (_, Bot)    => Bot
       case (Bot, _)    => Bot
       case _           => Top
    }

    ///
    /// Over-approximates integer `modulus`.
    ///
//    #safe2((x, y) -> x `rem` y)
//    #strict2 #monotone2
    pub def modulo(e1: Parity, e2: Parity): Parity = match (e1, e2) {
       case (_, Bot)        => Bot
       case (Bot, _)        => Bot
       case (Odd, Odd)      => Top
       case (Even, Odd)     => Top
       case (Odd, Even)     => Odd
       case (Even, Even)    => Even
       case _               => Top
    }

    ///
    /// Over-approximates `bitwise negation`.
    ///
//    #safe1(x -> ~~~x)
//    #strict1 #monotone1
    pub def negate(e: Parity): Parity = match e {
        case Bot    => Bot
        case Odd    => Even
        case Even   => Odd
        case Top    => Top
    }

    ///
    /// Over-approximates `bitwise and`.
    ///
//    #safe2((x, y) -> x &&& y)
//    #strict2 #monotone2 #commutative #associative
    pub def and(e1: Parity, e2: Parity): Parity = match (e1, e2) {
       case (_, Bot)        => Bot
       case (Bot, _)        => Bot
       case (Odd, Odd)      => Odd
       case (Even, Odd)     => Even
       case (Odd, Even)     => Even
       case (Even, Even)    => Even
       case _               => Top
    }

    ///
    /// Over-approximates `bitwise or`.
    ///
//    #safe2((x, y) -> x ||| y)
//    #strict2 #monotone2 #commutative #associative
    pub def or(e1: Parity, e2: Parity): Parity = match (e1, e2) {
      case (_, Bot)         => Bot
      case (Bot, _)         => Bot
      case (Odd, Odd)       => Odd
      case (Even, Odd)      => Odd
      case (Odd, Even)      => Odd
      case (Even, Even)     => Even
      case _                => Top
    }

    ///
    /// Over-approximates `bitwise xor`.
    ///
//    #safe2((x, y) -> x ^^^ y)
//    #strict2 #monotone2 #commutative #associative
    pub def xor(e1: Parity, e2: Parity): Parity = match (e1, e2) {
      case (_, Bot)         => Bot
      case (Bot, _)         => Bot
      case (Odd, Odd)       => Even
      case (Even, Odd)      => Odd
      case (Odd, Even)      => Odd
      case (Even, Even)     => Even
      case _                => Top
    }

    ///
    /// Over-approximates `bitwise left shift`.
    ///
//    #safe2((x, y) -> Int32.leftShift(x, y))
//    #strict2 #monotone2
    pub def leftShift(e1: Parity, e2: Parity): Parity = match (e1, e2) {
      case (_, Bot)         => Bot
      case (Bot, _)         => Bot
      case (Odd, Odd)       => Even
      case (Even, Odd)      => Even
      case (Odd, Even)      => Top
      case (Even, Even)     => Even
      case _                => Top
    }

    ///
    /// Over-approximates `bitwise right shift`.
    ///
//    #safe2((x, y) -> Int32.rightShift(x, y))
//    #strict2 #monotone2
    pub def rightShift(e1: Parity, e2: Parity): Parity = match (e1, e2) {
      case (_, Bot)     => Bot
      case (Bot, _)     => Bot
      case _            => Top
    }

    ///
    /// Over-approximates `equal`.
    ///
//    #PartialOrder.safe2((x, y) -> x == y, alpha, Belnap.alpha, Belnap.leq)
//    #Bounded.strict2(Bot, Bot, Belnap/Belnap.Bot)
//    #PartialOrder.monotone2(leq, leq, Belnap.leq)
//    #commutative
    pub def eq(e1: Parity, e2: Parity): Belnap.Belnap = match (e1, e2) {
        case (Bot, _)       => Belnap/Belnap.Bot
        case (_, Bot)       => Belnap/Belnap.Bot
        case (Odd, Even)    => Belnap/Belnap.False
        case (Even, Odd)    => Belnap/Belnap.False
        case _              => Belnap/Belnap.Top
    }

    ///
    /// Over-approximates `not equal`.
    ///
//    #PartialOrder.safe2((x, y) -> x != y, alpha, Belnap.alpha, Belnap.leq)
//    #Bounded.strict2(Bot, Bot, Belnap/Belnap.Bot)
//    #PartialOrder.monotone2(leq, leq, Belnap.leq)
//    #commutative
    pub def neq(e1: Parity, e2: Parity): Belnap.Belnap = Belnap.not(eq(e1, e2))

    ///
    /// Over-approximates `less than`.
    ///
//    #PartialOrder.safe2((x, y) -> x < y, alpha, Belnap.alpha, Belnap.leq)
//    #Bounded.strict2(Bot, Bot, Belnap/Belnap.Bot)
//    #PartialOrder.monotone2(leq, leq, Belnap.leq)
    pub def less(e1: Parity, e2: Parity): Belnap.Belnap = match (e1, e2) {
        case (Bot, _)   => Belnap/Belnap.Bot
        case (_, Bot)   => Belnap/Belnap.Bot
        case _          => Belnap/Belnap.Top
    }

    ///
    /// Over-approximates `less than or equal`.
    ///
//    #PartialOrder.safe2((x, y) -> x <= y, alpha, Belnap.alpha, Belnap.leq)
//    #Bounded.strict2(Bot, Bot, Belnap/Belnap.Bot)
//    #PartialOrder.monotone2(leq, leq, Belnap.leq)
    pub def lessEqual(e1: Parity, e2: Parity): Belnap.Belnap = Belnap.or(e1 `less` e2, e1 `eq` e2)

    //
    // ## Specialized Laws
    //

//    law upperBound(lub: (Parity, Parity) -> Parity): Bool = JoinLattice.leastUpperBound1(leq, lub)

//    law leastUpperBound(lub: (Parity, Parity) -> Parity): Bool = JoinLattice.leastUpperBound2(leq, lub)

//    law lowerBound(glb: (Parity, Parity) -> Parity): Bool = MeetLattice.greatestLowerBound1(leq, glb)

//    law greatestLowerBound(glb: (Parity, Parity) -> Parity): Bool = MeetLattice.greatestLowerBound2(leq, glb)

//    law leastElement(leq: (Parity, Parity) -> Bool, bot: Parity): Bool = Bounded.leastElement(bot, leq)

//    law greatestElement(leq: (Parity, Parity) -> Bool, top: Parity): Bool = Bounded.greatestElement(top, leq)

//    law strict1(f: Parity -> Parity): Bool = Bounded.strict1(f, Bot, Bot)

//    law strict2(f: (Parity, Parity) -> Parity): Bool = Bounded.strict2(f, Bot, Bot, Bot)

//    law monotone1(f: Parity -> Parity): Bool = PartialOrder.monotone1(f, leq, leq)

//    law monotone2(f: (Parity, Parity) -> Parity): Bool = PartialOrder.monotone2(f, leq, leq, leq)

//    law safe1(fa: Parity -> Parity, fc: Int32 -> Int32): Bool = PartialOrder.safe1(fa, fc, alpha, alpha, leq)

//    law safe2(fa: (Parity, Parity) -> Parity, fc: (Int32, Int32) -> Int32): Bool = PartialOrder.safe2(fa, fc, alpha, alpha, leq)

}
