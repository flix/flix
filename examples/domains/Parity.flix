//
// An implementation of the parity domain.
//
namespace Domain.Parity {

    ///
    /// The elements of the parity lattice.
    ///
    enum Parity {
                  case Top,

        case Odd,          case Even,

                  case Bot
    }

    ///
    /// Returns `true` iff `e1` is less than or equal to `e2`.
    ///
    #reflexive #antiSymmetric #transitive #leastElement(Bot) #greatestElement(Top)
    def leq(e1: Parity, e2: Parity): Bool = match (e1, e2) with {
        case (Bot, _)       => true
        case (Odd, Odd)     => true
        case (Even, Even)   => true
        case (_, Top)       => true
        case _              => false
    }

    ///
    /// Returns the least upper bound of `e1` and `e2`.
    ///
    #commutative #associative #upperBound #leastUpperBound
    def lub(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
        case (Bot, x)       => x
        case (x, Bot)       => x
        case (Odd, Odd)     => Odd
        case (Even, Even)   => Even
        case _              => Top
    }

    ///
    /// Returns the greatest lower bound of `e1` and `e2`.
    ///
    #commutative #associative #lowerBound #greatestLowerBound
    def glb(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
        case (Top, x)       => x
        case (x, Top)       => x
        case (Odd, Odd)     => Odd
        case (Even, Even)   => Even
        case _              => Bot
    }

    ///
    /// Abstracts a concrete integer into the Parity domain.
    ///
    def alpha(i: Int): Parity = if (i % 2 != 0) Odd else Even

    ///
    /// Over-approximates integer `increment`.
    ///
    #approximates1(x -> x + 1)
    #strict1 #monotone1
    def inc(e: Parity): Parity = match e with {
        case Bot  => Bot
        case Odd  => Even
        case Even => Odd
        case Top  => Top
    }

    ///
    /// Over-approximates integer `decrement`.
    ///
    #approximates1(x -> x - 1)
    #strict1 #monotone1
    def dec(e: Parity): Parity = match e with {
        case Bot  => Bot
        case Odd  => Even
        case Even => Odd
        case Top  => Top
    }

    ///
    /// Over-approximates integer `addition`.
    ///
    #approximates2((x, y) -> x + y)
    #strict2 #monotone2 #commutative #associative
    def plus(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
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
    #approximates2((x, y) -> x - y)
    #strict2 #monotone2
    def minus(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
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
    #approximates2((x, y) -> x * y)
    #strict2 #monotone2 #commutative #associative
    def times(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
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
    #approximates2((x, y) -> x / y)
    #strict2 #monotone2
    def divide(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
       case (_, Bot)    => Bot
       case (Bot, _)    => Bot
       case _           => Top
    }

    ///
    /// Over-approximates integer `modulus`.
    ///
    #approximates2((x, y) -> x % y)
    #strict2 #monotone2
    def modulo(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
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
    #approximates1(x -> ~~~x)
    #strict1 #monotone1
    // TODO: Bug Found!
    def negate(e: Parity): Parity = match e with {
        case Bot    => Bot
        case Odd    => Even
        case Even   => Odd
        case Top    => Top
    }

    ///
    /// Over-approximates `bitwise and`.
    ///
    #approximates2((x, y) -> x &&& y)
    #strict2 #monotone2 #commutative #associative
    def and(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
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
    #approximates2((x, y) -> x ||| y)
    #strict2 #monotone2 #commutative #associative
    def or(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
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
    #approximates2((x, y) -> x ^^^ y)
    #strict2 #monotone2 #commutative #associative
    def xor(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
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
    #approximates2((x, y) -> x <<< y)
    #strict2 #monotone2
    def leftShift(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
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
    #approximates2((x, y) -> x >>> y)
    #strict2 #monotone2
    def rightShift(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
      case (_, Bot)     => Bot
      case (Bot, _)     => Bot
      case _            => Top
    }

    ///
    /// Over-approximates `equal`.
    ///
//    #strict2 #monotone2 #commutative // TODO
    def eq(e1: Parity, e2: Parity): Belnap/Belnap = match (e1, e2) with {
        case (Bot, _)       => Belnap/Belnap.Bot
        case (_, Bot)       => Belnap/Belnap.Bot
        case (Odd, Even)    => Belnap/Belnap.False
        case (Even, Odd)    => Belnap/Belnap.False
        case _              => Belnap/Belnap.Top
    }

    ///
    /// Over-approximates `not equal`.
    ///
//    #strict2 #monotone2 #commutative // TODO
    def neq(e1: Parity, e2: Parity): Belnap/Belnap = Belnap/not(eq(e1, e2))

    ///
    /// Over-approximates `less than`.
    ///
//    #strict2 #monotone2 // TODO
    def less(e1: Parity, e2: Parity): Belnap/Belnap = match (e1, e2) with {
        case (Bot, _)   => Belnap/Belnap.Bot
        case (_, Bot)   => Belnap/Belnap.Bot
        case _          => Belnap/Belnap.Top
    }

    ///
    /// Over-approximates `less than or equal`.
    ///
//    #strict2 #monotone2 // TODO
    def lessEqual(e1: Parity, e2: Parity): Belnap/Belnap = Belnap/or(e1 `less` e2, e1 `eq` e2)

    let Parity<> = (Bot, Top, leq, lub, glb)




    // ------------------------------------------------------------------------
    // TODO: Refactor
    // ------------------------------------------------------------------------

    law reflexive(⊑: (Parity, Parity) -> Bool): Bool = ∀(x: Parity). x ⊑ x // TODO: Use Prelude

    law antiSymmetric(⊑: (Parity, Parity) -> Bool): Bool = ∀(x: Parity, y: Parity). ((x ⊑ y) ∧ (y ⊑ x)) → (x == y) // TODO: Use Prelude

    law transitive(⊑: (Parity, Parity) -> Bool): Bool = ∀(x: Parity, y: Parity, z: Parity). ((x ⊑ y) ∧ (y ⊑ z)) → (x ⊑ z) // TODO: Use Prelude

    law upperBound(⊔: (Parity, Parity) -> Parity): Bool = // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: Parity, y: Parity). (x ⊑ (x ⊔ y)) ∧ (y ⊑ (x ⊔ y))

    law leastUpperBound(⊔: (Parity, Parity) -> Parity): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: Parity, y: Parity, z: Parity). ((x ⊑ z) ∧ (y ⊑ z)) → ((x ⊔ y) ⊑ z)

    law lowerBound(⊓: (Parity, Parity) -> Parity): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: Parity, y: Parity). ((x ⊓ y) ⊑ x) ∧ ((x ⊓ y) ⊑ y)

    law greatestLowerBound(⊓: (Parity, Parity) -> Parity): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: Parity, y: Parity, z: Parity). ((z ⊑ x) ∧ (z ⊑ y)) → (z ⊑ (x ⊓ y))

    law leastElement(⊑: (Parity, Parity) -> Bool, ⊥: Parity): Bool = ∀(x: Parity). ⊥ ⊑ x  // TODO: Should call prelude

    law greatestElement(⊑: (Parity, Parity) -> Bool, ⊤: Parity): Bool = ∀(x: Parity). x ⊑ ⊤  // TODO: Should call prelude

    law associative(f: (Parity, Parity) -> Parity): Bool = // TODO: Use Prelude
        ∀(x: Parity, y: Parity, z: Parity). f(x, f(y, z)) == f(f(x, y), z)

    law commutative(f: (Parity, Parity) -> Parity): Bool = // TODO: Use Prelude
        ∀(x: Parity, y: Parity). f(x, y) == f(y, x)

    law strict1(f: Parity -> Parity): Bool =  // TODO: Should call prelude
        f(Bot) == Bot

    law strict2(f: (Parity, Parity) -> Parity): Bool =  // TODO: Should call prelude
        f(Bot, Bot) == Bot

    law monotone1(f: Parity -> Parity): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: Parity, y: Parity). (x ⊑ y) → (f(x) ⊑ f(y))

    law monotone2(f: (Parity, Parity) -> Parity): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x1: Parity, x2: Parity, y1: Parity, y2: Parity). ((x1 ⊑ y1) ∧ (x2 ⊑ y2) ) → (f(x1, x2) ⊑ f(y1, y2))

    // TODO: Names
    law approximates1(fa: Parity -> Parity, fc: Int -> Int): Bool =
        ∀(x: Int). alpha(fc(x)) `leq` fa(alpha(x))

    // TODO: Names
    law approximates2(fa: (Parity, Parity) -> Parity, fc: (Int, Int) -> Int): Bool =
        ∀(x: Int, y: Int). alpha(fc(x, y)) `leq` fa(alpha(x), alpha(y))

}