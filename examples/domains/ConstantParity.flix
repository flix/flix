/**
 * An implementation of the parity domain lifted over the constant propagation domain.
 */
namespace Domain.ConstantParity {

    ///
    /// The elements of the constant-parity lattice.
    ///
    enum ConstParity { // TODO: Name
                  case Top,

        case Odd,          case Even,

                case Cst(BigInt),

                  case Bot
    }

    ///
    /// Returns `true` iff `e1` is less than or equal to `e2`.
    ///
    #reflexive #antiSymmetric #transitive #leastElement(Bot) #greatestElement(Top)
    def leq(e1: ConstParity, e2: ConstParity): Bool = match (e1, e2) with {
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
    #commutative #associative #upperBound #leastUpperBound
    def lub(e1: ConstParity, e2: ConstParity): ConstParity = match (e1, e2) with {
        case (Bot, x)           => x
        case (x, Bot)           => x
        case (Cst(n1), Cst(n2)) => switch {
            case (n1 == n2)                 => Cst(n1)
            case (isOdd(n1) && isOdd(n2))   => Odd
            case (isEven(n1) && isEven(n2)) => Even
            case true                       => Top
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
    #commutative #associative #lowerBound #greatestLowerBound
    def glb(e1: ConstParity, e2: ConstParity): ConstParity = match (e1, e2) with {
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
    /// Abstracts a concrete number into the constant parity domain.
    ///
    def alpha(i: BigInt): ConstParity = Cst(i)

    ///
    /// Over-approximates integer `increment`.
    ///
    #approximates1(x -> x + 1ii)
    #strict1 #monotone1
    def inc(e: ConstParity): ConstParity = match e with {
        case Bot    => Bot
        case Cst(n) => Cst(n + 1ii)
        case Even   => Odd
        case Odd    => Even
        case Top    => Top
    }

    ///
    /// Over-approximates integer `decrement`.
    ///
    #approximates1(x -> x - 1ii)
    #strict1 #monotone1
    def dec(e: ConstParity): ConstParity = match e with {
        case Bot    => Bot
        case Cst(n) => Cst(n - 1ii)
        case Even   => Odd
        case Odd    => Even
        case Top    => Top
    }

    ///
    /// Over-approximates integer `addition`.
    ///
    #approximates2((x, y) -> x + y)
    #strict2 #monotone2 #commutative
    def plus(e1: ConstParity, e2: ConstParity): ConstParity = match (e1, e2) with {
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
    #approximates2((x, y) -> x - y)
    #strict2 #monotone2
    def minus(e1: ConstParity, e2: ConstParity): ConstParity = match (e1, e2) with {
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
    #approximates2((x, y) -> x * y)
    #strict2  // NB: @monotone annotation removed since Z3 reports unknown.
    def times(e1: ConstParity, e2: ConstParity): ConstParity = match (e1, e2) with {
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
    /// Over-approximates integer `division`.
    ///
    #approximates2((x, y) -> x / y)
    #strict2 #monotone2
    def divide(e1: ConstParity, e2: ConstParity): ConstParity = match (e1, e2) with {
        case (Bot, _)           => Bot
        case (_, Bot)           => Bot
        case (_, Cst(0ii))      => Bot
        case (Cst(n1), Cst(n2)) => Cst(n1 / n2)
        case _                  => Top
    }

    ///
    /// Over-approximates integer `modulus`.
    ///
    #approximates2((x, y) -> x % y)
    #strict2 // NB: @monotone annotation removed since Z3 reports unknown.
    def modulo(e1: ConstParity, e2: ConstParity): ConstParity = match (e1, e2) with {
        case (Bot, _)           => Bot
        case (_, Bot)           => Bot
        case (_, Cst(0ii))      => Bot
        case (Cst(n1), Cst(n2)) => Cst(n1 % n2)
        case (Cst(n), Even)     => if (isOdd(n)) Odd else Even
        case (Odd, Cst(n))      => if (isEven(n)) Odd else Top
        case (Even, Cst(n))     => if (isEven(n)) Even else Top
        case (Odd, Even)        => Odd
        case (Even, Even)       => Even
        case _                  => Top
    }

    ///
    /// Over-approximates `equal`.
    ///
    // @strict @monotone @commutative // TODO
    def eq(e1: ConstParity, e2: ConstParity): Belnap/Belnap = match (e1, e2) with {
        case (Bot, _)           => Belnap/Belnap.Bot
        case (_, Bot)           => Belnap/Belnap.Bot
        case (Cst(n1), Cst(n2)) => Belnap/alpha(n1 == n2)
        case (Cst(n), Even)     => if (isOdd(n))  Belnap/Belnap.False else Belnap/Belnap.Top
        case (Even, Cst(n))     => if (isOdd(n))  Belnap/Belnap.False else Belnap/Belnap.Top
        case (Cst(n), Odd)      => if (isEven(n)) Belnap/Belnap.False else Belnap/Belnap.Top
        case (Odd, Cst(n))      => if (isEven(n)) Belnap/Belnap.False else Belnap/Belnap.Top
        case _                  => Belnap/Belnap.Top
    }

    ///
    /// Over-approximates `not equal`.
    ///
    // @strict @monotone @commutative // TODO
    def neq(e1: ConstParity, e2: ConstParity): Belnap/Belnap = Belnap/not(eq(e1, e2))

    ///
    /// Over-approximates `less than`.
    ///
    // @strict @monotone // TODO
    def less(e1: ConstParity, e2: ConstParity): Belnap/Belnap = match (e1, e2) with {
        case (Bot, _)           => Belnap/Belnap.Bot
        case (_, Bot)           => Belnap/Belnap.Bot
        case (Cst(n1), Cst(n2)) => Belnap/alpha(n1 < n2)
        case _                  => Belnap/Belnap.Top
    }

    ///
    /// Over-approximates `less than or equal`.
    ///
    // @strict @monotone // TODO
    def lessEqual(e1: ConstParity, e2: ConstParity): Belnap/Belnap = Belnap/or(e1 `less` e2, e1 `eq` e2)

    ///
    /// Returns `true` iff the given number `n` is odd.
    ///
    def isOdd(n: BigInt): Bool = (n % 2ii) != 0ii

    ///
    /// Returns `true` iff the given number `n` is even.
    ///
    def isEven(n: BigInt): Bool = !isOdd(n)

    let ConstParity<> = (Bot, Top, leq, lub, glb)



    // ------------------------------------------------------------------------
    // TODO: Refactor
    // ------------------------------------------------------------------------

    law reflexive(⊑: (ConstParity, ConstParity) -> Bool): Bool = ∀(x: ConstParity). x ⊑ x // TODO: Use Prelude

    law antiSymmetric(⊑: (ConstParity, ConstParity) -> Bool): Bool = ∀(x: ConstParity, y: ConstParity). ((x ⊑ y) ∧ (y ⊑ x)) → (x == y) // TODO: Use Prelude

    law transitive(⊑: (ConstParity, ConstParity) -> Bool): Bool = ∀(x: ConstParity, y: ConstParity, z: ConstParity). ((x ⊑ y) ∧ (y ⊑ z)) → (x ⊑ z) // TODO: Use Prelude

    law upperBound(⊔: (ConstParity, ConstParity) -> ConstParity): Bool = // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: ConstParity, y: ConstParity). (x ⊑ (x ⊔ y)) ∧ (y ⊑ (x ⊔ y))

    law leastUpperBound(⊔: (ConstParity, ConstParity) -> ConstParity): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: ConstParity, y: ConstParity, z: ConstParity). ((x ⊑ z) ∧ (y ⊑ z)) → ((x ⊔ y) ⊑ z)

    law lowerBound(⊓: (ConstParity, ConstParity) -> ConstParity): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: ConstParity, y: ConstParity). ((x ⊓ y) ⊑ x) ∧ ((x ⊓ y) ⊑ y)

    law greatestLowerBound(⊓: (ConstParity, ConstParity) -> ConstParity): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: ConstParity, y: ConstParity, z: ConstParity). ((z ⊑ x) ∧ (z ⊑ y)) → (z ⊑ (x ⊓ y))

    law leastElement(⊑: (ConstParity, ConstParity) -> Bool, ⊥: ConstParity): Bool = ∀(x: ConstParity). ⊥ ⊑ x  // TODO: Should call prelude

    law greatestElement(⊑: (ConstParity, ConstParity) -> Bool, ⊤: ConstParity): Bool = ∀(x: ConstParity). x ⊑ ⊤  // TODO: Should call prelude

    law associative(f: (ConstParity, ConstParity) -> ConstParity): Bool = // TODO: Use Prelude
        ∀(x: ConstParity, y: ConstParity, z: ConstParity). f(x, f(y, z)) == f(f(x, y), z)

    law commutative(f: (ConstParity, ConstParity) -> ConstParity): Bool = // TODO: Use Prelude
        ∀(x: ConstParity, y: ConstParity). f(x, y) == f(y, x)

    law strict1(f: ConstParity -> ConstParity): Bool =  // TODO: Should call prelude
        f(Bot) == Bot

    law strict2(f: (ConstParity, ConstParity) -> ConstParity): Bool =  // TODO: Should call prelude
        f(Bot, Bot) == Bot

    law monotone1(f: ConstParity -> ConstParity): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: ConstParity, y: ConstParity). (x ⊑ y) → (f(x) ⊑ f(y))

    law monotone2(f: (ConstParity, ConstParity) -> ConstParity): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x1: ConstParity, x2: ConstParity, y1: ConstParity, y2: ConstParity). ((x1 ⊑ y1) ∧ (x2 ⊑ y2) ) → (f(x1, x2) ⊑ f(y1, y2))

    // TODO: Names
    law approximates1(fa: ConstParity -> ConstParity, fc: BigInt -> BigInt): Bool =
        ∀(x: BigInt). alpha(fc(x)) `leq` fa(alpha(x))

    // TODO: Names
    law approximates2(fa: (ConstParity, ConstParity) -> ConstParity, fc: (BigInt, BigInt) -> BigInt): Bool =
        ∀(x: BigInt, y: BigInt). alpha(fc(x, y)) `leq` fa(alpha(x), alpha(y))

}