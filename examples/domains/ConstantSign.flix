/**
 * An implementation of the sign domain lifted over the constant propagation domain.
 */
namespace ConstantSign {

    ///
    /// The elements of the constant-sign lattice.
    ///
    enum ConstSign {
                  case Top,

        case Neg,          case Pos,

                case Cst(BigInt),

                  case Bot
    }

    ///
    /// Returns `true` iff `e1` is less than or equal to `e2`.
    ///
    #reflexive #antiSymmetric #transitive #leastElement(Bot) #greatestElement(Top)
    def leq(e1: ConstSign, e2: ConstSign): Bool = match (e1, e2) with {
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
    #commutative #associative #upperBound #leastUpperBound
    def lub(e1: ConstSign, e2: ConstSign): ConstSign = match (e1, e2) with {
        case (Bot, x)           => x
        case (x, Bot)           => x
        case (Cst(n1), Cst(n2)) => switch {
            case n1 == n2                       => Cst(n1)
            case n1 >= 0ii && n2 >= 0ii         => Pos
            case n1 <= 0ii && n2 <= 0ii         => Neg
            case true                           => Top
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
    #commutative #associative #lowerBound #greatestLowerBound
    def glb(e1: ConstSign, e2: ConstSign): ConstSign = match (e1, e2) with {
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
    /// Abstracts a concrete number into the domain.
    ///
    def alpha(i: BigInt): ConstSign = Cst(i)

    ///
    /// Over-approximates integer `increment`.
    ///
    #approximates1(x -> x + 1ii)
    #strict1 #monotone1
    def inc(e: ConstSign): ConstSign = match e with {
        case Bot    => Bot
        case Cst(n) => Cst(n + 1ii)
        case Neg    => Top
        case Pos    => Pos
        case Top    => Top
    }

    ///
    /// Over-approximates integer `decrement`.
    ///
    #approximates1(x -> x - 1ii)
    #strict1 #monotone1
    def dec(e: ConstSign): ConstSign = match e with {
        case Bot    => Bot
        case Cst(n) => Cst(n - 1ii)
        case Neg    => Neg
        case Pos    => Top
        case Top    => Top
    }

    ///
    /// Over-approximates integer `addition`.
    ///
    #approximates2((x, y) -> x + y)
    #strict2 #monotone2 #commutative // TODO: Associative?
    def plus(e1: ConstSign, e2: ConstSign): ConstSign = match (e1, e2) with {
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
    #approximates2((x, y) -> x - y)
    #strict2 #monotone2
    def minus(e1: ConstSign, e2: ConstSign): ConstSign = match (e1, e2) with {
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
    #approximates2((x, y) -> x * y)
    #strict2 #monotone2 #commutative #associative
    def times(e1: ConstSign, e2: ConstSign): ConstSign = match (e1, e2) with {
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
    /// Over-approximates integer `division`.
    ///
    #approximates2((x, y) -> x / y)
    #strict2 #monotone2
    def divide(e1: ConstSign, e2: ConstSign): ConstSign = match (e1, e2) with {
        case (Bot, _)           => Bot
        case (_, Bot)           => Bot
        case (_, Cst(0ii))      => Bot
        case (Cst(0ii), _)      => Cst(0ii)
        case (Cst(n1), Cst(n2)) => Cst(n1 / n2)
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
    /// Over-approximates integer `modulus`.
    ///
    #approximates2((x, y) -> x % y)
    #strict2 #monotone2
    def modulo(e1: ConstSign, e2: ConstSign): ConstSign = match (e1, e2) with {
        case (Bot, _)           => Bot
        case (_, Bot)           => Bot
        case (_, Cst(0ii))      => Bot
        case (Cst(n1), Cst(n2)) => Cst(n1 % n2)
        case _                  => Top
    }

    ///
    /// Over-approximates `equal`.
    ///
    // @strict @monotone @commutative // TODO
    def eq(e1: ConstSign, e2: ConstSign): Belnap/Belnap = match (e1, e2) with {
        case (Bot, _)           => Belnap/Belnap.Bot
        case (_, Bot)           => Belnap/Belnap.Bot
        case (Cst(n1), Cst(n2)) => Belnap/alpha(n1 == n2)
        case (Cst(n1), Neg)     => if (n1 > 0ii) Belnap/Belnap.False else Belnap/Belnap.Top
        case (Neg, Cst(n1))     => if (n1 > 0ii) Belnap/Belnap.False else Belnap/Belnap.Top
        case (Cst(n1), Pos)     => if (n1 < 0ii) Belnap/Belnap.False else Belnap/Belnap.Top
        case (Pos, Cst(n1))     => if (n1 < 0ii) Belnap/Belnap.False else Belnap/Belnap.Top
        case _                  => Belnap/Belnap.Top
    }

    ///
    /// Over-approximates `not equal`.
    ///
    // @strict @monotone @commutative // TODO
    def neq(e1: ConstSign, e2: ConstSign): Belnap/Belnap = Belnap/not(eq(e1, e2))

    ///
    /// Over-approximates `less than`.
    ///
    // @strict @monotone // TODO
    def less(e1: ConstSign, e2: ConstSign): Belnap/Belnap = match (e1, e2) with {
        case (Bot, _)           => Belnap/Belnap.Bot
        case (_, Bot)           => Belnap/Belnap.Bot
        case (Cst(n1), Cst(n2)) => Belnap/alpha(n1 < n2)
        case (Cst(n1), Neg)     => if (n1 > 0ii) Belnap/Belnap.False else Belnap/Belnap.Top
        case (Neg, Cst(n1))     => if (n1 > 0ii) Belnap/Belnap.True else Belnap/Belnap.Top
        case (Cst(n1), Pos)     => if (n1 < 0ii) Belnap/Belnap.True else Belnap/Belnap.Top
        case (Pos, Cst(n1))     => if (n1 < 0ii) Belnap/Belnap.False else Belnap/Belnap.Top
        case _                  => Belnap/Belnap.Top
    }

    ///
    /// Over-approximates `less than or equal`.
    ///
    // @strict @monotone // TODO
    def lessEqual(e1: ConstSign, e2: ConstSign): Belnap/Belnap = Belnap/or(e1 `less` e2, e1 `eq` e2)

    let ConstSign<> = (Bot, Top, leq, lub, glb)



    // ------------------------------------------------------------------------
    // TODO: Refactor
    // ------------------------------------------------------------------------

    law reflexive(⊑: (ConstSign, ConstSign) -> Bool): Bool = ∀(x: ConstSign). x ⊑ x // TODO: Use Prelude

    law antiSymmetric(⊑: (ConstSign, ConstSign) -> Bool): Bool = ∀(x: ConstSign, y: ConstSign). ((x ⊑ y) ∧ (y ⊑ x)) → (x == y) // TODO: Use Prelude

    law transitive(⊑: (ConstSign, ConstSign) -> Bool): Bool = ∀(x: ConstSign, y: ConstSign, z: ConstSign). ((x ⊑ y) ∧ (y ⊑ z)) → (x ⊑ z) // TODO: Use Prelude

    law upperBound(⊔: (ConstSign, ConstSign) -> ConstSign): Bool = // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: ConstSign, y: ConstSign). (x ⊑ (x ⊔ y)) ∧ (y ⊑ (x ⊔ y))

    law leastUpperBound(⊔: (ConstSign, ConstSign) -> ConstSign): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: ConstSign, y: ConstSign, z: ConstSign). ((x ⊑ z) ∧ (y ⊑ z)) → ((x ⊔ y) ⊑ z)

    law lowerBound(⊓: (ConstSign, ConstSign) -> ConstSign): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: ConstSign, y: ConstSign). ((x ⊓ y) ⊑ x) ∧ ((x ⊓ y) ⊑ y)

    law greatestLowerBound(⊓: (ConstSign, ConstSign) -> ConstSign): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: ConstSign, y: ConstSign, z: ConstSign). ((z ⊑ x) ∧ (z ⊑ y)) → (z ⊑ (x ⊓ y))

    law leastElement(⊑: (ConstSign, ConstSign) -> Bool, ⊥: ConstSign): Bool = ∀(x: ConstSign). ⊥ ⊑ x  // TODO: Should call prelude

    law greatestElement(⊑: (ConstSign, ConstSign) -> Bool, ⊤: ConstSign): Bool = ∀(x: ConstSign). x ⊑ ⊤  // TODO: Should call prelude

    law associative(f: (ConstSign, ConstSign) -> ConstSign): Bool = // TODO: Use Prelude
        ∀(x: ConstSign, y: ConstSign, z: ConstSign). f(x, f(y, z)) == f(f(x, y), z)

    law commutative(f: (ConstSign, ConstSign) -> ConstSign): Bool = // TODO: Use Prelude
        ∀(x: ConstSign, y: ConstSign). f(x, y) == f(y, x)

    law strict1(f: ConstSign -> ConstSign): Bool =  // TODO: Should call prelude
        f(Bot) == Bot

    law strict2(f: (ConstSign, ConstSign) -> ConstSign): Bool =  // TODO: Should call prelude
        f(Bot, Bot) == Bot

    law monotone1(f: ConstSign -> ConstSign): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: ConstSign, y: ConstSign). (x ⊑ y) → (f(x) ⊑ f(y))

    law monotone2(f: (ConstSign, ConstSign) -> ConstSign): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x1: ConstSign, x2: ConstSign, y1: ConstSign, y2: ConstSign). ((x1 ⊑ y1) ∧ (x2 ⊑ y2) ) → (f(x1, x2) ⊑ f(y1, y2))

    // TODO: Names
    law approximates1(fa: ConstSign -> ConstSign, fc: BigInt -> BigInt): Bool =
        ∀(x: BigInt). alpha(fc(x)) `leq` fa(alpha(x))

    // TODO: Names
    law approximates2(fa: (ConstSign, ConstSign) -> ConstSign, fc: (BigInt, BigInt) -> BigInt): Bool =
        ∀(x: BigInt, y: BigInt). alpha(fc(x, y)) `leq` fa(alpha(x), alpha(y))


}