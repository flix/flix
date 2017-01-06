/**
 * An implementation of the sign domain.
 *
 * See Cousot & Cousot '77 for details.
 */
namespace Domain.Sign {

    ///
    /// The elements of the sign lattice.
    ///
    /// Notice that Neg and Pos implicitly includes zero.
    ///
    enum Sign {
                  case Top,

        case Neg,          case Pos,

                  case Zer,

                  case Bot
    }

    ///
    /// Returns `true` iff `e1` is less than or equal to `e2`.
    ///
    #reflexive #antiSymmetric #transitive #leastElement(Bot) #greatestElement(Top)
    def leq(e1: Sign, e2: Sign): Bool = match (e1, e2) with {
        case (Bot, _)   => true
        case (Zer, Zer) => true
        case (Zer, Neg) => true
        case (Zer, Pos) => true
        case (Neg, Neg) => true
        case (Pos, Pos) => true
        case (_, Top)   => true
        case _          => false
    }

    ///
    /// Returns the least upper bound of `e1` and `e2`.
    ///
    #commutative #associative #upperBound #leastUpperBound
    def lub(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Bot, x)   => x
        case (x, Bot)   => x
        case (Zer, Zer) => Zer
        case (Zer, Neg) => Neg
        case (Neg, Zer) => Neg
        case (Zer, Pos) => Pos
        case (Pos, Zer) => Pos
        case (Neg, Neg) => Neg
        case (Pos, Pos) => Pos
        case _          => Top
    }

    ///
    /// Returns the greatest lower bound of `e1` and `e2`.
    ///
    #commutative #associative #lowerBound #greatestLowerBound
    def glb(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Top, x)   => x
        case (x, Top)   => x
        case (Zer, Zer) => Zer
        case (Neg, Neg) => Neg
        case (Pos, Pos) => Pos
        case (Zer, Neg) => Zer
        case (Neg, Zer) => Zer
        case (Zer, Pos) => Zer
        case (Pos, Zer) => Zer
        case (Neg, Pos) => Zer
        case (Pos, Neg) => Zer
        case (x, Top)   => x
        case (Top, x)   => x
        case _          => Bot
    }

    ///
    /// Abstracts a concrete number into the sign domain.
    ///
    def alpha(i: Int): Sign = switch {
        case i < 0  => Neg
        case i > 0  => Pos
        case true   => Zer
    }

    ///
    /// Over-approximates integer `increment`.
    ///
    #approximates1(x -> x + 1)
    #strict1 #monotone1
    def inc(e: Sign): Sign = match e with {
        case Bot => Bot
        case Neg => Top
        case Zer => Pos
        case Pos => Pos
        case Top => Top
    }

    ///
    /// Over-approximates integer `decrement`.
    ///
    #approximates1(x -> x - 1)
    #strict1 #monotone1
    def dec(e: Sign): Sign = match e with {
        case Bot => Bot
        case Neg => Neg
        case Zer => Neg
        case Pos => Top
        case Top => Top
    }

    ///
    /// Over-approximates integer `addition`.
    ///
    #approximates2((x, y) -> x + y)
    #strict2 #monotone2 #commutative #associative
    def plus(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Bot, _) => Bot
        case (_, Bot) => Bot
        case (Neg, Neg) => Neg
        case (Neg, Zer) => Neg
        case (Neg, Pos) => Top
        case (Zer, Neg) => Neg
        case (Zer, Zer) => Zer
        case (Zer, Pos) => Pos
        case (Pos, Neg) => Top
        case (Pos, Zer) => Pos
        case (Pos, Pos) => Pos
        case  _         => Top
    }

    ///
    /// Over-approximates integer `subtraction`.
    ///
    #approximates2((x, y) -> x - y)
    #strict2 #monotone2
    def minus(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Bot, _)   => Bot
        case (_, Bot)   => Bot
        case (Neg, Neg) => Top
        case (Neg, Zer) => Neg
        case (Neg, Pos) => Neg
        case (Zer, Neg) => Pos
        case (Zer, Zer) => Zer
        case (Zer, Pos) => Neg
        case (Pos, Neg) => Pos
        case (Pos, Zer) => Pos
        case (Pos, Pos) => Top
        case  _         => Top
    }

    ///
    /// Over-approximates integer `multiplication`.
    ///
    #approximates2((x, y) -> x * y)
    #strict2 #monotone2
    def times(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Bot, _)   => Bot
        case (_, Bot)   => Bot
        case (Neg, Neg) => Pos
        case (Neg, Zer) => Zer
        case (Neg, Pos) => Neg
        case (Zer, Neg) => Zer
        case (Zer, Zer) => Zer
        case (Zer, Pos) => Zer
        case (Pos, Neg) => Neg
        case (Pos, Zer) => Zer
        case (Pos, Pos) => Pos
        case  _         => Top
    }

    ///
    /// Over-approximates integer `division`.
    ///
    #approximates2((x, y) -> x / y)
    #strict2 #monotone2
    def divide(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Bot, _)   => Bot
        case (_, Bot)   => Bot
        case (Neg, Neg) => Pos
        case (Neg, Zer) => Bot
        case (Neg, Pos) => Neg
        case (Zer, Neg) => Zer
        case (Zer, Zer) => Bot
        case (Zer, Pos) => Zer
        case (Pos, Neg) => Neg
        case (Pos, Zer) => Bot
        case (Pos, Pos) => Pos
        case  _         => Top
    }

    ///
    /// Over-approximates integer `modulus`.
    ///
    #approximates2((x, y) -> x % y)
    #strict2 #monotone2
    def modulo(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Bot, _)   => Bot
        case (_, Bot)   => Bot
        case (Neg, Neg) => Bot
        case (Neg, Zer) => Bot
        case (Neg, Pos) => Top
        case (Zer, Neg) => Bot
        case (Zer, Zer) => Bot
        case (Zer, Pos) => Pos
        case (Pos, Neg) => Top
        case (Pos, Zer) => Bot
        case (Pos, Pos) => Top
        case  _         => Top
    }

    ///
    /// Over-approximates `bitwise negation`.
    ///
    #approximates1(x -> -x)
    #strict1 #monotone1
    def negate(e: Sign): Sign = match e with {
        case Bot => Bot
        case Zer => Neg
        case Neg => Top
        case Pos => Top
        case Top => Top
    }

    ///
    /// Over-approximates `bitwise and`.
    ///
    #approximates2((x, y) -> x & y)
    #strict2 #monotone2 #commutative #associative
    def and(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Bot, _)   => Bot
        case (_, Bot)   => Bot
        case (Neg, Neg) => Neg
        case (Neg, Zer) => Zer
        case (Neg, Pos) => Pos
        case (Zer, Neg) => Zer
        case (Zer, Zer) => Zer
        case (Zer, Pos) => Zer
        case (Pos, Neg) => Pos
        case (Pos, Zer) => Zer
        case (Pos, Pos) => Pos
        case  _         => Top
    }

    ///
    /// Over-approximates `bitwise or`.
    ///
    #approximates2((x, y) -> x | y)
    #strict2 #monotone2 #commutative #associative
    def or(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Bot, _)   => Bot
        case (_, Bot)   => Bot
        case (Neg, Neg) => Neg
        case (Neg, Zer) => Neg
        case (Neg, Pos) => Top
        case (Zer, Neg) => Neg
        case (Zer, Zer) => Zer
        case (Zer, Pos) => Pos
        case (Pos, Neg) => Top
        case (Pos, Zer) => Pos
        case (Pos, Pos) => Pos
        case  _         => Top
    }

    ///
    /// Over-approximates `bitwise xor`.
    ///
    #approximates2((x, y) -> x ^ y)
    #strict2 #monotone2 #commutative #associative
    def xor(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Bot, _)   => Bot
        case (_, Bot)   => Bot
        case (Neg, Neg) => Top
        case (Neg, Zer) => Neg
        case (Neg, Pos) => Top
        case (Zer, Neg) => Neg
        case (Zer, Zer) => Zer
        case (Zer, Pos) => Pos
        case (Pos, Neg) => Top
        case (Pos, Zer) => Pos
        case (Pos, Pos) => Top
        case  _         => Top
    }

    ///
    /// Over-approximates `bitwise left shift`.
    ///
    #approximates2((x, y) -> x << y)
    #strict2 #monotone2
    def leftShift(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
       case (Bot, _)   => Bot
       case (_, Bot)   => Bot
       case (Neg, Neg) => Top
       case (Neg, Zer) => Neg
       case (Neg, Pos) => Top
       case (Zer, Neg) => Top
       case (Zer, Zer) => Zer
       case (Zer, Pos) => Zer
       case (Pos, Neg) => Top
       case (Pos, Zer) => Pos
       case (Pos, Pos) => Top
       case  _         => Top
   }

    ///
    /// Over-approximates `bitwise right shift`.
    ///
    #approximates2((x, y) -> x >> y)
    #strict2 #monotone2
    def rightShift(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Bot, _)   => Bot
        case (_, Bot)   => Bot
        case (Neg, Neg) => Top
        case (Neg, Zer) => Neg
        case (Neg, Pos) => Top
        case (Zer, Neg) => Top
        case (Zer, Zer) => Zer
        case (Zer, Pos) => Zer
        case (Pos, Neg) => Top
        case (Pos, Zer) => Pos
        case (Pos, Pos) => Pos
        case  _         => Top
    }

    ///
    /// Over-approximates `equal`.
    ///
//    #strict2 #monotone2 #commutativeB // TODO: Requires more general laws
    def eq(e1: Sign, e2: Sign): Belnap/Belnap = match (e1, e2) with {
        case (Bot, _)   => Belnap/Belnap.Bot
        case (_, Bot)   => Belnap/Belnap.Bot
        case (Zer, Zer) => Belnap/Belnap.True
        case _          => Belnap/Belnap.Top
    }

    ///
    /// Over-approximates `not equal`.
    ///
    //@strict @monotone @commutative // TODO
    def neq(e1: Sign, e2: Sign): Belnap/Belnap = Belnap/not(eq(e1, e2))

    ///
    /// Over-approximates `less than`.
    ///
    // @strict @monotone // TODO
    def less(e1: Sign, e2: Sign): Belnap/Belnap = match (e1, e2) with {
        case (Bot, _)   => Belnap/Belnap.Bot
        case (_, Bot)   => Belnap/Belnap.Bot
        case (Zer, Zer) => Belnap/Belnap.False
        case _          => Belnap/Belnap.Top
    }

    ///
    /// Over-approximates `less than or equal`.
    ///
    // @strict @monotone // TODO
    def lessEqual(e1: Sign, e2: Sign): Belnap/Belnap = Belnap/or(e1 `less` e2, e1 `eq` e2)

    let Sign<> = (Bot, Top, leq, lub, glb)

    // ------------------------------------------------------------------------
    // TODO: Refactor
    // ------------------------------------------------------------------------

    law reflexive(⊑: (Sign, Sign) -> Bool): Bool = ∀(x: Sign). x ⊑ x // TODO: Use Prelude

    law antiSymmetric(⊑: (Sign, Sign) -> Bool): Bool = ∀(x: Sign, y: Sign). ((x ⊑ y) ∧ (y ⊑ x)) → (x == y) // TODO: Use Prelude

    law transitive(⊑: (Sign, Sign) -> Bool): Bool = ∀(x: Sign, y: Sign, z: Sign). ((x ⊑ y) ∧ (y ⊑ z)) → (x ⊑ z) // TODO: Use Prelude

    law upperBound(⊔: (Sign, Sign) -> Sign): Bool = // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: Sign, y: Sign). (x ⊑ (x ⊔ y)) ∧ (y ⊑ (x ⊔ y))

    law leastUpperBound(⊔: (Sign, Sign) -> Sign): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: Sign, y: Sign, z: Sign). ((x ⊑ z) ∧ (y ⊑ z)) → ((x ⊔ y) ⊑ z)

    law lowerBound(⊓: (Sign, Sign) -> Sign): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: Sign, y: Sign). ((x ⊓ y) ⊑ x) ∧ ((x ⊓ y) ⊑ y)

    law greatestLowerBound(⊓: (Sign, Sign) -> Sign): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: Sign, y: Sign, z: Sign). ((z ⊑ x) ∧ (z ⊑ y)) → (z ⊑ (x ⊓ y))

    law leastElement(⊑: (Sign, Sign) -> Bool, ⊥: Sign): Bool = ∀(x: Sign). ⊥ ⊑ x  // TODO: Should call prelude

    law greatestElement(⊑: (Sign, Sign) -> Bool, ⊤: Sign): Bool = ∀(x: Sign). x ⊑ ⊤  // TODO: Should call prelude

    law associative(f: (Sign, Sign) -> Sign): Bool = // TODO: Use Prelude
        ∀(x: Sign, y: Sign, z: Sign). f(x, f(y, z)) == f(f(x, y), z)

    law commutative(f: (Sign, Sign) -> Sign): Bool = // TODO: Use Prelude
        ∀(x: Sign, y: Sign). f(x, y) == f(y, x)

    law strict1(f: Sign -> Sign): Bool =  // TODO: Should call prelude
        f(Bot) == Bot

    law strict2(f: (Sign, Sign) -> Sign): Bool =  // TODO: Should call prelude
        f(Bot, Bot) == Bot

    law monotone1(f: Sign -> Sign): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: Sign, y: Sign). (x ⊑ y) → (f(x) ⊑ f(y))

    law monotone2(f: (Sign, Sign) -> Sign): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x1: Sign, x2: Sign, y1: Sign, y2: Sign). ((x1 ⊑ y1) ∧ (x2 ⊑ y2) ) → (f(x1, x2) ⊑ f(y1, y2))

    // TODO: Names
    law approximates1(fa: Sign -> Sign, fc: Int -> Int): Bool =
        ∀(x: Int). alpha(fc(x)) `leq` fa(alpha(x))

    // TODO: Names
    law approximates2(fa: (Sign, Sign) -> Sign, fc: (Int, Int) -> Int): Bool =
        ∀(x: Int, y: Int). alpha(fc(x, y)) `leq` fa(alpha(x), alpha(y))


}
