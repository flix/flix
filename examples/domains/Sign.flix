/**
 * An implementation of the sign domain.
 *
 * See Cousot & Cousot '77 for details.
 */
namespace Domain.Sign {

    ///
    ///The elements of the sign lattice.
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
    #reflexive
    def leq(e1: Sign, e2: Sign): Bool = match (e1, e2) with {
        case (Sign.Bot, _)          => true
        case (Sign.Zer, Sign.Zer)   => true
        case (Sign.Zer, Sign.Neg)   => true
        case (Sign.Zer, Sign.Pos)   => true
        case (Sign.Neg, Sign.Neg)   => true
        case (Sign.Pos, Sign.Pos)   => true
        case (_, Sign.Top)          => true
        case _                      => false
    }

    ///
    /// Returns the least upper bound of `e1` and `e2`.
    ///
    #commutative #associative
    def lub(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Sign.Bot, x)          => x
        case (x, Sign.Bot)          => x
        case (Sign.Zer, Sign.Zer)   => Sign.Zer
        case (Sign.Zer, Sign.Neg)   => Sign.Neg
        case (Sign.Neg, Sign.Zer)   => Sign.Neg
        case (Sign.Zer, Sign.Pos)   => Sign.Pos
        case (Sign.Pos, Sign.Zer)   => Sign.Pos
        case (Sign.Neg, Sign.Neg)   => Sign.Neg
        case (Sign.Pos, Sign.Pos)   => Sign.Pos
        case _                      => Sign.Top
    }

    ///
    /// Returns the greatest lower bound of `e1` and `e2`.
    ///
    #commutative #associative
    def glb(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Sign.Top, x)          => x
        case (x, Sign.Top)          => x
        case (Sign.Zer, Sign.Zer)   => Sign.Zer
        case (Sign.Neg, Sign.Neg)   => Sign.Neg
        case (Sign.Pos, Sign.Pos)   => Sign.Pos
        case (Sign.Zer, Sign.Neg)   => Sign.Zer
        case (Sign.Neg, Sign.Zer)   => Sign.Zer
        case (Sign.Zer, Sign.Pos)   => Sign.Zer
        case (Sign.Pos, Sign.Zer)   => Sign.Zer
        case (Sign.Neg, Sign.Pos)   => Sign.Zer
        case (Sign.Pos, Sign.Neg)   => Sign.Zer
        case (x, Sign.Top)          => x
        case (Sign.Top, x)          => x
        case _                      => Sign.Bot
    }

    ///
    /// Abstracts a concrete number into the (alternative) sign domain.
    ///
    def alpha(i: Int): Sign = switch {
        case i < 0  => Sign.Neg
        case i > 0  => Sign.Pos
        case true   => Sign.Zer
    }

    ///
    /// Over-approximates integer `increment`.
    ///
    #approximates1(x -> x + 1)
    #strict1 #monotone1
    def inc(e: Sign): Sign = match e with {
        case Sign.Bot => Sign.Bot
        case Sign.Neg => Sign.Top
        case Sign.Zer => Sign.Pos
        case Sign.Pos => Sign.Pos
        case Sign.Top => Sign.Top
    }

    ///
    /// Over-approximates integer `decrement`.
    ///
    #approximates1(x -> x - 1)
    #strict1 #monotone1
    def dec(e: Sign): Sign = match e with {
        case Sign.Bot => Sign.Bot
        case Sign.Neg => Sign.Neg
        case Sign.Zer => Sign.Neg
        case Sign.Pos => Sign.Top
        case Sign.Top => Sign.Top
    }

    ///
    /// Over-approximates integer `addition`.
    ///
    #approximates2((x, y) -> x + y)
    #strict2 #monotone2 #commutative #associative
    def plus(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Sign.Bot, _)          => Sign.Bot
        case (_, Sign.Bot)          => Sign.Bot
        case (Sign.Neg, Sign.Neg)   => Sign.Neg
        case (Sign.Neg, Sign.Zer)   => Sign.Neg
        case (Sign.Neg, Sign.Pos)   => Sign.Top
        case (Sign.Zer, Sign.Neg)   => Sign.Neg
        case (Sign.Zer, Sign.Zer)   => Sign.Zer
        case (Sign.Zer, Sign.Pos)   => Sign.Pos
        case (Sign.Pos, Sign.Neg)   => Sign.Top
        case (Sign.Pos, Sign.Zer)   => Sign.Pos
        case (Sign.Pos, Sign.Pos)   => Sign.Pos
        case  _                     => Sign.Top
    }

    ///
    /// Over-approximates integer `subtraction`.
    ///
    #approximates2((x, y) -> x - y)
    #strict2 #monotone2
    def minus(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Sign.Bot, _)          => Sign.Bot
        case (_, Sign.Bot)          => Sign.Bot
        case (Sign.Neg, Sign.Neg)   => Sign.Top
        case (Sign.Neg, Sign.Zer)   => Sign.Neg
        case (Sign.Neg, Sign.Pos)   => Sign.Neg
        case (Sign.Zer, Sign.Neg)   => Sign.Pos
        case (Sign.Zer, Sign.Zer)   => Sign.Zer
        case (Sign.Zer, Sign.Pos)   => Sign.Neg
        case (Sign.Pos, Sign.Neg)   => Sign.Pos
        case (Sign.Pos, Sign.Zer)   => Sign.Pos
        case (Sign.Pos, Sign.Pos)   => Sign.Top
        case  _                     => Sign.Top
    }

    ///
    /// Over-approximates integer `multiplication`.
    ///
    #approximates2((x, y) -> x * y)
    #strict2 #monotone2
    def times(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Sign.Bot, _)          => Sign.Bot
        case (_, Sign.Bot)          => Sign.Bot
        case (Sign.Neg, Sign.Neg)   => Sign.Pos
        case (Sign.Neg, Sign.Zer)   => Sign.Zer
        case (Sign.Neg, Sign.Pos)   => Sign.Neg
        case (Sign.Zer, Sign.Neg)   => Sign.Zer
        case (Sign.Zer, Sign.Zer)   => Sign.Zer
        case (Sign.Zer, Sign.Pos)   => Sign.Zer
        case (Sign.Pos, Sign.Neg)   => Sign.Neg
        case (Sign.Pos, Sign.Zer)   => Sign.Zer
        case (Sign.Pos, Sign.Pos)   => Sign.Pos
        case  _                     => Sign.Top
    }

    ///
    /// Over-approximates integer `division`.
    ///
    #approximates2((x, y) -> x / y)
    #strict2 #monotone2
    def divide(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Sign.Bot, _)          => Sign.Bot
        case (_, Sign.Bot)          => Sign.Bot
        case (Sign.Neg, Sign.Neg)   => Sign.Pos
        case (Sign.Neg, Sign.Zer)   => Sign.Bot
        case (Sign.Neg, Sign.Pos)   => Sign.Neg
        case (Sign.Zer, Sign.Neg)   => Sign.Zer
        case (Sign.Zer, Sign.Zer)   => Sign.Bot
        case (Sign.Zer, Sign.Pos)   => Sign.Zer
        case (Sign.Pos, Sign.Neg)   => Sign.Neg
        case (Sign.Pos, Sign.Zer)   => Sign.Bot
        case (Sign.Pos, Sign.Pos)   => Sign.Pos
        case  _                     => Sign.Top
    }

    ///
    /// Over-approximates integer `modulus`.
    ///
    #approximates2((x, y) -> x % y)
    #strict2 #monotone2
    def modulo(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Sign.Bot, _)          => Sign.Bot
        case (_, Sign.Bot)          => Sign.Bot
        case (Sign.Neg, Sign.Neg)   => Sign.Bot
        case (Sign.Neg, Sign.Zer)   => Sign.Bot
        case (Sign.Neg, Sign.Pos)   => Sign.Top
        case (Sign.Zer, Sign.Neg)   => Sign.Bot
        case (Sign.Zer, Sign.Zer)   => Sign.Bot
        case (Sign.Zer, Sign.Pos)   => Sign.Pos
        case (Sign.Pos, Sign.Neg)   => Sign.Top
        case (Sign.Pos, Sign.Zer)   => Sign.Bot
        case (Sign.Pos, Sign.Pos)   => Sign.Top
        case  _                     => Sign.Top
    }

    ///
    /// Over-approximates `bitwise negation`.
    ///
    #approximates1(x -> -x)
    #strict1 #monotone1
    def negate(e: Sign): Sign = match e with {
        case Sign.Bot => Sign.Bot
        case Sign.Zer => Sign.Neg
        case Sign.Neg => Sign.Top
        case Sign.Pos => Sign.Top
        case Sign.Top => Sign.Top
    }

    ///
    /// Over-approximates `bitwise and`.
    ///
    #approximates2((x, y) -> x & y)
    #strict2 #monotone2 #commutative #associative
    def and(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Sign.Bot, _)          => Sign.Bot
        case (_, Sign.Bot)          => Sign.Bot
        case (Sign.Neg, Sign.Neg)   => Sign.Neg
        case (Sign.Neg, Sign.Zer)   => Sign.Zer
        case (Sign.Neg, Sign.Pos)   => Sign.Pos
        case (Sign.Zer, Sign.Neg)   => Sign.Zer
        case (Sign.Zer, Sign.Zer)   => Sign.Zer
        case (Sign.Zer, Sign.Pos)   => Sign.Zer
        case (Sign.Pos, Sign.Neg)   => Sign.Pos
        case (Sign.Pos, Sign.Zer)   => Sign.Zer
        case (Sign.Pos, Sign.Pos)   => Sign.Pos
        case  _                     => Sign.Top
    }

    ///
    /// Over-approximates `bitwise or`.
    ///
    #approximates2((x, y) -> x | y)
    #strict2 #monotone2 #commutative #associative
    def or(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Sign.Bot, _)          => Sign.Bot
        case (_, Sign.Bot)          => Sign.Bot
        case (Sign.Neg, Sign.Neg)   => Sign.Neg
        case (Sign.Neg, Sign.Zer)   => Sign.Neg
        case (Sign.Neg, Sign.Pos)   => Sign.Top
        case (Sign.Zer, Sign.Neg)   => Sign.Neg
        case (Sign.Zer, Sign.Zer)   => Sign.Zer
        case (Sign.Zer, Sign.Pos)   => Sign.Pos
        case (Sign.Pos, Sign.Neg)   => Sign.Top
        case (Sign.Pos, Sign.Zer)   => Sign.Pos
        case (Sign.Pos, Sign.Pos)   => Sign.Pos
        case  _                     => Sign.Top
    }

    ///
    /// Over-approximates `bitwise xor`.
    ///
    #approximates2((x, y) -> x ^ y)
    #strict2 #monotone2 #commutative #associative
    def xor(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Sign.Bot, _)          => Sign.Bot
        case (_, Sign.Bot)          => Sign.Bot
        case (Sign.Neg, Sign.Neg)   => Sign.Top
        case (Sign.Neg, Sign.Zer)   => Sign.Neg
        case (Sign.Neg, Sign.Pos)   => Sign.Top
        case (Sign.Zer, Sign.Neg)   => Sign.Neg
        case (Sign.Zer, Sign.Zer)   => Sign.Zer
        case (Sign.Zer, Sign.Pos)   => Sign.Pos
        case (Sign.Pos, Sign.Neg)   => Sign.Top
        case (Sign.Pos, Sign.Zer)   => Sign.Pos
        case (Sign.Pos, Sign.Pos)   => Sign.Top
        case  _                     => Sign.Top
    }

    ///
    /// Over-approximates `bitwise left shift`.
    ///
    #approximates2((x, y) -> x << y)
    #strict2 #monotone2
    def leftShift(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
       case (Sign.Bot, _)          => Sign.Bot
       case (_, Sign.Bot)          => Sign.Bot
       case (Sign.Neg, Sign.Neg)   => Sign.Top
       case (Sign.Neg, Sign.Zer)   => Sign.Neg
       case (Sign.Neg, Sign.Pos)   => Sign.Top
       case (Sign.Zer, Sign.Neg)   => Sign.Top
       case (Sign.Zer, Sign.Zer)   => Sign.Zer
       case (Sign.Zer, Sign.Pos)   => Sign.Zer
       case (Sign.Pos, Sign.Neg)   => Sign.Top
       case (Sign.Pos, Sign.Zer)   => Sign.Pos
       case (Sign.Pos, Sign.Pos)   => Sign.Top
       case  _                     => Sign.Top
   }

    ///
    /// Over-approximates `bitwise right shift`.
    ///
    #approximates2((x, y) -> x >> y)
    #strict2 #monotone2
    def rightShift(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Sign.Bot, _)          => Sign.Bot
        case (_, Sign.Bot)          => Sign.Bot
        case (Sign.Neg, Sign.Neg)   => Sign.Top
        case (Sign.Neg, Sign.Zer)   => Sign.Neg
        case (Sign.Neg, Sign.Pos)   => Sign.Top
        case (Sign.Zer, Sign.Neg)   => Sign.Top
        case (Sign.Zer, Sign.Zer)   => Sign.Zer
        case (Sign.Zer, Sign.Pos)   => Sign.Zer
        case (Sign.Pos, Sign.Neg)   => Sign.Top
        case (Sign.Pos, Sign.Zer)   => Sign.Pos
        case (Sign.Pos, Sign.Pos)   => Sign.Pos
        case  _                     => Sign.Top
    }

    ///
    /// Over-approximates `equal`.
    ///
//    #strict2 #monotone2 #commutativeB // TODO: Requires more general laws
    def eq(e1: Sign, e2: Sign): Belnap/Belnap = match (e1, e2) with {
        case (Sign.Bot, _)          => Belnap/Belnap.Bot
        case (_, Sign.Bot)          => Belnap/Belnap.Bot
        case (Sign.Zer, Sign.Zer)   => Belnap/Belnap.True
        case _                      => Belnap/Belnap.Top
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
        case (Sign.Bot, _)          => Belnap/Belnap.Bot
        case (_, Sign.Bot)          => Belnap/Belnap.Bot
        case (Sign.Zer, Sign.Zer)   => Belnap/Belnap.False
        case _                      => Belnap/Belnap.Top
    }

    ///
    /// Over-approximates `less than or equal`.
    ///
    // @strict @monotone // TODO
    def lessEqual(e1: Sign, e2: Sign): Belnap/Belnap = Belnap/or(e1 `less` e2, e1 `eq` e2)

    let Sign<> = (Sign.Bot, Sign.Top, leq, lub, glb)

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
