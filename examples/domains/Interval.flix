///
/// An implementation of the interval domain.
///
/// This implementation uses an explicit bottom and top element.
///
/// See Cousot & Cousot '77 for details.
///
namespace Domain/Interval {

    // TODO: Add to TestExamples.

    ///
    /// The elements of the interval lattice.
    ///
    enum Interval {
                  case Top,

        case Range(BigInt, BigInt),

                  case Bot
    }
    
    ///
    /// Returns the canonical representation of the given interval `e`.
    ///
    def norm(e: Interval): Interval = match e with {
        case Bot            => Bot
        case Range(b, e)    => if (b <= e) Range(b, e) else Bot
        case Top            => Top
    }

    ///
    /// Returns `true` iff `e1` is less than or equal to `e2`.
    ///
    #reflexive #antiSymmetric #transitive #leastElement(Bot) #greatestElement(Top)
    def leq(e1: Interval, e2: Interval): Bool = match (e1, e2) with {
        case (Bot, _)                       => true
        case (Range(b1, e1), Range(b2, e2)) => b2 <= b1 && e1 <= e2
        case (_, Top)                       => true
        case _                              => false
    }

    ///
    /// Returns the least upper bound of `e1` and `e2`.
    ///
    #upperBound #leastUpperBound #commutative /* slow #associative */
    def lub(e1: Interval, e2: Interval): Interval = match (e1, e2) with {
        case (Bot, x)                       => x
        case (x, Bot)                       => x
        case (Range(b1, e1), Range(b2, e2)) => norm(Range(BigInt.min(b1, b2), BigInt.max(e1, e2)))
        case _                              => Top
    }

    ///
    /// Returns the greatest lower bound of `e1` and `e2`.
    ///
    #lowerBound #greatestLowerBound #commutative /* slow #associative*/
    def glb(e1: Interval, e2: Interval): Interval = match (e1, e2) with {
        case (Top, x)                       => x
        case (x, Top)                       => x
        case (Range(b1, e1), Range(b2, e2)) => norm(Range(BigInt.max(b1, b2), BigInt.min(e1, e2)))
        case _                              => Bot
    }

    ///
    /// Abstracts a concrete integer into the interval domain.
    ///
    def alpha(i: BigInt): Interval = Range(i, i)

    ///
    /// Over-approximates integer `increment`.
    ///
    #approximates1(x -> x + 1ii)
    #strict1 #monotone1
    def inc(e: Interval): Interval = match e with {
        case Bot            => Bot
        case Range(b, e)    => norm(Range(b + 1ii, e + 1ii))
        case Top            => Top
    }

    ///
    /// Over-approximates integer `decrement`.
    ///
    #approximates1(x -> x - 1ii)
    #strict1 #monotone1
    def dec(e: Interval): Interval = match e with {
        case Bot            => Bot
        case Range(b, e)    => norm(Range(b - 1ii, e - 1ii))
        case Top            => Top
    }

    ///
    /// Over-approximates integer `addition`.
    ///
    #approximates2((x, y) -> x + y)
    #strict2 #monotone2 #commutative #associative
    def plus(e1: Interval, e2: Interval): Interval = match (e1, e2) with {
        case (Bot, _)                       => Bot
        case (_, Bot)                       => Bot
        case (Range(b1, e1), Range(b2, e2)) => norm(Range(b1 + b2, e1 + e2))
        case _                              => Top
    }

    ///
    /// Over-approximates integer `subtraction`.
    ///
    #approximates2((x, y) -> x - y)
    #strict2 #monotone2
    def minus(e1: Interval, e2: Interval): Interval = match (e1, e2) with {
        case (Bot, _)                       => Bot
        case (_, Bot)                       => Bot
        case (Range(b1, e1), Range(b2, e2)) => norm(Range(b1 - e2, e1 - b2))
        case _                              => Top
    }

    ///
    /// Over-approximates integer `multiplication`.
    ///
    #approximates2((x, y) -> x * y)
    #strict2 #monotone2 #commutative #associative
    def times(e1: Interval, e2: Interval): Interval = match (e1, e2) with {
        case (Bot, _)                       => Bot
        case (_, Bot)                       => Bot
        case (Range(b1, e1), Range(b2, e2)) => norm(Range(b1 * b2, e1 * e2))
        case _                              => Top
    }

    // TODO: Remaining operations.

    /**
     * Over-approximates integer `division`.
     */
    //@strict @monotone // TODO
    def divide(e1: Interval, e2: Interval): Interval = match (e1, e2) with {
        case (Bot, _)                                  => Bot
        case (_, Bot)                                  => Bot
        case (_, Range(0ii, 0ii))                      => Bot
        case (Range(b1, e1), Range(b2, e2))   =>
            if (b2 <= 0ii && 0ii <= e2)
                Top
            else
                Range(b1 / e2, e1 / b2)
        case _                                                  => Top
    }

//    /**
//     * Over-approximates `bitwise negation`.
//     */
//    @strict @monotone
//    fn negate(e: Interval): Interval = match e with {
//        case Bot           => Bot
//        case Range(b, e)   =>
//            ???: Interval // TODO
//        case Top           => Top
//    }

    /**
      * Over-approximates `equal`.
      */
    //@strict @monotone @commutative // TODO
    def eq(e1: Interval, e2: Interval): Belnap.Belnap = match (e1, e2) with {
        case (Bot, _)                                  => Belnap/Belnap.Bot
        case (_, Bot)                                  => Belnap/Belnap.Bot
        case (Range(b1, e1), Range(b2, e2))   => switch {
            case (b1 == e1) && ((e1 == b2) && (b2 == e2))       => Belnap/Belnap.True
            case (e1 < b2) || (e2 < b1)                         => Belnap/Belnap.False
            case true                                           => Belnap/Belnap.Top
        }
        case _                                                  => Belnap/Belnap.Top
    }

    /**
      * Over-approximates `not equal`.
      */
    //@strict @monotone @commutative // TODO
    def neq(e1: Interval, e2: Interval): Belnap.Belnap = Belnap.not(eq(e1, e2))

    /**
      * Over-approximates `less than`.
      */
    //@strict @monotone // TODO
    def less(e1: Interval, e2: Interval): Belnap.Belnap = match (e1, e2) with {
        case (Bot, _)                                  => Belnap/Belnap.Bot
        case (_, Bot)                                  => Belnap/Belnap.Bot
        case (Range(b1, e1), Range(b2, e2))   =>
            if (e1 < b2)
                Belnap/Belnap.True
            else
                if (e2 < b1)
                    Belnap/Belnap.False
                else
                    Belnap/Belnap.Top

        case _                                                  => Belnap/Belnap.Top
    }

    /**
      * Over-approximates `less than or equal`.
      */
    //@strict @monotone // TODO
    def lessEqual(e1: Interval, e2: Interval): Belnap.Belnap = Belnap.or(e1 `less` e2, e1 `eq` e2)

    let Interval<> = (Bot, Top, leq, lub, glb)





    // ------------------------------------------------------------------------
    // TODO: Refactor
    // ------------------------------------------------------------------------

    law reflexive(⊑: (Interval, Interval) -> Bool): Bool = ∀(x: Interval). x ⊑ x // TODO: Use Prelude

    law antiSymmetric(⊑: (Interval, Interval) -> Bool): Bool = ∀(x: Interval, y: Interval). ((x ⊑ y) ∧ (y ⊑ x)) → (x == y) // TODO: Use Prelude

    law transitive(⊑: (Interval, Interval) -> Bool): Bool = ∀(x: Interval, y: Interval, z: Interval). ((x ⊑ y) ∧ (y ⊑ z)) → (x ⊑ z) // TODO: Use Prelude

    law upperBound(⊔: (Interval, Interval) -> Interval): Bool = // TODO: Should call prelude
        let ⊑ = leq;
            ∀(xx: Interval, yy: Interval).
                let x = norm(xx);
                let y = norm(yy);
                    (x ⊑ (x ⊔ y)) ∧ (y ⊑ (x ⊔ y))

    law leastUpperBound(⊔: (Interval, Interval) -> Interval): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(xx: Interval, yy: Interval, zz: Interval).
            let x = norm(xx);
            let y = norm(yy);
            let z = norm(zz);
                ((x ⊑ z) ∧ (y ⊑ z)) → ((x ⊔ y) ⊑ z)

    law lowerBound(⊓: (Interval, Interval) -> Interval): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: Interval, y: Interval). ((x ⊓ y) ⊑ x) ∧ ((x ⊓ y) ⊑ y)

    law greatestLowerBound(⊓: (Interval, Interval) -> Interval): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(xx: Interval, yy: Interval, zz: Interval).
                let x = norm(xx);
                let y = norm(yy);
                let z = norm(zz);
                    ((z ⊑ x) ∧ (z ⊑ y)) → (z ⊑ (x ⊓ y))

    law leastElement(⊑: (Interval, Interval) -> Bool, ⊥: Interval): Bool = ∀(x: Interval). ⊥ ⊑ x  // TODO: Should call prelude

    law greatestElement(⊑: (Interval, Interval) -> Bool, ⊤: Interval): Bool = ∀(x: Interval). x ⊑ ⊤  // TODO: Should call prelude

    law associative(f: (Interval, Interval) -> Interval): Bool = // TODO: Use Prelude
        ∀(xx: Interval, yy: Interval, zz: Interval).
            let x = norm(xx);
            let y = norm(yy);
            let z = norm(zz);
                f(x, f(y, z)) == f(f(x, y), z)

    law commutative(f: (Interval, Interval) -> Interval): Bool = // TODO: Use Prelude
        ∀(x: Interval, y: Interval). f(x, y) == f(y, x)

    law strict1(f: Interval -> Interval): Bool =  // TODO: Should call prelude
        f(Bot) == Bot

    law strict2(f: (Interval, Interval) -> Interval): Bool =  // TODO: Should call prelude
        f(Bot, Bot) == Bot

    law monotone1(f: Interval -> Interval): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: Interval, y: Interval). (x ⊑ y) → (f(x) ⊑ f(y))

    law monotone2(f: (Interval, Interval) -> Interval): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x1: Interval, x2: Interval, y1: Interval, y2: Interval). ((x1 ⊑ y1) ∧ (x2 ⊑ y2) ) → (f(x1, x2) ⊑ f(y1, y2))

    // TODO: Names
    law approximates1(fa: Interval -> Interval, fc: BigInt -> BigInt): Bool =
        ∀(x: BigInt). alpha(fc(x)) `leq` fa(alpha(x))

    // TODO: Names
    law approximates2(fa: (Interval, Interval) -> Interval, fc: (BigInt, BigInt) -> BigInt): Bool =
        ∀(x: BigInt, y: BigInt). alpha(fc(x, y)) `leq` fa(alpha(x), alpha(y))




}


    // TODO: Consider reduced product with Parity and Sign.
