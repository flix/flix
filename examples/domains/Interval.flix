///
/// An implementation of the interval domain.
///
/// This implementation uses an explicit bottom and top element.
///
/// See Cousot & Cousot '77 for details.
///
namespace Domain/Interval {

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
    #safe1(x -> x + 1ii)
    #strict1 #monotone1
    def inc(e: Interval): Interval = match e with {
        case Bot            => Bot
        case Range(b, e)    => norm(Range(b + 1ii, e + 1ii))
        case Top            => Top
    }

    ///
    /// Over-approximates integer `decrement`.
    ///
    #safe1(x -> x - 1ii)
    #strict1 #monotone1
    def dec(e: Interval): Interval = match e with {
        case Bot            => Bot
        case Range(b, e)    => norm(Range(b - 1ii, e - 1ii))
        case Top            => Top
    }

    ///
    /// Over-approximates integer `addition`.
    ///
    #safe2((x, y) -> x + y)
    #strict2 #monotone2 #commutative
    def plus(e1: Interval, e2: Interval): Interval = match (e1, e2) with {
        case (Bot, _)                       => Bot
        case (_, Bot)                       => Bot
        case (Range(b1, e1), Range(b2, e2)) => norm(Range(b1 + b2, e1 + e2))
        case _                              => Top
    }

    ///
    /// Over-approximates integer `subtraction`.
    ///
    #safe2((x, y) -> x - y)
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
    #safe2((x, y) -> x * y)
    #strict2 /* #monotone2 */ /* #commutative */
    def times(e1: Interval, e2: Interval): Interval = match (e1, e2) with {
        case (Bot, _)                       => Bot
        case (_, Bot)                       => Bot
        case (Range(b1, e1), Range(b2, e2)) =>
            let b = BigInt.min(BigInt.min(b1 * b2, b1 * e2), BigInt.min(e1 * b2, e1 * e2));
            let e = BigInt.max(BigInt.max(b1 * b2, b1 * e2), BigInt.max(e1 * b2, e1 * e2));
                norm(Range(b, e))
        case _                              => Top
    }

    /**
      * Over-approximates `equal`.
      */
    //@strict @monotone @commutative // TODO
    #Bounded.strict2(Bot, Bot, Belnap/Belnap.Bot) //#commutative
    def eq(e1: Interval, e2: Interval): Belnap.Belnap = match (e1, e2) with {
        case (Bot, _)                                           => Belnap/Belnap.Bot
        case (_, Bot)                                           => Belnap/Belnap.Bot
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
    // Specialized Laws
    // ------------------------------------------------------------------------

    law reflexive(⊑: (Interval, Interval) -> Bool): Bool =
        ∀(x: Interval). norm(x) ⊑ norm(x)

    law antiSymmetric(⊑: (Interval, Interval) -> Bool): Bool =
        ∀(x: Interval, y: Interval). ((norm(x) ⊑ norm(y)) ∧ (norm(y) ⊑ norm(x))) → (norm(x) == norm(y))

    law transitive(⊑: (Interval, Interval) -> Bool): Bool =
        ∀(x: Interval, y: Interval, z: Interval). ((norm(x) ⊑ norm(y)) ∧ (norm(y) ⊑ norm(z))) → (norm(x) ⊑ norm(z))

    law upperBound(⊔: (Interval, Interval) -> Interval): Bool =
        let ⊑ = leq;
            ∀(x: Interval, y: Interval).
                (norm(x) ⊑ (norm(x) ⊔ norm(y))) ∧ (norm(y) ⊑ (norm(x) ⊔ norm(y)))

    law leastUpperBound(⊔: (Interval, Interval) -> Interval): Bool =
        let ⊑ = leq;
            ∀(x: Interval, y: Interval, z: Interval).
                ((norm(x) ⊑ norm(z)) ∧ (norm(y) ⊑ norm(z))) → ((norm(x) ⊔ norm(y)) ⊑ norm(z))

    law lowerBound(⊓: (Interval, Interval) -> Interval): Bool =
        let ⊑ = leq;
            ∀(x: Interval, y: Interval).
                ((norm(x) ⊓ norm(y)) ⊑ norm(x)) ∧ ((norm(x) ⊓ norm(y)) ⊑ norm(y))

    law greatestLowerBound(⊓: (Interval, Interval) -> Interval): Bool =
        let ⊑ = leq;
            ∀(x: Interval, y: Interval, z: Interval).
                    ((norm(z) ⊑ norm(x)) ∧ (norm(z) ⊑ norm(y))) → (norm(z) ⊑ (norm(x) ⊓ norm(y)))

    law leastElement(⊑: (Interval, Interval) -> Bool, ⊥: Interval): Bool =
        ∀(x: Interval). ⊥ ⊑ norm(x)

    law greatestElement(⊑: (Interval, Interval) -> Bool, ⊤: Interval): Bool =
        ∀(x: Interval). norm(x) ⊑ ⊤

    law associative(f: (Interval, Interval) -> Interval): Bool =
        ∀(x: Interval, y: Interval, z: Interval).
                f(norm(x), f(norm(y), norm(z))) == f(f(norm(x), norm(y)), norm(z))

    law commutative(f: (Interval, Interval) -> Interval): Bool =
        ∀(x: Interval, y: Interval).
            f(norm(x), norm(y)) == f(norm(y), norm(x))

    law strict1(f: Interval -> Interval): Bool =
        f(Bot) == Bot

    law strict2(f: (Interval, Interval) -> Interval): Bool =
        f(Bot, Bot) == Bot

    law monotone1(f: Interval -> Interval): Bool =
        let ⊑ = leq;
            ∀(x: Interval, y: Interval).
                (norm(x) ⊑ norm(y)) → (f(norm(x)) ⊑ f(norm(y)))

    law monotone2(f: (Interval, Interval) -> Interval): Bool =
        let ⊑ = leq;
            ∀(x1: Interval, x2: Interval, y1: Interval, y2: Interval).
                    ((norm(x1) ⊑ norm(y1)) ∧ (norm(x2) ⊑ norm(y2))) → (f(norm(x1), norm(x2)) ⊑ f(norm(y1), norm(y2)))

    law safe1(fa: Interval -> Interval, fc: BigInt -> BigInt): Bool =
        ∀(x: BigInt). alpha(fc(x)) `leq` fa(alpha(x))

    law safe2(fa: (Interval, Interval) -> Interval, fc: (BigInt, BigInt) -> BigInt): Bool =
        ∀(x: BigInt, y: BigInt). alpha(fc(x, y)) `leq` fa(alpha(x), alpha(y))

}
