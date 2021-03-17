///
/// An implementation of the interval domain.
///
/// This implementation uses an explicit bottom and top element.
///
/// See Cousot & Cousot '77 for details.
///
use Bool.{∧, ∨, →, ↔, ⊕};

namespace Domain/Interval {

    ///
    /// The elements of the interval lattice.
    ///
    enum Interval {
                  case Top,

        case Range(BigInt, BigInt),

                  case Bot
    }

    instance Eq[Interval] {
        pub def eq(e1: Interval, e2: Interval): Bool = match (e1, e2) {
            case (Top, Top)                     => true
            case (Range(x1, y1), Range(x2, y2)) => x1 == x2 and y1 == y2
            case (Bot, Bot)                     => true
            case _                              => false
        }
        pub def neq(e1: Interval, e2: Interval): Bool = not (e1 == e2)
    }

    ///
    /// Returns the canonical representation of the given interval `e`.
    ///
    pub def norm(x: Interval): Interval = match x {
        case Bot            => Bot
        case Range(b, e)    => if (b <= e) Range(b, e) else Bot
        case Top            => Top
    }

    ///
    /// Returns `true` iff `e1` is less than or equal to `e2`.
    ///
    #reflexive #antiSymmetric #transitive #leastElement(Bot) #greatestElement(Top)
    pub def leq(x: Interval, y: Interval): Bool = match (x, y) {
        case (Bot, _)                       => true
        case (Range(b1, e1), Range(b2, e2)) => b2 <= b1 and e1 <= e2
        case (_, Top)                       => true
        case _                              => false
    }

    ///
    /// Returns the least upper bound of `e1` and `e2`.
    ///
    #upperBound #leastUpperBound
    pub def lub(x: Interval, y: Interval): Interval = match (x, y) {
        case (Bot, _)                       => y
        case (_, Bot)                       => x
        case (Range(b1, e1), Range(b2, e2)) => norm(Range(BigInt.min(b1, b2), BigInt.max(e1, e2)))
        case _                              => Top
    }

    ///
    /// Returns the greatest lower bound of `e1` and `e2`.
    ///
    #lowerBound #greatestLowerBound
    pub def glb(x: Interval, y: Interval): Interval = match (x, y) {
        case (Top, _)                       => y
        case (_, Top)                       => x
        case (Range(b1, e1), Range(b2, e2)) => norm(Range(BigInt.max(b1, b2), BigInt.min(e1, e2)))
        case _                              => Bot
    }

    ///
    /// Abstracts a concrete integer into the interval domain.
    ///
    pub def alpha(i: BigInt): Interval = Range(i, i)

    ///
    /// Over-approximates integer `increment`.
    ///
    #safe1(x -> x + 1ii)
    #strict1 #monotone1
    pub def inc(x: Interval): Interval = match x {
        case Bot            => Bot
        case Range(b, e)    => norm(Range(b + 1ii, e + 1ii))
        case Top            => Top
    }

    ///
    /// Over-approximates integer `decrement`.
    ///
    #safe1(x -> x - 1ii)
    #strict1 #monotone1
    pub def dec(x: Interval): Interval = match x {
        case Bot            => Bot
        case Range(b, e)    => norm(Range(b - 1ii, e - 1ii))
        case Top            => Top
    }

    ///
    /// Over-approximates integer `addition`.
    ///
    #safe2((x, y) -> x + y)
    #strict2 #monotone2
    pub def plus(x: Interval, y: Interval): Interval = match (x, y) {
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
    pub def minus(x: Interval, y: Interval): Interval = match (x, y) {
        case (Bot, _)                       => Bot
        case (_, Bot)                       => Bot
        case (Range(b1, e1), Range(b2, e2)) => norm(Range(b1 - e2, e1 - b2))
        case _                              => Top
    }

    ///
    /// Over-approximates integer `multiplication`.
    ///
    #safe2((x, y) -> x * y)
    #strict2 /* undecidable #monotone2 */
    pub def times(x: Interval, y: Interval): Interval = match (x, y) {
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
    #safeBelnap2((x, y) -> x == y)
    #Bounded.strict2(Bot, Bot, Belnap/Belnap.Bot) #monotoneBelnap2 #commutativeBelnap
    pub def eq(x: Interval, y: Interval): Belnap.Belnap = match (x, y) {
        case (Bot, _)                                           => Belnap/Belnap.Bot
        case (_, Bot)                                           => Belnap/Belnap.Bot
        case (Range(b1, e1), Range(b2, e2)) => {
            if      ((b1 == e1) and ((e1 == b2) and (b2 == e2)))     Belnap/Belnap.True
            else if ((e1 < b2) or (e2 < b1))                       Belnap/Belnap.False
            else                                                   Belnap/Belnap.Top
        }
        case _                                                  => Belnap/Belnap.Top
    }

    /**
      * Over-approximates `not equal`.
      */
    #safeBelnap2((x, y) -> x != y)
    // NB: Strictness and monotonicity follows from the properties of `not` and `eq`.
    pub def neq(x: Interval, y: Interval): Belnap.Belnap = Belnap.not(eq(x, y))

    /**
      * Over-approximates `less than`.
      */
   #safeBelnap2((x, y) -> x < y)
   #Bounded.strict2(Bot, Bot, Belnap/Belnap.Bot) #monotoneBelnap2
   pub def less(x: Interval, y: Interval): Belnap.Belnap = match (x, y) {
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
    #safeBelnap2((x, y) -> x <= y)
    // NB: Strictness and monotonicity follows from the properties of `or` and `less`.
    pub def lessEqual(x: Interval, y: Interval): Belnap.Belnap = Belnap.or(x `less` y, x `eq` y)

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

    // ------------------------------------------------------------------------
    // Specialized Laws for Interval/Belnap.
    // ------------------------------------------------------------------------

    law commutativeBelnap(f: (Interval, Interval) -> Belnap.Belnap): Bool =
        ∀(x: Interval, y: Interval).
            f(norm(x), norm(y)) == f(norm(y), norm(x))

    law monotoneBelnap2(f: (Interval, Interval) -> Belnap.Belnap): Bool =
        let ⊑ = leq;
            ∀(x1: Interval, x2: Interval, y1: Interval, y2: Interval).
                    ((norm(x1) ⊑ norm(y1)) ∧ (norm(x2) ⊑ norm(y2))) → (f(norm(x1), norm(x2)) `Belnap.leq` f(norm(y1), norm(y2)))

    law safeBelnap2(fa: (Interval, Interval) -> Belnap.Belnap, fc: (BigInt, BigInt) -> Bool): Bool =
        ∀(x: BigInt, y: BigInt). Belnap.alpha(fc(x, y)) `Belnap.leq` fa(alpha(x), alpha(y))

}
