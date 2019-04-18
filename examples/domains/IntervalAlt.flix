///
/// An implementation of the IntervalAlt domain.
///
/// This implementation uses an explicit bottom and top element.
///
/// See Cousot & Cousot '77 for details.
///
namespace Domain/IntervalAlt {

    ///
    /// The elements of the IntervalAlt lattice.
    ///
    enum IntervalAlt {
                  case Top,

        case Range(BigInt, BigInt),

                  case Bot
    }

    ///
    /// Returns the maximum width of an interval before we go to top.
    ///
    def maxWidth(): BigInt = 10ii

    ///
    /// Returns the canonical representation of the given IntervalAlt `e`.
    ///
    def norm(e: IntervalAlt): IntervalAlt = match e with {
        case Bot            => Bot
        case Range(b, e)    => if (b <= e) (if (e - b <= maxWidth()) Range(b, e) else Top) else Bot
        case Top            => Top
    }

    ///
    /// Returns `true` iff `e1` is equal to `e2`.
    ///
    pub def equ(e1: IntervalAlt, e2: IntervalAlt): Bool = e1 == e2

    ///
    /// Returns `true` iff `e1` is less than or equal to `e2`.
    ///
    #reflexive #antiSymmetric #transitive #leastElement(Bot) #greatestElement(Top)
    def leq(e1: IntervalAlt, e2: IntervalAlt): Bool = match (e1, e2) with {
        case (Bot, _)                       => true
        case (Range(b1, e1), Range(b2, e2)) => b2 <= b1 && e1 <= e2
        case (_, Top)                       => true
        case _                              => false
    }

    ///
    /// Returns the least upper bound of `e1` and `e2`.
    ///
    #upperBound #leastUpperBound
    def lub(e1: IntervalAlt, e2: IntervalAlt): IntervalAlt = match (e1, e2) with {
        case (Bot, x)                       => x
        case (x, Bot)                       => x
        case (Range(b1, e1), Range(b2, e2)) => norm(Range(BigInt.min(b1, b2), BigInt.max(e1, e2)))
        case _                              => Top
    }

    ///
    /// Returns the greatest lower bound of `e1` and `e2`.
    ///
    #lowerBound #greatestLowerBound
    def glb(e1: IntervalAlt, e2: IntervalAlt): IntervalAlt = match (e1, e2) with {
        case (Top, x)                       => x
        case (x, Top)                       => x
        case (Range(b1, e1), Range(b2, e2)) => norm(Range(BigInt.max(b1, b2), BigInt.min(e1, e2)))
        case _                              => Bot
    }

    ///
    /// The lattice height function.
    ///
    #nonNegative #decreasing(equ, leq)
    pub def height(e: IntervalAlt): BigInt = match e with {
        case Top            => 0ii
        case Range(b, e)    => 99ii - (e - b)
        case Bot            => 100ii
    }

    ///
    /// Abstracts a concrete integer into the IntervalAlt domain.
    ///
    pub def alpha(i: BigInt): IntervalAlt = Range(i, i)

    ///
    /// Over-approximates integer `increment`.
    ///
    #safe1(x -> x + 1ii)
    #strict1 #monotone1
    pub def inc(e: IntervalAlt): IntervalAlt = match e with {
        case Bot            => Bot
        case Range(b, e)    => norm(Range(b + 1ii, e + 1ii))
        case Top            => Top
    }

    ///
    /// Over-approximates integer `decrement`.
    ///
    #safe1(x -> x - 1ii)
    #strict1 #monotone1
    pub def dec(e: IntervalAlt): IntervalAlt = match e with {
        case Bot            => Bot
        case Range(b, e)    => norm(Range(b - 1ii, e - 1ii))
        case Top            => Top
    }

    ///
    /// Over-approximates integer `addition`.
    ///
    #safe2((x, y) -> x + y)
    #strict2 #monotone2
    pub def plus(e1: IntervalAlt, e2: IntervalAlt): IntervalAlt = match (e1, e2) with {
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
    pub def minus(e1: IntervalAlt, e2: IntervalAlt): IntervalAlt = match (e1, e2) with {
        case (Bot, _)                       => Bot
        case (_, Bot)                       => Bot
        case (Range(b1, e1), Range(b2, e2)) => norm(Range(b1 - e2, e1 - b2))
        case _                              => Top
    }

    ///
    /// Over-approximates integer `multiplication`.
    ///
    #safe2((x, y) -> x * y)
    #strict2 /* #monotone2 loops/timeout */
    pub def times(e1: IntervalAlt, e2: IntervalAlt): IntervalAlt = match (e1, e2) with {
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
    pub def eq(e1: IntervalAlt, e2: IntervalAlt): Belnap.Belnap = match (e1, e2) with {
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
    #safeBelnap2((x, y) -> x != y)
    // NB: Strictness and monotonicity follows from the properties of `not` and `eq`.
    pub def neq(e1: IntervalAlt, e2: IntervalAlt): Belnap.Belnap = Belnap.not(eq(e1, e2))

    /**
      * Over-approximates `less than`.
      */
    #safeBelnap2((x, y) -> x < y)
    #Bounded.strict2(Bot, Bot, Belnap/Belnap.Bot) #monotoneBelnap2
    pub def less(e1: IntervalAlt, e2: IntervalAlt): Belnap.Belnap = match (e1, e2) with {
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
    pub def lessEqual(e1: IntervalAlt, e2: IntervalAlt): Belnap.Belnap = Belnap.or(e1 `less` e2, e1 `eq` e2)

    let IntervalAlt<> = (Bot, Top, equ, leq, lub, glb)

    // ------------------------------------------------------------------------
    // Specialized Laws
    // ------------------------------------------------------------------------

    law reflexive(⊑: (IntervalAlt, IntervalAlt) -> Bool): Bool =
        ∀(x: IntervalAlt). norm(x) ⊑ norm(x)

    law antiSymmetric(⊑: (IntervalAlt, IntervalAlt) -> Bool): Bool =
        ∀(x: IntervalAlt, y: IntervalAlt). ((norm(x) ⊑ norm(y)) ∧ (norm(y) ⊑ norm(x))) → (norm(x) == norm(y))

    law transitive(⊑: (IntervalAlt, IntervalAlt) -> Bool): Bool =
        ∀(x: IntervalAlt, y: IntervalAlt, z: IntervalAlt). ((norm(x) ⊑ norm(y)) ∧ (norm(y) ⊑ norm(z))) → (norm(x) ⊑ norm(z))

    law upperBound(⊔: (IntervalAlt, IntervalAlt) -> IntervalAlt): Bool =
        let ⊑ = leq;
            ∀(x: IntervalAlt, y: IntervalAlt).
                (norm(x) ⊑ (norm(x) ⊔ norm(y))) ∧ (norm(y) ⊑ (norm(x) ⊔ norm(y)))

    law leastUpperBound(⊔: (IntervalAlt, IntervalAlt) -> IntervalAlt): Bool =
        let ⊑ = leq;
            ∀(x: IntervalAlt, y: IntervalAlt, z: IntervalAlt).
                ((norm(x) ⊑ norm(z)) ∧ (norm(y) ⊑ norm(z))) → ((norm(x) ⊔ norm(y)) ⊑ norm(z))

    law lowerBound(⊓: (IntervalAlt, IntervalAlt) -> IntervalAlt): Bool =
        let ⊑ = leq;
            ∀(x: IntervalAlt, y: IntervalAlt).
                ((norm(x) ⊓ norm(y)) ⊑ norm(x)) ∧ ((norm(x) ⊓ norm(y)) ⊑ norm(y))

    law greatestLowerBound(⊓: (IntervalAlt, IntervalAlt) -> IntervalAlt): Bool =
        let ⊑ = leq;
            ∀(x: IntervalAlt, y: IntervalAlt, z: IntervalAlt).
                    ((norm(z) ⊑ norm(x)) ∧ (norm(z) ⊑ norm(y))) → (norm(z) ⊑ (norm(x) ⊓ norm(y)))

    law leastElement(⊑: (IntervalAlt, IntervalAlt) -> Bool, ⊥: IntervalAlt): Bool =
        ∀(x: IntervalAlt). ⊥ ⊑ norm(x)

    law greatestElement(⊑: (IntervalAlt, IntervalAlt) -> Bool, ⊤: IntervalAlt): Bool =
        ∀(x: IntervalAlt). norm(x) ⊑ ⊤

    law nonNegative(h: IntervalAlt -> BigInt): Bool =
        ∀(x: IntervalAlt). h(norm(x)) >= 0ii

    law decreasing(h: IntervalAlt -> BigInt, equ: (IntervalAlt, IntervalAlt) -> Bool, ⊑: (IntervalAlt, IntervalAlt) -> Bool): Bool =
        ∀(x1: IntervalAlt, y1: IntervalAlt).
            let x = norm(x1);
            let y = norm(y1);
                (x ⊑ y && !equ(x, y)) → (h(x) > h(y))

    law associative(f: (IntervalAlt, IntervalAlt) -> IntervalAlt): Bool =
        ∀(x: IntervalAlt, y: IntervalAlt, z: IntervalAlt).
                f(norm(x), f(norm(y), norm(z))) == f(f(norm(x), norm(y)), norm(z))

    law commutative(f: (IntervalAlt, IntervalAlt) -> IntervalAlt): Bool =
        ∀(x: IntervalAlt, y: IntervalAlt).
            f(norm(x), norm(y)) == f(norm(y), norm(x))

    law strict1(f: IntervalAlt -> IntervalAlt): Bool =
        f(Bot) == Bot

    law strict2(f: (IntervalAlt, IntervalAlt) -> IntervalAlt): Bool =
        f(Bot, Bot) == Bot

    law monotone1(f: IntervalAlt -> IntervalAlt): Bool =
        let ⊑ = leq;
            ∀(x: IntervalAlt, y: IntervalAlt).
                (norm(x) ⊑ norm(y)) → (f(norm(x)) ⊑ f(norm(y)))

    law monotone2(f: (IntervalAlt, IntervalAlt) -> IntervalAlt): Bool =
        let ⊑ = leq;
            ∀(x1: IntervalAlt, x2: IntervalAlt, y1: IntervalAlt, y2: IntervalAlt).
                    ((norm(x1) ⊑ norm(y1)) ∧ (norm(x2) ⊑ norm(y2))) → (f(norm(x1), norm(x2)) ⊑ f(norm(y1), norm(y2)))

    law safe1(fa: IntervalAlt -> IntervalAlt, fc: BigInt -> BigInt): Bool =
        ∀(x: BigInt). alpha(fc(x)) `leq` fa(alpha(x))

    law safe2(fa: (IntervalAlt, IntervalAlt) -> IntervalAlt, fc: (BigInt, BigInt) -> BigInt): Bool =
        ∀(x: BigInt, y: BigInt). alpha(fc(x, y)) `leq` fa(alpha(x), alpha(y))

    // ------------------------------------------------------------------------
    // Specialized Laws for IntervalAlt/Belnap.
    // ------------------------------------------------------------------------

    law commutativeBelnap(f: (IntervalAlt, IntervalAlt) -> Belnap.Belnap): Bool =
        ∀(x: IntervalAlt, y: IntervalAlt).
            f(norm(x), norm(y)) == f(norm(y), norm(x))

    law monotoneBelnap2(f: (IntervalAlt, IntervalAlt) -> Belnap.Belnap): Bool =
        let ⊑ = leq;
            ∀(x1: IntervalAlt, x2: IntervalAlt, y1: IntervalAlt, y2: IntervalAlt).
                    ((norm(x1) ⊑ norm(y1)) ∧ (norm(x2) ⊑ norm(y2))) → (f(norm(x1), norm(x2)) `Belnap.leq` f(norm(y1), norm(y2)))

    law safeBelnap2(fa: (IntervalAlt, IntervalAlt) -> Belnap.Belnap, fc: (BigInt, BigInt) -> Bool): Bool =
        ∀(x: BigInt, y: BigInt). Belnap.alpha(fc(x, y)) `Belnap.leq` fa(alpha(x), alpha(y))

}
