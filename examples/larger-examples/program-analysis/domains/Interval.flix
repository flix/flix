///
/// An implementation of the interval domain.
///
/// This implementation uses an explicit Interval.Bottom and Interval.Top element.
///
/// See Cousot & Cousot '77 for details.
///

mod Domain.Interval {

    ///
    /// The elements of the interval lattice.
    ///
    enum Interval with Eq {
                  case Top,

        case Range(BigInt, BigInt),

                  case Bot
    }

    ///
    /// Returns the canonical representation of the given interval `e`.
    ///
    pub def norm(x: Interval): Interval = match x {
        case Interval.Bot         => Interval.Bot
        case Interval.Range(b, e) => if (b <= e) Interval.Range(b, e) else Interval.Bot
        case Interval.Top         => Interval.Top
    }

    ///
    /// Returns `true` iff `e1` is less than or equal to `e2`.
    ///
//    #reflexive #antiSymmetric #transitive #leastElement(Interval.Bot) #greatestElement(Interval.Top)
    pub def leq(x: Interval, y: Interval): Bool = match (x, y) {
        case (Interval.Bot, _)                                => true
        case (Interval.Range(b1, e1), Interval.Range(b2, e2)) => b2 <= b1 and e1 <= e2
        case (_, Interval.Top)                                => true
        case _                                                => false
    }

    ///
    /// Returns the least upper bound of `e1` and `e2`.
    ///
//    #upperBound #leastUpperBound
    pub def lub(x: Interval, y: Interval): Interval = match (x, y) {
        case (Interval.Bot, _)                                => y
        case (_, Interval.Bot)                                => x
        case (Interval.Range(b1, e1), Interval.Range(b2, e2)) => norm(Interval.Range(BigInt.min(b1, b2), BigInt.max(e1, e2)))
        case _                                                => Interval.Top
    }

    ///
    /// Returns the greatest lower bound of `e1` and `e2`.
    ///
//    #lowerBound #greatestLowerBound
    pub def glb(x: Interval, y: Interval): Interval = match (x, y) {
        case (Interval.Top, _)                                => y
        case (_, Interval.Top)                                => x
        case (Interval.Range(b1, e1), Interval.Range(b2, e2)) => norm(Interval.Range(BigInt.max(b1, b2), BigInt.min(e1, e2)))
        case _                                                => Interval.Bot
    }

    ///
    /// Abstracts a concrete integer into the interval domain.
    ///
    pub def alpha(i: BigInt): Interval = Interval.Range(i, i)

    ///
    /// Over-approximates integer `increment`.
    ///
//    #safe1(x -> x + 1ii)
//    #strict1 #monotone1
    pub def inc(x: Interval): Interval = match x {
        case Interval.Bot         => Interval.Bot
        case Interval.Range(b, e) => norm(Interval.Range(b + 1ii, e + 1ii))
        case Interval.Top         => Interval.Top
    }

    ///
    /// Over-approximates integer `decrement`.
    ///
//    #safe1(x -> x - 1ii)
//    #strict1 #monotone1
    pub def dec(x: Interval): Interval = match x {
        case Interval.Bot         => Interval.Bot
        case Interval.Range(b, e) => norm(Interval.Range(b - 1ii, e - 1ii))
        case Interval.Top         => Interval.Top
    }

    ///
    /// Over-approximates integer `addition`.
    ///
//    #safe2((x, y) -> x + y)
//    #strict2 #monotone2
    pub def plus(x: Interval, y: Interval): Interval = match (x, y) {
        case (Interval.Bot, _)                                => Interval.Bot
        case (_, Interval.Bot)                                => Interval.Bot
        case (Interval.Range(b1, e1), Interval.Range(b2, e2)) => norm(Interval.Range(b1 + b2, e1 + e2))
        case _                                                => Interval.Top
    }

    ///
    /// Over-approximates integer `subtraction`.
    ///
//    #safe2((x, y) -> x - y)
//    #strict2 #monotone2
    pub def minus(x: Interval, y: Interval): Interval = match (x, y) {
        case (Interval.Bot, _)                                => Interval.Bot
        case (_, Interval.Bot)                                => Interval.Bot
        case (Interval.Range(b1, e1), Interval.Range(b2, e2)) => norm(Interval.Range(b1 - e2, e1 - b2))
        case _                                                => Interval.Top
    }

    ///
    /// Over-approximates integer `multiplication`.
    ///
//    #safe2((x, y) -> x * y)
//    #strict2 /* undecidable #monotone2 */
    pub def times(x: Interval, y: Interval): Interval = match (x, y) {
        case (Interval.Bot, _)                                => Interval.Bot
        case (_, Interval.Bot)                                => Interval.Bot
        case (Interval.Range(b1, e1), Interval.Range(b2, e2)) =>
            let b = BigInt.min(BigInt.min(b1 * b2, b1 * e2), BigInt.min(e1 * b2, e1 * e2));
            let e = BigInt.max(BigInt.max(b1 * b2, b1 * e2), BigInt.max(e1 * b2, e1 * e2));
                norm(Interval.Range(b, e))
        case _                                                => Interval.Top
    }

    // ------------------------------------------------------------------------
    // Specialized Laws
    // ------------------------------------------------------------------------

//    law reflexive(⊑: (Interval, Interval) -> Bool): Bool =
//        ∀(x: Interval). norm(x) ⊑ norm(x)

//    law antiSymmetric(⊑: (Interval, Interval) -> Bool): Bool =
//        ∀(x: Interval, y: Interval). ((norm(x) ⊑ norm(y)) ∧ (norm(y) ⊑ norm(x))) → (norm(x) == norm(y))

//    law transitive(⊑: (Interval, Interval) -> Bool): Bool =
//        ∀(x: Interval, y: Interval, z: Interval). ((norm(x) ⊑ norm(y)) ∧ (norm(y) ⊑ norm(z))) → (norm(x) ⊑ norm(z))

//    law upperBound(⊔: (Interval, Interval) -> Interval): Bool =
//        let ⊑ = leq;
//            ∀(x: Interval, y: Interval).
//                (norm(x) ⊑ (norm(x) ⊔ norm(y))) ∧ (norm(y) ⊑ (norm(x) ⊔ norm(y)))

//    law leastUpperBound(⊔: (Interval, Interval) -> Interval): Bool =
//        let ⊑ = leq;
//            ∀(x: Interval, y: Interval, z: Interval).
//                ((norm(x) ⊑ norm(z)) ∧ (norm(y) ⊑ norm(z))) → ((norm(x) ⊔ norm(y)) ⊑ norm(z))

//    law lowerBound(⊓: (Interval, Interval) -> Interval): Bool =
//        let ⊑ = leq;
//            ∀(x: Interval, y: Interval).
//                ((norm(x) ⊓ norm(y)) ⊑ norm(x)) ∧ ((norm(x) ⊓ norm(y)) ⊑ norm(y))

//    law greatestLowerBound(⊓: (Interval, Interval) -> Interval): Bool =
//        let ⊑ = leq;
//            ∀(x: Interval, y: Interval, z: Interval).
//                    ((norm(z) ⊑ norm(x)) ∧ (norm(z) ⊑ norm(y))) → (norm(z) ⊑ (norm(x) ⊓ norm(y)))

//    law leastElement(⊑: (Interval, Interval) -> Bool, ⊥: Interval): Bool =
//        ∀(x: Interval). ⊥ ⊑ norm(x)

//    law greatestElement(⊑: (Interval, Interval) -> Bool, ⊤: Interval): Bool =
//        ∀(x: Interval). norm(x) ⊑ ⊤

//    law associative(f: (Interval, Interval) -> Interval): Bool =
//        ∀(x: Interval, y: Interval, z: Interval).
//                f(norm(x), f(norm(y), norm(z))) == f(f(norm(x), norm(y)), norm(z))

//    law commutative(f: (Interval, Interval) -> Interval): Bool =
//        ∀(x: Interval, y: Interval).
//            f(norm(x), norm(y)) == f(norm(y), norm(x))

//    law strict1(f: Interval -> Interval): Bool =
//        f(Interval.Bot) == Interval.Bot

//    law strict2(f: (Interval, Interval) -> Interval): Bool =
//        f(Interval.Bot, Interval.Bot) == Interval.Bot

//    law monotone1(f: Interval -> Interval): Bool =
//        let ⊑ = leq;
//            ∀(x: Interval, y: Interval).
//                (norm(x) ⊑ norm(y)) → (f(norm(x)) ⊑ f(norm(y)))

//    law monotone2(f: (Interval, Interval) -> Interval): Bool =
//        let ⊑ = leq;
//            ∀(x1: Interval, x2: Interval, y1: Interval, y2: Interval).
//                    ((norm(x1) ⊑ norm(y1)) ∧ (norm(x2) ⊑ norm(y2))) → (f(norm(x1), norm(x2)) ⊑ f(norm(y1), norm(y2)))

//    law safe1(fa: Interval -> Interval, fc: BigInt -> BigInt): Bool =
//        ∀(x: BigInt). alpha(fc(x)) `leq` fa(alpha(x))

//    law safe2(fa: (Interval, Interval) -> Interval, fc: (BigInt, BigInt) -> BigInt): Bool =
//        ∀(x: BigInt, y: BigInt). alpha(fc(x, y)) `leq` fa(alpha(x), alpha(y))

}
