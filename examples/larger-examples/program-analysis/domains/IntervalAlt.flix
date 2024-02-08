///
/// An implementation of the IntervalAlt domain.
///
/// This implementation uses an explicit bottom and top element.
///
/// See Cousot & Cousot '77 for details.
///

mod Domain.IntervalAlt {

    ///
    /// The elements of the IntervalAlt lattice.
    ///
    enum IntervalAlt with Eq {
                  case Top,

        case Range(BigInt, BigInt),

                  case Bot
    }

    ///
    /// Returns the maximum width of an interval before we go to top.
    ///
    pub def maxWidth(): BigInt = 10ii

    ///
    /// Returns the canonical representation of the given IntervalAlt `e`.
    ///
    pub def norm(x: IntervalAlt): IntervalAlt = match x {
        case IntervalAlt.Bot         => IntervalAlt.Bot
        case IntervalAlt.Range(b, e) => if (b <= e) (if (e - b <= maxWidth()) IntervalAlt.Range(b, e) else IntervalAlt.Top) else IntervalAlt.Bot
        case IntervalAlt.Top         => IntervalAlt.Top
    }

    ///
    /// Returns `true` iff `e1` is equal to `e2`.
    ///
    pub def equ(e1: IntervalAlt, e2: IntervalAlt): Bool = e1 == e2

    ///
    /// Returns `true` iff `e1` is less than or equal to `e2`.
    ///
//    #reflexive #antiSymmetric #transitive #leastElement(IntervalAlt.Bot) #greatestElement(IntervalAlt.Top)
    pub def leq(x: IntervalAlt, y: IntervalAlt): Bool = match (x, y) {
        case (IntervalAlt.Bot, _)                                   => true
        case (IntervalAlt.Range(b1, e1), IntervalAlt.Range(b2, e2)) => b2 <= b1 and e1 <= e2
        case (_, IntervalAlt.Top)                                   => true
        case _                                                      => false
    }

    ///
    /// Returns the least upper bound of `e1` and `e2`.
    ///
//    #upperBound #leastUpperBound
    pub def lub(x: IntervalAlt, y: IntervalAlt): IntervalAlt = match (x, y) {
        case (IntervalAlt.Bot, _)                                   => y
        case (_, IntervalAlt.Bot)                                   => x
        case (IntervalAlt.Range(b1, e1), IntervalAlt.Range(b2, e2)) => norm(IntervalAlt.Range(BigInt.min(b1, b2), BigInt.max(e1, e2)))
        case _                                                      => IntervalAlt.Top
    }

    ///
    /// Returns the greatest lower bound of `e1` and `e2`.
    ///
//    #lowerBound #greatestLowerBound
    pub def glb(x: IntervalAlt, y: IntervalAlt): IntervalAlt = match (x, y) {
        case (IntervalAlt.Top, _)                                   => y
        case (_, IntervalAlt.Top)                                   => x
        case (IntervalAlt.Range(b1, e1), IntervalAlt.Range(b2, e2)) => norm(IntervalAlt.Range(BigInt.max(b1, b2), BigInt.min(e1, e2)))
        case _                                                      => IntervalAlt.Bot
    }

    ///
    /// The lattice height function.
    ///
//    #nonNegative #decreasing(equ, leq)
    pub def height(x: IntervalAlt): BigInt = match x {
        case IntervalAlt.Top         => 0ii
        case IntervalAlt.Range(b, e) => 99ii - (e - b)
        case IntervalAlt.Bot         => 100ii
    }

    ///
    /// Abstracts a concrete integer into the IntervalAlt domain.
    ///
    pub def alpha(i: BigInt): IntervalAlt = IntervalAlt.Range(i, i)

    ///
    /// Over-approximates integer `increment`.
    ///
//    #safe1(x -> x + 1ii)
//    #strict1 #monotone1
    pub def inc(x: IntervalAlt): IntervalAlt = match x {
        case IntervalAlt.Bot         => IntervalAlt.Bot
        case IntervalAlt.Range(b, e) => norm(IntervalAlt.Range(b + 1ii, e + 1ii))
        case IntervalAlt.Top         => IntervalAlt.Top
    }

    ///
    /// Over-approximates integer `decrement`.
    ///
//    #safe1(x -> x - 1ii)
//    #strict1 #monotone1
    pub def dec(x: IntervalAlt): IntervalAlt = match x {
        case IntervalAlt.Bot         => IntervalAlt.Bot
        case IntervalAlt.Range(b, e) => norm(IntervalAlt.Range(b - 1ii, e - 1ii))
        case IntervalAlt.Top         => IntervalAlt.Top
    }

    ///
    /// Over-approximates integer `addition`.
    ///
//    #safe2((x, y) -> x + y)
//    #strict2 #monotone2
    pub def plus(x: IntervalAlt, y: IntervalAlt): IntervalAlt = match (x, y) {
        case (IntervalAlt.Bot, _)                                   => IntervalAlt.Bot
        case (_, IntervalAlt.Bot)                                   => IntervalAlt.Bot
        case (IntervalAlt.Range(b1, e1), IntervalAlt.Range(b2, e2)) => norm(IntervalAlt.Range(b1 + b2, e1 + e2))
        case _                                                      => IntervalAlt.Top
    }

    ///
    /// Over-approximates integer `subtraction`.
    ///
//    #safe2((x, y) -> x - y)
//    #strict2 #monotone2
    pub def minus(x: IntervalAlt, y: IntervalAlt): IntervalAlt = match (x, y) {
        case (IntervalAlt.Bot, _)                                   => IntervalAlt.Bot
        case (_, IntervalAlt.Bot)                                   => IntervalAlt.Bot
        case (IntervalAlt.Range(b1, e1), IntervalAlt.Range(b2, e2)) => norm(IntervalAlt.Range(b1 - e2, e1 - b2))
        case _                                                      => IntervalAlt.Top
    }

    ///
    /// Over-approximates integer `multiplication`.
    ///
//    #safe2((x, y) -> x * y)
//    #strict2 /* #monotone2 loops/timeout */
    pub def times(x: IntervalAlt, y: IntervalAlt): IntervalAlt = match (x, y) {
        case (IntervalAlt.Bot, _)                                   => IntervalAlt.Bot
        case (_, IntervalAlt.Bot)                                   => IntervalAlt.Bot
        case (IntervalAlt.Range(b1, e1), IntervalAlt.Range(b2, e2)) =>
            let b = BigInt.min(BigInt.min(b1 * b2, b1 * e2), BigInt.min(e1 * b2, e1 * e2));
            let e = BigInt.max(BigInt.max(b1 * b2, b1 * e2), BigInt.max(e1 * b2, e1 * e2));
                norm(IntervalAlt.Range(b, e))
        case _                                                      => IntervalAlt.Top
    }

    // ------------------------------------------------------------------------
    // Specialized Laws
    // ------------------------------------------------------------------------

//    law reflexive(⊑: (IntervalAlt, IntervalAlt) -> Bool): Bool =
//        ∀(x: IntervalAlt). norm(x) ⊑ norm(x)

//    law antiSymmetric(⊑: (IntervalAlt, IntervalAlt) -> Bool): Bool =
//        ∀(x: IntervalAlt, y: IntervalAlt). ((norm(x) ⊑ norm(y)) ∧ (norm(y) ⊑ norm(x))) → (norm(x) == norm(y))

//    law transitive(⊑: (IntervalAlt, IntervalAlt) -> Bool): Bool =
//        ∀(x: IntervalAlt, y: IntervalAlt, z: IntervalAlt). ((norm(x) ⊑ norm(y)) ∧ (norm(y) ⊑ norm(z))) → (norm(x) ⊑ norm(z))

//    law upperBound(⊔: (IntervalAlt, IntervalAlt) -> IntervalAlt): Bool =
//        let ⊑ = leq;
//            ∀(x: IntervalAlt, y: IntervalAlt).
//                (norm(x) ⊑ (norm(x) ⊔ norm(y))) ∧ (norm(y) ⊑ (norm(x) ⊔ norm(y)))

//    law leastUpperBound(⊔: (IntervalAlt, IntervalAlt) -> IntervalAlt): Bool =
//        let ⊑ = leq;
//            ∀(x: IntervalAlt, y: IntervalAlt, z: IntervalAlt).
//                ((norm(x) ⊑ norm(z)) ∧ (norm(y) ⊑ norm(z))) → ((norm(x) ⊔ norm(y)) ⊑ norm(z))

//    law lowerBound(⊓: (IntervalAlt, IntervalAlt) -> IntervalAlt): Bool =
//        let ⊑ = leq;
//            ∀(x: IntervalAlt, y: IntervalAlt).
//                ((norm(x) ⊓ norm(y)) ⊑ norm(x)) ∧ ((norm(x) ⊓ norm(y)) ⊑ norm(y))

//    law greatestLowerBound(⊓: (IntervalAlt, IntervalAlt) -> IntervalAlt): Bool =
//        let ⊑ = leq;
//            ∀(x: IntervalAlt, y: IntervalAlt, z: IntervalAlt).
//                    ((norm(z) ⊑ norm(x)) ∧ (norm(z) ⊑ norm(y))) → (norm(z) ⊑ (norm(x) ⊓ norm(y)))

//    law leastElement(⊑: (IntervalAlt, IntervalAlt) -> Bool, ⊥: IntervalAlt): Bool =
//        ∀(x: IntervalAlt). ⊥ ⊑ norm(x)

//    law greatestElement(⊑: (IntervalAlt, IntervalAlt) -> Bool, ⊤: IntervalAlt): Bool =
//        ∀(x: IntervalAlt). norm(x) ⊑ ⊤

//    law nonNegative(h: IntervalAlt -> BigInt): Bool =
//        ∀(x: IntervalAlt). h(norm(x)) >= 0ii

//    law decreasing(h: IntervalAlt -> BigInt, equ: (IntervalAlt, IntervalAlt) -> Bool, ⊑: (IntervalAlt, IntervalAlt) -> Bool): Bool =
//        ∀(x1: IntervalAlt, y1: IntervalAlt).
//            let x = norm(x1);
//            let y = norm(y1);
//                (x ⊑ y and not equ(x, y)) → (h(x) > h(y))

//    law associative(f: (IntervalAlt, IntervalAlt) -> IntervalAlt): Bool =
//        ∀(x: IntervalAlt, y: IntervalAlt, z: IntervalAlt).
//                f(norm(x), f(norm(y), norm(z))) == f(f(norm(x), norm(y)), norm(z))

//    law commutative(f: (IntervalAlt, IntervalAlt) -> IntervalAlt): Bool =
//        ∀(x: IntervalAlt, y: IntervalAlt).
//            f(norm(x), norm(y)) == f(norm(y), norm(x))

//    law strict1(f: IntervalAlt -> IntervalAlt): Bool =
//        f(IntervalAlt.Bot) == IntervalAlt.Bot

//    law strict2(f: (IntervalAlt, IntervalAlt) -> IntervalAlt): Bool =
//        f(IntervalAlt.Bot, IntervalAlt.Bot) == IntervalAlt.Bot

//    law monotone1(f: IntervalAlt -> IntervalAlt): Bool =
//        let ⊑ = leq;
//            ∀(x: IntervalAlt, y: IntervalAlt).
//                (norm(x) ⊑ norm(y)) → (f(norm(x)) ⊑ f(norm(y)))

//    law monotone2(f: (IntervalAlt, IntervalAlt) -> IntervalAlt): Bool =
//        let ⊑ = leq;
//            ∀(x1: IntervalAlt, x2: IntervalAlt, y1: IntervalAlt, y2: IntervalAlt).
//                    ((norm(x1) ⊑ norm(y1)) ∧ (norm(x2) ⊑ norm(y2))) → (f(norm(x1), norm(x2)) ⊑ f(norm(y1), norm(y2)))

//    law safe1(fa: IntervalAlt -> IntervalAlt, fc: BigInt -> BigInt): Bool =
//        ∀(x: BigInt). alpha(fc(x)) `leq` fa(alpha(x))

//    law safe2(fa: (IntervalAlt, IntervalAlt) -> IntervalAlt, fc: (BigInt, BigInt) -> BigInt): Bool =
//        ∀(x: BigInt, y: BigInt). alpha(fc(x, y)) `leq` fa(alpha(x), alpha(y))

}
