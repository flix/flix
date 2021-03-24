///
/// An implementation of the IntervalInf domain.
///
/// This implementation uses an explicit bottom and top element.
///
/// See Cousot & Cousot '77 for details.
///
namespace Domain/IntervalInf {

    ///
    /// The elements of the IntervalInf lattice.
    ///
    /// Here Upper(x) represents the interval ]-inf, x] and Lower(x) represent the interval [x, inf[.
    ///
    enum IntervalInf {
                        case Top,

        case Upper(BigInt), case Lower(BigInt),

               case Range(BigInt, BigInt),

                        case Bot
    }

    ///
    /// Returns the canonical representation of the given IntervalInf `e`.
    ///
    def norm(e: IntervalInf): IntervalInf = match e {
        case Bot            => Bot
        case Range(b, e)    => if (b <= e) Range(b, e) else Bot
        case Upper(e)       => Upper(e)
        case Lower(b)       => Lower(b)
        case Top            => Top
    }

    ///
    /// Returns `true` iff `e1` is less than or equal to `e2`.
    ///
//   #reflexive #antiSymmetric #transitive #leastElement(Bot) #greatestElement(Top)
    def leq(e1: IntervalInf, e2: IntervalInf): Bool = match (e1, e2) {
        case (Bot, _)                       => true
        case (Range(b1, e1), Range(b2, e2)) => b2 <= b1 and e1 <= e2
        case (Range(b1, e1), Upper(e2))     => e1 <= e2
        case (Range(b1, e1), Lower(b2))     => b2 <= b1
        case (Upper(e1), Upper(e2))         => e1 <= e2
        case (Lower(b1), Lower(b2))         => b2 <= b1
        case (_, Top)                       => true
        case _                              => false
    }

    ///
    /// Returns the least upper bound of `e1` and `e2`.
    ///
//    #upperBound #leastUpperBound #commutative /* slow #associative */
    def lub(e1: IntervalInf, e2: IntervalInf): IntervalInf = match (e1, e2) {
        case (Bot, x)                       => x
        case (x, Bot)                       => x
        case (Range(b1, e1), Range(b2, e2)) => norm(Range(BigInt.min(b1, b2), BigInt.max(e1, e2)))
        case (Range(b1, e1), Upper(e2))     => norm(Upper(BigInt.max(e1, e2)))
        case (Upper(e1), Range(b2, e2))     => norm(Upper(BigInt.max(e1, e2)))
        case (Range(b1, e1), Lower(b2))     => norm(Lower(BigInt.min(b1, b2)))
        case (Lower(b1), Range(b2, e2))     => norm(Lower(BigInt.min(b1, b2)))
        case (Upper(e1), Upper(e2))         => norm(Upper(BigInt.max(e1, e2)))
        case (Lower(e1), Lower(e2))         => norm(Lower(BigInt.min(e1, e2)))
        case _                              => Top
    }

    ///
    /// Returns the greatest lower bound of `e1` and `e2`.
    ///
//    #lowerBound #greatestLowerBound #commutative /* slow #associative */
    def glb(e1: IntervalInf, e2: IntervalInf): IntervalInf = match (e1, e2) {
        case (Top, x)                       => x
        case (x, Top)                       => x
        case (Range(b1, e1), Range(b2, e2)) => norm(Range(BigInt.max(b1, b2), BigInt.min(e1, e2)))
        case (Upper(e1), Range(b2, e2))     => norm(Range(b2, BigInt.min(e1, e2)))
        case (Range(b1, e1), Upper(e2))     => norm(Range(b1, BigInt.min(e1, e2)))
        case (Lower(b1), Range(b2, e2))     => norm(Range(BigInt.max(b1, b2), e2))
        case (Range(b1, e1), Lower(b2))     => norm(Range(BigInt.max(b1, b2), e1))
        case (Upper(e1), Upper(e2))         => norm(Upper(BigInt.min(e1, e2)))
        case (Lower(b1), Lower(b2))         => norm(Lower(BigInt.max(b1, b2)))
        case (Lower(b), Upper(e))           => if (b <= e) norm(Range(b, e)) else Bot
        case (Upper(e), Lower(b))           => if (b <= e) norm(Range(b, e)) else Bot
        case _                              => Bot
    }

    ///
    /// Abstracts a concrete integer into the IntervalInf domain.
    ///
    def alpha(i: BigInt): IntervalInf = Range(i, i)

    ///
    /// Over-approximates integer `increment`.
    ///
//    #safe1(x -> x + 1ii)
//    #strict1 #monotone1
    def inc(e: IntervalInf): IntervalInf = match e {
        case Bot            => Bot
        case Range(b, e)    => norm(Range(b + 1ii, e + 1ii))
        case Upper(e)       => norm(Upper(e + 1ii))
        case Lower(b)       => norm(Lower(b + 1ii))
        case Top            => Top
    }

    ///
    /// Over-approximates integer `decrement`.
    ///
//    #safe1(x -> x - 1ii)
//    #strict1 #monotone1
    def dec(e: IntervalInf): IntervalInf = match e {
        case Bot            => Bot
        case Range(b, e)    => norm(Range(b - 1ii, e - 1ii))
        case Upper(e)       => norm(Upper(e - 1ii))
        case Lower(b)       => norm(Lower(b - 1ii))
        case Top            => Top
    }

    ///
    /// Over-approximates integer `addition`.
    ///
//    #safe2((x, y) -> x + y)
//    #strict2 #monotone2 #commutative
    def plus(e1: IntervalInf, e2: IntervalInf): IntervalInf = match (e1, e2) {
        case (Bot, _)                       => Bot
        case (_, Bot)                       => Bot
        case (Range(b1, e1), Range(b2, e2)) => norm(Range(b1 + b2, e1 + e2))
        case (Range(b1, e1), Upper(e2))     => norm(Upper(e1 + e2))
        case (Upper(e2), Range(b1, e1))     => norm(Upper(e1 + e2))
        case (Range(b1, e2), Lower(b2))     => norm(Lower(b1 + b2))
        case (Lower(b2), Range(b1, e2))     => norm(Lower(b1 + b2))
        case (Upper(e1), Upper(e2))         => norm(Upper(e1 + e2))
        case (Lower(b1), Lower(b2))         => norm(Lower(b1 + b2))
        case _                              => Top
    }

//    ///
//    /// Over-approximates integer `subtraction`.
//    ///
//    #safe2((x, y) -> x - y)
//    #strict2 #monotone2
//    def minus(e1: IntervalInf, e2: IntervalInf): IntervalInf = match (e1, e2) {
//        case (Bot, _)                       => Bot
//        case (_, Bot)                       => Bot
//        case (Range(b1, e1), Range(b2, e2)) => norm(Range(b1 - e2, e1 - b2))
//        case _                              => Top
//    }

    ///
    /// Over-approximates integer `multiplication`.
    ///
//    #safe2((x, y) -> x * y)
//    #strict2  #monotone2 /**/ /* slow? #commutative */
    def times(e1: IntervalInf, e2: IntervalInf): IntervalInf = match (e1, e2) {
        case (Bot, _)                       => Bot
        case (_, Bot)                       => Bot
        case (Range(b1, e1), Range(b2, e2)) =>
            let b = BigInt.min(BigInt.min(b1 * b2, b1 * e2), BigInt.min(e1 * b2, e1 * e2));
            let e = BigInt.max(BigInt.max(b1 * b2, b1 * e2), BigInt.max(e1 * b2, e1 * e2));
                norm(Range(b, e))
        case (Range(b1, e1), Upper(e2))     => norm(Upper(BigInt.max(b1 * e2, e1 * e2)))
        case (Upper(e1), Range(b2, e2))     => norm(Upper(BigInt.max(e1 * b2, e1 * e2)))
        case (Range(b1, e1), Lower(b2))     => norm(Lower(BigInt.min(b1 * b2, e1 * b2)))
        case (Lower(b1), Range(b2, e2))     => norm(Lower(BigInt.min(b1 * b2, b1 * e2)))
        case _                              => Top
    }

    /**
      * Over-approximates `equal`.
      */
//    #safeBelnap2((x, y) -> x == y)
//    #Bounded.strict2(Bot, Bot, Belnap/Belnap.Bot) #monotoneBelnap2 #commutativeBelnap
    def eq(e1: IntervalInf, e2: IntervalInf): Belnap.Belnap = match (e1, e2) {
        case (Bot, _)                                           => Belnap/Belnap.Bot
        case (_, Bot)                                           => Belnap/Belnap.Bot
        case (Range(b1, e1), Range(b2, e2)) => {
            if      ((b1 == e1) and ((e1 == b2) and (b2 == e2)))     Belnap/Belnap.True
            else if ((e1 < b2) or (e2 < b1))                       Belnap/Belnap.False
            else                                                   Belnap/Belnap.Top
        }
        case (Range(b1, e1), Upper(e2))                         => if (b1 > e2) Belnap/Belnap.False else Belnap/Belnap.Top
        case (Upper(e1), Range(b2, e2))                         => if (b2 > e1) Belnap/Belnap.False else Belnap/Belnap.Top
        case (Range(b1, e1), Lower(b2))                         => if (e1 < b2) Belnap/Belnap.False else Belnap/Belnap.Top
        case (Lower(b1), Range(b2, e2))                         => if (e2 < b1) Belnap/Belnap.False else Belnap/Belnap.Top
        case (Upper(e1), Lower(b2))                             => if (e1 < b2) Belnap/Belnap.False else Belnap/Belnap.Top
        case (Lower(b1), Upper(e2))                             => if (e2 < b1) Belnap/Belnap.False else Belnap/Belnap.Top
        case _                                                  => Belnap/Belnap.Top
    }

    /**
      * Over-approximates `not equal`.
      */
//    #safeBelnap2((x, y) -> x != y)
//    #Bounded.strict2(Bot, Bot, Belnap/Belnap.Bot) #monotoneBelnap2 #commutativeBelnap
    def neq(e1: IntervalInf, e2: IntervalInf): Belnap.Belnap = Belnap.not(eq(e1, e2))

    /**
      * Over-approximates `less than`.
      */
//   #safeBelnap2((x, y) -> x < y)
//   #Bounded.strict2(Bot, Bot, Belnap/Belnap.Bot) #monotoneBelnap2
    def less(e1: IntervalInf, e2: IntervalInf): Belnap.Belnap = match (e1, e2) {
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
        case (Range(b1, e1), Upper(e2))                         => if (e2 < b1) Belnap/Belnap.False else Belnap/Belnap.Top
        case (Upper(e1), Range(b2, e2))                         => if (e1 < b2) Belnap/Belnap.True else Belnap/Belnap.Top
        case (Range(b1, e1), Lower(b2))                         => if (e1 < b2) Belnap/Belnap.True else Belnap/Belnap.Top
        case (Lower(b1), Range(b2, e2))                         => if (e2 < b1) Belnap/Belnap.False else Belnap/Belnap.Top
        case (Upper(e1), Lower(b2))                             => if (e1 < b2) Belnap/Belnap.True else Belnap/Belnap.Top
        case _                                                  => Belnap/Belnap.Top
    }

    /**
      * Over-approximates `less than or equal`.
      */
//    #safeBelnap2((x, y) -> x <= y)
    //#Bounded.strict2(Bot, Bot, Belnap/Belnap.Bot) #monotoneBelnap2 // TODO
    def lessEqual(e1: IntervalInf, e2: IntervalInf): Belnap.Belnap = Belnap.or(e1 `less` e2, e1 `eq` e2)


    // ------------------------------------------------------------------------
    // Specialized Laws
    // ------------------------------------------------------------------------

//    law reflexive(⊑: (IntervalInf, IntervalInf) -> Bool): Bool =
//        ∀(x: IntervalInf). norm(x) ⊑ norm(x)

//    law antiSymmetric(⊑: (IntervalInf, IntervalInf) -> Bool): Bool =
//        ∀(x: IntervalInf, y: IntervalInf). ((norm(x) ⊑ norm(y)) ∧ (norm(y) ⊑ norm(x))) → (norm(x) == norm(y))

//    law transitive(⊑: (IntervalInf, IntervalInf) -> Bool): Bool =
//        ∀(x: IntervalInf, y: IntervalInf, z: IntervalInf). ((norm(x) ⊑ norm(y)) ∧ (norm(y) ⊑ norm(z))) → (norm(x) ⊑ norm(z))

//    law upperBound(⊔: (IntervalInf, IntervalInf) -> IntervalInf): Bool =
//        let ⊑ = leq;
//            ∀(x: IntervalInf, y: IntervalInf).
//                (norm(x) ⊑ (norm(x) ⊔ norm(y))) ∧ (norm(y) ⊑ (norm(x) ⊔ norm(y)))

//    law leastUpperBound(⊔: (IntervalInf, IntervalInf) -> IntervalInf): Bool =
//        let ⊑ = leq;
//            ∀(x: IntervalInf, y: IntervalInf, z: IntervalInf).
//                ((norm(x) ⊑ norm(z)) ∧ (norm(y) ⊑ norm(z))) → ((norm(x) ⊔ norm(y)) ⊑ norm(z))

//    law lowerBound(⊓: (IntervalInf, IntervalInf) -> IntervalInf): Bool =
//        let ⊑ = leq;
//            ∀(x: IntervalInf, y: IntervalInf).
//                ((norm(x) ⊓ norm(y)) ⊑ norm(x)) ∧ ((norm(x) ⊓ norm(y)) ⊑ norm(y))

//    law greatestLowerBound(⊓: (IntervalInf, IntervalInf) -> IntervalInf): Bool =
//        let ⊑ = leq;
//            ∀(x: IntervalInf, y: IntervalInf, z: IntervalInf).
//                    ((norm(z) ⊑ norm(x)) ∧ (norm(z) ⊑ norm(y))) → (norm(z) ⊑ (norm(x) ⊓ norm(y)))

//    law leastElement(⊑: (IntervalInf, IntervalInf) -> Bool, ⊥: IntervalInf): Bool =
//        ∀(x: IntervalInf). ⊥ ⊑ norm(x)

//    law greatestElement(⊑: (IntervalInf, IntervalInf) -> Bool, ⊤: IntervalInf): Bool =
//        ∀(x: IntervalInf). norm(x) ⊑ ⊤

//    law associative(f: (IntervalInf, IntervalInf) -> IntervalInf): Bool =
//        ∀(x: IntervalInf, y: IntervalInf, z: IntervalInf).
//                f(norm(x), f(norm(y), norm(z))) == f(f(norm(x), norm(y)), norm(z))

//    law commutative(f: (IntervalInf, IntervalInf) -> IntervalInf): Bool =
//        ∀(x: IntervalInf, y: IntervalInf).
//            f(norm(x), norm(y)) == f(norm(y), norm(x))

//    law strict1(f: IntervalInf -> IntervalInf): Bool =
//        f(Bot) == Bot

//    law strict2(f: (IntervalInf, IntervalInf) -> IntervalInf): Bool =
//        f(Bot, Bot) == Bot

//    law monotone1(f: IntervalInf -> IntervalInf): Bool =
//        let ⊑ = leq;
//            ∀(x: IntervalInf, y: IntervalInf).
//                (norm(x) ⊑ norm(y)) → (f(norm(x)) ⊑ f(norm(y)))

//    law monotone2(f: (IntervalInf, IntervalInf) -> IntervalInf): Bool =
//        let ⊑ = leq;
//            ∀(x1: IntervalInf, x2: IntervalInf, y1: IntervalInf, y2: IntervalInf).
//                    ((norm(x1) ⊑ norm(y1)) ∧ (norm(x2) ⊑ norm(y2))) → (f(norm(x1), norm(x2)) ⊑ f(norm(y1), norm(y2)))

//    law safe1(fa: IntervalInf -> IntervalInf, fc: BigInt -> BigInt): Bool =
//        ∀(x: BigInt). alpha(fc(x)) `leq` fa(alpha(x))

//    law safe2(fa: (IntervalInf, IntervalInf) -> IntervalInf, fc: (BigInt, BigInt) -> BigInt): Bool =
//        ∀(x: BigInt, y: BigInt). alpha(fc(x, y)) `leq` fa(alpha(x), alpha(y))

    // ------------------------------------------------------------------------
    // Specialized Laws for IntervalInf/Belnap.
    // ------------------------------------------------------------------------

//    law commutativeBelnap(f: (IntervalInf, IntervalInf) -> Belnap.Belnap): Bool =
//        ∀(x: IntervalInf, y: IntervalInf).
//            f(norm(x), norm(y)) == f(norm(y), norm(x))

//    law monotoneBelnap2(f: (IntervalInf, IntervalInf) -> Belnap.Belnap): Bool =
//        let ⊑ = leq;
//            ∀(x1: IntervalInf, x2: IntervalInf, y1: IntervalInf, y2: IntervalInf).
//                    ((norm(x1) ⊑ norm(y1)) ∧ (norm(x2) ⊑ norm(y2))) → (f(norm(x1), norm(x2)) `Belnap.leq` f(norm(y1), norm(y2)))

//    law safeBelnap2(fa: (IntervalInf, IntervalInf) -> Belnap.Belnap, fc: (BigInt, BigInt) -> Bool): Bool =
//        ∀(x: BigInt, y: BigInt). Belnap.alpha(fc(x, y)) `Belnap.leq` fa(alpha(x), alpha(y))

}
