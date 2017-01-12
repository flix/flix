/**
 * An implementation of Prefix-Suffix domain.
 */
namespace Domain.PrefixSuffix {

    ///
    /// The elements of the prefix-suffix lattice.
    ///
    enum PrefixSuffix {
                   case Top,

        case Pre(Int), case Suf(Int),

              case PreSuf(Int, Int),

                   case Bot
    }

    ///
    /// Returns `true` iff `e1` is less than or equal to `e2`.
    ///
    #reflexive #antiSymmetric #transitive #leastElement(Bot) #greatestElement(Top)
    def leq(e1: PrefixSuffix, e2: PrefixSuffix): Bool = match (e1, e2) with {
        case (Bot, _)                           => true
        case (PreSuf(p1, s1), PreSuf(p2, s2))   => p1 == p2 && s1 == s2
        case (PreSuf(p1, _), Pre(p2))           => p1 == p2
        case (PreSuf(_, s1), Suf(s2))           => s1 == s2
        case (Pre(p1), Pre(p2))                 => p1 == p2
        case (Suf(s1), Suf(s2))                 => s1 == s2
        case (_, Top)                           => true
        case _                                  => false
    }

    ///
    /// Returns the least upper bound of `e1` and `e2`.
    ///
    #commutative /* #associative slow */ #upperBound #leastUpperBound
    def lub(e1: PrefixSuffix, e2: PrefixSuffix): PrefixSuffix = match (e1, e2) with {
        case (Bot, x)                           => x
        case (x, Bot)                           => x
        case (PreSuf(p1, s1), PreSuf(p2, s2))   => switch {
            case (p1 == p2) && (s1 == s2)       => PreSuf(p1, s1)
            case (p1 == p2) && (s1 != s2)       => Pre(p1)
            case (p1 != p2) && (s1 == s2)       => Suf(s1)
            case true                           => Top
        }
        case (PreSuf(p1, _), Pre(p2))           => if (p1 == p2) Pre(p1) else Top
        case (Pre(p1), PreSuf(p2, _))           => if (p1 == p2) Pre(p1) else Top
        case (PreSuf(_, s1), Suf(s2))           => if (s1 == s2) Suf(s1) else Top
        case (Suf(s1), PreSuf(_, s2))           => if (s1 == s2) Suf(s1) else Top
        case (Pre(p1), Pre(p2))                 => if (p1 == p2) Pre(p1) else Top
        case (Suf(s1), Suf(s2))                 => if (s1 == s2) Suf(s1) else Top
        case _                                  => Top
    }

    ///
    /// Returns the greatest lower bound of `e1` and `e2`.
    ///
    #commutative #associative #lowerBound #greatestLowerBound
    def glb(e1: PrefixSuffix, e2: PrefixSuffix): PrefixSuffix = match (e1, e2) with {
        case (Top, x)                           => x
        case (x, Top)                           => x
        case (PreSuf(p1, s1), PreSuf(p2, s2))   => if (p1 == p2 && s1 == s2) PreSuf(p1, s1) else Bot
        case (PreSuf(p1, s1), Pre(p2))          => if (p1 == p2) PreSuf(p1, s1) else Bot
        case (Pre(p1), PreSuf(p2, s2))          => if (p1 == p2) PreSuf(p2, s2) else Bot
        case (PreSuf(p1, s1), Suf(s2))          => if (s1 == s2) PreSuf(p1, s1) else Bot
        case (Suf(s1), PreSuf(p2, s2))          => if (s1 == s2) PreSuf(p2, s2) else Bot
        case (Pre(p1), Pre(p2))                 => if (p1 == p2) Pre(p1) else Bot
        case (Suf(s1), Suf(s2))                 => if (s1 == s2) Suf(s1) else Bot
        case (Pre(p), Suf(s))                   => PreSuf(p, s)
        case (Suf(s), Pre(p))                   => PreSuf(p, s)
        case _                                  => Bot
    }

    ///
    /// Over-approximates `concatenate`.
    ///
    #strict2 #monotone2 #associative
    def concatenate(e1: PrefixSuffix, e2: PrefixSuffix): PrefixSuffix = match (e1, e2) with {
        case (Bot, _)                       => Bot
        case (_, Bot)                       => Bot
        case (PreSuf(p1, _), PreSuf(_, s2)) => PreSuf(p1, s2)
        case (PreSuf(p1, _), Suf(s2))       => PreSuf(p1, s2)
        case (Pre(p1), PreSuf(_, s2))       => PreSuf(p1, s2)
        case (Pre(p1), Suf(s2))             => PreSuf(p1, s2)
        case (PreSuf(p1, _), Pre(_))        => Pre(p1)
        case (Pre(p1), Pre(_))              => Pre(p1)
        case (Suf(_), PreSuf(_, s2))        => Suf(s2)
        case (Suf(_), Suf(s2))              => Suf(s2)
        case (Suf(_), Pre(_))               => Top
        case (Pre(p1), Top)                 => Pre(p1)
        case (PreSuf(p1, _), Top)           => Pre(p1)
        case (Top, Suf(s2))                 => Suf(s2)
        case (Top, PreSuf(_, s2))           => Suf(s2)
        case _                              => Top
    }

    ///
    /// Over-approximates `equal`.
    ///
    // TODO @strict2 @monotone @commutative
    def eq(e1: PrefixSuffix, e2: PrefixSuffix): Belnap/Belnap = match (e1, e2) with {
        case (Bot, _)            => Belnap/Belnap.Bot
        case (_, Bot)            => Belnap/Belnap.Bot
        case (PreSuf(p1, s1), PreSuf(p2, s2)) => switch {
            case (p1 != p2) => Belnap/Belnap.False
            case (s1 != s2) => Belnap/Belnap.False
            case true       => Belnap/Belnap.Top
        }
        case (Pre(p1), Pre(p2))           => if (p1 != p2) Belnap/Belnap.False else Belnap/Belnap.Top
        case (Suf(s1), Suf(s2))           => if (s1 != s2) Belnap/Belnap.False else Belnap/Belnap.Top
        case (Pre(p1), PreSuf(p2, _))     => if (p1 != p2) Belnap/Belnap.False else Belnap/Belnap.Top
        case (PreSuf(p1, _), Pre(p2))     => if (p1 != p2) Belnap/Belnap.False else Belnap/Belnap.Top
        case (Suf(s1), PreSuf(_, s2))     => if (s1 != s2) Belnap/Belnap.False else Belnap/Belnap.Top
        case (PreSuf(_, s1), Suf(s2))     => if (s1 != s2) Belnap/Belnap.False else Belnap/Belnap.Top
        case _                                  => Belnap/Belnap.Top
    }

    ///
    /// Over-approximates `not equal`.
    ///
    // TODO: @strict @monotone @commutative
    def neq(e1: PrefixSuffix, e2: PrefixSuffix): Belnap/Belnap = Belnap/not(eq(e1, e2))

    // TODO: Less equal?

    let PrefixSuffix<> = (Bot, Top, leq, lub, glb)


    // ------------------------------------------------------------------------
    // TODO: Refactor
    // ------------------------------------------------------------------------

    law reflexive(⊑: (PrefixSuffix, PrefixSuffix) -> Bool): Bool = ∀(x: PrefixSuffix). x ⊑ x // TODO: Use Prelude

    law antiSymmetric(⊑: (PrefixSuffix, PrefixSuffix) -> Bool): Bool = ∀(x: PrefixSuffix, y: PrefixSuffix). ((x ⊑ y) ∧ (y ⊑ x)) → (x == y) // TODO: Use Prelude

    law transitive(⊑: (PrefixSuffix, PrefixSuffix) -> Bool): Bool = ∀(x: PrefixSuffix, y: PrefixSuffix, z: PrefixSuffix). ((x ⊑ y) ∧ (y ⊑ z)) → (x ⊑ z) // TODO: Use Prelude

    law upperBound(⊔: (PrefixSuffix, PrefixSuffix) -> PrefixSuffix): Bool = // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: PrefixSuffix, y: PrefixSuffix). (x ⊑ (x ⊔ y)) ∧ (y ⊑ (x ⊔ y))

    law leastUpperBound(⊔: (PrefixSuffix, PrefixSuffix) -> PrefixSuffix): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: PrefixSuffix, y: PrefixSuffix, z: PrefixSuffix). ((x ⊑ z) ∧ (y ⊑ z)) → ((x ⊔ y) ⊑ z)

    law lowerBound(⊓: (PrefixSuffix, PrefixSuffix) -> PrefixSuffix): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: PrefixSuffix, y: PrefixSuffix). ((x ⊓ y) ⊑ x) ∧ ((x ⊓ y) ⊑ y)

    law greatestLowerBound(⊓: (PrefixSuffix, PrefixSuffix) -> PrefixSuffix): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: PrefixSuffix, y: PrefixSuffix, z: PrefixSuffix). ((z ⊑ x) ∧ (z ⊑ y)) → (z ⊑ (x ⊓ y))

    law leastElement(⊑: (PrefixSuffix, PrefixSuffix) -> Bool, ⊥: PrefixSuffix): Bool = ∀(x: PrefixSuffix). ⊥ ⊑ x  // TODO: Should call prelude

    law greatestElement(⊑: (PrefixSuffix, PrefixSuffix) -> Bool, ⊤: PrefixSuffix): Bool = ∀(x: PrefixSuffix). x ⊑ ⊤  // TODO: Should call prelude

    law associative(f: (PrefixSuffix, PrefixSuffix) -> PrefixSuffix): Bool = // TODO: Use Prelude
        ∀(x: PrefixSuffix, y: PrefixSuffix, z: PrefixSuffix). f(x, f(y, z)) == f(f(x, y), z)

    law commutative(f: (PrefixSuffix, PrefixSuffix) -> PrefixSuffix): Bool = // TODO: Use Prelude
        ∀(x: PrefixSuffix, y: PrefixSuffix). f(x, y) == f(y, x)

    law strict1(f: PrefixSuffix -> PrefixSuffix): Bool =  // TODO: Should call prelude
        f(Bot) == Bot

    law strict2(f: (PrefixSuffix, PrefixSuffix) -> PrefixSuffix): Bool =  // TODO: Should call prelude
        f(Bot, Bot) == Bot

    law monotone1(f: PrefixSuffix -> PrefixSuffix): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: PrefixSuffix, y: PrefixSuffix). (x ⊑ y) → (f(x) ⊑ f(y))

    law monotone2(f: (PrefixSuffix, PrefixSuffix) -> PrefixSuffix): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x1: PrefixSuffix, x2: PrefixSuffix, y1: PrefixSuffix, y2: PrefixSuffix). ((x1 ⊑ y1) ∧ (x2 ⊑ y2) ) → (f(x1, x2) ⊑ f(y1, y2))

}