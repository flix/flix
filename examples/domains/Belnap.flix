/**
 * An implementation of the four-valued boolean lattice named for Nuel Belnap.
 */
namespace Belnap {

    ///
    /// The elements of the boolean lattice.
    ///
    enum Belnap {
                  case Top,

        case True,         case False,

                  case Bot
    }

    ///
    /// Returns `true` iff `e1` is less than or equal to `e2`.
    ///
    #reflexive #antiSymmetric #transitive #leastElement(Bot) #greatestElement(Top)
    def leq(e1: Belnap, e2: Belnap): Bool = match (e1, e2) with {
        case (Bot, _)       => true
        case (True, True)   => true
        case (False, False) => true
        case (_, Top)       => true
        case _              => false
    }

    ///
    /// Returns the least upper bound of `e1` and `e2`.
    ///
    #commutative #associative #upperBound #leastUpperBound
    def lub(e1: Belnap, e2: Belnap): Belnap = match (e1, e2) with {
        case (Bot, x)       => x
        case (x, Bot)       => x
        case (True, True)   => True
        case (False, False) => False
        case _              => Top
    }

    ///
    /// Returns the greatest lower bound of `e1` and `e2`.
    ///
    #commutative #associative #lowerBound #greatestLowerBound
    def glb(e1: Belnap, e2: Belnap): Belnap = match (e1, e2) with {
        case (Top, x)       => x
        case (x, Top)       => x
        case (True, True)   => True
        case (False, False) => False
        case _              => Bot
    }

    ///
    /// Abstracts a concrete boolean into the Belnap domain.
    ///
    // TODO: Need to come up with new name.
    def alpha(b: Bool): Belnap = if (b) True else False

    ///
    /// Over-approximates the logical `not` operator.
    ///
    #approximates1(x -> !x)
    #strict1 #monotone1
    def not(e: Belnap): Belnap = match e with {
        case Bot    => Bot
        case True   => False
        case False  => True
        case Top    => Top
    }

    ///
    /// Over-approximates the logical `and` operator.
    ///
    #approximates2((x, y) -> x && y)
    #strict2 #monotone2 #commutative #associative
    def and(e1: Belnap, e2: Belnap): Belnap = match (e1, e2) with {
        case (Bot, _)       => Bot
        case (_, Bot)       => Bot
        case (True, True)   => True
        case (True, False)  => False
        case (False, True)  => False
        case (False, False) => False
        case _              => Top
    }

    ///
    /// Over-approximates the logical `or` operator.
    ///
    #approximates2((x, y) -> x || y)
    #strict2 #monotone2 #commutative #associative
    def or(e1: Belnap, e2: Belnap): Belnap = match (e1, e2) with {
        case (Bot, _)       => Bot
        case (_, Bot)       => Bot
        case (True, True)   => True
        case (True, False)  => True
        case (False, True)  => True
        case (False, False) => False
        case _              => Top
    }

    ///
    /// Over-approximates the logical `xor` operator.
    ///
    #approximates2((x, y) -> x ⊕ y)
    #strict2 #monotone2 #commutative #associative
    def xor(e1: Belnap, e2: Belnap): Belnap = match (e1, e2) with {
        case (Bot, _)       => Bot
        case (_, Bot)       => Bot
        case (True, True)   => False
        case (True, False)  => True
        case (False, True)  => True
        case (False, False) => False
        case _              => Top
    }

    ///
    /// Over-approximates the logical `implies` operator.
    ///
    #approximates2((x, y) -> x → y)
    #strict2 #monotone2
    def implies(e1: Belnap, e2: Belnap): Belnap = match (e1, e2) with {
        case (Bot, _)         => Bot
        case (_, Bot)         => Bot
        case (True, True)     => True
        case (True, False)    => False
        case (False, True)    => True
        case (False, False)   => True
        case _                => Top
    }

    ///
    /// Over-approximates the logical `bicondition` operator.
    ///
    #approximates2((x, y) -> x ↔ y)
    #strict2 #monotone2 #commutative #associative
    def bicondition(e1: Belnap, e2: Belnap): Belnap = match (e1, e2) with {
        case (Bot, _)       => Bot
        case (_, Bot)       => Bot
        case (True, True)   => True
        case (True, False)  => False
        case (False, True)  => False
        case (False, False) => True
        case _              => Top
    }

    ///
    /// Over-approximates the logical `nand` operator.
    ///
    #approximates2((x, y) -> x ↑ y)
    #strict2 #monotone2 #commutative // NB: Commutative, but not associative!
    def nand(e1: Belnap, e2: Belnap): Belnap = match (e1, e2) with {
        case (Bot, _)       => Bot
        case (_, Bot)       => Bot
        case (True, True)   => False
        case (True, False)  => True
        case (False, True)  => True
        case (False, False) => True
        case _              => Top
    }

    let Belnap<> = (Bot, Top, leq, lub, glb)

    // ------------------------------------------------------------------------
    // TODO: Refactor
    // ------------------------------------------------------------------------

    law reflexive(⊑: (Belnap, Belnap) -> Bool): Bool = ∀(x: Belnap). x ⊑ x // TODO: Use Prelude

    law antiSymmetric(⊑: (Belnap, Belnap) -> Bool): Bool = ∀(x: Belnap, y: Belnap). ((x ⊑ y) ∧ (y ⊑ x)) → (x == y) // TODO: Use Prelude

    law transitive(⊑: (Belnap, Belnap) -> Bool): Bool = ∀(x: Belnap, y: Belnap, z: Belnap). ((x ⊑ y) ∧ (y ⊑ z)) → (x ⊑ z) // TODO: Use Prelude

    law upperBound(⊔: (Belnap, Belnap) -> Belnap): Bool = // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: Belnap, y: Belnap). (x ⊑ (x ⊔ y)) ∧ (y ⊑ (x ⊔ y))

    law leastUpperBound(⊔: (Belnap, Belnap) -> Belnap): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: Belnap, y: Belnap, z: Belnap). ((x ⊑ z) ∧ (y ⊑ z)) → ((x ⊔ y) ⊑ z)

    law lowerBound(⊓: (Belnap, Belnap) -> Belnap): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: Belnap, y: Belnap). ((x ⊓ y) ⊑ x) ∧ ((x ⊓ y) ⊑ y)

    law greatestLowerBound(⊓: (Belnap, Belnap) -> Belnap): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: Belnap, y: Belnap, z: Belnap). ((z ⊑ x) ∧ (z ⊑ y)) → (z ⊑ (x ⊓ y))

    law leastElement(⊑: (Belnap, Belnap) -> Bool, ⊥: Belnap): Bool = ∀(x: Belnap). ⊥ ⊑ x  // TODO: Should call prelude

    law greatestElement(⊑: (Belnap, Belnap) -> Bool, ⊤: Belnap): Bool = ∀(x: Belnap). x ⊑ ⊤  // TODO: Should call prelude

    law associative(f: (Belnap, Belnap) -> Belnap): Bool = // TODO: Use Prelude
        ∀(x: Belnap, y: Belnap, z: Belnap). f(x, f(y, z)) == f(f(x, y), z)

    law commutative(f: (Belnap, Belnap) -> Belnap): Bool = // TODO: Use Prelude
        ∀(x: Belnap, y: Belnap). f(x, y) == f(y, x)

    law strict1(f: Belnap -> Belnap): Bool =  // TODO: Should call prelude
        f(Bot) == Bot

    law strict2(f: (Belnap, Belnap) -> Belnap): Bool =  // TODO: Should call prelude
        f(Bot, Bot) == Bot

    law monotone1(f: Belnap -> Belnap): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: Belnap, y: Belnap). (x ⊑ y) → (f(x) ⊑ f(y))

    law monotone2(f: (Belnap, Belnap) -> Belnap): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x1: Belnap, x2: Belnap, y1: Belnap, y2: Belnap). ((x1 ⊑ y1) ∧ (x2 ⊑ y2) ) → (f(x1, x2) ⊑ f(y1, y2))

    // TODO: Names
    law approximates1(fa: Belnap -> Belnap, fc: Bool -> Bool): Bool =
        ∀(x: Bool). alpha(fc(x)) `leq` fa(alpha(x))

    // TODO: Names
    law approximates2(fa: (Belnap, Belnap) -> Belnap, fc: (Bool, Bool) -> Bool): Bool =
        ∀(x: Bool, y: Bool). alpha(fc(x, y)) `leq` fa(alpha(x), alpha(y))

    // TODO: Paper title: Safe and Sound: ...

}
