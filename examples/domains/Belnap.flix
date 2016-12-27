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
    #reflexive #antiSymmetric #transitive
    def leq(e1: Belnap, e2: Belnap): Bool = match (e1, e2) with {
        case (Belnap.Bot, _)                => true
        case (Belnap.True, Belnap.True)     => true
        case (Belnap.False, Belnap.False)   => true
        case (_, Belnap.Top)                => true
        case _                              => false
    }

    ///
    /// Returns the least upper bound of `e1` and `e2`.
    ///
    #commutative #associative
    def lub(e1: Belnap, e2: Belnap): Belnap = match (e1, e2) with {
        case (Belnap.Bot, x)                => x
        case (x, Belnap.Bot)                => x
        case (Belnap.True, Belnap.True)     => Belnap.True
        case (Belnap.False, Belnap.False)   => Belnap.False
        case _                              => Belnap.Top
    }

    ///
    /// Returns the greatest lower bound of `e1` and `e2`.
    ///
    #commutative #associative
    def glb(e1: Belnap, e2: Belnap): Belnap = match (e1, e2) with {
        case (Belnap.Top, x)                => x
        case (x, Belnap.Top)                => x
        case (Belnap.True, Belnap.True)     => Belnap.True
        case (Belnap.False, Belnap.False)   => Belnap.False
        case _                              => Belnap.Bot
    }

    ///
    /// Abstracts a concrete boolean into the Belnap domain.
    ///
    def alpha(b: Bool): Belnap = if (b) Belnap.True else Belnap.False

    ///
    /// Over-approximates the logical `not` operator.
    ///
    #sound1(x -> !x)
    #strict1 #monotone1
    def not(e: Belnap): Belnap = match e with {
        case Belnap.Bot     => Belnap.Bot
        case Belnap.True    => Belnap.False
        case Belnap.False   => Belnap.True
        case Belnap.Top     => Belnap.Top
    }

    ///
    /// Over-approximates the logical `and` operator.
    ///
    #sound2((x, y) -> x && y)
    #strict2 #monotone2 #commutative #associative
    def and(e1: Belnap, e2: Belnap): Belnap = match (e1, e2) with {
        case (Belnap.Bot, _)                => Belnap.Bot
        case (_, Belnap.Bot)                => Belnap.Bot
        case (Belnap.True, Belnap.True)     => Belnap.True
        case (Belnap.True, Belnap.False)    => Belnap.False
        case (Belnap.False, Belnap.True)    => Belnap.False
        case (Belnap.False, Belnap.False)   => Belnap.False
        case _                              => Belnap.Top
    }

    ///
    /// Over-approximates the logical `or` operator.
    ///
    #sound2((x, y) -> x || y)
    #strict2 #monotone2 #commutative #associative
    def or(e1: Belnap, e2: Belnap): Belnap = match (e1, e2) with {
        case (Belnap.Bot, _)                => Belnap.Bot
        case (_, Belnap.Bot)                => Belnap.Bot
        case (Belnap.True, Belnap.True)     => Belnap.True
        case (Belnap.True, Belnap.False)    => Belnap.True
        case (Belnap.False, Belnap.True)    => Belnap.True
        case (Belnap.False, Belnap.False)   => Belnap.False
        case _                              => Belnap.Top
    }

    ///
    /// Over-approximates the logical `xor` operator.
    ///
    #sound2((x, y) -> (x || y) && !(x && y))
    #strict2 #monotone2 #commutative #associative
    def xor(e1: Belnap, e2: Belnap): Belnap = match (e1, e2) with {
        case (Belnap.Bot, _)                => Belnap.Bot
        case (_, Belnap.Bot)                => Belnap.Bot
        case (Belnap.True, Belnap.True)     => Belnap.False
        case (Belnap.True, Belnap.False)    => Belnap.True
        case (Belnap.False, Belnap.True)    => Belnap.True
        case (Belnap.False, Belnap.False)   => Belnap.False
        case _                              => Belnap.Top
    }

    ///
    /// Over-approximates the logical `implies` operator.
    ///
    #sound2((x, y) -> !x || y)
    #strict2 #monotone2
    def implies(e1: Belnap, e2: Belnap): Belnap = match (e1, e2) with {
        case (Belnap.Bot, _)                => Belnap.Bot
        case (_, Belnap.Bot)                => Belnap.Bot
        case (Belnap.True, Belnap.True)     => Belnap.True
        case (Belnap.True, Belnap.False)    => Belnap.False
        case (Belnap.False, Belnap.True)    => Belnap.True
        case (Belnap.False, Belnap.False)   => Belnap.True
        case _                              => Belnap.Top
    }

    ///
    /// Over-approximates the logical `bicondition` operator.
    ///
    #strict2 #monotone2 #commutative #associative
    def bicondition(e1: Belnap, e2: Belnap): Belnap = match (e1, e2) with {
        case (Belnap.Bot, _)                => Belnap.Bot
        case (_, Belnap.Bot)                => Belnap.Bot
        case (Belnap.True, Belnap.True)     => Belnap.True
        case (Belnap.True, Belnap.False)    => Belnap.False
        case (Belnap.False, Belnap.True)    => Belnap.False
        case (Belnap.False, Belnap.False)   => Belnap.True
        case _                              => Belnap.Top
    }

    ///
    /// Over-approximates the logical `nand` operator.
    ///
    #strict2 #monotone2 #commutative // NB: Commutative, but not associative!
    def nand(e1: Belnap, e2: Belnap): Belnap = match (e1, e2) with {
        case (Belnap.Bot, _)                => Belnap.Bot
        case (_, Belnap.Bot)                => Belnap.Bot
        case (Belnap.True, Belnap.True)     => Belnap.False
        case (Belnap.True, Belnap.False)    => Belnap.True
        case (Belnap.False, Belnap.True)    => Belnap.True
        case (Belnap.False, Belnap.False)   => Belnap.True
        case _                              => Belnap.Top
    }

    let Belnap<> = (Belnap.Bot, Belnap.Top, leq, lub, glb)



    // TODO -------------- refactor below this line:

    law reflexive(⊑: (Belnap, Belnap) -> Bool): Bool = ∀(x: Belnap). x ⊑ x

    law antiSymmetric(⊑: (Belnap, Belnap) -> Bool): Bool = ∀(x: Belnap, y: Belnap). ((x ⊑ y) ∧ (y ⊑ x)) → (x == y)

    law transitive(⊑: (Belnap, Belnap) -> Bool): Bool = ∀(x: Belnap, y: Belnap, z: Belnap). ((x ⊑ y) ∧ (y ⊑ z)) → (x ⊑ z)





    law associative(f: (Belnap, Belnap) -> Belnap): Bool =
        true // TODO

    law commutative(f: (Belnap, Belnap) -> Belnap): Bool =
        ∀(x: Belnap, y: Belnap). f(x, y) == f(y, x)

    law strict1(f: Belnap -> Belnap): Bool =
        f(Bot) == Bot

    law strict2(f: (Belnap, Belnap) -> Belnap): Bool =
        f(Bot, Bot) == Bot

    law monotone1(f: Belnap -> Belnap): Bool =
        true // TODO

    law monotone2(f: (Belnap, Belnap) -> Belnap): Bool =
        true // TODO

    law sound1(fa: Belnap -> Belnap, fc: Bool -> Bool): Bool =
        ∀(x: Bool). alpha(fc(x)) `leq` fa(alpha(x))

    law sound2(fa: (Belnap, Belnap) -> Belnap, fc: (Bool, Bool) -> Bool): Bool =
        ∀(x: Bool, y: Bool). alpha(fc(x, y)) `leq` fa(alpha(x), alpha(y))

}
