/**
 * An implementation of the modulo three abstract domain.
 *
 * Note: The Parity domain (e.g. even and odd) can be viewed as the Mod2 domain.
 */
namespace Domain.Mod3 {

    ///
    /// The elements of the modulo three lattice.
    ///
    enum Mod3 {
                   case Top,

        case Zer,  case One,  case Two,

                   case Bot
    }

    ///
    /// Returns `true` iff `e1` is less than or equal to `e2`.
    ///
    #reflexive #antiSymmetric #transitive #leastElement(Bot) #greatestElement(Top)
    def leq(e1: Mod3, e2: Mod3): Bool = match (e1, e2) with {
        case (Bot, _)   => true
        case (Zer, Zer) => true
        case (One, One) => true
        case (Two, Two) => true
        case (_, Top)   => true
        case _          => false
    }

    ///
    /// Returns the least upper bound of `e1` and `e2`.
    ///
    #commutative #associative #upperBound #leastUpperBound
    def lub(e1: Mod3, e2: Mod3): Mod3 = match (e1, e2) with {
        case (Bot, x)   => x
        case (x, Bot)   => x
        case (Zer, Zer) => Zer
        case (One, One) => One
        case (Two, Two) => Two
        case _          => Top
    }

    ///
    /// Returns the greatest lower bound of `e1` and `e2`.
    ///
    #commutative #associative #lowerBound #greatestLowerBound
    def glb(e1: Mod3, e2: Mod3): Mod3 = match (e1, e2) with {
        case (Top, x)   => x
        case (x, Top)   => x
        case (Zer, Zer) => Zer
        case (One, One) => One
        case (Two, Two) => Two
        case _          => Bot
    }

    ///
    /// Abstracts a concrete integer into the Parity domain.
    ///
    def alpha(i: Int): Mod3 = switch {
        case i % 3 == 0 => Zer
        case i % 3 == 1 => One
        case i % 3 == 2 => Two
        case true => Bot // TODO: there is some bug here...
    }

    ///
    /// Over-approximates integer `increment`.
    ///
    #approximates1(x -> x + 1)
    #strict1 #monotone1
    def inc(e: Mod3): Mod3 = match e with {
       case Bot => Bot
       case Zer => One
       case One => Two
       case Two => Zer
       case Top => Top
    }

    ///
    /// Over-approximates integer `decrement`.
    ///
    #approximates1(x -> x - 1)
    #strict1 #monotone1
    def dec(e: Mod3): Mod3 = match e with {
       case Bot => Bot
       case Zer => Two
       case One => Zer
       case Two => One
       case Top => Top
    }

    ///
    /// Over-approximates integer `addition`.
    ///
    //#approximates2((x, y) -> x + y) // TODO
    #strict2 #monotone2 #commutative #associative
    def plus(e1: Mod3, e2: Mod3): Mod3 = match (e1, e2) with {
       case (_, Bot)   => Bot
       case (Bot, _)   => Bot
       case (Zer, x)   => x
       case (x, Zer)   => x
       case (One, One) => Two
       case (One, Two) => Zer
       case (Two, One) => Zer
       case (Two, Two) => One
       case _          => Top
    }

    // TODO: Add approximate for the rest.

    ///
    /// Over-approximates integer `subtraction`.
    ///
    #strict2 #monotone2
    def minus(e1: Mod3, e2: Mod3): Mod3 = match (e1, e2) with {
       case (_, Bot)   => Bot
       case (Bot, _)   => Bot
       case (Zer, x)   => x
       case (x, Zer)   => x
       case (One, One) => Zer
       case (One, Two) => One
       case (Two, One) => One
       case (Two, Two) => Zer
       case _          => Top
    }

    ///
    /// Over-approximates integer `multiplication`.
    ///
    #strict2 #monotone2 #commutative #associative
    def times(e1: Mod3, e2: Mod3): Mod3 = match (e1, e2) with {
        case (_, Bot) => Bot
        case (Bot, _) => Bot
        // TODO: rest
        case _        => Top
    }

    ///
    /// Over-approximates integer `division`.
    ///
    #strict2 #monotone2
    def divide(e1: Mod3, e2: Mod3): Mod3 = match (e1, e2) with {
        case (_, Bot) => Bot
        case (Bot, _) => Bot
        // TODO: rest
        case _        => Top
    }

    ///
    /// Over-approximates integer `modulus`.
    ///
    #strict2 #monotone2
    def modulo(e1: Mod3, e2: Mod3): Mod3 = match (e1, e2) with {
        case (_, Bot) => Bot
        case (Bot, _) => Bot
        // TODO: rest
        case _        => Top
    }

    ///
    /// Over-approximates `equal`.
    ///
    // #strict2 #monotone2 #commutative // TODO
    def eq(e1: Mod3, e2: Mod3): Belnap/Belnap = match (e1, e2) with {
        case (Bot, _)   => Belnap/Belnap.Bot
        case (_, Bot)   => Belnap/Belnap.Bot
        case (Zer, One) => Belnap/Belnap.False
        case (Zer, Two) => Belnap/Belnap.False
        case (One, Zer) => Belnap/Belnap.False
        case (One, Two) => Belnap/Belnap.False
        case (Two, Zer) => Belnap/Belnap.False
        case (Two, One) => Belnap/Belnap.False
        case _          => Belnap/Belnap.Top
    }

    ///
    /// Over-approximates `not equal`.
    ///
    // @strict @monotone @commutative // TODO
    def neq(e1: Mod3, e2: Mod3): Belnap/Belnap = Belnap/not(eq(e1, e2))

    ///
    /// Over-approximates `less than`.
    ///
    // @strict @monotone // TODO
    def less(e1: Mod3, e2: Mod3): Belnap/Belnap = match (e1, e2) with {
        case (Bot, _) => Belnap/Belnap.Bot
        case (_, Bot) => Belnap/Belnap.Bot
        case _        => Belnap/Belnap.Top
    }

    ///
    /// Over-approximates `less than or equal`.
    ///
    // @strict @monotone // TODO
    def lessEqual(e1: Mod3, e2: Mod3): Belnap/Belnap = Belnap/or(e1 `less` e2, e1 `eq` e2)

    let Mod3<> = (Bot, Top, leq, lub, glb)



    // ------------------------------------------------------------------------
    // TODO: Refactor
    // ------------------------------------------------------------------------

    law reflexive(⊑: (Mod3, Mod3) -> Bool): Bool = ∀(x: Mod3). x ⊑ x // TODO: Use Prelude

    law antiSymmetric(⊑: (Mod3, Mod3) -> Bool): Bool = ∀(x: Mod3, y: Mod3). ((x ⊑ y) ∧ (y ⊑ x)) → (x == y) // TODO: Use Prelude

    law transitive(⊑: (Mod3, Mod3) -> Bool): Bool = ∀(x: Mod3, y: Mod3, z: Mod3). ((x ⊑ y) ∧ (y ⊑ z)) → (x ⊑ z) // TODO: Use Prelude

    law upperBound(⊔: (Mod3, Mod3) -> Mod3): Bool = // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: Mod3, y: Mod3). (x ⊑ (x ⊔ y)) ∧ (y ⊑ (x ⊔ y))

    law leastUpperBound(⊔: (Mod3, Mod3) -> Mod3): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: Mod3, y: Mod3, z: Mod3). ((x ⊑ z) ∧ (y ⊑ z)) → ((x ⊔ y) ⊑ z)

    law lowerBound(⊓: (Mod3, Mod3) -> Mod3): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: Mod3, y: Mod3). ((x ⊓ y) ⊑ x) ∧ ((x ⊓ y) ⊑ y)

    law greatestLowerBound(⊓: (Mod3, Mod3) -> Mod3): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: Mod3, y: Mod3, z: Mod3). ((z ⊑ x) ∧ (z ⊑ y)) → (z ⊑ (x ⊓ y))

    law leastElement(⊑: (Mod3, Mod3) -> Bool, ⊥: Mod3): Bool = ∀(x: Mod3). ⊥ ⊑ x  // TODO: Should call prelude

    law greatestElement(⊑: (Mod3, Mod3) -> Bool, ⊤: Mod3): Bool = ∀(x: Mod3). x ⊑ ⊤  // TODO: Should call prelude

    law associative(f: (Mod3, Mod3) -> Mod3): Bool = // TODO: Use Prelude
        ∀(x: Mod3, y: Mod3, z: Mod3). f(x, f(y, z)) == f(f(x, y), z)

    law commutative(f: (Mod3, Mod3) -> Mod3): Bool = // TODO: Use Prelude
        ∀(x: Mod3, y: Mod3). f(x, y) == f(y, x)

    law strict1(f: Mod3 -> Mod3): Bool =  // TODO: Should call prelude
        f(Bot) == Bot

    law strict2(f: (Mod3, Mod3) -> Mod3): Bool =  // TODO: Should call prelude
        f(Bot, Bot) == Bot

    law monotone1(f: Mod3 -> Mod3): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: Mod3, y: Mod3). (x ⊑ y) → (f(x) ⊑ f(y))

    law monotone2(f: (Mod3, Mod3) -> Mod3): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x1: Mod3, x2: Mod3, y1: Mod3, y2: Mod3). ((x1 ⊑ y1) ∧ (x2 ⊑ y2) ) → (f(x1, x2) ⊑ f(y1, y2))

    // TODO: Names
    law approximates1(fa: Mod3 -> Mod3, fc: Int -> Int): Bool =
        ∀(x: Int). alpha(fc(x)) `leq` fa(alpha(x))

    // TODO: Names
    law approximates2(fa: (Mod3, Mod3) -> Mod3, fc: (Int, Int) -> Int): Bool =
        ∀(x: Int, y: Int). alpha(fc(x, y)) `leq` fa(alpha(x), alpha(y))

}