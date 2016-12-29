/**
 * An implementation of the constant propagation lattice.
 */
namespace Domain.Constant {

    ///
    /// The elements of the constant propagation lattice.
    ///
    enum Constant {
          case Top,

        case Cst(Int),

          case Bot
    }

    ///
    /// Returns `true` iff `e1` is less than or equal to `e2`.
    ///
    #reflexive #antiSymmetric #transitive #leastElement(Bot) #greatestElement(Top)
    def leq(e1: Constant, e2: Constant): Bool = match (e1, e2) with {
        case (Bot, _)           => true
        case (Cst(n1), Cst(n2)) => n1 == n2
        case (_, Top)           => true
        case _                  => false
    }

    ///
    /// Returns the least upper bound of `e1` and `e2`.
    ///
    #commutative #associative #upperBound #leastUpperBound
    def lub(e1: Constant, e2: Constant): Constant = match (e1, e2) with {
        case (Bot, x)           => x
        case (x, Bot)           => x
        case (Cst(n1), Cst(n2)) => if (n1 == n2) e1 else Top
        case _                  => Top
    }

    ///
    /// Returns the greatest lower bound of `e1` and `e2`.
    ///
    #commutative #associative #lowerBound #greatestLowerBound
    def glb(e1: Constant, e2: Constant): Constant = match (e1, e2) with {
        case (Top, x)           => x
        case (x, Top)           => x
        case (Cst(n1), Cst(n2)) => if (n1 == n2) e1 else Bot
        case _                  => Bot
    }

    ///
    /// Abstracts a concrete integer into the Sign domain.
    ///
    def alpha(i: Int): Constant = Cst(i)

    /**
     * Over-approximates integer `increment`.
     */
    #approximates1(x -> x + 1)
    #strict1 #monotone1
    def inc(e: Constant): Constant = match e with {
        case Bot    => Bot
        case Cst(n) => Cst(n + 1)
        case Top    => Top
    }

    ///
    /// Over-approximates integer `decrement`.
    ///
    #approximates1(x -> x - 1)
    #strict1 #monotone1
    def dec(e: Constant): Constant = match e with {
        case Bot    => Bot
        case Cst(n) => Cst(n - 1)
        case Top    => Top
    }

    ///
    /// Over-approximates integer `addition`.
    ///
    #approximates2((x, y) -> x + y)
    #strict2 #monotone2 #commutative #associative
    def plus(e1: Constant, e2: Constant): Constant = match (e1, e2) with {
        case (Bot, _)           => Bot
        case (_, Bot)           => Bot
        case (Cst(n1), Cst(n2)) => Cst(n1 + n2)
        case _                  => Top
    }

    ///
    /// Over-approximates integer `subtraction`.
    ///
    #approximates2((x, y) -> x - y)
    #strict2 #monotone2
    def minus(e1: Constant, e2: Constant): Constant = match (e1, e2) with {
        case (Bot, _)           => Bot
        case (_, Bot)           => Bot
        case (Cst(n1), Cst(n2)) => Cst(n1 - n2)
        case _                  => Top
    }

    ///
    /// Over-approximates integer `multiplication`.
    ///
    #approximates2((x, y) -> x * y)
    #strict2 #monotone2 #commutative #associative
    def times(e1: Constant, e2: Constant): Constant = match (e1, e2) with {
        case (Bot, _)           => Bot
        case (_, Bot)           => Bot
        case (Cst(n1), Cst(n2)) => Cst(n1 * n2)
        case _                  => Top
    }

    ///
    /// Over-approximates integer `division`.
    ///
    #approximates2((x, y) -> x / y)
    #strict2 #monotone2
    def divide(e1: Constant, e2: Constant): Constant = match (e1, e2) with {
        case (Bot, _)           => Bot
        case (_, Bot)           => Bot
        case (Cst(n1), Cst(n2)) => Cst(n1 / n2)
        case _                  => Top
    }

    ///
    /// Over-approximates integer `modulus`.
    ///
    #approximates2((x, y) -> x % y)
    #strict2 #monotone2
    def modulo(e1: Constant, e2: Constant): Constant = match (e1, e2) with {
        case (Bot, _)           => Bot
        case (_, Bot)           => Bot
        case (Cst(n1), Cst(n2)) => Cst(n1 % n2)
        case _                  => Top
    }

    ///
    /// Over-approximates `bitwise negation`.
    ///
    #approximates1(x -> -x)
    #strict1 #monotone1
    def negate(e: Constant): Constant = match e with {
        case Bot       => Bot
        case Cst(n)    => Cst(~ n)
        case Top       => Top
    }

    ///
    /// Over-approximates `bitwise and`.
    ///
    #approximates2((x, y) -> x & y)
    #strict2 #monotone2 #commutative #associative
    def and(e1: Constant, e2: Constant): Constant = match (e1, e2) with {
        case (Bot, _)           => Bot
        case (_, Bot)           => Bot
        case (Cst(n1), Cst(n2)) => Cst(n1 & n2)
        case _                  => Top
    }

    ///
    /// Over-approximates `bitwise or`.
    ///
    #approximates2((x, y) -> x | y)
    #strict2 #monotone2 #commutative #associative
    def or(e1: Constant, e2: Constant): Constant = match (e1, e2) with {
        case (Bot, _)           => Bot
        case (_, Bot)           => Bot
        case (Cst(n1), Cst(n2)) => Cst(n1 | n2)
        case _                  => Top
    }

    ///
    /// Over-approximates `bitwise xor`.
    ///
    #approximates2((x, y) -> x ^ y)
    #strict2 #monotone2 #commutative #associative
    def xor(e1: Constant, e2: Constant): Constant = match (e1, e2) with {
        case (Bot, _)           => Bot
        case (_, Bot)           => Bot
        case (Cst(n1), Cst(n2)) => Cst(n1 ^ n2)
        case _                  => Top
    }

    ///
    /// Over-approximates `bitwise left shift`.
    ///
    #approximates2((x, y) -> x << y)
    #strict2 #monotone2
    def leftShift(e1: Constant, e2: Constant): Constant = match (e1, e2) with {
        case (Bot, _)           => Bot
        case (_, Bot)           => Bot
        case (Cst(n1), Cst(n2)) => Cst(n1 << n2)
        case _                  => Top
    }

    ///
    /// Over-approximates `bitwise right shift`.
    ///
    ///
    #approximates2((x, y) -> x >> y)
    #strict2 #monotone2
    def rightShift(e1: Constant, e2: Constant): Constant = match (e1, e2) with {
        case (Bot, _)           => Bot
        case (_, Bot)           => Bot
        case (Cst(n1), Cst(n2)) => Cst(n1 >> n2)
        case _                  => Top
    }

    ///
    /// Over-approximates `equal`.
    ///
    //@strict @monotone @commutative // TODO
    def eq(e1: Constant, e2: Constant): Belnap/Belnap = match (e1, e2) with {
        case (Bot, _)           => Belnap/Belnap.Bot
        case (_, Bot)           => Belnap/Belnap.Bot
        case (Cst(n1), Cst(n2)) => Belnap/alpha(n1 == n2)
        case _                  => Belnap/Belnap.Top
    }

    ///
    /// Over-approximates `not equal`.
    ///
    //@strict @monotone @commutative  // TODO
    def neq(e1: Constant, e2: Constant): Belnap/Belnap = Belnap/not(eq(e1, e2))

    ///
    /// Over-approximates `less than`.
    ///
    //@strict @monotone  // TODO
    def less(e1: Constant, e2: Constant): Belnap/Belnap = match (e1, e2) with {
        case (Bot, _)           => Belnap/Belnap.Bot
        case (_, Bot)           => Belnap/Belnap.Bot
        case (Cst(n1), Cst(n2)) => Belnap/alpha(n1 < n2)
        case _                  => Belnap/Belnap.Top
    }

    ///
    /// Over-approximates `less than or equal`.
    ///
    //@strict @monotone  // TODO
    def lessEqual(e1: Constant, e2: Constant): Belnap/Belnap = Belnap/or(e1 `less` e2, e1 `eq` e2)

    let Constant<> = (Bot, Top, leq, lub, glb)


   // ------------------------------------------------------------------------
    // TODO: Refactor
    // ------------------------------------------------------------------------

    law reflexive(⊑: (Constant, Constant) -> Bool): Bool = ∀(x: Constant). x ⊑ x // TODO: Use Prelude

    law antiSymmetric(⊑: (Constant, Constant) -> Bool): Bool = ∀(x: Constant, y: Constant). ((x ⊑ y) ∧ (y ⊑ x)) → (x == y) // TODO: Use Prelude

    law transitive(⊑: (Constant, Constant) -> Bool): Bool = ∀(x: Constant, y: Constant, z: Constant). ((x ⊑ y) ∧ (y ⊑ z)) → (x ⊑ z) // TODO: Use Prelude

    law upperBound(⊔: (Constant, Constant) -> Constant): Bool = // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: Constant, y: Constant). (x ⊑ (x ⊔ y)) ∧ (y ⊑ (x ⊔ y))

    law leastUpperBound(⊔: (Constant, Constant) -> Constant): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: Constant, y: Constant, z: Constant). ((x ⊑ z) ∧ (y ⊑ z)) → ((x ⊔ y) ⊑ z)

    law lowerBound(⊓: (Constant, Constant) -> Constant): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: Constant, y: Constant). ((x ⊓ y) ⊑ x) ∧ ((x ⊓ y) ⊑ y)

    law greatestLowerBound(⊓: (Constant, Constant) -> Constant): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: Constant, y: Constant, z: Constant). ((z ⊑ x) ∧ (z ⊑ y)) → (z ⊑ (x ⊓ y))

    law leastElement(⊑: (Constant, Constant) -> Bool, ⊥: Constant): Bool = ∀(x: Constant). ⊥ ⊑ x  // TODO: Should call prelude

    law greatestElement(⊑: (Constant, Constant) -> Bool, ⊤: Constant): Bool = ∀(x: Constant). x ⊑ ⊤  // TODO: Should call prelude

    law associative(f: (Constant, Constant) -> Constant): Bool = // TODO: Use Prelude
        ∀(x: Constant, y: Constant, z: Constant). f(x, f(y, z)) == f(f(x, y), z)

    law commutative(f: (Constant, Constant) -> Constant): Bool = // TODO: Use Prelude
        ∀(x: Constant, y: Constant). f(x, y) == f(y, x)

    law strict1(f: Constant -> Constant): Bool =  // TODO: Should call prelude
        f(Bot) == Bot

    law strict2(f: (Constant, Constant) -> Constant): Bool =  // TODO: Should call prelude
        f(Bot, Bot) == Bot

    law monotone1(f: Constant -> Constant): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x: Constant, y: Constant). (x ⊑ y) → (f(x) ⊑ f(y))

    law monotone2(f: (Constant, Constant) -> Constant): Bool =  // TODO: Should call prelude
        let ⊑ = leq;
            ∀(x1: Constant, x2: Constant, y1: Constant, y2: Constant). ((x1 ⊑ y1) ∧ (x2 ⊑ y2) ) → (f(x1, x2) ⊑ f(y1, y2))

    // TODO: Names
    law approximates1(fa: Constant -> Constant, fc: Int -> Int): Bool =
        ∀(x: Int). alpha(fc(x)) `leq` fa(alpha(x))

    // TODO: Names
    law approximates2(fa: (Constant, Constant) -> Constant, fc: (Int, Int) -> Int): Bool =
        ∀(x: Int, y: Int). alpha(fc(x, y)) `leq` fa(alpha(x), alpha(y))


// TODO: We can make galois connections betweem these guys...

}