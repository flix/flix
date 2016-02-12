/**
 * A partial order is a type E equipped with a partial order.
 *
 * Informally,
 *
 *  - If `leq(x, y)` then `x` is less than or equal to `y`.
 *  - If both `leq(x, y)` and `leq(y, x)` hold then `x == y`.
 *  - If neither `leq(x, y)` and `leq(y, x)` are true then `x` and `y` are incomparable.
 */
class PartialOrder[E] {

    /**
     * Returns `true` iff the element `x` is less than or equal to the element `y`.
     */
    fn leq(x: E, y: E): Bool

    /**
     * Alias for `leq`.
     */
    fn ⊑(x: E, y: E): Bool = leq(x, y)

    /**
     * The reflexivity law asserts that any element is less than or equal to itself.
     */
    law Reflexivity(x: E): Bool = x ⊑ x

    /**
     * The anti-symmetry law asserts that if `x` is less than or equal to `y`
     * and vice versa then the two elements must be equal.
     */
    law AntiSymmetry(x: E, y: E): Bool = (x ⊑ y ∧ y ⊑ x) → (x ≡ y)

    /**
     * The transitivity law asserts that if `x` less than or equal to `y`
     * and `y` is less than or equal to `z` then `x` must be less than or
     * equal to `z`.
     */
    law Transitivity(x: E, y: E, z: E): Bool = (x ⊑ y ∧ y ⊑ z) → (x ⊑ z)

}


/**
 * A join semi lattice is a partial order equipped with a bottom element and a least upper bound.
 */
class JoinSemiLattice[E] => PartialOrder[E] {

    /**
     * Returns the least element of the lattice.
     */
    fn bot: E

    /**
     * Returns the least upper bound of the two elements `x` and `y`.
     */
    fn lub(x: E, y: E): E

    /**
     * Alias for the least element of the lattice.
     */
    fn ⊥: E = bot

    /**
     * Alias for the least upper bound operator.
     */
    fn ⊑(x: E, y: E): E = lub(x, y)

    /**
     * The least element law asserts that every element of the lattice is
     * less than or equal to the bottom element.
     */
    law LeastElement(x: E): Bool = ⊥ ⊑ x

    /**
     * The upper bound law asserts that the least upper bound operator returns an
     * element that is greater than or equal to each of its arguments.
     */
    law UpperBound(x: E, y: E): Bool = (x ⊑ (x ⊔ y)) ∧ (y ⊑ (x ⊔ y))

    /**
     * The least upper bound law asserts that the least upper bound operator returns
     * the smallest element that is larger than its two arguments.
     */
    law LeastUpperBound(x: E, y: E, z: E): Bool = ((x ⊑ z) ∧ (y ⊑ z)) → (x ⊔ y) ⊑ z

}


/**
 * A meet semi lattice is a partial order equipped with a top element and a greatest lower bound.
 */
class MeetSemiLattice[E] => PartialOrder[E] {

    /**
     * Returns the greatest element of the lattice.
     */
    fn top: E

    /**
     * Returns the greatest lower bound of the two elements `x` and `y`.
     */
    fn glb(x: E, y: E): E


    /**
     * Alias for the greatest element of the lattice.
     */
    fn ⊤: E

    /**
     * Alias for the greatest lower bound operator.
     */
    fn ⊓(x: E, y: E): E = glb(x, y)

    /**
     * The greatest element law asserts that every element of the lattice is
     * greater than or equal to the top element.
     */
    law GreatestElement(x: E): Bool = x ⊑ ⊤

    /**
     * The lower bound law asserts that the greatest lower bound operator returns an
     * element that is less than or equal to each of its arguments.
     */
    law LowerBound(x: E, y: E): Bool = ((x ⊓ y) ⊑ x) ∧ ((x ⊓ y) ⊑ y)

    /**
     * The greatest lower bound law asserts that the greatest lower bound operator returns
     * the largest element that is smaller than its two arguments.
     */
    law GreatestLowerBound(x: E, y: E, z: E): Bool = ((z ⊑ x) ∧ (z ⊑ y)) → (z ⊑ (x ⊓ y))

}


/**
 * A lattice is a join semi lattice and a meet semi lattice.
 */
class Lattice[E] => JoinSemiLattice[E], MeetSemiLattice[E] {
    /* no defs. */
    /* no laws. */
}


/**
 * A bounded lattice is lattice that satisfies the ascending chain condition.

 * Specifically, a bounded lattice must provide a height function `h` that maps every lattice
 * element to a non-negative number that is decreasing according to the partial order.
 */
class BoundedLattice[E] {

    /**
     * Returns a non-negative integer that is decreasing according to the partial order.
     */
    fn h(e: E): Int

    /**
     * The non-negative law asserts that the `h` function is non-negative for any element.
     */
    law NonNegative(x: E): Bool = h(x) >= 0

    /**
     * The ascending-chain law asserts that if an element `x` is strictly less than an
     * element `y` then f(x) must be strictly larger than f(y).
     *
     * Intuitively, every time we move upwards in the lattice the value of f(x) decreases.
     */
    law AscendingChain(x: E, y: E): Bool = (x ⊑ y) → (f(x) > f(y))

}


/**
 * A widening operator equips a join semi lattice with an operator `widen`
 * that accelerates the computation towards the fixpoint.
 */
class Widening[E] => JoinSemiLattice[E] {

    /**
     * The widening operator. Returns an upper bound on `x` and `y`.
     */
    fn widen(x: E, y: E): E

    /**
     * Alias for the widening operator `widen`.
     */
    fn ▽(x: E, y: E): E = widen(x, y)

    /**
     * The upper bound law asserts that the widening operator returns an
     * element that is greater than or equal to each of its arguments.
     */
    law UpperBound(x: E, y: E): Bool = (x ⊑ (x ▽ y)) ∧ (y ⊑ (x ▽ y))

    // TODO: Chain conditions?

}


/**
 * A narrowing operator equips meet semi lattice with an operator `narrow`
 * that may improve precision after computation with widening.
 */
class Narrowing[E] => MeetSemiLattice[E] {

    /**
     * The narrowing operator. Returns an element between `x` and `y`.
     */
    fn narrow(x: E, y: Y): E

    /**
     * Alias for the narrowing operator.
     */
    fn △(x: E, y: E): E = narrow(x, y)

    /**
     *
     */
    // TODO: Name of law?
    law Widening(x: E, y: E): Bool = (x ⊑ y) → ((x ⊑ (x △ y)) ∧ ((x △ y) ⊑ y))

    // TODO: Chain conditions?

}


/**
 * A pre order or quasi order is a type E equipped with a pre order.
 *
 * A pre order is like to a partial order, but is not required to satisfy the anti-symmetry law.
 */
class PreOrder[E] {

    /**
     * Returns `true` iff the element `x` is less than or equal to the element `y`.
     */
    fn leq2(x: E, y: E): Bool



}




/**
 *
 */
// class PreOrder e => NormOperator e
class NormOperator[E] => PreOrder[E] {
    fn norm(x: E): E
}

impl PartialOrder[Sign] {
    /* ... */
}

// instance PreOrder e, Norm e => Lattice e
impl Lattice[E] <= PreOrder[E], Norm[E] {
    // disambiguate?
    fn bot: E = norm(bot)

    // disambiguate?
    fn leq(x: E, y: E): Bool = leq2(norm(x), norm(y))
}


/**
 * on pairs
 */
impl PartialOrder[(A, B)] <= PartialOrder[A], PartialOrder[B] {

    fn leq(x: (A, B), y: (A, B)): Bool = ???: Bool

}

// TODO: Lattice defined by lub and glb.







// TODO: How to deal with these?
// TODO: Need some notion of function type here?
class Strict[A, B] => JoinSemiLattice[A], JoinSemiLattice[B] {
    // and then even this requirement cant readily be described?
    // how do you even disambiguate bot?
    law Strict(f: (A) -> B): Bool = f(bot) == bot
}

//prop Strict[A: JoinSemiLattice, B: JoinSemiLattice](f: A => B) {
//    law Strict(f: A => B): Bool = f(bot) == bot
//}
//
//is Strict[Sign, Belnap](Sign::mayEq)
//
//alternative

//fn Strict[A, B](f: A => B): Bool where A: JoinSemiLattice, B: JoinSemiLattice
//  = f(bot) == bot
//
//// instance of Law.
//law Strict(Sign/mayEq)
//
//// TODO: How to deal with these?
//class Monotone[E: PartialOrder] {
//    fn f(e: E): E
//}

// TODO: Commutativity, Associativity

// TODO: use word "impl"? for ?
// TODO: class or concept?
impl PartialOrder[Sign] {
    fn leq(x: Sign, y: Sign): Bool = true
}

//fn bar<T, K>(x: T, y: K) where T: Clone, K: Clone + Debug {
//    x.clone();
//    y.clone();
//    println!("{:?}", y);
//}
//

// TODO: Everything in here is experimental and not yet implemented.

// ops to impl:  ∀, ¬, ∧, ∨, →, ↔, ≡, ⊥, ⊤, ⊑, ⊔, ⊓, ▽, △

/**
 * Example implementation.
 */
// TODO: Can we implement this and get the super-classes for free?
// TODO: It seem in haskell we must implement each class
impl BoundedLattice[Sign] {
    /* ... */
}

