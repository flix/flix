// TODO: Everything in here is experimental and not yet implemented.

// ops to impl:  ∀, ¬, ∧, ∨, →, ↔, ≡, ⊥, ⊤, ⊑, ⊔, ⊓, ▽, △

// Scala code:
// case class Class(className: Ident, constraints: Set[ContextBound], typeVars: List[Ident])
// case class Impl(className: Ident, constraints: Set[ContextBound], forType: Type, defs: List[Definition])
// case class ContextBound(typeVar: Ident, className: Ident)

/**
 * A partial order is a set E equipped with a partial order `leq`.
 *
 * Informally,
 *  - If `leq(x, y)` then `x` is less than or equal to `y`.
 *  - If `leq(x, y)` and `leq(y, x)` are both true then `x == y`.
 *  - Finally, if neither `leq(x, y)` and `leq(y, x)` are true then `x` and `y` are incomparable.
 *
 * A partial order must satisfy three laws:
 *   - Reflexivity
 *   - AntiSymmetry
 *   - Transitivity
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
     * The reflexivity law requires that any element be less than or equal to itself.
     */
    law Reflexivity(x: E): Bool = x ⊑ x

    /**
     * The anti symmetry law requires that if `x` is less than or equal to `y`
     * and vice versa then those two elements must be equal.
     */
    law AntiSymmetry(x: E, y: E): Bool = (x ⊑ y ∧ y ⊑ x) → (x ≡ y)

    /**
     * The transitivity law requires that if `x` less than or equal to `y`
     * and `y` is less than or equal to `z` then `x` must be less than or
     * equal to `z`.
     */
    law Transitivity(x: E, y: E, z: E): Bool = (x ⊑ y ∧ y ⊑ z) → (x ⊑ z)
}

/**
 * A join semi lattice is a partial order equipped with
 * a bottom element `bot` and a least upper bound operator `lub`.
 *
 * A join semi lattice must satisfy the laws:
 */
class JoinSemiLattice[E] => PartialOrder[E] {
    /**
     * Returns the least element of the lattice.
     */
    fn bot: E

    /**
     *
     */
    fn lub(x: E, y: E): E

    /**
     * Alias for the least element of the lattice.
     */
    fn ⊥: E

    /**
     *
     */
    law LeastElement(x: E): Bool = ⊥ ⊑ x

    // upperbound

    // greatest upper bound
}

/**
 * A meet semi lattice is a partial order equipped with
 * a top element `top` and a greatest upper bound operator `glb`.
 *
 * A meet semi lattice must satisfy the laws:
 */
class MeetSemiLattice[E] {
    /**
     * Returns the greatest element of the lattice.
     */
    fn top: E

    /**
     * Alias for the greatest element of the lattice.
     */
    fn ⊤: E


    // top, glb
}

class BoundedLattice[E] {
    // height?
}



class WidenOperator[E] {

}

class NarrowOperator[E] {
    fn narrow(x: E): E

    /**
     *
     */
    law LowerBound(x: E, y: E): Bool = true
}


class PreOrder[E] {
    // ... if this has a different name?
    fn leq2(x: E, y: E): Bool = true
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

impl JoinSemiLattice[Sign] {
    /* ... */
}


impl PartialOrder[(A, B)] <= PartialOrder[A], PartialOrder[B] {

//    fn leq(x: (A, B), y: (A, B)): Bool =
//        let (xa, xb) = x in
//        let (ya, yb) = y in
//            leq(xa, xb) && leq(ya, yb)

}


// instance PreOrder e, Norm e => Lattice e
impl Lattice[E] <= PreOrder[E], Norm[E] {
    // disambiguate?
    fn bot: E = norm(bot)

    // disambiguate?
    fn leq(x: E, y: E): Bool = leq2(norm(x), norm(y))
}



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
