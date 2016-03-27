/**
 * Types equipped with a widening operator.
 */
class Widening[E] => JoinLattice[E] {

    /**
     * The widening operator. Returns an upper bound on `x` and `y`.
     */
    def widen(x: E, y: E): E

    /**
     * Alias for the widening operator `widen`.
     */
    def ▽(x: E, y: E): E = widen(x, y)

    /**
     * The covering law asserts that the widening operator returns an
     * element that is greater than or equal to each of its arguments.
     * It is equivalent to the upper bound law.
     */
    law Covering: Prop = ∀(x: E, y: E). (x ⊑ (x ▽ y)) ∧ (y ⊑ (x ▽ y))

}

/**
 * A type that redefines the partial order in terms of a widening operator.
 */
enum Widen[E: Widening] {
    case Widen(E)
}

/**
 * Implementation of `JoinLattice` for `Widen[E]`.
 */
impl JoinLattice[Widen[E]] {
    def bot: Widen[E] = Widen.Widen(bot)

    def lub(x: Widen[E], y: Widen[E]): Bool =
        let x1 = Widen.Widen(x) in
        let y1 = Widen.Widen(y) in
            x1 ▽ y1
}

/**
 * Implementation of `MeetLattice` for `Widen[E]`.
 */
impl MeetLattice[Widen[E: MeetLattice]] {
    def top: Widen[E] = Widen.Widen(top)

    def glb(x: Widen[E], y: Widen[E]): Bool =
        let x1 = Widen.Widen(x) in
        let y1 = Widen.Widen(y) in
            x1 ⊓ y1
}

// TODO: An alternative would be to introduce a LeastUpperBound type class
// and then let it be implemented by partial orders and widenings?

