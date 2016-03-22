/**
 * A widening operator equips a join semi lattice with an operator `widen`
 * that accelerates the computation towards the fixpoint.
 */
class Widening[E] => PartialOrder[E] {

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

    // TODO: AscendingChain law here?

}