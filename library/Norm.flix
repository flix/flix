/**
 * Types equipped with an operator that turns a pre order into a partial order.
 */
class Norm[E] => PreOrder[E] {

    /**
     * Returns the representative element of the element `x`.
     */
    def norm(x: E): E

    /**
     * Alias for the norm.
     */
    def ⊡(x: E): E = norm(x)

    /**
     * The anti-symmetry law asserts that if `x` is less than or equal to `y`
     * and vice versa then the two elements must be equal.
     */
    law AntiSymmetry: Prop =
        ∀(x: E, y: E).
            let x1 = ⊡(x) in
            let y1 = ⊡(y) in
                (x1 ≼ y1 ∧ y1 ≼ x1) → (x1 ≡ y1)

}

/**
 * A pre order equipped with a norm is a partial order.
 */
impl PartialOrder[E] <= PreOrder[E], Norm[E] {

    /**
     * Returns `true` iff the element `x` is less than or equal to the element `y`.
     */
    def leq(x: E, y: E): Bool = ⊡(x) ⊑ ⊡(y)

}
