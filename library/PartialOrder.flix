/**
 * Types equipped with a partial order.
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
    law Reflexivity: Prop = ∀(x: E). x ⊑ x

    /**
     * The anti-symmetry law asserts that if `x` is less than or equal to `y`
     * and vice versa then the two elements must be equal.
     */
    law AntiSymmetry: Prop = ∀(x: E, y: E). (x ⊑ y ∧ y ⊑ x) → (x ≡ y)

    /**
     * The transitivity law asserts that if `x` less than or equal to `y`
     * and `y` is less than or equal to `z` then `x` must be less than or
     * equal to `z`.
     */
    law Transitivity: Prop = ∀(x: E, y: E, z: E). (x ⊑ y ∧ y ⊑ z) → (x ⊑ z)

}