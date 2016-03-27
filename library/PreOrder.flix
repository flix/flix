/**
 * Types equiped with a pre order.
 */
class PreOrder[E] {

    /**
     * Returns `true` iff the element `x` is less than or equal to the element `y`.
     */
    def pleq(x: E, y: E): Bool

    /**
     * Alias for pleq.
     */
    def ≼(x: E, y: E): Bool = pleq(x, y)

    /**
     * The reflexivity law asserts that any element is less than or equal to itself.
     */
    law Reflexivity: Prop = ∀(x: E). x ≼ x

    /**
     * The transitivity law asserts that if `x` less than or equal to `y`
     * and `y` is less than or equal to `z` then `x` must be less than or
     * equal to `z`.
     */
    law Transitivity: Prop = ∀(x: E, y: E, z: E). (x ≼ y ∧ y ≼ z) → (x ≼ z)

}

