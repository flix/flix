/**
 * A bounded lattice is lattice that satisfies the ascending chain condition.

 * Specifically, a bounded lattice must provide a height function `h` that maps every lattice
 * element to a non-negative number that is decreasing according to the partial order.
 */
class BoundedLattice[E] => Lattice[E] {

    /**
     * Returns a non-negative integer that is decreasing according to the partial order.
     */
    def h(e: E): Int

    /**
     * The non-negative law asserts that the `h` function is non-negative for any element.
     */
    law NonNegative: Prop = ∀(x: E). h(x) >= 0

    /**
     * The ascending-chain law asserts that if an element `x` is strictly less than an
     * element `y` then f(x) must be strictly larger than f(y).
     *
     * Intuitively, every time we move upwards in the lattice the value of f(x) decreases.
     */
    law AscendingChain: Prop = ∀(x: E, y: E). (x ⊑ y) → (f(x) > f(y))
    // TODO: This does not prove descendingchain. We need to split h into two: distUp, distDn.

}