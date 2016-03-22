/**
 * A narrowing operator equips meet semi lattice with an operator `narrow`
 * that may improve precision after computation with widening.
 */
class Narrowing[E] => PartialOrder[E] {

    /**
     * The narrowing operator. Returns an element between `x` and `y`.
     */
    def narrow(x: E, y: Y): E

    /**
     * Alias for the narrowing operator.
     */
    def △(x: E, y: E): E = narrow(x, y)

    /**
     * The bounding law asserts that the narrowing operator returns an
     * element that is bounded or "squeezed" between its two arguments.
     */
    law Bounding: Prop = ∀(x: E, y: E). (x ⊑ y) → ((x ⊑ (x △ y)) ∧ ((x △ y) ⊑ y))

    // TODO: Descending chain condition.

}