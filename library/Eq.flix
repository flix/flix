/**
 * A type equipped with equality.
 */
class Eq[E] {

    /**
     *  Returns `true` if `x` is equal to `y`.
     */
    def eq(x: E, y: E): Bool

    /**
     *  Returns `true` if `x` is *not* equal to `y`.
     */
    def neq(x: E, y: E): Bool = !eq(x, y)

}
