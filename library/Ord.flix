/**
 *  Types equipped with a total order.
 */
class Ord[E] => Eq[E] {

    /**
     *  Returns `true` iff `x` is less than or equal to `y`.
     */
    def leq(x: E, y: E): Bool

}