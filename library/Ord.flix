/**
 *  Types equipped with a total order.
 */
// TODO: rename to TotalOrder?
class Ord[E] => Eq[E] {

    /**
     *  Returns `true` iff `x` is less than or equal to `y`.
     */
    def leq(x: E, y: E): Bool

}

//
// ## Ints
//

impl Ord[Int8] {
    def leq(x: Int8, y: Int8): Bool = x <= y
}

