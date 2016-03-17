/**
 * A constructor class for types that support the map operation.
 */
class Mappable[M[A]] {

    /**
     * Applies the function `f` to the value inside the container `x`.
     */
    def map[B](f: A => B, x: M[A]): M[B]

}
