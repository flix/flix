/**
 * A constructor class for types that support the map operation.
 */
class Applicative[M[A]] {

    /**
     * Lifts `x` into the container `M`.
     */
    def lift(x: A): M[A]

    /**
     * Applies the function in the container `f` to value in the container `x`.
     */
    def ap(f: M[A -> B], x: M[A]): M[B]

}

//
// ## Options.
//

/**
 * Implementation of `Applicative[M[A]]` for `Opt[A]`.
 */
impl Applicative[Opt[A]] {
    def lift(x: A): Opt[A] = Some(x)

    def ap(f: Opt[A -> B], x: Opt[A]): Opt[B] = ???
}

//
// ## Lists.
//

/**
 * Implementation of `Applicative[M[A]]` for `List[A]`.
 */
impl Applicative[List[A]] {
    def lift(x: A): List[A] = x :: Nil

    def ap(f: List[A -> B], x: List[A]): List[B] = match x with {
        case Nil => Nil
        case ff :: rs => list/append(list/map(ff, x), ap(f, rs))
    }
}

