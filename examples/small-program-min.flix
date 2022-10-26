///
/// A type class for addition.
///
pub class Add[a] {
    ///
    /// Returns the sum of `x` and `y`.
    ///
    pub def add(x: a, y: a): a
}

instance Add[Int32] {
    pub def add(x: Int32, y: Int32): Int32 = $INT32_ADD$(x, y)
}

///
/// The identity function.
///
pub def identity(x: a): a = x

///
/// Casts a pure function to `ef`.
///
/// `upcast` is safe to use anywhere.
///
pub def upcast(f: a -> b): a -> b \ ef =
    x -> f(x) as \ ef

///
/// Meta-programming facility to express the time complexity of a function `f`.
///
@Internal
pub def time(_: a -> Int32): Int32 = 1

///
/// Meta-programming facility to express the space complexity of a function `f`.
///
@Internal
pub def space(_: a -> Int32): Int32 = 1


///
/// The List type.
///
/// A list is either the empty list represented by `Nil`, or
/// an element `v` followed by a list `vs` represented by `v :: vs`.
///
pub enum List[t] {
    case Nil,
    case Cons(t, List[t])
}

namespace List {

    ///
    /// Returns the length of `l`.
    ///
    @Time(length(l)) @Space(1)
    pub def length(l: List[a]): Int32 =
        def loop(ll, acc) = match ll {
            case Nil     => acc
            case _ :: xs => loop(xs, acc + 1)
        };
        loop(l, 0)

    ///
    /// Returns `l2` appended to `l1`.
    ///
    /// The infix operator `:::` is an alias for `append` (`l1 ::: l2 = append(l1, l2)`).
    ///
    @Time(length(l1)) @Space(length(l1))
    pub def append(l1: List[a], l2: List[a]): List[a] =
        foldRight((x, acc) -> x :: acc, l2, l1)

    ///
    /// Applies `f` to a start value `s` and all elements in `l` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    pub def foldRight(f: (a, b) -> b \ ef, s: b, l: List[a]): b \ ef =
        def loop(ll, k) = match ll {
            case Nil     => k(s)
            case x :: xs => loop(xs, ks -> k(f(x, ks)))
        };
        loop(l, upcast(identity)) // NB: Cast required because the identity continuation is pure,
                                  // but the intermediate continuations are effect polymorphic

}
