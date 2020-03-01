///
/// An experimental RedBlackSet type represented internally as a red-black tree.
///
///
enum RedBlackSet[t] {
    case RedBlackSet(RedBlackTree[t])
}

namespace RedBlackSet {

    ///
    /// Returns the size of `xs`.
    ///
    pub def size(xs: RedBlackSet[a]): Int32 =
        let RedBlackSet(es) = xs;
            RedBlackTree.size(es)

    ///
    /// Returns the empty set.
    ///
    /// `RedBlackSet#{}` is syntactic sugar for `empty` (`RedBlackSet#{} = empty()`).
    ///
    /// TODO: Investigate how to activate the RedBlackSet#{} syntax
    pub def empty(): RedBlackSet[a] = RedBlackSet(Leaf)

    ///
    /// Returns the singleton set containing `x`.
    ///
    /// `RedBlackSet#{x}` is syntactic sugar for `singleton` (`RedBlackSet#{x} = singleton(x)`).
    ///
    /// TODO: Investigate how to activate the RedBlackSet#{x} syntax
    pub def singleton(x: a): RedBlackSet[a] = insert(x, empty())

    ///
    /// Adds `x` to `xs`.
    ///
    pub def insert(x: a, xs: RedBlackSet[a]): RedBlackSet[a] =
        let RedBlackSet(es) = xs;
            RedBlackSet(RedBlackTree.insert(x, es))

    ///
    /// Removes `x` from `xs`.
    ///
    pub def delete(x: a, xs: RedBlackSet[a]): RedBlackSet[a] =
        let RedBlackSet(es) = xs;
            RedBlackSet(RedBlackTree.delete(x, es))

    ///
    /// Returns true if and only if `xs` is the empty set, i.e. `RedBlackSet(Leaf)`.
    ///
    pub def isEmpty(xs: RedBlackSet[a]): Bool =
        let RedBlackSet(s) = xs;
            match s {
                case Leaf => true
                case _ => false
            }

    ///
    /// Returns true if and only if `x` is a member of `xs`.
    ///
    pub def memberOf(x: a, xs: RedBlackSet[a]): Bool =
        let RedBlackSet(es) = xs;
            RedBlackTree.contains(x, es)

    ///
    /// Helper function for `range`.
    ///
    def rangeHelper(b: Int32, n: Int32, acc: RedBlackSet[Int32]): RedBlackSet[Int32] = if (n < b) acc else rangeHelper(b, n - 1, insert(n, acc))

    ///
    /// Returns a set of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns `empty()` if `b >= e`.
    ///
    pub def range(b: Int32, e: Int32): RedBlackSet[Int32] = if (b >= e) empty() else rangeHelper(b, e - 1, empty())

    ///
    /// Alias for `foldLeft`.
    ///
    pub def fold(f: (b, a) -> b, s: b, xs: RedBlackSet[a]): b = foldLeft(f, s, xs)

    ///
    /// Applies `f` to a start value `s` and all elements in `xs` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b, s: b, xs: RedBlackSet[a]): b =
        let RedBlackSet(es) = xs;
            RedBlackTree.foldLeft(f, s, es)

    ///
    /// Applies `f` to a start value `s` and all elements in `xs` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    pub def foldRight(f: (b, a) -> b, s: b, xs: RedBlackSet[a]): b =
        let RedBlackSet(es) = xs;
            RedBlackTree.foldRight(f, s, es)

    ///
    /// Returns the result of applying `f` to every element in `xs`.
    ///
    /// Note: The returned set may be smaller than `xs`.
    ///
    pub def map(f: a -> b, xs: RedBlackSet[a]): RedBlackSet[b] = foldLeft((acc, x) -> insert(f(x), acc), empty(), xs)

    ///
    /// Returns a rudimentary string representation of `xs`.
    ///
    /// Note: Currently only supports RedBlackSet[Int32].
    pub def toString(xs: RedBlackSet[Int32]): Str =
        "RedBlackSet(" +
            fold((acc: Str, x: Int32) ->
                if (String.isEmpty(acc)) Int32.toString(x)
                else acc + ", " + Int32.toString(x), "", xs)
         + ")"

}