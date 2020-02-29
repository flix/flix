///
/// An experimental RedBlackSet type represented internally as a red-black tree.
///
///
enum RedBlackSet[t] {
    case RedBlackSet(RedBlackTree[t])
}

namespace RedBlackSet {

    ///
    /// Returns the empty set.
    ///
    pub def empty(): RedBlackSet[a] = RedBlackSet(Leaf)

    ///
    /// Returns the size of `xs`.
    ///
    pub def size(xs: RedBlackSet[a]): Int32 =
        let RedBlackSet(es) = xs;
            RedBlackTree.size(es)

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
    /// Applies `f` to a start value `s` and all elements in `xs`.
    ///
    pub def fold(f: (b, a) -> b, s: b, xs: RedBlackSet[a]): b =
        let RedBlackSet(es) = xs;
            RedBlackTree.fold(f, s, es)

    ///
    /// Returns the result of applying `f` to every element in `xs`.
    ///
    /// Note: The returned set may be smaller than `xs`.
    ///
    pub def map(f: a -> b, xs: RedBlackSet[a]): RedBlackSet[b] =
        let RedBlackSet(es) = xs;
            RedBlackTree.fold((acc, x) -> insert(f(x), acc), empty(), es)

}