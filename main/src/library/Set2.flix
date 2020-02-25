///
/// An experimental Set2 type represented internally as a red-black tree.
///
///
enum Set2 {
    case Set2(RBTree)
}

namespace Set2 {

    ///
    /// Returns the empty set.
    ///
    pub def empty(): Set2 = Set2(Leaf)

    ///
    /// Returns the size of `xs`.
    ///
    pub def size(xs: Set2): Int32 =
        let Set2(s) = xs;
        RBTree.size(s)

    ///
    /// Adds `x` to `xs`.
    ///
    pub def insert(x: a, xs: Set2): Set2 =
        let Set2(s) = xs;
        Set2(RBTree.insert(s, x))

    ///
    /// Removes `x` from `xs`.
    ///
    pub def delete(x: a, xs: Set2): Set2 =
        let Set2(s) = xs;
        Set2(RBTree.delete(s, x))

    ///
    /// Returns true if and only if `x` is a member of `xs`.
    ///
    pub def memberOf(x: a, xs: Set2): Bool =
        let Set2(s) = xs;
        RBTree.contains(s, x)

    ///
    /// Helper function for `range`.
    ///
    def rangeHelper(b: Int32, n: Int32, acc: Set2): Set2 = if (n < b) acc else rangeHelper(b, n - 1, insert(n, acc))

    ///
    /// Returns a set of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns `empty()` if `b >= e`.
    ///
    /// TODO: Use a tail recursive version in List also, as List.range currently overflows the stack for large n
    pub def range(b: Int32, e: Int32): Set2 = if (b >= e) empty() else rangeHelper(b, e - 1, empty())

}