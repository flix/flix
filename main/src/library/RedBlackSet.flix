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

/// TODO: Reintroduce when Set is replaced by RedBlackSet
///    ///
///    /// Returns the empty set.
///    ///
///    /// `RedBlackSet#{}` is syntactic sugar for `empty` (`RedBlackSet#{} = empty()`).
///    ///
///    pub def empty(): RedBlackSet[a] = RedBlackSet(Leaf)
///
///    ///
///    /// Returns the singleton set containing `x`.
///    ///
///    /// `RedBlackSet#{x}` is syntactic sugar for `singleton` (`RedBlackSet#{x} = singleton(x)`).
///    ///
///    pub def singleton(x: a): RedBlackSet[a] = insert(x, empty())

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
    /// Returns true if and only if every element in `xs` appears in `ys`.
    ///
    pub def isSubsetOf(xs: RedBlackSet[a], ys: RedBlackSet[a]): Bool = forall(x -> memberOf(x, ys), xs)

    ///
    /// Returns true if and only if every element in `xs` appears in `ys` and `xs != ys`.
    ///
    pub def isProperSubsetOf(xs: RedBlackSet[a], ys: RedBlackSet[a]): Bool =
        if (isSubsetOf(xs, ys)) size(xs) != size(ys) else false

    ///
    /// Alias for `findLeft`.
    ///
    /// The function `f` must be pure.
    ///
    pub def find(f: a -> Bool, xs: RedBlackSet[a]): Option[a] = findLeft(f, xs)

    ///
    /// Optionally returns the first element of `xs` that satisfies the predicate `f` when searching from left to right.
    ///
    /// The function `f` must be pure.
    ///
    pub def findLeft(f: a -> Bool, xs: RedBlackSet[a]): Option[a] =
        let result = foldLeft((acc, x) -> if (f(x)) insert(x, acc) else acc, empty(), xs);
        if (isEmpty(result)) None
        else let RedBlackSet(es) = result;
        RedBlackTree.leftMost(es)

    ///
    /// Optionally returns the first element of `xs` that satisfies the predicate `f` when searching from right to left.
    ///
    /// The function `f` must be pure.
    ///
    pub def findRight(f: a -> Bool, xs: RedBlackSet[a]): Option[a] =
        let result = foldLeft((acc, x) -> if (f(x)) insert(x, acc) else acc, empty(), xs);
        if (isEmpty(result)) None
        else let RedBlackSet(es) = result;
        RedBlackTree.rightMost(es)

    ///
    /// Alias for `foldLeft`.
    ///
    pub def fold(f: (b, a) -> b & e, s: b, xs: RedBlackSet[a]): b & e = foldLeft(f, s, xs)

    ///
    /// Applies `f` to a start value `s` and all elements in `xs` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b & e, s: b, xs: RedBlackSet[a]): b & e =
        let RedBlackSet(es) = xs;
        RedBlackTree.foldLeft(f, s, es)

    ///
    /// Applies `f` to a start value `s` and all elements in `xs` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    pub def foldRight(f: (b, a) -> b & e, s: b, xs: RedBlackSet[a]): b & e =
        let RedBlackSet(es) = xs;
        RedBlackTree.foldRight(f, s, es)

    ///
    /// Alias for `reduceLeft`.
    ///
    pub def reduce(f: (a, a) -> a & e, xs: RedBlackSet[a]): Option[a] & e = reduceLeft(f, xs)

    ///
    /// Applies `f` to all elements in `xs` going from left to right until a single value `v` is obtained.  Returns `Some(v)`.
    /// That is, the result is of the form: `Some(f(...f(f(x1, x2), x3)..., xn))`
    /// Returns `None` if `xs` is the empty set.
    ///
    /// TODO: Fix, how to get first value as initial?
    pub def reduceLeft(f: (a, a) -> a & e, xs: RedBlackSet[a]): Option[a] & e =
        if (isEmpty(xs)) None else Some(foldLeft(f, empty(), xs))

    ///
    /// Applies `f` to all elements in `xs` going from right to left until a single value `v` is obtained.  Returns `Some(v)`.
    /// That is, the result is of the form: `Some(f(x1, ...f(xn-2, f(xn-1, xn))...))`
    /// Returns `None` if `xs` is the empty set.
    ///
    pub def reduceRight(f: (a, a) -> a & e, xs: RedBlackSet[a]): Option[a] & e = ???

    ///
    /// Returns the number of elements in `xs` that satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def count(f: a -> Bool, xs: RedBlackSet[a]): Int32 = fold((acc, x) -> if (f(x)) 1 + acc else acc, 0, xs)

    ///
    /// Returns the union of the elements in `xs`.
    ///
    pub def flatten(xs: RedBlackSet[RedBlackSet[a]]): RedBlackSet[a] = ???

    ///
    /// Returns `true` if and only if at least one element in `xs` satisfies the predicate `f`.
    ///
    /// Returns `false` if `xs` is the empty set.
    ///
    /// The function `f` must be pure.
    ///
    pub def exists(f: a -> Bool, xs: RedBlackSet[a]): Bool =
        let result = foldLeft((acc, x) -> if (f(x)) insert(x, acc) else acc, empty(), xs);
        !isEmpty(result)

    ///
    /// Returns `true` if and only if all elements in `xs` satisfy the predicate `f`.
    ///
    /// Returns `true` if `xs` is the empty set.
    ///
    /// The function `f` must be pure.
    ///
    pub def forall(f: a -> Bool, xs: RedBlackSet[a]): Bool =
        let result = foldLeft((acc, x) -> if (f(x)) insert(x, acc) else acc, empty(), xs);
        size(xs) == size(result)

    ///
    /// Returns the union of `xs` and `ys`.
    ///
    pub def union(xs: RedBlackSet[a], ys: RedBlackSet[a]): RedBlackSet[a] =
        foldLeft((acc, x) -> insert(x, acc), ys, xs)

    ///
    /// Returns the intersection of `xs` and `ys`.
    ///
    pub def intersection(xs: RedBlackSet[a], ys: RedBlackSet[a]): RedBlackSet[a] =
        foldLeft((acc, x) -> if (memberOf(x, xs) && memberOf(x, ys)) insert(x, acc) else acc, empty(), xs)

    ///
    /// Returns the difference of `xs` and `ys`, i.e. `xs - ys`.
    ///
    pub def difference(xs: RedBlackSet[a], ys: RedBlackSet[a]): RedBlackSet[a] =
        foldLeft((acc, x) -> if (!memberOf(x, ys)) insert(x, acc) else acc, empty(), xs)

    ///
    /// Returns all subsets of `xs`.
    ///
    pub def subsets(xs: RedBlackSet[a]): RedBlackSet[RedBlackSet[a]] = ???

    ///
    /// Returns the set of all elements of `xs` that satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def filter(f: a -> Bool, xs: RedBlackSet[a]): RedBlackSet[a] =
        foldLeft((acc, x) -> if (f(x)) insert(x, acc) else acc, empty(), xs)

    ///
    /// Returns the result of applying `f` to every element in `xs`.
    ///
    /// Note: The returned set may be smaller than `xs`.
    ///
    pub def map(f: a -> b & e, xs: RedBlackSet[a]): RedBlackSet[b] & e =
        foldLeft((acc, x) -> insert(f(x), acc), empty(), xs)

    ///
    /// Returns the result of applying `f` to every element in `xs` and taking the union.
    ///
    pub def flatMap(f: a -> RedBlackSet[b] & e, xs: RedBlackSet[a]): RedBlackSet[b] & e = ???

    ///
    /// Replaces the element `x` with `y` if `x` is in `xs`. Otherwise, returns `xs`.
    ///
    /// Note: The returned set may be smaller than `xs`.
    ///
    pub def replace(x: a, y: a, xs: RedBlackSet[a]): RedBlackSet[a] = map(e -> if (e == x) y else e, xs)

    ///
    /// Returns a pair of sets `(ys, zs)`.
    ///
    /// `ys` contains all elements of `xs` that satisfy the predicate `f`.
    /// `zs` contains all elements of `xs` that do not satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def partition(f: a -> Bool, xs: RedBlackSet[a]): (RedBlackSet[a], RedBlackSet[a]) = ???

    ///
    /// Returns the set `xs` as a list.
    ///
    pub def toList(xs: RedBlackSet[a]): List[a] = foldRight((acc, x) -> x :: acc, Nil, xs)

    ///
    /// Returns the association set `xs` as a map.
    ///
    /// If `xs` contains multiple mappings with the same key, `toMap` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    pub def toMap(xs: RedBlackSet[(a, b)]): Map[a, b] = ???

    ///
    /// Applies `f` to every element of `xs`.
    ///
    /// The function `f` must be impure.
    ///
    pub def foreach(f: a ~> Unit, xs: RedBlackSet[a]): Unit & Impure = ???

    ///
    /// Returns `true` if and only if `xs` and `ys` and equal, i.e. they have the same elements.
    ///
    pub def __eq(xs: RedBlackSet[a], ys: RedBlackSet[a]): Bool = isSubsetOf(xs, ys) && size(xs) == size(ys)

    ///
    /// Helper function for `range`.
    ///
    def rangeHelper(b: Int32, n: Int32, acc: RedBlackSet[Int32]): RedBlackSet[Int32] =
        if (n < b) acc else rangeHelper(b, n - 1, insert(n, acc))

    ///
    /// Returns a set of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns `empty()` if `b >= e`.
    ///
    pub def range(b: Int32, e: Int32): RedBlackSet[Int32] =
        if (b >= e) empty() else rangeHelper(b, e - 1, empty())

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