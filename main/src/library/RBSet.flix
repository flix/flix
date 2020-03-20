///
/// An experimental RBSet type represented internally as a red-black tree.
///
enum RBSet[t] {
    case RBSet(RBTree[t, Unit])
}

namespace RBSet {

    ///
    /// Returns the size of `xs`.
    ///
    pub def size(xs: RBSet[a]): Int32 =
        let RBSet(es) = xs;
        RBTree.size(es)

    ///
    /// Returns the empty set.
    ///
    pub def empty(): RBSet[a] = RBSet(RBTree.empty())

    ///
    /// Returns the singleton set containing `x`.
    ///
    pub def singleton(x: a): RBSet[a] = insert(x, empty())

    ///
    /// Adds `x` to `xs`.
    ///
    pub def insert(x: a, xs: RBSet[a]): RBSet[a] =
        let RBSet(es) = xs;
        RBSet(RBTree.insert(x, (), es))

    ///
    /// Removes `x` from `xs`.
    ///
    pub def delete(x: a, xs: RBSet[a]): RBSet[a] =
        let RBSet(es) = xs;
        RBSet(RBTree.delete(x, es))

    ///
    /// Returns true if and only if `xs` is the empty set.
    ///
    pub def isEmpty(xs: RBSet[a]): Bool =
        let RBSet(es) = xs;
        RBTree.isEmpty(es)

    ///
    /// Returns true if and only if `x` is a member of `xs`.
    ///
    pub def memberOf(x: a, xs: RBSet[a]): Bool =
        let RBSet(es) = xs;
        RBTree.contains(x, es)

    ///
    /// Returns true if and only if every element in `xs` appears in `ys`.
    ///
    pub def isSubsetOf(xs: RBSet[a], ys: RBSet[a]): Bool = forall(x -> memberOf(x, ys), xs)

    ///
    /// Returns true if and only if every element in `xs` appears in `ys` and `xs != ys`.
    ///
    pub def isProperSubsetOf(xs: RBSet[a], ys: RBSet[a]): Bool =
        if (isSubsetOf(xs, ys)) size(xs) != size(ys) else false

    ///
    /// Alias for `findLeft`.
    ///
    /// The function `f` must be pure.
    ///
    pub def find(f: a -> Bool, xs: RBSet[a]): Option[a] = findLeft(f, xs)

    ///
    /// Optionally returns the first element of `xs` that satisfies the predicate `f` when searching from left to right.
    ///
    /// The function `f` must be pure.
    ///
    pub def findLeft(f: a -> Bool, xs: RBSet[a]): Option[a] =
        let RBSet(es) = xs;
        match RBTree.findLeft((x, _) -> f(x), es) {
            case None => None
            case Some(x, _) => Some(x)
        }

    ///
    /// Optionally returns the first element of `xs` that satisfies the predicate `f` when searching from right to left.
    ///
    /// The function `f` must be pure.
    ///
    pub def findRight(f: a -> Bool, xs: RBSet[a]): Option[a] =
        let RBSet(es) = xs;
        match RBTree.findRight((x, _) -> f(x), es) {
            case None => None
            case Some(x, _) => Some(x)
        }

    ///
    /// Alias for `foldLeft`.
    ///
    pub def fold(f: (b, a) -> b & e, s: b, xs: RBSet[a]): b & e = foldLeft(f, s, xs)

    ///
    /// Applies `f` to a start value `s` and all elements in `xs` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b & e, s: b, xs: RBSet[a]): b & e =
        let RBSet(es) = xs;
        RBTree.foldLeft((b, k, _) -> f(b, k), s, es)

    ///
    /// Applies `f` to a start value `s` and all elements in `xs` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    pub def foldRight(f: (b, a) -> b & e, s: b, xs: RBSet[a]): b & e =
        let RBSet(es) = xs;
        RBTree.foldRight((b, k, _) -> f(b, k), s, es)

    ///
    /// Alias for `reduceLeft`.
    ///
    pub def reduce(f: (a, a) -> a & e, xs: RBSet[a]): Option[a] & e = reduceLeft(f, xs)

    ///
    /// Applies `f` to all elements in `xs` going from left to right until a single value `v` is obtained.  Returns `Some(v)`.
    /// That is, the result is of the form: `Some(f(...f(f(x1, x2), x3)..., xn))`
    /// Returns `None` if `xs` is the empty set.
    ///
    pub def reduceLeft(f: (a, a) -> a & e, xs: RBSet[a]): Option[a] & e =
        let RBSet(es) = xs;
        match RBTree.reduceLeft((x, _, y, _) -> (f(x, y), ()), es) {
            case None => None
            case Some(x, _) => Some(x)
        }

    ///
    /// Applies `f` to all elements in `xs` going from right to left until a single value `v` is obtained.  Returns `Some(v)`.
    /// That is, the result is of the form: `Some(f(x1, ...f(xn-2, f(xn-1, xn))...))`
    /// Returns `None` if `xs` is the empty set.
    ///
    pub def reduceRight(f: (a, a) -> a & e, xs: RBSet[a]): Option[a] & e =
        let RBSet(es) = xs;
        match RBTree.reduceRight((x, _, y, _) -> (f(x, y), ()), es) {
            case None => None
            case Some(x, _) => Some(x)
        }

    ///
    /// Returns the number of elements in `xs` that satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def count(f: a -> Bool, xs: RBSet[a]): Int32 = foldLeft((acc, x) -> if (f(x)) 1 + acc else acc, 0, xs)

    ///
    /// Returns the union of the elements in `xs`.
    ///
    pub def flatten(xs: RBSet[RBSet[a]]): RBSet[a] =
        foldLeft((acc, x) -> union(acc, x), empty(), xs)

    ///
    /// Returns `true` if and only if at least one element in `xs` satisfies the predicate `f`.
    ///
    /// Returns `false` if `xs` is the empty set.
    ///
    /// The function `f` must be pure.
    ///
    pub def exists(f: a -> Bool, xs: RBSet[a]): Bool =
        let RBSet(es) = xs;
        RBTree.exists((x, _) -> f(x), es)

    ///
    /// Returns `true` if and only if all elements in `xs` satisfy the predicate `f`.
    ///
    /// Returns `true` if `xs` is the empty set.
    ///
    /// The function `f` must be pure.
    ///
    pub def forall(f: a -> Bool, xs: RBSet[a]): Bool =
        let RBSet(es) = xs;
        RBTree.forall((x, _) -> f(x), es)

    ///
    /// Returns the union of `xs` and `ys`.
    ///
    pub def union(xs: RBSet[a], ys: RBSet[a]): RBSet[a] =
        if (size(xs) >= size(ys)) foldLeft((acc, x) -> insert(x, acc), xs, ys) else union(ys, xs)

    ///
    /// Returns the intersection of `xs` and `ys`.
    ///
    pub def intersection(xs: RBSet[a], ys: RBSet[a]): RBSet[a] =
        if (size(xs) >= size(ys))
        foldLeft((acc, x) -> if (memberOf(x, xs)) insert(x, acc) else acc, empty(), ys)
        else intersection(ys, xs)

    ///
    /// Returns the difference of `xs` and `ys`, i.e. `xs - ys`.
    ///
    pub def difference(xs: RBSet[a], ys: RBSet[a]): RBSet[a] =
        foldLeft((acc, x) -> if (!memberOf(x, ys)) insert(x, acc) else acc, empty(), xs)

    ///
    /// Returns all subsets of `xs`.
    ///
    pub def subsets(xs: RBSet[a]): RBSet[RBSet[a]] =
        foldLeft((acc, x) -> union(map(y -> insert(x, y), acc), acc), insert(empty(), empty()), xs)

    ///
    /// Returns the set of all elements of `xs` that satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def filter(f: a -> Bool, xs: RBSet[a]): RBSet[a] =
        foldLeft((acc, x) -> if (f(x)) insert(x, acc) else acc, empty(), xs)

    ///
    /// Returns the result of applying `f` to every element in `xs`.
    ///
    /// Note: The returned set may be smaller than `xs`.
    ///
    pub def map(f: a -> b & e, xs: RBSet[a]): RBSet[b] & e =
        foldLeft((acc, x) -> insert(f(x), acc), empty(), xs)

    ///
    /// Returns the result of applying `f` to every element in `xs` and taking the union.
    ///
    pub def flatMap(f: a -> RBSet[b] & e, xs: RBSet[a]): RBSet[b] & e =
        foldLeft((acc, x) -> union(acc, f(x)), empty(), xs)

    ///
    /// Replaces the element `x` with `y` if `x` is in `xs`. Otherwise, returns `xs`.
    ///
    /// Note: The returned set may be smaller than `xs`.
    ///
    pub def replace(x: a, y: a, xs: RBSet[a]): RBSet[a] =
        if (memberOf(x, xs)) insert(y, delete(x, xs)) else xs

    ///
    /// Returns a pair of sets `(ys, zs)`.
    ///
    /// `ys` contains all elements of `xs` that satisfy the predicate `f`.
    /// `zs` contains all elements of `xs` that do not satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def partition(f: a -> Bool, xs: RBSet[a]): (RBSet[a], RBSet[a]) =
        foldLeft((acc, x) -> let (a, b) = acc; if (f(x)) (insert(x, a), b) else (a, insert(x, b)), (empty(), empty()), xs)

    ///
    /// Returns the set `xs` as a list.
    ///
    pub def toList(xs: RBSet[a]): List[a] = foldLeft((acc, x) -> x :: acc, Nil, xs)

    ///
    /// Returns the association set `xs` as a map.
    ///
    /// If `xs` contains multiple mappings with the same key, `toMap` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    pub def toMap(xs: RBSet[(a, b)]): Map[a, b] =
        foldLeft((acc, x) -> let (k, v) = x; Map.insert(k, v, acc), Map#{}, xs)

    ///
    /// Applies `f` to every element of `xs`.
    ///
    /// The function `f` must be impure.
    ///
    pub def foreach(f: a ~> Unit, xs: RBSet[a]): Unit & Impure =
        let RBSet(es) = xs;
        RBTree.foreach((x, _) -> f(x), es)

    ///
    /// Helper function for `cmp`.
    ///
    def __cmpHelper(xs: List[a], ys: List[a]): Int32 = match (xs, ys) {
        case (z :: zs, w :: ws) =>
            let cmp = z <=> w;
            if (cmp == 0) __cmpHelper(zs, ws) else cmp
        case (_ :: _, Nil) => 1
        case (Nil, _ :: _) => -1
        case (Nil, Nil) => 0
    }

    ///
    /// Returns the result of a three-way comparison between `xs` and `ys`.
    ///
    /// Returns n < 0 if `xs` is lexicographically smaller than `ys`.
    /// Returns n = 0 if `xs` is equal to `ys`.
    /// Returns n > 0 if `xs` is lexicographically greater than `ys`.
    ///
    pub def __cmp(xs: RBSet[a], ys: RBSet[a]): Int32 = __cmpHelper(toList(xs), toList(ys))

    ///
    /// Returns `true` if and only if `xs` and `ys` are equal, i.e. they have the same elements.
    ///
    pub def __eq(xs: RBSet[a], ys: RBSet[a]): Bool = __cmp(xs, ys) == 0

    ///
    /// Helper function for `range`.
    ///
    def rangeHelper(b: Int32, n: Int32, acc: RBSet[Int32]): RBSet[Int32] =
        if (n < b) acc else rangeHelper(b, n - 1, insert(n, acc))

    ///
    /// Returns a set of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns `empty()` if `b >= e`.
    ///
    pub def range(b: Int32, e: Int32): RBSet[Int32] =
        if (b >= e) empty() else rangeHelper(b, e - 1, empty())

    ///
    /// Returns a rudimentary string representation of `xs`.
    ///
    /// Note: Currently only supports RBSet[Int32].
    pub def toString(xs: RBSet[Int32]): Str =
        "RBSet(" +
            foldLeft((acc: Str, x: Int32) ->
                if (String.isEmpty(acc)) Int32.toString(x)
                else acc + ", " + Int32.toString(x), "", xs)
         + ")"

}