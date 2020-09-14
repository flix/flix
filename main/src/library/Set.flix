/*
 * Copyright 2017 Liam Palmer, Esben Bjerre
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use RedBlackTree.RedBlackTree;

///
/// The Set type.
///
/// A set is currently represented internally as a red-black tree.
///
enum Set[t] {
    case Set(RedBlackTree[t, Unit])
}

namespace Set {

    ///
    /// Returns the size of `xs`.
    ///
    @Time(size(xs)) @Space(1)
    pub def size(xs: Set[a]): Int32 =
        let Set(es) = xs;
        RedBlackTree.size(es)






    ///
    /// Returns the empty set.
    ///
    /// `Set#{}` is syntactic sugar for `empty` (`Set#{} = empty()`).
    ///
    @Time(1) @Space(1)
    pub def empty(): Set[a] = Set(RedBlackTree.empty())

    ///
    /// Returns the singleton set containing `x`.
    ///
    /// `Set#{x}` is syntactic sugar for `singleton` (`Set#{x} = singleton(x)`).
    ///
    @Time(1) @Space(1)
    pub def singleton(x: a): Set[a] = insert(x, empty())

    ///
    /// Returns a set of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns `empty()` if `b >= e`.
    ///
    @Time(e - b) @Space(e - b)
    pub def range(b: Int32, e: Int32): Set[Int32] =
        if (b >= e) empty() else rangeHelper(b, e - 1, empty())

    ///
    /// Adds `x` to `xs`.
    ///
    @Time(Int32.log2(size(xs))) @Space(Int32.log2(size(xs)))
    pub def insert(x: a, xs: Set[a]): Set[a] =
        let Set(es) = xs;
        Set(RedBlackTree.insert(x, (), es))

    ///
    /// Removes `x` from `xs`.
    ///
    @Time(Int32.log2(size(xs))) @Space(Int32.log2(size(xs)))
    pub def delete(x: a, xs: Set[a]): Set[a] =
        let Set(es) = xs;
        Set(RedBlackTree.delete(x, es))





    ///
    /// Returns true if and only if `xs` is the empty set.
    ///
    @Time(1) @Space(1)
    pub def isEmpty(xs: Set[a]): Bool =
        let Set(es) = xs;
        es == RedBlackTree.empty()

    ///
    /// Returns true if and only if `x` is a member of `xs`.
    ///
    @Time(Int32.log2(size(xs))) @Space(1)
    pub def memberOf(x: a, xs: Set[a]): Bool =
        let Set(es) = xs;
        RedBlackTree.memberOf(x, es)

    ///
    /// Returns true if and only if every element in `xs` appears in `ys`.
    ///
    @Time(size(xs)) @Space(size(xs) * Int32.log2(size(ys)))
    pub def isSubsetOf(xs: Set[a], ys: Set[a]): Bool = forall(x -> memberOf(x, ys), xs)

    ///
    /// Returns true if and only if every element in `xs` appears in `ys` and `xs != ys`.
    ///
    @Time(size(xs)) @Space(size(xs) * Int32.log2(size(ys)))
    pub def isProperSubsetOf(xs: Set[a], ys: Set[a]): Bool = size(xs) != size(ys) && isSubsetOf(xs, ys)

    ///
    /// Alias for `findLeft`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * size(xs)) @Space(space(f) * Int32.log2(size(xs)))
    pub def find(f: a -> Bool, xs: Set[a]): Option[a] = findLeft(f, xs)

    ///
    /// Optionally returns the first element of `xs` that satisfies the predicate `f` when searching from left to right.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * size(xs)) @Space(space(f) * Int32.log2(size(xs)))
    pub def findLeft(f: a -> Bool, xs: Set[a]): Option[a] =
        let Set(es) = xs;
        RedBlackTree.findLeft((x, _) -> f(x), es) |> Option.map(fst)

    ///
    /// Optionally returns the first element of `xs` that satisfies the predicate `f` when searching from right to left.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * size(xs)) @Space(space(f) * Int32.log2(size(xs)))
    pub def findRight(f: a -> Bool, xs: Set[a]): Option[a] =
        let Set(es) = xs;
        RedBlackTree.findRight((x, _) -> f(x), es) |> Option.map(fst)






    ///
    /// Alias for `foldLeft`.
    ///
    pub def fold(f: (b, a) -> b & e, s: b, xs: Set[a]): b & e = foldLeft(f, s, xs)

    ///
    /// Applies `f` to a start value `s` and all elements in `xs` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b & e, s: b, xs: Set[a]): b & e =
        let Set(es) = xs;
        RedBlackTree.foldLeft((acc, k, _) -> f(acc, k), s, es)

    ///
    /// Applies `f` to a start value `s` and all elements in `xs` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    pub def foldRight(f: (a, b) -> b & e, s: b, xs: Set[a]): b & e =
        let Set(es) = xs;
        RedBlackTree.foldRight((k, _, acc) -> f(k, acc), s, es)

    ///
    /// Alias for `reduceLeft`.
    ///
    pub def reduce(f: (a, a) -> a & e, xs: Set[a]): Option[a] & e = reduceLeft(f, xs)

    ///
    /// Applies `f` to all elements in `xs` going from left to right until a single value `v` is obtained.  Returns `Some(v)`.
    /// That is, the result is of the form: `Some(f(...f(f(x1, x2), x3)..., xn))`
    /// Returns `None` if `xs` is the empty set.
    ///
    pub def reduceLeft(f: (a, a) -> a & e, xs: Set[a]): Option[a] & e =
        let Set(es) = xs;
        RedBlackTree.reduceLeft((x, _, y, _) -> (f(x, y), ()), es) |> Option.map(fst)

    ///
    /// Applies `f` to all elements in `xs` going from right to left until a single value `v` is obtained.  Returns `Some(v)`.
    /// That is, the result is of the form: `Some(f(x1, ...f(xn-2, f(xn-1, xn))...))`
    /// Returns `None` if `xs` is the empty set.
    ///
    pub def reduceRight(f: (a, a) -> a & e, xs: Set[a]): Option[a] & e =
        let Set(es) = xs;
        RedBlackTree.reduceRight((x, _, y, _) -> (f(x, y), ()), es) |> Option.map(fst)






    ///
    /// Returns the number of elements in `xs` that satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * size(xs)) @Space(space(f) * Int32.log2(size(xs)))
    pub def count(f: a -> Bool, xs: Set[a]): Int32 =
        foldLeft((acc, x) -> if (f(x)) 1 + acc else acc, 0, xs)

    ///
    /// Returns the union of the elements in `xs`.
    ///
    @Time(size(xs) * Int32.log2(size(xs))) @Space(size(xs) * Int32.log2(size(xs)))
    pub def flatten(xs: Set[Set[a]]): Set[a] =
        foldLeft((acc, x) -> union(acc, x), empty(), xs)

    ///
    /// Returns `true` if and only if at least one element in `xs` satisfies the predicate `f`.
    ///
    /// Returns `false` if `xs` is the empty set.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * size(xs)) @Space(space(f) * Int32.log2(size(xs)))
    pub def exists(f: a -> Bool, xs: Set[a]): Bool =
        let Set(es) = xs;
        RedBlackTree.exists((x, _) -> f(x), es)

    ///
    /// Returns `true` if and only if all elements in `xs` satisfy the predicate `f`.
    ///
    /// Returns `true` if `xs` is the empty set.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * size(xs)) @Space(space(f) * Int32.log2(size(xs)))
    pub def forall(f: a -> Bool, xs: Set[a]): Bool =
        let Set(es) = xs;
        RedBlackTree.forall((x, _) -> f(x), es)






    ///
    /// Returns the union of `xs` and `ys`.
    ///
    @Time(size(ys) * Int32.log2(size(xs))) @Space(size(ys) * Int32.log2(size(xs)))
    pub def union(xs: Set[a], ys: Set[a]): Set[a] =
        foldLeft((acc, x) -> insert(x, acc), xs, ys)

    ///
    /// Returns the intersection of `xs` and `ys`.
    ///
    @Time(size(ys) * Int32.log2(size(xs))) @Space(size(ys) * Int32.log2(size(xs)))
    pub def intersection(xs: Set[a], ys: Set[a]): Set[a] =
        foldLeft((acc, x) -> if (memberOf(x, xs)) insert(x, acc) else acc, empty(), ys)

    ///
    /// Returns the difference of `xs` and `ys`, i.e. `xs - ys`.
    ///
    @Time(size(xs) * Int32.log2(size(ys))) @Space(size(xs) * Int32.log2(size(ys)))
    pub def difference(xs: Set[a], ys: Set[a]): Set[a] =
        foldLeft((acc, x) -> if (!memberOf(x, ys)) insert(x, acc) else acc, empty(), xs)

    ///
    /// Returns all subsets of `xs`.
    ///
    @Time(size(xs) * (1 <<< size(xs))) @Space(size(xs) * (1 <<< size(xs)))
    pub def subsets(xs: Set[a]): Set[Set[a]] =
        foldLeft((acc, x) -> union(map(y -> insert(x, y), acc), acc), insert(empty(), empty()), xs)






    ///
    /// Returns the set of all elements of `xs` that satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * size(xs)) @Space(space(f) * size(xs))
    pub def filter(f: a -> Bool, xs: Set[a]): Set[a] =
        foldLeft((acc, x) -> if (f(x)) insert(x, acc) else acc, empty(), xs)

    ///
    /// Returns the result of applying `f` to every element in `xs`.
    ///
    /// Note: The returned set may be smaller than `xs`.
    ///
    @Time(time(f) * size(xs)) @Space(space(f) * size(xs))
    pub def map(f: a -> b & e, xs: Set[a]): Set[b] & e =
        foldLeft((acc, x) -> insert(f(x), acc), empty(), xs)

    ///
    /// Returns the result of applying `f` to every element in `xs` and taking the union.
    ///
    @Time(time(f) * size(xs) * Int32.log2(size(xs))) @Space(space(f) * size(xs) * Int32.log2(size(xs)))
    pub def flatMap(f: a -> Set[b] & e, xs: Set[a]): Set[b] & e =
        foldLeft((acc, x) -> union(acc, f(x)), empty(), xs)

    ///
    /// Replaces the element `x` with `y` if `x` is in `xs`. Otherwise, returns `xs`.
    ///
    /// Note: The returned set may be smaller than `xs`.
    ///
    @Time(Int32.log2(size(xs))) @Space(Int32.log2(size(xs)))
    pub def replace(x: a, y: a, xs: Set[a]): Set[a] =
        if (memberOf(x, xs)) insert(y, delete(x, xs)) else xs

    ///
    /// Returns a pair of sets `(ys, zs)`.
    ///
    /// `ys` contains all elements of `xs` that satisfy the predicate `f`.
    /// `zs` contains all elements of `xs` that do not satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(size(xs) * Int32.log2(size(xs))) @Space(size(xs) * Int32.log2(size(xs)))
    pub def partition(f: a -> Bool, xs: Set[a]): (Set[a], Set[a]) =
        foldLeft((acc, x) ->
            let (a, b) = acc;
            if (f(x))
                (insert(x, a), b)
            else
                (a, insert(x, b)), (empty(), empty()),
            xs)




    ///
    /// Returns `xs` as a mutable set.
    ///
    @Time(1) @Space(1)
    pub def toMut(xs: Set[a]): MutSet[a] & Impure =
        MutSet(ref xs)

    ///
    /// Returns the set `xs` as a list.
    ///
    @Time(size(xs)) @Space(size(xs))
    pub def toList(xs: Set[a]): List[a] =
        foldLeft((acc, x) -> x :: acc, Nil, xs)

    ///
    /// Returns the association set `xs` as a map.
    ///
    /// If `xs` contains multiple mappings with the same key, `toMap` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    @Time(size(xs)) @Space(size(xs))
    pub def toMap(xs: Set[(a, b)]): Map[a, b] =
        foldRight((x, acc) -> let (k, v) = x; Map.insert(k, v, acc), Map#{}, xs)

    ///
    /// Applies `f` to every element of `xs`.
    ///
    /// The function `f` must be impure.
    ///
    @Time(size(f) * size(xs)) @Space(space(f) * Int32.log2(size(xs)))
    pub def foreach(f: a ~> Unit, xs: Set[a]): Unit & Impure =
        let Set(es) = xs;
        RedBlackTree.foreach((x, _) -> f(x), es)

    ///
    /// Build a set by applying `f` to the seed value `st`.
    ///
    /// `f` should return `Some(a,st1)` to signal a new set element `a` and a new seed value `st1`.
    ///
    /// `f` should return `None` to signal the end of building the set.
    ///
    pub def unfold(f: s -> Option[(a, s)] & e, st: s): Set[a] & e = unfoldHelper(f, st, empty())

    ///
    /// Helper function for `unfold`.
    ///
    def unfoldHelper(f: s -> Option[(a, s)] & e, st: s, ac: Set[a]): Set[a] & e = match f(st) {
        case None => ac
        case Some((a, st1)) => unfoldHelper(f, st1, insert(a, ac))
    }

    ///
    /// Build a set by applying the function `next` to `()`. `next` is expected to encapsulate
    /// a stateful resource such as a file handle that can be iterated.
    ///
    /// `next` should return `Some(a)` to signal a value pair `a`.
    ///
    /// `next` should return `None` to signal the end of building the set.
    ///
    pub def unfoldWithIter(next: () ~> Option[a]): Set[a] & Impure = unfoldWithIterHelper(next, empty())

    ///
    /// Helper function for `unfoldWithIter`.
    ///
    def unfoldWithIterHelper(next: () ~> Option[a], ac: Set[a]): Set[a] & Impure = match next() {
        case None => ac
        case Some(a) => unfoldWithIterHelper(next, insert(a, ac))
    }

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
    pub def __cmp(xs: Set[a], ys: Set[a]): Int32 =
        __cmpHelper(toList(xs), toList(ys))

    ///
    /// Helper function for `range`.
    ///
    def rangeHelper(b: Int32, n: Int32, acc: Set[Int32]): Set[Int32] =
        if (n < b) acc else rangeHelper(b, n - 1, insert(n, acc))

    ///
    /// Returns `true` if and only if `xs` and `ys` are equal, i.e. they have the same elements.
    ///
    pub def __eq(xs: Set[a], ys: Set[a]): Bool = __cmp(xs, ys) == 0
}