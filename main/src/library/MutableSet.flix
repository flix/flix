/*
 * Copyright 2020 Magnus Madsen
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

///
/// The Mutable Set type.
///
// TODO: Rename to MutSet
enum MutableSet[t] {
    case MutableSet(Ref[Set[t]])
}

namespace MutableSet {

    ///
    /// Returns the size of `xs`.
    ///
    pub def size(xs: MutableSet[a]): Int32 & Impure =
        let MutableSet(s) = xs;
        Set.size(deref s)

    ///
    /// Returns a fresh empty set.
    ///
    pub def empty(): MutableSet[a] & Impure = MutableSet(ref Set.empty())

    ///
    /// Returns the singleton set containing `x`.
    ///
    pub def singleton(x: a): MutableSet[a] & Impure = MutableSet(ref Set.singleton(x))

    ///
    /// Adds `x` to `xs`.
    ///
    pub def insert(x: a, xs: mut MutableSet[a]): Unit & Impure =
        let MutableSet(s) = xs;
        s := Set.insert(x, deref s)

    ///
    /// Removes `x` from `xs`.
    ///
    pub def delete(x: a, xs: mut MutableSet[a]): Unit & Impure =
        let MutableSet(s) = xs;
        s := Set.delete(x, deref s)

    ///
    /// Returns true if and only if `xs` is the empty set, i.e. `Set(Nil)`.
    ///
    pub def isEmpty(xs: MutableSet[a]): Bool & Impure =
        let MutableSet(s) = xs;
        Set.isEmpty(deref s)

    ///
    /// Returns true if and only if `x` is a member of `xs`.
    ///
    pub def memberOf(x: a, xs: MutableSet[a]): Bool & Impure =
        let MutableSet(s) = xs;
        Set.memberOf(x, deref s)

    ///
    /// Returns true if and only if every element in `xs` appears in `ys`.
    ///
    pub def isSubsetOf(xs: MutableSet[a], ys: MutableSet[a]): Bool & Impure =
        forall(x -> memberOf(x, ys) as & Pure, xs) // TODO: MemberOf is impure so not safe to use here..

    ///
    /// Returns true if and only if every element in `xs` appears in `ys` and `xs != ys`.
    ///
    pub def isProperSubsetOf(xs: MutableSet[a], ys: MutableSet[a]): Bool & Impure =
        if (isSubsetOf(xs, ys)) size(xs) != size(ys) else false // TODO: Weird definition?

    ///
    /// Alias for `findLeft`.
    ///
    /// The function `f` must be pure.
    ///
    pub def find(f: a -> Bool, xs: MutableSet[a]): Option[a] & Impure = findLeft(f, xs)

    ///
    /// Optionally returns the first element of `xs` that satisfies the predicate `f` when searching from left to right.
    ///
    /// The function `f` must be pure.
    ///
    pub def findLeft(f: a -> Bool, xs: MutableSet[a]): Option[a] & Impure =
        let MutableSet(s) = xs;
            Set.findLeft(f, deref s)

/*
    ///
    /// Optionally returns the first element of `xs` that satisfies the predicate `f` when searching from right to left.
    ///
    /// The function `f` must be pure.
    ///
    pub def findRight(f: a -> Bool, xs: Set[a]): Option[a] =
        let Set(s) = xs;
            List.findRight(f, s)






    ///
    /// Alias for `foldLeft`.
    ///
    pub def fold(f: (b, a) -> b & e, s: b, xs: Set[a]): b & e =
        let Set(es) = xs;
            List.fold(f, s, es)

    ///
    /// Applies `f` to a start value `s` and all elements in `xs` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b & e, s: b, xs: Set[a]): b & e =
        let Set(es) = xs;
            List.foldLeft(f, s, es)

    ///
    /// Applies `f` to a start value `s` and all elements in `xs` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    pub def foldRight(f: (a, b) -> b & e, s: b, xs: Set[a]): b & e =
        let Set(es) = xs;
            List.foldRight(f, s, es)

    ///
    /// Alias for `reduceLeftOpt`.
    ///
    pub def reduce(f: (a, a) -> a & e, xs: Set[a]): Option[a] & e =
        let Set(s) = xs;
            List.reduce(f, s)

    ///
    /// Applies `f` to all elements in `xs` going from left to right until a single value `v` is obtained.  Returns `Some(v)`.
    /// That is, the result is of the form: `Some(f(...f(f(x1, x2), x3)..., xn))`
    /// Returns `None` if `xs` is the empty set.
    ///
    pub def reduceLeft(f: (a, a) -> a & e, xs: Set[a]): Option[a] & e =
        let Set(s) = xs;
            List.reduceLeft(f, s)

    ///
    /// Applies `f` to all elements in `xs` going from right to left until a single value `v` is obtained.  Returns `Some(v)`.
    /// That is, the result is of the form: `Some(f(x1, ...f(xn-2, f(xn-1, xn))...))`
    /// Returns `None` if `xs` is the empty set.
    ///
    pub def reduceRight(f: (a, a) -> a & e, xs: Set[a]): Option[a] & e =
        let Set(s) = xs;
            List.reduceRight(f, s)

    ///
    /// Returns the number of elements in `xs` that satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def count(f: a -> Bool, xs: Set[a]): Int32 =
        let Set(s) = xs;
            List.count(f, s)

    ///
    /// Returns the union of the elements in `xs`.
    ///
    pub def flatten(xs: Set[Set[a]]): Set[a] = match xs {
        case Set(Nil) => empty()
        case Set(x :: rs) => union(x, flatten(Set(rs)))
    }
*/

    ///
    /// Returns `true` if and only if at least one element in `xs` satisfies the predicate `f`.
    ///
    /// Returns `false` if `xs` is the empty set.
    ///
    /// The function `f` must be pure.
    ///
    pub def exists(f: a -> Bool, xs: MutableSet[a]): Bool & Impure =
        let MutableSet(s) = xs;
            Set.exists(f, deref s)

    ///
    /// Returns `true` if and only if all elements in `xs` satisfy the predicate `f`.
    ///
    /// Returns `true` if `xs` is the empty set.
    ///
    /// The function `f` must be pure.
    ///
    pub def forall(f: a -> Bool, xs: MutableSet[a]): Bool & Impure =
        let MutableSet(s) = xs;
            Set.forall(f, deref s)

/*



    ///
    /// Returns the union of `xs` and `ys`.
    ///
    pub def union(xs: Set[a], ys: Set[a]): Set[a] =
        let Set(s) = xs;
            List.foldRight(insert, ys, s)

    ///
    /// Returns the intersection of `xs` and `ys`.
    ///
    pub def intersection(xs: Set[a], ys: Set[a]): Set[a] =
        let Set(s) = xs;
            List.foldRight((x, y) -> if (memberOf(x, ys)) insert(x, y) else y, empty(), s)

    ///
    /// Returns the difference of `xs` and `ys`, i.e. `xs - ys`.
    ///
    pub def difference(xs: Set[a], ys: Set[a]): Set[a] =
        let Set(s) = xs;
            List.foldRight((x, y) -> if (memberOf(x, ys)) y else insert(x, y), empty(), s)

    ///
    /// Returns all subsets of `xs`.
    ///
    pub def subsets(xs: Set[a]): Set[Set[a]] =
        let Set(s) = xs;
            Set(List.map(x -> Set(x), List.subsequences(s)))






    ///
    /// Returns the set of all elements of `xs` that satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def filter(f: a -> Bool, xs: Set[a]): Set[a] =
        let Set(s) = xs;
            Set(List.filter(f, s))

    ///
    /// Returns the result of applying `f` to every element in `xs`.
    ///
    /// Note: The returned set may be smaller than `xs`.
    ///
    pub def map(f: a -> b & e, xs: Set[a]): Set[b] & e = match xs {
        case Set(Nil) => Set(Nil)
        case Set(x :: rs) => insert(f(x), map(f, Set(rs)))
    }

    ///
    /// Returns the result of applying `f` to every element in `xs` and taking the union.
    ///
    pub def flatMap(f: a -> Set[b] & e, xs: Set[a]): Set[b] & e = match xs {
        case Set(Nil) => Set(Nil)
        case Set(x :: rs) => union(f(x), flatMap(f, Set(rs)))
    }

    ///
    /// Replaces the element `x` with `y` if `x` is in `xs`. Otherwise, returns `xs`.
    ///
    /// Note: The returned set may be smaller than `xs`.
    ///
    pub def replace(x: a, y: a, xs: Set[a]): Set[a] = map(e -> if (e == x) y else e, xs)

    ///
    /// Returns a pair of sets `(ys, zs)`.
    //
    /// `ys` contains all elements of `xs` that satisfy the predicate `f`.
    /// `zs` contains all elements of `xs` that do not satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def partition(f: a -> Bool, xs: Set[a]): (Set[a], Set[a]) =
        let Set(s) = xs;
        let (ys, zs) = List.partition(f, s);
            (Set(ys), Set(zs))
    */

    // TODO: toImmutable and toMutable?

    ///
    /// Returns the set `xs` as a list.
    ///
    pub def toList(xs: MutableSet[a]): List[a] & Impure =
        let MutableSet(s) = xs;
        Set.toList(deref s)

    ///
    /// Returns the association set `xs` as a map.
    ///
    /// If `xs` contains multiple mappings with the same key, `toMap` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    pub def toMap(xs: MutableSet[(a, b)]): Map[a, b] & Impure =
        let MutableSet(s) = xs;
        Set.toMap(deref s)

    ///
    /// Applies `f` to every element of `xs`.
    ///
    /// The function `f` must be impure.
    ///
    pub def foreach(f: a ~> Unit, xs: MutableSet[a]): Unit & Impure =
        let MutableSet(s) = xs;
        Set.foreach(f, deref s)

    ///
    /// Returns `true` if and only if `xs` and `ys` and equal, i.e. they have the same elements.
    ///
    pub def __eq(xs: MutableSet[a], ys: MutableSet[a]): Bool & Impure =
        isSubsetOf(xs, ys) && size(xs) == size(ys)

    /////////////////////////////////////////////////////////////////////////////
    // Lints: Simplifications                                                  //
    /////////////////////////////////////////////////////////////////////////////


    /////////////////////////////////////////////////////////////////////////////
    // Lints: Use Cases                                                        //
    /////////////////////////////////////////////////////////////////////////////


}

