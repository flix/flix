/*
 * Copyright 2017 Liam Palmer
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
/// The Set type.
///
/// A set is currently represented internally as a list.
///
enum Set[t] {
    case Set(List[t])
}

namespace Set {

    ///
    /// Returns the size of `xs`.
    ///
    def size[a](xs: Set[a]): Int32 =
        let Set(s) = xs;
            List.length(s)

    //
    // ## Set Construction
    //

    ///
    /// Returns the empty set.
    ///
    def empty[a]: Set[a] = Set(Nil)

    ///
    /// Returns the singleton set containing `x`.
    ///
    def singleton[a](x: a): Set[a] = Set(x :: Nil)

    ///
    /// Adds `x` to `xs`.
    ///
    def insert[a](x: a, xs: Set[a]): Set[a] =
        let Set(s) = xs;
            if (List.memberOf(x, s)) xs else Set(x :: s)

    ///
    /// Removes `x` from `xs`.
    ///
    def delete[a](x: a, xs: Set[a]): Set[a] =
        let Set(s) = xs;
            Set(List.remove(x, s))

    //
    // ## Set Predicates
    //

    ///
    /// Returns true if and only if `xs` is the empty set, i.e. `Set(Nil)`.
    ///
    def null[a](xs: Set[a]): Bool =
        let Set(s) = xs;
            List.isEmpty(s)

    ///
    /// Returns true if and only if `x` is a member of `xs`.
    ///
    def memberOf[a](x: a, xs: Set[a]): Bool =
        let Set(s) = xs;
            List.memberOf(x, s)

    ///
    /// Returns true if and only if every element in `xs` appears in `ys`.
    ///
    def isSubsetOf[a](xs: Set[a], ys: Set[a]): Bool = forall(x -> memberOf(x, ys), xs)

    ///
    /// Returns true if and only if every element in `xs` appears in `ys` and `xs != ys`.
    ///
    def isProperSubsetOf[a](xs: Set[a], ys: Set[a]): Bool = if (isSubsetOf(xs, ys)) size(xs) != size(ys) else false

    ///
    /// Returns `true` if and only if at least one element in `xs` satisfies the predicate `f`.
    ///
    def exists[a](f: a -> Bool, xs: Set[a]): Bool =
        let Set(s) = xs;
            List.exists(f, s)

    ///
    /// Returns `true` if and only if all elements in `xs` satisfy the predicate `f`.
    ///
    def forall[a](f: a -> Bool, xs: Set[a]): Bool =
        let Set(s) = xs;
            List.forall(f, s)

    //
    // ## Elementary Set Operations
    //

    ///
    /// Returns the union of `xs` and `ys`.
    ///
    def union[a](xs: Set[a], ys: Set[a]): Set[a] =
        let Set(s) = xs;
            List.foldRight(insert, ys, s)

    ///
    /// Returns the intersection of `xs` and `ys`.
    ///
    def intersection[a](xs: Set[a], ys: Set[a]): Set[a] =
        let Set(s) = xs;
            List.foldRight((x, y) -> if (memberOf(x, ys)) insert(x, y) else y, empty(), s)

    ///
    /// Returns the difference of `xs` and `ys`, i.e. `xs - ys`.
    ///
    def difference[a](xs: Set[a], ys: Set[a]): Set[a] =
        let Set(s) = xs;
            List.foldRight((x, y) -> if (memberOf(x, ys)) y else insert(x, y), empty(), s)

    ///
    /// Returns all subsets of `xs`.
    ///
    def subsets[a](xs: Set[a]): Set[Set[a]] =
        let Set(s) = xs;
            Set(List.map(x -> Set(x), List.subsequences(s)))

    //
    // ## Set Transformations
    //

    ///
    /// Returns the set of all elements of `xs` that satisfy the predicate `f`.
    ///
    def filter[a](f: a -> Bool, xs: Set[a]): Set[a] =
        let Set(s) = xs;
            Set(List.filter(f, s))

    ///
    /// Returns the result of applying `f` to every element in `xs`.
    /// Note: The returned set may be smaller than `xs`.
    ///
    def map[a, b](f: a -> b, xs: Set[a]): Set[b] = match xs with {
        case Set(Nil) => Set(Nil)
        case Set(x :: rs) => insert(f(x), map(f, Set(rs)))
    }

    ///
    /// Returns the result of applying `f` to every element in `xs` and taking the union.
    ///
    def flatMap[a,b](f: a -> Set[b], xs: Set[a]): Set[b] = match xs with {
        case Set(Nil) => Set(Nil)
        case Set(x :: rs) => union(f(x), flatMap(f, Set(rs)))
    }

    ///
    /// Returns a pair of sets `(ys, zs)`.
    /// `ys` contains all elements of `xs` that satisfy the predicate `f`.
    /// `zs` contains all elements of `xs` that do not satisfy the predicate `f`.
    ///
    def partition[a](f: a -> Bool, xs: Set[a]): (Set[a], Set[a]) =
        let Set(s) = xs;
        let (ys, zs) = List.partition(f, s);
            (Set(ys), Set(zs))

    //
    // ## Set Equality
    //
    def eq[a](xs: Set[a], ys: Set[a]): Bool = isSubsetOf(xs, ys) && size(xs) == size(ys)

// TODO
//## Set Conversions
//- **toList(xs: Set[A]): List[A]**
//    - Alias for toAscList.
//- **toAscList(xs: Set[A]): List[A]**
//    - Returns the set as a list in ascending order (w.r.t. its natural order).
//- **toDescList(xs: Set[A]): List[A]**
//    - Returns the set as a list in descending order (w.r.t. its natural order).
//- **toMap(xs: Set[(A, B)]): Map[A, B]**
//    - Returns the set of pairs as a map. If the set contains the same mapping multiple times the one with the greatest key (w.r.t. the natural order) takes precedence. Functionally equivalent to List.toMap(Set.toList(xs)).
//
//## Order and Lattice Operations
}
