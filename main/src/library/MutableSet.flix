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
/// The Set type.
///
/// A set is currently represented internally as a list.
///
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

    /////////////////////////////////////////////////////////////////////////////
    // Lints: Simplifications                                                  //
    /////////////////////////////////////////////////////////////////////////////


    /////////////////////////////////////////////////////////////////////////////
    // Lints: Use Cases                                                        //
    /////////////////////////////////////////////////////////////////////////////


}

