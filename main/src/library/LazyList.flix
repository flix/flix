/*
 * Copyright 2021 Esben Bjerre, Jakob Schneider Villumsen
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

pub enum LazyList[a] {
    case ENil,
    case ECons(a, LazyList[a]),
    case LCons(a, Lazy[LazyList[a]]),
    case LList(Lazy[LazyList[a]])
}

namespace LazyList {

    ///
    /// Returns an empty LazyList.
    ///
    pub def empty(): LazyList[a] = LList(lazy ENil)

    ///
    /// Alias for `empty`.
    ///
    /// Returns an empty LazyList.
    ///
    pub def new(): LazyList[a] = empty()

    ///
    /// Returns an infinite LazyList of repeating `x`s.
    ///
    pub def repeat(x: a): LazyList[a] = LList(lazy LCons(x, lazy repeat(x)))

    ///
    /// Returns an infinite sequence of integers starting from and including `n`.
    ///
    pub def from(n: Int32): LazyList[Int32] = LList(lazy LCons(n, lazy from(n + 1)))

    ///
    /// Returns a lazy list of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns an empty lazy list if `b >= e`.
    ///
    pub def range(b: Int32, e: Int32): LazyList[Int32] =
        if (b >= e)
            empty()
        else
            LList(lazy LCons(b, lazy range(b + 1, e)))

    ///
    /// Returns `Some(x)` if `x` is the first element of `xs`.
    ///
    /// Returns `None` if `xs` is empty.
    ///
    /// Does not force the tail of the list `xs`.
    ///
    pub def head(xs: LazyList[a]): Option[a] = match xs {
        case ENil        => None
        case ECons(x, _) => Some(x)
        case LCons(x, _) => Some(x)
        case LList(ys)   => head(force ys)
    }

    ///
    /// Returns true if and only if `xs` is the empty LazyList, i.e. `ENil`.
    ///
    /// Does not force the tail of the list `xs`.
    ///
    pub def isEmpty(xs: LazyList[a]): Bool = match xs {
        case ENil => true
        case _    => false
    }

    ///
    /// Returns the first `n` elements of `xs`.
    ///
    /// Forces the first `n` elements of `xs`.
    ///
    pub def take(n: Int32, xs: LazyList[a]): LazyList[a] =
        if (n <= 0)
            empty()
        else
            match xs {
                case ENil                     => ENil
                case ECons(x, _)  if (n == 1) => LCons(x, lazy ENil)
                case ECons(x, ys)             => LCons(x, lazy take(n - 1, ys))
                case LCons(x, _)  if (n == 1) => LCons(x, lazy ENil)
                case LCons(x, ys)             => LCons(x, lazy take(n - 1, force ys))
                case LList(ys)                => LList(lazy take(n, force ys))
            }

    ///
    /// Returns a lazy list of every element in `xs` that satisfies the predicate `f`.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    ///
    /// - If `f` is pure then it is applied lazily (i.e. the tail is not forced).
    /// - If `f` is impure then it is applied eagerly (i.e. the entire list `xs` is forced).
    ///
    pub def filter(f: a -> Bool & ef, xs: LazyList[a]): LazyList[a] & Impure =
        matchEff f {
            case Pure   => filterL(f as a -> Bool & Pure,   xs)
            case Impure => filterE(f as a -> Bool & Impure, xs)
        }

    ///
    /// Returns a lazy list of every element in `xs` that satisfies the predicate `f`.
    ///
    /// Applies `f` lazily (i.e. the tail is not forced).
    ///
    def filterL(f: a -> Bool, xs: LazyList[a]): LazyList[a] = match xs {
        case ENil                   => ENil
        case ECons(x, ys) if (f(x)) => LCons(x, lazy filterL(f, ys))
        case LCons(x, ys) if (f(x)) => LCons(x, lazy filterL(f, force ys))
        case ECons(_, ys)           => LList(lazy filterL(f, ys))
        case LCons(_, ys)           => LList(lazy filterL(f, force ys))
        case LList(ys)              => LList(lazy filterL(f, force ys))
    }

    ///
    /// Returns a lazy list of every element in `xs` that satisfies the predicate `f`.
    ///
    /// Applies `f` eagerly (i.e. the entire list `xs` is forced).
    ///
    def filterE(f: a -> Bool & Impure, xs: LazyList[a]): LazyList[a] & Impure =
        filterEAcc(f, xs, Nil) |> List.reverse |> toLazy

    ///
    /// Helper function for `filterE`
    ///
    def filterEAcc(f: a -> Bool & Impure, xs: LazyList[a], acc: List[a]): List[a] & Impure = match xs {
        case ENil         => acc
        case ECons(x, ys) => if (f(x)) filterEAcc(f, ys, x :: acc)       else filterEAcc(f, ys, acc)
        case LCons(x, ys) => if (f(x)) filterEAcc(f, force ys, x :: acc) else filterEAcc(f, force ys, acc)
        case LList(ys)    => filterEAcc(f, force ys, acc)
    }

    ///
    /// Returns the result of applying `f` to every element in `xs`.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    ///
    /// - If `f` is pure then it is applied lazily (i.e. the tail is not forced).
    /// - If `f` is impure then it is applied eagerly (i.e. the entire list `xs` is forced).
    ///
    pub def map(f: a -> b & ef, xs: LazyList[a]): LazyList[b] & Impure =
        matchEff f {
            case Pure   => mapL(f as a -> b & Pure,   xs)
            case Impure => mapE(f as a -> b & Impure, xs)
        }

    ///
    /// Returns the result of applying `f` to every element in `xs`.
    ///
    /// Applies `f` lazily (i.e. the tail is not forced).
    ///
    def mapL(f: a -> b, xs: LazyList[a]): LazyList[b] = match xs {
        case ENil         => ENil
        case ECons(x, ys) => LCons(f(x), lazy mapL(f, ys))
        case LCons(x, ys) => LCons(f(x), lazy mapL(f, force ys))
        case LList(ys)    => LList(lazy mapL(f, force ys))
    }

    ///
    /// Returns the result of applying `f` to every element in `xs`.
    ///
    /// Applies `f` eagerly (i.e. the entire list `xs` is forced).
    ///
    def mapE(f: a -> b & Impure, xs: LazyList[a]): LazyList[b] & Impure = toList(xs) |> List.map(f) |> toLazy

    ///
    /// Returns the elements of `l` in a lazy list.
    ///
    // TODO: Move to List.
    def toLazy(l: List[a]): LazyList[a] = match l {
        case Nil     => ENil
        case x :: xs => LCons(x, lazy toLazy(xs))
    }

    ///
    /// Returns `xs` as an `Array`.
    ///
    /// Forces the entire list `xs`.
    ///
    pub def toArray(xs: LazyList[a]): Array[a] & Impure = ???

    ///
    /// Returns `xs` as an `Array`.
    ///
    /// Does not force any elements of the list.
    ///
    pub def toIter(xs: LazyList[a]): Iterator[a] & Impure = ???

    ///
    /// Returns `xs` as a `List`.
    ///
    /// Forces the entire list `xs`.
    ///
    pub def toList(xs: LazyList[a]): List[a] = toListAcc(xs, Nil) |> List.reverse

    ///
    /// Helper function for `toList`.
    ///
    def toListAcc(xs: LazyList[a], acc: List[a]): List[a] = match xs {
        case ENil         => acc
        case ECons(x, ys) => toListAcc(ys, x :: acc)
        case LCons(x, ys) => toListAcc(force ys, x :: acc)
        case LList(ys)    => toListAcc(force ys, acc)
    }

    ///
    /// Returns `xs` as a `Set`.
    ///
    /// Forces the entire list `xs`.
    ///
    pub def toSet(xs: LazyList[a]): Set[a] = ???

    ///
    /// Returns `xs` as a `Map`.
    ///
    /// Forces the entire list `xs`.
    ///
    pub def toMap(xs: LazyList[(a, b)]): Map[a, b] = ???

}