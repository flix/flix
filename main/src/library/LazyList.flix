/*
 * Copyright 2021 Jakob Schneider Villumsen
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

instance Eq[LazyList[a]] with Eq[a] {
    pub def eq(l1: LazyList[a], l2: LazyList[a]): Bool = match (l1, l2) {
        case (ENil, ENil)                 => true
        case (ECons(x, xs), ECons(y, ys)) => if (x != y) false else xs == ys
        case (ECons(x, xs), LCons(y, ys)) => if (x != y) false else xs == force ys
        case (LCons(x, xs), ECons(y, ys)) => if (x != y) false else (force xs) == ys
        case (LCons(x, xs), LCons(y, ys)) => if (x != y) false else (force xs) == force ys
        case (LList(xs), LList(ys))       => (force xs) == force ys
        case (l, LList(ys))               => l == force ys
        case (LList(xs), l)               => (force xs) == l
        case _                            => false
    }
}

namespace LazyList {
    // TODO: Implement functions used in TestExamples

    ///
    /// Returns an empty LazyList.
    ///
    pub def empty(): LazyList[a] = LList(lazy ENil)

    ///
    /// Returns true if and only if `l` is the empty LazyList, i.e. `ENil`.
    ///
    /// Does not force the tail of `l`.
    ///
    pub def isEmpty(l: LazyList[a]): Bool = match l {
        case ENil      => true
        case LList(xs) => isEmpty(force xs)
        case _         => false
    }

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
    /// Returns `Some(x)` if `x` is the first element of `l`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    /// Does not force the tail of `l`.
    ///
    pub def head(l: LazyList[a]): Option[a] = match l {
        case ENil        => None
        case ECons(x, _) => Some(x)
        case LCons(x, _) => Some(x)
        case LList(xs)   => head(force xs)
    }

    ///
    /// Returns the first `n` elements of `l`.
    ///
    /// Does not force the tail of `l`.
    ///
    pub def take(n: Int32, l: LazyList[a]): LazyList[a] =
        if (n <= 0)
            empty()
        else
            match l {
                case ENil         => ENil
                case ECons(x, xs) => ECons(x, take(n - 1, xs))
                case LCons(x, xs) => LCons(x, lazy take(n - 1, force xs))
                case LList(xs)    => LList(lazy take(n, force xs))
            }

    ///
    /// Returns the length of `l`.
    ///
    /// Forces the entire list `l`.
    ///
    pub def length(l: LazyList[a]): Int32 = lengthAcc(l, 0)

    ///
    /// Helper function for `length`
    ///
    def lengthAcc(l: LazyList[a], acc: Int32): Int32 = match l {
        case ENil         => acc
        case ECons(_, xs) => lengthAcc(xs, acc + 1)
        case LCons(_, xs) => lengthAcc(force xs, acc + 1)
        case LList(xs)    => lengthAcc(force xs, acc)
    }

    ///
    /// Returns the elements of `l` in a lazy list.
    ///
    // TODO: Move to List.
    pub def toLazy(l: List[a]): LazyList[a] = match l {
        case Nil     => ENil
        case x :: xs => LCons(x, lazy toLazy(xs))
    }

    ///
    /// Reverses the list `l`.
    ///
    pub def reverse(l: LazyList[a]): LazyList[a] = reverseAcc(l, ENil)

    ///
    /// Helper function for `reverse`.
    ///
    def reverseAcc(l: LazyList[a], acc: LazyList[a]): LazyList[a] = match l {
        case ENil         => acc
        case ECons(x, xs) => reverseAcc(xs, ECons(x, acc))
        case LCons(x, xs) => reverseAcc(force xs, ECons(x, acc))
        case LList(xs)    => reverseAcc(force xs, acc)
    }

    ///
    /// Returns the result of applying `f` to every element in `l`.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    ///
    /// - If `f` is pure then it is applied lazily (i.e. the tail is not forced).
    /// - If `f` is impure then it is applied eagerly (i.e. the entire list `l` is forced).
    ///
    pub def map(f: a -> b & ef, l: LazyList[a]): LazyList[b] & Impure =
        matchEff f {
            case Pure   => mapL(f as a -> b & Pure,   l)
            case Impure => mapE(f as a -> b & Impure, l)
        }

    ///
    /// Returns the result of applying `f` to every element in `l`.
    ///
    /// Applies `f` lazily (i.e. the tail is not forced).
    ///
    def mapL(f: a -> b, l: LazyList[a]): LazyList[b] = match l {
        case ENil         => ENil
        case ECons(x, xs) => LList(lazy LCons(f(x), lazy mapL(f, xs)))
        case LCons(x, xs) => LList(lazy LCons(f(x), lazy mapL(f, force xs)))
        case LList(xs)    => LList(lazy mapL(f, force xs))
    }

    ///
    /// Returns the result of applying `f` to every element in `l`.
    ///
    /// Applies `f` eagerly (i.e. the entire list `l` is forced).
    ///
    def mapE(f: a -> b & Impure, l: LazyList[a]): LazyList[b] & Impure = mapEAcc(f, l, ENil) |> reverse

    ///
    /// Helper function for `mapE`.
    ///
    def mapEAcc(f: a -> b & Impure, l: LazyList[a], acc: LazyList[b]): LazyList[b] & Impure = match l {
        case ENil         => acc
        case ECons(x, xs) => mapEAcc(f, xs, ECons(f(x), acc))
        case LCons(x, xs) => mapEAcc(f, force xs, ECons(f(x), acc))
        case LList(xs)    => mapEAcc(f, force xs, acc)
    }

    ///
    /// Returns a lazy list of every element in `l` that satisfies the predicate `f`.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    ///
    /// - If `f` is pure then it is applied lazily (i.e. the tail is not forced).
    /// - If `f` is impure then it is applied eagerly (i.e. the entire list `l` is forced).
    ///
    pub def filter(f: a -> Bool & ef, l: LazyList[a]): LazyList[a] & Impure =
        matchEff f {
            case Pure   => filterL(f as a -> Bool & Pure,   l)
            case Impure => filterE(f as a -> Bool & Impure, l)
        }

    ///
    /// Returns a lazy list of every element in `l` that satisfies the predicate `f`.
    ///
    /// Applies `f` lazily (i.e. the tail is not forced).
    ///
    def filterL(f: a -> Bool, l: LazyList[a]): LazyList[a] = match l {
        case ENil                 => ENil
        case ECons(x, xs) if f(x) => LCons(x, lazy filterL(f, xs))
        case LCons(x, xs) if f(x) => LCons(x, lazy filterL(f, force xs))
        case ECons(_, xs)         => LList(lazy filterL(f, xs))
        case LCons(_, xs)         => LList(lazy filterL(f, force xs))
        case LList(xs)            => LList(lazy filterL(f, force xs))
    }

    ///
    /// Returns a lazy list of every element in `l` that satisfies the predicate `f`.
    ///
    /// Applies `f` eagerly (i.e. the entire list `l` is forced).
    ///
    def filterE(f: a -> Bool & Impure, l: LazyList[a]): LazyList[a] & Impure =
        filterEAcc(f, l, ENil) |> reverse

    ///
    /// Helper function for `filterE`
    ///
    def filterEAcc(f: a -> Bool & Impure, l: LazyList[a], acc: LazyList[a]): LazyList[a] & Impure = match l {
        case ENil         => acc
        case ECons(x, xs) => if (f(x)) filterEAcc(f, xs, ECons(x, acc))       else filterEAcc(f, xs, acc)
        case LCons(x, xs) => if (f(x)) filterEAcc(f, force xs, ECons(x, acc)) else filterEAcc(f, force xs, acc)
        case LList(xs)    => filterEAcc(f, force xs, acc)
    /// Returns `l` as a `List`.
    ///
    /// Forces the entire list `l`.
    ///
    pub def toList(l: LazyList[a]): List[a] = toListAcc(l, Nil) |> List.reverse

    ///
    /// Helper function for `toList`.
    ///
    def toListAcc(l: LazyList[a], acc: List[a]): List[a] = match l {
        case ENil         => acc
        case ECons(x, ys) => toListAcc(ys, x :: acc)
        case LCons(x, ys) => toListAcc(force ys, x :: acc)
        case LList(ys)    => toListAcc(force ys, acc)
    }

    ///
    /// Returns the association list `l` as a map.
    ///
    /// If `l` contains multiple mappings with the same key, `toMap` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    /// Forces the entire list `l`.
    ///
    pub def toMap(l: LazyList[(a, b)]): Map[a, b] with Order[a] = toMapAcc(l, Map.empty())

    ///
    /// Helper functions for `toMap`.
    ///
    def toMapAcc(l: LazyList[(a, b)], acc: Map[a, b]): Map[a, b] with Order[a] = match l {
        case ENil              => acc
        case ECons((k, v), xs) => toMapAcc(xs, Map.insert(k, v, acc))
        case LCons((k, v), xs) => toMapAcc(force xs, Map.insert(k, v, acc))
        case LList(xs)         => toMapAcc(force xs, acc)
    }

    ///
    /// Returns `l` as a `Set`.
    ///
    /// Forces the entire list `l`.
    ///
    pub def toSet(l: LazyList[a]): Set[a] with Order[a] = toSetAcc(l, Set.empty())

    ///
    /// Helper functions for `toSet`.
    ///
    def toSetAcc(l: LazyList[a], acc: Set[a]): Set[a] with Order[a] = match l {
        case ENil         => acc
        case ECons(x, xs) => toSetAcc(xs, Set.insert(x, acc))
        case LCons(x, xs) => toSetAcc(force xs, Set.insert(x, acc))
        case LList(xs)    => toSetAcc(force xs, acc)
    }
}