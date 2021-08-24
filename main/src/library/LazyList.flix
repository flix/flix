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
    
instance Foldable[LazyList] {
    pub def foldLeft( f: (b, a) -> b & ef, s: b, l: LazyList[a]): b & ef = LazyList.foldLeft( f, s, l)
    pub def foldRight(f: (a, b) -> b & ef, s: b, l: LazyList[a]): b & ef = LazyList.foldRight(f, s, l)
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
    /// Returns `l` without the first element.
    ///
    pub def tail(l: LazyList[a]): LazyList[a] = match l {
        case ENil         => ENil
        case ECons(_, xs) => xs
        case LCons(_, xs) => LList(xs)
        case LList(xs)    => LList(lazy tail(force xs))
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
    /// Reverses the list `l`.
    ///
    pub def reverse(l: LazyList[a]): LazyList[a] = reverseAcc(l, ENil)

    ///
    /// Helper function for `reverse`.
    ///
    def reverseAcc(l: LazyList[a], acc: LazyList[a]): LazyList[a] = match l {
        case ENil         => acc
        case ECons(x, xs) => LList(lazy reverseAcc(xs, ECons(x, acc)))
        case LCons(x, xs) => LList(lazy reverseAcc(force xs, ECons(x, acc)))
        case LList(xs)    => LList(lazy reverseAcc(force xs, acc))
    }

    ///
    /// Returns the result of applying `f` to every element in `l`.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    ///
    /// - If `f` is pure then it is applied lazily (i.e. the tail is not forced).
    /// - If `f` is impure then it is applied eagerly (i.e. the entire list `l` is forced).
    ///
    pub def map(f: a -> b & ef, l: LazyList[a]): LazyList[b] & ef =
        matchEff f {
            case Pure   => mapL(f as a -> b & Pure,   l)
            case Impure => mapE(f as a -> b & Impure, l)
        } as & ef

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
    pub def filter(f: a -> Bool & ef, l: LazyList[a]): LazyList[a] & ef =
        matchEff f {
            case Pure   => filterL(f as a -> Bool & Pure,   l)
            case Impure => filterE(f as a -> Bool & Impure, l)
        } as & ef

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
    }

    ///
    /// Returns `l2` appended to `l1`.
    ///
    pub def append(l1: LazyList[a], l2: LazyList[a]): LazyList[a] = match l1 {
        case ENil         => l2
        case ECons(x, xs) => LCons(x, lazy append(      xs, l2))
        case LCons(x, xs) => LCons(x, lazy append(force xs, l2))
        case LList(xs)    => LList(   lazy append(force xs, l2))
    }

    ///
    /// Applies `f` to a start value `s` and all elements in `l` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b & ef, s: b, l: LazyList[a]): b & ef = match l {
        case ENil         => s
        case ECons(x, xs) => foldLeft(f, f(s, x),       xs)
        case LCons(x, xs) => foldLeft(f, f(s, x), force xs)
        case LList(xs)    => foldLeft(f, s,       force xs)
    }

    ///
    /// Applies `f` to a start value `s` and all elements in `l` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    pub def foldRight(f: (a, b) -> b & ef, s: b, l: LazyList[a]): b & ef = foldRightAcc(f, s, l, s1 -> s1 as & ef)

    ///
    /// Helper function for `foldRight`.
    ///
    def foldRightAcc(f: (a, b) -> b & ef, s: b, l: LazyList[a], k: b -> b & ef): b & ef = match l {
        case ENil         => k(s)
        case ECons(x, xs) => foldRightAcc(f, s,       xs, s1 -> k(f(x, s1)))
        case LCons(x, xs) => foldRightAcc(f, s, force xs, s1 -> k(f(x, s1)))
        case LList(xs)    => foldRightAcc(f, s, force xs, k)
    }

    ///
    /// Returns `l` as an `Array`.
    ///
    /// Forces the entire list `l`.
    ///
    pub def toArray(l: LazyList[a]): Array[a] & Impure =
        let a = Array.new(default, length(l));
        let f = (i, y) -> { a[i] = y; i + 1 };
        foldLeft(f, 0, l);
        a

    ///
    /// Returns `l` as an `Iterator`.
    ///
    /// Does not force any elements of the list.
    ///
    pub def toIter(l: LazyList[a]): Iterator[a] & Impure =
        let cursor = ref l;
        let done = () -> match head(deref cursor) {
            case None    => true
            case Some(_) => false
        };

        let next = () -> match head(deref cursor) {
            case None    => bug!("Empty iterator")
            case Some(x) =>
                cursor := tail(deref cursor);
                x
        };

        Iterator(done, next)

    ///
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

    ///
    /// Returns the number of elements in `l` that satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def count(f: a -> Bool, l: LazyList[a]): Int32 =
        foldLeft((i, x) -> if (f(x)) i + 1 else i, 0, l)

    ///
    /// Returns `l` without the first `n` elements.
    ///
    /// Returns `ENil` if `n > length(l)`.
    /// Returns `l` if `n < 1`.
    ///
    pub def drop(n: Int32, l: LazyList[a]): LazyList[a] =
        if (n < 1)
            l
        else
            match l {
                case ENil         => l
                case ECons(_, xs) => LList(lazy drop(n - 1,       xs))
                case LCons(_, xs) => LList(lazy drop(n - 1, force xs))
                case LList(xs)    => LList(lazy drop(n,     force xs))
            }

    ///
    /// Returns the concatenation of the elements in `l`.
    ///
    pub def flatten(l: LazyList[LazyList[a]]): LazyList[a] = match l {
        case ENil          => ENil
        case ECons(xs, ys) => append(xs, flatten(      ys))
        case LCons(xs, ys) => append(xs, flatten(force ys))
        case LList(xs)     => flatten(force xs)
    }

    ///
    /// Returns `true` if and only if at least one element in `l` satisfies the predicate `f`.
    ///
    /// Returns `false` if `l` is empty.
    ///
    /// The function `f` must be pure.
    ///
    pub def exists(f: a -> Bool, l: LazyList[a]): Bool = match l {
        case ENil         => false
        case ECons(x, xs) => f(x) or exists(f, xs)
        case LCons(x, xs) => f(x) or exists(f, force xs)
        case LList(xs)    => exists(f, force xs)
    }

    ///
    /// Returns `true` if and only if all elements in `l` satisfy the predicate `f`.
    ///
    /// Returns `true` if `l` is empty.
    ///
    /// The function `f` must be pure.
    ///
    pub def forall(f: a -> Bool, l: LazyList[a]): Bool = match l {
        case ENil         => true
        case ECons(x, xs) => f(x) and forall(f, xs)
        case LCons(x, xs) => f(x) and forall(f, force xs)
        case LList(xs)    => forall(f, force xs)
    }

    ///
    /// Returns an infinite LazyList of repeating `x`s.
    ///
    pub def repeat(x: a): LazyList[a] = LCons(x, lazy repeat(x))

    ///
    /// Returns an infinite sequence of integers starting from and including `n`.
    ///
    pub def from(n: Int32): LazyList[Int32] = LCons(n, lazy from(n + 1))

    ///
    /// Returns `Some(x)` if `x` is the last element of `l`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    /// Forces the entire list `l`.
    ///
    pub def last(l: LazyList[a]): Option[a] = match l {
        case ENil         => None
        case ECons(x, xs) => if (isEmpty(      xs)) Some(x) else last(      xs)
        case LCons(x, xs) => if (isEmpty(force xs)) Some(x) else last(force xs)
        case LList(xs)    => last(force xs)
    }

    ///
    /// Returns a list where the element at index `i` is `(a, b)` where
    /// `a` is the element at index `i` in `l1` and `b` is the element at index `i` in `l2`.
    ///
    /// If either `l1` or `l2` becomes depleted, then no further elements are added to the resulting list.
    ///
    pub def zip(l1: LazyList[a], l2: LazyList[b]): LazyList[(a, b)] = match (l1, l2) {
        case (ENil, _)                    => ENil
        case (_, ENil)                    => ENil
        case (ECons(x, xs), ECons(y, ys)) => LCons((x, y), lazy zip(      xs,       ys))
        case (ECons(x, xs), LCons(y, ys)) => LCons((x, y), lazy zip(      xs, force ys))
        case (LCons(x, xs), ECons(y, ys)) => LCons((x, y), lazy zip(force xs,       ys))
        case (LCons(x, xs), LCons(y, ys)) => LCons((x, y), lazy zip(force xs, force ys))
        case (LList(xs), LList(ys))       => LList(lazy zip(force xs, force ys))
        case (xs, LList(ys))              => LList(lazy zip(      xs, force ys))
        case (LList(xs), ys)              => LList(lazy zip(force xs,       ys))
    }

    ///
    /// Returns a list where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `l1` and `b` is the element at index `i` in `l2`.
    ///
    /// If either `l1` or `l2` becomes depleted, then no further elements are added to the resulting list.
    ///
    pub def zipWith(f: (a, b) -> c & ef, l1: LazyList[a], l2: LazyList[b]): LazyList[c] & Impure =
        matchEff f {
            case Pure   => zipWithL(f as (a, b) -> c & Pure,   l1, l2)
            case Impure => zipWithE(f as (a, b) -> c & Impure, l1, l2)
        }

    ///
    /// Helper function for `zipWith`. Applies `f` lazily.
    ///
    def zipWithL(f: (a, b) -> c, l1: LazyList[a], l2: LazyList[b]): LazyList[c] = match (l1, l2) {
        case (ENil, _)                    => ENil
        case (_, ENil)                    => ENil
        case (ECons(x, xs), ECons(y, ys)) => LCons(f(x, y), lazy zipWithL(f,       xs,       ys))
        case (ECons(x, xs), LCons(y, ys)) => LCons(f(x, y), lazy zipWithL(f,       xs, force ys))
        case (LCons(x, xs), ECons(y, ys)) => LCons(f(x, y), lazy zipWithL(f, force xs,       ys))
        case (LCons(x, xs), LCons(y, ys)) => LCons(f(x, y), lazy zipWithL(f, force xs, force ys))
        case (LList(xs), LList(ys))       => LList(lazy zipWithL(f, force xs, force ys))
        case (xs, LList(ys))              => LList(lazy zipWithL(f,       xs, force ys))
        case (LList(xs), ys)              => LList(lazy zipWithL(f, force xs,       ys))
    }

    ///
    /// Helper function for `zipWith`. Applies `f` eagerly.
    ///
    def zipWithE(f: (a, b) -> c & Impure, l1: LazyList[a], l2: LazyList[b]): LazyList[c] & Impure = match (l1, l2) {
        case (ENil, _)                    => ENil
        case (_, ENil)                    => ENil
        case (ECons(x, xs), ECons(y, ys)) => ECons(f(x, y), zipWithE(f,       xs,       ys))
        case (ECons(x, xs), LCons(y, ys)) => ECons(f(x, y), zipWithE(f,       xs, force ys))
        case (LCons(x, xs), ECons(y, ys)) => ECons(f(x, y), zipWithE(f, force xs,       ys))
        case (LCons(x, xs), LCons(y, ys)) => ECons(f(x, y), zipWithE(f, force xs, force ys))
        case (LList(xs), LList(ys))       => zipWithE(f, force xs, force ys)
        case (xs, LList(ys))              => zipWithE(f,       xs, force ys)
        case (LList(xs), ys)              => zipWithE(f, force xs,       ys)
    }

}