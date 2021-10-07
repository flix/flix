/*
 * Copyright 2021 Magnus Madsen, Jakob Schneider Villumsen
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


namespace DemandList {

    ///
    /// Represents a lazy list.
    ///
    /// If the Boolean type `f` is equiv. to true then all elements are forced.
    ///
    pub enum DemandList[a: Type, f: Bool] {
        case ENil,
        case ECons(a, DemandList[a, f]),
        case LCons(a, Lazy[DemandList[a, f]]),
        case LList(Lazy[DemandList[a, f]])
    }

    type alias Forced = true

    ///
    /// Returns an empty `DemandList`.
    ///
    pub def empty(): DemandList[a, d] = ENil

    ///
    /// Returns `Some(x)` if `x` is the first element of `l`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    /// Does not force the tail of `l`.
    ///
    pub def head(l: DemandList[a, _]): Option[a] = match l {
        case ENil        => None
        case ECons(x, _) => Some(x)
        case LCons(x, _) => Some(x)
        case LList(xs)   => head(force xs)
    }

    ///
    /// Returns a lazy list of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns an empty lazy list if `b >= e`.
    ///
    pub def range(b: Int32, e: Int32): DemandList[Int32, _] =
        LList(lazy rangeHelper(b, e))

    ///
    /// Helper function for `range`.
    ///
    def rangeHelper(b: Int32, e:Int32): DemandList[Int32, _] =
        if (b >= e)
            ENil
        else
            LCons(b, lazy rangeHelper(b + 1, e))

    ///
    /// Returns the length of `l`.
    ///
    /// Forces the entire list `l`.
    ///
    pub def length(l: DemandList[a, Forced]): Int32 =
        lengthAcc(l, 0)

    ///
    /// Helper function for `length`
    ///
    def lengthAcc(l: DemandList[a, Forced], acc: Int32): Int32 = match l {
        case ENil         => acc
        case ECons(_, xs) => lengthAcc(      xs, acc + 1)
        case LCons(_, xs) => lengthAcc(force xs, acc + 1)
        case LList(xs)    => lengthAcc(force xs, acc)
    }

    ///
    /// Returns the sum of all elements in the list `l`.
    ///
    pub def sum(l: DemandList[Int32, Forced]): Int32 =
        sumAcc(l, 0)

    ///
    /// Helper function for `sum`.
    ///
    def sumAcc(l: DemandList[Int32, Forced], acc: Int32): Int32 = match l {
        case ENil         => acc
        case ECons(x, xs) => sumAcc(      xs, x + acc)
        case LCons(x, xs) => sumAcc(force xs, x + acc)
        case LList(xs)    => sumAcc(force xs,     acc)
    }

    ///
    /// Returns a lazy list of every element in `l` that satisfies the predicate `f`.
    ///
    pub def filter(f: a -> Bool & ef, l: DemandList[a, not ef or d]): DemandList[a, not ef or d] & ef =
        if (reify ef)
            filterL(f as a -> Bool & Pure,   l as DemandList[a, false]) as DemandList[a, not ef or d] & ef
        else
            filterE(f as a -> Bool & Impure, l as DemandList[a, true])  as DemandList[a, not ef or d] & ef

    ///
    /// Returns a lazy list of every element in `l` that satisfies the predicate `f`.
    ///
    /// Applies `f` lazily (i.e. the tail is not forced).
    ///
    def filterL(f: a -> Bool, l: DemandList[a, false]): DemandList[a, false] = match l {
        case ENil                 => ENil
        case ECons(x, xs) if f(x) => LCons(x, lazy filterL(f,       xs))
        case LCons(x, xs) if f(x) => LCons(x, lazy filterL(f, force xs))
        case ECons(_, xs)         => LList(   lazy filterL(f,       xs))
        case LCons(_, xs)         => LList(   lazy filterL(f, force xs))
        case LList(xs)            => LList(   lazy filterL(f, force xs))
    }

    ///
    /// Returns a lazy list of every element in `l` that satisfies the predicate `f`.
    ///
    /// Applies `f` eagerly (i.e. the entire list `l` is forced).
    ///
    def filterE(f: a -> Bool & Impure, l: DemandList[a, true]): DemandList[a, true] & Impure =
        filterEAcc(f, l as DemandList[a, Forced], ENil) |> reverse

    ///
    /// Helper function for `filterE`
    ///
    def filterEAcc(f: a -> Bool & Impure, l: DemandList[a, f], acc: DemandList[a, f]): DemandList[a, f] & Impure = match l {
        case ENil         => acc
        case ECons(x, xs) => if (f(x)) filterEAcc(f,       xs, ECons(x, acc)) else filterEAcc(f,       xs, acc)
        case LCons(x, xs) => if (f(x)) filterEAcc(f, force xs, ECons(x, acc)) else filterEAcc(f, force xs, acc)
        case LList(xs)    => filterEAcc(f, force xs, acc)
    }

    ///
    /// Reverses the list `l`.
    ///
    pub def reverse(l: DemandList[a, Forced]): DemandList[a, Forced] =
        LList(lazy reverseAcc(l, ENil))

    ///
    /// Helper function for `reverse`.
    ///
    def reverseAcc(l: DemandList[a, Forced], acc: DemandList[a, Forced]): DemandList[a, Forced] = match l {
        case ENil         => acc
        case ECons(x, xs) => reverseAcc(      xs, ECons(x, acc))
        case LCons(x, xs) => reverseAcc(force xs, ECons(x, acc))
        case LList(xs)    => reverseAcc(force xs,          acc)
    }

    ///
    /// Returns true if and only if `l` is the empty LazyList, i.e. `ENil`.
    ///
    /// Does not force the tail of `l`.
    ///
    pub def isEmpty(l: DemandList[a, d]): Bool = match l {
        case ENil      => true
        case LList(xs) => isEmpty(force xs)
        case _         => false
    }

    ///
    /// Returns the result of applying `f` to every element in `l`.
    ///
    pub def map(f: a -> b & ef, l: DemandList[a, not ef or d]): DemandList[b, not ef or d] & ef =
        if (reify ef)
            mapL(f as a -> b & Pure,   l as DemandList[a, false]) as DemandList[b, not ef or d] & ef
        else
            mapE(f as a -> b & Impure, l as DemandList[a, true])  as DemandList[b, not ef or d] & ef

    ///
    /// Returns the result of applying `f` to every element in `l`.
    ///
    /// Applies `f` lazily (i.e. the tail is not forced).
    ///
    def mapL(f: a -> b, l: DemandList[a, false]): DemandList[b, false] = match l {
        case ENil         => ENil
        case ECons(x, xs) => LList(lazy LCons(f(x), lazy mapL1(f,       xs)))
        case LCons(x, xs) => LList(lazy LCons(f(x), lazy mapL1(f, force xs)))
        case LList(xs)    => LList(                 lazy mapL1(f, force xs))
    }

    ///
    /// Helper function for `mapL`.
    ///
    def mapL1(f: a -> b, l: DemandList[a, false]): DemandList[b, false] = match l {
        case ENil         => ENil
        case ECons(x, xs) => LCons(f(x), lazy mapL1(f,       xs))
        case LCons(x, xs) => LCons(f(x), lazy mapL1(f, force xs))
        case LList(xs)    => LList(      lazy mapL1(f, force xs))
    }

    ///
    /// Returns the result of applying `f` to every element in `l`.
    ///
    /// Applies `f` eagerly (i.e. the entire list `l` is forced).
    ///
    def mapE(f: a -> b & Impure, l: DemandList[a, Forced]): DemandList[b, Forced] & Impure =
        mapEAcc(f, l, ks -> ks)

    ///
    /// Helper function for `mapE`.
    ///
    def mapEAcc(f: a -> b & Impure, l: DemandList[a, Forced], k: DemandList[b, Forced] -> DemandList[b, Forced]): DemandList[b, Forced] & Impure = match l {
        case ENil         => k(ENil)
        case ECons(x, xs) =>
            let x1 = f(x);
            mapEAcc(f, xs, ks -> k(ECons(x1, ks)))
        case LCons(x, xs) =>
            let x1 = f(x);
            mapEAcc(f, force xs, ks -> k(ECons(x1, ks)))
        case LList(xs)    => mapEAcc(f, force xs, k)
    }

    ///
    /// Returns `l` as a `List`.
    ///
    /// Forces the entire list `l`.
    ///
    pub def toList(l: DemandList[a, d]): List[a] =
        toListAcc(l, ks -> ks)

    ///
    /// Helper function for `toList`.
    ///
    def toListAcc(l: DemandList[a, d], k: List[a] -> List[a]): List[a] = match l {
        case ENil         => k(Nil)
        case ECons(x, xs) => toListAcc(      xs, ks -> k(x :: ks))
        case LCons(x, xs) => toListAcc(force xs, ks -> k(x :: ks))
        case LList(xs)    => toListAcc(force xs,       k)
    }

    ///
    /// Applies `f` to a start value `s` and all elements in `l` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b & ef, s: b, l: DemandList[a, Forced]): b & ef = match l {
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
    pub def foldRight(f: (a, b) -> b & ef, s: b, l: DemandList[a, Forced]): b & ef =
        foldRightAcc(f, s, l, ks -> ks as & ef)

    ///
    /// Helper function for `foldRight`.
    ///
    def foldRightAcc(f: (a, b) -> b & ef, s: b, l: DemandList[a, Forced], k: b -> b & ef): b & ef = match l {
        case ENil         => k(s)
        case ECons(x, xs) => foldRightAcc(f, s,       xs, ks -> k(f(x, ks)))
        case LCons(x, xs) => foldRightAcc(f, s, force xs, ks -> k(f(x, ks)))
        case LList(xs)    => foldRightAcc(f, s, force xs, k)
    }

    ///
    /// Returns the first `n` elements of `l`.
    ///
    /// Does not force the tail of `l`.
    ///
    pub def take(n: Int32, l: DemandList[a, d]): DemandList[a, d] =
        LList(lazy takeHelper(n, l))

    ///
    /// Helper function for `take`.
    ///
    def takeHelper(n: Int32, l: DemandList[a, d]): DemandList[a, d] =
        if (n <= 0)
            ENil
        else
            match l {
                case ENil         => ENil
                case ECons(x, xs) => LCons(x, lazy takeHelper(n - 1,       xs))
                case LCons(x, xs) => LCons(x, lazy takeHelper(n - 1, force xs))
                case LList(xs)    => LList(   lazy takeHelper(n,     force xs))
            }

    ///
    /// Returns `l` without the first `n` elements.
    ///
    /// Returns `ENil` if `n > length(l)`.
    /// Returns `l` if `n < 1`.
    ///
    pub def drop(n: Int32, l: DemandList[a, d]): DemandList[a, d] =
        LList(lazy dropHelper(n, l))

    ///
    /// Helper function for `drop`.
    ///
    def dropHelper(n: Int32, l: DemandList[a, d]): DemandList[a, d] =
        if (n < 1)
            l
        else
            match l {
                case ENil         => l
                case ECons(_, xs) => LList(lazy dropHelper(n - 1,       xs))
                case LCons(_, xs) => LList(lazy dropHelper(n - 1, force xs))
                case LList(xs)    => LList(lazy dropHelper(n,     force xs))
            }

    ///
    /// Returns `l2` appended to `l1`.
    ///
    pub def append(l1: DemandList[a, d], l2: DemandList[a, d]): DemandList[a, d] = match l1 {
        case ENil         => l2
        case ECons(x, xs) => LCons(x, lazy append(      xs, l2))
        case LCons(x, xs) => LCons(x, lazy append(force xs, l2))
        case LList(xs)    => LList(   lazy append(force xs, l2))
    }

    ///
    /// Returns an infinite `DemandList` of repeating `x`s.
    ///
    pub def repeat(x: a): DemandList[a, d] =
        LList(lazy repeatHelper(x))

    ///
    /// Helper function for `repeat`.
    ///
    def repeatHelper(x: a): DemandList[a, d] =
        LCons(x, lazy repeatHelper(x))

    ///
    /// Returns the result of applying `f` to every element in `l` and concatenating the results.
    ///
    pub def flatMap(f: a -> DemandList[b, false] & ef, l: DemandList[a, not ef or d]): DemandList[b, not ef or d] & ef =
        if (reify ef)
            flatMapL(f as a -> DemandList[b, false] & Pure,  l as DemandList[a, false]) as DemandList[b, not ef or d] & ef
        else
            flatMapE(f as a -> DemandList[b, true] & Impure, l as DemandList[a, true]) as DemandList[b, not ef or d] & ef

    ///
    /// Returns the result of applying `f` to every element in `l` and concatenating the results.
    ///
    /// Applies `f` lazily (i.e. the tail is not forced).
    ///
    def flatMapL(f: a -> DemandList[b, false], l: DemandList[a, false]): DemandList[b, false] = match l {
        case ENil         => ENil
        case ECons(x, xs) => LList(lazy append(f(x), LList(lazy flatMapL(f,       xs))))
        case LCons(x, xs) => LList(lazy append(f(x), LList(lazy flatMapL(f, force xs))))
        case LList(xs)    => LList(lazy flatMapL(f, force xs))
    }

    ///
    /// Returns the result of applying `f` to every element in `l` and concatenating the results.
    ///
    /// Applies `f` eagerly (i.e. the entire list `l` is forced).
    ///
    def flatMapE(f: a -> DemandList[b, true] & Impure, l: DemandList[a, Forced]): DemandList[b, Forced] & Impure =
        flatMapEHelper(f, l, ks -> ks)

    ///
    /// Helper function for `flatMapE`.
    ///
    def flatMapEHelper(f: a -> DemandList[b, true] & Impure, l: DemandList[a, true], k: DemandList[b, true] -> DemandList[b, true]): DemandList[b, true] & Impure = match l {
        case ENil         => k(ENil)
        case ECons(x, xs) =>
            let xs1 = f(x);
            flatMapEHelper(f,       xs, ks -> k(append(xs1, ks)))
        case LCons(x, xs) =>
            let xs1 = f(x);
            flatMapEHelper(f, force xs, ks -> k(append(xs1, ks)))
        case LList(xs)    => flatMapEHelper(f, force xs, k)
    }
}