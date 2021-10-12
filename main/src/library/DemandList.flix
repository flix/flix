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
            filterE(f as a -> Bool & Impure, l as DemandList[a, Forced])  as DemandList[a, not ef or d] & ef

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
    def filterE(f: a -> Bool & Impure, l: DemandList[a, Forced]): DemandList[a, Forced] & Impure =
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
            mapE(f as a -> b & Impure, l as DemandList[a, Forced])  as DemandList[b, not ef or d] & ef

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
            flatMapE(f as a -> DemandList[b, Forced] & Impure, l as DemandList[a, Forced]) as DemandList[b, not ef or d] & ef

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
    def flatMapE(f: a -> DemandList[b, Forced] & Impure, l: DemandList[a, Forced]): DemandList[b, Forced] & Impure =
        flatMapEHelper(f, l, ks -> ks)

    ///
    /// Helper function for `flatMapE`.
    ///
    def flatMapEHelper(f: a -> DemandList[b, Forced] & Impure, l: DemandList[a, Forced], k: DemandList[b, Forced] -> DemandList[b, Forced]): DemandList[b, Forced] & Impure = match l {
        case ENil         => k(ENil)
        case ECons(x, xs) =>
            let xs1 = f(x);
            flatMapEHelper(f,       xs, ks -> k(append(xs1, ks)))
        case LCons(x, xs) =>
            let xs1 = f(x);
            flatMapEHelper(f, force xs, ks -> k(append(xs1, ks)))
        case LList(xs)    => flatMapEHelper(f, force xs, k)
    }

    ///
    /// Returns `true` if and only if at least one element in `l` satisfies the predicate `f`.
    ///
    /// Returns `false` if `l` is empty.
    ///
    pub def exists(f: a -> Bool & ef, l: DemandList[a, Forced]): Bool & ef = match l {
        case ENil         => false
        case ECons(x, xs) => if (f(x)) true else exists(f,       xs)
        case LCons(x, xs) => if (f(x)) true else exists(f, force xs)
        case LList(xs)    => exists(f, force xs)
    }

    ///
    /// Returns `true` if and only if all elements in `l` satisfy the predicate `f`.
    ///
    /// Returns `true` if `l` is empty.
    ///
    pub def forall(f: a -> Bool & ef, l: DemandList[a, Forced]): Bool & ef = match l {
        case ENil         => true
        case ECons(x, xs) => if (f(x)) forall(f,       xs) else false
        case LCons(x, xs) => if (f(x)) forall(f, force xs) else false
        case LList(xs)    => forall(f, force xs)
    }
/*
    ///
    /// Returns a list where the element at index `i` is `(a, b)` where
    /// `a` is the element at index `i` in `l1` and `b` is the element at index `i` in `l2`.
    ///
    /// If either `l1` or `l2` becomes depleted, then no further elements are added to the resulting list.
    ///
    pub def zip(l1: DemandList[a, d], l2: DemandList[b, e]): DemandList[(a, b), d or e] = match (l1, l2) {
        case (ENil, _)                    => ENil
        case (_, ENil)                    => ENil
        case (ECons(x, xs), ECons(y, ys)) => LList(lazy LCons((x, y), lazy zipHelper(      xs,       ys)))
        case (ECons(x, xs), LCons(y, ys)) => LList(lazy LCons((x, y), lazy zipHelper(      xs, force ys)))
        case (LCons(x, xs), ECons(y, ys)) => LList(lazy LCons((x, y), lazy zipHelper(force xs,       ys)))
        case (LCons(x, xs), LCons(y, ys)) => LList(lazy LCons((x, y), lazy zipHelper(force xs, force ys)))
        case (LList(xs), LList(ys))       => LList(                   lazy zipHelper(force xs, force ys))
        case (xs, LList(ys))              => LList(                   lazy zipHelper(      xs, force ys))
        case (LList(xs), ys)              => LList(                   lazy zipHelper(force xs,       ys))
    }

    ///
    /// Helper function for `zip`.
    ///
    def zipHelper(l1: DemandList[a, d], l2: DemandList[b, e]): DemandList[(a, b), d or e] = match (l1, l2) {
        case (ENil, _)                    => ENil
        case (_, ENil)                    => ENil
        case (ECons(x, xs), ECons(y, ys)) => LCons((x, y), lazy zipHelper(      xs,       ys))
        case (ECons(x, xs), LCons(y, ys)) => LCons((x, y), lazy zipHelper(      xs, force ys))
        case (LCons(x, xs), ECons(y, ys)) => LCons((x, y), lazy zipHelper(force xs,       ys))
        case (LCons(x, xs), LCons(y, ys)) => LCons((x, y), lazy zipHelper(force xs, force ys))
        case (LList(xs), LList(ys))       => LList(lazy zipHelper(force xs, force ys))
        case (xs, LList(ys))              => LList(lazy zipHelper(      xs, force ys))
        case (LList(xs), ys)              => LList(lazy zipHelper(force xs,       ys))
    }
*/
    ///
    /// Returns the longest prefix of `l` that satisfies the predicate `f`.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    ///
    pub def takeWhile(f: a -> Bool & ef, l: DemandList[a, not ef or d]): DemandList[a, not ef or d] & ef =
        if (reify ef)
            takeWhileL(f as a -> Bool & Pure,   l as DemandList[a, false]) as DemandList[a, not ef or d] & ef
        else
            takeWhileE(f as a -> Bool & Impure, l as DemandList[a, Forced]) as DemandList[a, not ef or d] & ef

    ///
    /// Returns the longest prefix of `l` that satisfies the predicate `f`.
    ///
    /// Applies `f` lazily (i.e. the tail is not forced).
    ///
    def takeWhileL(f: a -> Bool, l: DemandList[a, false]): DemandList[a, false] =
        LList(lazy takeWhileLHelper(f, l))

    ///
    /// Helper function for `takeWhileL`.
    ///
    def takeWhileLHelper(f: a -> Bool, l: DemandList[a, false]): DemandList[a, false] = match l {
        case ENil         => ENil
        case ECons(x, xs) => if (f(x)) LCons(x, lazy takeWhileL(f,       xs)) else ENil
        case LCons(x, xs) => if (f(x)) LCons(x, lazy takeWhileL(f, force xs)) else ENil
        case LList(xs)    => LList(lazy takeWhileL(f, force xs))
    }

    ///
    /// Returns the longest prefix of `l` that satisfies the predicate `f`.
    ///
    /// Applies `f` eagerly (i.e. the entire list `l` is forced).
    ///
    def takeWhileE(f: a -> Bool & Impure, l: DemandList[a, Forced]): DemandList[a, Forced] & Impure =
        takeWhileEAcc(f, l, ks -> ks)

    ///
    /// Helper function for `takeWhileE`.
    ///
    def takeWhileEAcc(f: a -> Bool & Impure, l: DemandList[a, d], k: DemandList[a, d] -> DemandList[a, d]): DemandList[a, d] & Impure = match l {
        case ENil         => k(ENil)
        case ECons(x, xs) => if (f(x)) takeWhileEAcc(f,       xs, ks -> k(ECons(x, ks))) else k(ENil)
        case LCons(x, xs) => if (f(x)) takeWhileEAcc(f, force xs, ks -> k(ECons(x, ks))) else k(ENil)
        case LList(xs)    => takeWhileEAcc(f, force xs, k)
    }

    ///
    /// Returns `l` without the longest prefix that satisfies the predicate `f`.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    ///
    pub def dropWhile(f: a -> Bool & ef, l: DemandList[a, not ef or d]): DemandList[a, not ef or d] & ef =
        if (reify ef)
            dropWhileL(f as a -> Bool & Pure,   l as DemandList[a, false]) as DemandList[a, not ef or d] & ef
        else
            dropWhileE(f as a -> Bool & Impure, l as DemandList[a, Forced]) as DemandList[a, not ef or d] & ef

    ///
    /// Returns `l` without the longest prefix that satisfies the predicate `f`.
    ///
    /// Applies `f` lazily (i.e. the tail is not forced).
    ///
    def dropWhileL(f: a -> Bool, l: DemandList[a, false]): DemandList[a, false] =
        LList(lazy dropWhileLHelper(f, l))

    ///
    /// Helper function for `dropWhileL`.
    ///
    def dropWhileLHelper(f: a -> Bool, l: DemandList[a, false]): DemandList[a, false] = match l {
        case ENil         => ENil
        case ECons(x, xs) => if (f(x)) LList(lazy dropWhileL(f,       xs)) else l
        case LCons(x, xs) => if (f(x)) LList(lazy dropWhileL(f, force xs)) else l
        case LList(xs)    => LList(lazy dropWhileL(f, force xs))
    }

    ///
    /// Returns `l` without the longest prefix that satisfies the predicate `f`.
    ///
    /// Applies `f` eagerly (i.e. the entire list `l` is forced).
    ///
    def dropWhileE(f: a -> Bool & Impure, l: DemandList[a, Forced]): DemandList[a, Forced] & Impure = match l {
        case ENil         => ENil
        case ECons(x, xs) => if (f(x)) dropWhileE(f,       xs) else l
        case LCons(x, xs) => if (f(x)) dropWhileE(f, force xs) else l
        case LList(xs)    => dropWhileE(f, force xs)
    }
}