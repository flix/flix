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

pub enum Step[a, s] {
    case Done,
    case Skip(s),
    case Yield(a, s)
}

pub enum Stream[a, s] {
    case Stream(s -> Step[a, s], s)
}

namespace Stream {

    ///
    /// Returns an empty stream.
    ///
    pub def empty(): Stream[a, s] =
        Stream(_ -> Done, default)

    ///
    /// Returns true if and only if the step function of `s` evaluates to `Done`.
    ///
    pub def isEmpty(s: Stream[a, s]): Bool =
        let Stream(next0, s0) = s;
        match next0(s0) {
            case Done => true
            case _    => false
        }

    ///
    /// Returns the length of `s`.
    ///
    // TODO: Add tests
    pub def length(s: Stream[a, s]): Int32 =
        let Stream(next0, s0) = s;
        lengthAcc(next0, s0, 0)

    pub def lengthAcc(next0: s -> Step[a, s], s0: s, acc: Int32): Int32 =
        match next0(s0) {
            case Done           => acc
            case Skip(s1)       => lengthAcc(next0, s1, acc)
            case Yield(_, s1)   => lengthAcc(next0, s1, acc + 1)
        }

    ///
    /// Returns `s` as a `List`.
    ///
    pub def toList(s: Stream[a, s]): List[a] =
        let Stream(next0, s0) = s;
        toListAcc(next0, s0, ks -> ks)

    ///
    /// Helper function for `toList`.
    ///
    def toListAcc(next: s -> Step[a, s], s0: s, k: List[a] -> List[a]): List[a] = match next(s0) {
        case Done         => k(Nil)
        case Skip(s1)     => toListAcc(next, s1, k)
        case Yield(x, s1) => toListAcc(next, s1, ks -> k(x :: ks))
    }

    ///
    /// Returns a stream of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    pub def range(b: Int32, e: Int32): Stream[Int32, Int32] =
        let next = x -> if (x >= e) Done else Yield(x, x + 1);
        Stream(next, b)

    ///
    /// Returns a stream of every element in `s` that satisfies the predicate `f`.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    ///
    /// - If `f` is pure then it is applied lazily.
    /// - If `f` is impure then it is applied eagerly.
    ///
    pub def filter(f: a -> Bool & ef, s: Stream[a, s]): Stream[a, s] & ef =
        if (reify ef)
            filterL(f as a -> Bool & Pure,   s)
        else
            filterE(f as a -> Bool & Impure, s) as & ef

    ///
    /// Returns a stream of every element in `s` that satisfies the predicate `f`.
    ///
    /// Applies `f` lazily.
    ///
    def filterL(f: a -> Bool, s: Stream[a, s]): Stream[a, s] =
        let Stream(next0, s0) = s;
        let next1 = s1 -> match next0(s1) {
            case Done         => Done
            case Skip(s2)     => Skip(s2)
            case Yield(x, s2) => if (f(x)) Yield(x, s2) else Skip(s2)
        };
        Stream(next1, s0)

    ///
    /// Returns a stream of every element in `s` that satisfies the predicate `f`.
    ///
    /// Applies `f` eagerly.
    ///
    def filterE(f: a -> Bool & Impure, s: Stream[a, s]): Stream[a, s] & Impure =
        let Stream(next, s1) = s;
        (filterEAcc(f, next, s1, ks -> ks) |> List.toStream) as Stream[a, s]

    ///
    /// Helper function for `filterE`.
    ///
    def filterEAcc(f: a -> Bool & Impure, next: s -> Step[a, s], s0: s, k: List[a] -> List[a]): List[a] & Impure = match next(s0) {
        case Done         => k(Nil)
        case Skip(s1)     => filterEAcc(f, next, s1, k)
        case Yield(x, s1) => if (f(x)) filterEAcc(f, next, s1, ks -> k(x :: ks)) else filterEAcc(f, next, s1, k)
    }

    ///
    /// Returns the result of applying `f` to every element in `s`.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    ///
    /// - If `f` is pure then it is applied lazily.
    /// - If `f` is impure then it is applied eagerly.
    ///
    pub def map(f: a -> b & ef, s: Stream[a, s]): Stream[b, s] & ef =
        if (reify ef)
             mapL(f as a -> b & Pure,   s)
        else
             mapE(f as a -> b & Impure, s) as & ef

    ///
    /// Returns the result of applying `f` to every element in `s`.
    ///
    /// Applies `f` lazily.
    ///
    def mapL(f: a -> b, s: Stream[a, s]): Stream[b, s] =
        let Stream(next0, s0) = s;
        let next1 = s1 -> match next0(s1) {
            case Done         => Done
            case Skip(s2)     => Skip(s2)
            case Yield(x, s2) => Yield(f(x), s2)
        };
        Stream(next1, s0)

    ///
    /// Returns the result of applying `f` to every element in `s`.
    ///
    /// Applies `f` eagerly.
    ///
    def mapE(f: a -> b & Impure, s: Stream[a, s]): Stream[b, s] & Impure =
        let Stream(next, s0) = s;
        (mapEAcc(f, next, s0, ks -> ks) |> List.toStream) as Stream[b, s]

    ///
    /// Helper function for `mapE`.
    ///
    def mapEAcc(f: a -> b & Impure, next: s -> Step[a, s], s0: s, k: List[b] -> List[b]): List[b] & Impure = match next(s0) {
        case Done         => k(Nil)
        case Skip(s1)     => mapEAcc(f, next, s1, k)
        case Yield(x, s1) =>
            let x1 = f(x);
            mapEAcc(f, next, s1, ks -> k(x1 :: ks))
    }

    ///
    /// Applies `f` to a start value `s` and all elements in `st` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b & ef, s: b, st: Stream[a, state]): b & ef =
        let Stream(next, st0) = st;
        match next(st0) {
            case Done          => s
            case Skip(st1)     => foldLeft(f, s,       Stream(next, st1))
            case Yield(x, st1) => foldLeft(f, f(s, x), Stream(next, st1))
        }

    ///
    /// Applies `f` to a start value `s` and all elements in `l` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    pub def foldRight(f: (a, b) -> b & ef, s: b, st: Stream[a, st]): b & ef =
        foldRightAcc(f, s, st, ks -> ks as & ef)

    ///
    /// Helper function for `foldRight`.
    ///
    def foldRightAcc(f: (a, b) -> b & ef, s: b, st: Stream[a, st], k: b -> b & ef): b & ef =
        let Stream(next, st0) = st;
        match next(st0) {
            case Done          => k(s)
            case Skip(st1)     => foldRightAcc(f, s, Stream(next, st1), k)
            case Yield(x, st1) => foldRightAcc(f, s, Stream(next, st1), ks -> k(f(x, ks)))
        }

}
