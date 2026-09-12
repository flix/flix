/*
 * Copyright 2026 Flix Authors
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

pub mod Range {

    ///
    /// Represents a range (b, e) of discrete values from `b` (inclusive) to `e` (exclusive).
    ///
    enum Range[t] with Eq, ToString {
        case Range(t, t)
    }

    instance ForEach[Range[t]] with Discrete[t] {
        type Elm = t
        pub def forEach(f: t -> Unit \ ef, t: Range[t]): Unit \ ef = Range.forEach(f, t)
    }

    instance Iterable[Range[t]] with Discrete[t] {
        type Elm = t
        pub def iterator(rc: Region[r], rng: Range[t]): Iterator[t, r, r] \ r = Range.iterator(rc, rng)
    }

    ///
    /// Returns `true` if and only if the range `r` is empty, i.e. it contains no elements.
    ///
    pub def isEmpty(r: Range[t]): Bool with Order[t] =
        let Range(b, e) = r;
        not (b < e)

    ///
    /// Returns `true` if and only if `x` is a member of the range `r`.
    ///
    pub def memberOf(x: t, r: Range[t]): Bool with Order[t] =
        let Range(b, e) = r;
        b <= x and x < e

    ///
    /// Returns `true` if and only if the ranges `r1` and `r2` overlap, i.e. they share at least one element.
    ///
    pub def overlaps(r1: Range[t], r2: Range[t]): Bool with Order[t] =
        not isEmpty(intersection(r1, r2))

    ///
    /// Returns the intersection of the ranges `r1` and `r2`.
    ///
    /// The result is empty if `r1` and `r2` do not overlap.
    ///
    pub def intersection(r1: Range[t], r2: Range[t]): Range[t] with Order[t] =
        let Range(b1, e1) = r1;
        let Range(b2, e2) = r2;
        Range(Order.max(b1, b2), Order.min(e1, e2))

    ///
    /// Applies `f` to every element of `r`.
    ///
    pub def forEach(f: t -> Unit \ ef, r: Range[t]): Unit \ ef with Discrete[t] =
        let Range(b, e) = r;
        def loop(i) =
            if (i < e) {
                f(i); loop(Discrete.succ(i))
            };
        loop(b)

    ///
    /// Returns an iterator over the elements of `r`.
    ///
    pub def iterator(rc: Region[r], rng: Range[t]): Iterator[t, r, r] \ r with Discrete[t] =
        let Range(b, e) = rng;
        Iterator.unfold(rc, i -> if (i < e) Some((i, Discrete.succ(i))) else None, b)

    ///
    /// Returns the elements of `r` as a list.
    ///
    pub def toList(r: Range[t]): List[t] with Discrete[t] = region rc {
        Range.iterator(rc, r) |> Iterator.toList
    }

    ///
    /// Returns the elements of `r` as a set.
    ///
    pub def toSet(r: Range[t]): Set[t] with Discrete[t] = region rc {
        Range.iterator(rc, r) |> Iterator.toSet
    }

    ///
    /// Returns the elements of `r` as a vector.
    ///
    pub def toVector(r: Range[t]): Vector[t] with Discrete[t] = region rc {
        Range.iterator(rc, r) |> Iterator.toVector
    }

}
