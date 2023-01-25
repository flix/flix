/*
 *  Copyright 2023 Magnus Madsen, Stephen Tetley
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

///
/// A Vector data type which is fundamentally like Array but is immutable.
///

type alias Vector[a] = Array[a, true]

namespace Vector {

    ///
    /// Compares `a` and `b` lexicographically.
    ///
    pub def compare(a: Vector[a], b: Vector[a]): Comparison with Order[a] =
        let len = Int32.min(length(a), length(b));
        def loop(i) = {
            if (i < len) {
                let cmp = get(i, a) <=> get(i, b);
                if (cmp == Comparison.EqualTo)
                    loop(i + 1)
                else
                    cmp
            } else if (i < Array.length(a)) {
                Comparison.GreaterThan
            } else if (i < Array.length(b)) {
                Comparison.LessThan
            } else {
                Comparison.EqualTo
            }
        };
        loop(0)

    ///
    /// Returns a string representation of the given vector `v`.
    ///
    pub def toString(v: Vector[a]): String with ToString[a] = region rc {
        let sb = new StringBuilder(rc);
        let first = ref true @ rc;

        StringBuilder.appendString!("Vector#{", sb);

        let f = (x) -> {
            if (not deref first) StringBuilder.appendString!(", ", sb) else first := false;
            StringBuilder.appendString!("${x}", sb)
        };
        forEach(f, v);

        StringBuilder.appendString!("}", sb);
        StringBuilder.toString(sb)
    }

    ///
    /// Retrieves the value at position `i` in the vector `v`.
    ///
    pub def get(i: Int32, v: Vector[a]): a =
        v[i]


    ///
    /// Optionally returns the element at position `i` in the vector `v`.
    ///
    pub def nth(i: Int32, v: Vector[a]): Option[a] =
        if (0 <= i and i < length(v))
            Some(get(i, v))
        else
            None

    ///
    /// Returns `true` if the given vector `v` is empty.
    ///
    pub def isEmpty(v: Vector[a]): Bool = length(v) == 0

    ///
    /// Returns the length of the vector `v`.
    ///
    pub def length(v: Vector[a]): Int32 = Array.length(v)

    ///
    /// Returns a fresh array with the elements from the vector `v` from index `b` (inclusive) until index `e` (exclusive).
    ///
    pub def slice(b: Int32, e: Int32, v: Vector[a]): Vector[a] =
        unsafe_cast Array.slice(Static, b, e, v) as Array[a, true] \ { }

    ///
    /// Returns the vector `v` as a list.
    ///
    pub def toList(v: Vector[a]): List[a] =
        def loop(i, acc) = {
            if (i == 0) {
                acc
            } else {
                let x = get(i - 1, v);
                loop(i - 1, x :: acc)
            }
        };
        loop(length(v), Nil)

    ///
    /// Optionally returns the vector `v` as a non-empty list.
    ///
    /// If `v` is empty return `None`, otherwise return the Nel wrapped in `Some`.
    ///
    pub def toNel(v: Vector[a]): Option[Nel[a]] =
        def loop(i, acc) = {
            if (i == 0) {
                acc
            } else {
                let x = get(i - 1, v);
                loop(i - 1, Nel.cons(x, acc))
            }
        };
        if (Array.isEmpty(v)) {
            None
        } else {
            let i = length(v) - 1;
            Some(loop(i, Nel.singleton(get(i, v))))
        }

    ///
    /// Returns the vector `v` as a `DelayList`.
    ///
    pub def toDelayList(v: Vector[a]): DelayList[a] =
        def loop(i, acc) = {
            if (i < 0)
                acc
            else
                loop(i - 1, DelayList.ECons(get(i, v), acc))
        };
        loop(length(v) - 1, DelayList.ENil)

    ///
    /// Returns the vector `v` as a chain.
    ///
    pub def toChain(v: Vector[a]): Chain[a] =
        def loop(i, acc) = {
            if (i == 0) {
                acc
            } else {
                let x = get(i - 1, v);
                loop(i - 1, Chain.cons(x, acc))
            }
        };
        loop(length(v), Chain.empty())

    ///
    /// Optionally returns the vector `v` as a non-empty chain.
    ///
    /// If `v` is empty return `None`, otherwise return the Nec wrapped in `Some`.
    ///
    pub def toNec(v: Vector[a]): Option[Nec[a]] =
        def loop(i, acc) = {
            if (i == 0) {
                acc
            } else {
                let x = get(i - 1, v);
                loop(i - 1, Nec.cons(x, acc))
            }
        };
        if (isEmpty(v)) {
            None
        } else {
            let i = length(v) - 1;
            Some(loop(i, Nec.singleton(get(i, v))))
        }

    ///
    /// Returns `v` as a MutDeque.
    ///
    pub def toMutDeque(rc: Region[r], v: Vector[a]): MutDeque[a, r] \ Write(r)   =
        let d = new MutDeque(rc);
        forEach(x -> MutDeque.pushBack(x, d), v);
        d

    ///
    /// Returns `Some(x)` if `x` is the first element of `v`.
    ///
    /// Returns `None` if `a` is empty.
    ///
    pub def head(v: Vector[a]): Option[a] =
        nth(0, v)

    ///
    /// Returns `Some(x)` if `x` is the last element of `v`.
    ///
    /// Returns `None` if `a` is empty.
    ///
    pub def last(v: Vector[a]): Option[a] =
        nth(length(v) - 1, v)

    ///
    /// Apply the effectful function `f` to all the elements in the vector `v`.
    ///
    pub def forEach(f: a -> Unit \ ef, v: Vector[a]): Unit \ ef =
        let len = length(v);
        def loop(i) = {
            if (i >= len)
                ()
            else {
                f(get(i, v));
                loop(i + 1)
            }
        };
        loop(0)

}
