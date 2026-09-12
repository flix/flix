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

mod Discrete {

    ///
    /// A trait for types whose values form a discrete sequence, i.e. where every
    /// value has an immediate successor and predecessor.
    ///
    /// The successor and predecessor functions must satisfy the following laws:
    ///
    ///   - `prev(succ(x)) == x`
    ///   - `succ(prev(x)) == x`
    ///   - `succ(x) > x`, except at the maximum value of a bounded type (see below).
    ///
    /// For bounded types (e.g. `Int8`, `Int16`, `Int32`, `Int64`), `succ` wraps
    /// around from the maximum value to the minimum value, and `prev` wraps around
    /// from the minimum value to the maximum value. The unbounded `BigInt` never wraps.
    ///
    trait Discrete[t] with Order[t] {
        ///
        /// Returns the successor of the given `x`.
        ///
        pub def succ(x: t): t

        ///
        /// Returns the predecessor of the given `x`.
        ///
        pub def prev(x: t): t
    }

    instance Discrete[Int8] {
        pub def succ(x: Int8): Int8 = x + 1i8
        pub def prev(x: Int8): Int8 = x - 1i8
    }

    instance Discrete[Int16] {
        pub def succ(x: Int16): Int16 = x + 1i16
        pub def prev(x: Int16): Int16 = x - 1i16
    }

    instance Discrete[Int32] {
        pub def succ(x: Int32): Int32 = x + 1
        pub def prev(x: Int32): Int32 = x - 1
    }

    instance Discrete[Int64] {
        pub def succ(x: Int64): Int64 = x + 1i64
        pub def prev(x: Int64): Int64 = x - 1i64
    }

    instance Discrete[BigInt] {
        pub def succ(x: BigInt): BigInt = x + 1ii
        pub def prev(x: BigInt): BigInt = x - 1ii
    }

    ///
    /// Returns `true` if `x` and `y` are adjacent, i.e. if one is the successor of the other.
    ///
    pub def adjacent(x: t, y: t): Bool with Discrete[t] =
        Discrete.succ(x) == y or Discrete.succ(y) == x

}
