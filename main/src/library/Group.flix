/*
 *  Copyright 2022 Jakob Schneider Villumsen
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
/// A trait for types that form a group.
///
/// The default instances for numbers define the additive inverse in the real numbers.
///
pub trait Group[a] with Monoid[a] {

    ///
    /// Returns the neutral element.
    ///
    pub def empty(): a =
        Monoid.empty()

    ///
    /// Returns the inverse element of `x`.
    ///
    pub def inverse(x: a): a

    ///
    /// Returns the result of combining `x` and `y` using `Monoid`'s combine.
    ///
    pub def combine(x: a, y: a): a =
        Monoid.combine(x, y)

    ///
    /// Returns `y` removed from `x`.
    ///
    /// Equivalent to `Group.combine(x, Group.inverse(y))`
    ///
    pub def remove(x: a, y: a): a =
        Group.combine(x, Group.inverse(y))

    ///
    /// Combining the inverse of `x` with `x` yields the neutral element in the group.
    ///
    /// The operation is commutative i.e. if `y` is the inverse of `x` and `e` is the
    /// identity (neutral element) then
    /// `combine(x, y) == combine(y, x) == e`
    ///
    law leftInvertible: forall(x: a) with Eq[a] . Group.combine(Group.inverse(x), x) == Group.empty()

    ///
    /// Combining the inverse of `x` with `x` yields the neutral element in the group.
    ///
    /// The operation is commutative i.e. if `y` is the inverse of `x` and `e` is the
    /// identity (neutral element) then
    /// `combine(x, y) == combine(y, x) == e`
    ///
    law rightInvertible: forall(x: a) with Eq[a] . Group.combine(x, Group.inverse(x)) == Group.empty()

}

instance Group[Unit] {
    pub def inverse(_: Unit): Unit = ()
}

instance Group[Int8] {
    pub def inverse(x: Int8): Int8 = -x
    override pub def remove(x: Int8, y: Int8): Int8 = x - y
}

instance Group[Int16] {
    pub def inverse(x: Int16): Int16 = -x
    override pub def remove(x: Int16, y: Int16): Int16 = x - y
}

instance Group[Int32] {
    pub def inverse(x: Int32): Int32 = -x
    override pub def remove(x: Int32, y: Int32): Int32 = x - y
}

instance Group[Int64] {
    pub def inverse(x: Int64): Int64 = -x
    override pub def remove(x: Int64, y: Int64): Int64 = x - y
}

instance Group[BigInt] {
    pub def inverse(x: BigInt): BigInt = -x
    override pub def remove(x: BigInt, y: BigInt): BigInt = x - y
}

instance Group[Float32] {
    pub def inverse(x: Float32): Float32 = -x
    override pub def remove(x: Float32, y: Float32): Float32 = x - y
}

instance Group[Float64] {
    pub def inverse(x: Float64): Float64 = -x
    override pub def remove(x: Float64, y: Float64): Float64 = x - y
}

instance Group[BigDecimal] {
    pub def inverse(x: BigDecimal): BigDecimal = -x
    override pub def remove(x: BigDecimal, y: BigDecimal): BigDecimal = x - y
}

instance Group[(a1, a2)] with Group[a1], Group[a2] {
    pub def inverse(x: (a1, a2)): (a1, a2) = match x {
        case (a1, a2) => (Group.inverse(a1), Group.inverse(a2))
    }
}

instance Group[(a1, a2, a3)] with Group[a1], Group[a2], Group[a3] {
    pub def inverse(x: (a1, a2, a3)): (a1, a2, a3) = match x {
        case (a1, a2, a3) => (Group.inverse(a1), Group.inverse(a2), Group.inverse(a3))
    }
}

instance Group[(a1, a2, a3, a4)] with Group[a1], Group[a2], Group[a3], Group[a4] {
    pub def inverse(x: (a1, a2, a3, a4)): (a1, a2, a3, a4) = match x {
        case (a1, a2, a3, a4) => (Group.inverse(a1), Group.inverse(a2), Group.inverse(a3), Group.inverse(a4))
    }
}

instance Group[(a1, a2, a3, a4, a5)] with Group[a1], Group[a2], Group[a3], Group[a4], Group[a5] {
    pub def inverse(x: (a1, a2, a3, a4, a5)): (a1, a2, a3, a4, a5) = match x {
        case (a1, a2, a3, a4, a5) => (Group.inverse(a1), Group.inverse(a2), Group.inverse(a3), Group.inverse(a4), Group.inverse(a5))
    }
}

instance Group[(a1, a2, a3, a4, a5, a6)] with Group[a1], Group[a2], Group[a3], Group[a4], Group[a5], Group[a6] {
    pub def inverse(x: (a1, a2, a3, a4, a5, a6)): (a1, a2, a3, a4, a5, a6) = match x {
        case (a1, a2, a3, a4, a5, a6) => (Group.inverse(a1), Group.inverse(a2), Group.inverse(a3), Group.inverse(a4), Group.inverse(a5), Group.inverse(a6))
    }
}

instance Group[(a1, a2, a3, a4, a5, a6, a7)] with Group[a1], Group[a2], Group[a3], Group[a4], Group[a5], Group[a6], Group[a7] {
    pub def inverse(x: (a1, a2, a3, a4, a5, a6, a7)): (a1, a2, a3, a4, a5, a6, a7) = match x {
        case (a1, a2, a3, a4, a5, a6, a7) => (Group.inverse(a1), Group.inverse(a2), Group.inverse(a3), Group.inverse(a4), Group.inverse(a5), Group.inverse(a6), Group.inverse(a7))
    }
}

instance Group[(a1, a2, a3, a4, a5, a6, a7, a8)] with Group[a1], Group[a2], Group[a3], Group[a4], Group[a5], Group[a6], Group[a7], Group[a8] {
    pub def inverse(x: (a1, a2, a3, a4, a5, a6, a7, a8)): (a1, a2, a3, a4, a5, a6, a7, a8) = match x {
        case (a1, a2, a3, a4, a5, a6, a7, a8) => (Group.inverse(a1), Group.inverse(a2), Group.inverse(a3), Group.inverse(a4), Group.inverse(a5), Group.inverse(a6), Group.inverse(a7), Group.inverse(a8))
    }
}

instance Group[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with Group[a1], Group[a2], Group[a3], Group[a4], Group[a5], Group[a6], Group[a7], Group[a8], Group[a9] {
    pub def inverse(x: (a1, a2, a3, a4, a5, a6, a7, a8, a9)): (a1, a2, a3, a4, a5, a6, a7, a8, a9) = match x {
        case (a1, a2, a3, a4, a5, a6, a7, a8, a9) => (Group.inverse(a1), Group.inverse(a2), Group.inverse(a3), Group.inverse(a4), Group.inverse(a5), Group.inverse(a6), Group.inverse(a7), Group.inverse(a8), Group.inverse(a9))
    }
}

instance Group[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with Group[a1], Group[a2], Group[a3], Group[a4], Group[a5], Group[a6], Group[a7], Group[a8], Group[a9], Group[a10] {
    pub def inverse(x: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)): (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = match x {
        case (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) => (Group.inverse(a1), Group.inverse(a2), Group.inverse(a3), Group.inverse(a4), Group.inverse(a5), Group.inverse(a6), Group.inverse(a7), Group.inverse(a8), Group.inverse(a9), Group.inverse(a10))
    }
}
