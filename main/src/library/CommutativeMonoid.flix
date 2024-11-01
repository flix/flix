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
/// A trait for types that form a commutative monoid.
///
pub lawful trait CommutativeMonoid[a] with Monoid[a] {

    ///
    /// Returns a neutral element.
    ///
    pub def empty(): a = Monoid.empty()

    ///
    /// An associative & commutative binary operation on `a`.
    ///
    pub def combine(x: a, y: a): a =
        Monoid.combine(x, y)

    law leftIdentity: forall(x: a) with Eq[a] CommutativeMonoid.combine(CommutativeMonoid.empty(), x) == x

    law rightIdentity: forall(x: a) with Eq[a] CommutativeMonoid.combine(x, CommutativeMonoid.empty()) == x

    law associative: forall(x: a, y: a, z: a) with Eq[a] CommutativeMonoid.combine(CommutativeMonoid.combine(x, y), z) == CommutativeMonoid.combine(x, CommutativeMonoid.combine(y, z))

    law commutative: forall(x: a, y: a) with Eq[a] CommutativeMonoid.combine(x, y) == CommutativeMonoid.combine(y, x)

}
instance CommutativeMonoid[Unit]

instance CommutativeMonoid[Int8]

instance CommutativeMonoid[Int16]

instance CommutativeMonoid[Int32]

instance CommutativeMonoid[Int64]

instance CommutativeMonoid[BigInt]

instance CommutativeMonoid[Float32]

instance CommutativeMonoid[Float64]

instance CommutativeMonoid[BigDecimal]

instance CommutativeMonoid[(a1, a2)] with CommutativeMonoid[a1], CommutativeMonoid[a2]

instance CommutativeMonoid[(a1, a2, a3)] with CommutativeMonoid[a1], CommutativeMonoid[a2], CommutativeMonoid[a3]

instance CommutativeMonoid[(a1, a2, a3, a4)] with CommutativeMonoid[a1], CommutativeMonoid[a2], CommutativeMonoid[a3], CommutativeMonoid[a4]

instance CommutativeMonoid[(a1, a2, a3, a4, a5)] with CommutativeMonoid[a1], CommutativeMonoid[a2], CommutativeMonoid[a3], CommutativeMonoid[a4], CommutativeMonoid[a5]

instance CommutativeMonoid[(a1, a2, a3, a4, a5, a6)] with CommutativeMonoid[a1], CommutativeMonoid[a2], CommutativeMonoid[a3], CommutativeMonoid[a4], CommutativeMonoid[a5], CommutativeMonoid[a6]

instance CommutativeMonoid[(a1, a2, a3, a4, a5, a6, a7)] with CommutativeMonoid[a1], CommutativeMonoid[a2], CommutativeMonoid[a3], CommutativeMonoid[a4], CommutativeMonoid[a5], CommutativeMonoid[a6], CommutativeMonoid[a7]

instance CommutativeMonoid[(a1, a2, a3, a4, a5, a6, a7, a8)] with CommutativeMonoid[a1], CommutativeMonoid[a2], CommutativeMonoid[a3], CommutativeMonoid[a4], CommutativeMonoid[a5], CommutativeMonoid[a6], CommutativeMonoid[a7], CommutativeMonoid[a8]

instance CommutativeMonoid[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with CommutativeMonoid[a1], CommutativeMonoid[a2], CommutativeMonoid[a3], CommutativeMonoid[a4], CommutativeMonoid[a5], CommutativeMonoid[a6], CommutativeMonoid[a7], CommutativeMonoid[a8], CommutativeMonoid[a9]

instance CommutativeMonoid[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with CommutativeMonoid[a1], CommutativeMonoid[a2], CommutativeMonoid[a3], CommutativeMonoid[a4], CommutativeMonoid[a5], CommutativeMonoid[a6], CommutativeMonoid[a7], CommutativeMonoid[a8], CommutativeMonoid[a9], CommutativeMonoid[a10]
