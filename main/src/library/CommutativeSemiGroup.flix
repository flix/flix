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
/// A trait for types that form a commutative semigroup.
///
pub lawful trait CommutativeSemiGroup[a] with SemiGroup[a] {

    ///
    /// An associative & commutative binary operation on `a`.
    ///
    pub def combine(x: a, y: a): a = SemiGroup.combine(x, y)

    law associative: forall(x: a, y: a, z: a) with Eq[a] CommutativeSemiGroup.combine(CommutativeSemiGroup.combine(x, y), z) == CommutativeSemiGroup.combine(x, CommutativeSemiGroup.combine(y, z))

    law commutative: forall(x: a, y: a) with Eq[a] CommutativeSemiGroup.combine(x, y) == CommutativeSemiGroup.combine(y, x)

}

///
/// Alias for `CommutativeSemiGroup.combine`.
///
pub def |+|(x: a, y: a): a with CommutativeSemiGroup[a] = CommutativeSemiGroup.combine(x, y)

instance CommutativeSemiGroup[Unit]

instance CommutativeSemiGroup[Int8]

instance CommutativeSemiGroup[Int16]

instance CommutativeSemiGroup[Int32]

instance CommutativeSemiGroup[Int64]

instance CommutativeSemiGroup[BigInt]

instance CommutativeSemiGroup[Float32]

instance CommutativeSemiGroup[Float64]

instance CommutativeSemiGroup[BigDecimal]

instance CommutativeSemiGroup[(a1, a2)] with CommutativeSemiGroup[a1], CommutativeSemiGroup[a2]

instance CommutativeSemiGroup[(a1, a2, a3)] with CommutativeSemiGroup[a1], CommutativeSemiGroup[a2], CommutativeSemiGroup[a3]

instance CommutativeSemiGroup[(a1, a2, a3, a4)] with CommutativeSemiGroup[a1], CommutativeSemiGroup[a2], CommutativeSemiGroup[a3], CommutativeSemiGroup[a4]

instance CommutativeSemiGroup[(a1, a2, a3, a4, a5)] with CommutativeSemiGroup[a1], CommutativeSemiGroup[a2], CommutativeSemiGroup[a3], CommutativeSemiGroup[a4], CommutativeSemiGroup[a5]

instance CommutativeSemiGroup[(a1, a2, a3, a4, a5, a6)] with CommutativeSemiGroup[a1], CommutativeSemiGroup[a2], CommutativeSemiGroup[a3], CommutativeSemiGroup[a4], CommutativeSemiGroup[a5], CommutativeSemiGroup[a6]

instance CommutativeSemiGroup[(a1, a2, a3, a4, a5, a6, a7)] with CommutativeSemiGroup[a1], CommutativeSemiGroup[a2], CommutativeSemiGroup[a3], CommutativeSemiGroup[a4], CommutativeSemiGroup[a5], CommutativeSemiGroup[a6], CommutativeSemiGroup[a7]

instance CommutativeSemiGroup[(a1, a2, a3, a4, a5, a6, a7, a8)] with CommutativeSemiGroup[a1], CommutativeSemiGroup[a2], CommutativeSemiGroup[a3], CommutativeSemiGroup[a4], CommutativeSemiGroup[a5], CommutativeSemiGroup[a6], CommutativeSemiGroup[a7], CommutativeSemiGroup[a8]

instance CommutativeSemiGroup[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with CommutativeSemiGroup[a1], CommutativeSemiGroup[a2], CommutativeSemiGroup[a3], CommutativeSemiGroup[a4], CommutativeSemiGroup[a5], CommutativeSemiGroup[a6], CommutativeSemiGroup[a7], CommutativeSemiGroup[a8], CommutativeSemiGroup[a9]

instance CommutativeSemiGroup[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with CommutativeSemiGroup[a1], CommutativeSemiGroup[a2], CommutativeSemiGroup[a3], CommutativeSemiGroup[a4], CommutativeSemiGroup[a5], CommutativeSemiGroup[a6], CommutativeSemiGroup[a7], CommutativeSemiGroup[a8], CommutativeSemiGroup[a9], CommutativeSemiGroup[a10]

