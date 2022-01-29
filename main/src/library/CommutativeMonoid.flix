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
/// A type class for types that form a commutative monoid.
///
pub class CommutativeMonoid[a] with Monoid[a] {

    ///
    /// Returns a neutral element.
    ///
    pub def empty(): a = Monoid.empty()

    ///
    /// An associative & commutative binary operation on `a`.
    ///
    pub def combine(x: a, y: a): a =
        Monoid.combine(x, y)

    law leftIdentity: forall(x: a) with Eq[a] . CommutativeMonoid.combine(CommutativeMonoid.empty(), x) == x

    law rightIdentity: forall(x: a) with Eq[a] . CommutativeMonoid.combine(x, CommutativeMonoid.empty()) == x

    law associative: forall(x: a, y: a, z: a) with Eq[a] . CommutativeMonoid.combine(CommutativeMonoid.combine(x, y), z) == CommutativeMonoid.combine(x, CommutativeMonoid.combine(y, z))

    law commutative: forall(x: a, y: a) with Eq[a] . CommutativeMonoid.combine(x, y) == CommutativeMonoid.combine(y, x)

}
