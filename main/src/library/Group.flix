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
/// A type class for types that form a group.
///
pub class Group[a] with Monoid[a] {

    ///
    /// Returns a neutral element.
    ///
    pub def empty(): a =
        Monoid.empty()

    ///
    /// Returns the inverse element of `x`.
    ///
    pub def inverse(x: a): a

    ///
    /// Returns `y` subtracted from `x` using a group subtraction.
    ///
    pub def remove(x: a, y: a): a =
        Group.combine(x, Group.invert(y))

    ///
    /// Returns the result of combining `x` and `y` using Monoid's combine.
    ///
    pub def combine(x: a, y: a): a =
        Monoid.combine(x, y)


    law leftIdentity: forall(x: a) with Eq[a] . Group.combine(Group.empty(), x) == x

    law rightIdentity: forall(x: a) with Eq[a] . Group.combine(x, Group.empty()) == x

    law associative: forall(x: a, y: a, z: a) with Eq[a] . Group.combine(Group.combine(x, y), z) == Group.combine(x, Group.combine(y, z))

    law leftInvertible: forall(x: a) with Eq[a] . Group.combine(Group.inverse(x), x) == Group.empty()

    law rightInvertible: forall(x: a) with Eq[a] . Group.combine(x, Group.inverse(x)) == Group.empty()

}
