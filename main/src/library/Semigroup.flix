/*
 *  Copyright 2021 Esben Bjerre
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
/// A type class for types that form a semigroup.
///
pub lawless class Semigroup[a] {
    ///
    /// An associative binary operation on `a`.
    ///
    pub def combine(x: a, y: a): a
}

instance Semigroup[Unit] {
    pub def combine(_: Unit, _: Unit): Unit = ()
}

instance Semigroup[String] {
    pub def combine(x: String, y: String): String = x + y
}
