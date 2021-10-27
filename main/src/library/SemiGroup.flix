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
pub lawless class SemiGroup[a] {
    ///
    /// An associative binary operation on `a`.
    ///
    pub def combine(x: a, y: a): a

    ///
    /// Returns `x` combined with itself `n` times.
    ///
    pub def combineN(x: a, n: Int32): a
}

instance SemiGroup[Unit] {
    pub def combine(_: Unit, _: Unit): Unit = ()
    pub def combineN(_: Unit, _: Int32): Unit = ()
}

instance SemiGroup[String] {
    pub def combine(x: String, y: String): String = x + y
    pub def combineN(x: String, n: Int32): String =
        if (n <= 0) ???
        else if (n == 1) x
        else SemiGroup.combine(x, SemiGroup.combineN(x, n - 1))
}

instance SemiGroup1[List[a]] {
  pub def combine(x: List[a], y: List[a]): List[a] = match x {
    case (z :: zs) => SemiGroup1.combine(zs, z :: y)
    case (Nil) => y
  }
  pub def combineN(x: List[a], n: Int32): List[a] = ???
}
