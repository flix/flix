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
    pub def combineN(x: a, n: Int32): a =
        if (n <= 1) x
        else SemiGroup.combine(x, SemiGroup.combineN(x, n - 1))
}

instance SemiGroup[Unit] {
    pub def combine(_: Unit, _: Unit): Unit = ()
}

instance SemiGroup[String] {
    pub def combine(x: String, y: String): String = x + y
}

instance SemiGroup[Option[a]] with SemiGroup[a] {
  pub def combine(x: Option[a], y: Option[a]): Option[a] = match (x, y) {
        case (Some(x1), Some(y1)) => Some(SemiGroup.combine(x1, y1))
        case (a, None) => a
        case (None, b) => b
    }
}

instance SemiGroup[List[a]] {
  pub def combine(x: List[a], y: List[a]): List[a] = x ::: y
}

instance SemiGroup[Set[a]] with Order[a] {
  pub def combine(x: Set[a], y: Set[a]): Set[a] = Set.union(x,y)
}

instance SemiGroup[Map[k,v]] with Order[k] {
  pub def combine(x: Map[k,v], y: Map[k,v]): Map[k,v] = Map.union(x, y)
}

instance SemiGroup[Validation[t, e]] with SemiGroup[t] {
  pub def combine(x: Validation[t, e], y: Validation[t, e]): Validation[t, e] = match (x, y) {
        case (Success(x1), Success(y1)) => Success(SemiGroup.combine(x1, y1))
        case (Failure(es1), Failure(es2)) => Failure(Nel.append(es1, es2))
        case (Failure(_), _) => x
        case (_, Failure(_)) => y
    }
}