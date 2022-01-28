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
/// A type class for types that form a commutative semigroup.
///
pub class CommutativeSemiGroup[a] with SemiGroup[a] {

    ///
    /// An associative binary operation on `a`.
    ///
    pub def combine(x: a, y: a): a = SemiGroup.combine(x, y)

    ///
    /// Returns `x` combined with itself `n` times.
    ///
    pub def combineN(x: a, n: Int32): a = SemiGroup.combineN(x, n)

    law commutative: forall(x: a, y: a) with Eq[a] . CommutativeSemiGroup.combine(x, y) == CommutativeCommutativeSemiGroup.combine(y, x)

}

instance CommutativeSemiGroup[Unit] {
}

instance CommutativeSemiGroup[Option[a]] with CommutativeSemiGroup[a] {
}

instance CommutativeSemiGroup[Set[a]] with Order[a] {
}

instance CommutativeSemiGroup[Map[k,v]] with Order[k], CommutativeSemiGroup[v] {
    pub def combine(x: Map[k,v], y: Map[k,v]): Map[k,v] = Map.unionWith(CommutativeSemiGroup.combine, x, y)
}
/*
instance CommutativeSemiGroup[Validation[t, e]] with CommutativeSemiGroup[t] {
    pub def combine(x: Validation[t, e], y: Validation[t, e]): Validation[t, e] = match (x, y) {
        case (Success(x1), Success(y1))     => Success(CommutativeSemiGroup.combine(x1, y1))
        case (Failure(es1), Failure(es2))   => Failure(Nel.append(es1, es2))
        case (Failure(_), _)                => x
        case (_, Failure(_))                => y
    }
}
*/
instance CommutativeSemiGroup[(a1, a2)] with CommutativeSemiGroup[a1], CommutativeSemiGroup[a2] {
}

instance CommutativeSemiGroup[(a1, a2, a3)] with CommutativeSemiGroup[a1], CommutativeSemiGroup[a2], CommutativeSemiGroup[a3] {
}

instance CommutativeSemiGroup[(a1, a2, a3, a4)] with CommutativeSemiGroup[a1], CommutativeSemiGroup[a2], CommutativeSemiGroup[a3], CommutativeSemiGroup[a4] {
}

instance CommutativeSemiGroup[(a1, a2, a3, a4, a5)] with CommutativeSemiGroup[a1], CommutativeSemiGroup[a2], CommutativeSemiGroup[a3], CommutativeSemiGroup[a4], CommutativeSemiGroup[a5] {
}

instance CommutativeSemiGroup[(a1, a2, a3, a4, a5, a6)] with CommutativeSemiGroup[a1], CommutativeSemiGroup[a2], CommutativeSemiGroup[a3], CommutativeSemiGroup[a4], CommutativeSemiGroup[a5], CommutativeSemiGroup[a6] {
}

instance CommutativeSemiGroup[(a1, a2, a3, a4, a5, a6, a7)] with CommutativeSemiGroup[a1], CommutativeSemiGroup[a2], CommutativeSemiGroup[a3], CommutativeSemiGroup[a4], CommutativeSemiGroup[a5], CommutativeSemiGroup[a6], CommutativeSemiGroup[a7] {
}

instance CommutativeSemiGroup[(a1, a2, a3, a4, a5, a6, a7, a8)] with CommutativeSemiGroup[a1], CommutativeSemiGroup[a2], CommutativeSemiGroup[a3], CommutativeSemiGroup[a4], CommutativeSemiGroup[a5], CommutativeSemiGroup[a6], CommutativeSemiGroup[a7], CommutativeSemiGroup[a8] {
}

instance CommutativeSemiGroup[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with CommutativeSemiGroup[a1], CommutativeSemiGroup[a2], CommutativeSemiGroup[a3], CommutativeSemiGroup[a4], CommutativeSemiGroup[a5], CommutativeSemiGroup[a6], CommutativeSemiGroup[a7], CommutativeSemiGroup[a8], CommutativeSemiGroup[a9] {
}

instance CommutativeSemiGroup[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with CommutativeSemiGroup[a1], CommutativeSemiGroup[a2], CommutativeSemiGroup[a3], CommutativeSemiGroup[a4], CommutativeSemiGroup[a5], CommutativeSemiGroup[a6], CommutativeSemiGroup[a7], CommutativeSemiGroup[a8], CommutativeSemiGroup[a9], CommutativeSemiGroup[a10] {
}

