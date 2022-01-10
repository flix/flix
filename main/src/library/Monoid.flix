/*
 *  Copyright 2020 Stephen Tetley
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
/// A type class for Monoids, objects that support an associative binary
/// operation `combine` and neutral element `empty`.
///
pub lawless class Monoid[a] with SemiGroup[a] {
    ///
    /// Returns a neutral element.
    ///
    pub def empty(): a

    ///
    /// Returns the result of combining `x` and `y` using SemiGroup's combine.
    ///
    pub def combine(x: a, y: a): a =
        SemiGroup.combine(x, y)

    law associative: forall(x: a, y: a, z: a) with Eq[a] . Monoid.combine(Monoid.combine(x, y), z) == Monoid.combine(x, Monoid.combine(y, z))

}

namespace Monoid {

    ///
    /// Returns the result of applying `combine` to all the elements in `t`, using `empty` as the initial value.
    ///
    pub def fold[f: Type -> Type, a: Type](f: f[a]): a with Foldable[f], Monoid[a] = Foldable.fold(f)

}

instance Monoid[Unit] {
    pub def empty(): Unit = ()
}

instance Monoid[String] {
    pub def empty(): String = ""
}

instance Monoid[Option[a]] with Monoid[a] {
    pub def empty(): Option[a] = None
}

instance Monoid[(a1, a2)] with Monoid[a1], Monoid[a2] {
    pub def empty(): (a1, a2) = (Monoid.empty(), Monoid.empty())
}

instance Monoid[(a1, a2, a3)] with Monoid[a1], Monoid[a2], Monoid[a3] {
    pub def empty(): (a1, a2, a3) = (Monoid.empty(), Monoid.empty(), Monoid.empty())
}

instance Monoid[(a1, a2, a3, a4)] with Monoid[a1], Monoid[a2], Monoid[a3], Monoid[a4] {
    pub def empty(): (a1, a2, a3, a4) = (Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty())
}

instance Monoid[(a1, a2, a3, a4, a5)] with Monoid[a1], Monoid[a2], Monoid[a3], Monoid[a4], Monoid[a5] {
    pub def empty(): (a1, a2, a3, a4, a5) = (Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty())
}

instance Monoid[(a1, a2, a3, a4, a5, a6)] with Monoid[a1], Monoid[a2], Monoid[a3], Monoid[a4], Monoid[a5], Monoid[a6] {
    pub def empty(): (a1, a2, a3, a4, a5, a6) = (Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty())
}

instance Monoid[(a1, a2, a3, a4, a5, a6, a7)] with Monoid[a1], Monoid[a2], Monoid[a3], Monoid[a4], Monoid[a5], Monoid[a6], Monoid[a7] {
    pub def empty(): (a1, a2, a3, a4, a5, a6, a7) = (Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty())
}

instance Monoid[(a1, a2, a3, a4, a5, a6, a7, a8)] with Monoid[a1], Monoid[a2], Monoid[a3], Monoid[a4], Monoid[a5], Monoid[a6], Monoid[a7], Monoid[a8] {
    pub def empty(): (a1, a2, a3, a4, a5, a6, a7, a8) = (Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty())
}

instance Monoid[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with Monoid[a1], Monoid[a2], Monoid[a3], Monoid[a4], Monoid[a5], Monoid[a6], Monoid[a7], Monoid[a8], Monoid[a9] {
    pub def empty(): (a1, a2, a3, a4, a5, a6, a7, a8, a9) = (Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty())
}

instance Monoid[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with Monoid[a1], Monoid[a2], Monoid[a3], Monoid[a4], Monoid[a5], Monoid[a6], Monoid[a7], Monoid[a8], Monoid[a9], Monoid[a10] {
    pub def empty(): (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = (Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty())
}

instance Monoid[Validation[t, e]] with Monoid[t] {
    pub def empty(): Validation[t, e] = Success(Monoid.empty())
}

instance Monoid[List[a]] {
    pub def empty(): List[a] = Nil
}

///
/// `combine` is set union.
///
instance Monoid[Set[a]] with Order[a] {
    pub def empty(): Set[a] = Set.empty()
}

instance Monoid[Map[k, v]] with Order[k], Monoid[v] {
    pub def empty(): Map[k, v] = Map.empty()
}
