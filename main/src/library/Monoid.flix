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
/// A type class for monoids, objects that support an associative binary
/// operation `combine` and neutral element `empty`.
///
pub class Monoid[a] {
    ///
    /// Returns a neutral element.
    ///
    pub def empty(): a

    ///
    /// Returns the result of __combining__ `x` and `y`.
    ///
    /// [When Flix has Superclasses this function should be moved into the
    /// class Semigroup.]
    ///
    pub def combine(x: a, y: a): a
}

namespace Monoid {

    ///
    /// Returns the result of applying `combine` to all the elements of list `xs`,
    /// using `empty` as the initial value.
    ///
    pub def combineAll[a: Monoid](xs: List[a]): a =
        List.foldLeft((ac, x) -> combine(ac, x), Monoid.empty(), xs)

}

instance Monoid[Unit] {
    def empty(): Unit = ()
    def combine(_: Unit, _: Unit): Unit = ()
}

instance Monoid[String] {
    def empty(): String = ""
    def combine(x: String, y: String): String = x + y
}

instance Monoid[Option[a]] with [a : Monoid] {
    def empty(): Option[a] = None
    def combine(x: Option[a], y: Option[a]): Option[a] = match (x, y) {
        case (Some(x1), Some(y1)) => Some(Monoid.combine(x1, y1))
        case (a, None) => a
        case (None, b) => b
    }
}

instance Monoid[(a1, a2)] with [a1 : Monoid, a2 : Monoid] {
    def empty(): (a1, a2) = (Monoid.empty(), Monoid.empty())
    def combine(x: (a1, a2), y: (a1, a2)): (a1, a2) = match (x, y) {
        case ((x1, x2), (y1, y2)) => (Monoid.combine(x1, y1), Monoid.combine(x2, y2))
    }
}

instance Monoid[(a1, a2, a3)] with [a1 : Monoid, a2 : Monoid, a3: Monoid] {
    def empty(): (a1, a2, a3) = (Monoid.empty(), Monoid.empty(), Monoid.empty())
    def combine(x: (a1, a2, a3), y: (a1, a2, a3)): (a1, a2, a3) = match (x, y) {
        case ((x1, x2, x3), (y1, y2, y3)) => (Monoid.combine(x1, y1), Monoid.combine(x2, y2), Monoid.combine(x3, y3))
    }
}

instance Monoid[(a1, a2, a3, a4)] with [a1 : Monoid, a2 : Monoid, a3: Monoid, a4: Monoid] {
    def empty(): (a1, a2, a3, a4) = (Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty())
    def combine(x: (a1, a2, a3, a4), y: (a1, a2, a3, a4)): (a1, a2, a3, a4) = match (x, y) {
        case ((x1, x2, x3, x4), (y1, y2, y3, y4)) => (Monoid.combine(x1, y1), Monoid.combine(x2, y2), Monoid.combine(x3, y3), Monoid.combine(x4, y4))
    }
}

instance Monoid[(a1, a2, a3, a4, a5)] with [a1 : Monoid, a2 : Monoid, a3: Monoid, a4: Monoid, a5: Monoid] {
    def empty(): (a1, a2, a3, a4, a5) = (Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty())
    def combine(x: (a1, a2, a3, a4, a5), y: (a1, a2, a3, a4, a5)): (a1, a2, a3, a4, a5) = match (x, y) {
        case ((x1, x2, x3, x4, x5), (y1, y2, y3, y4, y5)) => (Monoid.combine(x1, y1), Monoid.combine(x2, y2), Monoid.combine(x3, y3), Monoid.combine(x4, y4), Monoid.combine(x5, y5))
    }
}

instance Monoid[(a1, a2, a3, a4, a5, a6)] with [a1 : Monoid, a2 : Monoid, a3: Monoid, a4: Monoid, a5: Monoid, a6: Monoid] {
    def empty(): (a1, a2, a3, a4, a5, a6) = (Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty())
    def combine(x: (a1, a2, a3, a4, a5, a6), y: (a1, a2, a3, a4, a5, a6)): (a1, a2, a3, a4, a5, a6) = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6), (y1, y2, y3, y4, y5, y6)) => (Monoid.combine(x1, y1), Monoid.combine(x2, y2), Monoid.combine(x3, y3), Monoid.combine(x4, y4), Monoid.combine(x5, y5), Monoid.combine(x6, y6))
    }
}

instance Monoid[(a1, a2, a3, a4, a5, a6, a7)] with [a1 : Monoid, a2 : Monoid, a3: Monoid, a4: Monoid, a5: Monoid, a6: Monoid, a7: Monoid] {
    def empty(): (a1, a2, a3, a4, a5, a6, a7) = (Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty())
    def combine(x: (a1, a2, a3, a4, a5, a6, a7), y: (a1, a2, a3, a4, a5, a6, a7)): (a1, a2, a3, a4, a5, a6, a7) = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7), (y1, y2, y3, y4, y5, y6, y7)) => (Monoid.combine(x1, y1), Monoid.combine(x2, y2), Monoid.combine(x3, y3), Monoid.combine(x4, y4), Monoid.combine(x5, y5), Monoid.combine(x6, y6), Monoid.combine(x7, y7))
    }
}

instance Monoid[(a1, a2, a3, a4, a5, a6, a7, a8)] with [a1 : Monoid, a2 : Monoid, a3: Monoid, a4: Monoid, a5: Monoid, a6: Monoid, a7: Monoid, a8: Monoid] {
    def empty(): (a1, a2, a3, a4, a5, a6, a7, a8) = (Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty())
    def combine(x: (a1, a2, a3, a4, a5, a6, a7, a8), y: (a1, a2, a3, a4, a5, a6, a7, a8)): (a1, a2, a3, a4, a5, a6, a7, a8) = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7, x8), (y1, y2, y3, y4, y5, y6, y7, y8)) => (Monoid.combine(x1, y1), Monoid.combine(x2, y2), Monoid.combine(x3, y3), Monoid.combine(x4, y4), Monoid.combine(x5, y5), Monoid.combine(x6, y6), Monoid.combine(x7, y7), Monoid.combine(x8, y8))
    }
}

instance Monoid[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with [a1 : Monoid, a2 : Monoid, a3: Monoid, a4: Monoid, a5: Monoid, a6: Monoid, a7: Monoid, a8: Monoid, a9: Monoid] {
    def empty(): (a1, a2, a3, a4, a5, a6, a7, a8, a9) = (Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty())
    def combine(x: (a1, a2, a3, a4, a5, a6, a7, a8, a9), y: (a1, a2, a3, a4, a5, a6, a7, a8, a9)): (a1, a2, a3, a4, a5, a6, a7, a8, a9) = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7, x8, x9), (y1, y2, y3, y4, y5, y6, y7, y8, y9)) => (Monoid.combine(x1, y1), Monoid.combine(x2, y2), Monoid.combine(x3, y3), Monoid.combine(x4, y4), Monoid.combine(x5, y5), Monoid.combine(x6, y6), Monoid.combine(x7, y7), Monoid.combine(x8, y8), Monoid.combine(x9, y9))
    }
}

instance Monoid[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with [a1 : Monoid, a2 : Monoid, a3: Monoid, a4: Monoid, a5: Monoid, a6: Monoid, a7: Monoid, a8: Monoid, a9: Monoid, a10: Monoid] {
    def empty(): (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = (Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty())
    def combine(x: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10), y: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)): (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10), (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10)) => (Monoid.combine(x1, y1), Monoid.combine(x2, y2), Monoid.combine(x3, y3), Monoid.combine(x4, y4), Monoid.combine(x5, y5), Monoid.combine(x6, y6), Monoid.combine(x7, y7), Monoid.combine(x8, y8), Monoid.combine(x9, y9), Monoid.combine(x10, y10))
    }
}

instance Monoid[Validation[t, e]] with [t : Monoid] {
    def empty(): Validation[t, e] = Success(Monoid.empty())
    def combine(x: Validation[t, e], y: Validation[t, e]): Validation[t, e] = match (x, y) {
        case (Success(x1), Success(y1)) => Success(Monoid.combine(x1, y1))
        case (Failure(es1), Failure(es2)) => Failure(Nel.append(es1, es2))
        case (Failure(_), _) => x
        case (_, Failure(_)) => y
    }
}

instance Monoid[List[a]] {
    def empty(): List[a] = Nil
    def combine(x: List[a], y: List[a]): List[a] = List.append(x, y)
}

///
/// `combine` is set union.
///
instance Monoid[Set[a]] with [a : Order] {
    def empty(): Set[a] = Set.empty()
    def combine(x: Set[a], y: Set[a]): Set[a] = Set.union(x, y)
}

instance Monoid[Map[k, v]] with [k : Order, v: Monoid] {
    def empty(): Map[k, v] = Map.empty()
    def combine(x: Map[k, v], y: Map[k, v]): Map[k, v] =
        let ins = (ac, k, v) -> Map.insertWith(Monoid.combine, k, v, ac);
        Map.foldWithKey(ins, y, x)

}
