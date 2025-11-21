/*
 * Copyright 2020 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

///
/// The Mutable Set type.
///
pub struct MutSet[t: Type, r: Region] {
    rc: Region[r],
    mut inner: Set[t]
}

instance Iterable[MutSet[a, r]] {
    type Elm = a
    type Aef = r
    pub def iterator(rc: Region[r1], s: MutSet[a, r]): Iterator[a, r + r1, r1] \ (r + r1) = MutSet.iterator(rc, s)
}

instance ForEach[MutSet[a, r]] {
    type Elm = a
    type Aef = r
    pub def forEach(f: a -> Unit \ ef, s: MutSet[a, r]): Unit \ ef + r = MutSet.forEach(f, s)
}

instance Formattable[MutSet[t, r]] with Formattable[t] {
    type Aef = Formattable.Aef[t] + r

    pub def format(x: MutSet[t, r]): RichString \ (Formattable.Aef[t] + r) =
        use RichString.{fromString, joinWith};
        fromString("MutSet#{") + joinWith(Formattable.format, fromString(", "), MutSet.toList(x)) + fromString("}")
}

mod MutSet {

    ///
    /// Returns a string representation of the given mutable set `s`.
    ///
    pub def toString(s: MutSet[a, r]): String \r with ToString[a] = region rc {
        "MutSet#{" + (MutSet.iterator(rc, s) |> Iterator.join(", ")) + "}"
    }

    ///
    /// Returns a fresh empty set.
    ///
    pub def empty(rc: Region[r]): MutSet[a, r] \ r =
        new MutSet @ rc { rc = rc, inner = Set.empty() }

    ///
    /// Returns the singleton set containing `x`.
    ///
    pub def singleton(rc: Region[r], x: a): MutSet[a, r] \ r with Order[a] =
        new MutSet @ rc { rc = rc, inner = Set.singleton(x) }

    ///
    /// Adds the element `x` to the mutable set `s`.
    ///
    pub def add(x: a, s: MutSet[a, r]): Unit \ r with Order[a] =
        s->inner = Set.insert(x, s->inner)

    ///
    /// Adds all elements in the collection `m` to the mutable set `s`.
    ///
    pub def addAll(m: m[a], s: MutSet[a, r]): Unit \ (r + Foldable.Aef[m]) with Order[a], Foldable[m] =
        Foldable.forEach(x -> add(x, s), m)

    ///
    /// Removes all elements from the mutable set `s`.
    ///
    pub def clear(s: MutSet[a, r]): Unit \ r =
        s->inner = Set.empty()

    ///
    /// Removes the element `x` from the mutable set `s`.
    ///
    pub def remove(x: a, s: MutSet[a, r]): Unit \ r with Order[a] =
        s->inner = Set.remove(x, s->inner)

    ///
    /// Removes all elements in the collection `m` from the mutable set `s`.
    ///
    pub def removeAll(m: m[a], s: MutSet[a, r]): Unit \ (r + Foldable.Aef[m]) with Order[a], Foldable[m] =
        let s2 = Foldable.toSet(m);
        s->inner = Set.difference(s->inner, s2)

    ///
    /// Removes all elements from the mutable set `s` that are not in collection `m`.
    ///
    pub def retainAll(m: m[a], s: MutSet[a, r]): Unit \ (r + Foldable.Aef[m]) with Order[a], Foldable[m] =
        let s2 = Foldable.toSet(m);
        s->inner = Set.intersection(s2, s->inner)

    ///
    /// Removes all elements from the mutable set `s` that do not satisfy the predicate function `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def refine(f: a -> Bool, s: MutSet[a, r]): Unit \ r with Order[a] =
        s->inner = Set.filter(f, s->inner)

    ///
    /// Replaces the element `src` with the element `dst` if `from` is in the mutable set `s`.
    ///
    /// The mutable set `s` is unchanged if the element `from` is not in it.
    ///
    pub def replace(src: {src = a}, dst: {dst = a}, s: MutSet[a, r]): Unit \ r with Order[a] =
        s->inner = Set.replace(src = src#src, dst = dst#dst, s->inner)

    ///
    /// Returns true if and only if `s` is the empty set.
    ///
    pub def isEmpty(s: MutSet[a, r]): Bool \ r =
        Set.isEmpty(s->inner)

    ///
    /// Returns true if and only if `s` is a non-empty set.
    ///
    pub def nonEmpty(s: MutSet[a, r]): Bool \ r = not isEmpty(s)

    ///
    /// Returns true if and only if `x` is a member of the mutable set `s`.
    ///
    pub def memberOf(x: a, s: MutSet[a, r]): Bool \ r with Order[a] =
        Set.memberOf(x, s->inner)

    ///
    /// Optionally finds the smallest element of `s` according to the `Order` on `a`.
    ///
    /// Returns `None` if `s` is empty.
    ///
    pub def minimum(s: MutSet[a, r]): Option[a] \ r =
        Set.minimum(s->inner)

    ///
    /// Optionally finds the smallest element of `s` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `s` is empty.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    pub def minimumBy(cmp: (a, a) -> Comparison \ ef, s: MutSet[a, r]): Option[a] \ { ef, r } =
        Set.minimumBy(cmp, s->inner)

    ///
    /// Optionally finds the largest element of `s` according to the `Order` on `a`.
    ///
    /// Returns `None` if `s` is empty.
    ///
    pub def maximum(s: MutSet[a, r]): Option[a] \ r =
        Set.maximum(s->inner)

    ///
    /// Optionally finds the largest element of `s` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `s` is empty.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    pub def maximumBy(cmp: (a, a) -> Comparison \ ef, s: MutSet[a, r]): Option[a] \ { ef, r } =
        Set.maximumBy(cmp, s->inner)

    ///
    /// Returns the number of elements in the mutable set `s`.
    ///
    pub def size(s: MutSet[a, r]): Int32 \ r =
        Set.size(s->inner)

    ///
    /// Alias for `findLeft`.
    ///
    /// The function `f` must be pure.
    ///
    pub def find(f: a -> Bool, s: MutSet[a, r]): Option[a] \ r =
        findLeft(f, s)

    ///
    /// Optionally returns the first element of the mutable set `s` that satisfies the predicate function `f` when searching from left to right.
    ///
    /// The function `f` must be pure.
    ///
    pub def findLeft(f: a -> Bool, s: MutSet[a, r]): Option[a] \ r =
        Set.findLeft(f, s->inner)

    ///
    /// Optionally returns the first element of the mutable set `s` that satisfies the predicate function `f` when searching from right to left.
    ///
    /// The function `f` must be pure.
    ///
    pub def findRight(f: a -> Bool, s: MutSet[a, r]): Option[a] \ r =
        Set.findRight(f, s->inner)

    ///
    /// Applies `f` to a start value `i` and all elements in the mutable set `s` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(i, x1), x2)..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b \ ef, i: b, s: MutSet[a, r]): b \ { ef, r } =
        Set.foldLeft(f, i, s->inner)

    ///
    /// Applies `f` to a start value `z` and all elements in the mutable set `s` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, z))...)`.
    ///
    pub def foldRight(f: (a, b) -> b \ ef, z: b, s: MutSet[a, r]): b \ { ef, r } =
        Set.foldRight(f, z, s->inner)

    ///
    /// Returns the result of mapping each element and combining the results.
    ///
    pub def foldMap(f: a -> b \ ef, s: MutSet[a, r]): b \ {ef, r} with Monoid[b] =
        foldLeft((acc, x) -> Monoid.combine(acc, f(x)), Monoid.empty(), s)

    ///
    /// Returns the number of elements in the mutable set `s` that satisfy the predicate function `f`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    pub def count(f: a -> Bool \ ef, s: MutSet[a, r]): Int32 \ { ef, r } =
        Set.count(f, s->inner)

    ///
    /// Returns `true` if and only if at least one element in the mutable set `s` satisfies the predicate function `f`.
    ///
    /// Returns `false` if `s` is the empty set.
    ///
    pub def exists(f: a -> Bool \ ef, s: MutSet[a, r]): Bool \ { ef, r } =
        Set.exists(f, s->inner)

    ///
    /// Returns `true` if and only if all elements in the mutable set `s` satisfy the predicate function `f`.
    ///
    /// Returns `true` if `s` is the empty set.
    ///
    pub def forAll(f: a -> Bool \ ef, s: MutSet[a, r]): Bool \ { ef, r } =
        Set.forAll(f, s->inner)

    ///
    /// Returns a shallow copy of the mutable set `s`.
    ///
    pub def copy(rc1: Region[r1], s: MutSet[a, r]): MutSet[a, r1] \ { r, r1 } =
        new MutSet @ rc1 { rc = rc1, inner = s->inner }

    ///
    /// Returns the mutable set `s` as an immutable set.
    ///
    pub def toSet(s: MutSet[a, r]): Set[a] \ r =
        s->inner

    ///
    /// Returns the mutable set `s` as a list.
    ///
    pub def toList(s: MutSet[a, r]): List[a] \ r =
        Set.toList(s->inner)

    ///
    /// Returns the association set `s` as a map.
    ///
    /// If `s` contains multiple mappings with the same key, `toMap` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    pub def toMap(s: MutSet[(a, b), r]): Map[a, b] \ r with Order[a] =
        Set.toMap(s->inner)

    ///
    /// Returns the mutable set `s` as an array.
    ///
    pub def toArray(rc: Region[r1], s: MutSet[a, r2]): Array[a, r1] \ { r1, r2 } =
        Set.toArray(rc, s->inner)

    ///
    /// Returns the mutable set `s` as a vector.
    ///
    pub def toVector(s: MutSet[a, r]): Vector[a] \ r =
        Set.toVector(s->inner)

    ///
    /// Applies `f` to every element of the mutable set `s`.
    ///
    pub def forEach(f: a -> Unit \ ef, s: MutSet[a, r]): Unit \ { ef, r } =
        Set.forEach(f, s->inner)

    ///
    /// Applies `f` to every element of the mutable set `s` along with that element's index.
    ///
    pub def forEachWithIndex(f: (Int32, a) -> Unit \ ef, s: MutSet[a, r]): Unit \ { ef, r } = region rc {
        let ix = Ref.fresh(rc, 0);
        forEach(x -> { let i = Ref.get(ix); f(i, x); Ref.put(i + 1, ix) }, s)
    }

    ///
    /// Returns an iterator over `s`.
    ///
    pub def iterator(rc: Region[r1], s: MutSet[a, r2]): Iterator[a, r1 + r2, r1] \ { r1, r2 } =
        Set.iterator(rc, s->inner) |> Iterator.map(x -> checked_ecast(x))

    ///
    /// Returns `true` if MutSets `a` and `b` have the same elements, i.e. are structurally equal.
    ///
    pub def sameElements(a: MutSet[a, r], b: MutSet[a, r]): Bool \ r with Order[a] =
        Set.isSubsetOf(a->inner, b->inner) and Set.isSubsetOf(b->inner, a->inner)

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `s` with `sep` inserted between each element.
    ///
    pub def join(sep: String, s: MutSet[a, r]): String \ r with ToString[a] =
        Set.join(sep, s->inner)

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `s` according to `f` with `sep` inserted between each element.
    ///
    pub def joinWith(f: a -> String \ ef, sep: String, s: MutSet[a, r]): String \ { ef, r } =
        Set.joinWith(f, sep, s->inner)

}
