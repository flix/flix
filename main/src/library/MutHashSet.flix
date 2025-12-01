/*
 * Copyright 2025 Magnus Madsen
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
/// Represents a mutable hash set that preserves insertion order.
///
pub struct MutHashSet[t: Type, r: Region] {
    inner: MutHashMap[t, Unit, r]
}

instance ForEach[MutHashSet[a, r]] {
    type Elm = a
    type Aef = r
    pub def forEach(f: a -> Unit \ ef, s: MutHashSet[a, r]): Unit \ ef + r =
        MutHashSet.forEach(f, s)
}

instance Iterable[MutHashSet[a, r]] {
    type Elm = a
    type Aef = r
    pub def iterator(rc: Region[r1], s: MutHashSet[a, r]): Iterator[a, r + r1, r1] \ (r + r1) =
        MutHashSet.iterator(rc, s)
}

instance Formattable[MutHashSet[t, r]] with Formattable[t] {
    type Aef = Formattable.Aef[t] + r

    pub def format(x: MutHashSet[t, r]): RichString \ (Formattable.Aef[t] + r) =
        use RichString.{fromString, joinWith};
        fromString("MutHashSet#{") + joinWith(Formattable.format, fromString(", "), MutHashSet.toList(x)) + fromString("}")
}

mod MutHashSet {

    ///
    /// Returns a new empty set.
    ///
    pub def empty(rc: Region[r]): MutHashSet[t, r] \ r =
        new MutHashSet @ rc { inner = MutHashMap.empty(rc) }

    ///
    /// Returns a new empty set with the given capacity.
    ///
    /// The capacity is rounded up to the minimum capacity.
    ///
    pub def emptyWithCapacity(rc: Region[r], capacity: Int32): MutHashSet[t, r] \ r =
        new MutHashSet @ rc { inner = MutHashMap.emptyWithCapacity(rc, capacity) }

    ///
    /// Returns a new set with the element `x`.
    ///
    pub def singleton(rc: Region[r], x: t): MutHashSet[t, r] \ r with Eq[t], Hash[t] =
        new MutHashSet @ rc { inner = MutHashMap.singleton(rc, x, ()) }

    ///
    /// Returns `true` if `s` is empty.
    ///
    pub def isEmpty(s: MutHashSet[t, r]): Bool \ r =
        MutHashMap.isEmpty(s->inner)

    ///
    /// Returns `true` if `s` is non-empty.
    ///
    pub def nonEmpty(s: MutHashSet[t, r]): Bool \ r =
        MutHashMap.nonEmpty(s->inner)

    ///
    /// Returns the number of elements in `s`.
    ///
    pub def size(s: MutHashSet[t, r]): Int32 \ r =
        MutHashMap.size(s->inner)

    ///
    /// Returns a shallow copy of the mutable set `s`.
    ///
    pub def copy(rc: Region[r1], s: MutHashSet[t, r]): MutHashSet[t, r1] \ {r, r1} with Eq[t], Hash[t] =
        new MutHashSet @ rc { inner = MutHashMap.copy(rc, s->inner) }

    ///
    /// Returns an iterator over `s`.
    ///
    pub def iterator(rc: Region[r1], s: MutHashSet[t, r]): Iterator[t, r + r1, r1] \ {r, r1} =
        MutHashMap.iteratorKeys(rc, s->inner)

    ///
    /// Applies `f` to every element of the mutable hash set `s`.
    ///
    pub def forEach(f: t -> Unit \ ef, s: MutHashSet[t, r]): Unit \ {ef, r} =
        MutHashMap.forEach((k, _) -> f(k), s->inner)

    ///
    /// Applies `f` to every element of the mutable hash set `s` along with that element's index.
    ///
    pub def forEachWithIndex(f: (Int32, t) -> Unit \ ef, s: MutHashSet[t, r]): Unit \ {ef, r} =
        MutHashMap.forEachWithIndex((i, k, _) -> f(i, k), s->inner)

    ///
    /// Adds the element `x` to the mutable set `s`.
    ///
    pub def add(x: t, s: MutHashSet[t, r]): Unit \ r with Eq[t], Hash[t] =
        MutHashMap.put(x, (), s->inner)

    ///
    /// Adds all elements in the collection `m` to the mutable set `s`.
    ///
    pub def addAll(m: m[t], s: MutHashSet[t, r]): Unit \ (r + Foldable.Aef[m]) with Eq[t], Hash[t], Foldable[m] =
        Foldable.forEach(x -> add(x, s), m)

    ///
    /// Removes the element `x` from the mutable set `s`.
    ///
    pub def remove(x: t, s: MutHashSet[t, r]): Unit \ r with Eq[t], Hash[t] =
        MutHashMap.remove(x, s->inner)

    ///
    /// Removes all elements in the collection `m` from the mutable set `s`.
    ///
    pub def removeAll(m: m[t], s: MutHashSet[t, r]): Unit \ (r + Foldable.Aef[m]) with Eq[t], Hash[t], Foldable[m] =
        Foldable.forEach(x -> remove(x, s), m)

    ///
    /// Removes all elements from the mutable set `s` that are not in collection `m`.
    ///
    pub def retainAll(m: m[t], s: MutHashSet[t, r]): Unit \ (r + Foldable.Aef[m]) with Eq[t], Hash[t], Foldable[m] =
        MutHashMap.refineWithKey((k, _) -> Foldable.memberOf(k, m), s->inner)

    ///
    /// Removes all elements from the mutable set `s`.
    ///
    pub def clear(s: MutHashSet[t, r]): Unit \ r =
        MutHashMap.clear(s->inner)

    ///
    /// Returns the first element that was inserted into the mutable set `s`.
    ///
    /// Returns `None` if the set is empty.
    ///
    pub def first(s: MutHashSet[t, r]): Option[t] \ r =
        MutHashMap.firstKey(s->inner)

    ///
    /// Returns the last element that was inserted into the mutable set `s`.
    ///
    /// Returns `None` if the set is empty.
    ///
    pub def last(s: MutHashSet[t, r]): Option[t] \ r =
        MutHashMap.lastKey(s->inner)

    ///
    /// Returns `true` if and only if `x` is a member of the mutable set `s`.
    ///
    pub def memberOf(x: t, s: MutHashSet[t, r]): Bool \ r with Eq[t], Hash[t] =
        MutHashMap.memberOf(x, s->inner)

    ///
    /// Removes all elements from the mutable set `s` that do not satisfy the predicate function `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def refine(f: t -> Bool, s: MutHashSet[t, r]): Unit \ r with Eq[t], Hash[t] =
        MutHashMap.refineWithKey((k, _) -> f(k), s->inner)

    ///
    /// Replaces the element `src` with the element `dst` if `src` is in the mutable set `s`.
    ///
    /// The mutable set `s` is unchanged if the element `src` is not in it.
    ///
    pub def replace(src: {src = t}, dst: {dst = t}, s: MutHashSet[t, r]): Unit \ r with Eq[t], Hash[t] =
        if (memberOf(src#src, s)) {
            remove(src#src, s);
            add(dst#dst, s)
        } else {
            ()
        }

    ///
    /// Alias for `findLeft`.
    ///
    /// The function `f` must be pure.
    ///
    pub def find(f: t -> Bool, s: MutHashSet[t, r]): Option[t] \ r =
        findLeft(f, s)

    ///
    /// Optionally returns the first element of the mutable set `s` that satisfies the predicate function `f` when searching from left to right.
    ///
    /// The function `f` must be pure.
    ///
    pub def findLeft(f: t -> Bool, s: MutHashSet[t, r]): Option[t] \ r =
        MutHashMap.findLeft((k, _) -> f(k), s->inner) |> Option.map(fst)

    ///
    /// Optionally returns the first element of the mutable set `s` that satisfies the predicate function `f` when searching from right to left.
    ///
    /// The function `f` must be pure.
    ///
    pub def findRight(f: t -> Bool, s: MutHashSet[t, r]): Option[t] \ r =
        MutHashMap.findRight((k, _) -> f(k), s->inner) |> Option.map(fst)

    ///
    /// Returns `true` if and only if at least one element in the mutable set `s` satisfies the predicate function `f`.
    ///
    /// Returns `false` if `s` is the empty set.
    ///
    pub def exists(f: t -> Bool \ ef, s: MutHashSet[t, r]): Bool \ {ef, r} =
        MutHashMap.exists((k, _) -> f(k), s->inner)

    ///
    /// Returns `true` if and only if all elements in the mutable set `s` satisfy the predicate function `f`.
    ///
    /// Returns `true` if `s` is the empty set.
    ///
    pub def forAll(f: t -> Bool \ ef, s: MutHashSet[t, r]): Bool \ {ef, r} =
        MutHashMap.forAll((k, _) -> f(k), s->inner)

    ///
    /// Returns the number of elements in the mutable set `s` that satisfy the predicate function `f`.
    ///
    pub def count(f: t -> Bool \ ef, s: MutHashSet[t, r]): Int32 \ {ef, r} =
        MutHashMap.count((k, _) -> f(k), s->inner)

    ///
    /// Applies `f` to a start value `i` and all elements in the mutable set `s` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(i, x1), x2)..., xn)`.
    ///
    pub def foldLeft(f: (b, t) -> b \ ef, i: b, s: MutHashSet[t, r]): b \ {ef, r} =
        MutHashMap.foldLeftWithKey((acc, k, _) -> f(acc, k), i, s->inner)

    ///
    /// Applies `f` to a start value `z` and all elements in the mutable set `s` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, z))...)`.
    ///
    pub def foldRight(f: (t, b) -> b \ ef, z: b, s: MutHashSet[t, r]): b \ {ef, r} =
        MutHashMap.foldRightWithKey((k, _, acc) -> f(k, acc), z, s->inner)

    ///
    /// Returns the result of mapping each element and combining the results.
    ///
    pub def foldMap(f: t -> b \ ef, s: MutHashSet[t, r]): b \ {ef, r} with Monoid[b] =
        foldLeft((acc, x) -> Monoid.combine(acc, f(x)), Monoid.empty(), s)

    ///
    /// Returns the mutable set `s` as an array.
    ///
    pub def toArray(rc: Region[r1], s: MutHashSet[t, r]): Array[t, r1] \ {r, r1} =
        let len = size(s);
        let arr = Array.empty(rc, len);
        forEachWithIndex((i, x) -> Array.put(x, i, arr), s);
        arr

    ///
    /// Returns the mutable set `s` as a list.
    ///
    pub def toList(s: MutHashSet[t, r]): List[t] \ r =
        MutHashMap.foldRightWithKey((k, _, acc) -> k :: acc, Nil, s->inner)

    ///
    /// Returns the association set `s` as a map.
    ///
    /// If `s` contains multiple mappings with the same key, `toMap` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    pub def toMap(s: MutHashSet[(a, b), r]): Map[a, b] \ r with Order[a] =
        foldLeft((acc, pair) -> match pair {
            case (k, v) => Map.insert(k, v, acc)
        }, Map.empty(), s)

    ///
    /// Returns the mutable set `s` as an immutable set.
    ///
    pub def toSet(s: MutHashSet[t, r]): Set[t] \ r with Order[t] =
        MutHashMap.keysOf(s->inner)

    ///
    /// Returns the mutable set `s` as a vector.
    ///
    pub def toVector(s: MutHashSet[t, r]): Vector[t] \ r = region rc {
        let arr = toArray(rc, s);
        Array.toVector(arr)
    }

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `s` with `sep` inserted between each element.
    ///
    pub def join(sep: String, s: MutHashSet[t, r]): String \ r with ToString[t] =
        MutHashMap.joinKeys(sep, s->inner)

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `s` according to `f` with `sep` inserted between each element.
    ///
    pub def joinWith(f: t -> String \ ef, sep: String, s: MutHashSet[t, r]): String \ {ef, r} =
        let strs = foldRight((x, acc) -> f(x) :: acc, Nil, s);
        String.intercalate(sep, strs)

    ///
    /// Returns a string representation of the given mutable hash set `s`.
    ///
    pub def toString(s: MutHashSet[a, r]): String \ r with ToString[a] = region rc {
        "MutHashSet#{" + (MutHashSet.iterator(rc, s) |> Iterator.join(", ")) + "}"
    }

}
