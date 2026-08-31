/*
 * Copyright 2026 Flix Authors
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

pub mod Nes {

    ///
    /// A non-empty set.
    ///
    /// `Nes(x, xs)` represents `Set.insert(x, xs)`.
    ///
    pub enum Nes[a] {
        case Nes(a, Set[a])
    }

    instance Eq[Nes[a]] with Order[a] {
        pub def eq(n1: Nes[a], n2: Nes[a]): Bool = Nes.toSet(n1) == Nes.toSet(n2)
    }

    instance Order[Nes[a]] with Order[a] {
        pub def compare(n1: Nes[a], n2: Nes[a]): Comparison = Nes.toSet(n1) <=> Nes.toSet(n2)
    }

    instance Hash[Nes[a]] with Order[a], Hash[a] {
        pub def hash(n: Nes[a]): Int32 = Hash.hash(Nes.toSet(n))
    }

    instance ToString[Nes[a]] with Order[a], ToString[a] {
        pub def toString(n: Nes[a]): String = Nes.toString(n)
    }

    instance Formattable[Nes[a]] with Order[a], Formattable[a] {
        type Aef = Formattable.Aef[a]

        pub def format(n: Nes[a]): RichString \ Formattable.Aef[a] =
            RichString.fromString("Nes#{") + RichString.joinWith(Formattable.format, RichString.fromString(", "), Nes.toSet(n)) + RichString.fromString("}")
    }

    instance SemiGroup[Nes[a]] with Order[a] {
        pub def combine(x: Nes[a], y: Nes[a]): Nes[a] = Nes.union(x, y)
    }

    instance CommutativeSemiGroup[Nes[a]] with Order[a]

    instance Iterable[Nes[a]] with Order[a] {
        type Elm = a
        pub def iterator(rc: Region[r], n: Nes[a]): Iterator[a, r, r] \ r = Nes.iterator(rc, n)
    }

    instance ForEach[Nes[a]] with Order[a] {
        type Elm = a
        pub def forEach(f: a -> Unit \ ef, n: Nes[a]): Unit \ ef = Nes.forEach(f, n)
    }

    ///
    /// Returns `s` as a `Nes`. Crashes if `s` is empty.
    ///
    def unsafeFromSet(s: Set[a]): Nes[a] with Order[a] =
        match Set.minimum(s) {
            case Some(m) => Nes(m, Set.remove(m, s))
            case None    => unreachable!()
        }

    ///
    /// Returns a string representation of the given non-empty set `n`.
    ///
    pub def toString(n: Nes[a]): String with Order[a], ToString[a] = region rc {
        "Nes#{" + (Set.iterator(rc, toSet(n)) |> Iterator.join(", ")) + "}"
    }

    ///
    /// Returns a new non-empty set containing the single element `x`.
    ///
    pub def singleton(x: a): Nes[a] = Nes(x, Set.empty())

    ///
    /// Returns `n` as a `Set`.
    ///
    pub def toSet(n: Nes[a]): Set[a] with Order[a] =
        let Nes(x, xs) = n;
        Set.insert(x, xs)

    ///
    /// Returns the number of elements in `n`.
    ///
    pub def size(n: Nes[a]): Int32 with Order[a] = Set.size(toSet(n))

    ///
    /// Returns true if and only if `x` is a member of `n`.
    ///
    pub def memberOf(x: a, n: Nes[a]): Bool with Order[a] = Set.memberOf(x, toSet(n))

    ///
    /// Returns the smallest element of `n` according to the `Order` on `a`.
    ///
    pub def minimum(n: Nes[a]): a with Order[a] =
        match Set.minimum(toSet(n)) {
            case Some(m) => m
            case None    => unreachable!()
        }

    ///
    /// Returns the largest element of `n` according to the `Order` on `a`.
    ///
    pub def maximum(n: Nes[a]): a with Order[a] =
        match Set.maximum(toSet(n)) {
            case Some(m) => m
            case None    => unreachable!()
        }

    ///
    /// Returns `n` with `x` inserted.
    ///
    pub def insert(x: a, n: Nes[a]): Nes[a] with Order[a] =
        let Nes(y, ys) = n;
        Nes(x, Set.insert(y, ys))

    ///
    /// Returns the union of `n1` and `n2`.
    ///
    pub def union(n1: Nes[a], n2: Nes[a]): Nes[a] with Order[a] =
        let Nes(x1, xs1) = n1;
        Nes(x1, Set.union(xs1, toSet(n2)))

    ///
    /// Returns the intersection of `n1` and `n2`.
    ///
    /// The result may be empty.
    ///
    pub def intersection(n1: Nes[a], n2: Nes[a]): Set[a] with Order[a] =
        Set.intersection(toSet(n1), toSet(n2))

    ///
    /// Returns the difference of `n1` and `n2`, i.e. `n1 - n2`.
    ///
    /// The result may be empty.
    ///
    pub def difference(n1: Nes[a], n2: Nes[a]): Set[a] with Order[a] =
        Set.difference(toSet(n1), toSet(n2))

    ///
    /// Returns true if and only if every element in `n1` appears in `n2`.
    ///
    pub def isSubsetOf(n1: Nes[a], n2: Nes[a]): Bool with Order[a] =
        Set.isSubsetOf(toSet(n1), toSet(n2))

    ///
    /// Returns true if and only if every element in `n1` appears in `n2` and `n1 != n2`.
    ///
    pub def isProperSubsetOf(n1: Nes[a], n2: Nes[a]): Bool with Order[a] =
        Set.isProperSubsetOf(toSet(n1), toSet(n2))

    ///
    /// Alias for `findLeft`.
    ///
    pub def find(f: a -> Bool \ ef, n: Nes[a]): Option[a] \ ef with Order[a] = findLeft(f, n)

    ///
    /// Optionally returns the smallest element of `n` that satisfies the predicate `f`.
    ///
    pub def findLeft(f: a -> Bool \ ef, n: Nes[a]): Option[a] \ ef with Order[a] =
        Set.findLeft(f, toSet(n))

    ///
    /// Optionally returns the largest element of `n` that satisfies the predicate `f`.
    ///
    pub def findRight(f: a -> Bool \ ef, n: Nes[a]): Option[a] \ ef with Order[a] =
        Set.findRight(f, toSet(n))

    ///
    /// Returns the result of applying `combine` to all the elements in `n`, using `empty` as the initial value.
    ///
    pub def fold(n: Nes[a]): a with Order[a], Monoid[a] = Set.fold(toSet(n))

    ///
    /// Applies `f` to a start value `s` and all elements in `n` going from left to right.
    ///
    pub def foldLeft(f: (b, a) -> b \ ef, s: b, n: Nes[a]): b \ ef with Order[a] =
        Set.foldLeft(f, s, toSet(n))

    ///
    /// Applies `f` to a start value `s` and all elements in `n` going from right to left.
    ///
    pub def foldRight(f: (a, b) -> b \ ef, s: b, n: Nes[a]): b \ ef with Order[a] =
        Set.foldRight(f, s, toSet(n))

    ///
    /// Returns the result of mapping each element and combining the results.
    ///
    pub def foldMap(f: a -> b \ ef, n: Nes[a]): b \ ef with Order[a], Monoid[b] =
        Set.foldMap(f, toSet(n))

    ///
    /// Applies `combine` to all elements in `n` until a single value is obtained.
    ///
    pub def reduce(n: Nes[a]): a with Order[a], SemiGroup[a] =
        match Set.reduceLeft(SemiGroup.combine, toSet(n)) {
            case Some(v) => v
            case None    => unreachable!()
        }

    ///
    /// Returns the number of elements in `n` that satisfy the predicate `f`.
    ///
    pub def count(f: a -> Bool \ ef, n: Nes[a]): Int32 \ ef with Order[a] =
        Set.count(f, toSet(n))

    ///
    /// Returns `true` if and only if at least one element in `n` satisfies the predicate `f`.
    ///
    pub def exists(f: a -> Bool \ ef, n: Nes[a]): Bool \ ef with Order[a] =
        Set.exists(f, toSet(n))

    ///
    /// Returns `true` if and only if all elements in `n` satisfy the predicate `f`.
    ///
    pub def forAll(f: a -> Bool \ ef, n: Nes[a]): Bool \ ef with Order[a] =
        Set.forAll(f, toSet(n))

    ///
    /// Returns the result of applying `f` to every element in `n`.
    ///
    pub def map(f: a -> b \ ef, n: Nes[a]): Nes[b] \ ef with Order[a], Order[b] =
        unsafeFromSet(Set.map(f, toSet(n)))

    ///
    /// Returns the set of all elements of `n` that satisfy the predicate `f`.
    ///
    /// The result may be empty.
    ///
    pub def filter(f: a -> Bool \ ef, n: Nes[a]): Set[a] \ ef with Order[a] =
        Set.filter(f, toSet(n))

    ///
    /// Returns the result of applying `f` to every element in `n` and taking the union.
    ///
    pub def flatMap(f: a -> Nes[b] \ ef, n: Nes[a]): Nes[b] \ ef with Order[a], Order[b] =
        let s = toSet(n);
        match Set.minimum(s) {
            case None => unreachable!()
            case Some(m) =>
                let first = f(m);
                Set.foldLeft((acc, y) -> union(acc, f(y)), first, Set.remove(m, s))
        }

    ///
    /// Returns `n` as a list.
    ///
    pub def toList(n: Nes[a]): List[a] with Order[a] = Set.toList(toSet(n))

    ///
    /// Returns `n` as an array.
    ///
    pub def toArray(rc: Region[r], n: Nes[a]): Array[a, r] \ r with Order[a] =
        Set.toArray(rc, toSet(n))

    ///
    /// Returns `n` as a vector.
    ///
    pub def toVector(n: Nes[a]): Vector[a] with Order[a] = Set.toVector(toSet(n))

    ///
    /// Optionally returns `s` as a non-empty set.
    ///
    /// Returns `None` if `s` is empty.
    ///
    pub def fromSet(s: Set[a]): Option[Nes[a]] with Order[a] =
        if (Set.isEmpty(s)) None else Some(unsafeFromSet(s))

    ///
    /// Optionally returns the elements of `l` as a non-empty set.
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def fromList(l: List[a]): Option[Nes[a]] with Order[a] =
        fromSet(List.toSet(l))

    ///
    /// Applies `f` to every element of `n`.
    ///
    pub def forEach(f: a -> Unit \ ef, n: Nes[a]): Unit \ ef with Order[a] =
        Set.forEach(f, toSet(n))

    ///
    /// Applies `f` to every element of `n` along with that element's index.
    ///
    pub def forEachWithIndex(f: (Int32, a) -> Unit \ ef, n: Nes[a]): Unit \ ef with Order[a] =
        Set.forEachWithIndex(f, toSet(n))

    ///
    /// Returns an iterator over `n`.
    ///
    pub def iterator(rc: Region[r], n: Nes[a]): Iterator[a, r, r] \ r with Order[a] =
        Set.iterator(rc, toSet(n))

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `n` with `sep` inserted between each element.
    ///
    pub def join(sep: String, n: Nes[a]): String with Order[a], ToString[a] =
        Set.join(sep, toSet(n))

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `n` according to `f` with `sep` inserted between each element.
    ///
    pub def joinWith(f: a -> String \ ef, sep: String, n: Nes[a]): String \ ef with Order[a] =
        Set.joinWith(f, sep, toSet(n))

}
