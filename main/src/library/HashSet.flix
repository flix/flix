/*
 * Copyright 2017 Liam Palmer, Esben Bjerre
 * Copyright 2024 Jonathan Lindegaard Starup
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
/// The HashSet type.
///
/// A set is currently represented internally as a red-black tree.
///
pub enum HashSet[t] with Sendable, Eq {
    case HashSet(RedBlackTree[Int32, HashSet.ListSet[t]])
}

instance ToString[HashSet[t]] with ToString[t] {
    pub def toString(x: HashSet[t]): String = HashSet.toString(x)
}

instance Collectable[HashSet[a]] with Eq[a], Hash[a] {
    type Elm = a
    pub def collect(iter: Iterator[a, ef, r]): HashSet[a] \ { ef, r } =
        HashSet.collectI(iter)
}

instance Hash[HashSet[t]] with Hash[t] {
    pub def hash(s: HashSet[t]): Int32 =
        HashSet.foldLeft((acc, x) -> acc `Hash.combine` Hash.hash(x), Hash.magic(), s)
}

instance SemiGroup[HashSet[a]] with Eq[a], Hash[a] {
    pub def combine(x: HashSet[a], y: HashSet[a]): HashSet[a] = HashSet.union(x,y)
}

instance Monoid[HashSet[a]] with Eq[a], Hash[a] {
    pub def empty(): HashSet[a] = HashSet.empty()
}

instance Foldable[HashSet] {
    pub def foldLeft(f: (b, a) -> b \ ef, s: b, s1: HashSet[a]): b \ ef = HashSet.foldLeft(f, s, s1)
    pub def foldRight(f: (a, b) -> b \ ef, s: b, s1: HashSet[a]): b \ ef = HashSet.foldRight(f, s, s1)
    pub def foldRightWithCont(f: (a, Unit -> b \ ef) -> b \ ef, s: b, s1: HashSet[a]): b \ ef = HashSet.foldRightWithCont(f, s, s1)
}

instance Iterable[HashSet[a]] {
    type Elm = a
    pub def iterator(rc: Region[r], s: HashSet[a]): Iterator[a, r, r] \ r = HashSet.iterator(rc, s)
}

mod RedBlackTree {
    /// removes `k` if `f` return `None`
    pub def updateFilter(f: (k, v) -> Option[v] \ ef, k: k, t: RedBlackTree[k, v]): RedBlackTree[k, v] \ ef with Order[k] = {
        match get(k, t) {
            case Some(v) => match f(k, v) {
                case None => RedBlackTree.remove(k, t)
                case Some(v1) => RedBlackTree.insert(k, v1, t)
            }
            case None => t
        }
    }
}

mod HashSet {
    use ListSet.liftT
    use ListSet.lift

    /// Invariant: the list has no duplicates
    pub enum ListSet[t](List[t])

    instance Eq[ListSet[t]] with Eq[t] {
        pub def eq(x: ListSet[t], y: ListSet[t]): Bool = ListSet.eq(x, y)
    }

    mod ListSet {
        pub def empty(): ListSet[t] = ListSet(List.empty())

        pub def singleton(x: t): ListSet[t] = ListSet(List#{x})

        pub def isEmpty(s: ListSet[t]): Bool = match list(s) {
            case Nil => true
            case _ => false
        }

        pub def list(s: ListSet[t]): List[t] = {
            let ListSet(l) = s;
            l
        }

        pub def lift(f: List[t] -> List[r] \ ef): ListSet[t] -> ListSet[r] \ ef = {
            s -> ListSet(f(list(s)))
        }

        pub def liftT(f: List[t] -> b \ ef): ListSet[t] -> b \ ef = {
            s -> f(list(s))
        }

        pub def reduce(f: t -> t -> t \ ef, s: ListSet[t]): Option[t] \ ef = {
            List.reduceLeft(f, list(s))
        }

        pub def insert(x: t, s: ListSet[t]): ListSet[t] with Eq[t] =
            if (list(s) |> List.memberOf(x)) s
            else ListSet(x :: list(s))

        pub def insertAll(xs: List[t], s: ListSet[t]): ListSet[t] with Eq[t] = match xs {
            case Nil => s
            case hd :: tl => insertAll(tl, insert(hd, s))
        }

        pub def union(x: ListSet[t], y: ListSet[t]): ListSet[t] with Eq[t] = {
            let len = liftT(List.length);
            if (len(x) < len(y)) y |> insertAll(list(x))
            else x |> insertAll(list(y))
        }

        pub def remove(x: t, s: ListSet[t]): ListSet[t] with Eq[t] = {
            def aux(l, acc) = match l {
                case Nil => s
                case hd :: tl if hd == x =>
                    ListSet(List.reverse(acc) `List.append` tl)
                case hd :: tl =>
                    aux(tl, hd :: acc)
            };
            aux(list(s), Nil)
        }

        pub def eq(x: ListSet[t], y: ListSet[t]): Bool with Eq[t] = {
            def aux(x1: List[t], y1: List[t]): Bool = match x1 {
                case Nil => y1 == Nil
                case hd :: tl => match removeChecked(hd, y1) {
                    case None => false
                    case Some(y2) => aux(tl, y2)
                }
            };
            aux(list(x), list(y))
        }

        pub def removeChecked(x: t, s: List[t]): Option[List[t]] with Eq[t] = {
            def aux(l, acc) = match l {
                case Nil => None
                case hd :: tl if hd == x =>
                    Some(List.reverse(acc) `List.append` tl)
                case hd :: tl =>
                    aux(tl, hd :: acc)
            };
            aux(s, Nil)
        }
    }

    ///
    /// Returns the size of `s`.
    ///
    pub def size(s: HashSet[a]): Int32 =
        let HashSet(t) = s;
        t |> RedBlackTree.foldLeft(acc -> _hash -> curr -> acc + liftT(List.length)(curr), 0)

    pub def toString(s: HashSet[a]): String with ToString[a] = region local {
        let inner = iterator(local, s) |> Iterator.join(", ");
        "HashSet(${inner})"
    }


    ///
    /// Returns the empty set.
    ///
    /// `HashSet#{}` is syntactic sugar for `empty` i.e. `HashSet#{} == empty()`.
    ///
    pub def empty(): HashSet[a] = HashSet(RedBlackTree.empty())

    ///
    /// Adds `x` to `s`.
    ///
    pub def insert(x: a, s: HashSet[a]): HashSet[a] with Eq[a], Hash[a] =
        let HashSet(t) = s;
        HashSet(t |> RedBlackTree.insertWith(_ -> _ -> l -> ListSet.insert(x, l), Hash.hash(x), ListSet.singleton(x)))

    ///
    /// Removes `x` from `s`.
    ///
    pub def remove(x: a, s: HashSet[a]): HashSet[a] with Eq[a], Hash[a] =
        let HashSet(t) = s;
        HashSet((Hash.hash(x), t) ||> RedBlackTree.updateFilter(_ -> l -> {
            match ListSet.remove(x, l) {
                case ListSet.ListSet(Nil) => None
                case other => Some(other)
            }
        }))

    ///
    /// Returns true if and only if `s` is the empty set.
    ///
    pub def isEmpty(s: HashSet[a]): Bool =
        let HashSet(t) = s;
        RedBlackTree.isEmpty(t)

    ///
    /// Returns true if and only if `s` is a non-empty set.
    ///
    pub def nonEmpty(s: HashSet[a]): Bool = not isEmpty(s)

    ///
    /// Returns the singleton set containing `x`.
    ///
    /// `HashSet#{x}` is syntactic sugar for `singleton` i.e. `HashSet#{x} == singleton(x)`.
    ///
    pub def singleton(x: a): HashSet[a] with Eq[a], Hash[a] = insert(x, empty())


    pub def collectI(i: Iterator[a, ef, r]): HashSet[a] \ ef + r with Eq[a], Hash[a] =
        i |> Iterator.foldLeft(acc -> curr -> insert(curr, acc), empty())

    pub def collect(l: List[t]): HashSet[t] with Eq[t], Hash[t] = region local {
        l |> List.iterator(local) |> collectI
    }

    ///
    /// Returns a set of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns `empty()` if `b >= e`.
    ///
    pub def range(b: Int32, e: Int32): HashSet[Int32] =
        def loop(i, s) = {
            if (i >= e)
                s
            else
                loop(i + 1, insert(i, s))
        };
        loop(b, empty())

    ///
    /// Returns true if and only if `x` is a member of `s`.
    ///
    pub def memberOf(x: a, s: HashSet[a]): Bool with Eq[a], Hash[a] =
        let HashSet(t) = s;
        match RedBlackTree.get(Hash.hash(x), t) {
            case None => false
            case Some(l) => liftT(List.memberOf(x))(l)
        }

    ///
    /// Returns true if and only if every element in `s1` appears in `s2`.
    ///
    pub def isSubsetOf(s1: HashSet[a], s2: HashSet[a]): Bool with Eq[a], Hash[a] = forAll(x -> memberOf(x, s2), s1)

    ///
    /// Returns true if and only if every element in `s1` appears in `s2` and `s != s2`.
    ///
    pub def isProperSubsetOf(s1: HashSet[a], s2: HashSet[a]): Bool with Eq[a], Hash[a] = size(s1) != size(s2) and isSubsetOf(s1, s2)

    ///
    /// Optionally returns the first element of `s` that satisfies the predicate `f`.
    ///
    pub def find(f: a -> Bool \ ef, s: HashSet[a]): Option[a] \ ef =
        // TODO make exit fast
        let HashSet(t) = s;
        def g(running, curr) = match running {
            case None => curr |> liftT(List.findLeft(f))
            case Some(_) => running
        };
        RedBlackTree.foldLeft((acc, _, l) -> g(acc, l), None, t)

    ///
    /// Returns the result of applying `combine` to all the elements in `s`, using `empty` as the initial value.
    ///
    pub def fold(s: HashSet[a]): a with Monoid[a] = Foldable.fold(s)

    ///
    /// Applies `f` to a start value `s` and all elements in `s` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b \ ef, s: b, s1: HashSet[a]): b \ ef =
        let HashSet(t) = s1;
        RedBlackTree.foldLeft((acc, _, l) -> liftT(List.foldLeft(f, acc))(l), s, t)

    ///
    /// Applies `f` to a start value `s` and all elements in `s1` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    pub def foldRight(f: (a, b) -> b \ ef, s: b, s1: HashSet[a]): b \ ef =
        let HashSet(t) = s1;
        RedBlackTree.foldRight((_, l, acc) -> liftT(List.foldRight(f, acc))(l), s, t)

    ///
    /// Applies `f` to a start value `z` and all elements in `s` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, z))...)`.
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithCont(f: (a, Unit -> b \ ef) -> b \ ef, z: b, s: HashSet[a]): b \ ef =
        ???()

    ///
    /// Returns the result of mapping each element and combining the results.
    ///
    pub def foldMap(f: a -> b \ ef, s: HashSet[a]): b \ ef with Monoid[b] =
        foldLeft((acc, x) -> Monoid.combine(acc, f(x)), Monoid.empty(), s)

    ///
    /// Applies `f` to all elements in `s` until a single value `v` is obtained.  Returns `Some(v)`.
    /// Returns `None` if `s` is the empty set.
    ///
    pub def reduce(f: (a, a) -> a \ ef, s: HashSet[a]): Option[a] \ ef =
        let HashSet(t) = s;
        RedBlackTree.reduceLeft((_, x, _, y) -> {
            let fst = ListSet.reduce(f, x) |> Option.toList;
            let snd = ListSet.reduce(f, y) |> Option.toList;
            (-1, List.reduceLeft(f, fst `List.append` snd) |> Option.map(ListSet.singleton) |> Option.getWithDefault(ListSet.empty()))
            }, t) |> Option.flatMap(snd >> liftT(List.head))

    ///
    /// Returns the union of the elements in `s`.
    ///
    pub def flatten(s: HashSet[HashSet[a]]): HashSet[a] with Eq[a] =
        foldLeft((acc, x) -> union(acc, x), empty(), s)

    ///
    /// Returns `true` if and only if at least one element in `s` satisfies the predicate `f`.
    ///
    /// Returns `false` if `s` is the empty set.
    ///
    pub def exists(f: a -> Bool \ ef, s: HashSet[a]): Bool \ ef =
        let HashSet(t) = s;
        RedBlackTree.exists((_, l) -> liftT(List.exists(f))(l), t)

    ///
    /// Returns `true` if and only if all elements in `s` satisfy the predicate `f`.
    ///
    /// Returns `true` if `s` is the empty set.
    ///
    pub def forAll(f: a -> Bool \ ef, s: HashSet[a]): Bool \ ef =
        let HashSet(t) = s;
        RedBlackTree.forAll((_, l) -> liftT(List.forAll(f))(l), t)

    ///
    /// Returns the union of `s1` and `s2`.
    ///
    pub def union(s1: HashSet[a], s2: HashSet[a]): HashSet[a] with Eq[a] =
        use RedBlackTree.{blackHeight, foldLeft, insertWith};
        let HashSet(t1) = s1;
        let HashSet(t2) = s2;
        if (blackHeight(t1) < blackHeight(t2))
            HashSet(foldLeft((acc, hash, l) -> insertWith(_ -> _ -> v -> ListSet.union(l, v), hash, l, acc), t2, t1))
        else
            HashSet(foldLeft((acc, hash, l) -> insertWith(_ -> _ -> v -> ListSet.union(l, v), hash, l, acc), t1, t2))

    ///
    /// Returns the intersection of `s1` and `s2`.
    ///
    pub def intersection(s1: HashSet[a], s2: HashSet[a]): HashSet[a] with Eq[a], Hash[a] =
        foldLeft((acc, x) -> if (memberOf(x, s1)) insert(x, acc) else acc, empty(), s2)

    ///
    /// Returns the difference of `s1` and `s2`, i.e. `s1 - s2`.
    ///
    pub def difference(s1: HashSet[a], s2: HashSet[a]): HashSet[a] with Eq[a], Hash[a] =
        foldLeft((acc, x) -> if (not memberOf(x, s2)) insert(x, acc) else acc, empty(), s1)

    ///
    /// Returns all subsets of `s`.
    ///
    pub def subsets(s: HashSet[a]): HashSet[HashSet[a]] with Eq[a], Hash[a] =
        foldLeft((acc, x) -> union(map(y -> insert(x, y), acc), acc), insert(empty(), empty()), s)

    ///
    /// Returns the set of all elements of `s` that satisfy the predicate `f`.
    ///
    pub def filter(f: a -> Bool \ ef, s: HashSet[a]): HashSet[a] \ ef with Eq[a], Hash[a] =
        foldLeft((acc, x) -> if (f(x)) insert(x, acc) else acc, empty(), s)

    ///
    /// Returns the result of applying `f` to every element in `s`.
    ///
    /// Note: The returned set may be smaller than `s`.
    ///
    pub def map(f: a -> b \ ef, s: HashSet[a]): HashSet[b] \ ef with Eq[b], Hash[b] =
        foldLeft((acc, x) -> insert(f(x), acc), empty(), s)

    ///
    /// Returns the result of applying `f` to every element in `s` and taking the union.
    ///
    pub def flatMap(f: a -> HashSet[b] \ ef, s: HashSet[a]): HashSet[b] \ ef with Eq[b] =
        foldLeft((acc, x) -> union(acc, f(x)), empty(), s)

    ///
    /// Collects the results of applying the partial function `f` to every element in `s`.
    ///
    pub def filterMap(f: a -> Option[b] \ ef, s: HashSet[a]): HashSet[b] \ ef with Eq[b], Hash[b] =
        let step = (acc, x) -> match f(x) {
            case None    => acc
            case Some(v) => insert(v, acc)
        };
        foldLeft(step, empty(), s)

    ///
    /// Replaces the element `src` with `dst` if `src` is in `s`. Otherwise, returns `s`.
    ///
    /// Note: The returned set may be smaller than `s`.
    ///
    pub def replace(src: {src = a}, dst: {dst = a}, s: HashSet[a]): HashSet[a] with Eq[a], Hash[a] =
        if (memberOf(src#src, s)) insert(dst#dst, remove(src#src, s)) else s

    ///
    /// Returns a pair of sets `(s1, s2)`.
    ///
    /// `s1` contains all elements of `s` that satisfy the predicate `f`.
    /// `s2` contains all elements of `s` that do not satisfy the predicate `f`.
    ///
    pub def partition(f: a -> Bool \ ef, s: HashSet[a]): (HashSet[a], HashSet[a]) \ ef with Eq[a], Hash[a] =
        foldLeft((acc, x) ->
            let (a, b) = acc;
            if (f(x))
                (insert(x, a), b)
            else
                (a, insert(x, b)), (empty(), empty()),
            s)

    ///
    /// Returns the set `s` as a list.
    ///
    pub def toList(s: HashSet[a]): List[a] =
        foldRight((x, acc) -> x :: acc, Nil, s)

    ///
    /// Returns the set `s` as an array.
    ///
    pub def toArray(rc: Region[r], s: HashSet[a]): Array[a, r] \ r = match size(s) {
        case 0  => Array#{} @ rc
        case sz =>
            let a = Array.empty(rc, sz);
            Iterator.forEachWithIndex((i, b) -> Array.put(b, i, a), iterator(rc, s));
            a
        }

    ///
    /// Returns the set `s` as a vector.
    ///
    pub def toVector(s: HashSet[a]): Vector[a] = region rc {
        let arr = Array.empty(rc, size(s));
        Iterator.forEachWithIndex((i, x) -> Array.put(x, i, arr), iterator(rc, s));
        Array.toVector(arr)
    }

    ///
    /// Returns the set `s` as a set.
    ///
    pub def toSet(s: HashSet[a]): Set[a] with Order[a] =
        foldLeft((acc, x) -> Set.insert(x, acc), Set.empty(), s)

    ///
    /// Returns the set `s` as a chain.
    ///
    pub def toChain(s: HashSet[a]): Chain[a] =
        foldLeft((acc, x) -> Chain.snoc(acc, x), Chain.empty(), s)

    ///
    /// Returns the association set `s` as a map.
    ///
    /// If `s` contains multiple mappings with the same key, `toMap` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    pub def toMap(s: HashSet[(a, b)]): Map[a, b] with Order[a] =
        foldRight((x, acc) -> let (k, v) = x; Map.insert(k, v, acc), Map#{}, s)

    ///
    /// Returns a map with elements of `s` as keys and `f` applied as values.
    ///
    pub def toMapWith(f: a -> b, s: HashSet[a]): Map[a, b] with Order[a] =
        foldRight((x, acc) -> Map.insert(x, f(x), acc), Map.empty(), s)

    ///
    /// Returns `s` as a MutDeque.
    ///
    pub def toMutDeque(rc: Region[r], s: HashSet[a]): MutDeque[a, r] \ r =
        let d = MutDeque.empty(rc);
        forEach(x -> MutDeque.pushBack(x, d), s);
        d

    ///
    /// Applies `f` to every element of `s`.
    ///
    pub def forEach(f: a -> Unit \ ef, s: HashSet[a]): Unit \ ef =
        let HashSet(t) = s;
        RedBlackTree.forEach((_, l) -> liftT(List.forEach(f))(l), t)

    ///
    /// Build a set by applying `f` to the seed value `st`.
    ///
    /// `f` should return `Some(a,st1)` to signal a new set element `a` and a new seed value `st1`.
    ///
    /// `f` should return `None` to signal the end of building the set.
    ///
    pub def unfold(f: s -> Option[(a, s)] \ ef, st: s): HashSet[a] \ ef with Eq[a], Hash[a] =
        def loop(sst, s) = match f(sst) {
            case None         => s
            case Some(a, st1) => loop(st1, insert(a, s))
        };
        loop(st, empty())

    ///
    /// Build a set by applying the function `next` to `()`. `next` is expected to encapsulate
    /// a stateful resource such as a file handle that can be iterated.
    ///
    /// `next` should return `Some(a)` to signal a value pair `a`.
    ///
    /// `next` should return `None` to signal the end of building the set.
    ///
    pub def unfoldWithIter(next: Unit -> Option[a] \ ef): HashSet[a] \ ef with Eq[a], Hash[a] =
        def loop(s) = match next() {
            case None    => s
            case Some(a) => loop(insert(a, s))
        };
        loop(empty())

    ///
    /// Returns the set `s` as a `DelayList`.
    ///
    pub def toDelayList(s: HashSet[a]): DelayList[a] =
        foldRight((x, acc) -> DelayList.ECons(x, acc), DelayList.ENil, s)

    ///
    /// Returns an iterator over `s`.
    ///
    pub def iterator(rc: Region[r], s: HashSet[a]): Iterator[a, r, r] \ r =
        let HashSet(t) = s;
        RedBlackTree.iterator(rc, t) |> Iterator.flatMap(snd >> liftT(List.iterator(rc)))

    ///
    /// Returns an iterator over `s` zipped with the indices of the elements.
    ///
    pub def enumerator(rc: Region[r], s: HashSet[a]): Iterator[(Int32, a), r, r] \ r =
        iterator(rc, s) |> Iterator.zipWithIndex

}
