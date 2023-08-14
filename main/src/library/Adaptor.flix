/*
 *  Copyright 2023 Stephen Tetley
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

mod Adaptor {

    ///
    /// Returns a comparator for elements of type `a`.
    ///
    /// The comparator is backed by the ordering on `a`.
    ///
    /// Note: Currently requires a Proxy (until Java types support generics).
    ///
    pub def comparator(_: Proxy[a]): ##java.util.Comparator \ IO with Order[a] =
        let f1 = (o1, o2) -> {
            let a = unchecked_cast(o1 as a);
            let b = unchecked_cast(o2 as a);
            Order.compare(a, b) |> Comparison.toInt
        };
        new ##java.util.Comparator {
            def compare(_this: ##java.util.Comparator, t: ##java.lang.Object, u: ##java.lang.Object): Int32 =
                f1(t, u)
        }

    ///
    /// Returns all elements in the given Java List as a Flix List.
    ///
    pub def fromList(l: ##java.util.List): List[a] = region rc {
        import java.util.List.iterator(): ##java.util.Iterator \ rc;
        iterator(l) |> fromIterator(rc, Proxy.Proxy: Proxy[a]) |> Iterator.toList
    }

    ///
    /// Returns all elements in the given Java Set as a Flix Set.
    ///
    /// WARNING: The Flix Set will use the ordering defined on `a`.
    ///
    pub def fromSet(l: ##java.util.Set): Set[a] with Order[a] = region rc {
        import java.util.Set.iterator(): ##java.util.Iterator \ rc;
        iterator(l) |> fromIterator(rc, Proxy.Proxy: Proxy[a]) |> Iterator.toSet
    }

    ///
    /// Returns all key-value pairs in the given Java Map as a Flix Map.
    ///
    /// WARNING: The Flix Map will use the ordering defined on `k`.
    ///
    pub def fromMap(m: ##java.util.Map): Map[k, v] with Order[k] = region rc {
        def step(acc, entry) = {
            let (k, v) = entry;
            Map.insert(k, v, acc)
        };
        mapIterator(rc, m) |> Iterator.foldLeft(step, Map.empty())
    }

    ///
    /// Helper function for `fromMap`.
    ///
    def mapIterator(rc: Region[r], m: ##java.util.Map): Iterator[(k, v), r, r] \ r =
        import java.util.Map.entrySet(): ##java.util.Set \ r;
        import java.util.Set.iterator(): ##java.util.Iterator \ r;
        import java.util.Iterator.hasNext(): Bool \ r;
        import java.util.Iterator.next(): ##java.lang.Object \ r;
        import java.util.Map$Entry.getKey(): ##java.lang.Object \ {};
        import java.util.Map$Entry.getValue(): ##java.lang.Object \ {};
        let entries = entrySet(m);
        let iter = iterator(entries);
        let getNext = () -> match hasNext(iter) {
            case true  => {
                let entry = {let obj = next(iter); unchecked_cast(obj as ##java.util.Map$Entry)};
                let k = {let okey = getKey(entry); unchecked_cast(okey as k)};
                let v = {let ovalue = getValue(entry); unchecked_cast(ovalue as v)};
                Some((k, v))
            }
            case false => None
        };
        Iterator.iterate(rc, getNext)

    ///
    /// Returns a fresh Flix `Iterator` from the Java iterator `iter`.
    ///
    pub def fromIterator(rc: Region[r], _: Proxy[a], iter: ##java.util.Iterator): Iterator[a, r, r] =
        import java.util.Iterator.hasNext(): Bool \ r;
        import java.util.Iterator.next(): ##java.lang.Object \ r;
        let step = () -> {
            match hasNext(iter) {
                case true  => next(iter) |> (o -> unchecked_cast(o as a)) |> Some
                case false => None
            }
        };
        let iterF = () -> checked_ecast(step());
        Iterator.iterate(rc, iterF)

    ///
    /// Alias for `toArrayList`.
    ///
    pub def toList(ma: m[a]): ##java.util.List \ IO with Foldable[m] = checked_cast(toArrayList(ma))

    ///
    /// Returns the elements of the given foldable `ma` as a new Java `ArrayList`.
    ///
    /// Creates a fresh `ArrayList` and copies all elements in `ma` into it.
    ///
    pub def toArrayList(ma: m[a]): ##java.util.ArrayList \ IO with Foldable[m] =
        import new java.util.ArrayList(): ##java.util.ArrayList \ IO as newArrayList;
        import java.util.ArrayList.add(##java.lang.Object): Bool \ IO;
        let alist = newArrayList();
        Foldable.forEach(x -> discard add(alist, unchecked_cast(x as ##java.lang.Object)), ma);
        alist

    ///
    /// Returns the elements of the given foldable `ma` as a new Java `LinkedList`.
    ///
    /// Creates a fresh `LinkedList` and copies all elements in `ma` into it.
    ///
    pub def toLinkedList(ma: m[a]): ##java.util.LinkedList \ IO with Foldable[m] =
        import new java.util.LinkedList(): ##java.util.LinkedList \ IO as newLinkedList;
        import java.util.LinkedList.add(##java.lang.Object): Bool \ IO;
        let llist = newLinkedList();
        Foldable.forEach(x -> discard add(llist, unchecked_cast(x as ##java.lang.Object)), ma);
        llist

    ///
    /// Alias for `toTreeSet`.
    ///
    pub def toSet(ma: m[a]): ##java.util.Set \ IO with Order[a], Foldable[m] = checked_cast(toTreeSet(ma))

    ///
    /// Returns the elements of the given foldable `ma` as a new `TreeSet`.
    ///
    pub def toTreeSet(ma: m[a]): ##java.util.TreeSet \ IO with Order[a], Foldable[m] =
        import new java.util.TreeSet(): ##java.util.TreeSet \ IO as newTreeSet;
        import java.util.TreeSet.add(##java.lang.Object): Bool \ IO;
        let tset = newTreeSet();
        Foldable.forEach(x -> discard add(tset, unchecked_cast(x as ##java.lang.Object)), ma);
        tset

    ///
    /// Alias for `toTreeMap`.
    ///
    pub def toMap(m: Map[k, v]): ##java.util.Map \ IO with Order[k] = checked_cast(toTreeMap(m))

    ///
    /// Returns all key-value pairs of the given map `m` as a new `TreeMap`.
    ///
    /// The `TreeMap` uses a `Comparator` constructed from the `Order` on `k`.
    ///
    pub def toTreeMap(m: Map[k, v]): ##java.util.TreeMap \ IO with Order[k] =
        import new java.util.TreeMap(): ##java.util.TreeMap \ IO as newTreeMap;
        import java.util.TreeMap.put(##java.lang.Object, ##java.lang.Object): ##java.lang.Object \ IO;
        let tmap = newTreeMap();
        Map.forEach((k, v) -> discard put(tmap, unchecked_cast(k as ##java.lang.Object), unchecked_cast(v as ##java.lang.Object)), m);
        tmap

}
