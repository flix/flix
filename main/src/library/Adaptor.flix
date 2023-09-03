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
    /// Returns the given Java Optional as a Flix Option.
    ///
    pub def fromOptional(_: Proxy[a], o: ##java.util.Optional): Option[a] =
        import java.util.Optional.isPresent(): Bool \ {};
        import java.util.Optional.get(): ##java.lang.Object \ {};
        if (isPresent(o)) {
            let o1 = get(o);
            Some(unchecked_cast(o1 as a))
        } else
            None

    ///
    /// Returns the given Java Optional as a Flix Option.
    ///
    pub def fromMapEntry(_: Proxy[k], _: Proxy[v], e: ##java.util.Map$Entry): (k, v) =
        import java.util.Map$Entry.getKey(): ##java.lang.Object \ {};
        import java.util.Map$Entry.getValue(): ##java.lang.Object \ {};
        let k1 = getKey(e);
        let v1 = getValue(e);
        (unchecked_cast(k1 as k), unchecked_cast(v1 as v))


    ///
    /// Returns all elements in the given Java List as a Flix List.
    ///
    pub def fromList(l: ##java.util.List): List[a] = region rc {
        import java.util.List.iterator(): ##java.util.Iterator \ rc;
        iterator(l) |> fromIterator(rc, Proxy.Proxy: Proxy[a]) |> Iterator.toList
    }

    ///
    /// Returns a Flix Iterator of the elements in the given Java List.
    ///
    pub def fromListToIterator(rc: Region[r], prx: Proxy[a], l: ##java.util.List): Iterator[a, r, r] \ r =
        import java.util.List.iterator(): ##java.util.Iterator \ r;
        iterator(l) |> fromIterator(rc, prx)

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
    /// Returns a Flix Iterator of the elements in the given Java Set.
    ///
    pub def fromSetToIterator(rc: Region[r], prx: Proxy[a], s: ##java.util.Set): Iterator[a, r, r] \ r =
        import java.util.Set.iterator(): ##java.util.Iterator \ r;
        iterator(s) |> fromIterator(rc, prx)

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
        fromMapToIterator(rc, m) |> Iterator.foldLeft(step, Map.empty())
    }

    ///
    /// Returns a Flix Iterator of the key-value pairs in the given Java Map.
    ///
    pub def fromMapToIterator(rc: Region[r], m: ##java.util.Map): Iterator[(k, v), r, r] \ r =
        import java.util.Map.entrySet(): ##java.util.Set \ r;
        import java.util.Set.iterator(): ##java.util.Iterator \ r;
        import java.util.Iterator.hasNext(): Bool \ r;
        import java.util.Iterator.next(): ##java.lang.Object \ r;
        let entries = entrySet(m);
        let iter = iterator(entries);
        let getNext = () -> match hasNext(iter) {
            case true  => {
                let entry = {let obj = next(iter); unchecked_cast(obj as ##java.util.Map$Entry)};
                let (k, v) = fromMapEntry(Proxy.Proxy: Proxy[k], Proxy.Proxy: Proxy[v], entry);
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
    /// Returns a Flix Iterator of the elements in the given Java Stream.
    ///
    pub def fromStreamToIterator(rc: Region[r], proxy: Proxy[a], strm: ##java.util.stream.Stream): Iterator[a, r, r] =
        import java.util.stream.BaseStream.iterator(): ##java.util.Iterator \ {};
        let iter = iterator(unchecked_cast(strm as ##java.util.stream.BaseStream));
        Adaptor.fromIterator(rc, proxy, iter)

    ///
    /// Returns a Flix Iterator of the elements in the given Java Collection.
    ///
    pub def fromCollectionToIterator(rc: Region[r], proxy: Proxy[a], col: ##java.util.Collection): Iterator[a, r, r] =
        import java.util.Collection.iterator(): ##java.util.Iterator \ {};
        let iter = iterator(col);
        Adaptor.fromIterator(rc, proxy, iter)

    ///
    /// Returns the `o` as a new Java `Optional`.
    ///
    pub def toOptional(o: Option[a]): ##java.util.Optional \ IO =
        import static java.util.Optional.empty(): ##java.util.Optional \ IO;
        import static java.util.Optional.of(##java.lang.Object): ##java.util.Optional \ IO;
        match o {
            case None    => empty()
            case Some(a) => of(unchecked_cast(a as ##java.lang.Object))
        }

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
