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

    import java.lang.Object
    import java.util.{ArrayList, Comparator, Iterator => JIterator,
        LinkedList, List => JList, Map => JMap, Optional, Set => JSet,
        TreeSet, TreeMap};
    import java.util.{Map$Entry => Entry}
    import java.util.stream.Stream
    import java.util.Collection

    ///
    /// Returns a comparator for elements of type `a`.
    ///
    /// The comparator is backed by the ordering on `a`.
    ///
    /// Note: Currently requires a Proxy (until Java types support generics).
    ///
    pub def comparator(_: Proxy[a]): Comparator \ IO with Order[a] =
        let f1 = (o1, o2) -> {
            let a = unchecked_cast(o1 as a);
            let b = unchecked_cast(o2 as a);
            Order.compare(a, b) |> Comparison.toInt32
        };
        new Comparator {
            def compare(_this: Comparator, t: Object, u: Object): Int32 =
                f1(t, u)
        }

    ///
    /// Returns the given Java Optional as a Flix Option.
    ///
    pub def fromOptional(_: Proxy[a], o: Optional): Option[a] =
        unsafe {
            if (o.isPresent()) {
                let o1 = o.get();
                Some(unchecked_cast(o1 as a))
            } else
                None
        }

    ///
    /// Returns the given Java `Map.Entry` as a Flix tuple.
    ///
    pub def fromMapEntry(_: Proxy[k], _: Proxy[v], e: Entry): (k, v) =
        unsafe {
            let k1 = e.getKey();
            let v1 = e.getValue();
            (unchecked_cast(k1 as k), unchecked_cast(v1 as v))
        }


    ///
    /// Returns all elements in the given Java List as a Flix List.
    ///
    pub def fromList(l: JList): List[a] = region rc {
        unsafe l.iterator() |> fromIterator(rc, (Proxy.Proxy: Proxy[a])) |> Iterator.toList
    }

    ///
    /// Returns a Flix Iterator of the elements in the given Java List.
    ///
    pub def fromListToIterator(rc: Region[r], prx: Proxy[a], l: JList): Iterator[a, r, r] =
        unsafe l.iterator() |> fromIterator(rc, prx)

    ///
    /// Returns all elements in the given Java Set as a Flix Set.
    ///
    /// WARNING: The Flix Set will use the ordering defined on `a`.
    ///
    pub def fromSet(l: JSet): Set[a] with Order[a] = region rc {
        unsafe l.iterator() |> fromIterator(rc, (Proxy.Proxy: Proxy[a])) |> Iterator.toSet
    }

    ///
    /// Returns a Flix Iterator of the elements in the given Java Set.
    ///
    pub def fromSetToIterator(rc: Region[r], prx: Proxy[a], s: JSet): Iterator[a, r, r] =
        unsafe s.iterator() |> fromIterator(rc, prx)

    ///
    /// Returns all key-value pairs in the given Java Map as a Flix Map.
    ///
    /// WARNING: The Flix Map will use the ordering defined on `k`.
    ///
    pub def fromMap(m: JMap): Map[k, v] with Order[k] = region rc {
        def step(acc, entry) = {
            let (k, v) = entry;
            Map.insert(k, v, acc)
        };
        fromMapToIterator(rc, m) |> Iterator.foldLeft(step, Map.empty())
    }

    ///
    /// Returns a Flix Iterator of the key-value pairs in the given Java Map.
    ///
    pub def fromMapToIterator(rc: Region[r], m: JMap): Iterator[(k, v), r, r] =
        unsafe {
            let entries = m.entrySet();
            let iter = entries.iterator();
            let getNext = () -> match iter.hasNext() {
                case true  => {
                    let entry = {let obj = iter.next(); unchecked_cast(obj as Entry)};
                    let (k, v) = fromMapEntry((Proxy.Proxy: Proxy[k]), (Proxy.Proxy: Proxy[v]), entry);
                    Some((k, v))
                }
                case false => None
            };
            Iterator.iterate(rc, getNext)
        }

    ///
    /// Returns a fresh Flix `Iterator` from the Java iterator `iter`.
    ///
    pub def fromIterator(rc: Region[r], _: Proxy[a], iter: JIterator): Iterator[a, r, r] =
        unsafe {
            let step = () -> {
                match iter.hasNext() {
                    case true  => iter.next() |> (o -> unchecked_cast(o as a)) |> Some
                    case false => None
                }
            };
            let iterF = () -> (step());
            Iterator.iterate(rc, iterF)
        }

    ///
    /// Returns a Flix Iterator of the elements in the given Java Stream.
    ///
    pub def fromStreamToIterator(rc: Region[r], proxy: Proxy[a], strm: Stream): Iterator[a, r, r] =
        let iter = unsafe checked_cast(strm).iterator();
        Adaptor.fromIterator(rc, proxy, iter)

    ///
    /// Returns a Flix Iterator of the elements in the given Java Collection.
    ///
    pub def fromCollectionToIterator(rc: Region[r], proxy: Proxy[a], col: Collection): Iterator[a, r, r] =
        import java.util.Collection.iterator(): ##java.util.Iterator \ {};
        let iter = iterator(col);
        Adaptor.fromIterator(rc, proxy, iter)

    ///
    /// Returns the `o` as a new Java `Optional`.
    ///
    pub def toOptional(o: Option[a]): Optional \ IO =
        import static java.util.Optional.empty(): Optional \ IO;
        import static java.util.Optional.of(Object): Optional \ IO;
        match o {
            case None    => empty()
            case Some(a) => of(unchecked_cast(a as Object))
        }

    ///
    /// Returns the key-value pair `p` as a new Java `Map.Entry`.
    ///
    pub def toMapEntry(p: (k, v)): Entry \ IO =
        import static java.util.Map.entry(Object, Object): Entry \ IO;
        let (k, v) = p;
        entry(unchecked_cast(k as Object), unchecked_cast(v as Object))

    ///
    /// Alias for `toArrayList`.
    ///
    pub def toList(ma: m[a]): JList \ (IO + Foldable.Aef[m]) with Foldable[m] =
        checked_cast(toArrayList(ma))

    ///
    /// Returns the elements of the given foldable `ma` as a new Java `ArrayList`.
    ///
    /// Creates a fresh `ArrayList` and copies all elements in `ma` into it.
    ///
    pub def toArrayList(ma: m[a]): ArrayList \ (IO + Foldable.Aef[m]) with Foldable[m] =
        import java.util.ArrayList.add(Object): Bool \ IO;
        let alist = new ArrayList();
        Foldable.forEach(x -> discard add(alist, unchecked_cast(x as Object)), ma);
        alist

    ///
    /// Returns the elements of the given foldable `ma` as a new Java `LinkedList`.
    ///
    /// Creates a fresh `LinkedList` and copies all elements in `ma` into it.
    ///
    pub def toLinkedList(ma: m[a]): LinkedList \ (IO + Foldable.Aef[m]) with Foldable[m] =
        import java.util.LinkedList.add(Object): Bool \ IO;
        let llist = new LinkedList();
        Foldable.forEach(x -> discard add(llist, unchecked_cast(x as Object)), ma);
        llist

    ///
    /// Alias for `toTreeSet`.
    ///
    pub def toSet(ma: m[a]): JSet \ (IO + Foldable.Aef[m]) with Order[a], Foldable[m] = checked_cast(toTreeSet(ma))

    ///
    /// Returns the elements of the given foldable `ma` as a new `TreeSet`.
    ///
    pub def toTreeSet(ma: m[a]): TreeSet \ (IO + Foldable.Aef[m]) with Order[a], Foldable[m] =
        import java.util.TreeSet.add(Object): Bool \ IO;
        let tset = new TreeSet();
        Foldable.forEach(x -> discard add(tset, unchecked_cast(x as Object)), ma);
        tset

    ///
    /// Alias for `toTreeMap`.
    ///
    pub def toMap(m: Map[k, v]): JMap \ IO with Order[k] = checked_cast(toTreeMap(m))

    ///
    /// Returns all key-value pairs of the given map `m` as a new `TreeMap`.
    ///
    /// The `TreeMap` uses a `Comparator` constructed from the `Order` on `k`.
    ///
    pub def toTreeMap(m: Map[k, v]): TreeMap \ IO with Order[k] =
        import java.util.TreeMap.put(Object, Object): Object \ IO;
        let tmap = new TreeMap();
        Map.forEach((k, v) -> discard put(tmap, unchecked_cast(k as Object), unchecked_cast(v as Object)), m);
        tmap


}
