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

pub mod Adaptor {

    import java.lang.Object
    import java.util.ArrayList
    import java.util.Collection
    import java.util.Comparator
    import java.util.{Iterator => JIterator}
    import java.util.LinkedList
    import java.util.{List => JList}
    import java.util.{Map => JMap}
    import java.util.{Map$Entry => Entry}
    import java.util.Optional
    import java.util.{Set => JSet}
    import java.util.TreeMap
    import java.util.TreeSet
    import java.util.stream.BaseStream
    import java.util.stream.Stream

    ///
    /// Returns a comparator for elements of type `a`.
    ///
    /// The comparator is backed by the ordering on `a`.
    ///
    pub def comparator(_: Proxy[a]): Comparator[a] \ IO with Order[a] =
        new Comparator[a] {
            def compare(_this: Comparator[a], t: a, u: a): Int32 =
                Order.compare(t, u) |> Comparison.toInt32
        }

    ///
    /// Returns the given Java Optional as a Flix Option.
    ///
    pub def fromOptional(o: Optional[a]): Option[a] =
        if (o.isPresent()) {
            let o1 = o.get();
            Some(o1)
        } else
            None

    ///
    /// Returns the given Java `Map.Entry` as a Flix tuple.
    ///
    pub def fromMapEntry(e: Entry[k, v]): (k, v) =
        let k1 = e.getKey();
        let v1 = e.getValue();
        (k1, v1)


    ///
    /// Returns all elements in the given Java List as a Flix List.
    ///
    pub def fromList(l: JList[a]): List[a] = region rc {
        def iterator(x) = unsafe IO { x.iterator() };
        iterator(l) |> fromIterator(rc, (Proxy.Proxy: Proxy[a])) |> Iterator.toList
    }

    ///
    /// Returns a Flix Iterator of the elements in the given Java List.
    ///
    pub def fromListToIterator(rc: Region[r], l: JList[a]): Iterator[a, r, r] \ r =
        def iterator(x) = unsafe IO as r { x.iterator() };
        iterator(l) |> fromIterator(rc, (Proxy.Proxy: Proxy[a]))

    ///
    /// Returns all elements in the given Java Set as a Flix Set.
    ///
    /// WARNING: The Flix Set will use the ordering defined on `a`.
    ///
    pub def fromSet(s: JSet[a]): Set[a] with Order[a] = region rc {
        def iterator(x) = unsafe IO { x.iterator() };
        iterator(s) |> fromIterator(rc, (Proxy.Proxy: Proxy[a])) |> Iterator.toSet
    }

    ///
    /// Returns a Flix Iterator of the elements in the given Java Set.
    ///
    pub def fromSetToIterator(rc: Region[r], s: JSet[a]): Iterator[a, r, r] \ r =
        def iterator(x) = unsafe IO as r { x.iterator() };
        iterator(s) |> fromIterator(rc, (Proxy.Proxy: Proxy[a]))

    ///
    /// Returns all key-value pairs in the given Java Map as a Flix Map.
    ///
    /// WARNING: The Flix Map will use the ordering defined on `k`.
    ///
    pub def fromMap(m: JMap[k, v]): Map[k, v] with Order[k] = region rc {
        def step(acc, entry) = {
            let (k, v) = entry;
            Map.insert(k, v, acc)
        };
        fromMapToIterator(rc, m) |> Iterator.foldLeft(step, Map.empty())
    }

    ///
    /// Returns a Flix Iterator of the key-value pairs in the given Java Map.
    ///
    pub def fromMapToIterator(rc: Region[r], m: JMap[k, v]): Iterator[(k, v), r, r] \ r =
        def entrySet(x) = unsafe IO as r { x.entrySet() };
        def iterator(x) = unsafe IO as r { x.iterator() };
        def hasNext(x) = unsafe IO as r { x.hasNext() };
        def next(x) = unsafe IO as r { x.next() };
        let entries = entrySet(m);
        let iter = iterator(entries);
        let getNext = () -> match hasNext(iter) {
            case true  => {
                let entry = next(iter);
                let (k, v) = fromMapEntry(entry);
                Some((k, v))
            }
            case false => None
        };
        Iterator.unfoldWithIter(rc, getNext)

    ///
    /// Returns a fresh Flix `Iterator` from the Java iterator `iter`.
    ///
    pub def fromIterator(rc: Region[r], _: Proxy[a], iter: JIterator[a]): Iterator[a, r, r] =
        def hasNext(x) = unsafe IO as r { x.hasNext() };
        def next(x) = unsafe IO as r { x.next() };
        let step = () -> {
            match hasNext(iter) {
                case true  => next(iter) |> Some
                case false => None
            }
        };
        let iterF = () -> (step());
        Iterator.unfoldWithIter(rc, iterF)

    ///
    /// Returns a Flix Iterator of the elements in the given Java Stream.
    ///
    pub def fromStreamToIterator(rc: Region[r], strm: Stream[a]): Iterator[a, r, r] \ r =
        let baseStream: BaseStream[a, Stream[a]] = checked_cast(strm);
        let iter = unsafe IO as r { baseStream.iterator() };
        Adaptor.fromIterator(rc, (Proxy.Proxy: Proxy[a]), iter)

    ///
    /// Returns a Flix Iterator of the elements in the given Java Collection.
    ///
    pub def fromCollectionToIterator(rc: Region[r], col: Collection[a]): Iterator[a, r, r] \ r =
        let iter = unsafe IO as r { col.iterator() };
        Adaptor.fromIterator(rc, (Proxy.Proxy: Proxy[a]), iter)

    ///
    /// Returns the `o` as a new Java `Optional`.
    ///
    pub def toOptional(o: Option[a]): Optional[a] \ IO =
        match o {
            case None    => Optional.empty()
            case Some(a) => Optional.of(a)
        }

    ///
    /// Returns the key-value pair `p` as a new Java `Map.Entry`.
    ///
    pub def toMapEntry(p: (k, v)): Entry[k, v] \ IO =
        let (k, v) = p;
        JMap.entry(k, v)

    ///
    /// Alias for `toArrayList`.
    ///
    pub def toList(ma: m[a]): JList[a] \ (IO + Foldable.Aef[m]) with Foldable[m] =
        checked_cast(toArrayList(ma))

    ///
    /// Returns the elements of the given foldable `ma` as a new Java `ArrayList`.
    ///
    /// Creates a fresh `ArrayList` and copies all elements in `ma` into it.
    ///
    pub def toArrayList(ma: m[a]): ArrayList[a] \ (IO + Foldable.Aef[m]) with Foldable[m] =
        let alist: ArrayList[a] = new ArrayList();
        Foldable.forEach(x -> discard alist.add(x), ma);
        alist

    ///
    /// Returns the elements of the given foldable `ma` as a new Java `LinkedList`.
    ///
    /// Creates a fresh `LinkedList` and copies all elements in `ma` into it.
    ///
    pub def toLinkedList(ma: m[a]): LinkedList[a] \ (IO + Foldable.Aef[m]) with Foldable[m] =
        let llist: LinkedList[a] = new LinkedList();
        Foldable.forEach(x -> discard llist.add(x), ma);
        llist

    ///
    /// Alias for `toTreeSet`.
    ///
    pub def toSet(ma: m[a]): JSet[a] \ (IO + Foldable.Aef[m]) with Order[a], Foldable[m] = checked_cast(toTreeSet(ma))

    ///
    /// Returns the elements of the given foldable `ma` as a new `TreeSet`.
    ///
    pub def toTreeSet(ma: m[a]): TreeSet[a] \ (IO + Foldable.Aef[m]) with Order[a], Foldable[m] =
        let tset: TreeSet[a] = new TreeSet();
        Foldable.forEach(x -> discard tset.add(x), ma);
        tset

    ///
    /// Alias for `toTreeMap`.
    ///
    pub def toMap(m: Map[k, v]): JMap[k, v] \ IO with Order[k] = checked_cast(toTreeMap(m))

    ///
    /// Returns all key-value pairs of the given map `m` as a new `TreeMap`.
    ///
    /// The `TreeMap` uses a `Comparator` constructed from the `Order` on `k`.
    ///
    pub def toTreeMap(m: Map[k, v]): TreeMap[k, v] \ IO with Order[k] =
        let tmap: TreeMap[k, v] = new TreeMap();
        Map.forEach((k, v) -> discard tmap.put(k, v), m);
        tmap


}
