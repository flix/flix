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
/// Represents a mutable hash map that preserves insertion order.
///
/// Uses a hash table with separate chaining for collisions and a doubly-linked list
/// to maintain insertion order.
///
pub struct MutHashMap[k: Type, v: Type, r: Region] {
    rc: Region[r],
    mut buckets: Array[Option[MutHashMap.Entry[k, v, r]], r],   // Hash table buckets
    mut head: Option[MutHashMap.Entry[k, v, r]],                // First entry in insertion order
    mut tail: Option[MutHashMap.Entry[k, v, r]],                // Last entry in insertion order
    mut size: Int32                                             // Number of entries
}

instance Iterable[MutHashMap[k, v, r]] {
    type Elm = (k, v)
    type Aef = r
    pub def iterator(rc: Region[r1], m: MutHashMap[k, v, r]): Iterator[(k, v), r + r1, r1] \ (r + r1) =
        MutHashMap.iterator(rc, m)
}

instance ForEach[MutHashMap[k, v, r]] {
    type Elm = (k, v)
    type Aef = r
    pub def forEach(f: ((k, v)) -> Unit \ ef, m: MutHashMap[k, v, r]): Unit \ ef + r =
        MutHashMap.forEach((k, v) -> f((k, v)), m)
}

instance Indexable[MutHashMap[k, v, r]] with Eq[k], Hash[k] {
    type Elm = v
    type Idx = k
    type Aef = r + KeyNotFound

    pub def get(t: MutHashMap[k, v, r], i: k): v \ r + KeyNotFound = {
        match MutHashMap.get(i, t) {
            case Some(v) => v
            case None => KeyNotFound.keyNotFound("key not found")
        }
    }
}

instance IndexableMut[MutHashMap[k, v, r]] with Eq[k], Hash[k] {
    type Aef = r

    pub def put(t: MutHashMap[k, v, r], i: k, v: v): Unit \ r = MutHashMap.put(i, v, t)
}

instance Formattable[MutHashMap[k, v, r]] with Formattable[k], Formattable[v] {
    type Aef = Formattable.Aef[k] + Formattable.Aef[v] + r

    pub def format(x: MutHashMap[k, v, r]): RichString \ (Formattable.Aef[k] + Formattable.Aef[v] + r) =
        use RichString.{fromString, joinWith};
        let kvs = MutHashMap.toList(x) |> List.map(match (k, v) -> Formattable.format(k) + fromString(" => ") + Formattable.format(v));
        fromString("MutHashMap#{") + joinWith(identity, fromString(", "), kvs) + fromString("}")
}

mod MutHashMap {

    ///
    /// Represents an internal entry node for the hash map.
    ///
    /// Each entry is part of both a hash bucket chain and the insertion order chain.
    ///
    pub struct Entry[k: Type, v: Type, r: Region] {
        key: k,                                    // The key for this entry
        hash: Int32,                               // Cached hash code for the key
        mut value: v,                              // The value associated with the key
        mut bucketNext: Option[Entry[k, v, r]],    // Next entry in the same bucket
        mut orderPrev: Option[Entry[k, v, r]],     // Previous entry in insertion order
        mut orderNext: Option[Entry[k, v, r]]      // Next entry in insertion order
    }

    mod Entry {
        pub def getKey(e: Entry[k, v, r]): k = e->key
        pub def getValue(e: Entry[k, v, r]): v \ r = e->value
        pub def setValue(v: v, e: Entry[k, v, r]): Unit \ r = e->value = v
        pub def getHash(e: Entry[k, v, r]): Int32 = e->hash
        pub def getBucketNext(e: Entry[k, v, r]): Option[Entry[k, v, r]] \ r = e->bucketNext
        pub def setBucketNext(next: Option[Entry[k, v, r]], e: Entry[k, v, r]): Unit \ r = e->bucketNext = next
        pub def getOrderPrev(e: Entry[k, v, r]): Option[Entry[k, v, r]] \ r = e->orderPrev
        pub def setOrderPrev(prev: Option[Entry[k, v, r]], e: Entry[k, v, r]): Unit \ r = e->orderPrev = prev
        pub def getOrderNext(e: Entry[k, v, r]): Option[Entry[k, v, r]] \ r = e->orderNext
        pub def setOrderNext(next: Option[Entry[k, v, r]], e: Entry[k, v, r]): Unit \ r = e->orderNext = next
    }

    ///
    /// Constant denoting the minimum allowed capacity of the bucket array.
    ///
    def minCapacity(): Int32 = 8

    ///
    /// Constant denoting the smallest valid load factor.
    ///
    def minLoadFactor(): Float32 = 0.25f32

    ///
    /// Constant denoting the largest valid load factor.
    ///
    def maxLoadFactor(): Float32 = 0.75f32

    ///
    /// Returns `true` if `m` is empty.
    ///
    pub def isEmpty(m: MutHashMap[k, v, r]): Bool \ r =
        m->size == 0

    ///
    /// Returns `true` if `m` is non-empty.
    ///
    pub def nonEmpty(m: MutHashMap[k, v, r]): Bool \ r =
        not isEmpty(m)

    ///
    /// Returns the number of entries in `m`.
    ///
    pub def size(m: MutHashMap[k, v, r]): Int32 \ r =
        m->size

    ///
    /// Returns an empty MutHashMap.
    ///
    pub def empty(rc: Region[r]): MutHashMap[k, v, r] \ r =
        emptyWithCapacity(rc, minCapacity())

    ///
    /// Returns an empty mutable hash map with the given capacity.
    ///
    /// The capacity is rounded up to the minimum capacity.
    ///
    pub def emptyWithCapacity(rc: Region[r], capacity: Int32): MutHashMap[k, v, r] \ r =
        let cap = Int32.max(capacity, minCapacity());
        let buckets = Array.repeat(rc, cap, None);
        new MutHashMap @ rc { rc = rc, buckets = buckets, head = None, tail = None, size = 0 }

    ///
    /// Returns a mutable hash map with a single key-value pair `k => v`.
    ///
    pub def singleton(rc: Region[r], k: k, v: v): MutHashMap[k, v, r] \ r with Eq[k], Hash[k] =
        let m = empty(rc);
        put(k, v, m);
        m

    ///
    /// Returns the set of all keys in `m`.
    ///
    pub def keysOf(m: MutHashMap[k, v, r]): Set[k] \ r with Order[k] =
        foldLeftWithKey((acc, k, _) -> Set.insert(k, acc), Set.empty(), m)

    ///
    /// Returns a list of all values in `m` in insertion order.
    ///
    pub def valuesOf(m: MutHashMap[k, v, r]): List[v] \ r =
        foldRight((v, acc) -> v :: acc, Nil, m)

    ///
    /// Returns `Some(v)` if key `k` exists in `m`, otherwise `None`.
    ///
    pub def get(k: k, m: MutHashMap[k, v, r]): Option[v] \ r with Eq[k], Hash[k] =
        let hash = Hash.hash(k);
        let idx = indexOfBucket(hash, Array.length(m->buckets));
        match findInBucket(k, Array.get(idx, m->buckets)) {
            case None => None
            case Some(entry) => Some(Entry.getValue(entry))
        }

    ///
    /// Returns the value associated with key `k` in `m`.
    ///
    /// If `k` does not exist, inserts `k` with value `default` and returns `default`.
    ///
    pub def getOrElsePut(k: k, default: v, m: MutHashMap[k, v, r]): v \ r with Eq[k], Hash[k] =
        match get(k, m) {
            case Some(v) => v
            case None =>
                put(k, default, m);
                default
        }

    ///
    /// Returns the value associated with key `k`, or `default` if not found.
    ///
    pub def getWithDefault(k: k, default: v, m: MutHashMap[k, v, r]): v \ r with Eq[k], Hash[k] =
        Option.getWithDefault(default, get(k, m))

    ///
    /// Converts hash code to a valid bucket index.
    ///
    def indexOfBucket(hash: Int32, numBuckets: Int32): Int32 =
        // Note: Modulo ensures that the result cannot be negative.
        // So there is no risk that we index into the array with a negative index.
        Int32.modulo(hash, numBuckets)

    ///
    /// Searches for an entry with the given key in the bucket chain.
    ///
    def findInBucket(k: k, bucket: Option[Entry[k, v, r]]): Option[Entry[k, v, r]] \ r with Eq[k] =
        match bucket {
            case None => None
            case Some(entry) =>
                if (k == Entry.getKey(entry))
                    Some(entry)
                else
                    findInBucket(k, Entry.getBucketNext(entry))
        }

    ///
    /// Returns `true` if key `k` exists in `m`.
    ///
    pub def memberOf(k: k, m: MutHashMap[k, v, r]): Bool \ r with Eq[k], Hash[k] =
        let hash = Hash.hash(k);
        let idx = indexOfBucket(hash, Array.length(m->buckets));
        match findInBucket(k, Array.get(idx, m->buckets)) {
            case None => false
            case Some(_) => true
        }

    ///
    /// Returns the first key-value pair that was inserted into the mutable map `m`.
    ///
    /// Returns `None` if the map is empty.
    ///
    pub def first(m: MutHashMap[k, v, r]): Option[(k, v)] \ r =
        match m->head {
            case None => None
            case Some(entry) => Some((Entry.getKey(entry), Entry.getValue(entry)))
        }

    ///
    /// Returns the last key-value pair that was inserted into the mutable map `m`.
    ///
    /// Returns `None` if the map is empty.
    ///
    pub def last(m: MutHashMap[k, v, r]): Option[(k, v)] \ r =
        match m->tail {
            case None => None
            case Some(entry) => Some((Entry.getKey(entry), Entry.getValue(entry)))
        }

    ///
    /// Returns the first key that was inserted into the mutable map `m`.
    ///
    /// Returns `None` if the map is empty.
    ///
    pub def firstKey(m: MutHashMap[k, v, r]): Option[k] \ r =
        first(m) |> Option.map(fst)

    ///
    /// Returns the last key that was inserted into the mutable map `m`.
    ///
    /// Returns `None` if the map is empty.
    ///
    pub def lastKey(m: MutHashMap[k, v, r]): Option[k] \ r =
        last(m) |> Option.map(fst)

    ///
    /// Returns the first value that was inserted into the mutable map `m`.
    ///
    /// Returns `None` if the map is empty.
    ///
    pub def firstValue(m: MutHashMap[k, v, r]): Option[v] \ r =
        first(m) |> Option.map(snd)

    ///
    /// Returns the last value that was inserted into the mutable map `m`.
    ///
    /// Returns `None` if the map is empty.
    ///
    pub def lastValue(m: MutHashMap[k, v, r]): Option[v] \ r =
        last(m) |> Option.map(snd)

    ///
    /// Inserts key `k` with value `v` into `m`.
    ///
    /// If the key already exists, updates its value (does not change insertion order).
    ///
    pub def put(k: k, v: v, m: MutHashMap[k, v, r]): Unit \ r with Eq[k], Hash[k] =
        putWithKey((_, newV, _) -> newV, k, v, m)

    ///
    /// Adds all key-value pairs in the collection `kvs` to the mutable map `m`.
    ///
    pub def putAll(kvs: f[(k, v)], m: MutHashMap[k, v, r]): Unit \ (r + Foldable.Aef[f]) with Eq[k], Hash[k], Foldable[f] =
        Foldable.forEach(match (k, v) -> put(k, v, m), kvs)

    ///
    /// Inserts key `k` with value `v` into `m`, using combiner function `f` if key exists.
    ///
    /// If key exists with value `oldV`, updates to `f(v, oldV)`.
    ///
    /// Otherwise, inserts `k => v`.
    ///
    pub def putWith(f: (v, v) -> v \ ef, k: k, v: v, m: MutHashMap[k, v, r]): Unit \ {ef, r} with Eq[k], Hash[k] =
        putWithKey((_, newV, oldV) -> f(newV, oldV), k, v, m)

    ///
    /// Inserts key `k` with value `v` into `m`, using combiner function `f` with key if key exists.
    ///
    /// If key exists with value `oldV`, updates to `f(k, v, oldV)`.
    ///
    /// Otherwise, inserts `k => v`.
    ///
    pub def putWithKey(f: (k, v, v) -> v \ ef, k: k, v: v, m: MutHashMap[k, v, r]): Unit \ {ef, r} with Eq[k], Hash[k] =
        let hash = Hash.hash(k);
        let numBuckets = Array.length(m->buckets);
        let idx = indexOfBucket(hash, numBuckets);
        let bucket = Array.get(idx, m->buckets);

        match findInBucket(k, bucket) {
            case Some(entry) =>
                // Key exists, combine values with key
                Entry.setValue(f(k, v, Entry.getValue(entry)), entry)
            case None =>
                // Key doesn't exist, insert normally
                let entry = new Entry @ m->rc {
                    key = k,
                    value = v,
                    hash = hash,
                    bucketNext = bucket,
                    orderPrev = m->tail,
                    orderNext = None
                };
                Array.put(Some(entry), idx, m->buckets);
                match m->tail {
                    case None =>
                        m->head = Some(entry);
                        m->tail = Some(entry)
                    case Some(tailEntry) =>
                        Entry.setOrderNext(Some(entry), tailEntry);
                        m->tail = Some(entry)
                };
                m->size = m->size + 1;
                expand(m)
        }


    ///
    /// Updates the value at key `k` using function `f` if the key exists.
    ///
    /// Does nothing if the key does not exist.
    ///
    pub def adjust(f: v -> v \ ef, k: k, m: MutHashMap[k, v, r]): Unit \ {ef, r} with Eq[k], Hash[k] =
        adjustWithKey((_, v) -> f(v), k, m)

    ///
    /// Updates the value at key `k` using function `f` with key if the key exists.
    ///
    /// Does nothing if the key does not exist.
    ///
    pub def adjustWithKey(f: (k, v) -> v \ ef, k: k, m: MutHashMap[k, v, r]): Unit \ {ef, r} with Eq[k], Hash[k] =
        let hash = Hash.hash(k);
        let idx = indexOfBucket(hash, Array.length(m->buckets));
        match findInBucket(k, Array.get(idx, m->buckets)) {
            case Some(entry) => Entry.setValue(f(k, Entry.getValue(entry)), entry)
            case None => ()
        }

    ///
    /// Updates or removes the value at key `k` based on function `f`.
    ///
    /// If key exists and `f(oldValue) = Some(newValue)`, updates to newValue.
    ///
    /// If key exists and `f(oldValue) = None`, removes the entry.
    ///
    /// Does nothing if the key does not exist.
    ///
    pub def update(f: v -> Option[v] \ ef, k: k, m: MutHashMap[k, v, r]): Unit \ {ef, r} with Eq[k], Hash[k] =
        updateWithKey((_, v) -> f(v), k, m)

    ///
    /// Updates or removes the value at key `k` based on function `f` with key.
    ///
    /// If key exists and `f(k, oldValue) = Some(newValue)`, updates to newValue.
    ///
    /// If key exists and `f(k, oldValue) = None`, removes the entry.
    ///
    /// Does nothing if the key does not exist.
    ///
    pub def updateWithKey(f: (k, v) -> Option[v] \ ef, k: k, m: MutHashMap[k, v, r]): Unit \ {ef, r} with Eq[k], Hash[k] =
        let hash = Hash.hash(k);
        let idx = indexOfBucket(hash, Array.length(m->buckets));
        match findInBucket(k, Array.get(idx, m->buckets)) {
            case Some(entry) =>
                match f(k, Entry.getValue(entry)) {
                    case Some(newValue) => Entry.setValue(newValue, entry)
                    case None => remove(k, m)
                }
            case None => ()
        }

    ///
    /// Removes key `k` from `m`.
    ///
    pub def remove(k: k, m: MutHashMap[k, v, r]): Unit \ r with Eq[k], Hash[k] =
        // Removal is a two-phase process:
        // 1. Remove the entry from its bucket chain (hash table structure)
        // 2. Remove the entry from the insertion order chain (doubly-linked list)

        // Phase 1: Find and remove from bucket chain
        let hash = Hash.hash(k);
        let numBuckets = Array.length(m->buckets);
        let idx = indexOfBucket(hash, numBuckets);
        let bucket = Array.get(idx, m->buckets);

        // Helper to traverse bucket chain and remove matching entry
        // Returns Some(entry) if found and removed, None otherwise
        def removeFromBucket(curr, prev) = match curr {
            case None => None
            case Some(entry) =>
                if (k == Entry.getKey(entry)) {
                    // Found the entry - remove it from bucket chain
                    match prev {
                        case None =>
                            // Entry is first in bucket - update bucket head
                            Array.put(Entry.getBucketNext(entry), idx, m->buckets)
                        case Some(prevEntry) =>
                            // Entry is in middle/end - bypass it in the chain
                            Entry.setBucketNext(Entry.getBucketNext(entry), prevEntry)
                    };
                    Some(entry)
                } else {
                    // Keep searching
                    removeFromBucket(Entry.getBucketNext(entry), Some(entry))
                }
        };

        // Phase 2: If entry was found, also remove from insertion order chain
        match removeFromBucket(bucket, None) {
            case None => ()  // Key not found, nothing to do
            case Some(entry) =>
                // Update the previous entry's next pointer (or map head if no previous)
                match Entry.getOrderPrev(entry) {
                    case None =>
                        // Removing the head of insertion order
                        m->head = Entry.getOrderNext(entry)
                    case Some(prevEntry) =>
                        // Bypass this entry in the order chain
                        Entry.setOrderNext(Entry.getOrderNext(entry), prevEntry)
                };

                // Update the next entry's previous pointer (or map tail if no next)
                match Entry.getOrderNext(entry) {
                    case None =>
                        // Removing the tail of insertion order
                        m->tail = Entry.getOrderPrev(entry)
                    case Some(nextEntry) =>
                        // Bypass this entry in the order chain
                        Entry.setOrderPrev(Entry.getOrderPrev(entry), nextEntry)
                };

                // Update size and potentially shrink the bucket array
                m->size = m->size - 1;
                shrink(m)
        }

    ///
    /// Removes all entries from `m`.
    ///
    pub def clear(m: MutHashMap[k, v, r]): Unit \ r =
        let numBuckets = Array.length(m->buckets);
        Array.updateSequence(0, Array.repeat(m->rc, numBuckets, None), m->buckets);
        m->head = None;
        m->tail = None;
        m->size = 0

    ///
    /// Returns the load factor of `m`.
    ///
    def loadFactor(m: MutHashMap[k, v, r]): Float32 \ r =
        Int32.toFloat32(m->size) / Int32.toFloat32(Array.length(m->buckets))

    ///
    /// Doubles the bucket array size if load factor >= maxLoadFactor.
    ///
    def expand(m: MutHashMap[k, v, r]): Unit \ r =
        if (loadFactor(m) >= maxLoadFactor())
            resize(m, Array.length(m->buckets) * 2)
        else
            ()

    ///
    /// Halves the bucket array size if load factor <= minLoadFactor.
    ///
    def shrink(m: MutHashMap[k, v, r]): Unit \ r =
        let currentCapacity = Array.length(m->buckets);
        let newCapacity = currentCapacity / 2;
        if (loadFactor(m) <= minLoadFactor() and newCapacity >= minCapacity())
            resize(m, newCapacity)
        else
            ()

    ///
    /// Resizes the bucket array to `newCapacity` and rehashes all entries.
    ///
    def resize(m: MutHashMap[k, v, r], newCapacity: Int32): Unit \ r =
        // Create new bucket array
        let newBuckets = Array.repeat(m->rc, newCapacity, None);

        // Rehash all entries by walking the order chain
        def rehashEntry(curr) = match curr {
            case None => ()
            case Some(entry) =>
                // Compute new bucket index
                let idx = indexOfBucket(Entry.getHash(entry), newCapacity);
                let bucket = Array.get(idx, newBuckets);

                // Insert into new bucket chain
                Entry.setBucketNext(bucket, entry);
                Array.put(Some(entry), idx, newBuckets);

                // Continue with next entry in order chain
                rehashEntry(Entry.getOrderNext(entry))
        };

        rehashEntry(m->head);
        m->buckets = newBuckets

    ///
    /// Applies `f` to each key-value pair in `m` in insertion order.
    ///
    pub def forEach(f: (k, v) -> Unit \ ef, m: MutHashMap[k, v, r]): Unit \ {ef, r} =
        def loop(curr) = match curr {
            case None => ()
            case Some(entry) =>
                f(Entry.getKey(entry), Entry.getValue(entry));
                loop(Entry.getOrderNext(entry))
        };
        loop(m->head)

    ///
    /// Applies `f` to each key-value pair in `m` along with its index in insertion order.
    ///
    pub def forEachWithIndex(f: (Int32, k, v) -> Unit \ ef, m: MutHashMap[k, v, r]): Unit \ {ef, r} = region rc {
        let ix = Ref.fresh(rc, 0);
        forEach((k, v) -> {
            let i = Ref.get(ix);
            f(i, k, v);
            Ref.put(i + 1, ix)
        }, m)
    }

    ///
    /// Returns an iterator over key-value pairs in `m` in insertion order.
    ///
    pub def iterator(rc: Region[r2], m: MutHashMap[k, v, r]): Iterator[(k, v), r2 + r, r2] \ {r, r2} =
        let curr = Ref.fresh(rc, m->head);
        def next() = match Ref.get(curr) {
            case None => None
            case Some(entry) =>
                Ref.put(Entry.getOrderNext(entry), curr);
                Some((Entry.getKey(entry), Entry.getValue(entry)))
        };
        Iterator.iterate(rc, next)

    ///
    /// Returns an iterator over keys in `m` in insertion order.
    ///
    pub def iteratorKeys(rc: Region[r2], m: MutHashMap[k, v, r]): Iterator[k, r2 + r, r2] \ {r, r2} =
        iterator(rc, m) |> Iterator.map(fst)

    ///
    /// Returns an iterator over values in `m` in insertion order.
    ///
    pub def iteratorValues(rc: Region[r2], m: MutHashMap[k, v, r]): Iterator[v, r2 + r, r2] \ {r, r2} =
        iterator(rc, m) |> Iterator.map(snd)

    ///
    /// Returns a new mutable hash map with values transformed by function `f`.
    ///
    /// Keys and insertion order are preserved.
    ///
    pub def map(rc: Region[r2], f: v1 -> v2 \ ef, m: MutHashMap[k, v1, r1]): MutHashMap[k, v2, r2] \ {ef, r1, r2} with Eq[k], Hash[k] =
        let result = emptyWithCapacity(rc, size(m));
        forEach((k, v) -> put(k, f(v), result), m);
        result

    ///
    /// Returns a new mutable hash map with values transformed by function `f` with access to keys.
    ///
    /// Keys and insertion order are preserved.
    ///
    pub def mapWithKey(rc: Region[r2], f: (k, v1) -> v2 \ ef, m: MutHashMap[k, v1, r1]): MutHashMap[k, v2, r2] \ {ef, r1, r2} with Eq[k], Hash[k] =
        let result = emptyWithCapacity(rc, size(m));
        forEach((k, v) -> put(k, f(k, v), result), m);
        result

    ///
    /// Transforms all values in `m` in-place using function `f`.
    ///
    pub def transform(f: v -> v \ ef, m: MutHashMap[k, v, r]): Unit \ {ef, r} =
        transformWithKey((_, v) -> f(v), m)

    ///
    /// Transforms all values in `m` in-place using function `f` with access to keys.
    ///
    pub def transformWithKey(f: (k, v) -> v \ ef, m: MutHashMap[k, v, r]): Unit \ {ef, r} =
        def loop(curr) = match curr {
            case None => ()
            case Some(entry) =>
                Entry.setValue(f(Entry.getKey(entry), Entry.getValue(entry)), entry);
                loop(Entry.getOrderNext(entry))
        };
        loop(m->head)

    ///
    /// Removes all entries from `m` where `f(value)` returns false.
    ///
    pub def refine(f: v -> Bool \ ef, m: MutHashMap[k, v, r]): Unit \ {ef, r} with Eq[k], Hash[k] =
        refineWithKey((_, v) -> f(v), m)

    ///
    /// Removes all entries from `m` where `f(key, value)` returns false.
    ///
    pub def refineWithKey(f: (k, v) -> Bool \ ef, m: MutHashMap[k, v, r]): Unit \ {ef, r} with Eq[k], Hash[k] =
        // Two-pass: collect keys to remove, then remove them
        def collectKeys(curr, acc) = match curr {
            case None => acc
            case Some(entry) =>
                let k = Entry.getKey(entry);
                let v = Entry.getValue(entry);
                let newAcc = if (f(k, v)) acc else k :: acc;
                collectKeys(Entry.getOrderNext(entry), newAcc)
        };
        let keysToRemove = collectKeys(m->head, Nil);
        List.forEach(k -> remove(k, m), keysToRemove)

    ///
    /// Applies `f` to a start value `s` and all values in `m` going from left to right (insertion order).
    ///
    pub def foldLeft(f: (b, v) -> b \ ef, s: b, m: MutHashMap[k, v, r]): b \ {ef, r} =
        def loop(curr, acc) = match curr {
            case None => acc
            case Some(entry) =>
                loop(Entry.getOrderNext(entry), f(acc, Entry.getValue(entry)))
        };
        loop(m->head, s)

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in `m` going from left to right (insertion order).
    ///
    pub def foldLeftWithKey(f: (b, k, v) -> b \ ef, s: b, m: MutHashMap[k, v, r]): b \ {ef, r} =
        def loop(curr, acc) = match curr {
            case None => acc
            case Some(entry) =>
                loop(Entry.getOrderNext(entry), f(acc, Entry.getKey(entry), Entry.getValue(entry)))
        };
        loop(m->head, s)

    ///
    /// Applies `f` to a start value `s` and all values in `m` going from right to left (reverse insertion order).
    ///
    pub def foldRight(f: (v, b) -> b \ ef, s: b, m: MutHashMap[k, v, r]): b \ {ef, r} =
        def loop(curr, acc) = match curr {
            case None => acc
            case Some(entry) =>
                loop(Entry.getOrderPrev(entry), f(Entry.getValue(entry), acc))
        };
        loop(m->tail, s)

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in `m` going from right to left (reverse insertion order).
    ///
    pub def foldRightWithKey(f: (k, v, b) -> b \ ef, s: b, m: MutHashMap[k, v, r]): b \ {ef, r} =
        def loop(curr, acc) = match curr {
            case None => acc
            case Some(entry) =>
                loop(Entry.getOrderPrev(entry), f(Entry.getKey(entry), Entry.getValue(entry), acc))
        };
        loop(m->tail, s)

    ///
    /// Returns the result of mapping each value and combining the results using a monoid.
    ///
    pub def foldMap(f: v -> b \ ef, m: MutHashMap[k, v, r]): b \ {ef, r} with Monoid[b] =
        foldLeft((acc, x) -> Monoid.combine(acc, f(x)), Monoid.empty(), m)

    ///
    /// Returns `true` if at least one key-value pair in `m` satisfies the predicate `f`.
    ///
    pub def exists(f: (k, v) -> Bool \ ef, m: MutHashMap[k, v, r]): Bool \ {ef, r} =
        def loop(curr) = match curr {
            case None => false
            case Some(entry) =>
                if (f(Entry.getKey(entry), Entry.getValue(entry)))
                    true
                else
                    loop(Entry.getOrderNext(entry))
        };
        loop(m->head)

    ///
    /// Returns `true` if all key-value pairs in `m` satisfy the predicate `f`.
    ///
    pub def forAll(f: (k, v) -> Bool \ ef, m: MutHashMap[k, v, r]): Bool \ {ef, r} =
        def loop(curr) = match curr {
            case None => true
            case Some(entry) =>
                if (f(Entry.getKey(entry), Entry.getValue(entry)))
                    loop(Entry.getOrderNext(entry))
                else
                    false
        };
        loop(m->head)

    ///
    /// Returns the number of key-value pairs in `m` that satisfy the predicate `f`.
    ///
    pub def count(f: (k, v) -> Bool \ ef, m: MutHashMap[k, v, r]): Int32 \ {ef, r} =
        def loop(curr, acc) = match curr {
            case None => acc
            case Some(entry) =>
                let newAcc = if (f(Entry.getKey(entry), Entry.getValue(entry))) acc + 1 else acc;
                loop(Entry.getOrderNext(entry), newAcc)
        };
        loop(m->head, 0)

    ///
    /// Returns the first key-value pair in `m` (in insertion order) that satisfies the predicate `f`.
    ///
    /// Alias for `findLeft`.
    ///
    pub def find(f: (k, v) -> Bool \ ef, m: MutHashMap[k, v, r]): Option[(k, v)] \ {ef, r} =
        findLeft(f, m)

    ///
    /// Returns the first key-value pair in `m` (from left, in insertion order) that satisfies the predicate `f`.
    ///
    pub def findLeft(f: (k, v) -> Bool \ ef, m: MutHashMap[k, v, r]): Option[(k, v)] \ {ef, r} =
        def loop(curr) = match curr {
            case None => None
            case Some(entry) =>
                let k = Entry.getKey(entry);
                let v = Entry.getValue(entry);
                if (f(k, v))
                    Some((k, v))
                else
                    loop(Entry.getOrderNext(entry))
        };
        loop(m->head)

    ///
    /// Returns the first key-value pair in `m` (from right, in reverse insertion order) that satisfies the predicate `f`.
    ///
    pub def findRight(f: (k, v) -> Bool \ ef, m: MutHashMap[k, v, r]): Option[(k, v)] \ {ef, r} =
        def loop(curr) = match curr {
            case None => None
            case Some(entry) =>
                let k = Entry.getKey(entry);
                let v = Entry.getValue(entry);
                if (f(k, v))
                    Some((k, v))
                else
                    loop(Entry.getOrderPrev(entry))
        };
        loop(m->tail)

    ///
    /// Returns a shallow copy of `m` in region `rc2`.
    ///
    /// All key-value pairs are copied in insertion order.
    ///
    pub def copy(rc: Region[r2], m: MutHashMap[k, v, r1]): MutHashMap[k, v, r2] \ {r1, r2} with Eq[k], Hash[k] =
        let result = emptyWithCapacity(rc, size(m));
        forEach((k, v) -> put(k, v, result), m);
        result

    ///
    /// Returns an array of all key-value pairs in `m` in insertion order.
    ///
    pub def toArray(rc: Region[r2], m: MutHashMap[k, v, r]): Array[(k, v), r2] \ {r, r2} =
        let len = size(m);
        let arr = Array.empty(rc, len);
        forEachWithIndex((i, k, v) -> Array.put((k, v), i, arr), m);
        arr

    ///
    /// Returns a list of all key-value pairs in `m` in insertion order.
    ///
    pub def toList(m: MutHashMap[k, v, r]): List[(k, v)] \ r =
        foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil, m)

    ///
    /// Returns an immutable map containing all key-value pairs from `m`.
    ///
    pub def toMap(m: MutHashMap[k, v, r]): Map[k, v] \ r with Order[k] =
        foldLeftWithKey((acc, k, v) -> Map.insert(k, v, acc), Map.empty(), m)

    ///
    /// Returns a set containing all key-value pairs from `m`.
    ///
    pub def toSet(m: MutHashMap[k, v, r]): Set[(k, v)] \ r with Order[k], Order[v] =
        foldLeftWithKey((acc, k, v) -> Set.insert((k, v), acc), Set.empty(), m)

    ///
    /// Returns a vector of all key-value pairs in `m` in insertion order.
    ///
    pub def toVector(m: MutHashMap[k, v, r]): Vector[(k, v)] \ r = region rc {
        let arr = toArray(rc, m);
        Array.toVector(arr)
    }

    ///
    /// Returns a new mutable hash map containing all key-value pairs from list `l`.
    /// If the list contains duplicate keys, the last occurrence wins.
    ///
    pub def fromList(rc: Region[r], l: List[(k, v)]): MutHashMap[k, v, r] \ r with Eq[k], Hash[k] =
        let result = empty(rc);
        List.forEach(match (k, v) -> put(k, v, result), l);
        result

    ///
    /// Returns a new mutable hash map containing all key-value pairs from immutable map `m`.
    ///
    pub def fromMap(rc: Region[r], m: Map[k, v]): MutHashMap[k, v, r] \ r with Order[k], Hash[k] =
        let result = empty(rc);
        Map.forEach((k, v) -> put(k, v, result), m);
        result

    ///
    /// Returns a string formed by joining all keys in `m` with `sep`.
    ///
    pub def joinKeys(sep: String, m: MutHashMap[k, v, r]): String \ r with ToString[k] =
        let strs = foldRightWithKey((k, _v, acc) -> "${k}" :: acc, Nil, m);
        String.intercalate(sep, strs)

    ///
    /// Returns a string formed by joining all values in `m` with `sep`.
    ///
    pub def joinValues(sep: String, m: MutHashMap[k, v, r]): String \ r with ToString[v] =
        let strs = foldRight((v, acc) -> "${v}" :: acc, Nil, m);
        String.intercalate(sep, strs)

    ///
    /// Returns a string formed by applying `f` to each key-value pair and joining with `sep`.
    ///
    pub def joinWith(f: (k, v) -> String \ ef, sep: String, m: MutHashMap[k, v, r]): String \ {ef, r} =
        let strs = foldRightWithKey((k, v, acc) -> f(k, v) :: acc, Nil, m);
        String.intercalate(sep, strs)

    ///
    /// Returns a string representation of `m`.
    ///
    pub def toString(m: MutHashMap[k, v, r]): String \ r with ToString[k], ToString[v] =
        let kvs = foldRightWithKey((k, v, acc) -> "${k} -> ${v}" :: acc, Nil, m);
        let inner = String.intercalate(", ", kvs);
        "MutHashMap#{${inner}}"

}
