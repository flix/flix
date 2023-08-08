/*
 * Copyright 2023 Xavier deSouza
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

pub enum MutPriorityQueue[a: Type, r: Region] {
    case MutPriorityQueue(Region[r], Ref[Array[a, r], r], Ref[Int32, r])
}

mod MutPriorityQueue {

    ///
    /// Represents a mutable priority queue.
    /// Explanation of component types (left to right):
    /// Component 2: A reference to the backing array.
    /// Component 3: A reference to the number of elements in the mutable priority queue.
    ///
    /// The maximum element (if it exists) can always be accessed in constant time.
    ///
    pub def toString(pq: MutPriorityQueue[a, r]): String \ r with ToString[a] = region reg {
        let MutPriorityQueue(_, arr, s) = pq;
        let sb = StringBuilder.new(reg);
        StringBuilder.appendString!("MutPriorityQueue {", sb);
        Array.forEachWithIndex((i, x) -> {
            if (i < deref s) {
                if (i == 0) StringBuilder.appendString!("${x}", sb)
                else        StringBuilder.appendString!(", ${x}", sb)
            }
            else ()
        }, deref arr);
        StringBuilder.appendString!("}", sb);
        StringBuilder.toString(sb)
    }

    ///
    /// Returns an empty MutPriorityQueue.
    ///
    pub def new(rc: Region[r]): MutPriorityQueue[a, r] \ r =
        MutPriorityQueue(rc, ref Array.new(rc, 8) @ rc, ref 0 @ rc)

    ///
    /// Returns the number of elements in `pq`.
    ///
    pub def size(pq: MutPriorityQueue[a, r]): Int32 \ r =
        let MutPriorityQueue(_, _, s) = pq;
        deref s

    ///
    /// Returns whether `pq` is empty.
    ///
    pub def isEmpty(pq: MutPriorityQueue[a, r]): Bool \ r =
        let MutPriorityQueue(_, _, s) = pq;
        deref s == 0

    ///
    /// Optionally returns the top element of `pq`.
    ///
    pub def peek(pq: MutPriorityQueue[a, r]): Option[a] \ r =
        let MutPriorityQueue(_, arr, s) = pq;
        if (deref s == 0) None
        else Some(Array.get(0, deref arr))

    ///
    /// Enqueues an element `x` into a `pq`.
    ///
    pub def enqueue(pq: MutPriorityQueue[a, r], x: a): Unit \ r with Order[a] = {
        let MutPriorityQueue(_, arr, s) = pq;
        expand(pq);
        Array.put(x, deref s, deref arr);
        heapifyUp(deref s, pq);
        s := deref s + 1
    }

    ///
    /// Removes and optionally returns the top element of `pq`.
    ///
    pub def dequeue(pq: MutPriorityQueue[a, r]): Option[a] \ r with Order[a] =
        let MutPriorityQueue(_, arr, s) = pq;
        if (deref s > 0) {
            let top = peek(pq);
            Array.put(Array.get(deref s - 1, deref arr), 0, deref arr);
            heapifyDown(0, pq);
            s := deref s - 1;
            top
        }
        else None

    ///
    /// Enqueues each element in `l` into `pq`.
    ///
    pub def enqueueAll(pq: MutPriorityQueue[a, r], m: m[a]): Unit \ r with Order[a], Iterable[m] =
        foreach(x <- m) {
            enqueue(pq, x)
        }

    ///
    /// Returns an iterator over `pq`.
    ///
    /// Modifying `pq` during iteration is undefined and not recommended.
    ///
    pub def iterator(rc: Region[r1], pq: MutPriorityQueue[a, r2]): Iterator[a, r1 + r2, r1] \ {r1, r2} =
        let MutPriorityQueue(_, arr, s) = pq;
        let it1 = Iterator.range(rc, 0, deref s);
        Iterator.map(x -> Array.get(x, deref arr), it1)

    ///
    /// Returns a MutPriorityQueue which contains the elements of both `pq1` and `pq2`.
    ///
    pub def merge(pq1: MutPriorityQueue[a, r1], pq2: MutPriorityQueue[a, r2]): Unit \ {r1, r2} with Order[a] =
        let MutPriorityQueue(_, arr2, s) = pq2;
        Array.forEachWithIndex((i, x) -> {
            if (i < deref s) {
                MutPriorityQueue.enqueue(pq1, x)
            }
            else ()
        }, deref arr2)

    ///
    /// Returns a List representation of `pq`.
    ///
    /// Note that a MutPriorityQueue's element order depends on the order in which the elements were enqueued.
    ///
    pub def toList(pq: MutPriorityQueue[a, r]): List[a] \ r with Order[a] =
        let MutPriorityQueue(_, arr, s) = pq;
        List.take(deref s, Array.foldRight((x, acc) -> x :: acc, Nil, deref arr))

    ///
    /// Optionally returns a Nel representation of `pq`.
    ///
    pub def toNel(pq: MutPriorityQueue[a, r]): Option[Nel[a]] \ r with Order[a] =
        List.toNel(toList(pq))

    ///
    /// Returns an Array representation of `pq`.
    ///
    /// Note that a MutPriorityQueue's element order depends on the order in which the elements were enqueued.
    ///
    pub def toArray(rc: Region[r1], pq: MutPriorityQueue[a, r2]): Array[a, r1] \ {r1, r2} =
        let MutPriorityQueue(_, arr, s) = pq;
        Array.takeLeft(rc, deref s, deref arr)

    ///
    /// Reinforces the max heap invariant from `idx` after an element is added to `pq`.
    ///
    def heapifyUp(idx: Int32, pq: MutPriorityQueue[a, r]): Unit \ r with Order[a] =
        let MutPriorityQueue(_, arr, _) = pq;
        if (idx != 0) {
            let parentIdx = (idx - 1) / 2;
            let cur  = Array.get(idx, deref arr);
            let parent = Array.get(parentIdx, deref arr);
            if (cur > parent) {
                Array.put(parent, idx, deref arr);
                Array.put(cur, parentIdx, deref arr);
                heapifyUp(parentIdx, pq)
            }
            else ()
        }
        else ()

    ///
    /// Reinforces the max heap invariant from `idx` after an element is removed from `pq`.
    ///
    def heapifyDown(idx: Int32, pq: MutPriorityQueue[a, r]): Unit \ r with Order[a] =
        let MutPriorityQueue(_, arr, s) = pq;
        let size = deref s;
        let lChildIdx = idx * 2 + 1;
        let rChildIdx = idx * 2 + 2;
        let cur = Array.get(idx, deref arr);
        if (size >= rChildIdx) {
            if (size == rChildIdx) {
                let child = Array.get(lChildIdx, deref arr);
                if (cur < child) {
                    Array.put(child, idx, deref arr);
                    Array.put(cur, lChildIdx, deref arr)
                }
                else ()
            }
            else {
                let lChild = Array.get(lChildIdx, deref arr);
                let rChild = Array.get(rChildIdx, deref arr);
                if ((lChild > cur) or (rChild > cur)) {
                    if (lChild > rChild) {
                        Array.put(cur, lChildIdx, deref arr);
                        Array.put(lChild, idx, deref arr);
                        heapifyDown(lChildIdx, pq)
                    }
                    else {
                        Array.put(cur, rChildIdx, deref arr);
                        Array.put(rChild, idx, deref arr);
                        heapifyDown(rChildIdx, pq)
                    }
                }
                else ()
            }
        }
        else ()

    ///
    /// Expands the internal array of `pq` if its capacity is full.
    ///
    def expand(pq: MutPriorityQueue[a, r]): Unit \ r =
        let MutPriorityQueue(reg, arr, s) = pq;
        let oldCapacity = Array.length(deref arr);
        if (oldCapacity == deref s) {
            let newCapacity = 2 + (oldCapacity * 2);
            let newArr  = Array.new(reg, newCapacity);
            Array.forEachWithIndex((idx, x) -> Array.put(x, idx, newArr), deref arr);
            arr := newArr
        }
        else ()

}
