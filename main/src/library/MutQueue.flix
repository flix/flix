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

///
/// Represents a mutable priority queue.
/// Explanation of component types (left to right):
/// Component 1: The region capability the queue is associated with.
/// Component 2: A reference to the backing array.
/// Component 3: A reference to the number of elements in the mutable priority queue.
///
/// The maximum element (if it exists) can always be accessed in constant time.
///
pub enum MutQueue[a: Type, r: Region] {
    case MutQueue(Region[r], Ref[Array[a, r], r], Ref[Int32, r])
}

instance Iterable[MutQueue[a, r]] {
    type Elm = a
    type Aef = r
    pub def iterator(rc: Region[r1], q: MutQueue[a, r]): Iterator[a, r + r1, r1] \ (r + r1) = MutQueue.iterator(rc, q)
}

mod MutQueue {

    ///
    /// Returns a String representation of the mutable priority queue `mq`.
    ///
    pub def toString(mq: MutQueue[a, r]): String \ r with ToString[a] = region reg {
        let MutQueue(_, arr, s) = mq;
        let sb = StringBuilder.empty(reg);
        StringBuilder.appendString!("MutQueue {", sb);
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
    /// Returns an empty MutQueue.
    ///
    pub def empty(rc: Region[r]): MutQueue[a, r] \ r =
        MutQueue(rc, ref Array.empty(rc, 8) @ rc, ref 0 @ rc)

    ///
    /// Returns the number of elements in `mq`.
    ///
    pub def size(mq: MutQueue[a, r]): Int32 \ r =
        let MutQueue(_, _, s) = mq;
        deref s

    ///
    /// Returns whether `mq` is empty.
    ///
    pub def isEmpty(mq: MutQueue[a, r]): Bool \ r =
        let MutQueue(_, _, s) = mq;
        deref s == 0

    ///
    /// Returns whether `mq` is non-empty.
    ///
    pub def nonEmpty(mq: MutQueue[a, r]): Bool \ r = not isEmpty(mq)

    ///
    /// Optionally returns the top element of `mq`.
    ///
    pub def peek(mq: MutQueue[a, r]): Option[a] \ r =
        let MutQueue(_, arr, s) = mq;
        if (deref s == 0) None
        else Some(Array.get(0, deref arr))

    ///
    /// Enqueues an element `x` into a `mq`.
    ///
    pub def enqueue(mq: MutQueue[a, r], x: a): Unit \ r with Order[a] = {
        let MutQueue(_, arr, s) = mq;
        expand(mq);
        Array.put(x, deref s, deref arr);
        heapifyUp(deref s, mq);
        Ref.put(deref s + 1, s)
    }

    ///
    /// Removes and optionally returns the top element of `mq`.
    ///
    pub def dequeue(mq: MutQueue[a, r]): Option[a] \ r with Order[a] =
        let MutQueue(_, arr, s) = mq;
        if (deref s > 0) {
            let top = peek(mq);
            Array.put(Array.get(deref s - 1, deref arr), 0, deref arr);
            heapifyDown(0, mq);
            Ref.put(deref s - 1, s);
            top
        }
        else None

    ///
    /// Enqueues each element in `m` into `mq`.
    ///
    pub def enqueueAll(mq: MutQueue[elt, r], m: m): Unit \ (r + Iterable.Aef[m]) with Iterable[m], Order[elt] where Iterable.Elm[m] ~ elt =
        foreach(x <- m) {
            enqueue(mq, x)
        }

    ///
    /// Returns an iterator over `mq`.
    ///
    /// Modifying `mq` during iteration is undefined and not recommended.
    ///
    pub def iterator(rc: Region[r1], mq: MutQueue[a, r2]): Iterator[a, r1 + r2, r1] \ {r1, r2} =
        let MutQueue(_, arr, s) = mq;
        let it1 = Iterator.range(rc, 0, deref s);
        Iterator.map(x -> Array.get(x, deref arr), it1)

    ///
    /// Returns a List representation of `mq`.
    ///
    /// Note that a MutQueue's element order depends on the order in which the elements were enqueued.
    ///
    pub def toList(mq: MutQueue[a, r]): List[a] \ r with Order[a] =
        let MutQueue(_, arr, s) = mq;
        List.take(deref s, Array.foldRight((x, acc) -> x :: acc, Nil, deref arr))

    ///
    /// Optionally returns a Nel representation of `mq`.
    ///
    pub def toNel(mq: MutQueue[a, r]): Option[Nel[a]] \ r with Order[a] =
        List.toNel(toList(mq))

    ///
    /// Returns an Array representation of `mq`.
    ///
    /// Note that a MutQueue's element order depends on the order in which the elements were enqueued.
    ///
    pub def toArray(rc: Region[r1], mq: MutQueue[a, r2]): Array[a, r1] \ {r1, r2} =
        let MutQueue(_, arr, s) = mq;
        Array.takeLeft(rc, deref s, deref arr)

    ///
    /// Returns an Vector representation of `mq`.
    ///
    /// Note that a MutQueue's element order depends on the order in which the elements were enqueued.
    ///
    pub def toVector(mq: MutQueue[a, r]): Vector[a] \ r = region rc {
        toArray(rc, mq) |> Array.toVector
    }

    ///
    /// Reinforces the max heap invariant from `idx` after an element is added to `mq`.
    ///
    def heapifyUp(idx: Int32, mq: MutQueue[a, r]): Unit \ r with Order[a] =
        let MutQueue(_, arr, _) = mq;
        if (idx != 0) {
            let parentIdx = (idx - 1) / 2;
            let cur  = Array.get(idx, deref arr);
            let parent = Array.get(parentIdx, deref arr);
            if (cur > parent) {
                Array.put(parent, idx, deref arr);
                Array.put(cur, parentIdx, deref arr);
                heapifyUp(parentIdx, mq)
            }
            else ()
        }
        else ()

    ///
    /// Reinforces the max heap invariant from `idx` after an element is removed from `mq`.
    ///
    def heapifyDown(idx: Int32, mq: MutQueue[a, r]): Unit \ r with Order[a] =
        let MutQueue(_, arr, s) = mq;
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
                        heapifyDown(lChildIdx, mq)
                    }
                    else {
                        Array.put(cur, rChildIdx, deref arr);
                        Array.put(rChild, idx, deref arr);
                        heapifyDown(rChildIdx, mq)
                    }
                }
                else ()
            }
        }
        else ()

    ///
    /// Expands the internal array of `mq` if its capacity is full.
    ///
    def expand(mq: MutQueue[a, r]): Unit \ r =
        let MutQueue(reg, arr, s) = mq;
        let oldCapacity = Array.length(deref arr);
        if (oldCapacity == deref s) {
            let newCapacity = 2 + (oldCapacity * 2);
            let newArr  = Array.empty(reg, newCapacity);
            Array.forEachWithIndex((idx, x) -> Array.put(x, idx, newArr), deref arr);
            Ref.put(newArr, arr)
        }
        else ()

}
