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

pub enum MutHeap[a: Type, r: Region] {
    case MutHeap(Region[r], Ref[Array[a, r], r], Ref[Int32, r])
}

mod MutHeap {

    ///
    /// Returns an empty MutHeap
    ///
    pub def new(rc: Region[r]): MutHeap[a, r] \ r =
        MutHeap(rc, ref Array.new(rc, 8) @ rc, ref 0 @ rc)

    ///
    /// Returns an empty MutHeap with a reserved amount of space
    ///
    pub def newWithSpace(rc: Region[r], size: Int32): MutHeap[a, r] \ r =
        MutHeap(rc, ref Array.new(rc, size) @ rc, ref 0 @ rc)

    ///
    /// Reserves a specific amount of space for
    ///
    pub def reserve(h: MutHeap[a, r], size: Int32): Unit \ r =
        let MutHeap(rc, arr, s) = h;
        if (size > deref s) {
            let newArr = Array.new(rc, size);
            Array.forEachWithIndex((x, elem) -> {
                if (x < deref s) {
                    Array.put(elem, x, newArr)
                }
                else ()
            } , deref arr);
            arr := newArr
        }
        else ()


    ///
    /// Returns the number of elements in a MutHeap
    ///
    pub def size(h: MutHeap[a, r]): Int32 \ r =
        let MutHeap(_, _, s) = h;
        deref s

    ///
    /// Returns whether a MutHeap is empty (has 0 elements)
    ///
    pub def isEmpty(h: MutHeap[a, r]): Bool \ r =
        let MutHeap(_, _, s) = h;
        deref s == 0

    ///
    /// Returns the top element of a MutHeap
    ///
    pub def peek(h: MutHeap[a, r]): Option[a] \ r =
        let MutHeap(_, arr, s) = h;
        if (deref s == 0) None
        else Some(Array.get(0, deref arr))

    ///
    /// Removes and returns the top element of a MutHeap
    ///
    pub def pop(h: MutHeap[a, r]): Option[a] \ r with Order[a] =
        let MutHeap(_, _, s) = h;
        if (deref s > 0) {
            match peek(h) {
                case Some(elem) => {
                    delete(h, elem);
                    Some(elem)
                }
                case _          => None
            }
        }
        else None

    ///
    /// Inserts an element into a MutHeap
    ///
    pub def insert(h: MutHeap[a, r], elem: a): Unit \ r with Order[a] = {
        let MutHeap(_, arr, s) = h;
        if (deref s == 0) {
            Array.put(elem, 0, deref arr);
            s := 1
        }
        else {
            expand!(h);
            Array.put(elem, deref s, deref arr);
            heapifyUp(deref s, h);
            s := deref s + 1
        }
    }

    ///
    /// Deletes an element from a MutHeap
    ///
    pub def delete(h: MutHeap[a, r], elem: a): Unit \ r with Order[a] = {
        let MutHeap(_, arr, s) = h;
        match Array.findIndexOf(x -> x == elem, deref arr) {
            case Some(idx) =>
                let lastElem = Array.get(deref s - 1, deref arr);
                Array.put(lastElem, idx, deref arr);
                heapifyDown(idx, h);
                s := deref s - 1
            case None => ()
        }
    }

    ///
    /// Returns a string representation of the MutHeap
    ///
    pub def toString(h: MutHeap[a, r]): String \ r with ToString[a] = region reg {
        let MutHeap(_, arr, s) = h;
        let sb = StringBuilder.new(reg);
        StringBuilder.appendString!("MutHeap {", sb);
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
    /// Returns an iterator over MutHeap `h`
    ///
    /// Modifying `h` during iteration is undefined
    ///
    pub def iterator(rc: Region[r1], h: MutHeap[a, r2]): Iterator[a, r1 + r2, r1] \ {r1, r2} =
        let MutHeap(_, arr, s) = h;
        let numElems = deref s;
        let idx = ref 0 @ rc;
        let next = () -> {
            let i = deref idx;
            if (i < numElems) {
                let elem = Array.get(i, deref arr);
                Some(elem)
            }
            else {
                None
            }
        };
        Iterator.iterate(rc, next)

    ///
    /// Reinforces the max heap invariant after an element is added
    ///
    def heapifyUp(idx: Int32, h: MutHeap[a, r]): Unit \ r with Order[a] =
        let MutHeap(_, arr, _) = h;
        if (idx != 0) {
            let parentIdx = (idx - 1) / 2;
            let cur  = Array.get(idx, deref arr);
            let parent = Array.get(parentIdx, deref arr);
            if (cur > parent) {
                Array.put(parent, idx, deref arr);
                Array.put(cur, parentIdx, deref arr);
                heapifyUp(parentIdx, h)
            }
            else ()
        }
        else ()

    ///
    /// Reinforces the max heap invariant after an element is removed
    ///
    def heapifyDown(idx: Int32, h: MutHeap[a, r]): Unit \ r with Order[a] =
        let MutHeap(_, arr, s) = h;
        let size = deref s;
        let lChildIdx = idx * 2 + 1;
        let rChildIdx = idx * 2 + 2;
        let cur = Array.get(idx, deref arr);
        if (size >= rChildIdx) {
            if (size == rChildIdx) {
                let child = Array.get(lChildIdx, deref arr);
                if (Order.less(cur, child)) {
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
                        heapifyDown(lChildIdx, h)
                    }
                    else {
                        Array.put(cur, rChildIdx, deref arr);
                        Array.put(rChild, idx, deref arr);
                        heapifyDown(rChildIdx, h)
                    }
                }
                else ()
            }
        }
        else ()

    ///
    /// Expands the internal array of MutHeap `h` if the array capacity is being approached
    ///
    /// Expansion transpires only when the number of elements in `h` is 1 less than the array capacity
    ///
    def expand!(h: MutHeap[a, r]): Unit \ r =
        let MutHeap(reg, arr, s) = h;
        let oldCapacity = Array.length(deref arr);
        if (oldCapacity == 1 + deref s) {
            let newCapacity = 2 + (oldCapacity * 2);
            let newArr  = Array.new(reg, newCapacity);
            Array.forEachWithIndex((idx, x) -> Array.put(x, idx, newArr), deref arr);
            arr := newArr
        }
        else ()

    ///
    /// Inserts each element of a list into the MutHeap `h`
    ///
    pub def readFromList(h: MutHeap[a, r], l: List[a]): Unit \ r with Order[a] =
        List.forEach(x -> MutHeap.insert(h, x), l)

}
