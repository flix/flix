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
    /// Removes the top element of a MutHeap
    ///
    pub def pop(h: MutHeap[a, r]): Unit \ r with Order[a] =
        let MutHeap(_, _, s) = h;
        if (deref s > 0) {
            match peek(h) {
                case Some(elem) => delete(h, elem)
                case _          => ()
            }
        }
        else ()

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
    pub def delete(h: MutHeap[a, r], _elem: a): Unit \ r with Order[a] = {
        let MutHeap(_, arr, s) = h;
        match Array.findIndexOf(_ -> true, deref arr) {
            case Some(idx) =>
                Array.put(Array.get(deref s, deref arr), idx, deref arr);
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

    def heapifyUp(idx: Int32, h: MutHeap[a, r]): Unit \ r with Order[a] =
        let MutHeap(_, arr, _) = h;
        if (idx != 0) {
            let parentIdx = (idx - 1) / 2;
            let cur  = Array.get(idx, deref arr);
            let parent = Array.get(parentIdx, deref arr);
            if (Order.greater(cur, parent)) {
                Array.put(parent, idx, deref arr);
                Array.put(cur, parentIdx, deref arr);
                heapifyUp(parentIdx, h)
            }
            else ()
        }
        else ()

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
                if (Order.greater(lChild, cur) or Order.greater(rChild, cur)) {
                    if (Order.greater(lChild, rChild)) {
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

    def readFromList(h: MutHeap[a, r], l: List[a]): Unit \ r with Order[a] =
        match l {
            case x::xs => {
                MutHeap.insert(h, x);
                readFromList(h, xs)
            }
            case Nil => ()
        }


    @Test
    def heap01(): Bool = region reg {
        let h = MutHeap.new(reg);
        MutHeap.size(h) == 0
    }

    @Test
    def heap02(): Bool = region reg {
        let h = MutHeap.new(reg);
        MutHeap.insert(h, 1);
        MutHeap.toString(h) == "MutHeap {1}"
    }

    @Test
    def heap03(): Bool = region reg {
        let h = MutHeap.new(reg);
        MutHeap.insert(h, " ");
        MutHeap.pop(h);
        MutHeap.toString(h) == "MutHeap {}"
    }

    @Test
    def heap04(): Bool = region reg {
        let h = MutHeap.new(reg);
        MutHeap.readFromList(h, 3 :: 4 :: 5 :: 6 :: Nil); //3 4 5 6
        MutHeap.toString(h) == "MutHeap {6, 5, 4, 3}"
    }

    @Test
    def heap05(): Bool = region reg {
        let h = MutHeap.new(reg);
        MutHeap.readFromList(h, 3 :: 4 :: 5 :: 6 :: 10 :: 5 :: 7 :: 14 :: 20 :: Nil);
        MutHeap.toString(h) == "MutHeap {20, 14, 7, 10, 5, 4, 5, 3, 6}"
    }

    @Test
    def heap06(): Bool = region reg {
        let h = MutHeap.new(reg);
        MutHeap.readFromList(h, 3 :: 4 :: 5 :: 6 :: 10 :: 5 :: 7 :: 14 :: 20 :: Nil);
        MutHeap.toString(h) == "MutHeap {20, 14, 7, 10, 5, 4, 5, 3, 6}"
    }

    @Test
    def heap07(): Bool \ IO = region reg {
        let h = MutHeap.new(reg);
        MutHeap.readFromList(h, 3 :: 4 :: 5 :: 6 :: 10 :: 5 :: 7 :: 14 :: 20 :: Nil);
        MutHeap.pop(h);
        println(MutHeap.toString(h));
        MutHeap.toString(h) == "MutHeap {14, 10, 7, 6, 5, 4, 5, 3}"
    }

    @Test
    def heap08(): Bool \ IO = region reg {
        let h = MutHeap.new(reg);
        MutHeap.readFromList(h, 3 :: 4 :: 5 :: 6 :: 10 :: 5 :: 7 :: 14 :: 20 :: Nil);
        MutHeap.pop(h);
        MutHeap.pop(h);
        MutHeap.pop(h);
        println(MutHeap.toString(h));
        MutHeap.toString(h) == "MutHeap {7, 6, 5, 3, 5, 4}"
    }
}
