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

mod TestMutPriorityQueue {

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testToString01(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc) : MutPriorityQueue[Char, rc];
        MutPriorityQueue.toString(pq) == "MutPriorityQueue {}"
    }

    @Test
    def testToString02(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc);
        MutPriorityQueue.enqueue(pq, 3);
        MutPriorityQueue.toString(pq) == "MutPriorityQueue {3}"
    }

    @Test
    def testToString03(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc);
        MutPriorityQueue.enqueueAll(pq, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        MutPriorityQueue.dequeue(pq);
        MutPriorityQueue.dequeue(pq);
        MutPriorityQueue.toString(pq) == "MutPriorityQueue {6, 4, 5, 1, 3, 2}"
    }

    /////////////////////////////////////////////////////////////////////////////
    // new                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testNew01(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc);
        MutPriorityQueue.size(pq) == 0
    }

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testSize01(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc);
        MutPriorityQueue.size(pq) == 0
    }

    @Test
    def testSize02(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc);
        MutPriorityQueue.enqueueAll(pq, "a" :: "b" :: "c" :: "d" :: "e" :: Nil);
        MutPriorityQueue.size(pq) == 5
    }

    @Test
    def testSize03(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc);
        MutPriorityQueue.enqueueAll(pq, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        MutPriorityQueue.dequeue(pq);
        MutPriorityQueue.size(pq) == 7
    }

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testIsEmpty01(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc);
        MutPriorityQueue.isEmpty(pq)
    }

    @Test
    def testIsEmpty02(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc);
        MutPriorityQueue.enqueue(pq, "a");
        not MutPriorityQueue.isEmpty(pq)
    }

    @Test
    def testIsEmpty03(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc);
        MutPriorityQueue.enqueueAll(pq, 1 :: 2 :: 3 :: Nil);
        MutPriorityQueue.dequeue(pq);
        MutPriorityQueue.dequeue(pq);
        MutPriorityQueue.dequeue(pq);
        MutPriorityQueue.isEmpty(pq)
    }

    @Test def testIsEmpty04(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc);
        MutPriorityQueue.enqueueAll(pq, 1 :: 2 :: Nil);
        MutPriorityQueue.dequeue(pq);
        MutPriorityQueue.dequeue(pq);
        MutPriorityQueue.enqueue(pq, 1);
        not MutPriorityQueue.isEmpty(pq)
    }

    /////////////////////////////////////////////////////////////////////////////
    // peek                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testPeek01(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc);
        MutPriorityQueue.enqueue(pq, 3);
        MutPriorityQueue.dequeue(pq);
        MutPriorityQueue.peek(pq) == None
    }

    @Test
    def testPeek02(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc);
        MutPriorityQueue.enqueue(pq, 1);
        MutPriorityQueue.peek(pq) == Some(1)
    }

    @Test
    def testPeek03(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc);
        MutPriorityQueue.enqueueAll(pq, 1 :: 9 :: 2 :: 8 :: 3 :: 7 :: Nil);
        MutPriorityQueue.dequeue(pq);
        MutPriorityQueue.peek(pq) == Some(8)
    }

    @Test
    def testPeek04(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc) : MutPriorityQueue[Int32, rc];
        MutPriorityQueue.peek(pq) == None
    }

    @Test
    def testPeek05(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc);
        MutPriorityQueue.enqueue(pq, 5);
        MutPriorityQueue.enqueue(pq, 5);
        MutPriorityQueue.dequeue(pq);
        MutPriorityQueue.peek(pq) == Some(5)
    }

    /////////////////////////////////////////////////////////////////////////////
    // enqueue                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testEnqueue01(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc);
        MutPriorityQueue.enqueue(pq, "a");
        MutPriorityQueue.size(pq) == 1
    }

    @Test
    def testEnqueue02(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc);
        MutPriorityQueue.enqueue(pq, 3);
        MutPriorityQueue.enqueue(pq, 2);
        MutPriorityQueue.toString(pq) == "MutPriorityQueue {3, 2}"
    }

    @Test
    def testEnqueue03(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc);
        MutPriorityQueue.enqueue(pq, 2);
        MutPriorityQueue.enqueue(pq, 3);
        MutPriorityQueue.toString(pq) == "MutPriorityQueue {3, 2}"
    }

    @Test
    def testEnqueue04(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc);
        MutPriorityQueue.enqueue(pq, "a");
        MutPriorityQueue.enqueue(pq, "b");
        MutPriorityQueue.enqueue(pq, "c");
        MutPriorityQueue.enqueue(pq, "a");
        MutPriorityQueue.enqueue(pq, "a");
        MutPriorityQueue.peek(pq) == Some("c")
    }

    @Test
    def testEnqueue05(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc);
        MutPriorityQueue.enqueue(pq, 3);
        MutPriorityQueue.enqueue(pq, 4);
        MutPriorityQueue.enqueue(pq, 5);
        MutPriorityQueue.enqueue(pq, 6);
        MutPriorityQueue.enqueue(pq, 10);
        MutPriorityQueue.enqueue(pq, 5);
        MutPriorityQueue.enqueue(pq, 7);
        MutPriorityQueue.enqueue(pq, 14);
        MutPriorityQueue.enqueue(pq, 20);
        MutPriorityQueue.peek(pq) == Some(20)
    }

    /////////////////////////////////////////////////////////////////////////////
    // dequeue                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testDequeue01(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc) : MutPriorityQueue[Int32, rc];
        MutPriorityQueue.dequeue(pq);
        MutPriorityQueue.size(pq) == 0
    }

    @Test
    def testDequeue02(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc) : MutPriorityQueue[Int32, rc];
        MutPriorityQueue.dequeue(pq) == None
    }

    @Test
    def testDequeue03(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc);
        MutPriorityQueue.enqueueAll(pq, 1 :: 2 :: 3 :: 4 :: 5 :: Nil);
        MutPriorityQueue.dequeue(pq);
        MutPriorityQueue.dequeue(pq);
        MutPriorityQueue.size(pq) == 3
    }

    @Test
    def testDequeue04(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc);
        MutPriorityQueue.enqueueAll(pq, 1 :: 2 :: 3 :: 4 :: 5 :: Nil);
        MutPriorityQueue.dequeue(pq);
        MutPriorityQueue.dequeue(pq) == Some(4)
    }

    @Test
    def testDequeue05(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc);
        MutPriorityQueue.enqueueAll(pq, 1 :: 2 :: 3 :: Nil);
        MutPriorityQueue.dequeue(pq);
        MutPriorityQueue.dequeue(pq);
        MutPriorityQueue.dequeue(pq);
        MutPriorityQueue.size(pq) == 0
    }

    /////////////////////////////////////////////////////////////////////////////
    // enqueueAll                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testEnqueueAll01(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc) : MutPriorityQueue[Int32, rc];
        MutPriorityQueue.enqueueAll(pq, Nil);
        MutPriorityQueue.toString(pq) == "MutPriorityQueue {}"
    }

    @Test
    def testEnqueueAll02(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc);
        MutPriorityQueue.enqueueAll(pq, Nil);
        MutPriorityQueue.enqueueAll(pq, 1 :: Nil);
        MutPriorityQueue.toString(pq) == "MutPriorityQueue {1}"
    }

    @Test
    def testEnqueueAll03(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc);
        MutPriorityQueue.enqueueAll(pq, Nil);
        MutPriorityQueue.enqueueAll(pq, 1 :: 2 :: 3 :: Nil);
        MutPriorityQueue.enqueueAll(pq, 4 :: 5 :: 6 :: Nil);
        MutPriorityQueue.toString(pq) == "MutPriorityQueue {6, 4, 5, 1, 3, 2}"
    }

    @Test
    def testEnqueueAll04(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc);
        MutPriorityQueue.enqueueAll(pq, Nil);
        MutPriorityQueue.enqueueAll(pq, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        MutPriorityQueue.enqueueAll(pq, Nil);
        MutPriorityQueue.toString(pq) == "MutPriorityQueue {8, 7, 6, 4, 3, 2, 5, 1}"
    }

    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testIterator01(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc);
        let it = MutPriorityQueue.iterator(rc, pq);
        let sum = ref 0 @ rc;
        Iterator.forEach(x -> sum := deref sum + x, it);
        deref sum == 0
    }

    @Test
    def testIterator02(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc);
        MutPriorityQueue.enqueueAll(pq, 1 :: Nil);
        let it = MutPriorityQueue.iterator(rc, pq);
        Iterator.toList(it) == 1 :: Nil
    }

    @Test
    def testIterator03(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc);
        MutPriorityQueue.enqueueAll(pq, "a" :: "c" :: "g" :: "t" :: Nil);
        let it = MutPriorityQueue.iterator(rc, pq);
        let str = ref "" @ rc;
        Iterator.forEach(x -> str := String.concat(x, deref str), it);
        deref str == "acgt"
    }

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testToList01(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc) : MutPriorityQueue[Int32, rc];
        MutPriorityQueue.toList(pq) == Nil
    }

    @Test
    def testToList02(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc);
        MutPriorityQueue.enqueue(pq, 1);
        MutPriorityQueue.toList(pq) == 1 :: Nil
    }

    @Test
    def testToList03(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc);
        MutPriorityQueue.enqueueAll(pq, 1 :: 2 :: Nil);
        MutPriorityQueue.toList(pq) == 2 :: 1 :: Nil
    }

    @Test
    def testToList04(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc);
        MutPriorityQueue.enqueueAll(pq, 1 :: 2 :: Nil);
        MutPriorityQueue.toList(pq) == 2 :: 1 :: Nil
    }

    @Test
    def testToList05(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc);
        MutPriorityQueue.enqueueAll(pq, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        MutPriorityQueue.toList(pq) == 8 :: 7 :: 6 :: 4 :: 3 :: 2 :: 5 :: 1 :: Nil
    }

    @Test
    def testToList06(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc);
        MutPriorityQueue.enqueueAll(pq, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        MutPriorityQueue.dequeue(pq);
        MutPriorityQueue.dequeue(pq);
        MutPriorityQueue.toList(pq) == 6 :: 4 :: 5 :: 1 :: 3 :: 2 :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // toNel                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testToNel01(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc) : MutPriorityQueue[Int32, rc];
        MutPriorityQueue.toNel(pq) == None
    }

    @Test
    def testToNel02(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc);
        MutPriorityQueue.enqueue(pq, 1);
        MutPriorityQueue.toNel(pq) == Some(Nel.Nel(1, Nil))
    }

    @Test
    def testToNel03(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc);
        MutPriorityQueue.enqueueAll(pq, 1 :: 2 :: 3 :: Nil);
        MutPriorityQueue.toNel(pq) == Some(Nel.Nel(3, 1 :: 2 :: Nil))
    }

    @Test
    def testToNel04(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc);
        MutPriorityQueue.enqueueAll(pq, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        MutPriorityQueue.toNel(pq) == Some(Nel.Nel(8, 7 :: 6 :: 4 :: 3 :: 2 :: 5 :: 1 :: Nil))
    }

    @Test
    def testToNel05(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc);
        MutPriorityQueue.enqueueAll(pq, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        MutPriorityQueue.dequeue(pq);
        MutPriorityQueue.dequeue(pq);
        MutPriorityQueue.toNel(pq) == Some(Nel.Nel(6, 4 :: 5 :: 1 :: 3 :: 2 :: Nil))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testArray01(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc) : MutPriorityQueue[Int32, rc];
        Array.toString(MutPriorityQueue.toArray(rc, pq)) == "Array#{}"
    }

    @Test
    def testArray02(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc) : MutPriorityQueue[Int32, rc];
        MutPriorityQueue.enqueue(pq, 1);
        Array.toString(MutPriorityQueue.toArray(rc, pq)) == "Array#{1}"
    }

    @Test
    def testArray03(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc) : MutPriorityQueue[Int32, rc];
        MutPriorityQueue.enqueueAll(pq, 1 :: 2 :: Nil);
        Array.toString(MutPriorityQueue.toArray(rc, pq)) == "Array#{2, 1}"
    }

    @Test
    def testArray04(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc) : MutPriorityQueue[Int32, rc];
        MutPriorityQueue.enqueueAll(pq, 2 :: 1 :: Nil);
        Array.toString(MutPriorityQueue.toArray(rc, pq)) == "Array#{2, 1}"
    }

    @Test
    def testArray05(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc) : MutPriorityQueue[Int32, rc];
        MutPriorityQueue.enqueueAll(pq, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        Array.toString(MutPriorityQueue.toArray(rc, pq)) == "Array#{8, 7, 6, 4, 3, 2, 5, 1}"
    }

    @Test
    def testArray06(): Bool = region rc {
        let pq = MutPriorityQueue.new(rc) : MutPriorityQueue[Int32, rc];
        MutPriorityQueue.enqueueAll(pq, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        MutPriorityQueue.dequeue(pq);
        MutPriorityQueue.dequeue(pq);
        Array.toString(MutPriorityQueue.toArray(rc, pq)) == "Array#{6, 4, 5, 1, 3, 2}"
    }

}
