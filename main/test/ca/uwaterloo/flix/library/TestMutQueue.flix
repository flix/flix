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

mod TestMutQueue {

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testToString01(): Bool = region rc {
        let pq = MutQueue.new(rc) : MutQueue[Char, rc];
        MutQueue.toString(pq) == "MutQueue {}"
    }

    @Test
    def testToString02(): Bool = region rc {
        let pq = MutQueue.new(rc);
        MutQueue.enqueue(pq, 3);
        MutQueue.toString(pq) == "MutQueue {3}"
    }

    @Test
    def testToString03(): Bool = region rc {
        let pq = MutQueue.new(rc);
        MutQueue.enqueueAll(pq, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        MutQueue.dequeue(pq);
        MutQueue.dequeue(pq);
        MutQueue.toString(pq) == "MutQueue {6, 4, 5, 1, 3, 2}"
    }

    /////////////////////////////////////////////////////////////////////////////
    // new                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testNew01(): Bool = region rc {
        let pq = MutQueue.new(rc);
        MutQueue.size(pq) == 0
    }

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testSize01(): Bool = region rc {
        let pq = MutQueue.new(rc);
        MutQueue.size(pq) == 0
    }

    @Test
    def testSize02(): Bool = region rc {
        let pq = MutQueue.new(rc);
        MutQueue.enqueueAll(pq, "a" :: "b" :: "c" :: "d" :: "e" :: Nil);
        MutQueue.size(pq) == 5
    }

    @Test
    def testSize03(): Bool = region rc {
        let pq = MutQueue.new(rc);
        MutQueue.enqueueAll(pq, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        MutQueue.dequeue(pq);
        MutQueue.size(pq) == 7
    }

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testIsEmpty01(): Bool = region rc {
        let pq = MutQueue.new(rc);
        MutQueue.isEmpty(pq)
    }

    @Test
    def testIsEmpty02(): Bool = region rc {
        let pq = MutQueue.new(rc);
        MutQueue.enqueue(pq, "a");
        not MutQueue.isEmpty(pq)
    }

    @Test
    def testIsEmpty03(): Bool = region rc {
        let pq = MutQueue.new(rc);
        MutQueue.enqueueAll(pq, 1 :: 2 :: 3 :: Nil);
        MutQueue.dequeue(pq);
        MutQueue.dequeue(pq);
        MutQueue.dequeue(pq);
        MutQueue.isEmpty(pq)
    }

    @Test def testIsEmpty04(): Bool = region rc {
        let pq = MutQueue.new(rc);
        MutQueue.enqueueAll(pq, 1 :: 2 :: Nil);
        MutQueue.dequeue(pq);
        MutQueue.dequeue(pq);
        MutQueue.enqueue(pq, 1);
        not MutQueue.isEmpty(pq)
    }

    /////////////////////////////////////////////////////////////////////////////
    // peek                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testPeek01(): Bool = region rc {
        let pq = MutQueue.new(rc);
        MutQueue.enqueue(pq, 3);
        MutQueue.dequeue(pq);
        MutQueue.peek(pq) == None
    }

    @Test
    def testPeek02(): Bool = region rc {
        let pq = MutQueue.new(rc);
        MutQueue.enqueue(pq, 1);
        MutQueue.peek(pq) == Some(1)
    }

    @Test
    def testPeek03(): Bool = region rc {
        let pq = MutQueue.new(rc);
        MutQueue.enqueueAll(pq, 1 :: 9 :: 2 :: 8 :: 3 :: 7 :: Nil);
        MutQueue.dequeue(pq);
        MutQueue.peek(pq) == Some(8)
    }

    @Test
    def testPeek04(): Bool = region rc {
        let pq = MutQueue.new(rc) : MutQueue[Int32, rc];
        MutQueue.peek(pq) == None
    }

    @Test
    def testPeek05(): Bool = region rc {
        let pq = MutQueue.new(rc);
        MutQueue.enqueue(pq, 5);
        MutQueue.enqueue(pq, 5);
        MutQueue.dequeue(pq);
        MutQueue.peek(pq) == Some(5)
    }

    /////////////////////////////////////////////////////////////////////////////
    // enqueue                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testEnqueue01(): Bool = region rc {
        let pq = MutQueue.new(rc);
        MutQueue.enqueue(pq, "a");
        MutQueue.size(pq) == 1
    }

    @Test
    def testEnqueue02(): Bool = region rc {
        let pq = MutQueue.new(rc);
        MutQueue.enqueue(pq, 3);
        MutQueue.enqueue(pq, 2);
        MutQueue.toString(pq) == "MutQueue {3, 2}"
    }

    @Test
    def testEnqueue03(): Bool = region rc {
        let pq = MutQueue.new(rc);
        MutQueue.enqueue(pq, 2);
        MutQueue.enqueue(pq, 3);
        MutQueue.toString(pq) == "MutQueue {3, 2}"
    }

    @Test
    def testEnqueue04(): Bool = region rc {
        let pq = MutQueue.new(rc);
        MutQueue.enqueue(pq, "a");
        MutQueue.enqueue(pq, "b");
        MutQueue.enqueue(pq, "c");
        MutQueue.enqueue(pq, "a");
        MutQueue.enqueue(pq, "a");
        MutQueue.peek(pq) == Some("c")
    }

    @Test
    def testEnqueue05(): Bool = region rc {
        let pq = MutQueue.new(rc);
        MutQueue.enqueue(pq, 3);
        MutQueue.enqueue(pq, 4);
        MutQueue.enqueue(pq, 5);
        MutQueue.enqueue(pq, 6);
        MutQueue.enqueue(pq, 10);
        MutQueue.enqueue(pq, 5);
        MutQueue.enqueue(pq, 7);
        MutQueue.enqueue(pq, 14);
        MutQueue.enqueue(pq, 20);
        MutQueue.peek(pq) == Some(20)
    }

    /////////////////////////////////////////////////////////////////////////////
    // dequeue                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testDequeue01(): Bool = region rc {
        let pq = MutQueue.new(rc) : MutQueue[Int32, rc];
        MutQueue.dequeue(pq);
        MutQueue.size(pq) == 0
    }

    @Test
    def testDequeue02(): Bool = region rc {
        let pq = MutQueue.new(rc) : MutQueue[Int32, rc];
        MutQueue.dequeue(pq) == None
    }

    @Test
    def testDequeue03(): Bool = region rc {
        let pq = MutQueue.new(rc);
        MutQueue.enqueueAll(pq, 1 :: 2 :: 3 :: 4 :: 5 :: Nil);
        MutQueue.dequeue(pq);
        MutQueue.dequeue(pq);
        MutQueue.size(pq) == 3
    }

    @Test
    def testDequeue04(): Bool = region rc {
        let pq = MutQueue.new(rc);
        MutQueue.enqueueAll(pq, 1 :: 2 :: 3 :: 4 :: 5 :: Nil);
        MutQueue.dequeue(pq);
        MutQueue.dequeue(pq) == Some(4)
    }

    @Test
    def testDequeue05(): Bool = region rc {
        let pq = MutQueue.new(rc);
        MutQueue.enqueueAll(pq, 1 :: 2 :: 3 :: Nil);
        MutQueue.dequeue(pq);
        MutQueue.dequeue(pq);
        MutQueue.dequeue(pq);
        MutQueue.size(pq) == 0
    }

    /////////////////////////////////////////////////////////////////////////////
    // enqueueAll                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testEnqueueAll01(): Bool = region rc {
        let pq = MutQueue.new(rc) : MutQueue[Int32, rc];
        MutQueue.enqueueAll(pq, Nil);
        MutQueue.toString(pq) == "MutQueue {}"
    }

    @Test
    def testEnqueueAll02(): Bool = region rc {
        let pq = MutQueue.new(rc);
        MutQueue.enqueueAll(pq, Nil);
        MutQueue.enqueueAll(pq, 1 :: Nil);
        MutQueue.toString(pq) == "MutQueue {1}"
    }

    @Test
    def testEnqueueAll03(): Bool = region rc {
        let pq = MutQueue.new(rc);
        MutQueue.enqueueAll(pq, Nil);
        MutQueue.enqueueAll(pq, 1 :: 2 :: 3 :: Nil);
        MutQueue.enqueueAll(pq, 4 :: 5 :: 6 :: Nil);
        MutQueue.toString(pq) == "MutQueue {6, 4, 5, 1, 3, 2}"
    }

    @Test
    def testEnqueueAll04(): Bool = region rc {
        let pq = MutQueue.new(rc);
        MutQueue.enqueueAll(pq, Nil);
        MutQueue.enqueueAll(pq, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        MutQueue.enqueueAll(pq, Nil);
        MutQueue.toString(pq) == "MutQueue {8, 7, 6, 4, 3, 2, 5, 1}"
    }

    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testIterator01(): Bool = region rc {
        let pq = MutQueue.new(rc);
        let it = MutQueue.iterator(rc, pq);
        let sum = ref 0 @ rc;
        Iterator.forEach(x -> sum := deref sum + x, it);
        deref sum == 0
    }

    @Test
    def testIterator02(): Bool = region rc {
        let pq = MutQueue.new(rc);
        MutQueue.enqueueAll(pq, 1 :: Nil);
        let it = MutQueue.iterator(rc, pq);
        Iterator.toList(it) == 1 :: Nil
    }

    @Test
    def testIterator03(): Bool = region rc {
        let pq = MutQueue.new(rc);
        MutQueue.enqueueAll(pq, "a" :: "c" :: "g" :: "t" :: Nil);
        let it = MutQueue.iterator(rc, pq);
        let str = ref "" @ rc;
        Iterator.forEach(x -> str := String.concat(x, deref str), it);
        deref str == "acgt"
    }

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testToList01(): Bool = region rc {
        let pq = MutQueue.new(rc) : MutQueue[Int32, rc];
        MutQueue.toList(pq) == Nil
    }

    @Test
    def testToList02(): Bool = region rc {
        let pq = MutQueue.new(rc);
        MutQueue.enqueue(pq, 1);
        MutQueue.toList(pq) == 1 :: Nil
    }

    @Test
    def testToList03(): Bool = region rc {
        let pq = MutQueue.new(rc);
        MutQueue.enqueueAll(pq, 1 :: 2 :: Nil);
        MutQueue.toList(pq) == 2 :: 1 :: Nil
    }

    @Test
    def testToList04(): Bool = region rc {
        let pq = MutQueue.new(rc);
        MutQueue.enqueueAll(pq, 1 :: 2 :: Nil);
        MutQueue.toList(pq) == 2 :: 1 :: Nil
    }

    @Test
    def testToList05(): Bool = region rc {
        let pq = MutQueue.new(rc);
        MutQueue.enqueueAll(pq, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        MutQueue.toList(pq) == 8 :: 7 :: 6 :: 4 :: 3 :: 2 :: 5 :: 1 :: Nil
    }

    @Test
    def testToList06(): Bool = region rc {
        let pq = MutQueue.new(rc);
        MutQueue.enqueueAll(pq, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        MutQueue.dequeue(pq);
        MutQueue.dequeue(pq);
        MutQueue.toList(pq) == 6 :: 4 :: 5 :: 1 :: 3 :: 2 :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // toNel                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testToNel01(): Bool = region rc {
        let pq = MutQueue.new(rc) : MutQueue[Int32, rc];
        MutQueue.toNel(pq) == None
    }

    @Test
    def testToNel02(): Bool = region rc {
        let pq = MutQueue.new(rc);
        MutQueue.enqueue(pq, 1);
        MutQueue.toNel(pq) == Some(Nel.Nel(1, Nil))
    }

    @Test
    def testToNel03(): Bool = region rc {
        let pq = MutQueue.new(rc);
        MutQueue.enqueueAll(pq, 1 :: 2 :: 3 :: Nil);
        MutQueue.toNel(pq) == Some(Nel.Nel(3, 1 :: 2 :: Nil))
    }

    @Test
    def testToNel04(): Bool = region rc {
        let pq = MutQueue.new(rc);
        MutQueue.enqueueAll(pq, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        MutQueue.toNel(pq) == Some(Nel.Nel(8, 7 :: 6 :: 4 :: 3 :: 2 :: 5 :: 1 :: Nil))
    }

    @Test
    def testToNel05(): Bool = region rc {
        let pq = MutQueue.new(rc);
        MutQueue.enqueueAll(pq, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        MutQueue.dequeue(pq);
        MutQueue.dequeue(pq);
        MutQueue.toNel(pq) == Some(Nel.Nel(6, 4 :: 5 :: 1 :: 3 :: 2 :: Nil))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testArray01(): Bool = region rc {
        let pq = MutQueue.new(rc) : MutQueue[Int32, rc];
        Array.toString(MutQueue.toArray(rc, pq)) == "Array#{}"
    }

    @Test
    def testArray02(): Bool = region rc {
        let pq = MutQueue.new(rc) : MutQueue[Int32, rc];
        MutQueue.enqueue(pq, 1);
        Array.toString(MutQueue.toArray(rc, pq)) == "Array#{1}"
    }

    @Test
    def testArray03(): Bool = region rc {
        let pq = MutQueue.new(rc) : MutQueue[Int32, rc];
        MutQueue.enqueueAll(pq, 1 :: 2 :: Nil);
        Array.toString(MutQueue.toArray(rc, pq)) == "Array#{2, 1}"
    }

    @Test
    def testArray04(): Bool = region rc {
        let pq = MutQueue.new(rc) : MutQueue[Int32, rc];
        MutQueue.enqueueAll(pq, 2 :: 1 :: Nil);
        Array.toString(MutQueue.toArray(rc, pq)) == "Array#{2, 1}"
    }

    @Test
    def testArray05(): Bool = region rc {
        let pq = MutQueue.new(rc) : MutQueue[Int32, rc];
        MutQueue.enqueueAll(pq, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        Array.toString(MutQueue.toArray(rc, pq)) == "Array#{8, 7, 6, 4, 3, 2, 5, 1}"
    }

    @Test
    def testArray06(): Bool = region rc {
        let pq = MutQueue.new(rc) : MutQueue[Int32, rc];
        MutQueue.enqueueAll(pq, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        MutQueue.dequeue(pq);
        MutQueue.dequeue(pq);
        Array.toString(MutQueue.toArray(rc, pq)) == "Array#{6, 4, 5, 1, 3, 2}"
    }

}
