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
        let mq = (MutQueue.empty(rc) : MutQueue[Char, rc]);
        MutQueue.toString(mq) == "MutQueue {}"
    }

    @Test
    def testToString02(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueue(mq, 3);
        MutQueue.toString(mq) == "MutQueue {3}"
    }

    @Test
    def testToString03(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueueAll(mq, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        MutQueue.dequeue(mq);
        MutQueue.dequeue(mq);
        MutQueue.toString(mq) == "MutQueue {6, 4, 5, 1, 3, 2}"
    }

    /////////////////////////////////////////////////////////////////////////////
    // new                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testEmpty01(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.size(mq) == 0
    }

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testSize01(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.size(mq) == 0
    }

    @Test
    def testSize02(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueueAll(mq, "a" :: "b" :: "c" :: "d" :: "e" :: Nil);
        MutQueue.size(mq) == 5
    }

    @Test
    def testSize03(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueueAll(mq, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        MutQueue.dequeue(mq);
        MutQueue.size(mq) == 7
    }

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testIsEmpty01(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.isEmpty(mq)
    }

    @Test
    def testIsEmpty02(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueue(mq, "a");
        not MutQueue.isEmpty(mq)
    }

    @Test
    def testIsEmpty03(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueueAll(mq, 1 :: 2 :: 3 :: Nil);
        MutQueue.dequeue(mq);
        MutQueue.dequeue(mq);
        MutQueue.dequeue(mq);
        MutQueue.isEmpty(mq)
    }

    @Test def testIsEmpty04(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueueAll(mq, 1 :: 2 :: Nil);
        MutQueue.dequeue(mq);
        MutQueue.dequeue(mq);
        MutQueue.enqueue(mq, 1);
        not MutQueue.isEmpty(mq)
    }

    /////////////////////////////////////////////////////////////////////////////
    // nonEmpty                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testNonEmpty01(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        not MutQueue.nonEmpty(mq)
    }

    @Test
    def testNonEmpty02(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueue(mq, "a");
        MutQueue.nonEmpty(mq)
    }

    @Test
    def testNonEmpty03(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueueAll(mq, 1 :: 2 :: 3 :: Nil);
        MutQueue.dequeue(mq);
        MutQueue.dequeue(mq);
        MutQueue.dequeue(mq);
        not MutQueue.nonEmpty(mq)
    }

    @Test
    def testNonEmpty04(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueueAll(mq, 1 :: 2 :: Nil);
        MutQueue.dequeue(mq);
        MutQueue.dequeue(mq);
        MutQueue.enqueue(mq, 1);
        MutQueue.nonEmpty(mq)
    }

    /////////////////////////////////////////////////////////////////////////////
    // peek                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testPeek01(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueue(mq, 3);
        MutQueue.dequeue(mq);
        MutQueue.peek(mq) == None
    }

    @Test
    def testPeek02(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueue(mq, 1);
        MutQueue.peek(mq) == Some(1)
    }

    @Test
    def testPeek03(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueueAll(mq, 1 :: 9 :: 2 :: 8 :: 3 :: 7 :: Nil);
        MutQueue.dequeue(mq);
        MutQueue.peek(mq) == Some(8)
    }

    @Test
    def testPeek04(): Bool = region rc {
        let mq = (MutQueue.empty(rc) : MutQueue[Int32, rc]);
        MutQueue.peek(mq) == None
    }

    @Test
    def testPeek05(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueue(mq, 5);
        MutQueue.enqueue(mq, 5);
        MutQueue.dequeue(mq);
        MutQueue.peek(mq) == Some(5)
    }

    /////////////////////////////////////////////////////////////////////////////
    // enqueue                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testEnqueue01(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueue(mq, "a");
        MutQueue.size(mq) == 1
    }

    @Test
    def testEnqueue02(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueue(mq, 3);
        MutQueue.enqueue(mq, 2);
        MutQueue.toString(mq) == "MutQueue {3, 2}"
    }

    @Test
    def testEnqueue03(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueue(mq, 2);
        MutQueue.enqueue(mq, 3);
        MutQueue.toString(mq) == "MutQueue {3, 2}"
    }

    @Test
    def testEnqueue04(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueue(mq, "a");
        MutQueue.enqueue(mq, "b");
        MutQueue.enqueue(mq, "c");
        MutQueue.enqueue(mq, "a");
        MutQueue.enqueue(mq, "a");
        MutQueue.peek(mq) == Some("c")
    }

    @Test
    def testEnqueue05(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueue(mq, 3);
        MutQueue.enqueue(mq, 4);
        MutQueue.enqueue(mq, 5);
        MutQueue.enqueue(mq, 6);
        MutQueue.enqueue(mq, 10);
        MutQueue.enqueue(mq, 5);
        MutQueue.enqueue(mq, 7);
        MutQueue.enqueue(mq, 14);
        MutQueue.enqueue(mq, 20);
        MutQueue.peek(mq) == Some(20)
    }

    /////////////////////////////////////////////////////////////////////////////
    // dequeue                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testDequeue01(): Bool = region rc {
        let mq = (MutQueue.empty(rc) : MutQueue[Int32, rc]);
        MutQueue.dequeue(mq);
        MutQueue.size(mq) == 0
    }

    @Test
    def testDequeue02(): Bool = region rc {
        let mq = (MutQueue.empty(rc) : MutQueue[Int32, rc]);
        MutQueue.dequeue(mq) == None
    }

    @Test
    def testDequeue03(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueueAll(mq, 1 :: 2 :: 3 :: 4 :: 5 :: Nil);
        MutQueue.dequeue(mq);
        MutQueue.dequeue(mq);
        MutQueue.size(mq) == 3
    }

    @Test
    def testDequeue04(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueueAll(mq, 1 :: 2 :: 3 :: 4 :: 5 :: Nil);
        MutQueue.dequeue(mq);
        MutQueue.dequeue(mq) == Some(4)
    }

    @Test
    def testDequeue05(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueueAll(mq, 1 :: 2 :: 3 :: Nil);
        MutQueue.dequeue(mq);
        MutQueue.dequeue(mq);
        MutQueue.dequeue(mq);
        MutQueue.size(mq) == 0
    }

    /////////////////////////////////////////////////////////////////////////////
    // enqueueAll                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testEnqueueAll01(): Bool = region rc {
        let mq = (MutQueue.empty(rc) : MutQueue[Int32, rc]);
        MutQueue.enqueueAll(mq, Nil);
        MutQueue.toString(mq) == "MutQueue {}"
    }

    @Test
    def testEnqueueAll02(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueueAll(mq, Nil);
        MutQueue.enqueueAll(mq, 1 :: Nil);
        MutQueue.toString(mq) == "MutQueue {1}"
    }

    @Test
    def testEnqueueAll03(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueueAll(mq, Nil);
        MutQueue.enqueueAll(mq, 1 :: 2 :: 3 :: Nil);
        MutQueue.enqueueAll(mq, 4 :: 5 :: 6 :: Nil);
        MutQueue.toString(mq) == "MutQueue {6, 4, 5, 1, 3, 2}"
    }

    @Test
    def testEnqueueAll04(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueueAll(mq, Nil);
        MutQueue.enqueueAll(mq, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        MutQueue.enqueueAll(mq, Nil);
        MutQueue.toString(mq) == "MutQueue {8, 7, 6, 4, 3, 2, 5, 1}"
    }

    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testIterator01(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        let it = MutQueue.iterator(rc, mq);
        let sum = ref 0 @ rc;
        Iterator.forEach(x -> Ref.put(deref sum + x, sum), it);
        deref sum == 0
    }

    @Test
    def testIterator02(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueueAll(mq, 1 :: Nil);
        let it = MutQueue.iterator(rc, mq);
        Iterator.toList(it) == 1 :: Nil
    }

    @Test
    def testIterator03(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueueAll(mq, "a" :: "c" :: "g" :: "t" :: Nil);
        let it = MutQueue.iterator(rc, mq);
        let str = ref "" @ rc;
        Iterator.forEach(x -> Ref.put(String.concat(x, deref str), str), it);
        deref str == "acgt"
    }

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testToList01(): Bool = region rc {
        let mq = (MutQueue.empty(rc) : MutQueue[Int32, rc]);
        MutQueue.toList(mq) == Nil
    }

    @Test
    def testToList02(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueue(mq, 1);
        MutQueue.toList(mq) == 1 :: Nil
    }

    @Test
    def testToList03(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueueAll(mq, 1 :: 2 :: Nil);
        MutQueue.toList(mq) == 2 :: 1 :: Nil
    }

    @Test
    def testToList04(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueueAll(mq, 1 :: 2 :: Nil);
        MutQueue.toList(mq) == 2 :: 1 :: Nil
    }

    @Test
    def testToList05(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueueAll(mq, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        MutQueue.toList(mq) == 8 :: 7 :: 6 :: 4 :: 3 :: 2 :: 5 :: 1 :: Nil
    }

    @Test
    def testToList06(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueueAll(mq, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        MutQueue.dequeue(mq);
        MutQueue.dequeue(mq);
        MutQueue.toList(mq) == 6 :: 4 :: 5 :: 1 :: 3 :: 2 :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // toNel                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testToNel01(): Bool = region rc {
        let mq = (MutQueue.empty(rc) : MutQueue[Int32, rc]);
        MutQueue.toNel(mq) == None
    }

    @Test
    def testToNel02(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueue(mq, 1);
        MutQueue.toNel(mq) == Some(Nel.Nel(1, Nil))
    }

    @Test
    def testToNel03(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueueAll(mq, 1 :: 2 :: 3 :: Nil);
        MutQueue.toNel(mq) == Some(Nel.Nel(3, 1 :: 2 :: Nil))
    }

    @Test
    def testToNel04(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueueAll(mq, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        MutQueue.toNel(mq) == Some(Nel.Nel(8, 7 :: 6 :: 4 :: 3 :: 2 :: 5 :: 1 :: Nil))
    }

    @Test
    def testToNel05(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueueAll(mq, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        MutQueue.dequeue(mq);
        MutQueue.dequeue(mq);
        MutQueue.toNel(mq) == Some(Nel.Nel(6, 4 :: 5 :: 1 :: 3 :: 2 :: Nil))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testToArray01(): Bool = region rc {
        let mq = (MutQueue.empty(rc) : MutQueue[Int32, rc]);
        Array.toString(MutQueue.toArray(rc, mq)) == "Array#{}"
    }

    @Test
    def testToArray02(): Bool = region rc {
        let mq = (MutQueue.empty(rc) : MutQueue[Int32, rc]);
        MutQueue.enqueue(mq, 1);
        Array.toString(MutQueue.toArray(rc, mq)) == "Array#{1}"
    }

    @Test
    def testToArray03(): Bool = region rc {
        let mq = (MutQueue.empty(rc) : MutQueue[Int32, rc]);
        MutQueue.enqueueAll(mq, 1 :: 2 :: Nil);
        Array.toString(MutQueue.toArray(rc, mq)) == "Array#{2, 1}"
    }

    @Test
    def testToArray04(): Bool = region rc {
        let mq = (MutQueue.empty(rc) : MutQueue[Int32, rc]);
        MutQueue.enqueueAll(mq, 2 :: 1 :: Nil);
        Array.toString(MutQueue.toArray(rc, mq)) == "Array#{2, 1}"
    }

    @Test
    def testToArray05(): Bool = region rc {
        let mq = (MutQueue.empty(rc) : MutQueue[Int32, rc]);
        MutQueue.enqueueAll(mq, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        Array.toString(MutQueue.toArray(rc, mq)) == "Array#{8, 7, 6, 4, 3, 2, 5, 1}"
    }

    @Test
    def testToArray06(): Bool = region rc {
        let mq = (MutQueue.empty(rc) : MutQueue[Int32, rc]);
        MutQueue.enqueueAll(mq, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        MutQueue.dequeue(mq);
        MutQueue.dequeue(mq);
        Array.toString(MutQueue.toArray(rc, mq)) == "Array#{6, 4, 5, 1, 3, 2}"
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testToVector01(): Bool = region rc {
        let mq = (MutQueue.empty(rc) : MutQueue[Int32, rc]);
        MutQueue.toVector(mq) == (Vector#{} : Vector[Int32])
    }

    @Test
    def testToVector02(): Bool = region rc {
        let mq = (MutQueue.empty(rc) : MutQueue[Int32, rc]);
        MutQueue.enqueue(mq, 1);
        MutQueue.toVector(mq) == Vector#{1}
    }

    @Test
    def testToVector03(): Bool = region rc {
        let mq = (MutQueue.empty(rc) : MutQueue[Int32, rc]);
        MutQueue.enqueueAll(mq, 1 :: 2 :: Nil);
        MutQueue.toVector(mq) == Vector#{2, 1}
    }

    @Test
    def testToVector04(): Bool = region rc {
        let mq = (MutQueue.empty(rc) : MutQueue[Int32, rc]);
        MutQueue.enqueueAll(mq, 2 :: 1 :: Nil);
        MutQueue.toVector(mq) == Vector#{2, 1}
    }

    @Test
    def testToVector05(): Bool = region rc {
        let mq = (MutQueue.empty(rc) : MutQueue[Int32, rc]);
        MutQueue.enqueueAll(mq, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        MutQueue.toVector(mq) == Vector#{8, 7, 6, 4, 3, 2, 5, 1}
    }

    @Test
    def testToVector06(): Bool = region rc {
        let mq = (MutQueue.empty(rc) : MutQueue[Int32, rc]);
        MutQueue.enqueueAll(mq, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        MutQueue.dequeue(mq);
        MutQueue.dequeue(mq);
        MutQueue.toVector(mq) == Vector#{6, 4, 5, 1, 3, 2}
    }

}
