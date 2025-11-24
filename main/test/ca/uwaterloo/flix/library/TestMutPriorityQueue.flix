mod TestMutPriorityQueue {

    use Assert.{assertEq, assertTrue, assertFalse};

    /////////////////////////////////////////////////////////////////////////////
    // emptyWithCapacity                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def emptyWithCapacity01(): Unit \ Assert = region rc {
        let mq: MutPriorityQueue[Int32, rc] = MutPriorityQueue.emptyWithCapacity(rc, 10);
        assertEq(expected = "MutPriorityQueue {}", MutPriorityQueue.toString(mq))
    }

    @Test
    def emptyWithCapacity02(): Unit \ Assert = region rc {
        let mq: MutPriorityQueue[Int32, rc] = MutPriorityQueue.emptyWithCapacity(rc, -1);
        assertEq(expected = "MutPriorityQueue {}", MutPriorityQueue.toString(mq))
    }

    @Test
    def emptyWithCapacity03(): Unit \ Assert = region rc {
        let mq: MutPriorityQueue[Int32, rc] = MutPriorityQueue.emptyWithCapacity(rc, 0);
        MutPriorityQueue.enqueue(4, mq);
        MutPriorityQueue.enqueue(5, mq);
        assertEq(expected = "MutPriorityQueue {5, 4}", MutPriorityQueue.toString(mq))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toString01(): Unit \ Assert = region rc {
        let mq = (MutPriorityQueue.empty(rc) : MutPriorityQueue[Char, rc]);
        assertEq(expected = "MutPriorityQueue {}", MutPriorityQueue.toString(mq))
    }

    @Test
    def toString02(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueue(3, mq);
        assertEq(expected = "MutPriorityQueue {3}", MutPriorityQueue.toString(mq))
    }

    @Test
    def toString03(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueueAll(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil, mq);
        discard MutPriorityQueue.dequeue(mq);
        discard MutPriorityQueue.dequeue(mq);
        assertEq(expected = "MutPriorityQueue {6, 4, 5, 1, 3, 2}", MutPriorityQueue.toString(mq))
    }

    /////////////////////////////////////////////////////////////////////////////
    // new                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def empty01(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        assertEq(expected = 0, MutPriorityQueue.size(mq))
    }

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def size01(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        assertEq(expected = 0, MutPriorityQueue.size(mq))
    }

    @Test
    def size02(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueueAll("a" :: "b" :: "c" :: "d" :: "e" :: Nil, mq);
        assertEq(expected = 5, MutPriorityQueue.size(mq))
    }

    @Test
    def size03(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueueAll(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil, mq);
        discard MutPriorityQueue.dequeue(mq);
        assertEq(expected = 7, MutPriorityQueue.size(mq))
    }

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isEmpty01(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        assertTrue(MutPriorityQueue.isEmpty(mq))
    }

    @Test
    def isEmpty02(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueue("a", mq);
        assertFalse(MutPriorityQueue.isEmpty(mq))
    }

    @Test
    def isEmpty03(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueueAll(1 :: 2 :: 3 :: Nil, mq);
        discard MutPriorityQueue.dequeue(mq);
        discard MutPriorityQueue.dequeue(mq);
        discard MutPriorityQueue.dequeue(mq);
        assertTrue(MutPriorityQueue.isEmpty(mq))
    }

    @Test
    def isEmpty04(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueueAll(1 :: 2 :: Nil, mq);
        discard MutPriorityQueue.dequeue(mq);
        discard MutPriorityQueue.dequeue(mq);
        MutPriorityQueue.enqueue(1, mq);
        assertFalse(MutPriorityQueue.isEmpty(mq))
    }

    /////////////////////////////////////////////////////////////////////////////
    // nonEmpty                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def nonEmpty01(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        assertFalse(MutPriorityQueue.nonEmpty(mq))
    }

    @Test
    def nonEmpty02(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueue("a", mq);
        assertTrue(MutPriorityQueue.nonEmpty(mq))
    }

    @Test
    def nonEmpty03(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueueAll(1 :: 2 :: 3 :: Nil, mq);
        discard MutPriorityQueue.dequeue(mq);
        discard MutPriorityQueue.dequeue(mq);
        discard MutPriorityQueue.dequeue(mq);
        assertFalse(MutPriorityQueue.nonEmpty(mq))
    }

    @Test
    def nonEmpty04(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueueAll(1 :: 2 :: Nil, mq);
        discard MutPriorityQueue.dequeue(mq);
        discard MutPriorityQueue.dequeue(mq);
        MutPriorityQueue.enqueue(1, mq);
        assertTrue(MutPriorityQueue.nonEmpty(mq))
    }

    /////////////////////////////////////////////////////////////////////////////
    // peek                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def peek01(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueue(3, mq);
        discard MutPriorityQueue.dequeue(mq);
        assertEq(expected = None, MutPriorityQueue.peek(mq))
    }

    @Test
    def peek02(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueue(1, mq);
        assertEq(expected = Some(1), MutPriorityQueue.peek(mq))
    }

    @Test
    def peek03(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueueAll(1 :: 9 :: 2 :: 8 :: 3 :: 7 :: Nil, mq);
        discard MutPriorityQueue.dequeue(mq);
        assertEq(expected = Some(8), MutPriorityQueue.peek(mq))
    }

    @Test
    def peek04(): Unit \ Assert = region rc {
        let mq = (MutPriorityQueue.empty(rc) : MutPriorityQueue[Int32, rc]);
        assertEq(expected = None, MutPriorityQueue.peek(mq))
    }

    @Test
    def peek05(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueue(5, mq);
        MutPriorityQueue.enqueue(5, mq);
        discard MutPriorityQueue.dequeue(mq);
        assertEq(expected = Some(5), MutPriorityQueue.peek(mq))
    }

    /////////////////////////////////////////////////////////////////////////////
    // enqueue                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def enqueue01(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueue("a", mq);
        assertEq(expected = 1, MutPriorityQueue.size(mq))
    }

    @Test
    def enqueue02(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueue(3, mq);
        MutPriorityQueue.enqueue(2, mq);
        assertEq(expected = "MutPriorityQueue {3, 2}", MutPriorityQueue.toString(mq))
    }

    @Test
    def enqueue03(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueue(2, mq);
        MutPriorityQueue.enqueue(3, mq);
        assertEq(expected = "MutPriorityQueue {3, 2}", MutPriorityQueue.toString(mq))
    }

    @Test
    def enqueue04(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueue("a", mq);
        MutPriorityQueue.enqueue("b", mq);
        MutPriorityQueue.enqueue("c", mq);
        MutPriorityQueue.enqueue("a", mq);
        MutPriorityQueue.enqueue("a", mq);
        assertEq(expected = Some("c"), MutPriorityQueue.peek(mq))
    }

    @Test
    def enqueue05(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueue(3, mq);
        MutPriorityQueue.enqueue(4, mq);
        MutPriorityQueue.enqueue(5, mq);
        MutPriorityQueue.enqueue(6, mq);
        MutPriorityQueue.enqueue(10, mq);
        MutPriorityQueue.enqueue(5, mq);
        MutPriorityQueue.enqueue(7, mq);
        MutPriorityQueue.enqueue(14, mq);
        MutPriorityQueue.enqueue(20, mq);
        assertEq(expected = Some(20), MutPriorityQueue.peek(mq))
    }

    /////////////////////////////////////////////////////////////////////////////
    // dequeue                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dequeue01(): Unit \ Assert = region rc {
        let mq = (MutPriorityQueue.empty(rc) : MutPriorityQueue[Int32, rc]);
        discard MutPriorityQueue.dequeue(mq);
        assertEq(expected = 0, MutPriorityQueue.size(mq))
    }

    @Test
    def dequeue02(): Unit \ Assert = region rc {
        let mq = (MutPriorityQueue.empty(rc) : MutPriorityQueue[Int32, rc]);
        assertEq(expected = None, MutPriorityQueue.dequeue(mq))
    }

    @Test
    def dequeue03(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueueAll(1 :: 2 :: 3 :: 4 :: 5 :: Nil, mq);
        discard MutPriorityQueue.dequeue(mq);
        discard MutPriorityQueue.dequeue(mq);
        assertEq(expected = 3, MutPriorityQueue.size(mq))
    }

    @Test
    def dequeue04(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueueAll(1 :: 2 :: 3 :: 4 :: 5 :: Nil, mq);
        discard MutPriorityQueue.dequeue(mq);
        assertEq(expected = Some(4), MutPriorityQueue.dequeue(mq))
    }

    @Test
    def dequeue05(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueueAll(1 :: 2 :: 3 :: Nil, mq);
        discard MutPriorityQueue.dequeue(mq);
        discard MutPriorityQueue.dequeue(mq);
        discard MutPriorityQueue.dequeue(mq);
        assertEq(expected = 0, MutPriorityQueue.size(mq))
    }

    /////////////////////////////////////////////////////////////////////////////
    // enqueueAll                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def enqueueAll01(): Unit \ Assert = region rc {
        let mq = (MutPriorityQueue.empty(rc) : MutPriorityQueue[Int32, rc]);
        MutPriorityQueue.enqueueAll(Nil, mq);
        assertEq(expected = "MutPriorityQueue {}", MutPriorityQueue.toString(mq))
    }

    @Test
    def enqueueAll02(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueueAll(Nil, mq);
        MutPriorityQueue.enqueueAll(1 :: Nil, mq);
        assertEq(expected = "MutPriorityQueue {1}", MutPriorityQueue.toString(mq))
    }

    @Test
    def enqueueAll03(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueueAll(Nil, mq);
        MutPriorityQueue.enqueueAll(1 :: 2 :: 3 :: Nil, mq);
        MutPriorityQueue.enqueueAll(4 :: 5 :: 6 :: Nil, mq);
        assertEq(expected = "MutPriorityQueue {6, 4, 5, 1, 3, 2}", MutPriorityQueue.toString(mq))
    }

    @Test
    def enqueueAll04(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueueAll(Nil, mq);
        MutPriorityQueue.enqueueAll(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil, mq);
        MutPriorityQueue.enqueueAll(Nil, mq);
        assertEq(expected = "MutPriorityQueue {8, 7, 6, 4, 3, 2, 5, 1}", MutPriorityQueue.toString(mq))
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Unit \ Assert = region rc {
        let ml = MutList.empty(rc);
        let mq: MutPriorityQueue[Int32, rc] = MutPriorityQueue.empty(rc);
        MutPriorityQueue.forEach(x -> MutList.push(x, ml), mq);
        assertTrue(MutList.isEmpty(ml))
    }

    @Test
    def forEach02(): Unit \ Assert = region rc {
        let startVec = Vector#{1};
        let sortedStart = startVec |> Vector.sort;
        let ml = MutList.empty(rc);
        let mq: MutPriorityQueue[Int32, rc] = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueueAll(startVec, mq);
        MutPriorityQueue.forEach(x -> MutList.push(x, ml), mq);
        let resultVec = MutList.toVector(ml) |> Vector.sort;
        assertEq(expected = sortedStart, resultVec)
    }

    @Test
    def forEach03(): Unit \ Assert = region rc {
        let startVec = Vector#{8, 1, 2, 8, 9, 5, 6};
        let sortedStart = startVec |> Vector.sort;
        let ml = MutList.empty(rc);
        let mq: MutPriorityQueue[Int32, rc] = MutPriorityQueue.emptyWithCapacity(rc, 8);
        MutPriorityQueue.enqueueAll(startVec, mq);
        MutPriorityQueue.forEach(x -> MutList.push(x, ml), mq);
        let resultVec = MutList.toVector(ml) |> Vector.sort;
        assertEq(expected = sortedStart, resultVec)
    }

    @Test
    def forEach04(): Unit \ Assert = region rc {
        let startVec = Vector#{8, 1, 2, 8, 9, 5, 6, 3};
        let sortedStart = startVec |> Vector.sort;
        let ml = MutList.empty(rc);
        let mq: MutPriorityQueue[Int32, rc] = MutPriorityQueue.emptyWithCapacity(rc, 8);
        MutPriorityQueue.enqueueAll(startVec, mq);
        MutPriorityQueue.forEach(x -> MutList.push(x, ml), mq);
        let resultVec = MutList.toVector(ml) |> Vector.sort;
        assertEq(expected = sortedStart, resultVec)
    }

    @Test
    def forEach05(): Unit \ Assert = region rc {
        let startVec = Vector#{8, 1, 2, 8, 9, 5, 6, 3, 2};
        let sortedStart = startVec |> Vector.sort;
        let ml = MutList.empty(rc);
        let mq: MutPriorityQueue[Int32, rc] = MutPriorityQueue.emptyWithCapacity(rc, 8);
        MutPriorityQueue.enqueueAll(startVec, mq);
        MutPriorityQueue.forEach(x -> MutList.push(x, ml), mq);
        let resultVec = MutList.toVector(ml) |> Vector.sort;
        assertEq(expected = sortedStart, resultVec)
    }


    @Test
    def forEach06(): Unit \ Assert = region rc {
        let startVec = Vector#{(8, 1), (1, 2), (2, 4), (8, 9), (8, 9), (5, 3), (6, 2), (3, 9), (2, 20)};
        let sortedStart = startVec |> Vector.sort;
        let ml = MutList.empty(rc);
        let mq: MutPriorityQueue[(Int32, Int32), rc] = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueueAll(startVec, mq);
        MutPriorityQueue.forEach(x -> MutList.push(x, ml), mq);
        let resultVec = MutList.toVector(ml) |> Vector.sort;
        assertEq(expected = sortedStart, resultVec)
    }

    @Test
    def forEach07(): Unit \ Assert = region rc {
        let startVec = Vector#{8, 1, 2, 8, 9, 5, 6, 3, 2};
        let sortedStart = Vector#{8, 1, 2, 5, 6, 3, 2} |> Vector.sort;
        let ml = MutList.empty(rc);
        let mq: MutPriorityQueue[Int32, rc] = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueueAll(startVec, mq);
        discard MutPriorityQueue.dequeue(mq);
        discard MutPriorityQueue.dequeue(mq);
        MutPriorityQueue.forEach(x -> MutList.push(x, ml), mq);
        let resultVec = MutList.toVector(ml) |> Vector.sort;
        assertEq(expected = sortedStart, resultVec)
    }

    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterator01(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        let it = MutPriorityQueue.iterator(rc, mq);
        let sum = Ref.fresh(rc, 0);
        Iterator.forEach(x -> Ref.put(Ref.get(sum) + x, sum), it);
        assertEq(expected = 0, Ref.get(sum))
    }

    @Test
    def iterator02(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueueAll(1 :: Nil, mq);
        let it = MutPriorityQueue.iterator(rc, mq);
        assertEq(expected = 1 :: Nil, Iterator.toList(it))
    }

    @Test
    def iterator03(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueueAll("a" :: "c" :: "g" :: "t" :: Nil, mq);
        let it = MutPriorityQueue.iterator(rc, mq);
        let str = Ref.fresh(rc, "");
        Iterator.forEach(x -> Ref.put(String.concat(x, Ref.get(str)), str), it);
        assertEq(expected = "acgt", Ref.get(str))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toList01(): Unit \ Assert = region rc {
        let mq = (MutPriorityQueue.empty(rc) : MutPriorityQueue[Int32, rc]);
        assertEq(expected = Nil, MutPriorityQueue.toList(mq))
    }

    @Test
    def toList02(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueue(1, mq);
        assertEq(expected = 1 :: Nil, MutPriorityQueue.toList(mq))
    }

    @Test
    def toList03(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueueAll(1 :: 2 :: Nil, mq);
        assertEq(expected = 2 :: 1 :: Nil, MutPriorityQueue.toList(mq))
    }

    @Test
    def toList04(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueueAll(1 :: 2 :: Nil, mq);
        assertEq(expected = 2 :: 1 :: Nil, MutPriorityQueue.toList(mq))
    }

    @Test
    def toList05(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueueAll(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil, mq);
        assertEq(expected = 8 :: 7 :: 6 :: 4 :: 3 :: 2 :: 5 :: 1 :: Nil, MutPriorityQueue.toList(mq))
    }

    @Test
    def toList06(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueueAll(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil, mq);
        discard MutPriorityQueue.dequeue(mq);
        discard MutPriorityQueue.dequeue(mq);
        assertEq(expected = 6 :: 4 :: 5 :: 1 :: 3 :: 2 :: Nil, MutPriorityQueue.toList(mq))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toNel                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toNel01(): Unit \ Assert = region rc {
        let mq = (MutPriorityQueue.empty(rc) : MutPriorityQueue[Int32, rc]);
        assertEq(expected = None, MutPriorityQueue.toNel(mq))
    }

    @Test
    def toNel02(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueue(1, mq);
        assertEq(expected = Some(Nel.Nel(1, Nil)), MutPriorityQueue.toNel(mq))
    }

    @Test
    def toNel03(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueueAll(1 :: 2 :: 3 :: Nil, mq);
        assertEq(expected = Some(Nel.Nel(3, 1 :: 2 :: Nil)), MutPriorityQueue.toNel(mq))
    }

    @Test
    def toNel04(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueueAll(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil, mq);
        assertEq(expected = Some(Nel.Nel(8, 7 :: 6 :: 4 :: 3 :: 2 :: 5 :: 1 :: Nil)), MutPriorityQueue.toNel(mq))
    }

    @Test
    def toNel05(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueueAll(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil, mq);
        discard MutPriorityQueue.dequeue(mq);
        discard MutPriorityQueue.dequeue(mq);
        assertEq(expected = Some(Nel.Nel(6, 4 :: 5 :: 1 :: 3 :: 2 :: Nil)), MutPriorityQueue.toNel(mq))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toArray01(): Unit \ Assert = region rc {
        let mq = (MutPriorityQueue.empty(rc) : MutPriorityQueue[Int32, rc]);
        assertEq(expected = "Array#{}", Array.toString(MutPriorityQueue.toArray(rc, mq)))
    }

    @Test
    def toArray02(): Unit \ Assert = region rc {
        let mq = (MutPriorityQueue.empty(rc) : MutPriorityQueue[Int32, rc]);
        MutPriorityQueue.enqueue(1, mq);
        assertEq(expected = "Array#{1}", Array.toString(MutPriorityQueue.toArray(rc, mq)))
    }

    @Test
    def toArray03(): Unit \ Assert = region rc {
        let mq = (MutPriorityQueue.empty(rc) : MutPriorityQueue[Int32, rc]);
        MutPriorityQueue.enqueueAll(1 :: 2 :: Nil, mq);
        assertEq(expected = "Array#{2, 1}", Array.toString(MutPriorityQueue.toArray(rc, mq)))
    }

    @Test
    def toArray04(): Unit \ Assert = region rc {
        let mq = (MutPriorityQueue.empty(rc) : MutPriorityQueue[Int32, rc]);
        MutPriorityQueue.enqueueAll(2 :: 1 :: Nil, mq);
        assertEq(expected = "Array#{2, 1}", Array.toString(MutPriorityQueue.toArray(rc, mq)))
    }

    @Test
    def toArray05(): Unit \ Assert = region rc {
        let mq = (MutPriorityQueue.empty(rc) : MutPriorityQueue[Int32, rc]);
        MutPriorityQueue.enqueueAll(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil, mq);
        assertEq(expected = "Array#{8, 7, 6, 4, 3, 2, 5, 1}", Array.toString(MutPriorityQueue.toArray(rc, mq)))
    }

    @Test
    def toArray06(): Unit \ Assert = region rc {
        let mq = (MutPriorityQueue.empty(rc) : MutPriorityQueue[Int32, rc]);
        MutPriorityQueue.enqueueAll(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil, mq);
        discard MutPriorityQueue.dequeue(mq);
        discard MutPriorityQueue.dequeue(mq);
        assertEq(expected = "Array#{6, 4, 5, 1, 3, 2}", Array.toString(MutPriorityQueue.toArray(rc, mq)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toVector01(): Unit \ Assert = region rc {
        let mq = (MutPriorityQueue.empty(rc) : MutPriorityQueue[Int32, rc]);
        assertEq(expected = (Vector#{} : Vector[Int32]), MutPriorityQueue.toVector(mq))
    }

    @Test
    def toVector02(): Unit \ Assert = region rc {
        let mq = (MutPriorityQueue.empty(rc) : MutPriorityQueue[Int32, rc]);
        MutPriorityQueue.enqueue(1, mq);
        assertEq(expected = Vector#{1}, MutPriorityQueue.toVector(mq))
    }

    @Test
    def toVector03(): Unit \ Assert = region rc {
        let mq = (MutPriorityQueue.empty(rc) : MutPriorityQueue[Int32, rc]);
        MutPriorityQueue.enqueueAll(1 :: 2 :: Nil, mq);
        assertEq(expected = Vector#{2, 1}, MutPriorityQueue.toVector(mq))
    }

    @Test
    def toVector04(): Unit \ Assert = region rc {
        let mq = (MutPriorityQueue.empty(rc) : MutPriorityQueue[Int32, rc]);
        MutPriorityQueue.enqueueAll(2 :: 1 :: Nil, mq);
        assertEq(expected = Vector#{2, 1}, MutPriorityQueue.toVector(mq))
    }

    @Test
    def toVector05(): Unit \ Assert = region rc {
        let mq = (MutPriorityQueue.empty(rc) : MutPriorityQueue[Int32, rc]);
        MutPriorityQueue.enqueueAll(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil, mq);
        assertEq(expected = Vector#{8, 7, 6, 4, 3, 2, 5, 1}, MutPriorityQueue.toVector(mq))
    }

    @Test
    def toVector06(): Unit \ Assert = region rc {
        let mq = (MutPriorityQueue.empty(rc) : MutPriorityQueue[Int32, rc]);
        MutPriorityQueue.enqueueAll(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil, mq);
        discard MutPriorityQueue.dequeue(mq);
        discard MutPriorityQueue.dequeue(mq);
        assertEq(expected = Vector#{6, 4, 5, 1, 3, 2}, MutPriorityQueue.toVector(mq))
    }

}
