mod TestMutDeque {

    use Assert.{assertEq, assertNeq, assertTrue, assertFalse, fail}

    /////////////////////////////////////////////////////////////////////////////
    // emptyWithCapacity                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def emptyWithCapacity01(): Unit \ Assert = region rc {
        let l: MutDeque[Int32, rc] = MutDeque.emptyWithCapacity(rc, 10);
        assertEq(expected = "MutDeque#{}", MutDeque.toString(l))
    }

    @Test
    def emptyWithCapacity02(): Unit \ Assert = region rc {
        let l: MutDeque[Int32, rc] = MutDeque.emptyWithCapacity(rc, -1);
        assertEq(expected = "MutDeque#{}", MutDeque.toString(l))
    }

    @Test
    def emptyWithCapacity03(): Unit \ Assert = region rc {
        let l: MutDeque[Int32, rc] = MutDeque.emptyWithCapacity(rc, 0);
        MutDeque.pushBack(5, l);
        MutDeque.pushBack(4, l);
        assertEq(expected = "MutDeque#{5, 4}", MutDeque.toString(l))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toString01(): Unit \ Assert = region rc {
        let d: MutDeque[Int32, rc] = MutDeque.empty(rc);
        assertEq(expected = "MutDeque#{}", MutDeque.toString(d))
    }

    @Test
    def toString02(): Unit \ Assert = region rc {
        let d: MutDeque[Int32, rc] = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        assertEq(expected = "MutDeque#{1}", MutDeque.toString(d))
    }

    @Test
    def toString03(): Unit \ Assert = region rc {
        let d: MutDeque[Int32, rc] = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.pushBack(4, d);
        MutDeque.pushBack(5, d);
        assertEq(expected = "MutDeque#{1, 2, 3, 4, 5}", MutDeque.toString(d))
    }

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def size01(): Unit \ Assert = region rc {
        assertEq(expected = 0, MutDeque.size(MutDeque.empty(rc)))
    }

    @Test
    def size02(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);

        MutDeque.pushBack(1, d);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(1, d);

        assertEq(expected = 4, MutDeque.size(d))
    }

    @Test
    def size03(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);

        MutDeque.pushBack(1, d);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(1, d);

        MutDeque.pushBack(1, d);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(1, d);

        assertEq(expected = 8, MutDeque.size(d))
    }

    @Test
    def size04(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);

        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);

        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);

        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);

        assertEq(expected = 11, MutDeque.size(d))
    }

    @Test
    def size05(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);

        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);

        discard MutDeque.popFront(d);
        discard MutDeque.popFront(d);

        assertEq(expected = 2, MutDeque.size(d))

    }

    @Test
    def size06(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);

        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);

        discard MutDeque.popFront(d);
        discard MutDeque.popFront(d);
        discard MutDeque.popFront(d);
        discard MutDeque.popFront(d);

        assertEq(expected = 0, MutDeque.size(d))
    }

    @Test
    def size07(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushFront(1, d);
        discard MutDeque.popBack(d);
        assertEq(expected = 0, MutDeque.size(d))
    }

    @Test
    def size08(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        discard MutDeque.popFront(d);
        assertEq(expected = 0, MutDeque.size(d))
    }


    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isEmpty01(): Unit \ Assert = region rc {
        assertTrue(MutDeque.isEmpty(MutDeque.empty(rc)))
    }

    @Test
    def isEmpty02(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        assertFalse(MutDeque.isEmpty(d))
    }

    @Test
    def isEmpty03(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        discard MutDeque.popBack(d);
        assertTrue(MutDeque.isEmpty(d))
    }

    @Test
    def isEmpty04(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushFront(1, d);
        assertFalse(MutDeque.isEmpty(d))
    }

    @Test
    def isEmpty05(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushFront(1, d);
        discard MutDeque.popFront(d);
        assertTrue(MutDeque.isEmpty(d))
    }


    /////////////////////////////////////////////////////////////////////////////
    // nonEmpty                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def nonEmpty01(): Unit \ Assert = region rc {
        assertFalse(MutDeque.nonEmpty(MutDeque.empty(rc)))
    }

    @Test
    def nonEmpty02(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        assertTrue(MutDeque.nonEmpty(d))
    }

    @Test
    def nonEmpty03(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        discard MutDeque.popBack(d);
        assertFalse(MutDeque.nonEmpty(d))
    }

    @Test
    def nonEmpty04(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushFront(1, d);
        assertTrue(MutDeque.nonEmpty(d))
    }

    @Test
    def nonEmpty05(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushFront(1, d);
        discard MutDeque.popFront(d);
        assertFalse(MutDeque.nonEmpty(d))
    }


    /////////////////////////////////////////////////////////////////////////////
    // popFront                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def popFront01(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        assertEq(expected = (None: Option[Int32]), MutDeque.popFront(d))
    }

    @Test
    def popFront02(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushFront(1, d);
        assertEq(expected = Some(1), MutDeque.popFront(d))
    }

    @Test
    def popFront03(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(2, d);
        MutDeque.pushFront(3, d);
        assertEq(expected = Some(3), MutDeque.popFront(d));
        assertEq(expected = Some(2), MutDeque.popFront(d));
        assertEq(expected = Some(1), MutDeque.popFront(d));
        assertEq(expected = None, MutDeque.popFront(d))
    }

    @Test
    def popFront04(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        assertEq(expected = Some(1), MutDeque.popFront(d));
        assertEq(expected = Some(2), MutDeque.popFront(d));
        assertEq(expected = Some(3), MutDeque.popFront(d));
        assertEq(expected = None, MutDeque.popFront(d))
    }

    @Test
    def popFront05(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushFront(1, d);
        MutDeque.pushBack(2, d);
        assertEq(expected = Some(1), MutDeque.popFront(d));
        assertEq(expected = Some(2), MutDeque.popFront(d));
        assertEq(expected = None, MutDeque.popFront(d))
    }

    @Test
    def popFront06(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushFront(2, d);
        assertEq(expected = Some(2), MutDeque.popFront(d));
        assertEq(expected = Some(1), MutDeque.popFront(d));
        assertEq(expected = None, MutDeque.popFront(d))
    }


    /////////////////////////////////////////////////////////////////////////////
    // popBack                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def popBack01(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        assertEq(expected = (None: Option[Int32]), MutDeque.popBack(d))
    }

    @Test
    def popBack02(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        assertEq(expected = Some(1), MutDeque.popBack(d))
    }

    @Test
    def popBack03(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        assertEq(expected = Some(3), MutDeque.popBack(d));
        assertEq(expected = Some(2), MutDeque.popBack(d));
        assertEq(expected = Some(1), MutDeque.popBack(d));
        assertEq(expected = None, MutDeque.popBack(d))
    }

    @Test
    def popBack04(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(2, d);
        MutDeque.pushFront(3, d);
        assertEq(expected = Some(1), MutDeque.popBack(d));
        assertEq(expected = Some(2), MutDeque.popBack(d));
        assertEq(expected = Some(3), MutDeque.popBack(d));
        assertEq(expected = None, MutDeque.popBack(d))
    }

    @Test
    def popBack05(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushFront(2, d);
        assertEq(expected = Some(1), MutDeque.popBack(d));
        assertEq(expected = Some(2), MutDeque.popBack(d));
        assertEq(expected = None, MutDeque.popBack(d))
    }

    @Test
    def popBack06(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushFront(1, d);
        MutDeque.pushBack(2, d);
        assertEq(expected = Some(2), MutDeque.popBack(d));
        assertEq(expected = Some(1), MutDeque.popBack(d));
        assertEq(expected = None, MutDeque.popBack(d))
    }


    /////////////////////////////////////////////////////////////////////////////
    // pushFrontPushBack (expansion)                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def pushFrontPushBack01(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(2, d);
        // [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2]

        assertEq(expected = Some(2), MutDeque.popBack(d));
        assertEq(expected = Some(1), MutDeque.popFront(d));
        assertEq(expected = Some(2), MutDeque.popBack(d));
        assertEq(expected = Some(2), MutDeque.popBack(d));
        assertEq(expected = Some(2), MutDeque.popBack(d));
        assertEq(expected = Some(2), MutDeque.popBack(d));
        assertEq(expected = Some(2), MutDeque.popBack(d));
        assertEq(expected = Some(2), MutDeque.popBack(d));
        assertEq(expected = Some(2), MutDeque.popBack(d));
        assertEq(expected = Some(2), MutDeque.popBack(d));
        assertEq(expected = Some(2), MutDeque.popBack(d));
        assertEq(expected = Some(1), MutDeque.popBack(d));
        assertEq(expected = Some(1), MutDeque.popBack(d));
        assertEq(expected = Some(1), MutDeque.popBack(d));
        assertEq(expected = Some(1), MutDeque.popBack(d));
        assertEq(expected = Some(1), MutDeque.popBack(d));
        assertEq(expected = Some(1), MutDeque.popBack(d));
        assertEq(expected = Some(1), MutDeque.popBack(d));
        assertEq(expected = Some(1), MutDeque.popBack(d));
        assertEq(expected = Some(1), MutDeque.popBack(d));
        assertEq(expected = None, MutDeque.popBack(d));
        assertEq(expected = None, MutDeque.popFront(d))
    }


    /////////////////////////////////////////////////////////////////////////////
    // peekFront                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def peekFront01(): Unit \ Assert = region rc {
        let d = (MutDeque.empty(rc): MutDeque[Int32, rc]);
        assertEq(expected = None, MutDeque.peekFront(d))
    }

    @Test
    def peekFront02(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        List.forEach(x -> MutDeque.pushBack(x, d), 1 :: 2 :: 3 :: Nil);
        assertEq(expected = Some(1), MutDeque.peekFront(d))
    }

    @Test
    def peekFront03(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        List.forEach(x -> MutDeque.pushBack(x, d), 1 :: 2 :: 3 :: Nil);
        assertEq(expected = Some(1), MutDeque.peekFront(d));
        assertEq(expected = Some(1), MutDeque.peekFront(d));
        assertEq(expected = Some(1), MutDeque.peekFront(d))
    }

    @Test
    def peekFront04(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        List.forEach(x -> MutDeque.pushBack(x, d), 1 :: 2 :: 3 :: Nil);
        discard MutDeque.popFront(d);
        discard MutDeque.popFront(d);
        assertEq(expected = Some(3), MutDeque.peekFront(d))
    }

    @Test
    def peekFront05(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        List.forEach(x -> MutDeque.pushBack(x, d), 1 :: 2 :: 3 :: Nil);
        discard MutDeque.popFront(d);
        discard MutDeque.popBack(d);
        assertEq(expected = Some(2), MutDeque.peekFront(d))
    }

    @Test
    def peekFront06(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        List.forEach(x -> MutDeque.pushBack(x, d), 1 :: 2 :: 3 :: Nil);
        discard MutDeque.popFront(d);
        discard MutDeque.popFront(d);
        discard MutDeque.popFront(d);
        assertEq(expected = None, MutDeque.peekFront(d))
    }


    /////////////////////////////////////////////////////////////////////////////
    // peekBack                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def peekBack01(): Unit \ Assert = region rc {
        let d = (MutDeque.empty(rc): MutDeque[Int32, rc]);
        assertEq(expected = None, MutDeque.peekBack(d))
    }

    @Test
    def peekBack02(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        List.forEach(x -> MutDeque.pushBack(x, d), 1 :: 2 :: 3 :: Nil);
        assertEq(expected = Some(3), MutDeque.peekBack(d))
    }

    @Test
    def peekBack03(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        List.forEach(x -> MutDeque.pushBack(x, d), 1 :: 2 :: 3 :: Nil);
        assertEq(expected = Some(3), MutDeque.peekBack(d));
        assertEq(expected = Some(3), MutDeque.peekBack(d));
        assertEq(expected = Some(3), MutDeque.peekBack(d))
    }

    @Test
    def peekBack04(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        List.forEach(x -> MutDeque.pushBack(x, d), 1 :: 2 :: 3 :: Nil);
        discard MutDeque.popBack(d);
        discard MutDeque.popBack(d);
        assertEq(expected = Some(1), MutDeque.peekBack(d))
    }

    @Test
    def peekBack05(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        List.forEach(x -> MutDeque.pushBack(x, d), 1 :: 2 :: 3 :: Nil);
        discard MutDeque.popBack(d);
        discard MutDeque.popFront(d);
        assertEq(expected = Some(2), MutDeque.peekBack(d))
    }

    @Test
    def peekBack06(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        List.forEach(x -> MutDeque.pushBack(x, d), 1 :: 2 :: 3 :: Nil);
        discard MutDeque.popBack(d);
        discard MutDeque.popBack(d);
        discard MutDeque.popBack(d);
        assertEq(expected = None, MutDeque.peekBack(d))
    }


    /////////////////////////////////////////////////////////////////////////////
    // sameElements                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sameElements01(): Unit \ Assert = region rc {
        let d1 = (MutDeque.empty(rc): MutDeque[Int32, _]);
        let d2 = (MutDeque.empty(rc): MutDeque[Int32, _]);
        assertTrue(MutDeque.sameElements(d1, d2))
    }

    @Test
    def sameElements02(): Unit \ Assert = region rc {
        let d1 = MutDeque.empty(rc);
        MutDeque.pushBack(1, d1);

        let d2 = MutDeque.empty(rc);
        MutDeque.pushFront(1, d2);

        assertTrue(MutDeque.sameElements(d1, d2))
    }

    @Test
    def sameElements03(): Unit \ Assert = region rc {
        let d1 = MutDeque.empty(rc);
        MutDeque.pushBack(1, d1);
        MutDeque.pushBack(2, d1);

        let d2 = MutDeque.empty(rc);
        MutDeque.pushFront(1, d2);
        MutDeque.pushFront(2, d2);

        assertFalse(MutDeque.sameElements(d1, d2))
    }

    @Test
    def sameElements04(): Unit \ Assert = region rc {
        let d1 = MutDeque.empty(rc);
        MutDeque.pushBack("a", d1);
        MutDeque.pushBack("b", d1);
        MutDeque.pushBack("c", d1);

        let d2 = MutDeque.empty(rc);
        MutDeque.pushFront("b", d2);
        MutDeque.pushBack("c", d2);
        MutDeque.pushFront("a", d2);

        assertTrue(MutDeque.sameElements(d1, d2))
    }

    @Test
    def sameElements05(): Unit \ Assert = region rc {
        let d1 = MutDeque.empty(rc);
        MutDeque.pushBack("a", d1);
        MutDeque.pushBack("b", d1);
        MutDeque.pushBack("c", d1);

        let d2 = MutDeque.empty(rc);
        MutDeque.pushFront("a", d2);
        MutDeque.pushFront("b", d2);
        MutDeque.pushBack("c", d2);

        assertFalse(MutDeque.sameElements(d1, d2))
    }


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sum01(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        assertEq(expected = 0, MutDeque.sum(d))
    }

    @Test
    def sum02(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        assertEq(expected = 1, MutDeque.sum(d))
    }

    @Test
    def sum03(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        assertEq(expected = 6, MutDeque.sum(d))
    }

    @Test
    def sum04(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.pushBack(-3, d);
        assertEq(expected = 3, MutDeque.sum(d))
    }

    @Test
    def sum05(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.pushFront(-3, d);
        assertEq(expected = 3, MutDeque.sum(d))
    }

    @Test
    def sum06(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(-1, d);
        MutDeque.pushBack(-2, d);
        MutDeque.pushFront(-3, d);
        MutDeque.pushFront(-4, d);
        assertEq(expected = -10, MutDeque.sum(d))
    }

    @Test
    def sum07(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(10, d);
        MutDeque.pushBack(-10, d);
        assertEq(expected = 0, MutDeque.sum(d))
    }


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumWith01(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        assertEq(expected = 0, MutDeque.sumWith(x -> x + 1, d))
    }

    @Test
    def sumWith02(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        assertEq(expected = 2, MutDeque.sumWith(x -> x + 1, d))
    }

    @Test
    def sumWith03(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        assertEq(expected = 9, MutDeque.sumWith(x -> x + 1, d))
    }

    @Test
    def sumWith04(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.pushBack(-3, d);
        assertEq(expected = 7, MutDeque.sumWith(x -> x + 1, d))
    }

    @Test
    def sumWith05(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.pushFront(-3, d);
        assertEq(expected = 7, MutDeque.sumWith(x -> x + 1, d))
    }

    @Test
    def sumWith06(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(-1, d);
        MutDeque.pushBack(-2, d);
        MutDeque.pushFront(-3, d);
        MutDeque.pushFront(-4, d);
        assertEq(expected = -6, MutDeque.sumWith(x -> x + 1, d))
    }

    @Test
    def sumWith07(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(10, d);
        MutDeque.pushBack(-10, d);
        assertEq(expected = 2, MutDeque.sumWith(x -> x + 1, d))
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeft01(): Unit \ Assert = region rc {
        assertEq(expected = 100, MutDeque.foldLeft((i, e) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, MutDeque.empty(rc)))
    }

    @Test
    def foldLeft02(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        assertEq(expected = 198, MutDeque.foldLeft((i, e) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, d))
    }

    @Test
    def foldLeft03(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushFront(1, d);
        assertEq(expected = 198, MutDeque.foldLeft((i, e) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, d))
    }

    @Test
    def foldLeft04(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushFront(1, d);
        MutDeque.pushBack(2, d);
        assertEq(expected = 196, MutDeque.foldLeft((i, e) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, d))
    }

    @Test
    def foldLeft05(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.pushFront(1, d);
        assertEq(expected = 386, MutDeque.foldLeft((i, e) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, d))
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRight01(): Unit \ Assert = region rc {
        assertEq(expected = 100, MutDeque.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, MutDeque.empty(rc)))
    }

    @Test
    def foldRight02(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        assertEq(expected = 198, MutDeque.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, d))
    }

    @Test
    def foldRight03(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushFront(1, d);
        assertEq(expected = 198, MutDeque.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, d))
    }

    @Test
    def foldRight04(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushFront(1, d);
        MutDeque.pushBack(2, d);
        assertEq(expected = 194, MutDeque.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, d))
    }

    @Test
    def foldRight05(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.pushFront(1, d);
        assertEq(expected = 382, MutDeque.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, d))
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRightWithCont01(): Unit \ Assert = region rc {
        assertEq(expected = 100, MutDeque.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, MutDeque.empty(rc)))
    }

    @Test
    def foldRightWithCont02(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        assertEq(expected = 198, MutDeque.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, d))
    }

    @Test
    def foldRightWithCont03(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushFront(1, d);
        assertEq(expected = 198, MutDeque.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, d))
    }

    @Test
    def foldRightWithCont04(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushFront(1, d);
        MutDeque.pushBack(2, d);
        assertEq(expected = 194, MutDeque.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, d))
    }

    @Test
    def foldRightWithCont05(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.pushFront(1, d);
        assertEq(expected = 382, MutDeque.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, d))
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldMap01(): Unit \ Assert = region rc {
        assertEq(expected = 0, MutDeque.foldMap(x -> 2 * x, MutDeque.empty(rc)))
    }

    @Test
    def foldMap02(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        assertEq(expected = 2, MutDeque.foldMap(x -> 2 * x, d))
    }

    @Test
    def foldMap03(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushFront(1, d);
        MutDeque.pushBack(2, d);
        assertEq(expected = 6, MutDeque.foldMap(x -> 2 * x, d))
    }

    @Test
    def foldMap04(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.pushFront(1, d);
        assertEq(expected = 12, MutDeque.foldMap(x -> 2 * x, d))
    }

    @Test
    def foldMap05(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushFront("c", d);
        MutDeque.pushFront("b", d);
        MutDeque.pushFront("a", d);
        assertEq(expected = "abb", MutDeque.foldMap(x -> if (x == "c") "b" else x, d))
    }

    @Test
    def foldMap06(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        assertEq(expected = "123", MutDeque.foldMap(Int32.toString, d))
    }


    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toList01(): Unit \ Assert = region rc {
        assertEq(expected = (Nil: List[Int32]), MutDeque.toList((MutDeque.empty(rc): MutDeque[Int32, _])))
    }

    @Test
    def toList02(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        assertEq(expected = 1 :: Nil, MutDeque.toList(d))
    }

    @Test
    def toList03(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        assertEq(expected = 1 :: 2 :: Nil, MutDeque.toList(d))
    }

    @Test
    def toList04(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        assertEq(expected = 1 :: 2 :: 3 :: Nil, MutDeque.toList(d))
    }


    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toArray01(): Unit \ Assert = region rc {
        let d = (MutDeque.empty(rc): MutDeque[Int32, _]);

        let a = MutDeque.toArray(rc, d);
        assertTrue(Array.sameElements(a, Array#{} @ rc))
    }

    @Test
    def toArray02(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);

        let a = MutDeque.toArray(rc, d);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def toArray03(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);

        let a = MutDeque.toArray(rc, d);
        assertTrue(Array.sameElements(a, Array#{1, 2} @ rc))
    }

    @Test
    def toArray04(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushFront(3, d);
        MutDeque.pushFront(4, d);

        let a = MutDeque.toArray(rc, d);
        assertTrue(Array.sameElements(a, Array#{4, 3, 1, 2} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toVector01(): Unit \ Assert = region rc {
        let d = (MutDeque.empty(rc): MutDeque[Int32, _]);

        let v = MutDeque.toVector(d);
        assertEq(expected = Vector#{}, v)
    }

    @Test
    def toVector02(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);

        let v = MutDeque.toVector(d);
        assertEq(expected = Vector#{1}, v)
    }

    @Test
    def toVector03(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);

        let v = MutDeque.toVector(d);
        assertEq(expected = Vector#{1, 2}, v)
    }

    @Test
    def toVector04(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushFront(3, d);
        MutDeque.pushFront(4, d);

        let v = MutDeque.toVector(d);
        assertEq(expected = Vector#{4, 3, 1, 2}, v)
    }


    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def join01(): Unit \ Assert = region rc {
        assertEq(expected = "", (MutDeque.empty(rc): MutDeque[Int32, _]) |> MutDeque.join(","))
    }

    @Test
    def join02(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        assertEq(expected = "1", MutDeque.join(",", d))
    }

    @Test
    def join03(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        assertEq(expected = "1,2,3", MutDeque.join(",", d))
    }

    @Test
    def join04(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack("1", d);
        MutDeque.pushBack("2", d);
        MutDeque.pushBack("3", d);
        assertEq(expected = "1,2,3", MutDeque.join(",", d))
    }


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinWith01(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        assertEq(expected = "", MutDeque.joinWith(x -> "${x + 1}", ",", d))
    }

    @Test
    def joinWith02(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        assertEq(expected = "2", MutDeque.joinWith(x -> "${x + 1}", ",", d))
    }

    @Test
    def joinWith03(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        assertEq(expected = "2,3,4", MutDeque.joinWith(x -> "${x + 1}", ",", d))
    }

    @Test
    def joinWith04(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack("1", d);
        MutDeque.pushBack("2", d);
        MutDeque.pushBack("3", d);
        assertEq(expected = "11,22,33", MutDeque.joinWith(x -> x + x, ",", d))
    }


    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterator01(): Unit \ Assert = region rc {
        let d = (MutDeque.empty(rc): MutDeque[Int32, rc]);
        let iter = MutDeque.iterator(rc, d);
        match Iterator.next(iter) {
            case None => ()
            case _    => fail("Unexpected case")
        }
    }

    @Test
    def iterator02(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        List.forEach(x -> MutDeque.pushBack(x, d), 1 :: 2 :: 3 :: Nil);
        let iter = MutDeque.iterator(rc, d);
        match Iterator.next(iter) {
            case Some(_) => ()
            case _       => fail("Unexpected case")
        }
    }

    @Test
    def iterator03(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        List.forEach(x -> MutDeque.pushBack(x, d), 1 :: 2 :: 3 :: Nil);
        discard MutDeque.popBack(d);
        discard MutDeque.popBack(d);
        discard MutDeque.popBack(d);
        let iter = MutDeque.iterator(rc, d);
        match Iterator.next(iter) {
            case None => ()
            case _    => fail("Unexpected case")
        }
    }

    @Test
    def iterator04(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        List.forEach(x -> MutDeque.pushBack(x, d), 1 :: 2 :: 3 :: Nil);
        assertEq(expected = 1 :: 2 :: 3 :: Nil, MutDeque.iterator(rc, d) |> Iterator.toList)
    }

    @Test
    def iterator05(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        List.forEach(x -> MutDeque.pushBack(x, d), List.range(0, 100));
        assertEq(expected = List.range(0, 100), MutDeque.iterator(rc, d) |> Iterator.toList);
        assertEq(expected = List.range(0, 100), MutDeque.toList(d))
    }

    @Test
    def iterator06(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        List.forEach(x -> MutDeque.pushBack(x, d), List.range(0, 100));
        let iter = MutDeque.iterator(rc, d);
        let temp = MutDeque.empty(rc);
        let ml = MutList.range(rc, 0, 50);
        MutList.forEach(x -> MutDeque.pushBack(x, temp), ml);
        MutDeque.forEach(_ -> discard Iterator.next(iter), temp);
        assertEq(expected = List.range(50, 100), iter |> Iterator.toList);
        assertEq(expected = List.range(0, 100), MutDeque.toList(d))
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        let l = Ref.fresh(rc, Nil);
        let fn = x -> if (x > 0) Ref.put("T" :: Ref.get(l), l) else Ref.put("F" :: Ref.get(l), l);
        MutDeque.forEach(fn, d);
        assertEq(expected = "", List.reverse(Ref.get(l)) |> List.join(""))
    }

    @Test
    def forEach02(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(0, d);
        let l = Ref.fresh(rc, Nil);
        let fn = x -> if (x > 0) Ref.put("T" :: Ref.get(l), l) else Ref.put("F" :: Ref.get(l), l);
        MutDeque.forEach(fn, d);
        assertEq(expected = "F", List.reverse(Ref.get(l)) |> List.join(""))
    }

    @Test
    def forEach03(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        let l = Ref.fresh(rc, Nil);
        let fn = x -> if (x > 0) Ref.put("T" :: Ref.get(l), l) else Ref.put("F" :: Ref.get(l), l);
        MutDeque.forEach(fn, d);
        assertEq(expected = "T", List.reverse(Ref.get(l)) |> List.join(""))
    }

    @Test
    def forEach04(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(0, d);
        MutDeque.pushBack(1, d);
        let l = Ref.fresh(rc, Nil);
        let fn = x -> if (x > 0) Ref.put("T" :: Ref.get(l), l) else Ref.put("F" :: Ref.get(l), l);
        MutDeque.forEach(fn, d);
        assertEq(expected = "FT", List.reverse(Ref.get(l)) |> List.join(""))
    }

    @Test
    def forEach05(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushFront(0, d);
        let l = Ref.fresh(rc, Nil);
        let fn = x -> if (x > 0) Ref.put("T" :: Ref.get(l), l) else Ref.put("F" :: Ref.get(l), l);
        MutDeque.forEach(fn, d);
        assertEq(expected = "FT", List.reverse(Ref.get(l)) |> List.join(""))
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEachWithIndex01(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        let ri = Ref.fresh(rc, 21);
        MutDeque.forEachWithIndex((i, _) -> Ref.put(i, ri), d);
        assertEq(expected = 21, Ref.get(ri))
    }

    @Test
    def forEachWithIndex02(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        let ri = Ref.fresh(rc, 21);
        MutDeque.pushBack(0, d);
        MutDeque.forEachWithIndex((i, _) -> Ref.put(i, ri), d);
        assertEq(expected = 0, Ref.get(ri))
    }

    @Test
    def forEachWithIndex03(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        let ri = Ref.fresh(rc, 21);
        MutDeque.pushBack(0, d);
        MutDeque.pushBack(1, d);
        MutDeque.forEachWithIndex((i, _) -> Ref.put(i, ri), d);
        assertEq(expected = 1, Ref.get(ri))
    }

    /////////////////////////////////////////////////////////////////////////////
    // shuffle                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def shuffle01(): Unit \ {Assert, NonDet} = region rc {
        run {
            let l: List[Int32] = Nil;
            let d1 = MutDeque.empty(rc);
            List.forEach(x -> MutDeque.pushBack(x, d1), l);
            let d2 = MutDeque.shuffle(rc, d1);

            assertEq(expected = 0, MutDeque.size(d2));
            assertEq(expected = Set#{}, d2 |> MutDeque.toList |> List.toSet)
        } with Shuffle.runWithIO
    }

    @Test
    def shuffle02(): Unit \ {Assert, NonDet} = region rc {
        run {
            let l = 1 :: Nil;
            let d1 = MutDeque.empty(rc);
            List.forEach(x -> MutDeque.pushBack(x, d1), l);
            let d2 = MutDeque.shuffle(rc, d1);

            assertEq(expected = 1, MutDeque.size(d2));
            assertEq(expected = Set#{1}, d2 |> MutDeque.toList |> List.toSet)
        } with Shuffle.runWithIO
    }

    @Test
    def shuffle03(): Unit \ {Assert, NonDet} = region rc {
        run {
            let l = 1 :: 2 :: 3 :: Nil;
            let d1 = MutDeque.empty(rc);
            List.forEach(x -> MutDeque.pushBack(x, d1), l);
            let d2 = MutDeque.shuffle(rc, d1);

            assertEq(expected = 3, MutDeque.size(d2));
            assertEq(expected = Set#{1, 2, 3}, d2 |> MutDeque.toList |> List.toSet)
        } with Shuffle.runWithIO
    }

    @Test
    def shuffle04(): Unit \ {Assert, NonDet} = region rc {
        run {
            let l = 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: Nil;
            let d1 = MutDeque.empty(rc);
            List.forEach(x -> MutDeque.pushBack(x, d1), l);
            let d2 = MutDeque.shuffle(rc, d1);

            assertEq(expected = 10, MutDeque.size(d2));
            assertEq(expected = Set#{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}, d2 |> MutDeque.toList |> List.toSet)
        } with Shuffle.runWithIO
    }

}
