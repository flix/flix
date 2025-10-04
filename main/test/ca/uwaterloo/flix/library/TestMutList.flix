
mod TestMutList {
    use Assert.{assertEq, assertTrue, assertFalse, assertSome, fail};
    use ToString.toString
    use MutList.sameElements

    /////////////////////////////////////////////////////////////////////////////
    // emptyWithCapacity                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def emptyWithCapacity01(): Unit \ Assert = region rc {
        let l: MutList[Int32, rc] = MutList.emptyWithCapacity(rc, 10);
        assertEq(expected = "MutList#{}", MutList.toString(l))
    }

    @Test
    def emptyWithCapacity02(): Unit \ Assert = region rc {
        let l: MutList[Int32, rc] = MutList.emptyWithCapacity(rc, -1);
        assertEq(expected = "MutList#{}", MutList.toString(l))
    }

    @Test
    def emptyWithCapacity03(): Unit \ Assert = region rc {
        let l: MutList[Int32, rc] = MutList.emptyWithCapacity(rc, 0);
        MutList.push(5, l);
        MutList.push(4, l);
        assertEq(expected = "MutList#{5, 4}", MutList.toString(l))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toString01(): Unit \ Assert = region rc {
        let l: MutList[Int32, rc] = MutList.empty(rc);
        assertEq(expected = "MutList#{}", MutList.toString(l))
    }

    @Test
    def toString02(): Unit \ Assert = region rc {
        let l: MutList[Int32, rc] = MutList.empty(rc);
        MutList.push(1, l);
        assertEq(expected = "MutList#{1}", MutList.toString(l))
    }

    @Test
    def toString03(): Unit \ Assert = region rc {
        let l: MutList[Int32, rc] = MutList.empty(rc);
        MutList.push(1, l);
        MutList.push(2, l);
        MutList.push(3, l);
        MutList.push(4, l);
        MutList.push(5, l);
        assertEq(expected = "MutList#{1, 2, 3, 4, 5}", MutList.toString(l))
    }

    /////////////////////////////////////////////////////////////////////////////
    // sameElements                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sameElements01(): Unit \ Assert = region rc {
        let u: MutList[Int32, _] = MutList.empty(rc);
        let v: MutList[Int32, _] = MutList.empty(rc);
        assertTrue(u `sameElements` v)
    }

    @Test
    def sameElements02(): Unit \ Assert = region rc {
        let u = MutList.empty(rc);
        MutList.push(0, u);
        let v = MutList.empty(rc);
        assertFalse(u `sameElements` v)
    }

    @Test
    def sameElements03(): Unit \ Assert = region rc {
        let u = MutList.empty(rc);
        MutList.push(2, u);
        MutList.push(3, u);
        let v = MutList.empty(rc);
        MutList.push(2, v);
        MutList.push(3, v);
        assertTrue(u `sameElements` v)
    }

    @Test
    def sameElements04(): Unit \ Assert = region rc {
        let u = MutList.empty(rc);
        MutList.push(2, u);
        MutList.push(3, u);
        let v = MutList.empty(rc);
        MutList.push(2, v);
        MutList.push(2, v);
        assertFalse(u `sameElements` v)
    }

    @Test
    def sameElements05(): Unit \ Assert = region rc {
        let u = MutList.empty(rc);
        MutList.push(1, u);
        let v = MutList.empty(rc);
        assertFalse(u `sameElements` v)
    }

    /////////////////////////////////////////////////////////////////////////////
    // get                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def get01(): Unit \ Assert = region rc {
        let r = run MutList.get(0, MutList.empty(rc)) with handler OutOfBounds {
            def outOfBounds(msg, _) = msg
        };
        assertEq(expected = "index 0 is out of bounds for MutList of length 0", r)
    }

    @Test
    def get02(): Unit \ Assert = region rc {
        let l = MutList.empty(rc);
        MutList.push("item", l);
        let r = run MutList.get(0, l) with handler OutOfBounds {
            def outOfBounds(_, _) = "error"
        };
        assertEq(expected = "item", r)
    }

    @Test
    def get03(): Unit \ Assert = region rc {
        let l = MutList.empty(rc);
        MutList.push("item", l);
        let r = run MutList.get(1, l) with handler OutOfBounds {
            def outOfBounds(msg, _) = msg
        };
        assertEq(expected = "index 1 is out of bounds for MutList of length 1", r)
    }

    @Test
    def get04(): Unit \ Assert = region rc {
        let l = MutList.map(rc, i -> "item ${i}", MutList.range(rc, 0, 2));

        let r = run MutList.get(1, l) with handler OutOfBounds {
            def outOfBounds(msg, _) = msg
        };
        assertEq(expected = "item 1", r)
    }

    @Test
    def get05(): Unit \ Assert = region rc {
        let l = MutList.map(rc, i -> "item ${i}", MutList.range(rc, 0, 4));

        let r = run MutList.get(3, l) with handler OutOfBounds {
            def outOfBounds(msg, _) = msg
        };
        assertEq(expected = "item 3", r)
    }

    @Test
    def get06(): Unit \ Assert = region rc {
        let l = MutList.map(rc, i -> "item ${i}", MutList.range(rc, 0, 42));

        let r = run MutList.get(30, l) with handler OutOfBounds {
            def outOfBounds(msg, _) = msg
        };
        assertEq(expected = "item 30", r)
    }

    @Test
    def get07(): Unit \ Assert = region rc {
        let l = MutList.map(rc, i -> "item ${i}", MutList.range(rc, 0, 42));

        let r = run MutList.get(-30, l) with handler OutOfBounds {
            def outOfBounds(msg, _) = msg
        };
        assertEq(expected = "index -30 is out of bounds for MutList of length 42", r)
    }

    @Test
    def get08(): Unit \ Assert = region rc {
        let l = MutList.map(rc, i -> "item ${i}", MutList.range(rc, 0, 42));

        let r = run MutList.get(50, l) with handler OutOfBounds {
            def outOfBounds(msg, _) = msg
        };
        assertEq(expected = "index 50 is out of bounds for MutList of length 42", r)
    }


    /////////////////////////////////////////////////////////////////////////////
    // put                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def put01(): Unit \ Assert = region rc {
        let error = Ref.fresh(rc, None);
        let l = MutList.empty(rc);

        run MutList.put("new item", 0, l) with handler OutOfBounds {
            def outOfBounds(msg, _) = Ref.put(Some(msg), error)
        };
        assertEq(expected = Some("index 0 is out of bounds for MutList of length 0"), Ref.get(error))
    }

    @Test
    def put02(): Unit \ Assert = region rc {
        let error = Ref.fresh(rc, None);
        let l = MutList.empty(rc);
        MutList.push("item", l);

        run MutList.put("new item", 0, l) with handler OutOfBounds {
            def outOfBounds(msg, _) = Ref.put(Some(msg), error)
        };
        assertEq(expected = None, Ref.get(error));
        assertEq(expected = Some("new item"), MutList.nth(0, l))
    }

    @Test
    def put03(): Unit \ Assert = region rc {
        let error = Ref.fresh(rc, None);
        let l = MutList.empty(rc);
        MutList.push("item", l);

        run MutList.put("new item", 1, l) with handler OutOfBounds {
            def outOfBounds(msg, _) = Ref.put(Some(msg), error)
        };
        assertEq(expected = Some("index 1 is out of bounds for MutList of length 1"), Ref.get(error))
    }

    @Test
    def put04(): Unit \ Assert = region rc {
        let error = Ref.fresh(rc, None);
        let l = MutList.range(rc, 0, 2);

        run MutList.put(1234, 1, l) with handler OutOfBounds {
            def outOfBounds(msg, _) = Ref.put(Some(msg), error)
        };
        assertEq(expected = None, Ref.get(error));
        assertEq(expected = Some(1234), MutList.nth(1, l))
    }

    @Test
    def put05(): Unit \ Assert = region rc {
        let error = Ref.fresh(rc, None);
        let l = MutList.range(rc, 0, 4);

        run MutList.put(1234, 3, l) with handler OutOfBounds {
            def outOfBounds(msg, _) = Ref.put(Some(msg), error)
        };
        assertEq(expected = None, Ref.get(error));
        assertEq(expected = Some(1234), MutList.nth(3, l))
    }

    @Test
    def put06(): Unit \ Assert = region rc {
        let error = Ref.fresh(rc, None);
        let l = MutList.range(rc, 0, 42);

        run MutList.put(1234, 30, l) with handler OutOfBounds {
            def outOfBounds(msg, _) = Ref.put(Some(msg), error)
        };
        assertEq(expected = None, Ref.get(error));
        assertEq(expected = Some(1234), MutList.nth(30, l))
    }

    @Test
    def put07(): Unit \ Assert = region rc {
        let error = Ref.fresh(rc, None);
        let l = MutList.range(rc, 0, 42);

        run MutList.put(1234, -30, l) with handler OutOfBounds {
            def outOfBounds(msg, _) = Ref.put(Some(msg), error)
        };
        assertEq(expected = Some("index -30 is out of bounds for MutList of length 42"), Ref.get(error))
    }

    @Test
    def put08(): Unit \ Assert = region rc {
        let error = Ref.fresh(rc, None);
        let l = MutList.range(rc, 0, 42);

        run MutList.put(1234, 50, l) with handler OutOfBounds {
            def outOfBounds(msg, _) = Ref.put(Some(msg), error)
        };
        assertEq(expected = Some("index 50 is out of bounds for MutList of length 42"), Ref.get(error))
    }


    /////////////////////////////////////////////////////////////////////////////
    // nth                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def nth01(): Unit \ Assert = region rc {
        assertEq(expected = None, MutList.nth(0, (MutList.empty(rc): MutList[Int32, _])))
    }

    @Test
    def nth02(): Unit \ Assert = region rc {
        let v = MutList.empty(rc);
        MutList.push(42, v);
        assertEq(expected = Some(42), MutList.nth(0, v))
    }

    @Test
    def nth03(): Unit \ Assert = region rc {
        let v = MutList.empty(rc);
        MutList.push(42, v);
        assertEq(expected = None, MutList.nth(1, v))
    }

    @Test
    def nth04(): Unit \ Assert = region rc {
        assertEq(expected = Some(1), MutList.nth(1, MutList.range(rc, 0, 2)))
    }

    @Test
    def nth05(): Unit \ Assert = region rc {
        assertEq(expected = Some(3), MutList.nth(3, MutList.range(rc, 0, 4)))
    }

    @Test
    def nth06(): Unit \ Assert = region rc {
        assertEq(expected = Some(30), MutList.nth(30, MutList.range(rc, 0, 42)))
    }

    @Test
    def nth07(): Unit \ Assert = region rc {
        assertEq(expected = None, MutList.nth(-30, MutList.range(rc, 0, 42)))
    }

    @Test
    def nth08(): Unit \ Assert = region rc {
        assertEq(expected = None, MutList.nth(50, MutList.range(rc, 0, 42)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def length01(): Unit \ Assert = region rc {
        assertEq(expected = 0, MutList.length(MutList.empty(rc)))
    }

    @Test
    def length02(): Unit \ Assert = region rc {
        let v = MutList.empty(rc);
        MutList.push(42, v);
        assertEq(expected = 1, MutList.length(v))
    }

    @Test
    def length03(): Unit \ Assert = region rc {
        assertEq(expected = 2, MutList.length(MutList.range(rc, 0, 2)))
    }

    @Test
    def length04(): Unit \ Assert = region rc {
        assertEq(expected = 4, MutList.length(MutList.range(rc, 0, 4)))
    }

    @Test
    def length05(): Unit \ Assert = region rc {
        assertEq(expected = 42, MutList.length(MutList.range(rc, 0, 42)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def size01(): Unit \ Assert = region rc {
        assertEq(expected = 0, MutList.size(MutList.empty(rc)))
    }

    @Test
    def size02(): Unit \ Assert = region rc {
        let v = MutList.empty(rc);
        MutList.push(42, v);
        assertEq(expected = 1, MutList.size(v))
    }

    @Test
    def size03(): Unit \ Assert = region rc {
        assertEq(expected = 2, MutList.size(MutList.range(rc, 0, 2)))
    }

    @Test
    def size04(): Unit \ Assert = region rc {
        assertEq(expected = 4, MutList.size(MutList.range(rc, 0, 4)))
    }

    @Test
    def size05(): Unit \ Assert = region rc {
        assertEq(expected = 42, MutList.size(MutList.range(rc, 0, 42)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isEmpty01(): Unit \ Assert = region rc {
        assertTrue(MutList.isEmpty(MutList.empty(rc)))
    }

    @Test
    def isEmpty02(): Unit \ Assert = region rc {
        assertTrue(MutList.isEmpty(MutList.range(rc, 0, 0)))
    }

    @Test
    def isEmpty03(): Unit \ Assert = region rc {
        assertFalse(MutList.isEmpty(MutList.range(rc, 0, 1)))
    }

    @Test
    def isEmpty04(): Unit \ Assert = region rc {
        assertFalse(MutList.isEmpty(MutList.range(rc, 0, 2)))
    }

    @Test
    def isEmpty05(): Unit \ Assert = region rc {
        assertFalse(MutList.isEmpty(MutList.range(rc, 3, 4)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // nonEmpty                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def nonEmpty01(): Unit \ Assert = region rc {
        assertFalse(MutList.nonEmpty(MutList.empty(rc)))
    }

    @Test
    def nonEmpty02(): Unit \ Assert = region rc {
        assertFalse(MutList.nonEmpty(MutList.range(rc, 0, 0)))
    }

    @Test
    def nonEmpty03(): Unit \ Assert = region rc {
        assertTrue(MutList.nonEmpty(MutList.range(rc, 0, 1)))
    }

    @Test
    def nonEmpty04(): Unit \ Assert = region rc {
        assertTrue(MutList.nonEmpty(MutList.range(rc, 0, 2)))
    }

    @Test
    def nonEmpty05(): Unit \ Assert = region rc {
        assertTrue(MutList.nonEmpty(MutList.range(rc, 3, 4)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def memberOf01(): Unit \ Assert = region rc {
        assertFalse(MutList.memberOf(42, MutList.empty(rc)))
    }

    @Test
    def memberOf02(): Unit \ Assert = region rc {
        let v = MutList.empty(rc);
        MutList.push(2, v);
        assertTrue(MutList.memberOf(2, v))
    }

    @Test
    def memberOf03(): Unit \ Assert = region rc {
        let v = MutList.empty(rc);
        MutList.push(42, v);
        assertFalse(MutList.memberOf(2, v))
    }

    @Test
    def memberOf04(): Unit \ Assert = region rc {
        assertFalse(MutList.memberOf(4, MutList.range(rc, 0, 4)))
    }

    @Test
    def memberOf05(): Unit \ Assert = region rc {
        assertTrue(MutList.memberOf(0, MutList.range(rc, 0, 3)))
    }

    @Test
    def memberOf06(): Unit \ Assert = region rc {
        assertTrue(MutList.memberOf(2, MutList.range(rc, 0, 3)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def count01(): Unit \ Assert = region rc {
        assertEq(expected = 1, MutList.count(x -> x > 2, MutList.range(rc, 0, 4)))
    }

    @Test
    def count02(): Unit \ Assert = region rc {
        assertEq(expected = 2, MutList.count(x -> x >= 2, MutList.range(rc, 0, 4)))
    }

    @Test
    def count03(): Unit \ Assert = region rc {
        assertEq(expected = 5, MutList.count(x -> x `Int32.remainder` 2 == 0, MutList.range(rc, 0, 10)))
    }

    @Test
    def count04(): Unit \ Assert = region rc {
        assertEq(expected = 1, MutList.count(x -> x == 11, MutList.range(rc, 0, 21)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def exists01(): Unit \ Assert = region rc {
        assertFalse(MutList.exists(x -> x > 9, MutList.range(rc, 0, 9)))
    }

    @Test
    def exists02(): Unit \ Assert = region rc {
        assertFalse(MutList.exists(x -> x < 0, MutList.range(rc, 0, 9)))
    }

    @Test
    def exists03(): Unit \ Assert = region rc {
        assertTrue(MutList.exists(x -> x == 8, MutList.range(rc, 0, 9)))
    }

    @Test
    def exists04(): Unit \ Assert = region rc {
        assertTrue(MutList.exists(x -> x > 0, MutList.range(rc, 0, 9)))
    }

    @Test
    def exists05(): Unit \ Assert = region rc {
        assertFalse(MutList.exists(x -> x == 0, MutList.empty(rc)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forAll01(): Unit \ Assert = region rc {
        assertFalse(MutList.forAll(x -> x > 0, MutList.range(rc, 0, 9)))
    }

    @Test
    def forAll02(): Unit \ Assert = region rc {
        assertTrue(MutList.forAll(x -> x >= 0, MutList.range(rc, 0, 9)))
    }

    @Test
    def forAll03(): Unit \ Assert = region rc {
        assertTrue(MutList.forAll(x -> x >= 0 and x < 9, MutList.range(rc, 0, 9)))
    }

    @Test
    def forAll05(): Unit \ Assert = region rc {
        assertTrue(MutList.forAll(x -> x > 0, MutList.empty(rc)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def head01(): Unit \ Assert = region rc {
        let v = MutList.empty(rc);
        MutList.push(42, v);
        assertEq(expected = Some(42), MutList.head(v))
    }

    @Test
    def head02(): Unit \ Assert = region rc {
        assertEq(expected = None, MutList.head((MutList.empty(rc): MutList[Unit, _])))
    }

    @Test
    def head03(): Unit \ Assert = region rc {
        let v = MutList.empty(rc);
        MutList.push(101, v);
        MutList.push(102, v);
        assertEq(expected = Some(101), MutList.head(v))
    }


    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def last01(): Unit \ Assert = region rc {
        assertEq(expected = None, MutList.last((MutList.empty(rc): MutList[Unit, _])))
    }

    @Test
    def last02(): Unit \ Assert = region rc {
        assertEq(expected = Some(7), MutList.last(MutList.range(rc, 0, 8)))
    }

    @Test
    def last03(): Unit \ Assert = region rc {
        let v = MutList.empty(rc);
        MutList.push('A', v);
        MutList.push('B', v);
        assertEq(expected = Some('B'), MutList.last(v))
    }


    /////////////////////////////////////////////////////////////////////////////
    // indexOfLeft                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indexOfLeft01(): Unit \ Assert = region rc {
        assertEq(expected = Some(0), MutList.indexOfLeft(2, MutList.range(rc, 2, 3)))
    }

    @Test
    def indexOfLeft02(): Unit \ Assert = region rc {
        assertEq(expected = None, MutList.indexOfLeft(10, MutList.range(rc, 0, 10)))
    }

    @Test
    def indexOfLeft03(): Unit \ Assert = region rc {
        let v = MutList.empty(rc);
        MutList.push('A', v);
        MutList.push('B', v);
        MutList.push('C', v);
        assertEq(expected = Some(1), MutList.indexOfLeft('B', v))
    }

    @Test
    def indexOfLeft04(): Unit \ Assert = region rc {
        assertEq(expected = Some(99), MutList.indexOfLeft(99, MutList.range(rc, 0, 100)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // indexOfRight                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indexOfRight01(): Unit \ Assert = region rc {
        assertEq(expected = Some(0), MutList.indexOfRight(2, MutList.range(rc, 2, 3)))
    }

    @Test
    def indexOfRight02(): Unit \ Assert = region rc {
        assertEq(expected = None, MutList.indexOfRight(10, MutList.range(rc, 0, 10)))
    }

    @Test
    def indexOfRight03(): Unit \ Assert = region rc {
        let v = MutList.empty(rc);
        MutList.push('A', v);
        MutList.push('B', v);
        MutList.push('A', v);
        assertEq(expected = Some(2), MutList.indexOfRight('A', v))
    }

    @Test
    def indexOfRight04(): Unit \ Assert = region rc {
        let v1 = MutList.range(rc, 0, 4);
        let v2 = MutList.range(rc, 0, 4);
        MutList.append(MutList.toList(v2), v1);
        assertEq(expected = Some(6), MutList.indexOfRight(2, v1))
    }

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findLeft01(): Unit \ Assert = region rc {
        assertEq(expected = Some(4), MutList.findLeft(x -> x > 3, MutList.range(rc, 3, 11)))
    }

    @Test
    def findLeft02(): Unit \ Assert = region rc {
        assertEq(expected = None, MutList.findLeft(x -> x > 10, MutList.range(rc, 3, 11)))
    }

    @Test
    def findLeft03(): Unit \ Assert = region rc {
        assertEq(expected = None, MutList.findLeft(x -> x < 3, MutList.range(rc, 3, 11)))
    }

    @Test
    def findLeft04(): Unit \ Assert = region rc {
        assertEq(expected = Some(3), MutList.findLeft(x -> x `Int32.remainder` 2 == 1, MutList.range(rc, 3, 11)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findRight01(): Unit \ Assert = region rc {
        assertEq(expected = Some(4), MutList.findRight(x -> x > 0, MutList.range(rc, 2, 5)))
    }

    @Test
    def findRight02(): Unit \ Assert = region rc {
        assertEq(expected = None, MutList.findRight(x -> x == 10, MutList.range(rc, 9, 10)))
    }

    @Test
    def findRight03(): Unit \ Assert = region rc {
        assertEq(expected = None, MutList.findRight(x -> x `Int32.remainder` 2 == 0, MutList.range(rc, 3, 4)))
    }

    @Test
    def findRight04(): Unit \ Assert = region rc {
        assertEq(expected = Some(4), MutList.findRight(x -> x < 5, MutList.range(rc, 3, 11)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // scanLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def scanLeft01(): Unit \ Assert = region rc {
        let v = MutList.scanLeft(rc, (x, y) -> x + y, 0, MutList.empty(rc));
        assertEq(expected = 1, MutList.length(v));
        assertEq(expected = Some(0), MutList.head(v))
    }

    @Test
    def scanLeft02(): Unit \ Assert = region rc {
        let v1 = MutList.range(rc, 1, 6);
        let v2 = MutList.empty(rc);
        MutList.push(0, v2);
        MutList.push(1, v2);
        MutList.push(3, v2);
        MutList.push(6, v2);
        MutList.push(10, v2);
        MutList.push(15, v2);
        let v3 = MutList.scanLeft(rc, (x, y) -> x + y, 0, v1);
        assertTrue(MutList.sameElements(v2, v3))
    }

    @Test
    def scanLeft03(): Unit \ Assert = region rc {
        let v = MutList.scanLeft(rc, (x, y) -> x + y, 7, MutList.range(rc, 5, 13));
        assertEq(expected = Some(75), MutList.last(v))
    }


    /////////////////////////////////////////////////////////////////////////////
    // scanRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def scanRight01(): Unit \ Assert = region rc {
        let v = MutList.scanRight(rc, (x, y) -> x + y, 0, MutList.empty(rc));
        assertEq(expected = 1, MutList.length(v));
        assertEq(expected = Some(0), MutList.head(v))
    }

    @Test
    def scanRight02(): Unit \ Assert = region rc {
        let v1 = MutList.range(rc, 1, 6);
        let v2 = MutList.empty(rc);
        MutList.push(15, v2);
        MutList.push(14, v2);
        MutList.push(12, v2);
        MutList.push(9, v2);
        MutList.push(5, v2);
        MutList.push(0, v2);
        let v3 = MutList.scanRight(rc, (x, y) -> x + y, 0, v1);
        assertTrue(MutList.sameElements(v2, v3))
    }

    @Test
    def scanRight03(): Unit \ Assert = region rc {
        let v = MutList.scanRight(rc, (x, y) -> x + y, 7, MutList.range(rc, 5, 13));
        assertEq(expected = Some(7), MutList.last(v))
    }


    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map01(): Unit \ Assert = region rc {
        let v = MutList.map(rc, x -> x * 2, MutList.range(rc, 0, 6));
        let expected = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, expected), Array#{0, 2, 4, 6, 8, 10} @ rc);
        assertTrue(MutList.sameElements(v, expected))
    }

    @Test
    def map02(): Unit \ Assert = region rc {
        let v = MutList.map(rc, identity, MutList.range(rc, 0, 10));
        assertTrue(MutList.sameElements(v, MutList.range(rc, 0, 10)))
    }

    @Test
    def map03(): Unit \ Assert = region rc {
        let v = MutList.map(rc, identity, (MutList.empty(rc): MutList[Unit, _]));
        assertTrue(MutList.sameElements(v, MutList.empty(rc)))
    }

    @Test
    def map04(): Unit \ Assert = region rc {
        let v = MutList.map(rc, x -> x + 1, MutList.range(rc, 0, 100));
        assertTrue(MutList.sameElements(v, MutList.range(rc, 1, 101)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatten01(): Unit \ Assert = region rc {
        assertTrue(MutList.flatten(rc, MutList.empty(rc)) |> MutList.isEmpty)
    }

    @Test
    def flatten02(): Unit \ Assert = region rc {
        let empties = MutList.empty(rc);
        List.forEach(x -> MutList.push(x, empties), List.repeat(10, MutList.empty(rc)));
        assertTrue(MutList.flatten(rc, empties) |> MutList.isEmpty)
    }

    @Test
    def flatten03(): Unit \ Assert = region rc {
        let ml1 = MutList.empty(rc);
        List.forEach(x -> MutList.push(x, ml1), List#{1, 2});
        let ml2 = MutList.empty(rc);
        let ml3 = MutList.empty(rc);
        List.forEach(x -> MutList.push(x, ml3), List#{3, 4});
        let lists = MutList.empty(rc);
        MutList.push(ml1, lists);
        MutList.push(ml2, lists);
        MutList.push(ml3, lists);
        let expected = MutList.empty(rc);
        List.forEach(x -> MutList.push(x, expected), List#{1, 2, 3, 4});
        assertTrue(MutList.sameElements(MutList.flatten(rc, lists), expected))
    }

    /////////////////////////////////////////////////////////////////////////////
    // flatMap                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatMap01(): Unit \ Assert = region rc {
        assertTrue(MutList.flatMap(rc, x -> x, MutList.empty(rc)) |> MutList.isEmpty)
    }

    @Test
    def flatMap02(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 0, 5);
        assertTrue(MutList.flatMap(rc, _ -> MutList.empty(rc), v) |> MutList.isEmpty)
    }

    @Test
    def flatMap03(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 0, 100);
        def f(n) = {
            let ml = MutList.empty(rc);
            MutList.push(n, ml);
            ml
        };
        let w = MutList.flatMap(rc, f, v);
        assertTrue(MutList.sameElements(v, w))
    }

    @Test
    pub def flatMap04(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 0, 5);
        def f(n) = {
            let ml = MutList.empty(rc);
            List.forEach(x -> MutList.push(x, ml), List.repeat(n, Int32.toString(n)));
            ml
        };
        let w = MutList.flatMap(rc, f, v);
        let expected = MutList.empty(rc);
        List.forEach(x -> MutList.push(x, expected), List#{
            "1", "2", "2", "3", "3", "3", "4", "4", "4", "4"
        });
        assertTrue(MutList.sameElements(expected, w))
    }

    /////////////////////////////////////////////////////////////////////////////
    // transform                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def transform01(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 0, 6);
        MutList.transform(x -> x * 2, v);
        let expected = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, expected), Array#{0, 2, 4, 6, 8, 10} @ rc);
        assertTrue(MutList.sameElements(v, expected))
    }

    @Test
    def transform02(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 0, 10);
        MutList.transform(identity, v);
        assertTrue(MutList.sameElements(v, MutList.range(rc, 0, 10)))
    }

    @Test
    def transform03(): Unit \ Assert = region rc {
        let v: MutList[Unit, _] = MutList.empty(rc);
        MutList.transform(identity, v);
        assertTrue(MutList.sameElements(v, MutList.empty(rc)))
    }

    @Test
    def transform04(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 0, 100);
        MutList.transform(x -> x + 1, v);
        assertTrue(MutList.sameElements(v, MutList.range(rc, 1, 101)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapWithIndex01(): Unit \ Assert = region rc {
        let v = MutList.mapWithIndex(rc, (i, _) -> if (i < 3) 0 else 1, MutList.range(rc, 0, 6));
        let expected = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, expected), Array#{0, 0, 0, 1, 1, 1} @ rc);
        assertTrue(MutList.sameElements(v, expected))
    }

    @Test
    def mapWithIndex02(): Unit \ Assert = region rc {
        let v = MutList.mapWithIndex(rc, (i, x) -> if (x == 8 and i == 1) 42 else 0, MutList.range(rc, 7, 10));
        let expected = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, expected), Array#{0, 42, 0} @ rc);
        assertTrue(MutList.sameElements(v, expected))
    }

    @Test
    def mapWithIndex03(): Unit \ Assert = region rc {
        let v = MutList.mapWithIndex(rc, (i, _) -> if (i == 0) 42 else 0, MutList.empty(rc));
        assertTrue(MutList.sameElements(v, MutList.empty(rc)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // transformWithIndex                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def transformWithIndex01(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 0, 6);
        MutList.transformWithIndex((i, _) -> if (i < 3) 0 else 1, v);
        let expected = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, expected), Array#{0, 0, 0, 1, 1, 1} @ rc);
        assertTrue(MutList.sameElements(v, expected))
    }

    @Test
    def transformWithIndex02(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 7, 10);
        MutList.transformWithIndex((i, x) -> if (x == 8 and i == 1) 42 else 0, v);
        let expected = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, expected), Array#{0, 42, 0} @ rc);
        assertTrue(MutList.sameElements(v, expected))
    }

    @Test
    def transformWithIndex03(): Unit \ Assert = region rc {
        let v = MutList.empty(rc);
        MutList.transformWithIndex((i, _) -> if (i == 0) 42 else 0, v);
        assertTrue(MutList.sameElements(v, MutList.empty(rc)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeft01(): Unit \ Assert = region rc {
        assertEq(expected = 10, MutList.foldLeft((acc, x) -> acc + x, 0, MutList.range(rc, 1, 5)))
    }

    @Test
    def foldLeft02(): Unit \ Assert = region rc {
        let v1 = MutList.empty(rc);
        let v2 = MutList.range(rc, 0, 10);
        MutList.foldLeft((_, x) -> MutList.push(x, v1), (), v2);
        assertTrue(MutList.sameElements(v1, v2))
    }

    @Test
    def foldLeft03(): Unit \ Assert = region rc {
        assertEq(expected = 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil, MutList.foldLeft((acc, x) -> x :: acc, Nil, MutList.range(rc, 0, 6)))
    }

    @Test
    def foldLeft04(): Unit \ Assert = region rc {
        assertEq(expected = -4950, MutList.foldLeft((acc, x) -> acc - x, 0, MutList.range(rc, 0, 100)))
    }

    @Test
    def foldLeft05(): Unit \ Assert = region rc {
        assertEq(expected = 42, MutList.foldLeft((acc, x) -> acc + x, 42, MutList.empty(rc)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRight01(): Unit \ Assert = region rc {
        assertEq(expected = 10, MutList.foldRight((x, acc) -> acc + x, 0, MutList.range(rc, 1, 5)))
    }

    @Test
    def foldRight02(): Unit \ Assert = region rc {
        assertEq(expected = "34567", MutList.foldRight((x, acc) -> Int32.toString(x) + acc, "", MutList.range(rc, 3, 8)))
    }

    @Test
    def foldRight03(): Unit \ Assert = region rc {
        assertEq(expected = 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil, MutList.foldRight((x, acc) -> x :: acc, Nil, MutList.range(rc, 0, 6)))
    }

    @Test
    def foldRight04(): Unit \ Assert = region rc {
        assertEq(expected = -50, MutList.foldRight((x, acc) -> x - acc, 0, MutList.range(rc, 0, 100)))
    }

    @Test
    def foldRight05(): Unit \ Assert = region rc {
        assertEq(expected = 42, MutList.foldRight((x, acc) -> x + acc, 42, MutList.empty(rc)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRightWithCont01(): Unit \ Assert = region rc {
        assertEq(expected = 10, MutList.foldRightWithCont((x, k) -> k() + x, 0, MutList.range(rc, 1, 5)))
    }

    @Test
    def foldRightWithCont02(): Unit \ Assert = region rc {
        assertEq(expected = "34567", MutList.foldRightWithCont((x, k) -> Int32.toString(x) + k(), "", MutList.range(rc, 3, 8)))
    }

    @Test
    def foldRightWithCont03(): Unit \ Assert = region rc {
        assertEq(expected = 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil, MutList.foldRightWithCont((x, k) -> x :: k(), Nil, MutList.range(rc, 0, 6)))
    }

    @Test
    def foldRightWithCont04(): Unit \ Assert = region rc {
        assertEq(expected = -50, MutList.foldRightWithCont((x, k) -> x - k(), 0, MutList.range(rc, 0, 100)))
    }

    @Test
    def foldRightWithCont05(): Unit \ Assert = region rc {
        assertEq(expected = 42, MutList.foldRightWithCont((x, k) -> x + k(), 42, MutList.empty(rc)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldMap01(): Unit \ Assert = region rc {
        let ml = MutList.empty(rc);
        List.forEach(x -> MutList.push(x, ml), (Nil: List[Int32]));
        assertEq(expected = 0, MutList.foldMap(x -> 2 * x, ml))
    }

    @Test
    def foldMap02(): Unit \ Assert = region rc {
        let ml = MutList.empty(rc);
        List.forEach(x -> MutList.push(x, ml), 1 :: 2 :: Nil);
        assertEq(expected = 6, MutList.foldMap(x -> 2 * x, ml))
    }

    @Test
    def foldMap03(): Unit \ Assert = region rc {
        let ml = MutList.empty(rc);
        List.forEach(x -> MutList.push(x, ml), "a" :: Nil);
        assertEq(expected = "b", MutList.foldMap(x -> if (x == "a") "b" else x, ml))
    }

    @Test
    def foldMap04(): Unit \ Assert = region rc {
        let ml = MutList.empty(rc);
        List.forEach(x -> MutList.push(x, ml), "a" :: "b" :: "c":: Nil);
        assertEq(expected = "abb", MutList.foldMap(x -> if (x == "c") "b" else x, ml))
    }

    @Test
    def foldMap05(): Unit \ Assert = region rc {
        let ml = MutList.empty(rc);
        List.forEach(x -> MutList.push(x, ml), 1 :: 2 :: 3 :: Nil);
        assertEq(expected = "123", MutList.foldMap(Int32.toString, ml))
    }


    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceLeft01(): Unit \ Assert = region rc {
        assertEq(expected = None, MutList.reduceLeft((x, y) -> x + y, (MutList.empty(rc): MutList[Int32, _])))
    }

    @Test
    def reduceLeft02(): Unit \ Assert = region rc {
        assertEq(expected = Some(10), MutList.reduceLeft((x, y) -> x + y, MutList.range(rc, 0, 5)))
    }

    @Test
    def reduceLeft03(): Unit \ Assert = region rc {
        assertEq(expected = Some(-5050), MutList.reduceLeft((x, y) -> x - y, MutList.range(rc, 0, 101)))
    }

    @Test
    def reduceLeft04(): Unit \ Assert = region rc {
        assertEq(expected = Some(1), MutList.reduceLeft((_, y) -> if (y > 4) 1 else 0, MutList.range(rc, 0, 6)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceRight01(): Unit \ Assert = region rc {
        assertEq(expected = None, MutList.reduceRight((x, y) -> x + y, (MutList.empty(rc): MutList[Int32, _])))
    }

    @Test
    def reduceRight02(): Unit \ Assert = region rc {
        assertEq(expected = Some(10), MutList.reduceRight((x, y) -> x + y, MutList.range(rc, 0, 5)))
    }

    @Test
    def reduceRight03(): Unit \ Assert = region rc {
        assertEq(expected = Some(-3), MutList.reduceRight((x, y) -> x - y, MutList.range(rc, 0, 6)))
    }

    @Test
    def reduceRight04(): Unit \ Assert = region rc {
        assertEq(expected = Some(1), MutList.reduceRight((x, _) -> if (x < 2) 1 else 0, MutList.range(rc, 0, 6)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // clear                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def clear01(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 0, 1);
        let len = MutList.length(v);
        MutList.clear(v);
        assertEq(expected = 1, len);
        assertTrue(MutList.isEmpty(v))
    }

    @Test
    def clear02(): Unit \ Assert = region rc {
        let v = MutList.empty(rc);
        let len = MutList.length(v);
        MutList.clear(v);
        assertEq(expected = len, MutList.length(v))
    }

    @Test
    def clear03(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 0, 100);
        MutList.clear(v);
        assertTrue(MutList.isEmpty(v))
    }


    /////////////////////////////////////////////////////////////////////////////
    // copy                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def copy01(): Unit \ Assert = region rc {
        let v1 = MutList.range(rc, 0, 6);
        let v2 = MutList.copy(rc, v1);
        assertTrue(v1 `sameElements` v2)
    }

    @Test
    def copy02(): Unit \ Assert = region rc {
        let v1 = MutList.range(rc, 0, 100);
        let v2 = MutList.copy(rc, v1);
        MutList.transform(x -> x + 1, v2);
        assertEq(expected = Some(99), MutList.last(v1));
        assertEq(expected = Some(100), MutList.last(v2))
    }

    @Test
    def copy03(): Unit \ Assert = region rc {
        let v1 = MutList.range(rc, 0, 6);
        let v2 = MutList.copy(rc, v1);
        MutList.clear(v1);
        assertEq(expected = 0, MutList.length(v1));
        assertEq(expected = 6, MutList.length(v2))
    }

    @Test
    def copy04(): Unit \ Assert = region rc {
        let v1 = MutList.range(rc, 0, 1000);
        let v2 = MutList.copy(rc, v1);
        assertEq(expected = MutList.length(v1), MutList.length(v2));
        assertTrue(MutList.toArray(rc, v1) `Array.sameElements` MutList.toArray(rc, v2))
    }


    /////////////////////////////////////////////////////////////////////////////
    // pop                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def pop01(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 0, 6);
        let len = MutList.length(v);
        let last = MutList.pop(v);
        assertEq(expected = len - 1, MutList.length(v));
        assertEq(expected = Some(5), last)
    }

    @Test
    def pop02(): Unit \ Assert = region rc {
        let v: MutList[Unit, _] = MutList.empty(rc);
        assertEq(expected = None, MutList.pop(v))
    }

    @Test
    def pop03(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 1, 4);
        let three = MutList.pop(v);
        let two = MutList.pop(v);
        let one = MutList.pop(v);
        let none = MutList.pop(v);
        assertEq(expected = Some(3), three);
        assertEq(expected = Some(2), two);
        assertEq(expected = Some(1), one);
        assertEq(expected = None, none)
    }


    /////////////////////////////////////////////////////////////////////////////
    // push                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def push01(): Unit \ Assert = region rc {
        let v = MutList.empty(rc);
        let len = MutList.length(v);
        MutList.push(1, v);
        assertEq(expected = Some(1), MutList.last(v));
        assertEq(expected = len + 1, MutList.length(v))
    }

    @Test
    def push02(): Unit \ Assert = region rc {
        let v = MutList.empty(rc);
        let len = MutList.length(v);
        List.forEach(x -> MutList.push(x, v), List.range(0, 97));
        assertEq(expected = Some(0), MutList.head(v));
        assertEq(expected = Some(96), MutList.last(v));
        assertEq(expected = len + 97, MutList.length(v))
    }

    @Test
    def push03(): Unit \ Assert = region rc {
        let v = MutList.empty(rc);
        MutList.push(42, v);
        MutList.push(31, v);
        MutList.push(1019, v);
        assertEq(expected = 3, MutList.length(v));
        assertEq(expected = Some(42), MutList.nth(0, v));
        assertEq(expected = Some(31), MutList.nth(1, v));
        assertEq(expected = Some(1019), MutList.nth(2, v))
    }

    @Test
    def push04(): Unit \ Assert = region rc {
        let v = MutList.empty(rc);
        MutList.push(0, v);
        MutList.push(0, v);
        MutList.push(0, v);
        MutList.push(0, v);
        assertEq(expected = 4, MutList.length(v));
        assertEq(expected = Some(0), MutList.nth(0, v));
        assertEq(expected = Some(0), MutList.nth(1, v));
        assertEq(expected = Some(0), MutList.nth(2, v));
        assertEq(expected = Some(0), MutList.nth(3, v))
    }


    /////////////////////////////////////////////////////////////////////////////
    // insert                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def insert01(): Unit \ Assert = region rc {
        let v = MutList.empty(rc);
        MutList.insert(42, 0, v);
        assertEq(expected = 1, MutList.length(v));
        assertEq(expected = Some(42), MutList.head(v))
    }

    @Test
    def insert02(): Unit \ Assert = region rc {
        let v = MutList.empty(rc);
        MutList.insert(107, 0, v);
        MutList.insert(7, 0, v);
        assertEq(expected = 2, MutList.length(v));
        assertEq(expected = Some(7), MutList.head(v))
    }

    @Test
    def insert03(): Unit \ Assert = region rc {
        let v = MutList.empty(rc);
        List.forEach(x -> MutList.insert(x, 0, v), List.range(0, 107));
        assertEq(expected = 107, MutList.length(v));
        assertEq(expected = Some(106), MutList.head(v))
    }

    @Test
    def insert04(): Unit \ Assert = region rc {
        let v = MutList.empty(rc);
        MutList.insert(42, 0, v);
        MutList.insert(3, 1, v);
        MutList.insert(5, 2, v);
        MutList.insert(6, 0, v);
        assertEq(expected = 4, MutList.length(v));
        assertEq(expected = Some(6), MutList.head(v));
        assertEq(expected = Some(5), MutList.last(v))
    }

    @Test
    def insert05(): Unit \ Assert = region rc {
        let v = MutList.empty(rc);
        List.forEach(x -> MutList.push(x, v), List.range(97, 147));
        MutList.insert(42, 0, v);
        MutList.insert(49, 0, v);
        assertEq(expected = 52, MutList.length(v));
        assertEq(expected = Some(49), MutList.head(v));
        assertEq(expected = Some(146), MutList.last(v))
    }

    @Test
    def insert06(): Unit \ Assert = region rc {
        let v = MutList.empty(rc);
        MutList.insert(42, 0, v);
        MutList.insert(42, 0, v);
        MutList.insert(42, 0, v);
        MutList.insert(42, 1, v);
        MutList.insert(97, 0, v);
        MutList.insert(98, 2, v);
        assertEq(expected = 6, MutList.length(v));
        assertEq(expected = Some(97), MutList.head(v));
        assertEq(expected = Some(42), MutList.last(v))
    }

    @Test
    def insert07(): Unit \ Assert = region rc {
        let v = MutList.empty(rc);
        MutList.insert(97, 0, v);
        MutList.insert(98, 1, v);
        MutList.insert(99, 2, v);
        MutList.insert(107, 1, v);
        assertEq(expected = 4, MutList.length(v));
        assertEq(expected = Some(107), MutList.nth(1, v))
    }

    @Test
    def insert08(): Unit \ Assert = region rc {
        let l1 = MutList.range(rc, 0, 4);
        MutList.insert(42, 3, l1);
        MutList.insert(41, 2, l1);
        MutList.insert(40, 1, l1);
        MutList.insert(55, 0, l1);
        MutList.insert(33, 1, l1);
        let l2 = MutList.empty(rc);
        let xs = 55 :: 33 :: 0 :: 40 :: 1 :: 41 :: 2 :: 42 :: 3 :: Nil;
        List.forEach(x -> MutList.push(x, l2), xs);
        assertTrue(MutList.sameElements(l1, l2))
    }


    /////////////////////////////////////////////////////////////////////////////
    // remove                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def remove01(): Unit \ Assert = region rc {
        let v: MutList[Unit, _] = MutList.empty(rc);
        MutList.remove(0, v);
        assertTrue(MutList.sameElements(v, MutList.empty(rc)))
    }

    @Test
    def remove02(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 0, 4);
        MutList.remove(0, v);
        assertTrue(MutList.sameElements(v, MutList.range(rc, 1, 4)))
    }

    @Test
    def remove03(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 0, 4);
        MutList.remove(3, v);
        assertTrue(MutList.sameElements(v, MutList.range(rc, 0, 3)))
    }

    @Test
    def remove04(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 3, 7);
        MutList.remove(0, v);
        MutList.remove(0, v);
        MutList.remove(0, v);
        assertTrue(MutList.sameElements(v, MutList.range(rc, 6, 7)))
    }

    @Test
    def remove05(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 9, 12);
        MutList.remove(2, v);
        MutList.remove(1, v);
        MutList.remove(0, v);
        assertTrue(MutList.sameElements(v, MutList.empty(rc)))
    }

    @Test
    def remove06(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 7, 13);
        MutList.remove(0, v);
        MutList.remove(0, v);
        MutList.remove(0, v);
        MutList.remove(0, v);
        MutList.remove(0, v);
        MutList.remove(0, v);
        assertTrue(MutList.sameElements(v, MutList.empty(rc)))
    }

    @Test
    def remove07(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 0, 3);
        MutList.remove(3, v);
        MutList.remove(7, v);
        MutList.remove(99, v);
        assertTrue(MutList.sameElements(v, MutList.range(rc, 0, 3)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // pushAll                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def pushAll01(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 0, 4);
        let l = List.range(4, 8);
        MutList.pushAll(l, v);
        assertTrue(MutList.sameElements(v, MutList.range(rc, 0, 8)))
    }

    @Test
    def pushAll02(): Unit \ Assert = region rc {
        let v = MutList.empty(rc);
        let l = List.range(1, 4);
        MutList.pushAll(l, v);
        assertTrue(MutList.sameElements(v, MutList.range(rc, 1, 4)))
    }

    @Test
    def pushAll03(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 9, 24);
        let l = (Nil: List[Int32]);
        MutList.pushAll(l, v);
        assertTrue(MutList.sameElements(v, MutList.range(rc, 9, 24)))
    }

    @Test
    def pushAll04(): Unit \ Assert = region rc {
        let v: MutList[Unit, _] = MutList.empty(rc);
        let l: List[Unit] = Nil;
        MutList.pushAll(l, v);
        assertTrue(MutList.sameElements(v, MutList.empty(rc)))
    }

    @Test
    def pushAll05(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 1, 3);
        let l1 = List.range(3, 5);
        let l2 = List.range(5, 99);
        MutList.pushAll(l1, v);
        MutList.pushAll(l2, v);
        assertTrue(MutList.sameElements(v, MutList.range(rc, 1, 99)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // retain                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def retain01(): Unit \ Assert = region rc {
        let l1 = MutList.range(rc, 0, 10);
        MutList.retain(x -> x `Int32.remainder` 2 == 0, l1);
        let l2 = MutList.empty(rc);
        let xs = 0 :: 2 :: 4 :: 6 :: 8 :: Nil;
        List.forEach(x -> MutList.push(x, l2), xs);
        assertTrue(MutList.sameElements(l1, l2))
    }

    @Test
    def retain02(): Unit \ Assert = region rc {
        let l1 = MutList.range(rc, 0, 10);
        MutList.retain(x -> x `Int32.remainder` 2 == 1, l1);
        let l2 = MutList.empty(rc);
        let xs = 1 :: 3 :: 5 :: 7 :: 9 :: Nil;
        List.forEach(x -> MutList.push(x, l2), xs);
        assertTrue(MutList.sameElements(l1, l2))
    }

    @Test
    def retain03(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 0, 10);
        MutList.retain(x -> x > 5, v);
        assertTrue(MutList.sameElements(v, MutList.range(rc, 6, 10)))
    }

    @Test
    def retain04(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 0, 10);
        MutList.retain(x -> x > 10, v);
        assertTrue(MutList.sameElements(v, MutList.empty(rc)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def replace01(): Unit \ Assert = region rc {
        let l1 = MutList.empty(rc);
        MutList.push(1, l1);
        MutList.push(2, l1);
        MutList.push(2, l1);
        MutList.push(1, l1);
        MutList.push(1, l1);
        MutList.replace(src = 1, dst = 42, l1);
        let l2 = MutList.empty(rc);
        let xs = 42 :: 2 :: 2 :: 42 :: 42 :: Nil;
        List.forEach(x -> MutList.push(x, l2), xs);
        assertTrue(MutList.sameElements(l1, l2))
    }

    @Test
    def replace02(): Unit \ Assert = region rc {
        let l1 = MutList.range(rc, 4, 7);
        MutList.replace(src = 4, dst = 0, l1);
        let l2 = MutList.empty(rc);
        let xs = 0 :: 5 :: 6 :: Nil;
        List.forEach(x -> MutList.push(x, l2), xs);
        assertTrue(MutList.sameElements(l1, l2))
    }

    @Test
    def replace03(): Unit \ Assert = region rc {
        let l1 = MutList.range(rc, 4, 7);
        MutList.replace(src = 5, dst = 0, l1);
        let l2 = MutList.empty(rc);
        let xs = 4 :: 0 :: 6 :: Nil;
        List.forEach(x -> MutList.push(x, l2), xs);
        assertTrue(MutList.sameElements(l1, l2))
    }

    @Test
    def replace04(): Unit \ Assert = region rc {
        let l1 = MutList.range(rc, 4, 7);
        MutList.replace(src = 6, dst = 0, l1);
        let l2 = MutList.empty(rc);
        let xs = 4 :: 5 :: 0 :: Nil;
        List.forEach(x -> MutList.push(x, l2), xs);
        assertTrue(MutList.sameElements(l1, l2))
    }

    @Test
    def replace05(): Unit \ Assert = region rc {
        let l1 = MutList.empty(rc);
        let xs1 = List.repeat(97, 'a');
        List.forEach(x -> MutList.push(x, l1), xs1);
        MutList.replace(src = 'a', dst = 'z', l1);
        let l2 = MutList.empty(rc);
        let xs2 = List.repeat(97, 'z');
        List.forEach(x -> MutList.push(x, l2), xs2);
        assertTrue(MutList.sameElements(l1, l2))
    }


    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reverse01(): Unit \ Assert = region rc {
        let v: MutList[Unit, _] = MutList.empty(rc);
        MutList.reverse(v);
        assertTrue(MutList.sameElements(v, MutList.empty(rc)))
    }

    @Test
    def reverse02(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 0, 1);
        MutList.reverse(v);
        assertTrue(MutList.sameElements(v, MutList.range(rc, 0, 1)))
    }

    @Test
    def reverse03(): Unit \ Assert = region rc {
        let l1 = MutList.range(rc, 0, 5);
        MutList.reverse(l1);
        let l2 = MutList.empty(rc);
        let xs = 4 :: 3 :: 2 :: 1 :: 0 :: Nil;
        List.forEach(x -> MutList.push(x, l2), xs);
        assertTrue(MutList.sameElements(l1, l2))
    }

    @Test
    def reverse04(): Unit \ Assert = region rc {
        let l1 = MutList.range(rc, 0, 6);
        MutList.reverse(l1);
        let l2 = MutList.empty(rc);
        let xs = 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil;
        List.forEach(x -> MutList.push(x, l2), xs);
        assertTrue(MutList.sameElements(l1, l2))
    }

    @Test
    def reverse05(): Unit \ Assert = region rc {
        let l = MutList.empty(rc);
        let xs = 7 :: 6 :: 5 :: 4 :: 3 :: 2 :: Nil;
        List.forEach(x -> MutList.push(x, l), xs);
        MutList.reverse(l);
        assertTrue(MutList.sameElements(l, MutList.range(rc, 2, 8)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // shrink                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def shrink01(): Unit \ Assert = region rc {
        let v = MutList.empty(rc);
        MutList.push(0, v);
        MutList.push(1, v);
        MutList.push(2, v);
        MutList.shrink(v);
        assertEq(expected = 3, MutList.length(v))
    }

    @Test
    def shrink02(): Unit \ Assert = region rc {
        let v = MutList.empty(rc);
        MutList.shrink(v);
        assertEq(expected = 0, MutList.length(v))
    }

    @Test
    def shrink03(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 0, 9);
        MutList.push(9, v);
        MutList.shrink(v);
        assertEq(expected = 10, MutList.length(v))
    }

    @Test
    def shrink04(): Unit \ Assert = region rc {
        let v = MutList.empty(rc);
        MutList.push(4, v);
        MutList.append(MutList.toList(MutList.range(rc, 0, 4)), v);
        MutList.push(5, v);
        MutList.shrink(v);
        assertEq(expected = 6, MutList.length(v))
    }


    /////////////////////////////////////////////////////////////////////////////
    // truncate                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def truncate01(): Unit \ Assert = region rc {
        let v: MutList[Unit, _] = MutList.empty(rc);
        MutList.truncate(0, v);
        assertTrue(MutList.sameElements(v, MutList.empty(rc)))
    }

    @Test
    def truncate02(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 0, 10);
        MutList.truncate(5, v);
        assertTrue(MutList.sameElements(v, MutList.range(rc, 0, 5)))
    }

    @Test
    def truncate03(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 0, 99);
        MutList.truncate(-1, v);
        assertTrue(MutList.sameElements(v, MutList.empty(rc)))
    }

    @Test
    def truncate04(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 7, 8);
        MutList.truncate(2, v);
        assertTrue(MutList.sameElements(v, MutList.range(rc, 7, 8)))
    }

    @Test
    def truncate05(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 99, 102);
        MutList.truncate(1, v);
        assertTrue(MutList.sameElements(v, MutList.range(rc, 99, 100)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterator01(): Unit \ Assert = region rc {
        let l = (MutList.empty(rc): MutList[Int32, rc]);
        let iter = MutList.iterator(rc, l);
        match Iterator.next(iter) {
            case None => ()
            case _    => fail("Unexpected case")
        }
    }

    @Test
    def iterator02(): Unit \ Assert = region rc {
        let l = MutList.empty(rc);
        List.forEach(x -> MutList.push(x, l), 1 :: 2 :: 3 :: Nil);
        let iter = MutList.iterator(rc, l);
        match Iterator.next(iter) {
            case Some(_) => ()
            case _       => fail("Unexpected case")
        }
    }

    @Test
    def iterator03(): Unit \ Assert = region rc {
        let l = MutList.empty(rc);
        List.forEach(x -> MutList.push(x, l), 1 :: 2 :: 3 :: Nil);
        discard MutList.pop(l);
        discard MutList.pop(l);
        discard MutList.pop(l);
        let iter = MutList.iterator(rc, l);
        match Iterator.next(iter) {
            case None => ()
            case _    => fail("Unexpected case")
        }
    }

    @Test
    def iterator04(): Unit \ Assert = region rc {
        let l = MutList.empty(rc);
        List.forEach(x -> MutList.push(x, l), 1 :: 2 :: 3 :: Nil);
        assertEq(expected = 1 :: 2 :: 3 :: Nil, MutList.iterator(rc, l) |> Iterator.toList)
    }

    @Test
    def iterator05(): Unit \ Assert = region rc {
        let l = MutList.range(rc, 0, 100);
        assertEq(expected = List.range(0, 100), MutList.iterator(rc, l) |> Iterator.toList);
        assertEq(expected = List.range(0, 100), MutList.toList(l))
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Unit \ Assert = region rc {
        let v = MutList.empty(rc);
        let sb = StringBuilder.empty(rc);
        let f = x -> if (x > 0) StringBuilder.append('T', sb) else StringBuilder.append('F', sb);
        MutList.forEach(f, v);
        assertEq(expected = "", StringBuilder.toString(sb))
    }

    @Test
    def forEach02(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 0, 5);
        let sb = StringBuilder.empty(rc);
        let f = x -> if (x `Int32.remainder` 2 == 0) StringBuilder.append('T', sb) else StringBuilder.append('F', sb);
        MutList.forEach(f, v);
        assertEq(expected = "TFTFT", StringBuilder.toString(sb))
    }

    @Test
    def forEach03(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 0, 2);
        let sb = StringBuilder.empty(rc);
        let f = x -> if (x > 0) StringBuilder.append('T', sb) else StringBuilder.append('F', sb);
        MutList.forEach(f, v);
        assertEq(expected = "FT", StringBuilder.toString(sb))
    }

    @Test
    def forEach04(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 0, 2);
        let sb = StringBuilder.empty(rc);
        let f = x -> if (x < 0) StringBuilder.append('T', sb) else StringBuilder.append('F', sb);
        MutList.forEach(f, v);
        assertEq(expected = "FF", StringBuilder.toString(sb))
    }


    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEachWithIndex01(): Unit \ Assert = region rc {
        let v1 = MutList.range(rc, 0, 100);
        let v2 = MutList.empty(rc);
        MutList.forEachWithIndex((i, x) -> if (x `Int32.remainder` 2 == 0 and i < 10) MutList.push(x, v2) else (), v1);
        let l = MutList.empty(rc);
        let xs = 0 :: 2 :: 4 :: 6 :: 8 :: Nil;
        List.forEach(x -> MutList.push(x, l), xs);
        assertTrue(MutList.sameElements(v2, l))
    }

    @Test
    def forEachWithIndex02(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 1, 4);
        let sb = StringBuilder.empty(rc);
        let f = (i, x) -> if (x `Int32.remainder` 2 == 1 and i == 0) StringBuilder.append('T', sb) else StringBuilder.append('F', sb);
        MutList.forEachWithIndex(f, v);
        assertEq(expected = "TFF", StringBuilder.toString(sb))
    }

    @Test
    def forEachWithIndex03(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 0, 5);
        let sb = StringBuilder.empty(rc);
        let f = (_, x) -> if (x `Int32.remainder` 2 == 0) StringBuilder.append('T', sb) else StringBuilder.append('F', sb);
        MutList.forEachWithIndex(f, v);
        assertEq(expected = "TFTFT", StringBuilder.toString(sb))
    }


    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimumBy01(): Unit \ Assert = region rc {
        assertEq(expected = None, MutList.minimumBy((x, y) -> x <=> y, (MutList.empty(rc): MutList[Int32, _])))
    }

    @Test
    def minimumBy02(): Unit \ Assert = region rc {
        assertEq(expected = Some(0), MutList.minimumBy((x, y) -> x <=> y, MutList.range(rc, 0, 5)))
    }

    @Test
    def minimumBy03(): Unit \ Assert = region rc {
        assertEq(expected = Some(5), MutList.minimumBy((x, y) -> x <=> y, MutList.range(rc, 5, 10)))
    }

    @Test
    def minimumBy04(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 0, 6);
        MutList.push(-2, v);
        assertEq(expected = Some(-2), MutList.minimumBy((x, y) -> x <=> y, v))
    }

    @Test
    def minimumBy05(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 9, 19);
        let cmp = (x, y) -> if (x < y and x `Int32.remainder` 2 == 0) Comparison.LessThan else Comparison.GreaterThan;
        assertEq(expected = Some(10), MutList.minimumBy(cmp, v))
    }


    /////////////////////////////////////////////////////////////////////////////
    // maximumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maximumBy01(): Unit \ Assert = region rc {
        assertEq(expected = None, MutList.maximumBy((x, y) -> x <=> y, (MutList.empty(rc): MutList[Int32, _])))
    }

    @Test
    def maximumBy02(): Unit \ Assert = region rc {
        assertEq(expected = Some(4), MutList.maximumBy((x, y) -> x <=> y, MutList.range(rc, 0, 5)))
    }

    @Test
    def maximumBy03(): Unit \ Assert = region rc {
        assertEq(expected = Some(9), MutList.maximumBy((x, y) -> x <=> y, MutList.range(rc, 5, 10)))
    }

    @Test
    def maximumBy04(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 0, 6);
        MutList.push(11, v);
        assertEq(expected = Some(11), MutList.maximumBy((x, y) -> x <=> y, v))
    }

    @Test
    def maximumBy05(): Unit \ Assert = region rc {
        let v = MutList.range(rc, 9, 19);
        let cmp = (x, y) -> if (x > y and x `Int32.remainder` 2 == 0) Comparison.GreaterThan else Comparison.LessThan;
        assertEq(expected = Some(18), MutList.maximumBy(cmp, v))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toArray01(): Unit \ Assert = region rc {
        let ml = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, ml), Array#{1, 2, 3} @ rc);
        assertTrue(Array.sameElements(MutList.toArray(rc, ml), Array#{1, 2, 3} @ rc))
    }

    @Test
    def toArray02(): Unit \ Assert = region rc {
        let v = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, v), Array#{1, 2, 3} @ rc);
        let a = MutList.toArray(rc, v);
        Array.put(42, 1, a);
        assertEq(expected = Some(2), MutList.nth(1, v))
    }

    @Test
    def toArray03(): Unit \ Assert = region rc {
        let v = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, v), Array#{1, 2, 3} @ rc);
        discard MutList.pop(v);
        discard MutList.pop(v);
        assertEq(expected = 1, Array.length(MutList.toArray(rc, v)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toVector01(): Unit \ Assert = region rc {
        assertTrue(MutList.toVector((MutList.empty(rc) : MutList[Int32, _])) `Vector.equals` Vector.empty())
    }

    @Test
    def toVector02(): Unit \ Assert = region rc {
        let l = MutList.empty(rc);
        MutList.push(0, l);
        assertTrue(MutList.toVector(l) `Vector.equals` Vector.singleton(0))
    }

    @Test
    def toVector03(): Unit \ Assert = region rc {
        let l = MutList.empty(rc);
        MutList.push(1, l);
        MutList.push(2, l);
        MutList.push(3, l);
        assertTrue(MutList.toVector(l) `Vector.equals` List.toVector(1 :: 2 :: 3 :: Nil))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toList01(): Unit \ Assert = region rc {
        assertEq(expected = (Nil: List[Int32]), (MutList.empty(rc): MutList[Int32, _]) |> MutList.toList)
    }

    @Test
    def toList02(): Unit \ Assert = region rc {
        let ml = MutList.range(rc, 0, 1000);
        assertEq(expected = List.range(0, 1000), ml |> MutList.toList);
        assertFalse(MutList.isEmpty(ml))
    }

    @Test
    def toList03(): Unit \ Assert = region rc {
        let ml = MutList.range(rc, 0, 1000);
        assertEq(expected = List.range(0, 1000), ml |> MutList.toList);
        assertTrue(ml `sameElements` MutList.range(rc, 0, 1000))
    }


    /////////////////////////////////////////////////////////////////////////////
    // toChain                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toChain01(): Unit \ Assert = region rc {
        let l = MutList.empty(rc);
        let xs: List[Int32] = Nil;
        List.forEach(x -> MutList.push(x, l), xs);
        assertEq(expected = Chain.empty(), MutList.toChain(l))
    }

    @Test
    def toChain02(): Unit \ Assert = region rc {
        let l = MutList.empty(rc);
        let xs = 1 :: Nil;
        List.forEach(x -> MutList.push(x, l), xs);
        assertEq(expected = Chain.singleton(1), MutList.toChain(l))
    }

    @Test
    def toChain03(): Unit \ Assert = region rc {
        let l = MutList.empty(rc);
        let xs = 1 :: 2 :: Nil;
        List.forEach(x -> MutList.push(x, l), xs);
        assertEq(expected = List.toChain(1 :: 2 :: Nil), MutList.toChain(l))
    }

    @Test
    def toChain04(): Unit \ Assert = region rc {
        let l = MutList.empty(rc);
        let xs = 1 :: 2 :: 3 :: Nil;
        List.forEach(x -> MutList.push(x, l), xs);
        assertEq(expected = List.toChain(1 :: 2 :: 3 :: Nil), MutList.toChain(l))
    }


    /////////////////////////////////////////////////////////////////////////////
    // sortWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    def cmp(x: Int32, y: Int32): Comparison =
        if (x < y) Comparison.LessThan
        else if (x == y) Comparison.EqualTo
        else Comparison.GreaterThan

    @Test
    def sortWith01(): Unit \ Assert = region rc {
        let a = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, a), ((Array#{} @ rc): Array[Int32, rc]));
        MutList.sortWith(cmp, a);
        let expected = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, expected), ((Array#{} @ rc): Array[Int32, rc]));
        assertTrue(a `sameElements` expected)
    }

    @Test
    def sortWith02(): Unit \ Assert = region rc {
        let a = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, a), Array#{0} @ rc);
        MutList.sortWith(cmp, a);
        let expected = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, expected), Array#{0} @ rc);
        assertTrue(a `sameElements` expected)
    }

    @Test
    def sortWith03(): Unit \ Assert = region rc {
        let a = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, a), Array#{0,1} @ rc);
        MutList.sortWith(cmp, a);
        let expected = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, expected), Array#{0,1} @ rc);
        assertTrue(a `sameElements` expected)
    }

    @Test
    def sortWith04(): Unit \ Assert = region rc {
        let a = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, a), Array#{1,0} @ rc);
        MutList.sortWith(cmp, a);
        let expected = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, expected), Array#{0,1} @ rc);
        assertTrue(a `sameElements` expected)
    }
    @Test
    def sortWith05(): Unit \ Assert = region rc {
        let a = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, a), Array#{1,1} @ rc);
        MutList.sortWith(cmp, a);
        let expected = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, expected), Array#{1,1} @ rc);
        assertTrue(a `sameElements` expected)
    }

    @Test
    def sortWith06(): Unit \ Assert = region rc {
        let a = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, a), Array#{0,1,2,3,4,5} @ rc);
        MutList.sortWith(cmp, a);
        let expected = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, expected), Array#{0,1,2,3,4,5} @ rc);
        assertTrue(a `sameElements` expected)
    }

    @Test
    def sortWith07(): Unit \ Assert = region rc {
        let a = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, a), Array#{5,4,3,2,1,0} @ rc);
        MutList.sortWith(cmp, a);
        let expected = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, expected), Array#{0,1,2,3,4,5} @ rc);
        assertTrue(a `sameElements` expected)
    }

    @Test
    def sortWith08(): Unit \ Assert = region rc {
        let a = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, a), Array#{5,3,0,4,1,2} @ rc);
        MutList.sortWith(cmp, a);
        let expected = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, expected), Array#{0,1,2,3,4,5} @ rc);
        assertTrue(a `sameElements` expected)
    }

    @Test
    def sortWith09(): Unit \ Assert = region rc {
        let a = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, a), Array#{2,3,0,4,1,2} @ rc);
        MutList.sortWith(cmp, a);
        let expected = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, expected), Array#{0,1,2,2,3,4} @ rc);
        assertTrue(a `sameElements` expected)
    }

    @Test
    def sortWith10(): Unit \ Assert = region rc {
        let a = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, a), Array#{0,1,2,3,4,5} @ rc);
        MutList.sortWith(flip(cmp), a);
        let expected = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, expected), Array#{5,4,3,2,1,0} @ rc);
        assertTrue(a `sameElements` expected)
    }

    @Test
    def sortWith11(): Unit \ Assert = region rc {
        let a = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, a), Array#{5,4,3,2,1,0} @ rc);
        MutList.sortWith(flip(cmp), a);
        let expected = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, expected), Array#{5,4,3,2,1,0} @ rc);
        assertTrue(a `sameElements` expected)
    }

    @Test
    def sortWith12(): Unit \ Assert = region rc {
        let a = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, a), Array#{5,3,0,4,1,2} @ rc);
        MutList.sortWith(flip(cmp), a);
        let expected = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, expected), Array#{5,4,3,2,1,0} @ rc);
        assertTrue(a `sameElements` expected)
    }

    @Test
    def sortWith13(): Unit \ Assert = region rc {
        let a = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, a), Array#{2,3,0,4,1,2} @ rc);
        MutList.sortWith(flip(cmp), a);
        let expected = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, expected), Array#{4,3,2,2,1,0} @ rc);
        assertTrue(a `sameElements` expected)
    }

    /////////////////////////////////////////////////////////////////////////////
    // sort                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    def testSortSortWith(a: MutList[Int32, r]): Bool \ r = region rc1 {
        let b = MutList.copy(rc1, a);
        let c = MutList.copy(rc1, a);
        MutList.sort(b);
        MutList.sortWith(cmp, c);
        b `sameElements` c
    }

    @Test
    def sort01(): Unit \ Assert = region rc {
        let ml = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, ml), ((Array#{} @ rc): Array[Int32, rc]));
        assertTrue(testSortSortWith(ml))
    }

    @Test
    def sort02(): Unit \ Assert = region rc {
        let ml = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, ml), Array#{0} @ rc);
        assertTrue(testSortSortWith(ml))
    }

    @Test
    def sort03(): Unit \ Assert = region rc {
        let ml = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, ml), Array#{0,1} @ rc);
        assertTrue(testSortSortWith(ml))
    }

    @Test
    def sort04(): Unit \ Assert = region rc {
        let ml = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, ml), Array#{1,0} @ rc);
        assertTrue(testSortSortWith(ml))
    }

    @Test
    def sort05(): Unit \ Assert = region rc {
        let ml = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, ml), Array#{1,1} @ rc);
        assertTrue(testSortSortWith(ml))
    }

    @Test
    def sort06(): Unit \ Assert = region rc {
        let ml = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, ml), Array#{0,1,2,3,4,5} @ rc);
        assertTrue(testSortSortWith(ml))
    }

    @Test
    def sort07(): Unit \ Assert = region rc {
        let ml = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, ml), Array#{5,4,3,2,1,0} @ rc);
        assertTrue(testSortSortWith(ml))
    }

    @Test
    def sort08(): Unit \ Assert = region rc {
        let ml = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, ml), Array#{5,3,0,4,1,2} @ rc);
        assertTrue(testSortSortWith(ml))
    }

    @Test
    def sort09(): Unit \ Assert = region rc {
        let ml = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, ml), Array#{2,3,0,4,1,2} @ rc);
        assertTrue(testSortSortWith(ml))
    }


    /////////////////////////////////////////////////////////////////////////////
    // sortBy                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sortBy01(): Unit \ Assert = region rc {
        let a: MutList[Int32, rc] = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, a), Array#{} @ rc);
        MutList.sortBy(identity, a);
        let b: MutList[Int32, rc] = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, b), Array#{} @ rc);
        MutList.sortBy(x -> 4 * x + 7, b);
        let expected1 = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, expected1), Array#{} @ rc);
        let expected2 = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, expected2), Array#{} @ rc);
        assertTrue(a `sameElements` expected1);
        assertTrue(b `sameElements` expected2)
    }

    @Test
    def sortBy02(): Unit \ Assert = region rc {
        let a: MutList[Int32, rc] = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, a), Array#{0} @ rc);
        MutList.sortBy(identity, a);
        let b: MutList[Int32, rc] = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, b), Array#{0} @ rc);
        MutList.sortBy(x -> 4 * x + 7, b);
        let expected1 = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, expected1), Array#{0} @ rc);
        let expected2 = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, expected2), Array#{0} @ rc);
        assertTrue(a `sameElements` expected1);
        assertTrue(b `sameElements` expected2)
    }

    @Test
    def sortBy03(): Unit \ Assert = region rc {
        let a: MutList[Int32, rc] = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, a), Array#{0,1} @ rc);
        MutList.sortBy(identity, a);
        let b: MutList[Int32, rc] = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, b), Array#{0,1} @ rc);
        MutList.sortBy(x -> 4 * x + 7, b);
        let expected1 = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, expected1), Array#{0,1} @ rc);
        let expected2 = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, expected2), Array#{0,1} @ rc);
        assertTrue(a `sameElements` expected1);
        assertTrue(b `sameElements` expected2)
    }

    @Test
    def sortBy04(): Unit \ Assert = region rc {
        let a: MutList[Int32, rc] = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, a), Array#{1,0} @ rc);
        MutList.sortBy(identity, a);
        let b: MutList[Int32, rc] = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, b), Array#{1,0} @ rc);
        MutList.sortBy(x -> 4 * x + 7, b);
        let expected1 = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, expected1), Array#{0,1} @ rc);
        let expected2 = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, expected2), Array#{0,1} @ rc);
        assertTrue(a `sameElements` expected1);
        assertTrue(b `sameElements` expected2)
    }

    @Test
    def sortBy05(): Unit \ Assert = region rc {
        let a: MutList[Int32, rc] = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, a), Array#{1,1} @ rc);
        MutList.sortBy(identity, a);
        let b: MutList[Int32, rc] = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, b), Array#{1,1} @ rc);
        MutList.sortBy(x -> 4 * x + 7, b);
        let expected1 = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, expected1), Array#{1,1} @ rc);
        let expected2 = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, expected2), Array#{1,1} @ rc);
        assertTrue(a `sameElements` expected1);
        assertTrue(b `sameElements` expected2)
    }

    @Test
    def sortBy06(): Unit \ Assert = region rc {
        let a: MutList[Int32, rc] = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, a), Array#{0,1,2,3,4,5} @ rc);
        MutList.sortBy(identity, a);
        let b: MutList[Int32, rc] = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, b), Array#{0,1,2,3,4,5} @ rc);
        MutList.sortBy(x -> 4 * x + 7, b);
        let expected1 = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, expected1), Array#{0,1,2,3,4,5} @ rc);
        let expected2 = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, expected2), Array#{0,1,2,3,4,5} @ rc);
        assertTrue(a `sameElements` expected1);
        assertTrue(b `sameElements` expected2)
    }

    @Test
    def sortBy07(): Unit \ Assert = region rc {
        let a: MutList[Int32, rc] = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, a), Array#{5,4,3,2,1,0} @ rc);
        MutList.sortBy(identity, a);
        let b: MutList[Int32, rc] = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, b), Array#{5,4,3,2,1,0} @ rc);
        MutList.sortBy(x -> 4 * x + 7, b);
        let expected1 = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, expected1), Array#{0,1,2,3,4,5} @ rc);
        let expected2 = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, expected2), Array#{0,1,2,3,4,5} @ rc);
        assertTrue(a `sameElements` expected1);
        assertTrue(b `sameElements` expected2)
    }

    @Test
    def sortBy08(): Unit \ Assert = region rc {
        let a: MutList[Int32, rc] = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, a), Array#{5,3,0,4,1,2} @ rc);
        MutList.sortBy(identity, a);
        let b: MutList[Int32, rc] = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, b), Array#{5,3,0,4,1,2} @ rc);
        MutList.sortBy(x -> 4 * x + 7, b);
        let expected1 = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, expected1), Array#{0,1,2,3,4,5} @ rc);
        let expected2 = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, expected2), Array#{0,1,2,3,4,5} @ rc);
        assertTrue(a `sameElements` expected1);
        assertTrue(b `sameElements` expected2)
    }

    @Test
    def sortBy09(): Unit \ Assert = region rc {
        let a: MutList[Int32, rc] = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, a), Array#{2,3,0,4,1,2} @ rc);
        MutList.sortBy(identity, a);
        let b: MutList[Int32, rc] = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, b), Array#{2,3,0,4,1,2} @ rc);
        MutList.sortBy(x -> 4 * x + 7, b);
        let expected1 = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, expected1), Array#{0,1,2,2,3,4} @ rc);
        let expected2 = MutList.empty(rc);
        Array.forEach(x -> MutList.push(x, expected2), Array#{0,1,2,2,3,4} @ rc);
        assertTrue(a `sameElements` expected1);
        assertTrue(b `sameElements` expected2)
    }


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sum01(): Unit \ Assert = region rc {
        assertEq(expected = 0, MutList.empty(rc) |> MutList.sum)
    }

    @Test
    def sum02(): Unit \ Assert = region rc {
        let l = MutList.empty(rc);
        let xs = 1 :: Nil;
        List.forEach(x -> MutList.push(x, l), xs);
        assertEq(expected = 1, MutList.sum(l))
    }

    @Test
    def sum03(): Unit \ Assert = region rc {
        let l = MutList.empty(rc);
        let xs = 1 :: 2 :: 3 :: Nil;
        List.forEach(x -> MutList.push(x, l), xs);
        assertEq(expected = 6, MutList.sum(l))
    }

    @Test
    def sum04(): Unit \ Assert = region rc {
        let l = MutList.empty(rc);
        let xs = 1 :: 2 :: 3 :: -3 :: Nil;
        List.forEach(x -> MutList.push(x, l), xs);
        assertEq(expected = 3, MutList.sum(l))
    }

    @Test
    def sum05(): Unit \ Assert = region rc {
        let l = MutList.empty(rc);
        let xs = -1 :: -2 :: -3 :: -4 :: Nil;
        List.forEach(x -> MutList.push(x, l), xs);
        assertEq(expected = -10, MutList.sum(l))
    }

    @Test
    def sum06(): Unit \ Assert = region rc {
        let l = MutList.empty(rc);
        let xs = 10 :: -10 :: Nil;
        List.forEach(x -> MutList.push(x, l), xs);
        assertEq(expected = 0, MutList.sum(l))
    }

    @Test
    def sum07(): Unit \ Assert = region rc {
        let l = MutList.empty(rc);
        let xs = List.range(1, 101);
        List.forEach(x -> MutList.push(x, l), xs);
        assertEq(expected = 5050, MutList.sum(l))
    }


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumWith01(): Unit \ Assert = region rc {
        assertEq(expected = 0, MutList.empty(rc) |> MutList.sumWith(x -> x + 1))
    }

    @Test
    def sumWith02(): Unit \ Assert = region rc {
        let l = MutList.empty(rc);
        let xs = 1 :: Nil;
        List.forEach(x -> MutList.push(x, l), xs);
        assertEq(expected = 2, MutList.sumWith(x -> x + 1, l))
    }

    @Test
    def sumWith03(): Unit \ Assert = region rc {
        let l = MutList.empty(rc);
        let xs = 1 :: 2 :: 3 :: Nil;
        List.forEach(x -> MutList.push(x, l), xs);
        assertEq(expected = 9, MutList.sumWith(x -> x + 1, l))
    }

    @Test
    def sumWith04(): Unit \ Assert = region rc {
        let l = MutList.empty(rc);
        let xs = 1 :: 2 :: 3 :: -3 :: Nil;
        List.forEach(x -> MutList.push(x, l), xs);
        assertEq(expected = 7, MutList.sumWith(x -> x + 1, l))
    }

    @Test
    def sumWith05(): Unit \ Assert = region rc {
        let l = MutList.empty(rc);
        let xs = -1 :: -2 :: -3 :: -4 :: Nil;
        List.forEach(x -> MutList.push(x, l), xs);
        assertEq(expected = -6, MutList.sumWith(x -> x + 1, l))
    }

    @Test
    def sumWith06(): Unit \ Assert = region rc {
        let l = MutList.empty(rc);
        let xs = 10 :: -10 :: Nil;
        List.forEach(x -> MutList.push(x, l), xs);
        assertEq(expected = 2, MutList.sumWith(x -> x + 1, l))
    }


    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def join01(): Unit \ Assert = region rc {
        assertEq(expected = "", ((MutList.empty(rc): MutList[Int32, _])) |> MutList.join(","))
    }

    @Test
    def join02(): Unit \ Assert = region rc {
        let l = MutList.empty(rc);
        let xs = 1 :: Nil;
        List.forEach(x -> MutList.push(x, l), xs);
        assertEq(expected = "1", MutList.join(",", l))
    }

    @Test
    def join03(): Unit \ Assert = region rc {
        let l = MutList.empty(rc);
        let xs = 1 :: 2 :: 3 :: Nil;
        List.forEach(x -> MutList.push(x, l), xs);
        assertEq(expected = "1,2,3", MutList.join(",", l))
    }

    @Test
    def join04(): Unit \ Assert = region rc {
        let l = MutList.empty(rc);
        let xs = "1" :: "2" :: "3" :: Nil;
        List.forEach(x -> MutList.push(x, l), xs);
        assertEq(expected = "1,2,3", MutList.join(",", l))
    }


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinWith01(): Unit \ Assert = region rc {
        assertEq(expected = "", ((MutList.empty(rc): MutList[Int32, _])) |> MutList.joinWith(x -> "${x + 1}", ","))
    }

    @Test
    def joinWith02(): Unit \ Assert = region rc {
        let l = MutList.empty(rc);
        let xs = 1 :: Nil;
        List.forEach(x -> MutList.push(x, l), xs);
        assertEq(expected = "2", MutList.joinWith(x -> "${x + 1}", ",", l))
    }

    @Test
    def joinWith03(): Unit \ Assert = region rc {
        let l = MutList.empty(rc);
        let xs = 1 :: 2 :: 3 :: Nil;
        List.forEach(x -> MutList.push(x, l), xs);
        assertEq(expected = "2,3,4", MutList.joinWith(x -> "${x + 1}", ",", l))
    }

    @Test
    def joinWith04(): Unit \ Assert = region rc {
        let l = MutList.empty(rc);
        let xs = "1" :: "2" :: "3" :: Nil;
        List.forEach(x -> MutList.push(x, l), xs);
        assertEq(expected = "11,22,33", MutList.joinWith(x -> x + x, ",", l))
    }

    /////////////////////////////////////////////////////////////////////////////
    // shuffle                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def shuffle01(): Unit \ {Assert, NonDet} = region rc {
        run {
            let l: List[Int32] = Nil;
            let ml = MutList.empty(rc);
            List.forEach(x -> MutList.push(x, ml), l);
            let v = MutList.shuffle(rc, ml);
            MutList.sort(v);

            assertEq(expected = 0, MutList.length(v));
            let expected = MutList.empty(rc);
            List.forEach(x -> MutList.push(x, expected), l);
            assertTrue(v `MutList.sameElements` expected)
        } with Shuffle.runWithIO
    }

    @Test
    def shuffle02(): Unit \ {Assert, NonDet} = region rc {
        run {
            let l = 1 :: Nil;
            let ml = MutList.empty(rc);
            List.forEach(x -> MutList.push(x, ml), l);
            let v = MutList.shuffle(rc, ml);
            MutList.sort(v);

            assertEq(expected = 1, MutList.length(v));
            let expected = MutList.empty(rc);
            List.forEach(x -> MutList.push(x, expected), l);
            assertTrue(v `MutList.sameElements` expected)
        } with Shuffle.runWithIO
    }

    @Test
    def shuffle03(): Unit \ {Assert, NonDet} = region rc {
        run {
            let l = 1 :: 2 :: 3 :: Nil;
            let ml = MutList.empty(rc);
            List.forEach(x -> MutList.push(x, ml), l);
            let v = MutList.shuffle(rc, ml);
            MutList.sort(v);

            assertEq(expected = 3, MutList.length(v));
            let expected = MutList.empty(rc);
            List.forEach(x -> MutList.push(x, expected), l);
            assertTrue(v `MutList.sameElements` expected)
        } with Shuffle.runWithIO
    }

    @Test
    def shuffle04(): Unit \ {Assert, NonDet} = region rc {
        run {
            let l = 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: Nil;
            let ml = MutList.empty(rc);
            List.forEach(x -> MutList.push(x, ml), l);
            let v = MutList.shuffle(rc, ml);
            MutList.sort(v);

            assertEq(expected = 10, MutList.length(v));
            let expected = MutList.empty(rc);
            List.forEach(x -> MutList.push(x, expected), l);
            assertTrue(v `MutList.sameElements` expected)
        } with Shuffle.runWithIO
    }

}
