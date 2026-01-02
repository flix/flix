mod TestArray {

    use Array.sameElements;
    use Assert.{assertEq, assertNeq, assertTrue, assertFalse, assertSome, assertNone};

    /////////////////////////////////////////////////////////////////////////////
    // binarySearch                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def binarySearch01(): Unit \ Assert = region rc {
        assertEq(expected = None, Array.binarySearch(0, Array#{} @ rc))
    }

    @Test
    def binarySearch02(): Unit \ Assert = region rc {
        assertEq(expected = Some(0), Array.binarySearch(0, Array#{0} @ rc))
    }

    @Test
    def binarySearch03(): Unit \ Assert = region rc {
        assertEq(expected = Some(0), Array.binarySearch(0, Array#{0, 1, 2, 3} @ rc))
    }

    @Test
    def binarySearch04(): Unit \ Assert = region rc {
        assertEq(expected = Some(1), Array.binarySearch(1, Array#{0, 1, 2, 3} @ rc))
    }

    @Test
    def binarySearch05(): Unit \ Assert = region rc {
        assertEq(expected = Some(2), Array.binarySearch(2, Array#{0, 1, 2, 3} @ rc))
    }

    @Test
    def binarySearch06(): Unit \ Assert = region rc {
        assertEq(expected = Some(3), Array.binarySearch(3, Array#{0, 1, 2, 3} @ rc))
    }

    @Test
    def binarySearch07(): Unit \ Assert = region rc {
        assertEq(expected = Some(1), Array.binarySearch(1, Array#{0, 1} @ rc))
    }

    @Test
    def binarySearch08(): Unit \ Assert = region rc {
        assertEq(expected = Some(1), Array.binarySearch(1, Array#{0, 1, 2} @ rc))
    }

    @Test
    def binarySearch09(): Unit \ Assert = region rc {
        assertEq(expected = Some(1), Array.binarySearch(1, Array#{0, 1, 2, 3, 4} @ rc))
    }

    @Test
    def binarySearch10(): Unit \ Assert = region rc {
        assertEq(expected = Some(1), Array.binarySearch(1, Array#{0, 1, 2, 3, 4, 5} @ rc))
    }

    @Test
    def binarySearch11(): Unit \ Assert = region rc {
        assertEq(expected = Some(1), Array.binarySearch(1, Array#{0, 1, 2, 3, 4, 5, 6} @ rc))
    }

    @Test
    def binarySearch12(): Unit \ Assert = region rc {
        assertEq(expected = Some(1), Array.binarySearch(1, Array#{0, 1, 2, 3, 4, 5, 6, 7} @ rc))
    }

    @Test
    def binarySearch13(): Unit \ Assert = region rc {
        assertEq(expected = Some(1), Array.binarySearch(1, Array#{0, 1, 2, 3, 4, 5, 6, 7, 8} @ rc))
    }

    @Test
    def binarySearch14(): Unit \ Assert = region rc {
        assertEq(expected = Some(1), Array.binarySearch(1, Array#{0, 1, 2, 3, 4, 5, 6, 7, 8, 9} @ rc))
    }

    @Test
    def binarySearch15(): Unit \ Assert = region rc {
        assertEq(expected = Some(2), Array.binarySearch(4, Array#{0, 2, 4, 6, 8, 10} @ rc))
    }

    @Test
    def binarySearch16(): Unit \ Assert = region rc {
        assertEq(expected = None, Array.binarySearch(3, Array#{0, 2, 4, 6, 8, 10} @ rc))
    }

    @Test
    def binarySearch17(): Unit \ Assert = region rc {
        assertEq(expected = None, Array.binarySearch(-1, Array#{0, 2, 4, 6, 8, 10} @ rc))
    }

    @Test
    def binarySearch18(): Unit \ Assert = region rc {
        assertEq(expected = None, Array.binarySearch(11, Array#{0, 2, 4, 6, 8, 10} @ rc))
    }

    @Test
    def binarySearch19(): Unit \ Assert = region rc {
        assertEq(expected = None, Array.binarySearch(11i64, Array#{0i64, 2i64, 4i64, 6i64, 8i64, 10i64} @ rc))
    }

    @Test
    def binarySearch20(): Unit \ Assert = region rc {
        assertEq(expected = Some(1), Array.binarySearch(2i64, Array#{0i64, 2i64, 4i64, 6i64, 8i64, 10i64} @ rc))
    }

    @Test
    def binarySearch21(): Unit \ Assert = region rc {
        assertEq(expected = Some(0), Array.binarySearch("a", Array#{"a", "b", "c", "d", "e", "f"} @ rc))
    }

    @Test
    def binarySearch22(): Unit \ Assert = region rc {
        assertEq(expected = Some(1), Array.binarySearch("b", Array#{"a", "b", "c", "d", "e", "f"} @ rc))
    }

    @Test
    def binarySearch23(): Unit \ Assert = region rc {
        assertEq(expected = None, Array.binarySearch("g", Array#{"a", "b", "c", "d", "e", "f"} @ rc))
    }

    @Test
    def binarySearch24(): Unit \ Assert = region rc {
        let pos = Array.binarySearch(1, Array#{0, 1, 1, 2, 3, 4, 5, 6} @ rc);
        assertTrue(pos == Some(1) or pos == Some(2))
    }

    @Test
    def binarySearch25(): Unit \ Assert = region rc {
        assertEq(expected = Some(0), Array.binarySearch(0, Array#{0, 1, 1, 1, 1, 1, 1, 1, 2} @ rc))
    }

    @Test
    def binarySearch26(): Unit \ Assert = region rc {
        assertEq(expected = None, Array.binarySearch(-1, Array#{0, 1, 1, 1, 1, 1, 1, 1, 2} @ rc))
    }

    @Test
    def binarySearch27(): Unit \ Assert = region rc {
        let pos = Array.binarySearch(4, Array#{4, 3, 2, 1, 0} @ rc);
        assertTrue(pos == None or pos == Some(0))
    }

    @Test
    def binarySearch28(): Unit \ Assert = region rc {
        let pos = Array.binarySearch(3, Array#{4, 3, 2, 1, 0} @ rc);
        assertTrue(pos == None or pos == Some(1))
    }

    @Test
    def binarySearch29(): Unit \ Assert = region rc {
        let pos = Array.binarySearch(2, Array#{4, 3, 2, 1, 0} @ rc);
        assertTrue(pos == None or pos == Some(2))
    }

    @Test
    def binarySearch30(): Unit \ Assert = region rc {
        let pos = Array.binarySearch(1, Array#{4, 3, 2, 1, 0} @ rc);
        assertTrue(pos == None or pos == Some(3))
    }

    @Test
    def binarySearch31(): Unit \ Assert = region rc {
        let pos = Array.binarySearch(0, Array#{4, 3, 2, 1, 0} @ rc);
        assertTrue(pos == None or pos == Some(4))
    }


    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toString01(): Unit \ Assert = region rc {
        assertEq(expected = "Array#{}", Array.toString((Array#{} @ rc: Array[Int32, _])))
    }

    @Test
    def toString02(): Unit \ Assert = region rc {
        assertEq(expected = "Array#{1}", Array.toString((Array#{1} @ rc)))
    }

    @Test
    def toString03(): Unit \ Assert = region rc {
        assertEq(expected = "Array#{1, 2, 3, 4}", Array.toString((Array#{1, 2, 3, 4} @ rc)))
    }

    @Test
    def toString04(): Unit \ Assert = region rc {
        assertEq(expected = "Array#{a, b, c, d}", Array.toString((Array#{'a', 'b', 'c', 'd'} @ rc)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // new                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testEmpty01(): Unit \ Assert = region rc {
        assertEq(expected = 0, Array.get(0, Array.empty(rc, 10)))
    }

    @Test
    def testEmpty02(): Unit \ Assert = region rc {
        assertEq(expected = 0, Array.get(5, Array.empty(rc, 10)))
    }

    @Test
    def testEmpty03(): Unit \ Assert = region rc {
        assertEq(expected = 0, Array.get(9, Array.empty(rc, 10)))
    }

    @Test
    def testEmpty04(): Unit \ Assert = region rc {
        assertEq(expected = false, Array.get(0, Array.empty(rc, 10)))
    }

    @Test
    def testEmpty05(): Unit \ Assert = region rc {
        assertEq(expected = false, Array.get(5, Array.empty(rc, 10)))
    }

    @Test
    def testEmpty06(): Unit \ Assert = region rc {
        assertEq(expected = false, Array.get(9, Array.empty(rc, 10)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // nth                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testNth01(): Unit \ Assert = region rc {
        assertEq(expected = Some(1), Array.nth(0, Array#{1, 2, 3} @ rc))
    }

    @Test
    def testNth02(): Unit \ Assert = region rc {
        assertEq(expected = Some(2), Array.nth(1, Array#{1, 2, 3} @ rc))
    }

    @Test
    def testNth03(): Unit \ Assert = region rc {
        assertEq(expected = Some(3), Array.nth(2, Array#{1, 2, 3} @ rc))
    }

    @Test
    def testNth04(): Unit \ Assert = region rc {
        assertNone(Array.nth(3, Array#{1, 2, 3} @ rc))
    }

    @Test
    def testNth05(): Unit \ Assert = region rc {
        assertNone(Array.nth(-2, Array#{1, 2, 3} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // put                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testPut01(): Unit \ Assert = region rc {
        let a = (Array#{1} @ rc) !> Array.put(42, 0);
        assertEq(expected = 42, Array.get(0, a))
    }

    @Test
    def testPut02(): Unit \ Assert = region rc {
        let a = (Array#{1, 2} @ rc) !> Array.put(42, 1);
        assertEq(expected = 42, Array.get(1, a))
    }

    @Test
    def testPut03(): Unit \ Assert = region rc {
        let a = (Array#{1, 2, 3} @ rc) !> Array.put(42, 2);
        assertEq(expected = 42, Array.get(2, a))
    }

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testIsEmpty01(): Unit \ Assert = region rc {
        assertTrue(Array.isEmpty(Array#{} @ rc))
    }

    @Test
    def testIsEmpty02(): Unit \ Assert = region rc {
        assertFalse(Array.isEmpty(Array#{1} @ rc))
    }

    @Test
    def testIsEmpty03(): Unit \ Assert = region rc {
        assertFalse(Array.isEmpty(Array#{1, 2, 3} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // nonEmpty                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testNonEmpty01(): Unit \ Assert = region rc {
        assertFalse(Array.nonEmpty(Array#{} @ rc))
    }

    @Test
    def testNonEmpty02(): Unit \ Assert = region rc {
        assertTrue(Array.nonEmpty(Array#{1} @ rc))
    }

    @Test
    def testNonEmpty03(): Unit \ Assert = region rc {
        assertTrue(Array.nonEmpty(Array#{1, 2, 3} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testLength01(): Unit \ Assert = region rc {
        assertEq(expected = 0, Array.length(Array#{} @ rc))
    }

    @Test
    def testLength02(): Unit \ Assert = region rc {
        assertEq(expected = 1, Array.length(Array#{1} @ rc))
    }

    @Test
    def testLength03(): Unit \ Assert = region rc {
        assertEq(expected = 3, Array.length(Array#{1, 2, 3} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testSize01(): Unit \ Assert = region rc {
        assertEq(expected = 0, Array.size(Array#{} @ rc))
    }

    @Test
    def testSize02(): Unit \ Assert = region rc {
        assertEq(expected = 1, Array.size(Array#{1} @ rc))
    }

    @Test
    def testSize03(): Unit \ Assert = region rc {
        assertEq(expected = 3, Array.size(Array#{1, 2, 3} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // slice                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testSlice01(): Unit \ Assert = region rc {
        assertEq(expected = 1, Array.get(0, Array.slice(rc, start = 0, end = 1, Array#{1, 2, 3} @ rc)))
    }

    @Test
    def testSlice02(): Unit \ Assert = region rc {
        assertEq(expected = 2, Array.get(0, Array.slice(rc, start = 1, end = 2, Array#{1, 2, 3} @ rc)))
    }

    @Test
    def testSlice03(): Unit \ Assert = region rc {
        assertEq(expected = 3, Array.get(0, Array.slice(rc, start = 2, end = 3, Array#{1, 2, 3} @ rc)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testToList01(): Unit \ Assert = region rc {
        assertEq(expected = Nil, Array.toList((Array#{} @ rc: Array[Unit, _])))
    }

    @Test
    def testToList02(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: Nil, Array.toList(Array#{1} @ rc))
    }

    @Test
    def testToList03(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: 2 :: Nil, Array.toList(Array#{1, 2} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toNel                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toNel01(): Unit \ Assert = region rc {
        assertNone(Array.toNel((Array#{} @ rc: Array[Unit, _])))
    }

    @Test
    def toNel02(): Unit \ Assert = region rc {
        assertEq(expected = Some(Nel.singleton(1)), Array.toNel(Array#{1} @ rc))
    }

    @Test
    def toNel03(): Unit \ Assert = region rc {
        assertEq(expected = List.toNel(1 :: 2 :: Nil), Array.toNel(Array#{1,2} @ rc))
    }

    @Test
    def toNel04(): Unit \ Assert = region rc {
        assertEq(expected = List.toNel(1 :: 2 :: 3 :: Nil), Array.toNel(Array#{1,2,3} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toVector01(): Unit \ Assert = region rc {
        assertTrue(Array.toVector((Array#{} @ rc : Array[Int32, _])) `Vector.equals` Vector.empty())
    }

    @Test
    def toVector02(): Unit \ Assert = region rc {
        assertTrue(Array.toVector((Array#{0} @ rc : Array[Int32, _])) `Vector.equals` Vector.singleton(0))
    }

    @Test
    def toVector03(): Unit \ Assert = region rc {
        assertTrue(Array.toVector((Array#{1, 2, 3} @ rc : Array[Int32, _])) `Vector.equals` List.toVector(1 :: 2 :: 3 :: Nil))
    }

    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def head01(): Unit \ Assert = region rc {
        assertNone(Array.head((Array#{} @ rc: Array[Int32, _])))
    }

    @Test
    def head02(): Unit \ Assert = region rc {
        assertEq(expected = Some(1), Array.head(Array#{1} @ rc))
    }

    @Test
    def head03(): Unit \ Assert = region rc {
        assertEq(expected = Some(2), Array.head(Array#{2,1} @ rc))
    }

    @Test
    def head04(): Unit \ Assert = region rc {
        assertEq(expected = Some(3), Array.head(Array#{3,2,1} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def last01(): Unit \ Assert = region rc {
        assertNone((Array.last(Array#{} @ rc): Option[Unit]))
    }

    @Test
    def last02(): Unit \ Assert = region rc {
        assertEq(expected = Some(1), Array.last(Array#{1} @ rc))
    }

    @Test
    def last03(): Unit \ Assert = region rc {
        assertEq(expected = Some(2), Array.last(Array#{1,2} @ rc))
    }

    @Test
    def last04(): Unit \ Assert = region rc {
        assertEq(expected = Some(3), Array.last(Array#{1,2,3} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def append01(): Unit \ Assert = region rc {
        let a = Array.append(rc, ((Array#{} @ rc): Array[Int32, _]), ((Array#{} @ rc): Array[Int32, _]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def append02(): Unit \ Assert = region rc {
        let a = Array.append(rc, ((Array#{} @ rc): Array[Int32, _]), Array#{1} @ rc);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def append03(): Unit \ Assert = region rc {
        let a = Array.append(rc, ((Array#{} @ rc): Array[Int32, _]), Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc))
    }

    @Test
    def append04(): Unit \ Assert = region rc {
        let a = Array.append(rc, Array#{1} @ rc, ((Array#{} @ rc): Array[Int32, _]));
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def append05(): Unit \ Assert = region rc {
        let a = Array.append(rc, Array#{1,2} @ rc, ((Array#{} @ rc): Array[Int32, _]));
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc))
    }

    @Test
    def append06(): Unit \ Assert = region rc {
        let a = Array.append(rc, Array#{1} @ rc, Array#{2} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc))
    }

    @Test
    def append07(): Unit \ Assert = region rc {
        let a = Array.append(rc, Array#{1,2} @ rc, Array#{3} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2,3} @ rc))
    }

    @Test
    def append08(): Unit \ Assert = region rc {
        let a = Array.append(rc, Array#{1} @ rc, Array#{2,3} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2,3} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def memberOf01(): Unit \ Assert = region rc {
        assertFalse(Array.memberOf(0, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def memberOf02(): Unit \ Assert = region rc {
        assertFalse(Array.memberOf(0, Array#{1} @ rc))
    }

    @Test
    def memberOf03(): Unit \ Assert = region rc {
        assertTrue(Array.memberOf(0, Array#{0} @ rc))
    }

    @Test
    def memberOf04(): Unit \ Assert = region rc {
        assertFalse(Array.memberOf(0, Array#{1,2} @ rc))
    }

    @Test
    def memberOf05(): Unit \ Assert = region rc {
        assertTrue(Array.memberOf(1, Array#{1,2} @ rc))
    }

    @Test
    def memberOf06(): Unit \ Assert = region rc {
        assertTrue(Array.memberOf(2, Array#{1,2} @ rc))
    }

    @Test
    def memberOf07(): Unit \ Assert = region rc {
        assertFalse(Array.memberOf(3, Array#{1,2} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // indexOf                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indexOf01(): Unit \ Assert = region rc {
        assertNone(Array.indexOf(0, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def indexOf02(): Unit \ Assert = region rc {
        assertNone(Array.indexOf(0, Array#{1} @ rc))
    }

    @Test
    def indexOf03(): Unit \ Assert = region rc {
        assertEq(expected = Some(0), Array.indexOf(1, Array#{1} @ rc))
    }

    @Test
    def indexOf04(): Unit \ Assert = region rc {
        assertNone(Array.indexOf(0, Array#{1,2} @ rc))
    }

    @Test
    def indexOf05(): Unit \ Assert = region rc {
        assertEq(expected = Some(0), Array.indexOf(1, Array#{1,2} @ rc))
    }

    @Test
    def indexOf06(): Unit \ Assert = region rc {
        assertEq(expected = Some(1), Array.indexOf(2, Array#{1,2} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // indexOfLeft                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indexOfLeft01(): Unit \ Assert = region rc {
        assertNone(Array.indexOfLeft(0, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def indexOfLeft02(): Unit \ Assert = region rc {
        assertNone(Array.indexOfLeft(0, Array#{1} @ rc))
    }

    @Test
    def indexOfLeft03(): Unit \ Assert = region rc {
        assertEq(expected = Some(0), Array.indexOfLeft(1, Array#{1} @ rc))
    }

    @Test
    def indexOfLeft04(): Unit \ Assert = region rc {
        assertNone(Array.indexOfLeft(0, Array#{1,2} @ rc))
    }

    @Test
    def indexOfLeft05(): Unit \ Assert = region rc {
        assertEq(expected = Some(0), Array.indexOfLeft(1, Array#{1,2} @ rc))
    }

    @Test
    def indexOfLeft06(): Unit \ Assert = region rc {
        assertEq(expected = Some(1), Array.indexOfLeft(2, Array#{1,2} @ rc))
    }

    @Test
    def indexOfLeft07(): Unit \ Assert = region rc {
        assertEq(expected = Some(0), Array.indexOfLeft(1, Array#{1,1} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // indexOfRight                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indexOfRight01(): Unit \ Assert = region rc {
        assertNone(Array.indexOfRight(0, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def indexOfRight02(): Unit \ Assert = region rc {
        assertNone(Array.indexOfRight(0, Array#{1} @ rc))
    }

    @Test
    def indexOfRight03(): Unit \ Assert = region rc {
        assertEq(expected = Some(0), Array.indexOfRight(1, Array#{1} @ rc))
    }

    @Test
    def indexOfRight04(): Unit \ Assert = region rc {
        assertNone(Array.indexOfRight(0, Array#{1,2} @ rc))
    }

    @Test
    def indexOfRight05(): Unit \ Assert = region rc {
        assertEq(expected = Some(0), Array.indexOfRight(1, Array#{1,2} @ rc))
    }

    @Test
    def indexOfRight06(): Unit \ Assert = region rc {
        assertEq(expected = Some(1), Array.indexOfRight(2, Array#{1,2} @ rc))
    }

    @Test
    def indexOfRight07(): Unit \ Assert = region rc {
        assertEq(expected = Some(1), Array.indexOfRight(1, Array#{1,1} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // indicesOf                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indicesOf01(): Unit \ Assert = region rc {
        let a = Array.indicesOf(0, ((Array#{} @ rc): Array[Int32, rc]));
        assertEq(expected = Vector#{}, a)
    }

    @Test
    def indicesOf02(): Unit \ Assert = region rc {
        let a = Array.indicesOf(0, Array#{1} @ rc);
        assertEq(expected = Vector#{}, a)
    }

    @Test
    def indicesOf03(): Unit \ Assert = region rc {
        let a = Array.indicesOf(1, Array#{1} @ rc);
        assertEq(expected = Vector#{0}, a)
    }

    @Test
    def indicesOf04(): Unit \ Assert = region rc {
        let a = Array.indicesOf(0, Array#{1,2} @ rc);
        assertEq(expected = Vector#{}, a)
    }

    @Test
    def indicesOf05(): Unit \ Assert = region rc {
        let a = Array.indicesOf(1, Array#{1,2} @ rc);
        assertEq(expected = Vector#{0}, a)
    }

    @Test
    def indicesOf06(): Unit \ Assert = region rc {
        let a = Array.indicesOf(2, Array#{1,2} @ rc);
        assertEq(expected = Vector#{1}, a)
    }

    @Test
    def indicesOf07(): Unit \ Assert = region rc {
        let a = Array.indicesOf(1, Array#{1,1} @ rc);
        assertEq(expected = Vector#{0,1}, a)
    }

    /////////////////////////////////////////////////////////////////////////////
    // indices                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indices01(): Unit \ Assert = region rc {
        assertEq(expected = Range.Range(0, 0), Array.indices(Array#{} @ rc))
    }

    @Test
    def indices02(): Unit \ Assert = region rc {
        assertEq(expected = Range.Range(0, 3), Array.indices(Array#{1, 2, 3} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // find                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def find01(): Unit \ Assert = region rc {
        assertNone(Array.find(i -> i > 2, Array#{} @ rc))
    }

    @Test
    def find02(): Unit \ Assert = region rc {
        assertNone(Array.find(i -> i > 2, Array#{1} @ rc))
    }

    @Test
    def find03(): Unit \ Assert = region rc {
        assertEq(expected = Some(3), Array.find(i -> i > 2, Array#{3} @ rc))
    }

    @Test
    def find04(): Unit \ Assert = region rc {
        assertNone(Array.find(i -> i > 2, Array#{1,2} @ rc))
    }

    @Test
    def find05(): Unit \ Assert = region rc {
        assertEq(expected = Some(6), Array.find(i -> i > 2, Array#{6,-6} @ rc))
    }

    @Test
    def find06(): Unit \ Assert = region rc {
        assertEq(expected = Some(6), Array.find(i -> i > 2, Array#{-6,6} @ rc))
    }

    @Test
    def find07(): Unit \ Assert = region rc {
        assertEq(expected = Some(6), Array.find(i -> i > 2, Array#{6,7} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findLeft01(): Unit \ Assert = region rc {
        assertNone(Array.findLeft(i -> i > 2, Array#{} @ rc))
    }

    @Test
    def findLeft02(): Unit \ Assert = region rc {
        assertNone(Array.findLeft(i -> i > 2, Array#{1} @ rc))
    }

    @Test
    def findLeft03(): Unit \ Assert = region rc {
        assertEq(expected = Some(3), Array.findLeft(i -> i > 2, Array#{3} @ rc))
    }

    @Test
    def findLeft04(): Unit \ Assert = region rc {
        assertNone(Array.findLeft(i -> i > 2, Array#{1,2} @ rc))
    }

    @Test
    def findLeft05(): Unit \ Assert = region rc {
        assertEq(expected = Some(6), Array.findLeft(i -> i > 2, Array#{6,-6} @ rc))
    }

    @Test
    def findLeft06(): Unit \ Assert = region rc {
        assertEq(expected = Some(6), Array.findLeft(i -> i > 2, Array#{-6,6} @ rc))
    }

    @Test
    def findLeft07(): Unit \ Assert = region rc {
        assertEq(expected = Some(6), Array.findLeft(i -> i > 2, Array#{6,7} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findRight01(): Unit \ Assert = region rc {
        assertNone(Array.findRight(i -> i > 2, Array#{} @ rc))
    }

    @Test
    def findRight02(): Unit \ Assert = region rc {
        assertNone(Array.findRight(i -> i > 2, Array#{1} @ rc))
    }

    @Test
    def findRight03(): Unit \ Assert = region rc {
        assertEq(expected = Some(3), Array.findRight(i -> i > 2, Array#{3} @ rc))
    }

    @Test
    def findRight04(): Unit \ Assert = region rc {
        assertNone(Array.findRight(i -> i > 2, Array#{1,2} @ rc))
    }

    @Test
    def findRight05(): Unit \ Assert = region rc {
        assertEq(expected = Some(6), Array.findRight(i -> i > 2, Array#{6,-6} @ rc))
    }

    @Test
    def findRight06(): Unit \ Assert = region rc {
        assertEq(expected = Some(6), Array.findRight(i -> i > 2, Array#{-6,6} @ rc))
    }

    @Test
    def findRight07(): Unit \ Assert = region rc {
        assertEq(expected = Some(7), Array.findRight(i -> i > 2, Array#{6,7} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def range01(): Unit \ Assert = region rc {
        let a = Array.range(rc, 1, 0);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def range02(): Unit \ Assert = region rc {
        let a = Array.range(rc, 1, 1);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def range03(): Unit \ Assert = region rc {
        let a = Array.range(rc, 1, 2);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def range04(): Unit \ Assert = region rc {
        let a = Array.range(rc, 1, 3);
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc))
    }

    @Test
    def range05(): Unit \ Assert = region rc {
        let a = Array.range(rc, 1, 4);
        assertTrue(Array.sameElements(a, Array#{1,2,3} @ rc))
    }

    @Test
    def range06(): Unit \ Assert = region rc {
        let a = Array.range(rc, -1, 3);
        assertTrue(Array.sameElements(a, Array#{-1,0,1,2} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // repeat rc,
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def repeat01(): Unit \ Assert = region rc {
        let a = Array.repeat(rc, -1, 1);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def repeat02(): Unit \ Assert = region rc {
        let a = Array.repeat(rc, 0, 1);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def repeat03(): Unit \ Assert = region rc {
        let a = Array.repeat(rc, 1, 1);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def repeat04(): Unit \ Assert = region rc {
        let a = Array.repeat(rc, 2, 1);
        assertTrue(Array.sameElements(a, Array#{1,1} @ rc))
    }

    @Test
    def repeat05(): Unit \ Assert = region rc {
        let a = Array.repeat(rc, 3, 1);
        assertTrue(Array.sameElements(a, Array#{1,1,1} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // scan                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def scan01(): Unit \ Assert = region rc {
        let a = Array.scan(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{} @ rc);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def scan02(): Unit \ Assert = region rc {
        let a = Array.scan(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{false} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,3} @ rc))
    }

    @Test
    def scan03(): Unit \ Assert = region rc {
        let a = Array.scan(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{true} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc))
    }

    @Test
    def scan04(): Unit \ Assert = region rc {
        let a = Array.scan(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{false,false} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,3,5} @ rc))
    }

    @Test
    def scan05(): Unit \ Assert = region rc {
        let a = Array.scan(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{false,true} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,3,4} @ rc))
    }

    @Test
    def scan06(): Unit \ Assert = region rc {
        let a = Array.scan(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{true,false} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2,4} @ rc))
    }

    @Test
    def scan07(): Unit \ Assert = region rc {
        let a = Array.scan(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{true,true} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2,3} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // scanLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def scanLeft01(): Unit \ Assert = region rc {
        let a = Array.scanLeft(rc, (i, b) -> if (b) i + 1 else i + 2, 1, ((Array#{} @ rc): Array[Bool, _]));
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def scanLeft02(): Unit \ Assert = region rc {
        let a = Array.scanLeft(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{false} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,3} @ rc))
    }

    @Test
    def scanLeft03(): Unit \ Assert = region rc {
        let a = Array.scanLeft(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{true} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc))
    }

    @Test
    def scanLeft04(): Unit \ Assert = region rc {
        let a = Array.scanLeft(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{false,false} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,3,5} @ rc))
    }

    @Test
    def scanLeft05(): Unit \ Assert = region rc {
        let a = Array.scanLeft(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{false,true} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,3,4} @ rc))
    }

    @Test
    def scanLeft06(): Unit \ Assert = region rc {
        let a = Array.scanLeft(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{true,false} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2,4} @ rc))
    }

    @Test
    def scanLeft07(): Unit \ Assert = region rc {
        let a = Array.scanLeft(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{true,true} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2,3} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // scanRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def scanRight01(): Unit \ Assert = region rc {
        let a = Array.scanRight(rc, (b, i) -> if (b) i + 1 else i + 2, 1, Array#{} @ rc);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def scanRight02(): Unit \ Assert = region rc {
        let a = Array.scanRight(rc, (b, i) -> if (b) i + 1 else i + 2, 1, Array#{false} @ rc);
        assertTrue(Array.sameElements(a, Array#{3,1} @ rc))
    }

    @Test
    def scanRight03(): Unit \ Assert = region rc {
        let a = Array.scanRight(rc, (b, i) -> if (b) i + 1 else i + 2, 1, Array#{true} @ rc);
        assertTrue(Array.sameElements(a, Array#{2,1} @ rc))
    }

    @Test
    def scanRight04(): Unit \ Assert = region rc {
        let a = Array.scanRight(rc, (b, i) -> if (b) i + 1 else i + 2, 1, Array#{false,false} @ rc);
        assertTrue(Array.sameElements(a, Array#{5,3,1} @ rc))
    }

    @Test
    def scanRight05(): Unit \ Assert = region rc {
        let a = Array.scanRight(rc, (b, i) -> if (b) i + 1 else i + 2, 1, Array#{false,true} @ rc);
        assertTrue(Array.sameElements(a, Array#{4,2,1} @ rc))
    }

    @Test
    def scanRight06(): Unit \ Assert = region rc {
        let a = Array.scanRight(rc, (b, i) -> if (b) i + 1 else i + 2, 1, Array#{true,false} @ rc);
        assertTrue(Array.sameElements(a, Array#{4,3,1} @ rc))
    }

    @Test
    def scanRight07(): Unit \ Assert = region rc {
        let a = Array.scanRight(rc, (b, i) -> if (b) i + 1 else i + 2, 1, Array#{true,true} @ rc);
        assertTrue(Array.sameElements(a, Array#{3,2,1} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map01(): Unit \ Assert = region rc {
        let a = Array.map(rc, i -> i > 2, Array#{} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Bool, _])))
    }

    @Test
    def map02(): Unit \ Assert = region rc {
        let a = Array.map(rc, i -> i > 2, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, Array#{false} @ rc))
    }

    @Test
    def map03(): Unit \ Assert = region rc {
        let a = Array.map(rc, i -> i > 2, Array#{3} @ rc);
        assertTrue(Array.sameElements(a, Array#{true} @ rc))
    }

    @Test
    def map04(): Unit \ Assert = region rc {
        let a = Array.map(rc, i -> i > 2, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, Array#{false,false} @ rc))
    }

    @Test
    def map05(): Unit \ Assert = region rc {
        let a = Array.map(rc, i -> i > 2, Array#{1,8} @ rc);
        assertTrue(Array.sameElements(a, Array#{false, true} @ rc))
    }

    @Test
    def map06(): Unit \ Assert = region rc {
        let a = Array.map(rc, i -> i > 2, Array#{8,1} @ rc);
        assertTrue(Array.sameElements(a, Array#{true,false} @ rc))
    }

    @Test
    def map07(): Unit \ Assert = region rc {
        let a = Array.map(rc, i -> i > 2, Array#{7,8} @ rc);
        assertTrue(Array.sameElements(a, Array#{true,true} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // transform                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def transform01(): Unit \ Assert = region rc {
        let a: Array[Int32, _] = Array#{} @ rc;
        Array.transform(x -> x+1, a);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def transform02(): Unit \ Assert = region rc {
        let a = Array#{0} @ rc;
        Array.transform(x -> x+1, a);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def transform03(): Unit \ Assert = region rc {
        let a = Array#{0,1} @ rc;
        Array.transform(x -> x+1, a);
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc))
    }

    @Test
    def transform04(): Unit \ Assert = region rc {
        let a = Array#{0,1,2} @ rc;
        Array.transform(x -> x+1, a);
        assertTrue(Array.sameElements(a, Array#{1,2,3} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapWithIndex01(): Unit \ Assert = region rc {
        let a = Array.mapWithIndex(rc, (i, e) -> if (i < 1) e > 2 else e <= 2, Array#{} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Bool, _])))
    }

    @Test
    def mapWithIndex02(): Unit \ Assert = region rc {
        let a = Array.mapWithIndex(rc, (i, e) -> if (i < 1) e > 2 else e <= 2, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, Array#{false} @ rc))
    }

    @Test
    def mapWithIndex03(): Unit \ Assert = region rc {
        let a = Array.mapWithIndex(rc, (i, e) -> if (i < 1) e > 2 else e <= 2, Array#{3} @ rc);
        assertTrue(Array.sameElements(a, Array#{true} @ rc))
    }

    @Test
    def mapWithIndex04(): Unit \ Assert = region rc {
        let a = Array.mapWithIndex(rc, (i, e) -> if (i < 1) e > 2 else e <= 2, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, Array#{false,true} @ rc))
    }

    @Test
    def mapWithIndex05(): Unit \ Assert = region rc {
        let a = Array.mapWithIndex(rc, (i, e) -> if (i < 1) e > 2 else e <= 2, Array#{1,8} @ rc);
        assertTrue(Array.sameElements(a, Array#{false,false} @ rc))
    }

    @Test
    def mapWithIndex06(): Unit \ Assert = region rc {
        let a = Array.mapWithIndex(rc, (i, e) -> if (i < 1) e > 2 else e <= 2, Array#{8,1} @ rc);
        assertTrue(Array.sameElements(a, Array#{true,true} @ rc))
    }

    @Test
    def mapWithIndex07(): Unit \ Assert = region rc {
        let a = Array.mapWithIndex(rc, (i, e) -> if (i < 1) e > 2 else e <= 2, Array#{7,8} @ rc);
        assertTrue(Array.sameElements(a, Array#{true,false} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // transformWithIndex                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def transformWithIndex01(): Unit \ Assert = region rc {
        let a = ((Array#{} @ rc): Array[Int32, _]);
        Array.transformWithIndex((i, e) -> if (i < 1) e + 10 else e, a);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def transformWithIndex02(): Unit \ Assert = region rc {
        let a = Array#{1} @ rc;
        Array.transformWithIndex((i, e) -> if (i < 1) e + 10 else e, a);
        assertTrue(Array.sameElements(a, Array#{11} @ rc))
    }

    @Test
    def transformWithIndex03(): Unit \ Assert = region rc {
        let a = Array#{3} @ rc;
        Array.transformWithIndex((i, e) -> if (i < 1) e + 10 else e, a);
        assertTrue(Array.sameElements(a, Array#{13} @ rc))
    }

    @Test
    def transformWithIndex04(): Unit \ Assert = region rc {
        let a = Array#{1,2} @ rc;
        Array.transformWithIndex((i, e) -> if (i < 1) e + 10 else e, a);
        assertTrue(Array.sameElements(a, Array#{11,2} @ rc))
    }

    @Test
    def transformWithIndex05(): Unit \ Assert = region rc {
        let a = Array#{1,8} @ rc;
        Array.transformWithIndex((i, e) -> if (i < 1) e + 10 else e, a);
        assertTrue(Array.sameElements(a, Array#{11,8} @ rc))
    }

    @Test
    def transformWithIndex06(): Unit \ Assert = region rc {
        let a = Array#{8,1} @ rc;
        Array.transformWithIndex((i, e) -> if (i < 1) e + 10 else e, a);
        assertTrue(Array.sameElements(a, Array#{18,1} @ rc))
    }

    @Test
    def transformWithIndex07(): Unit \ Assert = region rc {
        let a = Array#{7,8,9} @ rc;
        Array.transformWithIndex((i, e) -> if (i < 1) e + 10 else e, a);
        assertTrue(Array.sameElements(a, Array#{17,8,9} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // flatMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    // depends on flatten

    @Test
    def flatMap01(): Unit \ Assert = region rc {
        let a = Array.flatMap(rc, i -> Array.repeat(rc, i, i), Array#{} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def flatMap02(): Unit \ Assert = region rc {
        let a = Array.flatMap(rc, i -> Array.repeat(rc, i, i), Array#{0} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def flatMap03(): Unit \ Assert = region rc {
        let a = Array.flatMap(rc, i -> Array.repeat(rc, i, i), Array#{1} @ rc);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def flatMap04(): Unit \ Assert = region rc {
        let a = Array.flatMap(rc, i -> Array.repeat(rc, i, i), Array#{2} @ rc);
        assertTrue(Array.sameElements(a, Array#{2,2} @ rc))
    }

    @Test
    def flatMap05(): Unit \ Assert = region rc {
        let a = Array.flatMap(rc, i -> Array.repeat(rc, i, i), Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2,2} @ rc))
    }

    @Test
    def flatMap06(): Unit \ Assert = region rc {
        let a = Array.flatMap(rc, i -> Array.repeat(rc, i, i), Array#{2,3} @ rc);
        assertTrue(Array.sameElements(a, Array#{2,2,3,3,3} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reverse01(): Unit \ Assert = region rc {
        let a: Array[Int32, _] = Array#{} @ rc;
        Array.reverse(a);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def reverse02(): Unit \ Assert = region rc {
        let a = Array#{1} @ rc;
        Array.reverse(a);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def reverse03(): Unit \ Assert = region rc {
        let a = Array#{1,2} @ rc;
        Array.reverse(a);
        assertTrue(Array.sameElements(a, Array#{2,1} @ rc))
    }

    @Test
    def reverse04(): Unit \ Assert = region rc {
        let a = Array#{1,1} @ rc;
        Array.reverse(a);
        assertTrue(Array.sameElements(a, Array#{1,1} @ rc))
    }

    @Test
    def reverse05(): Unit \ Assert = region rc {
        let a = Array#{1,2,3} @ rc;
        Array.reverse(a);
        assertTrue(Array.sameElements(a, Array#{3,2,1} @ rc))
    }

    @Test
    def reverse06(): Unit \ Assert = region rc {
        let a = Array#{1,2,3,4} @ rc;
        Array.reverse(a);
        assertTrue(Array.sameElements(a, Array#{4,3,2,1} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // rotateLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def rotateLeft01(): Unit \ Assert = region rc {
        let a = Array.rotateLeft(rc, 0, ((Array#{} @ rc): Array[Int32, _]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def rotateLeft02(): Unit \ Assert = region rc {
        let a = Array.rotateLeft(rc, 1, ((Array#{} @ rc): Array[Int32, _]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def rotateLeft03(): Unit \ Assert = region rc {
        let a = Array.rotateLeft(rc, 0, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def rotateLeft04(): Unit \ Assert = region rc {
        let a = Array.rotateLeft(rc, 0, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc))
    }

    @Test
    def rotateLeft05(): Unit \ Assert = region rc {
        let a = Array.rotateLeft(rc, 1, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, Array#{2,1} @ rc))
    }

    @Test
    def rotateLeft06(): Unit \ Assert = region rc {
        let a = Array.rotateLeft(rc, 2, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc))
    }

    @Test
    def rotateLeft07(): Unit \ Assert = region rc {
        let a = Array.rotateLeft(rc, 3, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, Array#{2,1} @ rc))
    }

    @Test
    def rotateLeft08(): Unit \ Assert = region rc {
        let a = Array.rotateLeft(rc, -1, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, Array#{2,1} @ rc))
    }

    @Test
    def rotateLeft09(): Unit \ Assert = region rc {
        let a = Array.rotateLeft(rc, 0, Array#{1,2,3} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2,3} @ rc))
    }

    @Test
    def rotateLeft10(): Unit \ Assert = region rc {
        let a = Array.rotateLeft(rc, 1, Array#{1,2,3} @ rc);
        assertTrue(Array.sameElements(a, Array#{2,3,1} @ rc))
    }

    @Test
    def rotateLeft11(): Unit \ Assert = region rc {
        let a = Array.rotateLeft(rc, 2, Array#{1,2,3} @ rc);
        assertTrue(Array.sameElements(a, Array#{3,1,2} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // rotateRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def rotateRight01(): Unit \ Assert = region rc {
        let a = Array.rotateRight(rc, 0, ((Array#{} @ rc): Array[Int32, _]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def rotateRight02(): Unit \ Assert = region rc {
        let a = Array.rotateRight(rc, 1, ((Array#{} @ rc): Array[Int32, _]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def rotateRight03(): Unit \ Assert = region rc {
        let a = Array.rotateRight(rc, 0, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def rotateRight04(): Unit \ Assert = region rc {
        let a = Array.rotateRight(rc, 0, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc))
    }

    @Test
    def rotateRight05(): Unit \ Assert = region rc {
        let a = Array.rotateRight(rc, 1, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, Array#{2,1} @ rc))
    }

    @Test
    def rotateRight06(): Unit \ Assert = region rc {
        let a = Array.rotateRight(rc, 2, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc))
    }

    @Test
    def rotateRight07(): Unit \ Assert = region rc {
        let a = Array.rotateRight(rc, 3, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, Array#{2,1} @ rc))
    }

    @Test
    def rotateRight08(): Unit \ Assert = region rc {
        let a = Array.rotateRight(rc, -1, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, Array#{2,1} @ rc))
    }

    @Test
    def rotateRight09(): Unit \ Assert = region rc {
        let a = Array.rotateRight(rc, 0, Array#{1,2,3} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2,3} @ rc))
    }

    @Test
    def rotateRight10(): Unit \ Assert = region rc {
        let a = Array.rotateRight(rc, 1, Array#{1,2,3} @ rc);
        assertTrue(Array.sameElements(a, Array#{3,1,2} @ rc))
    }

    @Test
    def rotateRight11(): Unit \ Assert = region rc {
        let a = Array.rotateRight(rc, 2, Array#{1,2,3} @ rc);
        assertTrue(Array.sameElements(a, Array#{2,3,1} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // update                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def update01(): Unit \ Assert = region rc {
        let a = Array.update(rc, 0, 2, ((Array#{} @ rc): Array[Int32, _]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def update02(): Unit \ Assert = region rc {
        let a = Array.update(rc, -1, 2, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def update03(): Unit \ Assert = region rc {
        let a = Array.update(rc, 0, 2, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, Array#{2} @ rc))
    }

    @Test
    def update04(): Unit \ Assert = region rc {
        let a = Array.update(rc, 1, 2, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def update05(): Unit \ Assert = region rc {
        let a = Array.update(rc, 0, 5, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, Array#{5,2} @ rc))
    }

    @Test
    def update06(): Unit \ Assert = region rc {
        let a = Array.update(rc, 1, 5, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,5} @ rc))
    }

    @Test
    def update07(): Unit \ Assert = region rc {
        let a = Array.update(rc, 2, 5, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def replace01(): Unit \ Assert = region rc {
        let a: Array[Int32, _] = Array#{} @ rc;
        Array.replace(src = 3, dst =  4, a);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def replace02(): Unit \ Assert = region rc {
        let a = Array#{1} @ rc;
        Array.replace(src = 3, dst =  4, a);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def replace03(): Unit \ Assert = region rc {
        let a = Array#{3} @ rc;
        Array.replace(src = 3, dst =  4, a);
        assertTrue(Array.sameElements(a, Array#{4} @ rc))
    }

    @Test
    def replace04(): Unit \ Assert = region rc {
        let a = Array#{4} @ rc;
        Array.replace(src = 3, dst =  4, a);
        assertTrue(Array.sameElements(a, Array#{4} @ rc))
    }

    @Test
    def replace05(): Unit \ Assert = region rc {
        let a = Array#{1, 2} @ rc;
        Array.replace(src = 3, dst =  4, a);
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc))
    }

    @Test
    def replace06(): Unit \ Assert = region rc {
        let a = Array#{1,3} @ rc;
        Array.replace(src = 3, dst =  4, a);
        assertTrue(Array.sameElements(a, Array#{1,4} @ rc))
    }

    @Test
    def replace07(): Unit \ Assert = region rc {
        let a = Array#{3,4} @ rc;
        Array.replace(src = 3, dst =  4, a);
        assertTrue(Array.sameElements(a, Array#{4,4} @ rc))
    }

    @Test
    def replace08(): Unit \ Assert = region rc {
        let a = Array#{3,3} @ rc;
        Array.replace(src = 3, dst =  4, a);
        assertTrue(Array.sameElements(a, Array#{4,4} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // patch                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def patch01(): Unit \ Assert = region rc {
        let a = ((Array#{} @ rc): Array[Int32, _]);
        Array.patch(0, 0, ((Array#{} @ rc): Array[Int32, _]), a);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def patch02(): Unit \ Assert = region rc {
        let a = ((Array#{} @ rc): Array[Int32, _]);
        Array.patch(0, 2, Array#{1,2} @ rc, a);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def patch03(): Unit \ Assert = region rc {
        let a = Array#{1,2} @ rc;
        Array.patch(0, 2, Array#{} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc))
    }

    @Test
    def patch04(): Unit \ Assert = region rc {
        let a = Array#{1,2} @ rc;
        Array.patch(-3, 3, Array#{1,2,4} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc))
    }

    @Test
    def patch05(): Unit \ Assert = region rc {
        let a = Array#{1,2} @ rc;
        Array.patch(2, 3, Array#{1,2,4} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc))
    }

    @Test
    def patch06(): Unit \ Assert = region rc {
        let a = Array#{1} @ rc;
        Array.patch(0, 0, Array#{} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def patch07(): Unit \ Assert = region rc {
        let a = Array#{1} @ rc;
        Array.patch(1, 0, Array#{2} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def patch08(): Unit \ Assert = region rc {
        let a = Array#{1} @ rc;
        Array.patch(0, 1, Array#{2} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{2} @ rc))
    }

    @Test
    def patch09(): Unit \ Assert = region rc {
        let a = Array#{1} @ rc;
        Array.patch(0, 2, Array#{2,4} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{2} @ rc))
    }

    @Test
    def patch10(): Unit \ Assert = region rc {
        let a = Array#{1} @ rc;
        Array.patch(-1, 2, Array#{2,4} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{4} @ rc))
    }

    @Test
    def patch11(): Unit \ Assert = region rc {
        let a = Array#{1,2} @ rc;
        Array.patch(-1, 2, Array#{3,4} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{4,2} @ rc))
    }

    @Test
    def patch12(): Unit \ Assert = region rc {
        let a = Array#{1,2} @ rc;
        Array.patch(1, 2, Array#{3,4} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{1,3} @ rc))
    }

    @Test
    def patch13(): Unit \ Assert = region rc {
        let a = Array#{1,2} @ rc;
        Array.patch(-2, 2, Array#{3,4} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc))
    }

    @Test
    def patch14(): Unit \ Assert = region rc {
        let a = Array#{1,2} @ rc;
        Array.patch(2, 2, Array#{3,4} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc))
    }

    @Test
    def patch15(): Unit \ Assert = region rc {
        let a = Array#{1,2} @ rc;
        Array.patch(1, 1, Array#{3} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{1,3} @ rc))
    }

    @Test
    def patch16(): Unit \ Assert = region rc {
        let a = Array#{1,2} @ rc;
        Array.patch(0, 2, Array#{3,4} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{3,4} @ rc))
    }

    @Test
    def patch17(): Unit \ Assert = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.patch(0, 1, Array#{4} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{4, 2, 3} @ rc))
    }

    @Test
    def patch18(): Unit \ Assert = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.patch(1, 1, Array#{4} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{1, 4, 3} @ rc))
    }

    @Test
    def patch19(): Unit \ Assert = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.patch(2, 1, Array#{4} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{1, 2, 4} @ rc))
    }

    @Test
    def patch20(): Unit \ Assert = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.patch(0, 2, Array#{4, 5} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{4, 5, 3} @ rc))
    }

    @Test
    def patch21(): Unit \ Assert = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.patch(1, 2, Array#{4, 5} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{1, 4, 5} @ rc))
    }

    @Test
    def patch22(): Unit \ Assert = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.patch(0, 2, Array#{4, 5, 6} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{4, 5, 3} @ rc))
    }

    @Test
    def patch23(): Unit \ Assert = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.patch(0, 3, Array#{4, 5, 6} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{4, 5, 6} @ rc))
    }

    @Test
    def patch24(): Unit \ Assert = region rc {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ rc;
        Array.patch(2, 4, Array#{14, 15, 16, 17} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{1, 2, 14, 15, 16, 17, 7} @ rc))
    }

    @Test
    def patch25(): Unit \ Assert = region rc {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ rc;
        Array.patch(-2, 4, Array#{14, 15, 16, 17} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{16, 17, 3, 4, 5, 6, 7} @ rc))
    }

    @Test
    def patch26(): Unit \ Assert = region rc {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ rc;
        Array.patch(4, 5, Array#{14, 15, 16, 17} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{1, 2, 3, 4, 14, 15, 16} @ rc))
    }

    @Test
    def patch27(): Unit \ Assert = region rc {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ rc;
        Array.patch(4, 2, Array#{14, 15, 16, 17} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{1, 2, 3, 4, 14, 15, 7} @ rc))
    }

    @Test
    def patch28(): Unit \ Assert = region rc {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ rc;
        Array.patch(-1, 10, Array#{-1, -2, -3, -4, -5, -6, -7, -8} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{-2, -3, -4, -5, -6, -7, -8} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // intersperse                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def intersperse01(): Unit \ Assert = region rc {
        let a = Array.intersperse(rc, 11, Array#{} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def intersperse02(): Unit \ Assert = region rc {
        let a = Array.intersperse(rc, 11, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def intersperse03(): Unit \ Assert = region rc {
        let a = Array.intersperse(rc, 11, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,11,2} @ rc))
    }

    @Test
    def intersperse04(): Unit \ Assert = region rc {
        let a = Array.intersperse(rc, 11, Array#{1,2,3} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,11,2,11,3} @ rc))
    }

    @Test
    def intersperse05(): Unit \ Assert = region rc {
        let a = Array.intersperse(rc, 11, Array#{1,2,3,4} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,11,2,11,3,11,4} @ rc))
    }


    /////////////////////////////////////////////////////////////////////////////
    // intercalate                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def intercalate01(): Unit \ Assert = region rc {
        let a = Array.intercalate(rc, ((Array#{} @ rc): Array[Int32, rc]), ((Array#{} @ rc): Array[Array[Int32, rc], rc]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, rc])))
    }

    @Test
    def intercalate02(): Unit \ Assert = region rc {
        let a = Array.intercalate(rc, ((Array#{} @ rc): Array[Int32, rc]), Array#{Array#{1} @ rc} @ rc);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def intercalate03(): Unit \ Assert = region rc {
        let a = Array.intercalate(rc, Array#{11,12,13} @ rc, ((Array#{} @ rc): Array[Array[Int32, rc], rc]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, rc])))
    }

    @Test
    def intercalate04(): Unit \ Assert = region rc {
        let a = Array.intercalate(rc, ((Array#{} @ rc): Array[Int32, rc]), Array#{Array#{1} @ rc, Array#{2,3} @ rc} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2,3} @ rc))
    }

    @Test
    def intercalate05(): Unit \ Assert = region rc {
        let a = Array.intercalate(rc, Array#{11,12,13} @ rc, Array#{Array#{1} @ rc, Array#{2,3} @ rc} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,11,12,13,2,3} @ rc))
    }

    @Test
    def intercalate06(): Unit \ Assert = region rc {
        let a = Array.intercalate(rc, ((Array#{} @ rc): Array[Int32, rc]), Array#{Array#{1} @ rc, Array#{2,3} @ rc, Array#{4} @ rc} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2,3,4} @ rc))
    }

    @Test
    def intercalate07(): Unit \ Assert = region rc {
        let a = Array.intercalate(rc, Array#{11,12,13} @ rc, Array#{Array#{1} @ rc, Array#{2,3} @ rc, Array#{4} @ rc} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,11,12,13,2,3,11,12,13,4} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // transpose                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def transpose01(): Unit \ Assert = region rc {
        let a = Array.transpose(rc, Array#{} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        assertEq(expected = Nil, (b: List[List[Unit]]))
    }

    @Test
    def transpose02(): Unit \ Assert = region rc {
        let a: Array[Array[Int32, _], _] = Array.transpose(rc, Array#{Array#{} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        assertEq(expected = Nil :: Nil, b)
    }

    @Test
    def transpose03(): Unit \ Assert = region rc {
        let a: Array[Array[Int32, _], _] = Array.transpose(rc, Array#{Array#{} @ rc, Array#{} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        assertEq(expected = Nil :: Nil :: Nil, b)
    }

    @Test
    def transpose04(): Unit \ Assert = region rc {
        let a: Array[Array[Int32, _], _] = Array.transpose(rc, Array#{Array#{} @ rc, Array#{} @ rc, Array#{} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        assertEq(expected = Nil :: Nil :: Nil :: Nil, b)
    }

    @Test
    def transpose05(): Unit \ Assert = region rc {
        let a = Array.transpose(rc, Array#{ Array#{1} @ rc } @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        assertEq(expected = (1 :: Nil) :: Nil, b)
    }

    @Test
    def transpose06(): Unit \ Assert = region rc {
        let a = Array.transpose(rc, Array#{Array#{1,2} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        assertEq(expected = (1 :: Nil) :: (2 :: Nil) :: Nil, b)
    }

    @Test
    def transpose07(): Unit \ Assert = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2, 3} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        assertEq(expected = (1 :: Nil) :: (2 :: Nil) :: (3 :: Nil) :: Nil, b)
    }

    @Test
    def transpose08(): Unit \ Assert = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2, 3, 4} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        assertEq(expected = (1 :: Nil) :: (2 :: Nil) :: (3 :: Nil) :: (4 :: Nil) :: Nil, b)
    }

    @Test
    def transpose09(): Unit \ Assert = region rc {
        let a = Array.transpose(rc, Array#{Array#{1} @ rc, Array#{2} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        assertEq(expected = (1 :: 2 :: Nil) :: Nil, b)
    }

    @Test
    def transpose10(): Unit \ Assert = region rc {
        let a = Array.transpose(rc, Array#{Array#{1} @ rc, Array#{2} @ rc, Array#{3} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        assertEq(expected = (1 :: 2 :: 3 :: Nil) :: Nil, b)
    }

    @Test
    def transpose11(): Unit \ Assert = region rc {
        let a = Array.transpose(rc, Array#{Array#{1} @ rc, Array#{2} @ rc, Array#{3} @ rc, Array#{4} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        assertEq(expected = (1 :: 2 :: 3 :: 4 :: Nil) :: Nil, b)
    }

    @Test
    def transpose12(): Unit \ Assert = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2} @ rc, Array#{3, 4} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        assertEq(expected = (1 :: 3 :: Nil) :: (2 :: 4 :: Nil) :: Nil, b)
    }

    @Test
    def transpose13(): Unit \ Assert = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2, 3} @ rc, Array#{4, 5, 6} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        assertEq(expected = (1 :: 4 :: Nil) :: (2 :: 5 :: Nil) :: (3 :: 6 :: Nil) :: Nil, b)
    }

    @Test
    def transpose14(): Unit \ Assert = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2, 3, 4} @ rc, Array#{5, 6, 7, 8} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        assertEq(expected = (1 :: 5 :: Nil) :: (2 :: 6 :: Nil) :: (3 :: 7 :: Nil) :: (4 :: 8 :: Nil) :: Nil, b)
    }

    @Test
    def transpose15(): Unit \ Assert = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2, 3, 4, 5} @ rc, Array#{6, 7, 8, 9, 10} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        assertEq(expected = (1 :: 6 :: Nil) :: (2 :: 7 :: Nil) :: (3 :: 8 :: Nil) :: (4 :: 9 :: Nil) :: (5 :: 10 :: Nil) :: Nil, b)
    }

    @Test
    def transpose16(): Unit \ Assert = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2} @ rc, Array#{3, 4} @ rc, Array#{5, 6} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        assertEq(expected = (1 :: 3 :: 5 :: Nil) :: (2 :: 4 :: 6 :: Nil) :: Nil, b)
    }

    @Test
    def transpose17(): Unit \ Assert = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2} @ rc, Array#{3, 4} @ rc, Array#{5, 6} @ rc, Array#{7,8} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        assertEq(expected = (1 :: 3 :: 5 :: 7 :: Nil) :: (2 :: 4 :: 6 :: 8 :: Nil) :: Nil, b)
    }

    @Test
    def transpose18(): Unit \ Assert = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2} @ rc, Array#{3, 4} @ rc, Array#{5, 6} @ rc, Array#{7, 8} @ rc, Array#{9, 10} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        assertEq(expected = (1 :: 3 :: 5 :: 7 :: 9 :: Nil) :: (2 :: 4 :: 6 :: 8 :: 10 :: Nil) :: Nil, b)
    }

    @Test
    def transpose19(): Unit \ Assert = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2, 3} @ rc, Array#{4, 5, 6} @ rc, Array#{7, 8, 9} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        assertEq(expected = (1 :: 4 :: 7 :: Nil) :: (2 :: 5 :: 8 :: Nil) :: (3 :: 6 :: 9 :: Nil) :: Nil, b)
    }

    @Test
    def transpose20(): Unit \ Assert = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2, 3} @ rc, Array#{4, 5} @ rc, Array#{7, 8, 9} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        assertEq(expected = (1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: Nil) :: (7 :: 8 :: 9 :: Nil) :: Nil, b)
    }

    @Test
    def transpose21(): Unit \ Assert = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2, 3} @ rc, Array#{} @ rc, Array#{7, 8, 9} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        assertEq(expected = (1 :: 2 :: 3 :: Nil) :: Nil :: (7 :: 8 :: 9 :: Nil) :: Nil, b)
    }

    @Test
    def transpose22(): Unit \ Assert = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2, 3} @ rc, Array#{4, 5, 6} @ rc, Array#{7, 8, 9, 10} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        assertEq(expected = (1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: 6 :: Nil) :: (7 :: 8 :: 9 :: 10 :: Nil) :: Nil, b)
    }

    /////////////////////////////////////////////////////////////////////////////
    // isPrefixOf                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isPrefixOf01(): Unit \ Assert = region rc {
        assertTrue(Array.isPrefixOf(((Array#{} @ rc): Array[Int32, _]), ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def isPrefixOf02(): Unit \ Assert = region rc {
        assertFalse(Array.isPrefixOf(Array#{1} @ rc, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def isPrefixOf03(): Unit \ Assert = region rc {
        assertTrue(Array.isPrefixOf(((Array#{} @ rc): Array[Int32, _]), Array#{1} @ rc))
    }

    @Test
    def isPrefixOf04(): Unit \ Assert = region rc {
        assertTrue(Array.isPrefixOf(Array#{1} @ rc, Array#{1} @ rc))
    }

    @Test
    def isPrefixOf05(): Unit \ Assert = region rc {
        assertFalse(Array.isPrefixOf(Array#{2} @ rc, Array#{1} @ rc))
    }

    @Test
    def isPrefixOf06(): Unit \ Assert = region rc {
        assertFalse(Array.isPrefixOf(Array#{1,2} @ rc, Array#{1} @ rc))
    }

    @Test
    def isPrefixOf07(): Unit \ Assert = region rc {
        assertTrue(Array.isPrefixOf(((Array#{} @ rc): Array[Int32, _]), Array#{1,2} @ rc))
    }

    @Test
    def isPrefixOf08(): Unit \ Assert = region rc {
        assertTrue(Array.isPrefixOf(Array#{1} @ rc, Array#{1,2} @ rc))
    }

    @Test
    def isPrefixOf09(): Unit \ Assert = region rc {
        assertFalse(Array.isPrefixOf(Array#{2} @ rc, Array#{1,2} @ rc))
    }

    @Test
    def isPrefixOf10(): Unit \ Assert = region rc {
        assertTrue(Array.isPrefixOf(Array#{1,2} @ rc, Array#{1,2} @ rc))
    }

    @Test
    def isPrefixOf11(): Unit \ Assert = region rc {
        assertFalse(Array.isPrefixOf(Array#{1,3} @ rc, Array#{1,2} @ rc))
    }

    @Test
    def isPrefixOf12(): Unit \ Assert = region rc {
        assertFalse(Array.isPrefixOf(Array#{1,2,3} @ rc, Array#{1,2} @ rc))
    }

    @Test
    def isPrefixOf13(): Unit \ Assert = region rc {
        assertFalse(Array.isPrefixOf(Array#{1,2,3} @ rc, Array#{89,11,1,2,3,4} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // isInfixOf                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isInfixOf01(): Unit \ Assert = region rc {
        assertTrue(Array.isInfixOf(((Array#{} @ rc): Array[Int32, _]), ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def isInfixOf02(): Unit \ Assert = region rc {
        assertFalse(Array.isInfixOf(Array#{1} @ rc, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def isInfixOf03(): Unit \ Assert = region rc {
        assertTrue(Array.isInfixOf(((Array#{} @ rc): Array[Int32, _]), Array#{1} @ rc))
    }

    @Test
    def isInfixOf04(): Unit \ Assert = region rc {
        assertTrue(Array.isInfixOf(Array#{1} @ rc, Array#{1} @ rc))
    }

    @Test
    def isInfixOf05(): Unit \ Assert = region rc {
        assertFalse(Array.isInfixOf(Array#{2} @ rc, Array#{1} @ rc))
    }

    @Test
    def isInfixOf06(): Unit \ Assert = region rc {
        assertFalse(Array.isInfixOf(Array#{1,2} @ rc, Array#{1} @ rc))
    }

    @Test
    def isInfixOf07(): Unit \ Assert = region rc {
        assertTrue(Array.isInfixOf(((Array#{} @ rc): Array[Int32, _]), Array#{1,2} @ rc))
    }

    @Test
    def isInfixOf08(): Unit \ Assert = region rc {
        assertTrue(Array.isInfixOf(Array#{1} @ rc, Array#{1,2} @ rc))
    }

    @Test
    def isInfixOf09(): Unit \ Assert = region rc {
        assertTrue(Array.isInfixOf(Array#{2} @ rc, Array#{1,2} @ rc))
    }

    @Test
    def isInfixOf10(): Unit \ Assert = region rc {
        assertTrue(Array.isInfixOf(Array#{1,2} @ rc, Array#{1,2} @ rc))
    }

    @Test
    def isInfixOf11(): Unit \ Assert = region rc {
        assertFalse(Array.isInfixOf(Array#{1,3} @ rc, Array#{1,2} @ rc))
    }

    @Test
    def isInfixOf12(): Unit \ Assert = region rc {
        assertFalse(Array.isInfixOf(Array#{1,2,3} @ rc, Array#{1,2} @ rc))
    }

    @Test
    def isInfixOf13(): Unit \ Assert = region rc {
        assertTrue(Array.isInfixOf(Array#{1,2,3} @ rc, Array#{89,11,1,2,3,4} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // isSuffixOf                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isSuffixOf01(): Unit \ Assert = region rc {
        assertTrue(Array.isSuffixOf(((Array#{} @ rc): Array[Int32, _]), ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def isSuffixOf02(): Unit \ Assert = region rc {
        assertFalse(Array.isSuffixOf(Array#{1} @ rc, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def isSuffixOf03(): Unit \ Assert = region rc {
        assertTrue(Array.isSuffixOf(((Array#{} @ rc): Array[Int32, _]), Array#{1} @ rc))
    }

    @Test
    def isSuffixOf04(): Unit \ Assert = region rc {
        assertTrue(Array.isSuffixOf(Array#{1} @ rc, Array#{1} @ rc))
    }

    @Test
    def isSuffixOf05(): Unit \ Assert = region rc {
        assertFalse(Array.isSuffixOf(Array#{2} @ rc, Array#{1} @ rc))
    }

    @Test
    def isSuffixOf06(): Unit \ Assert = region rc {
        assertFalse(Array.isSuffixOf(Array#{1,2} @ rc, Array#{1} @ rc))
    }

    @Test
    def isSuffixOf07(): Unit \ Assert = region rc {
        assertTrue(Array.isSuffixOf(((Array#{} @ rc): Array[Int32, _]), Array#{1,2} @ rc))
    }

    @Test
    def isSuffixOf08(): Unit \ Assert = region rc {
        assertFalse(Array.isSuffixOf(Array#{1} @ rc, Array#{1,2} @ rc))
    }

    @Test
    def isSuffixOf09(): Unit \ Assert = region rc {
        assertTrue(Array.isSuffixOf(Array#{2} @ rc, Array#{1,2} @ rc))
    }

    @Test
    def isSuffixOf10(): Unit \ Assert = region rc {
        assertTrue(Array.isSuffixOf(Array#{1,2} @ rc, Array#{1,2} @ rc))
    }

    @Test
    def isSuffixOf11(): Unit \ Assert = region rc {
        assertFalse(Array.isSuffixOf(Array#{1,3} @ rc, Array#{1,2} @ rc))
    }

    @Test
    def isSuffixOf12(): Unit \ Assert = region rc {
        assertFalse(Array.isSuffixOf(Array#{1,2,3} @ rc, Array#{1,2} @ rc))
    }

    @Test
    def isSuffixOf13(): Unit \ Assert = region rc {
        assertTrue(Array.isSuffixOf(Array#{1,2,3} @ rc, Array#{89,11,1,2,3} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // eq                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testSameElements01(): Unit \ Assert = region rc {
        assertTrue((((Array#{} @ rc): Array[Int32, _])) `sameElements` (((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def testSameElements02(): Unit \ Assert = region rc {
        assertFalse((Array#{1} @ rc) `sameElements` Array#{} @ rc)
    }

    @Test
    def testSameElements03(): Unit \ Assert = region rc {
        assertFalse((Array#{} @ rc) `sameElements` Array#{1} @ rc)
    }

    @Test
    def testSameElements04(): Unit \ Assert = region rc {
        assertTrue((Array#{1} @ rc) `sameElements` Array#{1} @ rc)
    }

    @Test
    def testSameElements05(): Unit \ Assert = region rc {
        assertFalse((Array#{1} @ rc) `sameElements` Array#{2} @ rc)
    }

    @Test
    def testSameElements06(): Unit \ Assert = region rc {
        assertFalse((Array#{1,2} @ rc) `sameElements` Array#{1} @ rc)
    }

    @Test
    def testSameElements07(): Unit \ Assert = region rc {
        assertFalse((Array#{1} @ rc) `sameElements` Array#{1,2} @ rc)
    }

    @Test
    def testSameElements08(): Unit \ Assert = region rc {
        assertTrue((Array#{1,2,3,4} @ rc) `sameElements` Array#{1,2,3,4} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // compare                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testCompare01(): Unit \ Assert = region rc {
        assertEq(expected = Comparison.EqualTo, (((Array#{} @ rc): Array[Int32, _])) `Array.compare` (((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def testCompare02(): Unit \ Assert = region rc {
        assertEq(expected = Comparison.GreaterThan, (Array#{1} @ rc) `Array.compare` (Array#{} @ rc))
    }

    @Test
    def testCompare03(): Unit \ Assert = region rc {
        assertEq(expected = Comparison.LessThan, (Array#{} @ rc) `Array.compare` (Array#{1} @ rc))
    }

    @Test
    def testCompare04(): Unit \ Assert = region rc {
        assertEq(expected = Comparison.EqualTo, (Array#{1} @ rc) `Array.compare` (Array#{1} @ rc))
    }

    @Test
    def testCompare05(): Unit \ Assert = region rc {
        assertEq(expected = Comparison.LessThan, (Array#{1} @ rc) `Array.compare` (Array#{2} @ rc))
    }

    @Test
    def testCompare06(): Unit \ Assert = region rc {
        assertEq(expected = Comparison.GreaterThan, (Array#{2} @ rc) `Array.compare` (Array#{1} @ rc))
    }

    @Test
    def testCompare07(): Unit \ Assert = region rc {
        assertEq(expected = Comparison.LessThan, (Array#{1} @ rc) `Array.compare` (Array#{1,2} @ rc))
    }

    @Test
    def testCompare08(): Unit \ Assert = region rc {
        assertEq(expected = Comparison.GreaterThan, (Array#{1,2} @ rc) `Array.compare` (Array#{1} @ rc))
    }

    @Test
    def testCompare09(): Unit \ Assert = region rc {
        assertEq(expected = Comparison.GreaterThan, (Array#{1} @ rc) `Array.compare` (Array#{0,0} @ rc))
    }

    @Test
    def testCompare10(): Unit \ Assert = region rc {
        assertEq(expected = Comparison.LessThan, (Array#{0,0} @ rc) `Array.compare` (Array#{1} @ rc))
    }

    @Test
    def testCompare11(): Unit \ Assert = region rc {
        assertEq(expected = Comparison.GreaterThan, (Array#{1,2} @ rc) `Array.compare` (Array#{1,1} @ rc))
    }

    @Test
    def testCompare12(): Unit \ Assert = region rc {
        assertEq(expected = Comparison.LessThan, (Array#{1,2} @ rc) `Array.compare` (Array#{1,3} @ rc))
    }

    @Test
    def testCompare13(): Unit \ Assert = region rc {
        assertEq(expected = Comparison.EqualTo, (Array#{1,2,3,4} @ rc) `Array.compare` (Array#{1,2,3,4} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // fold                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fold01(): Unit \ Assert = region rc {
        assertEq(expected = (), Array.fold(((Array#{} @ rc: Array[Unit, _]))))
    }

    @Test
    def fold02(): Unit \ Assert = region rc {
        assertEq(expected = "a", Array.fold(Array#{"a"} @ rc))
    }

    @Test
    def fold03(): Unit \ Assert = region rc {
        assertEq(expected = "abc", Array.fold(Array#{"a", "b", "c"} @ rc))
    }

    @Test
    def fold04(): Unit \ Assert = region rc {
        assertEq(expected = ("yesno", "noyes"), Array.fold(Array#{("yes", "no"), ("no", "yes")} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeft01(): Unit \ Assert = region rc {
        assertEq(expected = 100, Array.foldLeft((i, e) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, Array#{} @ rc))
    }

    @Test
    def foldLeft02(): Unit \ Assert = region rc {
        assertEq(expected = 198, Array.foldLeft((i, e) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, Array#{1} @ rc))
    }

    @Test
    def foldLeft03(): Unit \ Assert = region rc {
        assertEq(expected = 196, Array.foldLeft((i, e) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, Array#{1,2} @ rc))
    }

    @Test
    def foldLeft04(): Unit \ Assert = region rc {
        assertEq(expected = 386, Array.foldLeft((i, e) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, Array#{1,2,3} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRight01(): Unit \ Assert = region rc {
        assertEq(expected = 100, Array.foldRight((e, i) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, Array#{} @ rc))
    }

    @Test
    def foldRight02(): Unit \ Assert = region rc {
        assertEq(expected = 198, Array.foldRight((e, i) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, Array#{1} @ rc))
    }

    @Test
    def foldRight03(): Unit \ Assert = region rc {
        assertEq(expected = 194, Array.foldRight((e, i) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, Array#{1,2} @ rc))
    }

    @Test
    def foldRight04(): Unit \ Assert = region rc {
        assertEq(expected = 382, Array.foldRight((e, i) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, Array#{1,2,3} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldMap01(): Unit \ Assert = region rc {
        assertEq(expected = 0, Array.foldMap(x -> 2 * x, Array#{} @ rc))
    }

    @Test
    def foldMap02(): Unit \ Assert = region rc {
        assertEq(expected = 6, Array.foldMap(x -> 2 * x, Array#{1, 2} @ rc))
    }

    @Test
    def foldMap03(): Unit \ Assert = region rc {
        assertEq(expected = "b", Array.foldMap(x -> if (x == "a") "b" else x, Array#{"a"} @ rc))
    }

    @Test
    def foldMap04(): Unit \ Assert = region rc {
        assertEq(expected = "abb", Array.foldMap(x -> if (x == "c") "b" else x, Array#{"a", "b", "c"} @ rc))
    }

    @Test
    def foldMap05(): Unit \ Assert = region rc {
        assertEq(expected = "123", Array.foldMap(Int32.toString, Array#{1, 2, 3} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceLeft01(): Unit \ Assert = region rc {
        assertNone(Array.reduceLeft((a, b) -> a - b, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def reduceLeft02(): Unit \ Assert = region rc {
        assertEq(expected = Some(1), Array.reduceLeft((a, b) -> a - b, Array#{1} @ rc))
    }

    @Test
    def reduceLeft03(): Unit \ Assert = region rc {
        assertEq(expected = Some(-1), Array.reduceLeft((a, b) -> a - b, Array#{1,2} @ rc))
    }

    @Test
    def reduceLeft04(): Unit \ Assert = region rc {
        assertEq(expected = Some(-4), Array.reduceLeft((a, b) -> a - b, Array#{1,2,3} @ rc))
    }

    @Test
    def reduceLeft05(): Unit \ Assert = region rc {
        assertEq(expected = Some(-8), Array.reduceLeft((a, b) -> a - b, Array#{1,2,3,4} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceRight01(): Unit \ Assert = region rc {
        assertNone(Array.reduceRight((a, b) -> a - b, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def reduceRight02(): Unit \ Assert = region rc {
        assertEq(expected = Some(1), Array.reduceRight((a, b) -> a - b, Array#{1} @ rc))
    }

    @Test
    def reduceRight03(): Unit \ Assert = region rc {
        assertEq(expected = Some(-1), Array.reduceRight((a, b) -> a - b, Array#{1,2} @ rc))
    }

    @Test
    def reduceRight04(): Unit \ Assert = region rc {
        assertEq(expected = Some(2), Array.reduceRight((a, b) -> a - b, Array#{1,2,3} @ rc))
    }

    @Test
    def reduceRight05(): Unit \ Assert = region rc {
        assertEq(expected = Some(-2), Array.reduceRight((a, b) -> a - b, Array#{1,2,3,4} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def count01(): Unit \ Assert = region rc {
        assertEq(expected = 0, Array.count(i -> i > 3, Array#{} @ rc))
    }

    @Test
    def count02(): Unit \ Assert = region rc {
        assertEq(expected = 0, Array.count(i -> i > 3, Array#{1} @ rc))
    }

    @Test
    def count03(): Unit \ Assert = region rc {
        assertEq(expected = 1, Array.count(i -> i > 3, Array#{4} @ rc))
    }

    @Test
    def count04(): Unit \ Assert = region rc {
        assertEq(expected = 0, Array.count(i -> i > 3, Array#{1,2} @ rc))
    }

    @Test
    def count05(): Unit \ Assert = region rc {
        assertEq(expected = 1, Array.count(i -> i > 3, Array#{1,8} @ rc))
    }

    @Test
    def count06(): Unit \ Assert = region rc {
        assertEq(expected = 1, Array.count(i -> i > 3, Array#{8,1} @ rc))
    }

    @Test
    def count07(): Unit \ Assert = region rc {
        assertEq(expected = 2, Array.count(i -> i > 3, Array#{6,7} @ rc))
    }


    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatten01(): Unit \ Assert = region rc {
        let a = Array.flatten(rc, ((Array#{} @ rc): Array[Array[Int32, rc], rc]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, rc])))
    }

    @Test
    def flatten02(): Unit \ Assert = region rc {
        let a = Array.flatten(rc, ((Array#{Array#{} @ rc} @ rc): Array[Array[Int32, rc], rc]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, rc])))
    }

    @Test
    def flatten03(): Unit \ Assert = region rc {
        let a = Array.flatten(rc, Array#{Array#{1} @ rc} @ rc);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def flatten04(): Unit \ Assert = region rc {
        let a= Array.flatten(rc, Array#{Array#{1,2} @ rc} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc))
    }

    @Test
    def flatten05(): Unit \ Assert = region rc {
        let a = Array.flatten(rc, ((Array#{Array#{} @ rc,Array#{} @ rc} @ rc): Array[Array[Int32, rc], rc]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, rc])))
    }

    @Test
    def flatten06(): Unit \ Assert = region rc {
        let a = Array.flatten(rc, Array#{Array#{1} @ rc,Array#{} @ rc} @ rc);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def flatten07(): Unit \ Assert = region rc {
        let a = Array.flatten(rc, Array#{Array#{} @ rc,Array#{1} @ rc} @ rc);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def flatten08(): Unit \ Assert = region rc {
        let a = Array.flatten(rc, Array#{Array#{1} @ rc,Array#{2} @ rc} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc))
    }

    @Test
    def flatten09(): Unit \ Assert = region rc {
        let a = Array.flatten(rc, Array#{Array#{1,2} @ rc,Array#{3,4,5} @ rc} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2,3,4,5} @ rc))
    }

    @Test
    def flatten10(): Unit \ Assert = region rc {
        let a = Array.flatten(rc, Array#{Array#{1} @ rc,Array#{2,3} @ rc,Array#{4} @ rc} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2,3,4} @ rc))
    }
    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def exists01(): Unit \ Assert = region rc {
        assertFalse(Array.exists(i -> i > 3, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def exists02(): Unit \ Assert = region rc {
        assertFalse(Array.exists(i -> i > 3, Array#{1} @ rc))
    }

    @Test
    def exists03(): Unit \ Assert = region rc {
        assertTrue(Array.exists(i -> i > 3, Array#{5} @ rc))
    }

    @Test
    def exists04(): Unit \ Assert = region rc {
        assertFalse(Array.exists(i -> i > 3, Array#{1,2} @ rc))
    }

    @Test
    def exists05(): Unit \ Assert = region rc {
        assertTrue(Array.exists(i -> i > 3, Array#{1,6} @ rc))
    }

    @Test
    def exists06(): Unit \ Assert = region rc {
        assertTrue(Array.exists(i -> i > 3, Array#{6,1} @ rc))
    }

    @Test
    def exists07(): Unit \ Assert = region rc {
        assertTrue(Array.exists(i -> i > 3, Array#{16,6} @ rc))
    }

    @Test
    def exists08(): Unit \ Assert = region rc {
        assertFalse(Array.exists(i -> i > 3, Array#{1,-9,3} @ rc))
    }

    @Test
    def exists09(): Unit \ Assert = region rc {
        assertTrue(Array.exists(i -> i > 3, Array#{1,9,3} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forAll01(): Unit \ Assert = region rc {
        assertTrue(Array.forAll(i -> i > 3, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def forAll02(): Unit \ Assert = region rc {
        assertFalse(Array.forAll(i -> i > 3, Array#{1} @ rc))
    }

    @Test
    def forAll03(): Unit \ Assert = region rc {
        assertTrue(Array.forAll(i -> i > 3, Array#{5} @ rc))
    }

    @Test
    def forAll04(): Unit \ Assert = region rc {
        assertFalse(Array.forAll(i -> i > 3, Array#{1,2} @ rc))
    }

    @Test
    def forAll05(): Unit \ Assert = region rc {
        assertFalse(Array.forAll(i -> i > 3, Array#{1,6} @ rc))
    }

    @Test
    def forAll06(): Unit \ Assert = region rc {
        assertFalse(Array.forAll(i -> i > 3, Array#{6,1} @ rc))
    }

    @Test
    def forAll07(): Unit \ Assert = region rc {
        assertTrue(Array.forAll(i -> i > 3, Array#{16,6} @ rc))
    }

    @Test
    def forAll08(): Unit \ Assert = region rc {
        assertFalse(Array.forAll(i -> i > 3, Array#{1,-9,3} @ rc))
    }

    @Test
    def forAll09(): Unit \ Assert = region rc {
        assertFalse(Array.forAll(i -> i > 3, Array#{1,9,3} @ rc))
    }

    @Test
    def forAll10(): Unit \ Assert = region rc {
        assertTrue(Array.forAll(i -> i > 3, Array#{11,9,31} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filter01(): Unit \ Assert = region rc {
        let a = Array.filter(rc, i -> i > 3, ((Array#{} @ rc): Array[Int32, _]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def filter02(): Unit \ Assert = region rc {
        let a = Array.filter(rc, i -> i > 3, Array#{2} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def filter03(): Unit \ Assert = region rc {
        let a = Array.filter(rc, i -> i > 3, Array#{4} @ rc);
        assertTrue(Array.sameElements(a, Array#{4} @ rc))
    }

    @Test
    def filter04(): Unit \ Assert = region rc {
        let a = Array.filter(rc, i -> i > 3, Array#{1,3} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def filter05(): Unit \ Assert = region rc {
        let a = Array.filter(rc, i -> i > 3, Array#{1,8} @ rc);
        assertTrue(Array.sameElements(a, Array#{8} @ rc))
    }

    @Test
    def filter06(): Unit \ Assert = region rc {
        let a = Array.filter(rc, i -> i > 3, Array#{8,1} @ rc);
        assertTrue(Array.sameElements(a, Array#{8} @ rc))
    }

    @Test
    def filter07(): Unit \ Assert = region rc {
        let a = Array.filter(rc, i -> i > 3, Array#{8,9} @ rc);
        assertTrue(Array.sameElements(a, Array#{8,9} @ rc))
    }

    @Test
    def filter08(): Unit \ Assert = region rc {
        let a = Array.filter(rc, i -> i > 3, Array#{1,4,11,2,-22,17} @ rc);
        assertTrue(Array.sameElements(a, Array#{4,11,17} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // partition                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def partition01(): Unit \ Assert = region rc {
        let (a,b) = Array.partition(rc, rc, i -> i > 3, ((Array#{} @ rc): Array[Int32, _]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])));
        assertTrue(Array.sameElements(b, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def partition02(): Unit \ Assert = region rc {
        let (a,b) = Array.partition(rc, rc, i -> i > 3, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])));
        assertTrue(Array.sameElements(b, Array#{1} @ rc))
    }

    @Test
    def partition03(): Unit \ Assert = region rc {
        let (a,b) = Array.partition(rc, rc, i -> i > 3, Array#{4} @ rc);
        assertTrue(Array.sameElements(a, Array#{4} @ rc));
        assertTrue(Array.sameElements(b, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def partition04(): Unit \ Assert = region rc {
        let (a,b) = Array.partition(rc, rc, i -> i > 3, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])));
        assertTrue(Array.sameElements(b, Array#{1,2} @ rc))
    }

    @Test
    def partition05(): Unit \ Assert = region rc {
        let (a,b) = Array.partition(rc, rc, i -> i > 3, Array#{1,5} @ rc);
        assertTrue(Array.sameElements(a, Array#{5} @ rc));
        assertTrue(Array.sameElements(b, Array#{1} @ rc))
    }

    @Test
    def partition06(): Unit \ Assert = region rc {
        let (a,b) = Array.partition(rc, rc, i -> i > 3, Array#{5,1} @ rc);
        assertTrue(Array.sameElements(a, Array#{5} @ rc));
        assertTrue(Array.sameElements(b, Array#{1} @ rc))
    }

    @Test
    def partition07(): Unit \ Assert = region rc {
        let (a,b) = Array.partition(rc, rc, i -> i > 3, Array#{5,8} @ rc);
        assertTrue(Array.sameElements(a, Array#{5,8} @ rc));
        assertTrue(Array.sameElements(b, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def partition08(): Unit \ Assert = region rc {
        let (a,b) = Array.partition(rc, rc, i -> i > 3, Array#{4, -3, -5, 1, 2, 16, 7, 1, 7} @ rc);
        assertTrue(Array.sameElements(a, Array#{4, 16, 7, 7} @ rc));
        assertTrue(Array.sameElements(b, Array#{-3, -5, 1, 2, 1} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // span                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def span01(): Unit \ Assert = region rc {
        let (a,b) = Array.span(rc, rc, i -> i > 3, ((Array#{} @ rc): Array[Int32, _]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])));
        assertTrue(Array.sameElements(b, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def span02(): Unit \ Assert = region rc {
        let (a,b) = Array.span(rc, rc, i -> i > 3, Array#{1} @ rc);
        assertTrue(Array.sameElements(a,((Array#{} @ rc): Array[Int32, _])));
        assertTrue(Array.sameElements(b, Array#{1} @ rc))
    }

    @Test
    def span03(): Unit \ Assert = region rc {
        let (a,b) = Array.span(rc, rc, i -> i > 3, Array#{4} @ rc);
        assertTrue(Array.sameElements(a, Array#{4} @ rc));
        assertTrue(Array.sameElements(b, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def span04(): Unit \ Assert = region rc {
        let (a,b) = Array.span(rc, rc, i -> i > 3, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])));
        assertTrue(Array.sameElements(b, Array#{1,2} @ rc))
    }

    @Test
    def span05(): Unit \ Assert = region rc {
        let (a,b) = Array.span(rc, rc, i -> i > 3, Array#{1,5} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])));
        assertTrue(Array.sameElements(b, Array#{1,5} @ rc))
    }

    @Test
    def span06(): Unit \ Assert = region rc {
        let (a,b) = Array.span(rc, rc, i -> i > 3, Array#{5,1} @ rc);
        assertTrue(Array.sameElements(a, Array#{5} @ rc));
        assertTrue(Array.sameElements(b, Array#{1} @ rc))
    }

    @Test
    def span07(): Unit \ Assert = region rc {
        let (a,b) = Array.span(rc, rc, i -> i > 3, Array#{5,8} @ rc);
        assertTrue(Array.sameElements(a, Array#{5,8} @ rc));
        assertTrue(Array.sameElements(b, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def span08(): Unit \ Assert = region rc {
        let (a,b) = Array.span(rc, rc, i -> i > 3, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7} @ rc);
        assertTrue(Array.sameElements(a, Array#{4, 6} @ rc));
        assertTrue(Array.sameElements(b, Array#{-3, 11, -5, 1, 2, 16, 7, 1, 7} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // drop                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def drop01(): Unit \ Assert = region rc {
        let a = Array.drop(rc, -1, ((Array#{} @ rc): Array[Int32, _]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def drop02(): Unit \ Assert = region rc {
        let a = Array.drop(rc, 0, ((Array#{} @ rc): Array[Int32, _]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def drop03(): Unit \ Assert = region rc {
        let a = Array.drop(rc, 1, ((Array#{} @ rc): Array[Int32, _]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def drop04(): Unit \ Assert = region rc {
        let a = Array.drop(rc, -1, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def drop05(): Unit \ Assert = region rc {
        let a = Array.drop(rc, 0, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def drop06(): Unit \ Assert = region rc {
        let a = Array.drop(rc, 1, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def drop07(): Unit \ Assert = region rc {
        let a = Array.drop(rc, 2, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def drop08(): Unit \ Assert = region rc {
        let a = Array.drop(rc, 0, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc))
    }

    @Test
    def drop09(): Unit \ Assert = region rc {
        let a = Array.drop(rc, 1, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, Array#{2} @ rc))
    }

    @Test
    def drop10(): Unit \ Assert = region rc {
        let a = Array.drop(rc, 2, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def drop11(): Unit \ Assert = region rc {
        let a = Array.drop(rc, 2, Array#{1,2,3,4,5,6} @ rc);
        assertTrue(Array.sameElements(a, Array#{3,4,5,6} @ rc))
    }

    @Test
    def drop12(): Unit \ Assert = region rc {
        let a = Array.drop(rc, 4, Array#{1,2,3,4,5,6} @ rc);
        assertTrue(Array.sameElements(a, Array#{5,6} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // dropLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropLeft01(): Unit \ Assert = region rc {
        let a = Array.dropLeft(rc, -1, ((Array#{} @ rc): Array[Int32, _]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def dropLeft02(): Unit \ Assert = region rc {
        let a = Array.dropLeft(rc, 0, ((Array#{} @ rc): Array[Int32, _]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def dropLeft03(): Unit \ Assert = region rc {
        let a = Array.dropLeft(rc, 1, ((Array#{} @ rc): Array[Int32, _]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def dropLeft04(): Unit \ Assert = region rc {
        let a = Array.dropLeft(rc, -1, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def dropLeft05(): Unit \ Assert = region rc {
        let a = Array.dropLeft(rc, 0, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def dropLeft06(): Unit \ Assert = region rc {
        let a = Array.dropLeft(rc, 1, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def dropLeft07(): Unit \ Assert = region rc {
        let a = Array.dropLeft(rc, 2, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def dropLeft08(): Unit \ Assert = region rc {
        let a = Array.dropLeft(rc, 0, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc))
    }

    @Test
    def dropLeft09(): Unit \ Assert = region rc {
        let a = Array.dropLeft(rc, 1, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, Array#{2} @ rc))
    }

    @Test
    def dropLeft10(): Unit \ Assert = region rc {
        let a = Array.dropLeft(rc, 2, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def dropLeft11(): Unit \ Assert = region rc {
        let a = Array.dropLeft(rc, 2, Array#{1,2,3,4,5,6} @ rc);
        assertTrue(Array.sameElements(a, Array#{3,4,5,6} @ rc))
    }

    @Test
    def dropLeft12(): Unit \ Assert = region rc {
        let a = Array.dropLeft(rc, 4, Array#{1,2,3,4,5,6} @ rc);
        assertTrue(Array.sameElements(a, Array#{5,6} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // dropRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropRight01(): Unit \ Assert = region rc {
        let a = Array.dropRight(rc, -1, ((Array#{} @ rc): Array[Int32, _]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def dropRight02(): Unit \ Assert = region rc {
        let a = Array.dropRight(rc, 0, ((Array#{} @ rc): Array[Int32, _]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def dropRight03(): Unit \ Assert = region rc {
        let a = Array.dropRight(rc, 1, ((Array#{} @ rc): Array[Int32, _]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def dropRight04(): Unit \ Assert = region rc {
        let a = Array.dropRight(rc, -1, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def dropRight05(): Unit \ Assert = region rc {
        let a = Array.dropRight(rc, 0, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def dropRight06(): Unit \ Assert = region rc {
        let a = Array.dropRight(rc, 1, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def dropRight07(): Unit \ Assert = region rc {
        let a = Array.dropRight(rc, 2, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def dropRight08(): Unit \ Assert = region rc {
        let a = Array.dropRight(rc, 0, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc))
    }

    @Test
    def dropRight09(): Unit \ Assert = region rc {
        let a = Array.dropRight(rc, 1, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def dropRight10(): Unit \ Assert = region rc {
        let a = Array.dropRight(rc, 2, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def dropRight11(): Unit \ Assert = region rc {
        let a = Array.dropRight(rc, 2, Array#{1,2,3,4,5,6} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2,3,4} @ rc))
    }

    @Test
    def dropRight12(): Unit \ Assert = region rc {
        let a = Array.dropRight(rc, 4, Array#{1,2,3,4,5,6} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // dropWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropWhile01(): Unit \ Assert = region rc {
        let a = Array.dropWhile(rc, i -> i > 3, Array#{} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def dropWhile02(): Unit \ Assert = region rc {
        let a = Array.dropWhile(rc, i -> i > 3, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def dropWhile03(): Unit \ Assert = region rc {
        let a = Array.dropWhile(rc, i -> i > 3, Array#{4} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def dropWhile04(): Unit \ Assert = region rc {
        let a = Array.dropWhile(rc, i -> i > 3, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc))
    }

    @Test
    def dropWhile05(): Unit \ Assert = region rc {
        let a = Array.dropWhile(rc, i -> i > 3, Array#{1,5} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,5} @ rc))
    }

    @Test
    def dropWhile06(): Unit \ Assert = region rc {
        let a = Array.dropWhile(rc, i -> i > 3, Array#{5,1} @ rc);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def dropWhile07(): Unit \ Assert = region rc {
        let a = Array.dropWhile(rc, i -> i > 3, Array#{5,8} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def dropWhile08(): Unit \ Assert = region rc {
        let a = Array.dropWhile(rc, i -> i > 3, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7} @ rc);
        assertTrue(Array.sameElements(a, Array#{-3, 11, -5, 1, 2, 16, 7, 1, 7} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // dropWhileLeft                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropWhileLeft01(): Unit \ Assert = region rc {
        let a = Array.dropWhileLeft(rc, i -> i > 3, Array#{} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def dropWhileLeft02(): Unit \ Assert = region rc {
        let a = Array.dropWhileLeft(rc, i -> i > 3, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def dropWhileLeft03(): Unit \ Assert = region rc {
        let a = Array.dropWhileLeft(rc, i -> i > 3, Array#{4} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def dropWhileLeft04(): Unit \ Assert = region rc {
        let a = Array.dropWhileLeft(rc, i -> i > 3, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc))
    }

    @Test
    def dropWhileLeft05(): Unit \ Assert = region rc {
        let a = Array.dropWhileLeft(rc, i -> i > 3, Array#{1,5} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,5} @ rc))
    }

    @Test
    def dropWhileLeft06(): Unit \ Assert = region rc {
        let a = Array.dropWhileLeft(rc, i -> i > 3, Array#{5,1} @ rc);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def dropWhileLeft07(): Unit \ Assert = region rc {
        let a = Array.dropWhileLeft(rc, i -> i > 3, Array#{5,8} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def dropWhileLeft08(): Unit \ Assert = region rc {
        let a = Array.dropWhileLeft(rc, i -> i > 3, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7} @ rc);
        assertTrue(Array.sameElements(a, Array#{-3, 11, -5, 1, 2, 16, 7, 1, 7} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // dropWhileRight                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropWhileRight01(): Unit \ Assert = region rc {
        let a = Array.dropWhileRight(rc, i -> i > 3, Array#{} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def dropWhileRight02(): Unit \ Assert = region rc {
        let a = Array.dropWhileRight(rc, i -> i > 3, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def dropWhileRight03(): Unit \ Assert = region rc {
        let a = Array.dropWhileRight(rc, i -> i > 3, Array#{4} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def dropWhileRight04(): Unit \ Assert = region rc {
        let a = Array.dropWhileRight(rc, i -> i > 3, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc))
    }

    @Test
    def dropWhileRight05(): Unit \ Assert = region rc {
        let a = Array.dropWhileRight(rc, i -> i > 3, Array#{1,5} @ rc);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def dropWhileRight06(): Unit \ Assert = region rc {
        let a = Array.dropWhileRight(rc, i -> i > 3, Array#{5,1} @ rc);
        assertTrue(Array.sameElements(a, Array#{5,1} @ rc))
    }

    @Test
    def dropWhileRight07(): Unit \ Assert = region rc {
        let a = Array.dropWhileRight(rc, i -> i > 3, Array#{5,8} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def dropWhileRight08(): Unit \ Assert = region rc {
        let a = Array.dropWhileRight(rc, i -> i > 3, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7} @ rc);
        assertTrue(Array.sameElements(a, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // take                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def take01(): Unit \ Assert = region rc {
        let a = Array.take(rc, -1, ((Array#{} @ rc): Array[Int32, _]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def take02(): Unit \ Assert = region rc {
        let a = Array.take(rc, 0, ((Array#{} @ rc): Array[Int32, _]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def take03(): Unit \ Assert = region rc {
        let a = Array.take(rc, 1, ((Array#{} @ rc): Array[Int32, _]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def take04(): Unit \ Assert = region rc {
        let a = Array.take(rc, -1, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def take05(): Unit \ Assert = region rc {
        let a = Array.take(rc, 0, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))

    }
    @Test
    def take06(): Unit \ Assert = region rc {
        let a = Array.take(rc, 1, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }
    @Test
    def take07(): Unit \ Assert = region rc {
        let a = Array.take(rc, 2, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }
    @Test
    def take08(): Unit \ Assert = region rc {
        let a = Array.take(rc, 0, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def take09(): Unit \ Assert = region rc {
        let a = Array.take(rc, 1, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def take10(): Unit \ Assert = region rc {
        let a = Array.take(rc, 2, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc))
    }

    @Test
    def take11(): Unit \ Assert = region rc {
        let a = Array.take(rc, 2, Array#{1,2,3,4,5,6} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc))
    }

    @Test
    def take12(): Unit \ Assert = region rc {
        let a = Array.take(rc, 4, Array#{1,2,3,4,5,6} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2,3,4} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // takeLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeLeft01(): Unit \ Assert = region rc {
        let a = Array.takeLeft(rc, -1, ((Array#{} @ rc): Array[Int32, _]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def takeLeft02(): Unit \ Assert = region rc {
        let a = Array.takeLeft(rc, 0, ((Array#{} @ rc): Array[Int32, _]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def takeLeft03(): Unit \ Assert = region rc {
        let a = Array.takeLeft(rc, 1, ((Array#{} @ rc): Array[Int32, _]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def takeLeft04(): Unit \ Assert = region rc {
        let a = Array.takeLeft(rc, -1, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def takeLeft05(): Unit \ Assert = region rc {
        let a = Array.takeLeft(rc, 0, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def takeLeft06(): Unit \ Assert = region rc {
        let a = Array.takeLeft(rc, 1, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def takeLeft07(): Unit \ Assert = region rc {
        let a = Array.takeLeft(rc, 2, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def takeLeft08(): Unit \ Assert = region rc {
        let a = Array.takeLeft(rc, 0, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def takeLeft09(): Unit \ Assert = region rc {
        let a = Array.takeLeft(rc, 1, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def takeLeft10(): Unit \ Assert = region rc {
        let a = Array.takeLeft(rc, 2, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc))
    }

    @Test
    def takeLeft11(): Unit \ Assert = region rc {
        let a = Array.takeLeft(rc, 2, Array#{1,2,3,4,5,6} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc))
    }

    @Test
    def takeLeft12(): Unit \ Assert = region rc {
        let a = Array.takeLeft(rc, 4, Array#{1,2,3,4,5,6} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2,3,4} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // takeRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeRight01(): Unit \ Assert = region rc {
        let a = Array.takeRight(rc, -1, ((Array#{} @ rc): Array[Int32, _]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def takeRight02(): Unit \ Assert = region rc {
        let a = Array.takeRight(rc, 0, ((Array#{} @ rc): Array[Int32, _]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def takeRight03(): Unit \ Assert = region rc {
        let a = Array.takeRight(rc, 1, ((Array#{} @ rc): Array[Int32, _]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def takeRight04(): Unit \ Assert = region rc {
        let a = Array.takeRight(rc, -1, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def takeRight05(): Unit \ Assert = region rc {
        let a = Array.takeRight(rc, 0, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def takeRight06(): Unit \ Assert = region rc {
        let a = Array.takeRight(rc, 1, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def takeRight07(): Unit \ Assert = region rc {
        let a = Array.takeRight(rc, 2, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def takeRight08(): Unit \ Assert = region rc {
        let a = Array.takeRight(rc, 0, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def takeRight09(): Unit \ Assert = region rc {
        let a = Array.takeRight(rc, 1, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, Array#{2} @ rc))
    }

    @Test
    def takeRight10(): Unit \ Assert = region rc {
        let a = Array.takeRight(rc, 2, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc))
    }

    @Test
    def takeRight11(): Unit \ Assert = region rc {
        let a = Array.takeRight(rc, 2, Array#{1,2,3,4,5,6} @ rc);
        assertTrue(Array.sameElements(a, Array#{5,6} @ rc))
    }

    @Test
    def takeRight12(): Unit \ Assert = region rc {
        let a = Array.takeRight(rc, 4, Array#{1,2,3,4,5,6} @ rc);
        assertTrue(Array.sameElements(a, Array#{3,4,5,6} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // takeWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeWhile01(): Unit \ Assert = region rc {
        let a = Array.takeWhile(rc, i -> i > 3, ((Array#{} @ rc): Array[Int32, _]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def takeWhile02(): Unit \ Assert = region rc {
        let a = Array.takeWhile(rc, i -> i > 3, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def takeWhile03(): Unit \ Assert = region rc {
        let a = Array.takeWhile(rc, i -> i > 3, Array#{4} @ rc);
        assertTrue(Array.sameElements(a, Array#{4} @ rc))
    }

    @Test
    def takeWhile04(): Unit \ Assert = region rc {
        let a = Array.takeWhile(rc, i -> i > 3, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def takeWhile05(): Unit \ Assert = region rc {
        let a = Array.takeWhile(rc, i -> i > 3, Array#{1,5} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def takeWhile06(): Unit \ Assert = region rc {
        let a = Array.takeWhile(rc, i -> i > 3, Array#{5,1} @ rc);
        assertTrue(Array.sameElements(a, Array#{5} @ rc))
    }

    @Test
    def takeWhile07(): Unit \ Assert = region rc {
        let a = Array.takeWhile(rc, i -> i > 3, Array#{5,8} @ rc);
        assertTrue(Array.sameElements(a, Array#{5,8} @ rc))
    }

    @Test
    def takeWhile08(): Unit \ Assert = region rc {
        let a = Array.takeWhile(rc, i -> i > 3, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7} @ rc);
        assertTrue(Array.sameElements(a, Array#{4, 6} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // takeWhileLeft                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeWhileLeft01(): Unit \ Assert = region rc {
        let a = Array.takeWhileLeft(rc, i -> i > 3, ((Array#{} @ rc): Array[Int32, _]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def takeWhileLeft02(): Unit \ Assert = region rc {
        let a = Array.takeWhileLeft(rc, i -> i > 3, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def takeWhileLeft03(): Unit \ Assert = region rc {
        let a = Array.takeWhileLeft(rc, i -> i > 3, Array#{4} @ rc);
        assertTrue(Array.sameElements(a, Array#{4} @ rc))
    }

    @Test
    def takeWhileLeft04(): Unit \ Assert = region rc {
        let a = Array.takeWhileLeft(rc, i -> i > 3, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def takeWhileLeft05(): Unit \ Assert = region rc {
        let a = Array.takeWhileLeft(rc, i -> i > 3, Array#{1,5} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def takeWhileLeft06(): Unit \ Assert = region rc {
        let a = Array.takeWhileLeft(rc, i -> i > 3, Array#{5,1} @ rc);
        assertTrue(Array.sameElements(a, Array#{5} @ rc))
    }

    @Test
    def takeWhileLeft07(): Unit \ Assert = region rc {
        let a = Array.takeWhileLeft(rc, i -> i > 3, Array#{5,8} @ rc);
        assertTrue(Array.sameElements(a, Array#{5,8} @ rc))
    }

    @Test
    def takeWhileLeft08(): Unit \ Assert = region rc {
        let a = Array.takeWhileLeft(rc, i -> i > 3, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7} @ rc);
        assertTrue(Array.sameElements(a, Array#{4, 6} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // takeWhileRight                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeWhileRight01(): Unit \ Assert = region rc {
        let a = Array.takeWhileRight(rc, i -> i > 3, ((Array#{} @ rc): Array[Int32, _]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def takeWhileRight02(): Unit \ Assert = region rc {
        let a = Array.takeWhileRight(rc, i -> i > 3, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def takeWhileRight03(): Unit \ Assert = region rc {
        let a = Array.takeWhileRight(rc, i -> i > 3, Array#{4} @ rc);
        assertTrue(Array.sameElements(a, Array#{4} @ rc))
    }

    @Test
    def takeWhileRight04(): Unit \ Assert = region rc {
        let a = Array.takeWhileRight(rc, i -> i > 3, Array#{1,2} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def takeWhileRight05(): Unit \ Assert = region rc {
        let a = Array.takeWhileRight(rc, i -> i > 3, Array#{1,5} @ rc);
        assertTrue(Array.sameElements(a, Array#{5} @ rc))
    }

    @Test
    def takeWhileRight06(): Unit \ Assert = region rc {
        let a = Array.takeWhileRight(rc, i -> i > 3, Array#{5,1} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def takeWhileRight07(): Unit \ Assert = region rc {
        let a = Array.takeWhileRight(rc, i -> i > 3, Array#{5,8} @ rc);
        assertTrue(Array.sameElements(a, Array#{5,8} @ rc))
    }

    @Test
    def takeWhileRight08(): Unit \ Assert = region rc {
        let a = Array.takeWhileRight(rc, i -> i > 3, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7} @ rc);
        assertTrue(Array.sameElements(a, Array#{7} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // groupBy                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    enum A with ToString {
        case A(Int32, Int32)
    }

    instance Eq[A] {
        pub def eq(l1: A, l2: A): Bool = match (l1, l2) {
            case (A.A(x, _), A.A(y, _)) => x == y
        }
    }

    @Test
    def groupBy01(): Unit \ Assert = region rc {
        let arr = Array.groupBy(rc, identity, (Array#{} @ rc: Array[A, rc]));
        let xs = Array.toList(arr) |> List.map(Array.toList);
        assertEq(expected = Nil, xs)
    }

    @Test
    def groupBy02(): Unit \ Assert = region rc {
        let arr = Array.groupBy(rc, identity, Array#{A.A(1, 1)} @ rc);
        let xs = Array.toList(arr) |> List.map(Array.toList);
        assertEq(expected = (A.A(1, 1) :: Nil) :: Nil, xs)
    }

    @Test
    def groupBy03(): Unit \ Assert = region rc {
        let arr = Array.groupBy(rc, identity, Array#{A.A(1, 1), A.A(2, 1), A.A(1, 2), A.A(1, 1), A.A(2, 4)} @ rc);
        let xs = Array.toList(arr) |> List.map(Array.toList);
        assertEq(expected = (A.A(1, 1) :: A.A(1, 2) :: A.A(1, 1) :: Nil) :: (A.A(2, 1) :: A.A(2, 4) :: Nil) :: Nil, xs)
    }

    // /////////////////////////////////////////////////////////////////////////////
    // // groupWith                                                               //
    // /////////////////////////////////////////////////////////////////////////////

    @Test
    def groupWith01(): Unit \ Assert = region rc {
        let arr = Array.groupWith(rc, (a, b) -> a > 3 and b > 3, (Array#{} @ rc: Array[Int32, rc]));
        let xs = Array.toList(arr) |> List.map(Array.toList);
        assertEq(expected = Nil, xs)
    }

    @Test
    def groupWith02(): Unit \ Assert = region rc {
        let arr = Array.groupWith(rc, (a, b) -> a > 3 and b > 3, Array#{1} @ rc);
        let xs = Array.toList(arr) |> List.map(Array.toList);
        assertEq(expected = (1 :: Nil) :: Nil, xs)
    }

    @Test
    def groupWith03(): Unit \ Assert = region rc {
        let arr = Array.groupWith(rc, (a, b) -> a > 3 and b > 3, Array#{1, 4} @ rc);
        let xs = Array.toList(arr) |> List.map(Array.toList);
        assertEq(expected = (1 :: Nil) :: (4 :: Nil) :: Nil, xs)
    }

    @Test
    def groupWith04(): Unit \ Assert = region rc {
        let arr = Array.groupWith(rc, (a, b) -> a > 3 and b > 3, Array#{4, 9} @ rc);
        let xs = Array.toList(arr) |> List.map(Array.toList);
        assertEq(expected = (4 :: 9 :: Nil) :: Nil, xs)
    }

    @Test
    def groupWith05(): Unit \ Assert = region rc {
        let arr = Array.groupWith(rc, (a, b) -> (a > 3 and b > 3) or (a < 1 and b < 1) or ((a >= 1 and a <= 3) and (b >= 1 and b <= 3)), Array#{1, 4, 7, 6, 9, 2, 4, -1, 4, -5, 8, 16, 0} @ rc);
        let xs = Array.toList(arr) |> List.map(Array.toList);
        assertEq(expected = (1 :: 2 :: Nil) :: (4 :: 7 :: 6 :: 9 :: 4 :: 4 :: 8 :: 16 :: Nil) :: (-1 :: -5 :: 0 :: Nil) :: Nil, xs)
    }

    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zip01(): Unit \ Assert = region rc {
        let a = Array.zip(rc, ((Array#{} @ rc): Array[Int32, _]), ((Array#{} @ rc): Array[Int32, _]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[(Int32, Int32), _])))
    }

    @Test
    def zip02(): Unit \ Assert = region rc {
        let a = Array.zip(rc, Array#{1} @ rc, ((Array#{} @ rc): Array[Int32, _]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[(Int32, Int32), _])))
    }

    @Test
    def zip03(): Unit \ Assert = region rc {
        let a = Array.zip(rc, ((Array#{} @ rc): Array[Int32, _]), Array#{2} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[(Int32, Int32), _])))
    }

    @Test
    def zip04(): Unit \ Assert = region rc {
        let a = Array.zip(rc, Array#{1} @ rc, Array#{2} @ rc);
        assertTrue(Array.sameElements(a, Array#{(1, 2)} @ rc))
    }

    @Test
    def zip05(): Unit \ Assert = region rc {
        let a = Array.zip(rc, Array#{1,3} @ rc, Array#{2,4} @ rc);
        assertTrue(Array.sameElements(a, Array#{(1, 2), (3, 4)} @ rc))
    }

    @Test
    def zip06(): Unit \ Assert = region rc {
        let a = Array.zip(rc, Array#{1,3,5} @ rc, Array#{2,4,6} @ rc);
        assertTrue(Array.sameElements(a, Array#{(1, 2), (3, 4), (5, 6)} @ rc))
    }

    @Test
    def zip07(): Unit \ Assert = region rc {
        let a = Array.zip(rc, Array#{1,3,5,7} @ rc, Array#{2,4,6,8} @ rc);
        assertTrue(Array.sameElements(a, Array#{(1, 2), (3, 4), (5, 6), (7, 8)} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // zipWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWith01(): Unit \ Assert = region rc {
        let arr = Array.zipWith(rc, (a, b) -> if (b) a + 1 else a, ((Array#{} @ rc): Array[Int32, _]), ((Array#{} @ rc): Array[Bool, _]));
        assertTrue(Array.sameElements(arr, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def zipWith02(): Unit \ Assert = region rc {
        let arr = Array.zipWith(rc, (a, b) -> if (b) a + 1 else a, Array#{1} @ rc, ((Array#{} @ rc): Array[Bool, _]));
        assertTrue(Array.sameElements(arr, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def zipWith03(): Unit \ Assert = region rc {
        let arr = Array.zipWith(rc, (a, b) -> if (b) a + 1 else a, ((Array#{} @ rc): Array[Int32, _]), Array#{true} @ rc);
        assertTrue(Array.sameElements(arr, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def zipWith04(): Unit \ Assert = region rc {
        let arr = Array.zipWith(rc, (a, b) -> if (b) a + 1 else a, Array#{1} @ rc, Array#{true} @ rc);
        assertTrue(Array.sameElements(arr, Array#{2} @ rc))
    }

    @Test
    def zipWith05(): Unit \ Assert = region rc {
        let arr = Array.zipWith(rc, (a, b) -> if (b) a + 1 else a, Array#{1} @ rc, Array#{false} @ rc);
        assertTrue(Array.sameElements(arr, Array#{1} @ rc))
    }

    @Test
    def zipWith06(): Unit \ Assert = region rc {
        let arr = Array.zipWith(rc, (a, b) -> if (b) a + 1 else a,
                        Array#{1, 2, 3, 4, 5, 6, 7, 8} @ rc,
                        Array#{false, true, true, false, false, true, true, true} @ rc);
        assertTrue(Array.sameElements(arr, Array#{1, 3, 4, 4, 5, 7, 8, 9} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // unzip                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unzip01(): Unit \ Assert = region rc {
        let (a,b) = Array.unzip(rc, rc, ((Array#{} @ rc): Array[(Unit, Unit), _]));
        assertTrue(Array.sameElements(a, Array#{} @ rc));
        assertTrue(Array.sameElements(b, Array#{} @ rc))
    }

    @Test
    def unzip02(): Unit \ Assert = region rc {
        let (a,b) = Array.unzip(rc, rc, Array#{(1, true)} @ rc);
        assertTrue(Array.sameElements(a, Array#{1} @ rc));
        assertTrue(Array.sameElements(b, Array#{true} @ rc))
    }

    @Test
    def unzip03(): Unit \ Assert = region rc {
        let (a,b) = Array.unzip(rc, rc, Array#{(1, true), (2, true)} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc));
        assertTrue(Array.sameElements(b, Array#{true,true} @ rc))
    }

    @Test
    def unzip04(): Unit \ Assert = region rc {
        let (a,b) = Array.unzip(rc, rc, Array#{(1, true), (2, true), (3, false)} @ rc);
        assertTrue(Array.sameElements(a, Array#{1,2,3} @ rc));
        assertTrue(Array.sameElements(b, Array#{true,true,false} @ rc))
    }

    @Test
    def unzip05(): Unit \ Assert = region rc1 { region rc2 {
        assertTrue(unzip05Aux(rc1, Array#{(1, "1"), (2, "2"), (3, "3")} @ rc2) `Array.sameElements` (Array#{1, 2, 3} @ rc2))
    }}

    def unzip05Aux(rc1: Region[r1], a: Array[(a, b), r2]): Array[a, r1] \ { r1, r2 } =
        region rc3 {
            // snd is local, fst is for caller
            Array.unzip(rc1, rc3, a) |> fst
        }

    /////////////////////////////////////////////////////////////////////////////
    // fold2                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fold201(): Unit \ Assert = region rc {
        assertEq(expected = 4, Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, ((Array#{} @ rc): Array[Int32, _]), ((Array#{} @ rc): Array[Bool, _])))
    }

    @Test
    def fold202(): Unit \ Assert = region rc {
        assertEq(expected = 4, Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{1} @ rc, Array#{} @ rc))
    }

    @Test
    def fold203(): Unit \ Assert = region rc {
        assertEq(expected = 4, Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{} @ rc, Array#{true} @ rc))
    }

    @Test
    def fold204(): Unit \ Assert = region rc {
        assertEq(expected = 6, Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{2} @ rc, Array#{true} @ rc))
    }

    @Test
    def fold205(): Unit \ Assert = region rc {
        assertEq(expected = 8, Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{2} @ rc, Array#{false} @ rc))
    }

    @Test
    def fold206(): Unit \ Assert = region rc {
        assertEq(expected = 9, Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ rc, Array#{true,true} @ rc))
    }

    @Test
    def fold207(): Unit \ Assert = region rc {
        assertEq(expected = 14, Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ rc, Array#{true,false} @ rc))
    }

    @Test
    def fold208(): Unit \ Assert = region rc {
        assertEq(expected = 14, Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ rc, Array#{false,true} @ rc))
    }

    @Test
    def fold209(): Unit \ Assert = region rc {
        assertEq(expected = 24, Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ rc, Array#{false,false} @ rc))
    }

    @Test
    def fold210(): Unit \ Assert = region rc {
        assertEq(expected = -118, Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{6,-4,3,2} @ rc,
            Array#{true,false,false,true} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft2                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeft201(): Unit \ Assert = region rc {
        assertEq(expected = 4, Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, ((Array#{} @ rc): Array[Int32, _]), ((Array#{} @ rc): Array[Bool, _])))
    }

    @Test
    def foldLeft202(): Unit \ Assert = region rc {
        assertEq(expected = 4, Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Array#{1} @ rc, ((Array#{} @ rc): Array[Bool, _])))
    }

    @Test
    def foldLeft203(): Unit \ Assert = region rc {
        assertEq(expected = 4, Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, ((Array#{} @ rc): Array[Int32, _]), Array#{true} @ rc))
    }

    @Test
    def foldLeft204(): Unit \ Assert = region rc {
        assertEq(expected = 6, Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Array#{2} @ rc, Array#{true} @ rc))
    }

    @Test
    def foldLeft205(): Unit \ Assert = region rc {
        assertEq(expected = 8, Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Array#{2} @ rc, Array#{false} @ rc))
    }

    @Test
    def foldLeft206(): Unit \ Assert = region rc {
        assertEq(expected = 9, Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ rc, Array#{true,true} @ rc))
    }

    @Test
    def foldLeft207(): Unit \ Assert = region rc {
        assertEq(expected = 14, Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ rc, Array#{true,false} @ rc))
    }

    @Test
    def foldLeft208(): Unit \ Assert = region rc {
        assertEq(expected = 14, Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ rc, Array#{false,true} @ rc))
    }

    @Test
    def foldLeft209(): Unit \ Assert = region rc {
        assertEq(expected = 24, Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ rc, Array#{false,false} @ rc))
    }

    @Test
    def foldLeft210(): Unit \ Assert = region rc {
        assertEq(expected = -118, Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Array#{6,-4,3,2} @ rc,
            Array#{true,false,false,true} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldRight2                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRight201(): Unit \ Assert = region rc {
        assertEq(expected = 4, Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, ((Array#{} @ rc): Array[Int32, _]), ((Array#{} @ rc): Array[Bool, _])))
    }

    @Test
    def foldRight202(): Unit \ Assert = region rc {
        assertEq(expected = 4, Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Array#{1} @ rc, ((Array#{} @ rc): Array[Bool, _])))
    }

    @Test
    def foldRight203(): Unit \ Assert = region rc {
        assertEq(expected = 4, Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, ((Array#{} @ rc): Array[Int32, _]), Array#{true} @ rc))
    }

    @Test
    def foldRight204(): Unit \ Assert = region rc {
        assertEq(expected = 6, Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Array#{2} @ rc, Array#{true} @ rc))
    }

    @Test
    def foldRight205(): Unit \ Assert = region rc {
        assertEq(expected = 8, Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Array#{2} @ rc, Array#{false} @ rc))
    }

    @Test
    def foldRight206(): Unit \ Assert = region rc {
        assertEq(expected = 9, Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Array#{3,2} @ rc, Array#{true,true} @ rc))
    }

    @Test
    def foldRight207(): Unit \ Assert = region rc {
        assertEq(expected = 11, Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Array#{3,2} @ rc, Array#{true,false} @ rc))
    }

    @Test
    def foldRight208(): Unit \ Assert = region rc {
        assertEq(expected = 18, Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Array#{3,2} @ rc, Array#{false,true} @ rc))
    }

    @Test
    def foldRight209(): Unit \ Assert = region rc {
        assertEq(expected = 24, Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Array#{3,2} @ rc, Array#{false,false} @ rc))
    }

    @Test
    def foldRight210(): Unit \ Assert = region rc {
        assertEq(expected = -66, Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Array#{6,-4,3,2} @ rc,
            Array#{true,false,false,true} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // filterMap                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterMap01(): Unit \ Assert = region rc {
        let a = Array.filterMap(rc, i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, ((Array#{} @ rc): Array[Int32, _]));
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def filterMap02(): Unit \ Assert = region rc {
        let a = Array.filterMap(rc, i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Array#{1} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def filterMap03(): Unit \ Assert = region rc {
        let a = Array.filterMap(rc, i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Array#{2} @ rc);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def filterMap04(): Unit \ Assert = region rc {
        let a = Array.filterMap(rc, i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Array#{1,3} @ rc);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def filterMap05(): Unit \ Assert = region rc {
        let a = Array.filterMap(rc, i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Array#{1,4} @ rc);
        assertTrue(Array.sameElements(a, Array#{2} @ rc))
    }

    @Test
    def filterMap06(): Unit \ Assert = region rc {
        let a = Array.filterMap(rc, i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Array#{6,-1} @ rc);
        assertTrue(Array.sameElements(a, Array#{3} @ rc))
    }

    @Test
    def filterMap07(): Unit \ Assert = region rc {
        let a = Array.filterMap(rc, i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Array#{8,6} @ rc);
        assertTrue(Array.sameElements(a, Array#{4,3} @ rc))
    }

    @Test
    def filterMap08(): Unit \ Assert = region rc {
        let a = Array.filterMap(rc, i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Array#{0,1,2,3,4,5,10} @ rc);
        assertTrue(Array.sameElements(a, Array#{0,1,2,5} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // findMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findMap01(): Unit \ Assert = region rc {
        assertEq(expected = None, Array.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def findMap02(): Unit \ Assert = region rc {
        assertEq(expected = None, Array.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Array#{1} @ rc))
    }

    @Test
    def findMap03(): Unit \ Assert = region rc {
        assertEq(expected = Some(1), Array.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Array#{2} @ rc))
    }

    @Test
    def findMap04(): Unit \ Assert = region rc {
        assertEq(expected = None, Array.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Array#{1,3} @ rc))
    }

    @Test
    def findMap05(): Unit \ Assert = region rc {
        assertEq(expected = Some(2), Array.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Array#{1,4} @ rc))
    }

    @Test
    def findMap06(): Unit \ Assert = region rc {
        assertEq(expected = Some(3), Array.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Array#{6,-1} @ rc))
    }

    @Test
    def findMap07(): Unit \ Assert = region rc {
        assertEq(expected = Some(4), Array.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Array#{8,6} @ rc))
    }

    @Test
    def findMap08(): Unit \ Assert = region rc {
        assertEq(expected = Some(0), Array.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Array#{0,1,2,3,4,5,10} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toSet01(): Unit \ Assert = region rc {
        assertEq(expected = Set#{}, Array.toSet(((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def toSet02(): Unit \ Assert = region rc {
        assertEq(expected = Set#{1}, Array.toSet(Array#{1} @ rc))
    }

    @Test
    def toSet03(): Unit \ Assert = region rc {
        assertEq(expected = Set#{1, 2}, Array.toSet(Array#{1,2} @ rc))
    }

    @Test
    def toSet04(): Unit \ Assert = region rc {
        assertEq(expected = Set#{1}, Array.toSet(Array#{1,1} @ rc))
    }

    @Test
    def toSet05(): Unit \ Assert = region rc {
        assertEq(expected = Set#{1, 2, 3}, Array.toSet(Array#{1,2,3} @ rc))
    }

    @Test
    def toSet06(): Unit \ Assert = region rc {
        assertEq(expected = Set#{1, 2}, Array.toSet(Array#{1,2,1} @ rc))
    }

    @Test
    def toSet07(): Unit \ Assert = region rc {
        assertEq(expected = Set#{1, 2}, Array.toSet(Array#{1,1,2} @ rc))
    }

    @Test
    def toSet08(): Unit \ Assert = region rc {
        assertEq(expected = Set#{1, 2, 3, 4, 5, 6}, Array.toSet(Array#{2,1,2,3,4,5,6,3,3} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMap01(): Unit \ Assert = region rc {
        assertEq(expected = Map#{}, Array.toMap(((Array#{} @ rc): Array[(Int32, Bool), _])))
    }

    @Test
    def toMap02(): Unit \ Assert = region rc {
        assertEq(expected = Map#{1 => true}, Array.toMap(Array#{(1, true)} @ rc))
    }

    @Test
    def toMap03(): Unit \ Assert = region rc {
        assertEq(expected = Map#{1 => true, 2 => false}, Array.toMap(Array#{(1, true), (2, false)} @ rc))
    }

    @Test
    def toMap04(): Unit \ Assert = region rc {
        assertEq(expected = Map#{1 => true}, Array.toMap(Array#{(1, true), (1, false)} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // findIndexOf                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findIndexOf01(): Unit \ Assert = region rc {
        assertEq(expected = None, Array.findIndexOf(i -> i > 2, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def findIndexOf02(): Unit \ Assert = region rc {
        assertEq(expected = None, Array.findIndexOf(i -> i > 2, Array#{1} @ rc))
    }

    @Test
    def findIndexOf03(): Unit \ Assert = region rc {
        assertEq(expected = Some(0), Array.findIndexOf(i -> i > 2, Array#{3} @ rc))
    }

    @Test
    def findIndexOf04(): Unit \ Assert = region rc {
        assertEq(expected = None, Array.findIndexOf(i -> i > 2, Array#{1,2} @ rc))
    }

    @Test
    def findIndexOf05(): Unit \ Assert = region rc {
        assertEq(expected = Some(0), Array.findIndexOf(i -> i > 2, Array#{6,-6} @ rc))
    }

    @Test
    def findIndexOf06(): Unit \ Assert = region rc {
        assertEq(expected = Some(1), Array.findIndexOf(i -> i > 2, Array#{-6,6} @ rc))
    }

    @Test
    def findIndexOf07(): Unit \ Assert = region rc {
        assertEq(expected = Some(0), Array.findIndexOf(i -> i > 2, Array#{6,7} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // findIndexOfLeft                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findIndexOfLeft01(): Unit \ Assert = region rc {
        assertEq(expected = None, Array.findIndexOfLeft(i -> i > 2, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def findIndexOfLeft02(): Unit \ Assert = region rc {
        assertEq(expected = None, Array.findIndexOfLeft(i -> i > 2, Array#{1} @ rc))
    }

    @Test
    def findIndexOfLeft03(): Unit \ Assert = region rc {
        assertEq(expected = Some(0), Array.findIndexOfLeft(i -> i > 2, Array#{3} @ rc))
    }

    @Test
    def findIndexOfLeft04(): Unit \ Assert = region rc {
        assertEq(expected = None, Array.findIndexOfLeft(i -> i > 2, Array#{1,2} @ rc))
    }

    @Test
    def findIndexOfLeft05(): Unit \ Assert = region rc {
        assertEq(expected = Some(0), Array.findIndexOfLeft(i -> i > 2, Array#{6,-6} @ rc))
    }

    @Test
    def findIndexOfLeft06(): Unit \ Assert = region rc {
        assertEq(expected = Some(1), Array.findIndexOfLeft(i -> i > 2, Array#{-6,6} @ rc))
    }

    @Test
    def findIndexOfLeft07(): Unit \ Assert = region rc {
        assertEq(expected = Some(0), Array.findIndexOfLeft(i -> i > 2, Array#{6,7} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // findIndexOfRight                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findIndexOfRight01(): Unit \ Assert = region rc {
        assertEq(expected = None, Array.findIndexOfRight(i -> i > 2, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def findIndexOfRight02(): Unit \ Assert = region rc {
        assertEq(expected = None, Array.findIndexOfRight(i -> i > 2, Array#{1} @ rc))
    }

    @Test
    def findIndexOfRight03(): Unit \ Assert = region rc {
        assertEq(expected = Some(0), Array.findIndexOfRight(i -> i > 2, Array#{3} @ rc))
    }

    @Test
    def findIndexOfRight04(): Unit \ Assert = region rc {
        assertEq(expected = None, Array.findIndexOfRight(i -> i > 2, Array#{1,2} @ rc))
    }

    @Test
    def findIndexOfRight05(): Unit \ Assert = region rc {
        assertEq(expected = Some(0), Array.findIndexOfRight(i -> i > 2, Array#{6,-6} @ rc))
    }

    @Test
    def findIndexOfRight06(): Unit \ Assert = region rc {
        assertEq(expected = Some(1), Array.findIndexOfRight(i -> i > 2, Array#{-6,6} @ rc))
    }

    @Test
    def findIndexOfRight07(): Unit \ Assert = region rc {
        assertEq(expected = Some(1), Array.findIndexOfRight(i -> i > 2, Array#{6,7} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // findIndices                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findIndices01(): Unit \ Assert = region rc {
        let v = Array.findIndices(i -> i > 2, ((Array#{} @ rc): Array[Int32, rc]));
        assertEq(expected = Vector#{}, v)
    }

    @Test
    def findIndices02(): Unit \ Assert = region rc {
        let v = Array.findIndices(i -> i > 2, Array#{1} @ rc);
        assertEq(expected = Vector#{}, v)
    }

    @Test
    def findIndices03(): Unit \ Assert = region rc {
        let v = Array.findIndices(i -> i > 2, Array#{3} @ rc);
        assertEq(expected = Vector#{0}, v)
    }

    @Test
    def findIndices04(): Unit \ Assert = region rc {
        let v = Array.findIndices(i -> i > 2, Array#{1,2} @ rc);
        assertEq(expected = Vector#{}, v)
    }

    @Test
    def findIndices05(): Unit \ Assert = region rc {
        let v = Array.findIndices(i -> i > 2, Array#{6,-6} @ rc);
        assertEq(expected = Vector#{0}, v)
    }

    @Test
    def findIndices06(): Unit \ Assert = region rc {
        let v = Array.findIndices(i -> i > 2, Array#{-6,6} @ rc);
        assertEq(expected = Vector#{1}, v)
    }

    @Test
    def findIndices07(): Unit \ Assert = region rc {
        let v = Array.findIndices(i -> i > 2, Array#{6,7} @ rc);
        assertEq(expected = Vector#{0, 1}, v)
    }

    /////////////////////////////////////////////////////////////////////////////
    // init                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def init01(): Unit \ Assert = region rc {
        let a = Array.init(rc, x -> x, 0);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def init02(): Unit \ Assert = region rc {
        let a = Array.init(rc, x -> x, -1);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def init03(): Unit \ Assert = region rc {
        let a = Array.init(rc, x -> x, 1);
        assertTrue(Array.sameElements(a, Array#{0} @ rc))
    }

    @Test
    def init04(): Unit \ Assert = region rc {
        let a = Array.init(rc, x -> x, 2);
        assertTrue(Array.sameElements(a, Array#{0,1} @ rc))
    }

    @Test
    def init05(): Unit \ Assert = region rc {
        let a = Array.init(rc, x -> x, 50);
        assertEq(expected = 50, Array.length(a))
    }

    /////////////////////////////////////////////////////////////////////////////
    // sameElements                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sameElements01(): Unit \ Assert = region rc {
        let a = ((Array#{} @ rc): Array[Int32, _]);
        let b = ((Array#{} @ rc): Array[Int32, _]);
        assertTrue(Array.sameElements(a,b))
    }

    @Test
    def sameElements02(): Unit \ Assert = region rc {
        let a = (Array#{1} @ rc: Array[Int32, _]);
        let b = (Array#{} @ rc: Array[Int32, _]);
        assertFalse(Array.sameElements(a,b))
    }

    @Test
    def sameElements03(): Unit \ Assert = region rc {
        let a = ((Array#{} @ rc): Array[Int32, _]);
        let b = ((Array#{1} @ rc): Array[Int32, _]);
        assertFalse(Array.sameElements(a,b))
    }

    @Test
    def sameElements04(): Unit \ Assert = region rc {
        let a = ((Array#{1} @ rc): Array[Int32, _]);
        let b = ((Array#{1} @ rc): Array[Int32, _]);
        assertTrue(Array.sameElements(a,b))
    }

    @Test
    def sameElements05(): Unit \ Assert = region rc {
        let a = ((Array#{1} @ rc): Array[Int32, _]);
        let b = ((Array#{0} @ rc): Array[Int32, _]);
        assertFalse(Array.sameElements(a,b))
    }

    @Test
    def sameElements06(): Unit \ Assert = region rc {
        let a = ((Array#{1,2} @ rc): Array[Int32, _]);
        let b = ((Array#{1,2} @ rc): Array[Int32, _]);
        assertTrue(Array.sameElements(a,b))
    }

    @Test
    def sameElements07(): Unit \ Assert = region rc {
        let a = ((Array#{1,2} @ rc): Array[Int32, _]);
        let b = ((Array#{2,1} @ rc): Array[Int32, _]);
        assertFalse(Array.sameElements(a,b))
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Unit \ Assert = region rc {
        let a = ((Array#{} @ rc): Array[Int32, _]);
        let sb = StringBuilder.empty(rc);
        let fn = x -> if (x > 0) StringBuilder.append('T', sb) else StringBuilder.append('F', sb);
        Array.forEach(fn, a);
        assertEq(expected = "", StringBuilder.toString(sb))
    }

    @Test
    def forEach02(): Unit \ Assert = region rc {
        let a = Array#{0} @ rc;
        let sb = StringBuilder.empty(rc);
        let fn = x -> if (x > 0) StringBuilder.append('T', sb) else StringBuilder.append('F', sb);
        Array.forEach(fn, a);
        assertEq(expected = "F", StringBuilder.toString(sb))
    }

    @Test
    def forEach03(): Unit \ Assert = region rc {
        let a = Array#{1} @ rc;
        let sb = StringBuilder.empty(rc);
        let fn = x -> if (x > 0) StringBuilder.append('T', sb) else StringBuilder.append('F', sb);
        Array.forEach(fn, a);
        assertEq(expected = "T", StringBuilder.toString(sb))
    }

    @Test
    def forEach04(): Unit \ Assert = region rc {
        let a = Array#{0,1} @ rc;
        let sb = StringBuilder.empty(rc);
        let fn = x -> if (x > 0) StringBuilder.append('T', sb) else StringBuilder.append('F', sb);
        Array.forEach(fn, a);
        assertEq(expected = "FT", StringBuilder.toString(sb))
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEachWithIndex01(): Unit \ Assert = region rc {
        let a = ((Array#{} @ rc): Array[Float32, _]);
        let sb = StringBuilder.empty(rc);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append('T', sb) else StringBuilder.append('F', sb);
        Array.forEachWithIndex(fn, a);
        assertEq(expected = "", StringBuilder.toString(sb))
    }

    @Test
    def forEachWithIndex02(): Unit \ Assert = region rc {
        let a = Array#{0.0} @ rc;
        let sb = StringBuilder.empty(rc);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append('T', sb) else StringBuilder.append('F', sb);
        Array.forEachWithIndex(fn, a);
        assertEq(expected = "F", StringBuilder.toString(sb))
    }

    @Test
    def forEachWithIndex03(): Unit \ Assert = region rc {
        let a = Array#{0.0, 0.1} @ rc;
        let sb = StringBuilder.empty(rc);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append('T', sb) else StringBuilder.append('F', sb);
        Array.forEachWithIndex(fn, a);
        assertEq(expected = "FT", StringBuilder.toString(sb))
    }

    @Test
    def forEachWithIndex04(): Unit \ Assert = region rc {
        let a = Array#{0.0, 0.1, 0.2} @ rc;
        let sb = StringBuilder.empty(rc);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append('T', sb) else StringBuilder.append('F', sb);
        Array.forEachWithIndex(fn, a);
        assertEq(expected = "FTT", StringBuilder.toString(sb))
    }

    @Test
    def forEachWithIndex05(): Unit \ Assert = region rc {
        let a = Array#{0.0, 0.1, 0.2, 0.3} @ rc;
        let sb = StringBuilder.empty(rc);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append('T', sb) else StringBuilder.append('F', sb);
        Array.forEachWithIndex(fn, a);
        assertEq(expected = "FTTT", StringBuilder.toString(sb))
    }

    /////////////////////////////////////////////////////////////////////////////
    // updateSequence                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def updateSequence01(): Unit \ Assert = region rc {
         let a = ((Array#{} @ rc): Array[Int32, _]);
         Array.updateSequence(0, ((Array#{} @ rc): Array[Int32, _]), a);
         assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
     }

    @Test
    def updateSequence02(): Unit \ Assert = region rc {
        let a = ((Array#{} @ rc): Array[Int32, _]);
        Array.updateSequence(0, Array#{1,2} @ rc, a);
        assertTrue(Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def updateSequence03(): Unit \ Assert = region rc {
        let a = Array#{1,2} @ rc;
        Array.updateSequence(0, Array#{} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc))
    }

    @Test
    def updateSequence04(): Unit \ Assert = region rc {
        let a = Array#{1,2} @ rc;
        Array.updateSequence(-3, Array#{1,2,4} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc))
    }

    @Test
    def updateSequence05(): Unit \ Assert = region rc {
        let a = Array#{1,2} @ rc;
        Array.updateSequence(2, Array#{1,2,4} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc))
    }

    @Test
    def updateSequence06(): Unit \ Assert = region rc {
        let a = Array#{1} @ rc;
        Array.updateSequence(0, Array#{} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def updateSequence07(): Unit \ Assert = region rc {
        let a = Array#{1} @ rc;
        Array.updateSequence(1, Array#{2} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def updateSequence08(): Unit \ Assert = region rc {
        let a = Array#{1} @ rc;
        Array.updateSequence(0, Array#{2} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{2} @ rc))
    }

    @Test
    def updateSequence09(): Unit \ Assert = region rc {
        let a = Array#{1} @ rc;
        Array.updateSequence(0, Array#{2,4} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{2} @ rc))
    }

    @Test
    def updateSequence10(): Unit \ Assert = region rc {
        let a = Array#{1} @ rc;
        Array.updateSequence(-1, Array#{2,4} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{4} @ rc))
    }

    @Test
    def updateSequence11(): Unit \ Assert = region rc {
        let a = Array#{1,2} @ rc;
        Array.updateSequence(-1, Array#{3,4} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{4,2} @ rc))
    }

    @Test
    def updateSequence12(): Unit \ Assert = region rc {
        let a = Array#{1,2} @ rc;
        Array.updateSequence(1, Array#{3,4} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{1,3} @ rc))
    }

    @Test
    def updateSequence13(): Unit \ Assert = region rc {
        let a = Array#{1,2} @ rc;
        Array.updateSequence(-2, Array#{3,4} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc))
    }

    @Test
    def updateSequence14(): Unit \ Assert = region rc {
        let a = Array#{1,2} @ rc;
        Array.updateSequence(2, Array#{3,4} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{1,2} @ rc))
    }

    @Test
    def updateSequence15(): Unit \ Assert = region rc {
        let a = Array#{1,2} @ rc;
        Array.updateSequence(1, Array#{3} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{1,3} @ rc))
    }

    @Test
    def updateSequence16(): Unit \ Assert = region rc {
        let a = Array#{1,2} @ rc;
        Array.updateSequence(0, Array#{3,4} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{3,4} @ rc))
    }

    @Test
    def updateSequence17(): Unit \ Assert = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.updateSequence(0, Array#{4} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{4, 2, 3} @ rc))
    }

    @Test
    def updateSequence18(): Unit \ Assert = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.updateSequence(1, Array#{4} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{1, 4, 3} @ rc))
    }

    @Test
    def updateSequence19(): Unit \ Assert = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.updateSequence(2, Array#{4} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{1, 2, 4} @ rc))
    }

    @Test
    def updateSequence20(): Unit \ Assert = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.updateSequence(0, Array#{4, 5} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{4, 5, 3} @ rc))
    }

    @Test
    def updateSequence21(): Unit \ Assert = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.updateSequence(1, Array#{4, 5} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{1, 4, 5} @ rc))
    }

    @Test
    def updateSequence22(): Unit \ Assert = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.updateSequence(-1, Array#{4, 5, 6} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{5, 6, 3} @ rc))
    }

    @Test
    def updateSequence23(): Unit \ Assert = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.updateSequence(0, Array#{4, 5, 6} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{4, 5, 6} @ rc))
    }

    @Test
    def updateSequence24(): Unit \ Assert = region rc {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ rc;
        Array.updateSequence(2, Array#{14, 15, 16, 17} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{1, 2, 14, 15, 16, 17, 7} @ rc))
    }

    @Test
    def updateSequence25(): Unit \ Assert = region rc {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ rc;
        Array.updateSequence(-2, Array#{14, 15, 16, 17} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{16, 17, 3, 4, 5, 6, 7} @ rc))
    }

    @Test
    def updateSequence26(): Unit \ Assert = region rc {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ rc;
        Array.updateSequence(4, Array#{14, 15, 16, 17} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{1, 2, 3, 4, 14, 15, 16} @ rc))
    }

    @Test
    def updateSequence27(): Unit \ Assert = region rc {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ rc;
        Array.updateSequence(4, Array#{14, 15} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{1, 2, 3, 4, 14, 15, 7} @ rc))
    }

    @Test
    def updateSequence28(): Unit \ Assert = region rc {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ rc;
        Array.updateSequence(-1, Array#{-1, -2, -3, -4, -5, -6, -7, -8} @ rc, a);
        assertTrue(Array.sameElements(a, Array#{-2, -3, -4, -5, -6, -7, -8} @ rc))
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
        let a = ((Array#{} @ rc): Array[Int32, _]);
        Array.sortWith(cmp, a);
        assertTrue(a `sameElements` ((Array#{} @ rc): Array[Int32, _]))
    }

    @Test
    def sortWith02(): Unit \ Assert = region rc {
        let a = Array#{0} @ rc;
        Array.sortWith(cmp, a);
        assertTrue(a `sameElements` Array#{0} @ rc)
    }

    @Test
    def sortWith03(): Unit \ Assert = region rc {
        let a = Array#{0,1} @ rc;
        Array.sortWith(cmp, a);
        assertTrue(a `sameElements` Array#{0,1} @ rc)
    }

    @Test
    def sortWith04(): Unit \ Assert = region rc {
        let a = Array#{1,0} @ rc;
        Array.sortWith(cmp, a);
        assertTrue(a `sameElements` Array#{0,1} @ rc)
    }

    @Test
    def sortWith05(): Unit \ Assert = region rc {
        let a = Array#{1,1} @ rc;
        Array.sortWith(cmp, a);
        assertTrue(a `sameElements` Array#{1,1} @ rc)
    }

    @Test
    def sortWith06(): Unit \ Assert = region rc {
        let a = Array#{0,1,2,3,4,5} @ rc;
        Array.sortWith(cmp, a);
        assertTrue(a `sameElements` Array#{0,1,2,3,4,5} @ rc)
    }

    @Test
    def sortWith07(): Unit \ Assert = region rc {
        let a = Array#{5,4,3,2,1,0} @ rc;
        Array.sortWith(cmp, a);
        assertTrue(a `sameElements` Array#{0,1,2,3,4,5} @ rc)
    }

    @Test
    def sortWith08(): Unit \ Assert = region rc {
        let a = Array#{5,3,0,4,1,2} @ rc;
        Array.sortWith(cmp, a);
        assertTrue(a `sameElements` Array#{0,1,2,3,4,5} @ rc)
    }

    @Test
    def sortWith09(): Unit \ Assert = region rc {
        let a = Array#{2,3,0,4,1,2} @ rc;
        Array.sortWith(cmp, a);
        assertTrue(a `sameElements` Array#{0,1,2,2,3,4} @ rc)
    }

    @Test
    def sortWith10(): Unit \ Assert = region rc {
        let a = Array#{0,1,2,3,4,5} @ rc;
        Array.sortWith(flip(cmp), a);
        assertTrue(a `sameElements` Array#{5,4,3,2,1,0} @ rc)
    }

    @Test
    def sortWith11(): Unit \ Assert = region rc {
        let a = Array#{5,4,3,2,1,0} @ rc;
        Array.sortWith(flip(cmp), a);
        assertTrue(a `sameElements` Array#{5,4,3,2,1,0} @ rc)
    }

    @Test
    def sortWith12(): Unit \ Assert = region rc {
        let a = Array#{5,3,0,4,1,2} @ rc;
        Array.sortWith(flip(cmp), a);
        assertTrue(a `sameElements` Array#{5,4,3,2,1,0} @ rc)
    }

    @Test
    def sortWith13(): Unit \ Assert = region rc {
        let a = Array#{2,3,0,4,1,2} @ rc;
        Array.sortWith(flip(cmp), a);
        assertTrue(a `sameElements` Array#{4,3,2,2,1,0} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // sort                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    def testSortVsSortWith(a: Array[Int32, r]): Bool \ r = region rc1 {
        let b = Array.slice(rc1, start = 0, end = Array.length(a), a);
        let c = Array.slice(rc1, start = 0, end = Array.length(a), a);
        Array.sort(b);
        Array.sortWith(cmp, c);
        b `sameElements` c
    }

    @Test
    def sort01(): Unit \ Assert = region rc {
        assertTrue(testSortVsSortWith(((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def sort02(): Unit \ Assert = region rc {
        assertTrue(testSortVsSortWith(Array#{0} @ rc))
    }

    @Test
    def sort03(): Unit \ Assert = region rc {
        assertTrue(testSortVsSortWith(Array#{0,1} @ rc))
    }

    @Test
    def sort04(): Unit \ Assert = region rc {
        assertTrue(testSortVsSortWith(Array#{1,0} @ rc))
    }

    @Test
    def sort05(): Unit \ Assert = region rc {
        assertTrue(testSortVsSortWith(Array#{1,1} @ rc))
    }

    @Test
    def sort06(): Unit \ Assert = region rc {
        assertTrue(testSortVsSortWith(Array#{0,1,2,3,4,5} @ rc))
    }

    @Test
    def sort07(): Unit \ Assert = region rc {
        assertTrue(testSortVsSortWith(Array#{5,4,3,2,1,0} @ rc))
    }

    @Test
    def sort08(): Unit \ Assert = region rc {
        assertTrue(testSortVsSortWith(Array#{5,3,0,4,1,2} @ rc))
    }

    @Test
    def sort09(): Unit \ Assert = region rc {
        assertTrue(testSortVsSortWith(Array#{2,3,0,4,1,2} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // sortBy                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sortBy01(): Unit \ Assert = region rc {
        let a: Array[Int32, rc] = Array#{} @ rc;
        Array.sortBy(identity, a);
        let b: Array[Int32, rc] = Array#{} @ rc;
        Array.sortBy(x -> 4 * x + 7, b);
        assertTrue((a `sameElements` Array#{} @ rc)
            and (b `sameElements` Array#{} @ rc))
    }

    @Test
    def sortBy02(): Unit \ Assert = region rc {
        let a = Array#{0} @ rc;
        Array.sortBy(identity, a);
        let b = Array#{0} @ rc;
        Array.sortBy(x -> 4 * x + 7, b);
        assertTrue((a `sameElements` Array#{0} @ rc)
            and (b `sameElements` Array#{0} @ rc))
    }

    @Test
    def sortBy03(): Unit \ Assert = region rc {
        let a = Array#{0,1} @ rc;
        Array.sortBy(identity, a);
        let b = Array#{0,1} @ rc;
        Array.sortBy(x -> 4 * x + 7, b);
        assertTrue((a `sameElements` Array#{0,1} @ rc)
            and (b `sameElements` Array#{0,1} @ rc))
    }

    @Test
    def sortBy04(): Unit \ Assert = region rc {
        let a = Array#{1,0} @ rc;
        Array.sortBy(identity, a);
        let b = Array#{1,0} @ rc;
        Array.sortBy(x -> 4 * x + 7, b);
        assertTrue((a `sameElements` Array#{0,1} @ rc)
            and (b `sameElements` Array#{0,1} @ rc))
    }

    @Test
    def sortBy05(): Unit \ Assert = region rc {
        let a = Array#{1,1} @ rc;
        Array.sortBy(identity, a);
        let b = Array#{1,1} @ rc;
        Array.sortBy(x -> 4 * x + 7, b);
        assertTrue((a `sameElements` Array#{1,1} @ rc)
            and (b `sameElements` Array#{1,1} @ rc))
    }

    @Test
    def sortBy06(): Unit \ Assert = region rc {
        let a = Array#{0,1,2,3,4,5} @ rc;
        Array.sortBy(identity, a);
        let b = Array#{0,1,2,3,4,5} @ rc;
        Array.sortBy(x -> 4 * x + 7, b);
        assertTrue((a `sameElements` Array#{0,1,2,3,4,5} @ rc)
            and (b `sameElements` Array#{0,1,2,3,4,5} @ rc))
    }

    @Test
    def sortBy07(): Unit \ Assert = region rc {
        let a = Array#{5,4,3,2,1,0} @ rc;
        Array.sortBy(identity, a);
        let b = Array#{5,4,3,2,1,0} @ rc;
        Array.sortBy(x -> 4 * x + 7, b);
        assertTrue((a `sameElements` Array#{0,1,2,3,4,5} @ rc)
            and (b `sameElements` Array#{0,1,2,3,4,5} @ rc))
    }

    @Test
    def sortBy08(): Unit \ Assert = region rc {
        let a = Array#{5,3,0,4,1,2} @ rc;
        Array.sortBy(identity, a);
        let b = Array#{5,3,0,4,1,2} @ rc;
        Array.sortBy(x -> 4 * x + 7, b);
        assertTrue((a `sameElements` Array#{0,1,2,3,4,5} @ rc)
            and (b `sameElements` Array#{0,1,2,3,4,5} @ rc))
    }

    @Test
    def sortBy09(): Unit \ Assert = region rc {
        let a = Array#{2,3,0,4,1,2} @ rc;
        Array.sortBy(identity, a);
        let b = Array#{2,3,0,4,1,2} @ rc;
        Array.sortBy(x -> 4 * x + 7, b);
        assertTrue((a `sameElements` Array#{0,1,2,2,3,4} @ rc)
            and (b `sameElements` Array#{0,1,2,2,3,4} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimumBy01(): Unit \ Assert = region rc {
        assertEq(expected = None, Array.minimumBy((x, y) -> x <=> y, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def minimumBy02(): Unit \ Assert = region rc {
        assertEq(expected = Some(0), Array.minimumBy((x, y) -> x <=> y, Array.range(rc, 0, 5)))
    }

    @Test
    def minimumBy03(): Unit \ Assert = region rc {
        assertEq(expected = Some(4), Array.minimumBy((x, y) -> x <=> y, Array.range(rc, 4, 10)))
    }

    @Test
    def minimumBy04(): Unit \ Assert = region rc {
        let a = Array.range(rc, 10, 20);
        Array.put(-22, 5, a);
        assertEq(expected = Some(-22), Array.minimumBy((x, y) -> x <=> y, a))
    }

    /////////////////////////////////////////////////////////////////////////////
    // maximumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maximumBy01(): Unit \ Assert = region rc {
        assertEq(expected = None, Array.maximumBy((x, y) -> x <=> y, ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def maximumBy02(): Unit \ Assert = region rc {
        assertEq(expected = Some(4), Array.maximumBy((x, y) -> x <=> y, Array.range(rc, 0, 5)))
    }

    @Test
    def maximumBy03(): Unit \ Assert = region rc {
        assertEq(expected = Some(9), Array.maximumBy((x, y) -> x <=> y, Array.range(rc, 4, 10)))
    }

    @Test
    def maximumBy04(): Unit \ Assert = region rc {
        let a = Array.range(rc, 10, 20);
        Array.put(42, 5, a);
        assertEq(expected = Some(42), Array.maximumBy((x, y) -> x <=> y, a))
    }


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sum01(): Unit \ Assert = region rc {
        assertEq(expected = 0, Array.sum(Array#{} @ rc))
    }

    @Test
    def sum02(): Unit \ Assert = region rc {
        assertEq(expected = 1, Array.sum(Array#{1} @ rc))
    }

    @Test
    def sum03(): Unit \ Assert = region rc {
        assertEq(expected = 6, Array.sum(Array#{1, 2, 3} @ rc))
    }

    @Test
    def sum04(): Unit \ Assert = region rc {
        assertEq(expected = 3, Array.sum(Array#{1, 2, 3, -3} @ rc))
    }

    @Test
    def sum05(): Unit \ Assert = region rc {
        assertEq(expected = -10, Array.sum(Array#{-1, -2, -3, -4} @ rc))
    }
    @Test
    def sum06(): Unit \ Assert = region rc {
        assertEq(expected = 0, Array.sum(Array#{10, -10} @ rc))
    }

    @Test
    def sum07(): Unit \ Assert = region rc {
        assertEq(expected = 5050, Array.range(rc, 1, 101) |> Array.sum)
    }


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumWith01(): Unit \ Assert = region rc {
        assertEq(expected = 0, Array.sumWith(x -> x + 1, Array#{} @ rc))
    }

    @Test
    def sumWith02(): Unit \ Assert = region rc {
        assertEq(expected = 2, Array.sumWith(x -> x + 1, Array#{1} @ rc))
    }

    @Test
    def sumWith03(): Unit \ Assert = region rc {
        assertEq(expected = 9, Array.sumWith(x -> x + 1, Array#{1, 2, 3} @ rc))
    }

    @Test
    def sumWith04(): Unit \ Assert = region rc {
        assertEq(expected = 7, Array.sumWith(x -> x + 1, Array#{1, 2, 3, -3} @ rc))
    }

    @Test
    def sumWith05(): Unit \ Assert = region rc {
        assertEq(expected = -6, Array.sumWith(x -> x + 1, Array#{-1, -2, -3, -4} @ rc))
    }

    @Test
    def sumWith06(): Unit \ Assert = region rc {
        assertEq(expected = 2, Array.sumWith(x -> x + 1, Array#{10, -10} @ rc))
    }


    /////////////////////////////////////////////////////////////////////////////
    // toChain                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toChain01(): Unit \ Assert = region rc {
        assertEq(expected = (Chain.empty(): Chain[Unit]), Array.toChain(((Array#{} @ rc: Array[Unit, _]))))
    }

    @Test
    def toChain02(): Unit \ Assert = region rc {
        assertEq(expected = Chain.singleton(1), Array.toChain(Array#{1} @ rc))
    }

    @Test
    def toChain03(): Unit \ Assert = region rc {
        assertEq(expected = List.toChain(1 :: 2 :: Nil), Array.toChain(Array#{1,2} @ rc))
    }

    @Test
    def toChain04(): Unit \ Assert = region rc {
        assertEq(expected = List.toChain(1 :: 2 :: 3 :: Nil), Array.toChain(Array#{1,2,3} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toNec                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toNec01(): Unit \ Assert = region rc {
        assertEq(expected = None, Array.toNec(((Array#{} @ rc: Array[Unit, _]))))
    }

    @Test
    def toNec02(): Unit \ Assert = region rc {
        assertEq(expected = Some(Nec.singleton(1)), Array.toNec(Array#{1} @ rc))
    }

    @Test
    def toNec03(): Unit \ Assert = region rc {
        assertEq(expected = List.toNec(1 :: 2 :: Nil), Array.toNec(Array#{1,2} @ rc))
    }

    @Test
    def toNec04(): Unit \ Assert = region rc {
        assertEq(expected = List.toNec(1 :: 2 :: 3 :: Nil), Array.toNec(Array#{1,2,3} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // copyInto                                                                //
    /////////////////////////////////////////////////////////////////////////////

    // Test copying between arrays for primitive values
    @Test
    def copyInto01(): Unit \ Assert = region rc {
        let src = Array#{true, false, true, false, false} @ rc;
        let dst = Array.empty(rc, 5);
        Array.copyInto(srcPos = 0, dstPos = 0, len = 5, src = src, dst);
        assertTrue(src `sameElements` dst)
    }

    @Test
    def copyInto02(): Unit \ Assert = region rc {
        let src = Array#{1i8, 4i8, 5i8, 42i8, 123i8} @ rc;
        let dst = Array.empty(rc, 5);
        Array.copyInto(srcPos = 0, dstPos = 0, len = 5, src = src, dst);
        assertTrue(src `sameElements` dst)
    }

    @Test
    def copyInto04(): Unit \ Assert = region rc {
        let src = Array#{1i16, 4i16, 5i16, 42i16, 123i16} @ rc;
        let dst = Array.empty(rc, 5);
        Array.copyInto(srcPos = 0, dstPos = 0, len = 5, src = src, dst);
        assertTrue(src `sameElements` dst)
    }

    @Test
    def copyInto05(): Unit \ Assert = region rc {
        let src = Array#{1i32, 4i32, 5i32, 42i32, 123i32} @ rc;
        let dst = Array.empty(rc, 5);
        Array.copyInto(srcPos = 0, dstPos = 0, len = 5, src = src, dst);
        assertTrue(src `sameElements` dst)
    }


    @Test
    def copyInto06(): Unit \ Assert = region rc {
        let src = Array#{1i64, 4i64, 5i64, 42i64, 123i64} @ rc;
        let dst = Array.empty(rc, 5);
        Array.copyInto(srcPos = 0, dstPos = 0, len = 5, src = src, dst);
        assertTrue(src `sameElements` dst)
    }


    @Test
    def copyInto07(): Unit \ Assert = region rc {
        let src = Array#{1f32, 4f32, 5f32, 42f32, 123f32} @ rc;
        let dst = Array.empty(rc, 5);
        Array.copyInto(srcPos = 0, dstPos = 0, len = 5, src = src, dst);
        assertTrue(src `sameElements` dst)
    }

    @Test
    def copyInto08(): Unit \ Assert = region rc {
        let src = Array#{1f64, 4f64, 5f64, 42f64, 123f64} @ rc;
        let dst = Array.empty(rc, 5);
        Array.copyInto(srcPos = 0, dstPos = 0, len = 5, src = src, dst);
        assertTrue(src `sameElements` dst)
    }

    @Test
    def copyInto09(): Unit \ Assert = region rc {
        let src = Array#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10} @ rc;
        let dst = Array#{0, 0, 0, 0} @ rc;
        Array.copyInto(srcPos = 3, dstPos = 1, len = 2, src = src, dst);
        let copiedCorrectly = dst `sameElements` Array#{0, 4, 5, 0} @ rc;
        let sourceUnchanged = src `sameElements` Array#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10} @ rc;
        assertTrue(copiedCorrectly);
        assertTrue(sourceUnchanged)
    }

    @Test
    def copyInto10(): Unit \ Assert = region rc {
        let src = Array#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10} @ rc;
        let dst = Array#{0,0,0,0} @ rc;
        Array.copyInto(srcPos = 3, dstPos = 1, len = 0, src = src, dst);
        assertTrue(dst `sameElements` Array#{0,0,0,0} @ rc)
    }

    @Test
    def copyInto11(): Unit \ Assert = region rc {
        let src = Array#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10} @ rc;
        Array.copyInto(srcPos = 0, dstPos = 5, len = 5, src = src, src);
        assertTrue(src `sameElements` Array#{1, 2, 3, 4, 5, 1, 2, 3, 4, 5} @ rc)
    }

    @Test
    def copyInto12(): Unit \ Assert = region rc {
        let src = Array#{1, 2, 3, 4, 5} @ rc;
        Array.copyInto(srcPos = 0, dstPos = 1, len = 4, src = src, src);
        assertTrue(src `sameElements` Array#{1, 1, 2, 3, 4} @ rc)
    }

    @Test
    def copyInto13(): Unit \ Assert = region rc {
        let src = Array#{Some(1), None, Some(2), None, Some(3), None, Some(4)} @ rc;
        let dst = Array#{Some(-1),Some(-1),Some(-1),Some(-1),Some(-1),Some(-1)} @ rc;
        Array.copyInto(srcPos = 1, dstPos = 2, len = 4, src = src, dst);
        assertTrue(dst `sameElements` Array#{Some(-1), Some(-1), None, Some(2), None, Some(3)} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // copy                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def copy01(): Unit \ Assert = region rc {
        assertTrue(Array.copy(rc, Array#{true, false, true} @ rc) `sameElements` Array#{true, false, true} @ rc)
    }

    @Test
    def copy02(): Unit \ Assert = region rc {
        assertTrue(Array.copy(rc, Array#{'a', 'b', 'c'} @ rc) `sameElements` Array#{'a', 'b', 'c'} @ rc)
    }

    @Test
    def copy03(): Unit \ Assert = region rc {
        assertTrue(Array.copy(rc, Array#{1.0f32, 2.0f32, 3.0f32} @ rc) `sameElements` Array#{1.0f32, 2.0f32, 3.0f32} @ rc)
    }

    @Test
    def copy04(): Unit \ Assert = region rc {
        assertTrue(Array.copy(rc, Array#{1.0f64, 2.0f64, 3.0f64} @ rc) `sameElements` Array#{1.0f64, 2.0f64, 3.0f64} @ rc)
    }

    @Test
    def copy05(): Unit \ Assert = region rc {
        assertTrue(Array.copy(rc, Array#{1i8, 2i8, 3i8} @ rc) `sameElements` Array#{1i8, 2i8, 3i8} @ rc)
    }

    @Test
    def copy06(): Unit \ Assert = region rc {
        assertTrue(Array.copy(rc, Array#{1i16, 2i16, 3i16} @ rc) `sameElements` Array#{1i16, 2i16, 3i16} @ rc)
    }

    @Test
    def copy07(): Unit \ Assert = region rc {
        assertTrue(Array.copy(rc, Array#{1, 2, 3} @ rc) `sameElements` Array#{1, 2, 3} @ rc)
    }

    @Test
    def copy08(): Unit \ Assert = region rc {
        assertTrue(Array.copy(rc, Array#{1i64, 2i64, 3i64} @ rc) `sameElements` Array#{1i64, 2i64, 3i64} @ rc)
    }

    @Test
    def copy09(): Unit \ Assert = region rc {
        assertTrue(Array.copy(rc, Array#{Some(1), Some(2), None} @ rc) `sameElements` Array#{Some(1), Some(2), None} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // copyOfRange                                                             //
    /////////////////////////////////////////////////////////////////////////////

    // depends on flatten

    @Test
    def copyOfRange01(): Unit \ Assert = region rc {
        assertTrue(Array.copyOfRange(rc, 0, 3, Array#{true, false, true} @ rc) `sameElements` Array#{true, false, true} @ rc)
    }

    @Test
    def copyOfRange02(): Unit \ Assert = region rc {
        assertTrue(Array.copyOfRange(rc, 1, 3, Array#{true, false, true} @ rc) `sameElements` Array#{false, true} @ rc)
    }

    @Test
    def copyOfRange03(): Unit \ Assert = region rc {
        let a = Array.copyOfRange(rc, 2, 5, Array#{true, false, true} @ rc);
        assertTrue(Array.length(a) == 3 and Array.memberOf(true, a))
    }

    @Test
    def copyOfRange04(): Unit \ Assert = region rc {
        assertTrue(Array.copyOfRange(rc, 0, 3, Array#{'a', 'b', 'c'} @ rc) `sameElements` Array#{'a', 'b', 'c'} @ rc)
    }

    @Test
    def copyOfRange05(): Unit \ Assert = region rc {
        assertTrue(Array.copyOfRange(rc, 1, 3, Array#{'a', 'b', 'c'} @ rc) `sameElements` Array#{'b', 'c'} @ rc)
    }

    @Test
    def copyOfRange06(): Unit \ Assert = region rc {
        let a = Array.copyOfRange(rc, 2, 5, Array#{'a', 'b', 'c'} @ rc);
        assertTrue(Array.length(a) == 3 and Array.memberOf('c', a))
    }

    @Test
    def copyOfRange07(): Unit \ Assert = region rc {
        assertTrue(Array.copyOfRange(rc, 0, 3, Array#{1.0f32, 2.0f32, 3.0f32} @ rc) `sameElements` Array#{1.0f32, 2.0f32, 3.0f32} @ rc)
    }

    @Test
    def copyOfRange08(): Unit \ Assert = region rc {
        assertTrue(Array.copyOfRange(rc, 1, 3, Array#{1.0f32, 2.0f32, 3.0f32} @ rc) `sameElements` Array#{2.0f32, 3.0f32} @ rc)
    }

    @Test
    def copyOfRange09(): Unit \ Assert = region rc {
        let a = Array.copyOfRange(rc, 2, 5, Array#{1.0f32, 2.0f32, 3.0f32} @ rc);
        assertTrue(Array.length(a) == 3 and Array.memberOf(3.0f32, a))
    }

    @Test
    def copyOfRange10(): Unit \ Assert = region rc {
        assertTrue(Array.copyOfRange(rc, 0, 3, Array#{1.0f64, 2.0f64, 3.0f64} @ rc) `sameElements` Array#{1.0f64, 2.0f64, 3.0f64} @ rc)
    }

    @Test
    def copyOfRange11(): Unit \ Assert = region rc {
        assertTrue(Array.copyOfRange(rc, 1, 3, Array#{1.0f64, 2.0f64, 3.0f64} @ rc) `sameElements` Array#{2.0f64, 3.0f64} @ rc)
    }

    @Test
    def copyOfRange12(): Unit \ Assert = region rc {
        let a = Array.copyOfRange(rc, 2, 5, Array#{1.0f64, 2.0f64, 3.0f64} @ rc);
        assertTrue(Array.length(a) == 3 and Array.memberOf(3.0f64, a))
    }

    @Test
    def copyOfRange13(): Unit \ Assert = region rc {
        assertTrue(Array.copyOfRange(rc, 0, 3, Array#{1i8, 2i8, 3i8} @ rc) `sameElements` Array#{1i8, 2i8, 3i8} @ rc)
    }

    @Test
    def copyOfRange14(): Unit \ Assert = region rc {
        assertTrue(Array.copyOfRange(rc, 1, 3, Array#{1i8, 2i8, 3i8} @ rc) `sameElements` Array#{2i8, 3i8} @ rc)
    }

    @Test
    def copyOfRange15(): Unit \ Assert = region rc {
        let a = Array.copyOfRange(rc, 2, 5, Array#{1i8, 2i8, 3i8} @ rc);
        assertTrue(Array.length(a) == 3 and Array.memberOf(3i8, a))
    }

    @Test
    def copyOfRange16(): Unit \ Assert = region rc {
        assertTrue(Array.copyOfRange(rc, 0, 3, Array#{1i16, 2i16, 3i16} @ rc) `sameElements` Array#{1i16, 2i16, 3i16} @ rc)
    }

    @Test
    def copyOfRange17(): Unit \ Assert = region rc {
        assertTrue(Array.copyOfRange(rc, 1, 3, Array#{1i16, 2i16, 3i16} @ rc) `sameElements` Array#{2i16, 3i16} @ rc)
    }

    @Test
    def copyOfRange18(): Unit \ Assert = region rc {
        let a = Array.copyOfRange(rc, 2, 5, Array#{1i16, 2i16, 3i16} @ rc);
        assertTrue(Array.length(a) == 3 and Array.memberOf(3i16, a))
    }

    @Test
    def copyOfRange19(): Unit \ Assert = region rc {
        assertTrue(Array.copyOfRange(rc, 0, 3, Array#{1, 2, 3} @ rc) `sameElements` Array#{1, 2, 3} @ rc)
    }

    @Test
    def copyOfRange20(): Unit \ Assert = region rc {
        assertTrue(Array.copyOfRange(rc, 1, 3, Array#{1, 2, 3} @ rc) `sameElements` Array#{2, 3} @ rc)
    }

    @Test
    def copyOfRange21(): Unit \ Assert = region rc {
        let a = Array.copyOfRange(rc, 2, 5, Array#{1, 2, 3} @ rc);
        assertTrue(Array.length(a) == 3 and Array.memberOf(3, a))
    }

    @Test
    def copyOfRange22(): Unit \ Assert = region rc {
        assertTrue(Array.copyOfRange(rc, 0, 3, Array#{1i64, 2i64, 3i64} @ rc) `sameElements` Array#{1i64, 2i64, 3i64} @ rc)
    }

    @Test
    def copyOfRange23(): Unit \ Assert = region rc {
        assertTrue(Array.copyOfRange(rc, 1, 3, Array#{1i64, 2i64, 3i64} @ rc) `sameElements` Array#{2i64, 3i64} @ rc)
    }

    @Test
    def copyOfRange24(): Unit \ Assert = region rc {
        let a = Array.copyOfRange(rc, 2, 5, Array#{1i64, 2i64, 3i64} @ rc);
        assertTrue(Array.length(a) == 3 and Array.memberOf(3i64, a))
    }

    @Test
    def copyOfRange25(): Unit \ Assert = region rc {
        let a = Array.copyOfRange(rc, 0, 3, Array#{Array#{'a', 'b', 'c'} @ rc, Array#{'d', 'e', 'f', 'g'} @ rc, Array#{'h'} @ rc} @ rc);
        let b = Array#{Array#{'a', 'b', 'c'} @ rc, Array#{'d', 'e', 'f', 'g'} @ rc, Array#{'h'} @ rc} @ rc;
        assertTrue(Array.flatten(rc, a) `sameElements` Array.flatten(rc, b))
    }

    @Test
    def copyOfRange26(): Unit \ Assert = region rc {
        let a = Array.copyOfRange(rc, 1, 3, Array#{Array#{'a', 'b', 'c'} @ rc, Array#{'d', 'e', 'f', 'g'} @ rc, Array#{'h'} @ rc} @ rc);
        let b = Array#{Array#{'d', 'e', 'f', 'g'} @ rc, Array#{'h'} @ rc } @ rc;
        assertTrue(Array.flatten(rc, a) `sameElements` Array.flatten(rc, b))
    }

    @Test
    def copyOfRange27(): Unit \ Assert = region rc {
        assertTrue(Array.copyOfRange(rc, 0, 3, Array#{Some(1), Some(2), None} @ rc) `sameElements` Array#{Some(1), Some(2), None} @ rc)
    }

    @Test
    def copyOfRange28(): Unit \ Assert = region rc {
        assertTrue(Array.copyOfRange(rc, 1, 3, Array#{Some(1), Some(2), None} @ rc) `sameElements` Array#{Some(2), None} @ rc)
    }

    @Test
    def copyOfRange29(): Unit \ Assert = region rc {
        let a = Array.copyOfRange(rc, 2, 5, Array#{Some(1), Some(2), None} @ rc);
        assertTrue(Array.length(a) == 3 and Array.memberOf(None, a))
    }

    @Test
    def copyOfRange30(): Unit \ Assert = region rc {
        let a = Array.copyOfRange(rc, 0, 3, Array#{Array#{Some('a'), Some('b'), Some('c')} @ rc, Array#{Some('d'), Some('e'), Some('f'), Some('g')} @ rc, Array#{None} @ rc} @ rc);
        let b = Array#{Array#{Some('a'), Some('b'), Some('c')} @ rc, Array#{Some('d'), Some('e'), Some('f'), Some('g')} @ rc, Array#{None} @ rc } @ rc;
        assertTrue(Array.flatten(rc, a) `sameElements` Array.flatten(rc, b))
    }

    @Test
    def copyOfRange31(): Unit \ Assert = region rc {
        let a = Array.copyOfRange(rc, 1, 3, Array#{Array#{Some('a'), Some('b'), Some('c')} @ rc, Array#{Some('d'), Some('e'), Some('f'), Some('g')} @ rc, Array#{None} @ rc} @ rc);
        let b = Array#{Array#{Some('d'), Some('e'), Some('f'), Some('g')} @ rc, Array#{None} @ rc} @ rc;
        assertTrue(Array.flatten(rc, a) `sameElements` Array.flatten(rc, b))
    }

    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterator01(): Unit \ Assert = region rc {
        assertEq(expected = Nil, ((Array#{} @ rc): Array[Int32, rc]) |> Array.iterator(rc) |> Iterator.toList)
    }

    @Test
    def iterator02(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: 2 :: 3 :: Nil, (Array#{1, 2, 3} @ rc) |> Array.iterator(rc) |> Iterator.toList)
    }

    @Test
    def iterator03(): Unit \ Assert = region rc {
        assertTrue((Array#{1, 2, 3} @ rc) |> Array.iterator(rc) |> Iterator.toArray(rc) `sameElements` Array#{1, 2, 3} @ rc)
    }

    @Test
    def iterator04(): Unit \ Assert = region rc {
        assertTrue(Array.range(rc, -100, 100) |> Array.iterator(rc) |> Iterator.toArray(rc) `sameElements` Array.range(rc, -100, 100))
    }


    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def join01(): Unit \ Assert = region rc {
        assertEq(expected = "", Array.join(",", ((Array#{} @ rc): Array[Int32, _])))
    }

    @Test
    def join02(): Unit \ Assert = region rc {
        assertEq(expected = "1", Array.join(",", Array#{1} @ rc))
    }

    @Test
    def join03(): Unit \ Assert = region rc {
        assertEq(expected = "1,2,3", Array.join(",", Array#{1, 2, 3} @ rc))
    }

    @Test
    def join04(): Unit \ Assert = region rc {
        assertEq(expected = "1,2,3", Array.join(",", Array#{"1", "2", "3"} @ rc))
    }


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinWith01(): Unit \ Assert = region rc {
        assertEq(expected = "", Array.joinWith(x -> "${x + 1}", ",", Array#{} @ rc))
    }

    @Test
    def joinWith02(): Unit \ Assert = region rc {
        assertEq(expected = "2", Array.joinWith(x -> "${x + 1}", ",", Array#{1} @ rc))
    }

    @Test
    def joinWith03(): Unit \ Assert = region rc {
        assertEq(expected = "2,3,4", Array.joinWith(x -> "${x + 1}", ",", Array#{1, 2, 3} @ rc))
    }

    @Test
    def joinWith04(): Unit \ Assert = region rc {
        assertEq(expected = "11,22,33", Array.joinWith(x -> x + x, ",", Array#{"1", "2", "3"} @ rc))
    }

    @Test
    def joinWith05(): Unit \ Assert = region rc {
        let a: Array[Int32, _] = Array#{} @ rc;
        let s = Array.joinWith(Int32.toString, ",", a);
        assertEq(expected = "", s)
    }

    @Test
    def joinWith06(): Unit \ Assert = region rc {
        let a = Array#{1} @ rc;
        let s = Array.joinWith(Int32.toString, ",", a);
        assertEq(expected = "1", s)
    }

    @Test
    def joinWith07(): Unit \ Assert = region rc {
        let a = Array#{1,2} @ rc;
        let s = Array.joinWith(Int32.toString, ",", a);
        assertEq(expected = "1,2", s)
    }

    @Test
    def joinWith08(): Unit \ Assert = region rc {
        let a = Array#{1,2,3} @ rc;
        let s = Array.joinWith(Int32.toString, ",", a);
        assertEq(expected = "1,2,3", s)
    }

    @Test
    def joinWith09(): Unit \ Assert = region rc {
        let a = Array#{1,2,3} @ rc;
        let s = Array.joinWith(Int32.toString, "", a);
        assertEq(expected = "123", s)
    }

    @Test
    def joinWith10(): Unit \ Assert = region rc {
        let a = Array#{1,2,3} @ rc;
        let s = Array.joinWith(Int32.toString, "..", a);
        assertEq(expected = "1..2..3", s)
    }


    /////////////////////////////////////////////////////////////////////////////
    // shuffle                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def shuffle01(): Unit \ Assert + NonDet = region rc {
        run {
            let a = ((Array#{} @ rc): Array[Int32, _]);
            Array.shuffle(a);
            assertTrue(Array.length(a) == 0 and Array.toSet(a) == Set#{})
        } with Shuffle.runWithIO
    }

    @Test
    def shuffle02(): Unit \ Assert + NonDet = region rc {
        run {
            let a = (Array#{0} @ rc);
            Array.shuffle(a);
            assertTrue(Array.length(a) == 1 and Array.toSet(a) == Set#{0})
        } with Shuffle.runWithIO
    }

    @Test
    def shuffle03(): Unit \ Assert + NonDet = region rc {
        run {
            let a = (Array#{0, 1, 2, 3} @ rc);
            Array.shuffle(a);
            assertTrue(Array.length(a) == 4 and Array.toSet(a) == Set#{0, 1, 2, 3})
        } with Shuffle.runWithIO
    }

    @Test
    def shuffle04(): Unit \ Assert + NonDet = region rc {
        run {
            let a = (Array#{0, 1, 2, 3, 4, 5, 6, 7, 8, 9} @ rc);
            Array.shuffle(a);
            assertTrue(Array.length(a) == 10 and Array.toSet(a) == Set#{0, 1, 2, 3, 4, 5, 6, 7, 8, 9})
        } with Shuffle.runWithIO
    }

}
