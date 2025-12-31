mod TestVector {

    use Assert.{assertEq, assertNeq, assertTrue, assertFalse, assertSome, assertNone};

    /////////////////////////////////////////////////////////////////////////////
    // binarySearch                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def binarySearch01(): Unit \ Assert =
        assertEq(expected = None, Vector.binarySearch(0, Vector#{}))

    @Test
    def binarySearch02(): Unit \ Assert =
        assertEq(expected = Some(0), Vector.binarySearch(0, Vector#{0}))

    @Test
    def binarySearch03(): Unit \ Assert =
        assertEq(expected = Some(0), Vector.binarySearch(0, Vector#{0, 1, 2, 3}))

    @Test
    def binarySearch04(): Unit \ Assert =
        assertEq(expected = Some(1), Vector.binarySearch(1, Vector#{0, 1, 2, 3}))

    @Test
    def binarySearch05(): Unit \ Assert =
        assertEq(expected = Some(2), Vector.binarySearch(2, Vector#{0, 1, 2, 3}))

    @Test
    def binarySearch06(): Unit \ Assert =
        assertEq(expected = Some(3), Vector.binarySearch(3, Vector#{0, 1, 2, 3}))

    @Test
    def binarySearch07(): Unit \ Assert =
        assertEq(expected = Some(1), Vector.binarySearch(1, Vector#{0, 1}))

    @Test
    def binarySearch08(): Unit \ Assert =
        assertEq(expected = Some(1), Vector.binarySearch(1, Vector#{0, 1, 2}))

    @Test
    def binarySearch09(): Unit \ Assert =
        assertEq(expected = Some(1), Vector.binarySearch(1, Vector#{0, 1, 2, 3, 4}))

    @Test
    def binarySearch10(): Unit \ Assert =
        assertEq(expected = Some(1), Vector.binarySearch(1, Vector#{0, 1, 2, 3, 4, 5}))

    @Test
    def binarySearch11(): Unit \ Assert =
        assertEq(expected = Some(1), Vector.binarySearch(1, Vector#{0, 1, 2, 3, 4, 5, 6}))

    @Test
    def binarySearch12(): Unit \ Assert =
        assertEq(expected = Some(1), Vector.binarySearch(1, Vector#{0, 1, 2, 3, 4, 5, 6, 7}))

    @Test
    def binarySearch13(): Unit \ Assert =
        assertEq(expected = Some(1), Vector.binarySearch(1, Vector#{0, 1, 2, 3, 4, 5, 6, 7, 8}))

    @Test
    def binarySearch14(): Unit \ Assert =
        assertEq(expected = Some(1), Vector.binarySearch(1, Vector#{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}))

    @Test
    def binarySearch15(): Unit \ Assert =
        assertEq(expected = Some(2), Vector.binarySearch(4, Vector#{0, 2, 4, 6, 8, 10}))

    @Test
    def binarySearch16(): Unit \ Assert =
        assertEq(expected = None, Vector.binarySearch(3, Vector#{0, 2, 4, 6, 8, 10}))

    @Test
    def binarySearch17(): Unit \ Assert =
        assertEq(expected = None, Vector.binarySearch(-1, Vector#{0, 2, 4, 6, 8, 10}))

    @Test
    def binarySearch18(): Unit \ Assert =
        assertEq(expected = None, Vector.binarySearch(11, Vector#{0, 2, 4, 6, 8, 10}))

    @Test
    def binarySearch19(): Unit \ Assert =
        assertEq(expected = None, Vector.binarySearch(11i64, Vector#{0i64, 2i64, 4i64, 6i64, 8i64, 10i64}))

    @Test
    def binarySearch20(): Unit \ Assert =
        assertEq(expected = Some(1), Vector.binarySearch(2i64, Vector#{0i64, 2i64, 4i64, 6i64, 8i64, 10i64}))

    @Test
    def binarySearch21(): Unit \ Assert =
        assertEq(expected = Some(0), Vector.binarySearch("a", Vector#{"a", "b", "c", "d", "e", "f"}))

    @Test
    def binarySearch22(): Unit \ Assert =
        assertEq(expected = Some(1), Vector.binarySearch("b", Vector#{"a", "b", "c", "d", "e", "f"}))

    @Test
    def binarySearch23(): Unit \ Assert =
        assertEq(expected = None, Vector.binarySearch("g", Vector#{"a", "b", "c", "d", "e", "f"}))

    @Test
    def binarySearch24(): Unit \ Assert =
        let pos = Vector.binarySearch(1, Vector#{0, 1, 1, 2, 3, 4, 5, 6});
        assertTrue(pos == Some(1) or pos == Some(2))

    @Test
    def binarySearch25(): Unit \ Assert =
        assertEq(expected = Some(0), Vector.binarySearch(0, Vector#{0, 1, 1, 1, 1, 1, 1, 1, 2}))

    @Test
    def binarySearch26(): Unit \ Assert =
        assertEq(expected = None, Vector.binarySearch(-1, Vector#{0, 1, 1, 1, 1, 1, 1, 1, 2}))

    @Test
    def binarySearch27(): Unit \ Assert =
        let pos = Vector.binarySearch(4, Vector#{4, 3, 2, 1, 0});
        assertTrue(pos == None or pos == Some(0))

    @Test
    def binarySearch28(): Unit \ Assert =
        let pos = Vector.binarySearch(3, Vector#{4, 3, 2, 1, 0});
        assertTrue(pos == None or pos == Some(1))

    @Test
    def binarySearch29(): Unit \ Assert =
        let pos = Vector.binarySearch(2, Vector#{4, 3, 2, 1, 0});
        assertTrue(pos == None or pos == Some(2))

    @Test
    def binarySearch30(): Unit \ Assert =
        let pos = Vector.binarySearch(1, Vector#{4, 3, 2, 1, 0});
        assertTrue(pos == None or pos == Some(3))

    @Test
    def binarySearch31(): Unit \ Assert =
        let pos = Vector.binarySearch(0, Vector#{4, 3, 2, 1, 0});
        assertTrue(pos == None or pos == Some(4))

    /////////////////////////////////////////////////////////////////////////////
    // compare                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def compare01(): Unit \ Assert =
        assertEq(expected = Comparison.EqualTo, (Vector#{}: Vector[Int32]) `Vector.compare` (Vector#{}: Vector[Int32]))

    @Test
    def compare02(): Unit \ Assert =
        assertEq(expected = Comparison.GreaterThan, (Vector#{1}) `Vector.compare` (Vector#{}))

    @Test
    def compare03(): Unit \ Assert =
        assertEq(expected = Comparison.LessThan, (Vector#{}) `Vector.compare` (Vector#{1}))

    @Test
    def compare04(): Unit \ Assert =
        assertEq(expected = Comparison.EqualTo, (Vector#{1}) `Vector.compare` (Vector#{1}))

    @Test
    def compare05(): Unit \ Assert =
        assertEq(expected = Comparison.LessThan, (Vector#{1}) `Vector.compare` (Vector#{2}))

    @Test
    def compare06(): Unit \ Assert =
        assertEq(expected = Comparison.GreaterThan, (Vector#{2}) `Vector.compare` (Vector#{1}))

    @Test
    def compare07(): Unit \ Assert =
        assertEq(expected = Comparison.LessThan, (Vector#{1}) `Vector.compare` (Vector#{1, 2}))

    @Test
    def compare08(): Unit \ Assert =
        assertEq(expected = Comparison.GreaterThan, (Vector#{1, 2}) `Vector.compare` (Vector#{1}))

    @Test
    def compare09(): Unit \ Assert =
        assertEq(expected = Comparison.GreaterThan, (Vector#{1}) `Vector.compare` (Vector#{0, 0}))

    @Test
    def compare10(): Unit \ Assert =
        assertEq(expected = Comparison.LessThan, (Vector#{0, 0}) `Vector.compare` (Vector#{1}))

    @Test
    def compare11(): Unit \ Assert =
        assertEq(expected = Comparison.GreaterThan, (Vector#{1, 2}) `Vector.compare` (Vector#{1, 1}))

    @Test
    def compare12(): Unit \ Assert =
        assertEq(expected = Comparison.LessThan, (Vector#{1, 2}) `Vector.compare` (Vector#{1, 3}))

    @Test
    def compare13(): Unit \ Assert =
        assertEq(expected = Comparison.EqualTo, (Vector#{1, 2, 3, 4}) `Vector.compare` (Vector#{1, 2, 3, 4}))

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toString01(): Unit \ Assert =
        assertEq(expected = "Vector#{}", Vector.toString((Vector#{}: Vector[Int32])))

    @Test
    def toString02(): Unit \ Assert =
        assertEq(expected = "Vector#{1}", Vector.toString(Vector#{1}))

    @Test
    def toString03(): Unit \ Assert =
        assertEq(expected = "Vector#{1, 2, 3, 4}", Vector.toString(Vector#{1, 2, 3, 4}))

    @Test
    def toString04(): Unit \ Assert =
        assertEq(expected = "Vector#{a, b, c, d}", Vector.toString(Vector#{'a', 'b', 'c', 'd'}))

    /////////////////////////////////////////////////////////////////////////////
    // empty                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def empty01(): Unit \ Assert =
        let v = Vector.empty();
        assertEq(expected = 0, Vector.length(v))

    /////////////////////////////////////////////////////////////////////////////
    // singleton                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def singleton01(): Unit \ Assert =
        let v = Vector.singleton(0);
        assertEq(expected = Some(0), Vector.nth(0, v))

    @Test
    def singleton02(): Unit \ Assert =
        let v = Vector.singleton('A');
        assertEq(expected = Some('A'), Vector.nth(0, v))

    /////////////////////////////////////////////////////////////////////////////
    // nth                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def nth01(): Unit \ Assert =
        assertEq(expected = Some(1), Vector.nth(0, Vector#{1, 2, 3}))

    @Test
    def nth02(): Unit \ Assert =
        assertEq(expected = Some(2), Vector.nth(1, Vector#{1, 2, 3}))

    @Test
    def nth03(): Unit \ Assert =
        assertEq(expected = Some(3), Vector.nth(2, Vector#{1, 2, 3}))

    @Test
    def nth04(): Unit \ Assert =
        assertEq(expected = None, Vector.nth(3, Vector#{1, 2, 3}))

    @Test
    def nth05(): Unit \ Assert =
        assertEq(expected = None, Vector.nth(-2, Vector#{1, 2, 3}))

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isEmpty01(): Unit \ Assert =
        assertTrue(Vector.isEmpty(Vector#{}))

    @Test
    def isEmpty02(): Unit \ Assert =
        assertFalse(Vector.isEmpty(Vector#{1}))

    @Test
    def isEmpty03(): Unit \ Assert =
        assertFalse(Vector.isEmpty(Vector#{1, 2, 3}))

    /////////////////////////////////////////////////////////////////////////////
    // nonEmpty                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def nonEmpty01(): Unit \ Assert =
        assertFalse(Vector.nonEmpty(Vector#{}))

    @Test
    def nonEmpty02(): Unit \ Assert =
        assertTrue(Vector.nonEmpty(Vector#{1}))

    @Test
    def nonEmpty03(): Unit \ Assert =
        assertTrue(Vector.nonEmpty(Vector#{1, 2, 3}))

    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def length01(): Unit \ Assert =
        assertEq(expected = 0, Vector.length(Vector#{}))

    @Test
    def length02(): Unit \ Assert =
        assertEq(expected = 1, Vector.length(Vector#{1}))

    @Test
    def length03(): Unit \ Assert =
        assertEq(expected = 3, Vector.length(Vector#{1, 2, 3}))

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def size01(): Unit \ Assert =
        assertEq(expected = 0, Vector.size(Vector#{}))

    @Test
    def size02(): Unit \ Assert =
        assertEq(expected = 1, Vector.size(Vector#{1}))

    @Test
    def size03(): Unit \ Assert =
        assertEq(expected = 3, Vector.size(Vector#{1, 2, 3}))

    /////////////////////////////////////////////////////////////////////////////
    // slice                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def slice01(): Unit \ Assert =
        assertEq(expected = 1, Vector.slice(start = 0, end = 1, Vector#{1, 2, 3}) |> Vector.get(0))

    @Test
    def slice02(): Unit \ Assert =
        assertEq(expected = 2, Vector.slice(start = 1, end = 2, Vector#{1, 2, 3}) |> Vector.get(0))

    @Test
    def slice03(): Unit \ Assert =
        assertEq(expected = 3, Vector.slice(start = 2, end = 3, Vector#{1, 2, 3}) |> Vector.get(0))

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toList01(): Unit \ Assert =
        assertEq(expected = Nil, Vector.toList((Vector#{}: Vector[Int32])))

    @Test
    def toList02(): Unit \ Assert =
        assertEq(expected = 1 :: Nil, Vector.toList(Vector#{1}))

    @Test
    def toList03(): Unit \ Assert =
        assertEq(expected = 1 :: 2 :: Nil, Vector.toList(Vector#{1, 2}))

    /////////////////////////////////////////////////////////////////////////////
    // toNel                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toNel01(): Unit \ Assert =
        assertEq(expected = None, Vector.toNel((Vector#{}: Vector[Unit])))

    @Test
    def toNel02(): Unit \ Assert =
        assertEq(expected = Some(Nel.singleton(1)), Vector.toNel(Vector#{1}))

    @Test
    def toNel03(): Unit \ Assert =
        assertEq(expected = List.toNel(1 :: 2 :: Nil), Vector.toNel(Vector#{1, 2}))

    @Test
    def toNel04(): Unit \ Assert =
        assertEq(expected = List.toNel(1 :: 2 :: 3 :: Nil), Vector.toNel(Vector#{1, 2, 3}))

    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def head01(): Unit \ Assert =
        assertEq(expected = None, Vector.head((Vector#{}: Vector[Int32])))

    @Test
    def head02(): Unit \ Assert =
        assertEq(expected = Some(1), Vector.head(Vector#{1}))

    @Test
    def head03(): Unit \ Assert =
        assertEq(expected = Some(2), Vector.head(Vector#{2, 1}))

    @Test
    def head04(): Unit \ Assert =
        assertEq(expected = Some(3), Vector.head(Vector#{3, 2, 1}))

    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def last01(): Unit \ Assert =
        assertEq(expected = None, (Vector.last(Vector#{}): Option[Unit]))

    @Test
    def last02(): Unit \ Assert =
        assertEq(expected = Some(1), Vector.last(Vector#{1}))

    @Test
    def last03(): Unit \ Assert =
        assertEq(expected = Some(2), Vector.last(Vector#{1, 2}))

    @Test
    def last04(): Unit \ Assert =
        assertEq(expected = Some(3), Vector.last(Vector#{1, 2, 3}))

    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def append01(): Unit \ Assert =
        let v = Vector.append((Vector#{}: Vector[Int32]), (Vector#{}: Vector[Int32]));
        assertEq(expected = (Vector#{}: Vector[Int32]), v)

    @Test
    def append02(): Unit \ Assert =
        let v = Vector.append((Vector#{}: Vector[Int32]), Vector#{1});
        assertEq(expected = Vector#{1}, v)

    @Test
    def append03(): Unit \ Assert =
        let v = Vector.append((Vector#{}: Vector[Int32]), Vector#{1, 2});
        assertEq(expected = Vector#{1, 2}, v)

    @Test
    def append04(): Unit \ Assert =
        let v = Vector.append(Vector#{1}, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{1}, v)

    @Test
    def append05(): Unit \ Assert =
        let v = Vector.append(Vector#{1, 2}, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{1, 2}, v)

    @Test
    def append06(): Unit \ Assert =
        let v = Vector.append(Vector#{1}, Vector#{2});
        assertEq(expected = Vector#{1, 2}, v)

    @Test
    def append07(): Unit \ Assert =
        let v = Vector.append(Vector#{1, 2}, Vector#{3});
        assertEq(expected = Vector#{1, 2, 3}, v)

    @Test
    def append08(): Unit \ Assert =
        let v = Vector.append(Vector#{1}, Vector#{2, 3});
        assertEq(expected = Vector#{1, 2, 3}, v)

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def memberOf01(): Unit \ Assert =
        assertFalse(Vector.memberOf(0, (Vector#{}: Vector[Int32])))

    @Test
    def memberOf02(): Unit \ Assert =
        assertFalse(Vector.memberOf(0, Vector#{1}))

    @Test
    def memberOf03(): Unit \ Assert =
        assertTrue(Vector.memberOf(1, Vector#{1}))

    @Test
    def memberOf04(): Unit \ Assert =
        assertFalse(Vector.memberOf(0, Vector#{1, 2}))

    @Test
    def memberOf05(): Unit \ Assert =
        assertTrue(Vector.memberOf(1, Vector#{1, 2}))

    @Test
    def memberOf06(): Unit \ Assert =
        assertTrue(Vector.memberOf(2, Vector#{1, 2}))

    @Test
    def memberOf07(): Unit \ Assert =
        assertFalse(Vector.memberOf(3, Vector#{1, 2}))

    /////////////////////////////////////////////////////////////////////////////
    // indexOf                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indexOf01(): Unit \ Assert =
        assertEq(expected = None, Vector.indexOf(0, (Vector#{}: Vector[Int32])))

    @Test
    def indexOf02(): Unit \ Assert =
        assertEq(expected = None, Vector.indexOf(0, Vector#{1}))

    @Test
    def indexOf03(): Unit \ Assert =
        assertEq(expected = Some(0), Vector.indexOf(1, Vector#{1}))

    @Test
    def indexOf04(): Unit \ Assert =
        assertEq(expected = None, Vector.indexOf(0, Vector#{1, 2}))

    @Test
    def indexOf05(): Unit \ Assert =
        assertEq(expected = Some(0), Vector.indexOf(1, Vector#{1, 2}))

    @Test
    def indexOf06(): Unit \ Assert =
        assertEq(expected = Some(1), Vector.indexOf(2, Vector#{1, 2}))

    /////////////////////////////////////////////////////////////////////////////
    // indexOfLeft                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indexOfLeft01(): Unit \ Assert =
        assertEq(expected = None, Vector.indexOfLeft(0, (Vector#{}: Vector[Int32])))

    @Test
    def indexOfLeft02(): Unit \ Assert =
        assertEq(expected = None, Vector.indexOfLeft(0, Vector#{1}))

    @Test
    def indexOfLeft03(): Unit \ Assert =
        assertEq(expected = Some(0), Vector.indexOfLeft(1, Vector#{1}))

    @Test
    def indexOfLeft04(): Unit \ Assert =
        assertEq(expected = None, Vector.indexOfLeft(0, Vector#{1, 2}))

    @Test
    def indexOfLeft05(): Unit \ Assert =
        assertEq(expected = Some(0), Vector.indexOfLeft(1, Vector#{1, 2}))

    @Test
    def indexOfLeft06(): Unit \ Assert =
        assertEq(expected = Some(1), Vector.indexOfLeft(2, Vector#{1, 2}))

    @Test
    def indexOfLeft07(): Unit \ Assert =
        assertEq(expected = Some(0), Vector.indexOfLeft(1, Vector#{1, 1}))

    /////////////////////////////////////////////////////////////////////////////
    // indexOfRight                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indexOfRight01(): Unit \ Assert =
        assertEq(expected = None, Vector.indexOfRight(0, (Vector#{}: Vector[Int32])))

    @Test
    def indexOfRight02(): Unit \ Assert =
        assertEq(expected = None, Vector.indexOfRight(0, Vector#{1}))

    @Test
    def indexOfRight03(): Unit \ Assert =
        assertEq(expected = Some(0), Vector.indexOfRight(1, Vector#{1}))

    @Test
    def indexOfRight04(): Unit \ Assert =
        assertEq(expected = None, Vector.indexOfRight(0, Vector#{1, 2}))

    @Test
    def indexOfRight05(): Unit \ Assert =
        assertEq(expected = Some(0), Vector.indexOfRight(1, Vector#{1, 2}))

    @Test
    def indexOfRight06(): Unit \ Assert =
        assertEq(expected = Some(1), Vector.indexOfRight(2, Vector#{1, 2}))

    @Test
    def indexOfRight07(): Unit \ Assert =
        assertEq(expected = Some(1), Vector.indexOfRight(1, Vector#{1, 1}))

    /////////////////////////////////////////////////////////////////////////////
    // indicesOf                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indicesOf01(): Unit \ Assert =
        let v = Vector.indicesOf(0, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def indicesOf02(): Unit \ Assert =
        let v = Vector.indicesOf(0, Vector#{1});
        assertEq(expected = Vector#{}, v)

    @Test
    def indicesOf03(): Unit \ Assert =
        let v = Vector.indicesOf(1, Vector#{1});
        assertEq(expected = Vector#{0}, v)

    @Test
    def indicesOf04(): Unit \ Assert =
        let v = Vector.indicesOf(0, Vector#{1, 2});
        assertEq(expected = Vector#{}, v)

    @Test
    def indicesOf05(): Unit \ Assert =
        let v = Vector.indicesOf(1, Vector#{1, 2});
        assertEq(expected = Vector#{0}, v)

    @Test
    def indicesOf06(): Unit \ Assert =
        let v = Vector.indicesOf(2, Vector#{1, 2});
        assertEq(expected = Vector#{1}, v)

    @Test
    def indicesOf07(): Unit \ Assert =
        let v = Vector.indicesOf(1, Vector#{1, 1});
        assertEq(expected = Vector#{0, 1}, v)

    /////////////////////////////////////////////////////////////////////////////
    // indices                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indices01(): Unit \ Assert = assertEq(expected = Range.Range(0, 0), Vector.indices((Vector#{}: Vector[Int32])))

    @Test
    def indices02(): Unit \ Assert = assertEq(expected = Range.Range(0, 3), Vector.indices(Vector#{1, 2, 3}))

    /////////////////////////////////////////////////////////////////////////////
    // find                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def find01(): Unit \ Assert =
        assertEq(expected = None, Vector.find(i -> i > 2, Vector#{}))

    @Test
    def find02(): Unit \ Assert =
        assertEq(expected = None, Vector.find(i -> i > 2, Vector#{1}))

    @Test
    def find03(): Unit \ Assert =
        assertEq(expected = Some(3), Vector.find(i -> i > 2, Vector#{3}))

    @Test
    def find04(): Unit \ Assert =
        assertEq(expected = None, Vector.find(i -> i > 2, Vector#{1, 2}))

    @Test
    def find05(): Unit \ Assert =
        assertEq(expected = Some(6), Vector.find(i -> i > 2, Vector#{6, -6}))

    @Test
    def find06(): Unit \ Assert =
        assertEq(expected = Some(6), Vector.find(i -> i > 2, Vector#{-6, 6}))

    @Test
    def find07(): Unit \ Assert =
        assertEq(expected = Some(6), Vector.find(i -> i > 2, Vector#{6, 7}))

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findLeft01(): Unit \ Assert =
        assertEq(expected = None, Vector.findLeft(i -> i > 2, Vector#{}))

    @Test
    def findLeft02(): Unit \ Assert =
        assertEq(expected = None, Vector.findLeft(i -> i > 2, Vector#{1}))

    @Test
    def findLeft03(): Unit \ Assert =
        assertEq(expected = Some(3), Vector.findLeft(i -> i > 2, Vector#{3}))

    @Test
    def findLeft04(): Unit \ Assert =
        assertEq(expected = None, Vector.findLeft(i -> i > 2, Vector#{1, 2}))

    @Test
    def findLeft05(): Unit \ Assert =
        assertEq(expected = Some(6), Vector.findLeft(i -> i > 2, Vector#{6, -6}))

    @Test
    def findLeft06(): Unit \ Assert =
        assertEq(expected = Some(6), Vector.findLeft(i -> i > 2, Vector#{-6, 6}))

    @Test
    def findLeft07(): Unit \ Assert =
        assertEq(expected = Some(6), Vector.findLeft(i -> i > 2, Vector#{6, 7}))

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findRight01(): Unit \ Assert =
        assertEq(expected = None, Vector.findRight(i -> i > 2, Vector#{}))

    @Test
    def findRight02(): Unit \ Assert =
        assertEq(expected = None, Vector.findRight(i -> i > 2, Vector#{1}))

    @Test
    def findRight03(): Unit \ Assert =
        assertEq(expected = Some(3), Vector.findRight(i -> i > 2, Vector#{3}))

    @Test
    def findRight04(): Unit \ Assert =
        assertEq(expected = None, Vector.findRight(i -> i > 2, Vector#{1, 2}))

    @Test
    def findRight05(): Unit \ Assert =
        assertEq(expected = Some(6), Vector.findRight(i -> i > 2, Vector#{6, -6}))

    @Test
    def findRight06(): Unit \ Assert =
        assertEq(expected = Some(6), Vector.findRight(i -> i > 2, Vector#{-6, 6}))

    @Test
    def findRight07(): Unit \ Assert =
        assertEq(expected = Some(7), Vector.findRight(i -> i > 2, Vector#{6, 7}))

    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def range01(): Unit \ Assert =
        let v = Vector.range(1, 0);
        assertEq(expected = Vector#{}, v)

    @Test
    def range02(): Unit \ Assert =
        let v = Vector.range(1, 1);
        assertEq(expected = Vector#{}, v)

    @Test
    def range03(): Unit \ Assert =
        let v = Vector.range(1, 2);
        assertEq(expected = Vector#{1}, v)

    @Test
    def range04(): Unit \ Assert =
        let v = Vector.range(1, 3);
        assertEq(expected = Vector#{1, 2}, v)

    @Test
    def range05(): Unit \ Assert =
        let v = Vector.range(1, 4);
        assertEq(expected = Vector#{1, 2, 3}, v)

    @Test
    def range06(): Unit \ Assert =
        let v = Vector.range(-1, 3);
        assertEq(expected = Vector#{-1, 0, 1, 2}, v)

    /////////////////////////////////////////////////////////////////////////////
    // removeAdjDups                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def removeAdjDups01(): Unit \ Assert = {
        let list: Vector[Int32] = Vector#{};
        assertEq(expected = list, Vector.removeAdjDups(list))
    }

    @Test
    def removeAdjDups02(): Unit \ Assert = {
        let list: Vector[Int32] = Vector#{1};
        assertEq(expected = list, Vector.removeAdjDups(list))
    }

    @Test
    def removeAdjDups03(): Unit \ Assert = {
        let list: Vector[Int32] = Vector#{1, 1};
        assertEq(expected = Vector#{1}, Vector.removeAdjDups(list))
    }

    @Test
    def removeAdjDups04(): Unit \ Assert = {
        let list: Vector[Int32] = Vector#{3, 3, 3, 2, 2, 1, 2, 1, 1};
        assertEq(expected = Vector#{3, 2, 1, 2, 1}, Vector.removeAdjDups(list))
    }

    @Test
    def removeAdjDups05(): Unit \ Assert = {
        let list: Vector[(Int32, String)] = Vector#{(1, "a"), (2, "b"), (2, "b"), (3, "c"), (3, "a"), (3, "a"), (3, "c")};
        assertEq(expected = Vector#{(1, "a"), (2, "b"), (3, "c"), (3, "a"), (3, "c")}, Vector.removeAdjDups(list))
    }

    /////////////////////////////////////////////////////////////////////////////
    // removeAdjDups                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def removeAdjDupsWith01(): Unit \ Assert = {
        let list: Vector[Int32] = Vector#{};
        assertEq(expected = list, Vector.removeAdjDupsWith(x -> y -> x == y, list))
    }

    @Test
    def removeAdjDupsWith02(): Unit \ Assert = {
        let list: Vector[Int32] = Vector#{1};
        assertEq(expected = list, Vector.removeAdjDupsWith(x -> y -> x == y, list))
    }

    @Test
    def removeAdjDupsWith03(): Unit \ Assert = {
        let list: Vector[Int32] = Vector#{1, 1};
        assertEq(expected = Vector#{1}, Vector.removeAdjDupsWith(x -> y -> x == y, list))
    }

    @Test
    def removeAdjDupsWith04(): Unit \ Assert = {
        let list: Vector[Int32] = Vector#{3, 3, 3, 2, 2, 1, 2, 1, 1};
        assertEq(expected = Vector#{3, 2, 1, 2, 1}, Vector.removeAdjDupsWith(x -> y -> x == y, list))
    }

    @Test
    def removeAdjDupsWith05(): Unit \ Assert = {
        let list: Vector[(Int32, String)] = Vector#{(1, "a"), (2, "b"), (2, "c"), (3, "c"), (3, "a"), (2, "d")};
        assertEq(expected = Vector#{(1, "a"), (2, "b"), (3, "c"), (2, "d")}, Vector.removeAdjDupsWith(x -> y -> fst(x) == fst(y), list))
    }

    /////////////////////////////////////////////////////////////////////////////
    // repeat                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def repeat01(): Unit \ Assert =
        let v = Vector.repeat(-1, 1);
        assertEq(expected = Vector#{}, v)

    @Test
    def repeat02(): Unit \ Assert =
        let v = Vector.repeat(0, 1);
        assertEq(expected = Vector#{}, v)

    @Test
    def repeat03(): Unit \ Assert =
        let v = Vector.repeat(1, 1);
        assertEq(expected = Vector#{1}, v)

    @Test
    def repeat04(): Unit \ Assert =
        let v = Vector.repeat(2, 1);
        assertEq(expected = Vector#{1, 1}, v)

    @Test
    def repeat05(): Unit \ Assert =
        let v = Vector.repeat(3, 1);
        assertEq(expected = Vector#{1, 1, 1}, v)

    /////////////////////////////////////////////////////////////////////////////
    // scan                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def scan01(): Unit \ Assert =
        let v = Vector.scan((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{});
        assertEq(expected = Vector#{1}, v)

    @Test
    def scan02(): Unit \ Assert =
        let v = Vector.scan((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{false});
        assertEq(expected = Vector#{1, 3}, v)

    @Test
    def scan03(): Unit \ Assert =
        let v = Vector.scan((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{true});
        assertEq(expected = Vector#{1, 2}, v)

    @Test
    def scan04(): Unit \ Assert =
        let v = Vector.scan((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{false, false});
        assertEq(expected = Vector#{1, 3, 5}, v)

    @Test
    def scan05(): Unit \ Assert =
        let v = Vector.scan((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{false, true});
        assertEq(expected = Vector#{1, 3, 4}, v)

    @Test
    def scan06(): Unit \ Assert =
        let v = Vector.scan((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{true, false});
        assertEq(expected = Vector#{1, 2, 4}, v)

    @Test
    def scan07(): Unit \ Assert =
        let v = Vector.scan((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{true, true});
        assertEq(expected = Vector#{1, 2, 3}, v)

    /////////////////////////////////////////////////////////////////////////////
    // scanLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def scanLeft01(): Unit \ Assert =
        let v = Vector.scanLeft((i, b) -> if (b) i + 1 else i + 2, 1, (Vector#{}: Vector[Bool]));
        assertEq(expected = Vector#{1}, v)

    @Test
    def scanLeft02(): Unit \ Assert =
        let v = Vector.scanLeft((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{false});
        assertEq(expected = Vector#{1, 3}, v)

    @Test
    def scanLeft03(): Unit \ Assert =
        let v = Vector.scanLeft((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{true});
        assertEq(expected = Vector#{1, 2}, v)

    @Test
    def scanLeft04(): Unit \ Assert =
        let v = Vector.scanLeft((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{false, false});
        assertEq(expected = Vector#{1, 3, 5}, v)

    @Test
    def scanLeft05(): Unit \ Assert =
        let v = Vector.scanLeft((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{false, true});
        assertEq(expected = Vector#{1, 3, 4}, v)

    @Test
    def scanLeft06(): Unit \ Assert =
        let v = Vector.scanLeft((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{true, false});
        assertEq(expected = Vector#{1, 2, 4}, v)

    @Test
    def scanLeft07(): Unit \ Assert =
        let v = Vector.scanLeft((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{true, true});
        assertEq(expected = Vector#{1, 2, 3}, v)

    /////////////////////////////////////////////////////////////////////////////
    // scanRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def scanRight01(): Unit \ Assert =
        let v = Vector.scanRight((b, i) -> if (b) i + 1 else i + 2, 1, Vector#{});
        assertEq(expected = Vector#{1}, v)

    @Test
    def scanRight02(): Unit \ Assert =
        let v = Vector.scanRight((b, i) -> if (b) i + 1 else i + 2, 1, Vector#{false});
        assertEq(expected = Vector#{3, 1}, v)

    @Test
    def scanRight03(): Unit \ Assert =
        let v = Vector.scanRight((b, i) -> if (b) i + 1 else i + 2, 1, Vector#{true});
        assertEq(expected = Vector#{2, 1}, v)

    @Test
    def scanRight04(): Unit \ Assert =
        let v = Vector.scanRight((b, i) -> if (b) i + 1 else i + 2, 1, Vector#{false, false});
        assertEq(expected = Vector#{5, 3, 1}, v)

    @Test
    def scanRight05(): Unit \ Assert =
        let v = Vector.scanRight((b, i) -> if (b) i + 1 else i + 2, 1, Vector#{false, true});
        assertEq(expected = Vector#{4, 2, 1}, v)

    @Test
    def scanRight06(): Unit \ Assert =
        let v = Vector.scanRight((b, i) -> if (b) i + 1 else i + 2, 1, Vector#{true, false});
        assertEq(expected = Vector#{4, 3, 1}, v)

    @Test
    def scanRight07(): Unit \ Assert =
        let v = Vector.scanRight((b, i) -> if (b) i + 1 else i + 2, 1, Vector#{true, true});
        assertEq(expected = Vector#{3, 2, 1}, v)

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map01(): Unit \ Assert =
        let v = Vector.map(i -> i > 2, Vector#{});
        assertEq(expected = Vector#{}, v)

    @Test
    def map02(): Unit \ Assert =
        let v = Vector.map(i -> i > 2, Vector#{1});
        assertEq(expected = Vector#{false}, v)

    @Test
    def map03(): Unit \ Assert =
        let v = Vector.map(i -> i > 2, Vector#{3});
        assertEq(expected = Vector#{true}, v)

    @Test
    def map04(): Unit \ Assert =
        let v = Vector.map(i -> i > 2, Vector#{1, 2});
        assertEq(expected = Vector#{false, false}, v)

    @Test
    def map05(): Unit \ Assert =
        let v = Vector.map(i -> i > 2, Vector#{1, 8});
        assertEq(expected = Vector#{false, true}, v)

    @Test
    def map06(): Unit \ Assert =
        let v = Vector.map(i -> i > 2, Vector#{8, 1});
        assertEq(expected = Vector#{true, false}, v)

    @Test
    def map07(): Unit \ Assert =
        let v = Vector.map(i -> i > 2, Vector#{7, 8});
        assertEq(expected = Vector#{true, true}, v)

    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapWithIndex01(): Unit \ Assert =
        let v = Vector.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, Vector#{});
        assertEq(expected = Vector#{}, v)

    @Test
    def mapWithIndex02(): Unit \ Assert =
        let v = Vector.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, Vector#{1});
        assertEq(expected = Vector#{false}, v)

    @Test
    def mapWithIndex03(): Unit \ Assert =
        let v = Vector.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, Vector#{3});
        assertEq(expected = Vector#{true}, v)

    @Test
    def mapWithIndex04(): Unit \ Assert =
        let v = Vector.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, Vector#{1, 2});
        assertEq(expected = Vector#{false, true}, v)

    @Test
    def mapWithIndex05(): Unit \ Assert =
        let v = Vector.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, Vector#{1, 8});
        assertEq(expected = Vector#{false, false}, v)

    @Test
    def mapWithIndex06(): Unit \ Assert =
        let v = Vector.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, Vector#{8, 1});
        assertEq(expected = Vector#{true, true}, v)

    @Test
    def mapWithIndex07(): Unit \ Assert =
        let v = Vector.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, Vector#{7, 8});
        assertEq(expected = Vector#{true, false}, v)

    /////////////////////////////////////////////////////////////////////////////
    // ap                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def ap01(): Unit \ Assert =
        let v = Vector.ap((Vector#{} : Vector[Int32 -> Int32]), (Vector#{} : Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def ap02(): Unit \ Assert =
        let v = Vector.ap(Vector#{(x -> x+1)}, Vector#{});
        assertEq(expected = Vector#{}, v)

    @Test
    def ap03(): Unit \ Assert =
        let v = Vector.ap((Vector#{} : Vector[Int32 -> Int32]), Vector#{5});
        assertEq(expected = Vector#{}, v)

    @Test
    def ap04(): Unit \ Assert =
        let v = Vector.ap(Vector#{(x -> x+1)}, Vector#{5});
        assertEq(expected = Vector#{6}, v)

    @Test
    def ap05(): Unit \ Assert =
        let v = Vector.ap(Vector#{(x -> x+1)}, Vector#{0, 5});
        assertEq(expected = Vector#{1, 6}, v)

    @Test
    def ap06(): Unit \ Assert =
        let v = Vector.ap(Vector#{(x -> x+1), (x -> x*2)}, Vector#{0, 4});
        assertEq(expected = Vector#{1, 5, 0, 8}, v)

    /////////////////////////////////////////////////////////////////////////////
    // sequence                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sequence01(): Unit \ Assert =
        let v: Vector[Option[Int32]] = Vector#{};
        assertEq(expected = Some(Vector#{}), Vector.sequence(v))

    @Test
    def sequence02(): Unit \ Assert =
        let v: Vector[Option[Int32]] = Vector#{None};
        assertEq(expected = None, Vector.sequence(v))

    @Test
    def sequence03(): Unit \ Assert =
        let v = Vector#{Some(1)};
        assertEq(expected = Some(Vector#{1}), Vector.sequence(v))

    @Test
    def sequence04(): Unit \ Assert =
        let v: Vector[Option[Int32]] = Vector#{None, None};
        assertEq(expected = None, Vector.sequence(v))

    @Test
    def sequence05(): Unit \ Assert =
        let v = Vector#{Some(1), None};
        assertEq(expected = None, Vector.sequence(v))

    @Test
    def sequence06(): Unit \ Assert =
        let v = Vector#{None, Some(2)};
        assertEq(expected = None, Vector.sequence(v))

    @Test
    def sequence07(): Unit \ Assert =
        let v = Vector#{Some(1), Some(2)};
        assertEq(expected = Some(Vector#{1, 2}), Vector.sequence(v))

    /////////////////////////////////////////////////////////////////////////////
    // traverse                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def traverse01(): Unit \ Assert =
        let v = Vector#{};
        assertEq(expected = Some(Vector#{}), Vector.traverse(x -> if (x > 2) None else Some(x), v))

    @Test
    def traverse02(): Unit \ Assert =
        let v = Vector#{1};
        assertEq(expected = Some(Vector#{1}), Vector.traverse(x -> if (x > 2) None else Some(x), v))

    @Test
    def traverse03(): Unit \ Assert =
        let v = Vector#{1, 2};
        assertEq(expected = Some(Vector#{1, 2}), Vector.traverse(x -> if (x > 2) None else Some(x), v))

    @Test
    def traverse04(): Unit \ Assert =
        let v = Vector#{1, 2, 3};
        assertEq(expected = None, Vector.traverse(x -> if (x > 2) None else Some(x), v))

    @Test
    def traverse05(): Unit \ Assert =
        let v = Vector#{3};
        assertEq(expected = None, Vector.traverse(x -> if (x > 2) None else Some(x), v))

    @Test
    def traverse06(): Unit \ Assert =
        let v = Vector#{1, 2, 1};
        assertEq(expected = Some(Vector#{1, 2, 1}), Vector.traverse(x -> if (x > 2) None else Some(x), v))

    /////////////////////////////////////////////////////////////////////////////
    // zipWithA                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithA01(): Unit \ Assert =
        let v = Vector.zipWithA((x, y) -> if (x + y > 5) None else Some(x + y), Vector#{}, Vector#{});
        assertEq(expected = Some(Vector#{}), v)
    @Test
    def zipWithA02(): Unit \ Assert =
        let v = Vector.zipWithA((x, y) -> if (x + y > 5) None else Some(x + y), Vector#{1}, Vector#{});
        assertEq(expected = Some(Vector#{}), v)

    @Test
    def zipWithA03(): Unit \ Assert =
        let v = Vector.zipWithA((x, y) -> if (x + y > 5) None else Some(x + y), Vector#{1}, Vector#{1});
        assertEq(expected = Some(Vector#{2}), v)

    @Test
    def zipWithA04(): Unit \ Assert =
        let v = Vector.zipWithA((x, y) -> if (x + y > 5) None else Some(x + y), Vector#{1, 2}, Vector#{1, 2});
        assertEq(expected = Some(Vector#{2, 4}), v)

    @Test
    def zipWithA05(): Unit \ Assert =
        let v = Vector.zipWithA((x, y) -> if (x + y > 5) None else Some(x + y), Vector#{1, 2, 3}, Vector#{1, 2, 3});
        assertEq(expected = None, v)

    @Test
    def zipWithA06(): Unit \ Assert =
        let v = Vector.zipWithA((x, y) -> if (x + y > 5) None else Some(x + y), Vector#{1, 2, 3}, Vector#{1, 1, 1});
        assertEq(expected = Some(Vector#{2, 3, 4}), v)

    /////////////////////////////////////////////////////////////////////////////
    // flatMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatMap01(): Unit \ Assert =
        let v = Vector.flatMap(i -> Vector.repeat(i, i), (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def flatMap02(): Unit \ Assert =
        let v = Vector.flatMap(i -> Vector.repeat(i, i), (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def flatMap03(): Unit \ Assert =
        let v = Vector.flatMap(i -> Vector.repeat(i, i), Vector#{1});
        assertEq(expected = Vector#{1}, v)

    @Test
    def flatMap04(): Unit \ Assert =
        let v = Vector.flatMap(i -> Vector.repeat(i, i), Vector#{2});
        assertEq(expected = Vector#{2, 2}, v)

    @Test
    def flatMap05(): Unit \ Assert =
        let v = Vector.flatMap(i -> Vector.repeat(i, i), Vector#{1, 2});
        assertEq(expected = Vector#{1, 2, 2}, v)

    @Test
    def flatMap06(): Unit \ Assert =
        let v = Vector.flatMap(i -> Vector.repeat(i, i), Vector#{2, 3});
        assertEq(expected = Vector#{2, 2, 3, 3, 3}, v)

    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reverse01(): Unit \ Assert =
        let v = Vector.reverse((Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def reverse02(): Unit \ Assert =
        let v = Vector.reverse(Vector#{1});
        assertEq(expected = Vector#{1}, v)

    @Test
    def reverse03(): Unit \ Assert =
        let v = Vector.reverse(Vector#{1, 2});
        assertEq(expected = Vector#{2, 1}, v)

    @Test
    def reverse04(): Unit \ Assert =
        let v = Vector.reverse(Vector#{1, 1});
        assertEq(expected = Vector#{1, 1}, v)

    @Test
    def reverse05(): Unit \ Assert =
        let v = Vector.reverse(Vector#{1, 2, 3});
        assertEq(expected = Vector#{3, 2, 1}, v)

    @Test
    def reverse06(): Unit \ Assert =
        let v = Vector.reverse(Vector#{1, 2, 3, 4});
        assertEq(expected = Vector#{4, 3, 2, 1}, v)

    /////////////////////////////////////////////////////////////////////////////
    // rotateLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def rotateLeft01(): Unit \ Assert =
        let v = Vector.rotateLeft(0, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def rotateLeft02(): Unit \ Assert =
        let v = Vector.rotateLeft(1, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def rotateLeft03(): Unit \ Assert =
        let v = Vector.rotateLeft(0, Vector#{1});
        assertEq(expected = Vector#{1}, v)

    @Test
    def rotateLeft04(): Unit \ Assert =
        let v = Vector.rotateLeft(0, Vector#{1, 2});
        assertEq(expected = Vector#{1, 2}, v)

    @Test
    def rotateLeft05(): Unit \ Assert =
        let v = Vector.rotateLeft(1, Vector#{1, 2});
        assertEq(expected = Vector#{2, 1}, v)

    @Test
    def rotateLeft06(): Unit \ Assert =
        let v = Vector.rotateLeft(2, Vector#{1, 2});
        assertEq(expected = Vector#{1, 2}, v)

    @Test
    def rotateLeft07(): Unit \ Assert =
        let v = Vector.rotateLeft(3, Vector#{1, 2});
        assertEq(expected = Vector#{2, 1}, v)

    @Test
    def rotateLeft08(): Unit \ Assert =
        let v = Vector.rotateLeft(-1, Vector#{1, 2});
        assertEq(expected = Vector#{2, 1}, v)

    @Test
    def rotateLeft09(): Unit \ Assert =
        let v = Vector.rotateLeft(0, Vector#{1, 2, 3});
        assertEq(expected = Vector#{1, 2, 3}, v)

    @Test
    def rotateLeft10(): Unit \ Assert =
        let v = Vector.rotateLeft(1, Vector#{1, 2, 3});
        assertEq(expected = Vector#{2, 3, 1}, v)

    @Test
    def rotateLeft11(): Unit \ Assert =
        let v = Vector.rotateLeft(2, Vector#{1, 2, 3});
        assertEq(expected = Vector#{3, 1, 2}, v)

    /////////////////////////////////////////////////////////////////////////////
    // rotateRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def rotateRight01(): Unit \ Assert =
        let v = Vector.rotateRight(0, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def rotateRight02(): Unit \ Assert =
        let v = Vector.rotateRight(1, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def rotateRight03(): Unit \ Assert =
        let v = Vector.rotateRight(0, Vector#{1});
        assertEq(expected = Vector#{1}, v)

    @Test
    def rotateRight04(): Unit \ Assert =
        let v = Vector.rotateRight(0, Vector#{1, 2});
        assertEq(expected = Vector#{1, 2}, v)

    @Test
    def rotateRight05(): Unit \ Assert =
        let v = Vector.rotateRight(1, Vector#{1, 2});
        assertEq(expected = Vector#{2, 1}, v)

    @Test
    def rotateRight06(): Unit \ Assert =
        let v = Vector.rotateRight(2, Vector#{1, 2});
        assertEq(expected = Vector#{1, 2}, v)

    @Test
    def rotateRight07(): Unit \ Assert =
        let v = Vector.rotateRight(3, Vector#{1, 2});
        assertEq(expected = Vector#{2, 1}, v)

    @Test
    def rotateRight08(): Unit \ Assert =
        let v = Vector.rotateRight(-1, Vector#{1, 2});
        assertEq(expected = Vector#{2, 1}, v)

    @Test
    def rotateRight09(): Unit \ Assert =
        let v = Vector.rotateRight(0, Vector#{1, 2, 3});
        assertEq(expected = Vector#{1, 2, 3}, v)

    @Test
    def rotateRight10(): Unit \ Assert =
        let v = Vector.rotateRight(1, Vector#{1, 2, 3});
        assertEq(expected = Vector#{3, 1, 2}, v)

    @Test
    def rotateRight11(): Unit \ Assert =
        let v = Vector.rotateRight(2, Vector#{1, 2, 3});
        assertEq(expected = Vector#{2, 3, 1}, v)

    /////////////////////////////////////////////////////////////////////////////
    // update                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def update01(): Unit \ Assert =
        let v = Vector.update(0, 2, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def update02(): Unit \ Assert =
        let v = Vector.update(-1, 2, Vector#{1});
        assertEq(expected = Vector#{1}, v)

    @Test
    def update03(): Unit \ Assert =
        let v = Vector.update(0, 2, Vector#{1});
        assertEq(expected = Vector#{2}, v)

    @Test
    def update04(): Unit \ Assert =
        let v = Vector.update(1, 2, Vector#{1});
        assertEq(expected = Vector#{1}, v)

    @Test
    def update05(): Unit \ Assert =
        let v = Vector.update(0, 5, Vector#{1, 2});
        assertEq(expected = Vector#{5, 2}, v)

    @Test
    def update06(): Unit \ Assert =
        let v = Vector.update(1, 5, Vector#{1, 2});
        assertEq(expected = Vector#{1, 5}, v)

    @Test
    def update07(): Unit \ Assert =
        let v = Vector.update(2, 5, Vector#{1, 2});
        assertEq(expected = Vector#{1, 2}, v)

    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def replace01(): Unit \ Assert =
        let v = Vector.replace(src = 3, dst = 4, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def replace02(): Unit \ Assert =
        let v = Vector.replace(src = 3, dst = 4, Vector#{1});
        assertEq(expected = Vector#{1}, v)

    @Test
    def replace03(): Unit \ Assert =
        let v = Vector.replace(src = 3, dst = 4, Vector#{3});
        assertEq(expected = Vector#{4}, v)

    @Test
    def replace04(): Unit \ Assert =
        let v = Vector.replace(src = 3, dst = 4, Vector#{4});
        assertEq(expected = Vector#{4}, v)

    @Test
    def replace05(): Unit \ Assert =
        let v = Vector.replace(src = 3, dst = 4, Vector#{1, 2});
        assertEq(expected = Vector#{1, 2}, v)

    @Test
    def replace06(): Unit \ Assert =
        let v = Vector.replace(src = 3, dst = 4, Vector#{1, 3});
        assertEq(expected = Vector#{1, 4}, v)

    @Test
    def replace07(): Unit \ Assert =
        let v = Vector.replace(src = 3, dst = 4, Vector#{3, 4});
        assertEq(expected = Vector#{4, 4}, v)

    @Test
    def replace08(): Unit \ Assert =
        let v = Vector.replace(src = 3, dst = 4, Vector#{3, 3});
        assertEq(expected = Vector#{4, 4}, v)

    /////////////////////////////////////////////////////////////////////////////
    // patch                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def patch01(): Unit \ Assert =
        let v = Vector.patch(0, 0, (Vector#{}: Vector[Int32]), (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def patch02(): Unit \ Assert =
        let v = Vector.patch(0, 2, Vector#{1, 2}, Vector#{});
        assertEq(expected = Vector#{}, v)

    @Test
    def patch03(): Unit \ Assert =
        let v = Vector.patch(0, 2, Vector#{}, Vector#{1, 2});
        assertEq(expected = Vector#{1, 2}, v)

    @Test
    def patch04(): Unit \ Assert =
        let v = Vector.patch(-3, 3, Vector#{1, 2, 4}, Vector#{1, 2});
        assertEq(expected = Vector#{1, 2}, v)

    @Test
    def patch05(): Unit \ Assert =
        let v = Vector.patch(2, 3, Vector#{1, 2, 4}, Vector#{1, 2});
        assertEq(expected = Vector#{1, 2}, v)

    @Test
    def patch06(): Unit \ Assert =
        let v = Vector.patch(0, 0, Vector#{}, Vector#{1});
        assertEq(expected = Vector#{1}, v)

    @Test
    def patch07(): Unit \ Assert =
        let v = Vector.patch(1, 0, Vector#{2}, Vector#{1});
        assertEq(expected = Vector#{1}, v)

    @Test
    def patch08(): Unit \ Assert =
        let v = Vector.patch(0, 1, Vector#{2}, Vector#{1});
        assertEq(expected = Vector#{2}, v)

    @Test
    def patch09(): Unit \ Assert =
        let v = Vector.patch(0, 2, Vector#{2, 4}, Vector#{1});
        assertEq(expected = Vector#{2}, v)

    @Test
    def patch10(): Unit \ Assert =
        let v = Vector.patch(-1, 2, Vector#{2, 4}, Vector#{1});
        assertEq(expected = Vector#{4}, v)

    @Test
    def patch11(): Unit \ Assert =
        let v = Vector.patch(-1, 2, Vector#{3, 4}, Vector#{1, 2});
        assertEq(expected = Vector#{4, 2}, v)

    @Test
    def patch12(): Unit \ Assert =
        let v = Vector.patch(1, 2, Vector#{3, 4}, Vector#{1, 2});
        assertEq(expected = Vector#{1, 3}, v)

    @Test
    def patch13(): Unit \ Assert =
        let v = Vector.patch(-2, 2, Vector#{3, 4}, Vector#{1, 2});
        assertEq(expected = Vector#{1, 2}, v)

    @Test
    def patch14(): Unit \ Assert =
        let v = Vector.patch(2, 2, Vector#{3, 4}, Vector#{1, 2});
        assertEq(expected = Vector#{1, 2}, v)

    @Test
    def patch15(): Unit \ Assert =
        let v = Vector.patch(1, 1, Vector#{3}, Vector#{1, 2});
        assertEq(expected = Vector#{1, 3}, v)

    @Test
    def patch16(): Unit \ Assert =
        let v = Vector.patch(0, 2, Vector#{3, 4}, Vector#{1, 2});
        assertEq(expected = Vector#{3, 4}, v)

    @Test
    def patch17(): Unit \ Assert =
        let v = Vector.patch(0, 1, Vector#{4}, Vector#{1, 2, 3});
        assertEq(expected = Vector#{4, 2, 3}, v)

    @Test
    def patch18(): Unit \ Assert =
        let v = Vector.patch(1, 1, Vector#{4}, Vector#{1, 2, 3});
        assertEq(expected = Vector#{1, 4, 3}, v)

    @Test
    def patch19(): Unit \ Assert =
        let v = Vector.patch(2, 1, Vector#{4}, Vector#{1, 2, 3});
        assertEq(expected = Vector#{1, 2, 4}, v)

    @Test
    def patch20(): Unit \ Assert =
        let v = Vector.patch(0, 2, Vector#{4, 5}, Vector#{1, 2, 3});
        assertEq(expected = Vector#{4, 5, 3}, v)

    @Test
    def patch21(): Unit \ Assert =
        let v = Vector.patch(1, 2, Vector#{4, 5}, Vector#{1, 2, 3});
        assertEq(expected = Vector#{1, 4, 5}, v)

    @Test
    def patch22(): Unit \ Assert =
        let v = Vector.patch(0, 2, Vector#{4, 5, 6}, Vector#{1, 2, 3});
        assertEq(expected = Vector#{4, 5, 3}, v)

    @Test
    def patch23(): Unit \ Assert =
        let v = Vector.patch(0, 3, Vector#{4, 5, 6}, Vector#{1, 2, 3});
        assertEq(expected = Vector#{4, 5, 6}, v)

    @Test
    def patch24(): Unit \ Assert =
        let v = Vector.patch(2, 4, Vector#{14, 15, 16, 17}, Vector#{1, 2, 3, 4, 5, 6, 7});
        assertEq(expected = Vector#{1, 2, 14, 15, 16, 17, 7}, v)

    @Test
    def patch25(): Unit \ Assert =
        let v = Vector.patch(-2, 4, Vector#{14, 15, 16, 17}, Vector#{1, 2, 3, 4, 5, 6, 7});
        assertEq(expected = Vector#{16, 17, 3, 4, 5, 6, 7}, v)

    @Test
    def patch26(): Unit \ Assert =
        let v = Vector.patch(4, 5, Vector#{14, 15, 16, 17}, Vector#{1, 2, 3, 4, 5, 6, 7});
        assertEq(expected = Vector#{1, 2, 3, 4, 14, 15, 16}, v)

    @Test
    def patch27(): Unit \ Assert =
        let v = Vector.patch(4, 2, Vector#{14, 15, 16, 17}, Vector#{1, 2, 3, 4, 5, 6, 7});
        assertEq(expected = Vector#{1, 2, 3, 4, 14, 15, 7}, v)

    @Test
    def patch28(): Unit \ Assert =
        let v = Vector.patch(-1, 10, Vector#{-1, -2, -3, -4, -5, -6, -7, -8}, Vector#{1, 2, 3, 4, 5, 6, 7});
        assertEq(expected = Vector#{-2, -3, -4, -5, -6, -7, -8}, v)

    /////////////////////////////////////////////////////////////////////////////
    // intersperse                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def intersperse01(): Unit \ Assert =
        let v = Vector.intersperse(11, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def intersperse02(): Unit \ Assert =
        let v = Vector.intersperse(11, Vector#{1});
        assertEq(expected = Vector#{1}, v)

    @Test
    def intersperse03(): Unit \ Assert =
        let v = Vector.intersperse(11, Vector#{1, 2});
        assertEq(expected = Vector#{1, 11, 2}, v)

    @Test
    def intersperse04(): Unit \ Assert =
        let v = Vector.intersperse(11, Vector#{1, 2, 3});
        assertEq(expected = Vector#{1, 11, 2, 11, 3}, v)

    @Test
    def intersperse05(): Unit \ Assert =
        let v = Vector.intersperse(11, Vector#{1, 2, 3, 4});
        assertEq(expected = Vector#{1, 11, 2, 11, 3, 11, 4}, v)

    /////////////////////////////////////////////////////////////////////////////
    // intercalate                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def intercalate01(): Unit \ Assert =
        let v = Vector.intercalate((Vector#{}: Vector[Int32]), (Vector#{}: Vector[Vector[Int32]]));
        assertEq(expected = Vector#{}, v)

    @Test
    def intercalate02(): Unit \ Assert =
        let v = Vector.intercalate((Vector#{}: Vector[Int32]), Vector#{Vector#{1}});
        assertEq(expected = Vector#{1}, v)

    @Test
    def intercalate03(): Unit \ Assert =
        let v = Vector.intercalate(Vector#{11, 12, 13}, (Vector#{}: Vector[Vector[Int32]]));
        assertEq(expected = Vector#{}, v)

    @Test
    def intercalate04(): Unit \ Assert =
        let v = Vector.intercalate((Vector#{}: Vector[Int32]), Vector#{Vector#{1}, Vector#{2, 3}});
        assertEq(expected = Vector#{1, 2, 3}, v)

    @Test
    def intercalate05(): Unit \ Assert =
        let v = Vector.intercalate(Vector#{11, 12, 13}, Vector#{Vector#{1}, Vector#{2, 3}});
        assertEq(expected = Vector#{1, 11, 12, 13, 2, 3}, v)

    @Test
    def intercalate06(): Unit \ Assert =
        let v = Vector.intercalate((Vector#{}: Vector[Int32]), Vector#{Vector#{1}, Vector#{2, 3}, Vector#{4}});
        assertEq(expected = Vector#{1, 2, 3, 4}, v)

    @Test
    def intercalate07(): Unit \ Assert =
        let v = Vector.intercalate(Vector#{11, 12, 13}, Vector#{Vector#{1}, Vector#{2, 3}, Vector#{4}});
        assertEq(expected = Vector#{1, 11, 12, 13, 2, 3, 11, 12, 13, 4}, v)

    /////////////////////////////////////////////////////////////////////////////
    // transpose                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def transpose01(): Unit \ Assert =
        let v = Vector.transpose((Vector#{}: Vector[Vector[Int32]]));
        assertEq(expected = Vector#{}, v)

    @Test
    def transpose02(): Unit \ Assert =
        let v: Vector[Vector[Int32]] = Vector.transpose(Vector#{Vector#{}});
        assertEq(expected = Vector#{Vector#{}}, v)

    @Test
    def transpose03(): Unit \ Assert =
        let v: Vector[Vector[Int32]] = Vector.transpose(Vector#{Vector#{}, Vector#{}});
        assertEq(expected = Vector#{Vector#{}, Vector#{}}, v)

    @Test
    def transpose04(): Unit \ Assert =
        let v: Vector[Vector[Int32]] = Vector.transpose(Vector#{Vector#{}, Vector#{}, Vector#{}});
        assertEq(expected = Vector#{Vector#{}, Vector#{}, Vector#{}}, v)

    @Test
    def transpose05(): Unit \ Assert =
        let v = Vector.transpose(Vector#{Vector#{1}});
        assertEq(expected = Vector#{Vector#{1}}, v)

    @Test
    def transpose06(): Unit \ Assert =
        let v = Vector.transpose(Vector#{Vector#{1, 2}});
        assertEq(expected = Vector#{Vector#{1}, Vector#{2}}, v)

    @Test
    def transpose07(): Unit \ Assert =
        let v = Vector.transpose(Vector#{Vector#{1, 2, 3}});
        assertEq(expected = Vector#{Vector#{1}, Vector#{2}, Vector#{3}}, v)

    @Test
    def transpose08(): Unit \ Assert =
        let v = Vector.transpose(Vector#{Vector#{1, 2, 3, 4}});
        assertEq(expected = Vector#{Vector#{1}, Vector#{2}, Vector#{3}, Vector#{4}}, v)

    @Test
    def transpose09(): Unit \ Assert =
        let v = Vector.transpose(Vector#{Vector#{1}, Vector#{2}});
        assertEq(expected = Vector#{Vector#{1, 2}}, v)

    @Test
    def transpose10(): Unit \ Assert =
        let v = Vector.transpose(Vector#{Vector#{1}, Vector#{2}, Vector#{3}});
        assertEq(expected = Vector#{Vector#{1, 2, 3}}, v)

    @Test
    def transpose11(): Unit \ Assert =
        let v = Vector.transpose(Vector#{Vector#{1}, Vector#{2}, Vector#{3}, Vector#{4}});
        assertEq(expected = Vector#{Vector#{1, 2, 3, 4}}, v)

    @Test
    def transpose12(): Unit \ Assert =
        let v = Vector.transpose(Vector#{Vector#{1, 2}, Vector#{3, 4}});
        assertEq(expected = Vector#{Vector#{1, 3}, Vector#{2, 4}}, v)

    @Test
    def transpose13(): Unit \ Assert =
        let v = Vector.transpose(Vector#{Vector#{1, 2, 3}, Vector#{4, 5, 6}});
        assertEq(expected = Vector#{Vector#{1, 4}, Vector#{2, 5}, Vector#{3, 6}}, v)

    @Test
    def transpose14(): Unit \ Assert =
        let v = Vector.transpose(Vector#{Vector#{1, 2, 3, 4}, Vector#{5, 6, 7, 8}});
        assertEq(expected = Vector#{Vector#{1, 5}, Vector#{2, 6}, Vector#{3, 7}, Vector#{4, 8}}, v)

    @Test
    def transpose15(): Unit \ Assert =
        let v = Vector.transpose(Vector#{Vector#{1, 2, 3, 4, 5}, Vector#{6, 7, 8, 9, 10}});
        assertEq(expected = Vector#{Vector#{1, 6}, Vector#{2, 7}, Vector#{3, 8}, Vector#{4, 9}, Vector#{5, 10}}, v)

    @Test
    def transpose16(): Unit \ Assert =
        let v = Vector.transpose(Vector#{Vector#{1, 2}, Vector#{3, 4}, Vector#{5, 6}});
        assertEq(expected = Vector#{Vector#{1, 3, 5}, Vector#{2, 4, 6}}, v)

    @Test
    def transpose17(): Unit \ Assert =
        let v = Vector.transpose(Vector#{Vector#{1, 2}, Vector#{3, 4}, Vector#{5, 6}, Vector#{7, 8}});
        assertEq(expected = Vector#{Vector#{1, 3, 5, 7}, Vector#{2, 4, 6, 8}}, v)

    @Test
    def transpose18(): Unit \ Assert =
        let v = Vector.transpose(Vector#{Vector#{1, 2}, Vector#{3, 4}, Vector#{5, 6}, Vector#{7, 8}, Vector#{9, 10}});
        assertEq(expected = Vector#{Vector#{1, 3, 5, 7, 9}, Vector#{2, 4, 6, 8, 10}}, v)

    @Test
    def transpose19(): Unit \ Assert =
        let v = Vector.transpose(Vector#{Vector#{1, 2, 3}, Vector#{4, 5, 6}, Vector#{7, 8, 9}});
        assertEq(expected = Vector#{Vector#{1, 4, 7}, Vector#{2, 5, 8}, Vector#{3, 6, 9}}, v)

    @Test
    def transpose20(): Unit \ Assert =
        let v = Vector.transpose(Vector#{Vector#{1, 2, 3}, Vector#{4, 5}, Vector#{7, 8, 9}});
        assertEq(expected = Vector#{Vector#{1, 2, 3}, Vector#{4, 5}, Vector#{7, 8, 9}}, v)

    @Test
    def transpose21(): Unit \ Assert =
        let v = Vector.transpose(Vector#{Vector#{1, 2, 3}, Vector#{}, Vector#{7, 8, 9}});
        assertEq(expected = Vector#{Vector#{1, 2, 3}, Vector#{}, Vector#{7, 8, 9}}, v)

    @Test
    def transpose22(): Unit \ Assert =
        let v = Vector.transpose(Vector#{Vector#{1, 2, 3}, Vector#{4, 5, 6}, Vector#{7, 8, 9, 10}});
        assertEq(expected = Vector#{Vector#{1, 2, 3}, Vector#{4, 5, 6}, Vector#{7, 8, 9, 10}}, v)

    /////////////////////////////////////////////////////////////////////////////
    // isPrefixOf                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isPrefixOf01(): Unit \ Assert =
        assertTrue(Vector.isPrefixOf((Vector#{}: Vector[Int32]), (Vector#{}: Vector[Int32])))

    @Test
    def isPrefixOf02(): Unit \ Assert =
        assertFalse(Vector.isPrefixOf(Vector#{1}, (Vector#{}: Vector[Int32])))

    @Test
    def isPrefixOf03(): Unit \ Assert =
        assertTrue(Vector.isPrefixOf((Vector#{}: Vector[Int32]), Vector#{1}))

    @Test
    def isPrefixOf04(): Unit \ Assert =
        assertTrue(Vector.isPrefixOf(Vector#{1}, Vector#{1}))

    @Test
    def isPrefixOf05(): Unit \ Assert =
        assertFalse(Vector.isPrefixOf(Vector#{2}, Vector#{1}))

    @Test
    def isPrefixOf06(): Unit \ Assert =
        assertFalse(Vector.isPrefixOf(Vector#{1, 2}, Vector#{1}))

    @Test
    def isPrefixOf07(): Unit \ Assert =
        assertTrue(Vector.isPrefixOf((Vector#{}: Vector[Int32]), Vector#{1, 2}))

    @Test
    def isPrefixOf08(): Unit \ Assert =
        assertTrue(Vector.isPrefixOf(Vector#{1}, Vector#{1, 2}))

    @Test
    def isPrefixOf09(): Unit \ Assert =
        assertFalse(Vector.isPrefixOf(Vector#{2}, Vector#{1, 2}))

    @Test
    def isPrefixOf10(): Unit \ Assert =
        assertTrue(Vector.isPrefixOf(Vector#{1, 2}, Vector#{1, 2}))

    @Test
    def isPrefixOf11(): Unit \ Assert =
        assertFalse(Vector.isPrefixOf(Vector#{1, 3}, Vector#{1, 2}))

    @Test
    def isPrefixOf12(): Unit \ Assert =
        assertFalse(Vector.isPrefixOf(Vector#{1, 2, 3}, Vector#{1, 2}))

    @Test
    def isPrefixOf13(): Unit \ Assert =
        assertFalse(Vector.isPrefixOf(Vector#{1, 2, 3}, Vector#{89, 11, 1, 2, 3, 4}))

    /////////////////////////////////////////////////////////////////////////////
    // isInfixOf                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isInfixOf01(): Unit \ Assert =
        assertTrue(Vector.isInfixOf((Vector#{}: Vector[Int32]), (Vector#{}: Vector[Int32])))

    @Test
    def isInfixOf02(): Unit \ Assert =
        assertFalse(Vector.isInfixOf(Vector#{1}, (Vector#{}: Vector[Int32])))

    @Test
    def isInfixOf03(): Unit \ Assert =
        assertTrue(Vector.isInfixOf((Vector#{}: Vector[Int32]), Vector#{1}))

    @Test
    def isInfixOf04(): Unit \ Assert =
        assertTrue(Vector.isInfixOf(Vector#{1}, Vector#{1}))

    @Test
    def isInfixOf05(): Unit \ Assert =
        assertFalse(Vector.isInfixOf(Vector#{2}, Vector#{1}))

    @Test
    def isInfixOf06(): Unit \ Assert =
        assertFalse(Vector.isInfixOf(Vector#{1, 2}, Vector#{1}))

    @Test
    def isInfixOf07(): Unit \ Assert =
        assertTrue(Vector.isInfixOf((Vector#{}: Vector[Int32]), Vector#{1, 2}))

    @Test
    def isInfixOf08(): Unit \ Assert =
        assertTrue(Vector.isInfixOf(Vector#{1}, Vector#{1, 2}))

    @Test
    def isInfixOf09(): Unit \ Assert =
        assertTrue(Vector.isInfixOf(Vector#{2}, Vector#{1, 2}))

    @Test
    def isInfixOf10(): Unit \ Assert =
        assertTrue(Vector.isInfixOf(Vector#{1, 2}, Vector#{1, 2}))

    @Test
    def isInfixOf11(): Unit \ Assert =
        assertFalse(Vector.isInfixOf(Vector#{1, 3}, Vector#{1, 2}))

    @Test
    def isInfixOf12(): Unit \ Assert =
        assertFalse(Vector.isInfixOf(Vector#{1, 2, 3}, Vector#{1, 2}))

    @Test
    def isInfixOf13(): Unit \ Assert =
        assertTrue(Vector.isInfixOf(Vector#{1, 2, 3}, Vector#{89, 11, 1, 2, 3, 4}))

    /////////////////////////////////////////////////////////////////////////////
    // isSuffixOf                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isSuffixOf01(): Unit \ Assert =
        assertTrue(Vector.isSuffixOf((Vector#{}: Vector[Int32]), (Vector#{}: Vector[Int32])))

    @Test
    def isSuffixOf02(): Unit \ Assert =
        assertFalse(Vector.isSuffixOf(Vector#{1}, (Vector#{}: Vector[Int32])))

    @Test
    def isSuffixOf03(): Unit \ Assert =
        assertTrue(Vector.isSuffixOf((Vector#{}: Vector[Int32]), Vector#{1}))

    @Test
    def isSuffixOf04(): Unit \ Assert =
        assertTrue(Vector.isSuffixOf(Vector#{1}, Vector#{1}))

    @Test
    def isSuffixOf05(): Unit \ Assert =
        assertFalse(Vector.isSuffixOf(Vector#{2}, Vector#{1}))

    @Test
    def isSuffixOf06(): Unit \ Assert =
        assertFalse(Vector.isSuffixOf(Vector#{1, 2}, Vector#{1}))

    @Test
    def isSuffixOf07(): Unit \ Assert =
        assertTrue(Vector.isSuffixOf((Vector#{}: Vector[Int32]), Vector#{1, 2}))

    @Test
    def isSuffixOf08(): Unit \ Assert =
        assertFalse(Vector.isSuffixOf(Vector#{1}, Vector#{1, 2}))

    @Test
    def isSuffixOf09(): Unit \ Assert =
        assertTrue(Vector.isSuffixOf(Vector#{2}, Vector#{1, 2}))

    @Test
    def isSuffixOf10(): Unit \ Assert =
        assertTrue(Vector.isSuffixOf(Vector#{1, 2}, Vector#{1, 2}))

    @Test
    def isSuffixOf11(): Unit \ Assert =
        assertFalse(Vector.isSuffixOf(Vector#{1, 3}, Vector#{1, 2}))

    @Test
    def isSuffixOf12(): Unit \ Assert =
        assertFalse(Vector.isSuffixOf(Vector#{1, 2, 3}, Vector#{1, 2}))

    @Test
    def isSuffixOf13(): Unit \ Assert =
        assertTrue(Vector.isSuffixOf(Vector#{1, 2, 3}, Vector#{89, 11, 1, 2, 3}))

    /////////////////////////////////////////////////////////////////////////////
    // fold                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fold01(): Unit \ Assert =
        assertEq(expected = (), Vector.fold((Vector#{}: Vector[Unit])))

    @Test
    def fold02(): Unit \ Assert =
        assertEq(expected = "a", Vector.fold(Vector#{"a"}))

    @Test
    def fold03(): Unit \ Assert =
        assertEq(expected = "abc", Vector.fold(Vector#{"a", "b", "c"}))

    @Test
    def fold04(): Unit \ Assert =
        assertEq(expected = ("yesno", "noyes"), Vector.fold(Vector#{("yes", "no"), ("no", "yes")}))

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeft01(): Unit \ Assert =
        assertEq(expected = 100, Vector.foldLeft((i, e) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, Vector#{}))

    @Test
    def foldLeft02(): Unit \ Assert =
        assertEq(expected = 198, Vector.foldLeft((i, e) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, Vector#{1}))

    @Test
    def foldLeft03(): Unit \ Assert =
        assertEq(expected = 196, Vector.foldLeft((i, e) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, Vector#{1, 2}))

    @Test
    def foldLeft04(): Unit \ Assert =
        assertEq(expected = 386, Vector.foldLeft((i, e) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, Vector#{1, 2, 3}))

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRight01(): Unit \ Assert =
        assertEq(expected = 100, Vector.foldRight((e, i) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, Vector#{}))

    @Test
    def foldRight02(): Unit \ Assert =
        assertEq(expected = 198, Vector.foldRight((e, i) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, Vector#{1}))

    @Test
    def foldRight03(): Unit \ Assert =
        assertEq(expected = 194, Vector.foldRight((e, i) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, Vector#{1, 2}))

    @Test
    def foldRight04(): Unit \ Assert =
        assertEq(expected = 382, Vector.foldRight((e, i) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, Vector#{1, 2, 3}))

    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldMap01(): Unit \ Assert =
        assertEq(expected = 0, Vector.foldMap(x -> 2 * x, Vector#{}))

    @Test
    def foldMap02(): Unit \ Assert =
        assertEq(expected = 6, Vector.foldMap(x -> 2 * x, Vector#{1, 2}))

    @Test
    def foldMap03(): Unit \ Assert =
        assertEq(expected = "b", Vector.foldMap(x -> if (x == "a") "b" else x, Vector#{"a"}))

    @Test
    def foldMap04(): Unit \ Assert =
        assertEq(expected = "abb", Vector.foldMap(x -> if (x == "c") "b" else x, Vector#{"a", "b", "c"}))

    @Test
    def foldMap05(): Unit \ Assert =
        assertEq(expected = "123", Vector.foldMap(Int32.toString, Vector#{1, 2, 3}))

    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceLeft01(): Unit \ Assert =
        assertEq(expected = None, Vector.reduceLeft((a, b) -> a - b, (Vector#{}: Vector[Int32])))

    @Test
    def reduceLeft02(): Unit \ Assert =
        assertEq(expected = Some(1), Vector.reduceLeft((a, b) -> a - b, Vector#{1}))

    @Test
    def reduceLeft03(): Unit \ Assert =
        assertEq(expected = Some(-1), Vector.reduceLeft((a, b) -> a - b, Vector#{1, 2}))

    @Test
    def reduceLeft04(): Unit \ Assert =
        assertEq(expected = Some(-4), Vector.reduceLeft((a, b) -> a - b, Vector#{1, 2, 3}))

    @Test
    def reduceLeft05(): Unit \ Assert =
        assertEq(expected = Some(-8), Vector.reduceLeft((a, b) -> a - b, Vector#{1, 2, 3, 4}))

    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceRight01(): Unit \ Assert =
        assertEq(expected = None, Vector.reduceRight((a, b) -> a - b, (Vector#{}: Vector[Int32])))

    @Test
    def reduceRight02(): Unit \ Assert =
        assertEq(expected = Some(1), Vector.reduceRight((a, b) -> a - b, Vector#{1}))

    @Test
    def reduceRight03(): Unit \ Assert =
        assertEq(expected = Some(-1), Vector.reduceRight((a, b) -> a - b, Vector#{1, 2}))

    @Test
    def reduceRight04(): Unit \ Assert =
        assertEq(expected = Some(2), Vector.reduceRight((a, b) -> a - b, Vector#{1, 2, 3}))

    @Test
    def reduceRight05(): Unit \ Assert =
        assertEq(expected = Some(-2), Vector.reduceRight((a, b) -> a - b, Vector#{1, 2, 3, 4}))

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def count01(): Unit \ Assert =
        assertEq(expected = 0, Vector.count(i -> i > 3, Vector#{}))

    @Test
    def count02(): Unit \ Assert =
        assertEq(expected = 0, Vector.count(i -> i > 3, Vector#{1}))

    @Test
    def count03(): Unit \ Assert =
        assertEq(expected = 1, Vector.count(i -> i > 3, Vector#{4}))

    @Test
    def count04(): Unit \ Assert =
        assertEq(expected = 0, Vector.count(i -> i > 3, Vector#{1, 2}))

    @Test
    def count05(): Unit \ Assert =
        assertEq(expected = 1, Vector.count(i -> i > 3, Vector#{1, 8}))

    @Test
    def count06(): Unit \ Assert =
        assertEq(expected = 1, Vector.count(i -> i > 3, Vector#{8, 1}))

    @Test
    def count07(): Unit \ Assert =
        assertEq(expected = 2, Vector.count(i -> i > 3, Vector#{6, 7}))

    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatten01(): Unit \ Assert =
        let v = Vector.flatten((Vector#{}: Vector[Vector[Int32]]));
        assertEq(expected = Vector#{}, v)

    @Test
    def flatten02(): Unit \ Assert =
        let v = Vector.flatten((Vector#{Vector#{}}: Vector[Vector[Int32]]));
        assertEq(expected = Vector#{}, v)

    @Test
    def flatten03(): Unit \ Assert =
        let v = Vector.flatten(Vector#{Vector#{1}});
        assertEq(expected = Vector#{1}, v)

    @Test
    def flatten04(): Unit \ Assert =
        let v = Vector.flatten(Vector#{Vector#{1, 2}});
        assertEq(expected = Vector#{1, 2}, v)

    @Test
    def flatten05(): Unit \ Assert =
        let v = Vector.flatten((Vector#{Vector#{}, Vector#{}}: Vector[Vector[Int32]]));
        assertEq(expected = Vector#{}, v)

    @Test
    def flatten06(): Unit \ Assert =
        let v = Vector.flatten(Vector#{Vector#{1}, Vector#{}});
        assertEq(expected = Vector#{1}, v)

    @Test
    def flatten07(): Unit \ Assert =
        let v = Vector.flatten(Vector#{Vector#{}, Vector#{1}});
        assertEq(expected = Vector#{1}, v)

    @Test
    def flatten08(): Unit \ Assert =
        let v = Vector.flatten(Vector#{Vector#{1}, Vector#{2}});
        assertEq(expected = Vector#{1, 2}, v)

    @Test
    def flatten09(): Unit \ Assert =
        let v = Vector.flatten(Vector#{Vector#{1, 2}, Vector#{3, 4, 5}});
        assertEq(expected = Vector#{1, 2, 3, 4, 5}, v)

    @Test
    def flatten10(): Unit \ Assert =
        let v = Vector.flatten(Vector#{Vector#{1}, Vector#{2, 3}, Vector#{4}});
        assertEq(expected = Vector#{1, 2, 3, 4}, v)

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def exists01(): Unit \ Assert =
        assertFalse(Vector.exists(i -> i > 3, (Vector#{}: Vector[Int32])))

    @Test
    def exists02(): Unit \ Assert =
        assertFalse(Vector.exists(i -> i > 3, Vector#{1}))

    @Test
    def exists03(): Unit \ Assert =
        assertTrue(Vector.exists(i -> i > 3, Vector#{5}))

    @Test
    def exists04(): Unit \ Assert =
        assertFalse(Vector.exists(i -> i > 3, Vector#{1, 2}))

    @Test
    def exists05(): Unit \ Assert =
        assertTrue(Vector.exists(i -> i > 3, Vector#{1, 6}))

    @Test
    def exists06(): Unit \ Assert =
        assertTrue(Vector.exists(i -> i > 3, Vector#{6, 1}))

    @Test
    def exists07(): Unit \ Assert =
        assertTrue(Vector.exists(i -> i > 3, Vector#{16, 6}))

    @Test
    def exists08(): Unit \ Assert =
        assertFalse(Vector.exists(i -> i > 3, Vector#{1, -9, 3}))

    @Test
    def exists09(): Unit \ Assert =
        assertTrue(Vector.exists(i -> i > 3, Vector#{1, 9, 3}))

    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forAll01(): Unit \ Assert =
        assertTrue(Vector.forAll(i -> i > 3, (Vector#{}: Vector[Int32])))

    @Test
    def forAll02(): Unit \ Assert =
        assertFalse(Vector.forAll(i -> i > 3, Vector#{1}))

    @Test
    def forAll03(): Unit \ Assert =
        assertTrue(Vector.forAll(i -> i > 3, Vector#{5}))

    @Test
    def forAll04(): Unit \ Assert =
        assertFalse(Vector.forAll(i -> i > 3, Vector#{1, 2}))

    @Test
    def forAll05(): Unit \ Assert =
        assertFalse(Vector.forAll(i -> i > 3, Vector#{1, 6}))

    @Test
    def forAll06(): Unit \ Assert =
        assertFalse(Vector.forAll(i -> i > 3, Vector#{6, 1}))

    @Test
    def forAll07(): Unit \ Assert =
        assertTrue(Vector.forAll(i -> i > 3, Vector#{16, 6}))

    @Test
    def forAll08(): Unit \ Assert =
        assertFalse(Vector.forAll(i -> i > 3, Vector#{1, -9, 3}))

    @Test
    def forAll09(): Unit \ Assert =
        assertFalse(Vector.forAll(i -> i > 3, Vector#{1, 9, 3}))

    @Test
    def forAll10(): Unit \ Assert =
        assertTrue(Vector.forAll(i -> i > 3, Vector#{11, 9, 31}))

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filter01(): Unit \ Assert =
        let v = Vector.filter(i -> i > 3, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def filter02(): Unit \ Assert =
        let v = Vector.filter(i -> i > 3, Vector#{2});
        assertEq(expected = Vector#{}, v)

    @Test
    def filter03(): Unit \ Assert =
        let v = Vector.filter(i -> i > 3, Vector#{4});
        assertEq(expected = Vector#{4}, v)

    @Test
    def filter04(): Unit \ Assert =
        let v = Vector.filter(i -> i > 3, Vector#{1, 3});
        assertEq(expected = Vector#{}, v)

    @Test
    def filter05(): Unit \ Assert =
        let v = Vector.filter(i -> i > 3, Vector#{1, 8});
        assertEq(expected = Vector#{8}, v)

    @Test
    def filter06(): Unit \ Assert =
        let v = Vector.filter(i -> i > 3, Vector#{8, 1});
        assertEq(expected = Vector#{8}, v)

    @Test
    def filter07(): Unit \ Assert =
        let v = Vector.filter(i -> i > 3, Vector#{8, 9});
        assertEq(expected = Vector#{8, 9}, v)

    @Test
    def filter08(): Unit \ Assert =
        let v = Vector.filter(i -> i > 3, Vector#{1, 4, 11, 2, -22, 17});
        assertEq(expected = Vector#{4, 11, 17}, v)

    /////////////////////////////////////////////////////////////////////////////
    // partition                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def partition01(): Unit \ Assert =
        let (a, b) = Vector.partition(i -> i > 3, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, a);
        assertEq(expected = Vector#{}, b)

    @Test
    def partition02(): Unit \ Assert =
        let (a, b) = Vector.partition(i -> i > 3, Vector#{1});
        assertEq(expected = Vector#{}, a);
        assertEq(expected = Vector#{1}, b)

    @Test
    def partition03(): Unit \ Assert =
        let (a, b) = Vector.partition(i -> i > 3, Vector#{4});
        assertEq(expected = Vector#{4}, a);
        assertEq(expected = Vector#{}, b)

    @Test
    def partition04(): Unit \ Assert =
        let (a, b) = Vector.partition(i -> i > 3, Vector#{1, 2});
        assertEq(expected = Vector#{}, a);
        assertEq(expected = Vector#{1, 2}, b)

    @Test
    def partition05(): Unit \ Assert =
        let (a, b) = Vector.partition(i -> i > 3, Vector#{1, 5});
        assertEq(expected = Vector#{5}, a);
        assertEq(expected = Vector#{1}, b)

    @Test
    def partition06(): Unit \ Assert =
        let (a, b) = Vector.partition(i -> i > 3, Vector#{5, 1});
        assertEq(expected = Vector#{5}, a);
        assertEq(expected = Vector#{1}, b)

    @Test
    def partition07(): Unit \ Assert =
        let (a, b) = Vector.partition(i -> i > 3, Vector#{5, 8});
        assertEq(expected = Vector#{5, 8}, a);
        assertEq(expected = (Vector#{}: Vector[Int32]), b)

    @Test
    def partition08(): Unit \ Assert =
        let (a, b) = Vector.partition(i -> i > 3, Vector#{4, -3, -5, 1, 2, 16, 7, 1, 7});
        assertEq(expected = Vector#{4, 16, 7, 7}, a);
        assertEq(expected = Vector#{-3, -5, 1, 2, 1}, b)

    /////////////////////////////////////////////////////////////////////////////
    // span                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def span01(): Unit \ Assert =
        let (a, b) = Vector.span(i -> i > 3, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, a);
        assertEq(expected = Vector#{}, b)

    @Test
    def span02(): Unit \ Assert =
        let (a, b) = Vector.span(i -> i > 3, Vector#{1});
        assertEq(expected = Vector#{}, a);
        assertEq(expected = Vector#{1}, b)

    @Test
    def span03(): Unit \ Assert =
        let (a, b) = Vector.span(i -> i > 3, Vector#{4});
        assertEq(expected = Vector#{4}, a);
        assertEq(expected = Vector#{}, b)

    @Test
    def span04(): Unit \ Assert =
        let (a, b) = Vector.span(i -> i > 3, Vector#{1, 2});
        assertEq(expected = Vector#{}, a);
        assertEq(expected = Vector#{1, 2}, b)

    @Test
    def span05(): Unit \ Assert =
        let (a, b) = Vector.span(i -> i > 3, Vector#{1, 5});
        assertEq(expected = Vector#{}, a);
        assertEq(expected = Vector#{1, 5}, b)

    @Test
    def span06(): Unit \ Assert =
        let (a, b) = Vector.span(i -> i > 3, Vector#{5, 1});
        assertEq(expected = Vector#{5}, a);
        assertEq(expected = Vector#{1}, b)

    @Test
    def span07(): Unit \ Assert =
        let (a, b) = Vector.span(i -> i > 3, Vector#{5, 8});
        assertEq(expected = Vector#{5, 8}, a);
        assertEq(expected = Vector#{}, b)

    @Test
    def span08(): Unit \ Assert =
        let (a, b) = Vector.span(i -> i > 3, Vector#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7});
        assertEq(expected = Vector#{4, 6}, a);
        assertEq(expected = Vector#{-3, 11, -5, 1, 2, 16, 7, 1, 7}, b)

    /////////////////////////////////////////////////////////////////////////////
    // drop                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def drop01(): Unit \ Assert =
        let v = Vector.drop(-1, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def drop02(): Unit \ Assert =
        let v = Vector.drop(0, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def drop03(): Unit \ Assert =
        let v = Vector.drop(1, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def drop04(): Unit \ Assert =
        let v = Vector.drop(-1, Vector#{1});
        assertEq(expected = Vector#{1}, v)

    @Test
    def drop05(): Unit \ Assert =
        let v = Vector.drop(0, Vector#{1});
        assertEq(expected = Vector#{1}, v)

    @Test
    def drop06(): Unit \ Assert =
        let v = Vector.drop(1, Vector#{1});
        assertEq(expected = Vector#{}, v)

    @Test
    def drop07(): Unit \ Assert =
        let v = Vector.drop(2, Vector#{1});
        assertEq(expected = Vector#{}, v)

    @Test
    def drop08(): Unit \ Assert =
        let v = Vector.drop(0, Vector#{1, 2});
        assertEq(expected = Vector#{1, 2}, v)

    @Test
    def drop09(): Unit \ Assert =
        let v = Vector.drop(1, Vector#{1, 2});
        assertEq(expected = Vector#{2}, v)

    @Test
    def drop10(): Unit \ Assert =
        let v = Vector.drop(2, Vector#{1, 2});
        assertEq(expected = Vector#{}, v)

    @Test
    def drop11(): Unit \ Assert =
        let v = Vector.drop(2, Vector#{1, 2, 3, 4, 5, 6});
        assertEq(expected = Vector#{3, 4, 5, 6}, v)

    @Test
    def drop12(): Unit \ Assert =
        let v = Vector.drop(4, Vector#{1, 2, 3, 4, 5, 6});
        assertEq(expected = Vector#{5, 6}, v)

    /////////////////////////////////////////////////////////////////////////////
    // dropLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropLeft01(): Unit \ Assert =
        let v = Vector.dropLeft(-1, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def dropLeft02(): Unit \ Assert =
        let v = Vector.dropLeft(0, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def dropLeft03(): Unit \ Assert =
        let v = Vector.dropLeft(1, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def dropLeft04(): Unit \ Assert =
        let v = Vector.dropLeft(-1, Vector#{1});
        assertEq(expected = Vector#{1}, v)

    @Test
    def dropLeft05(): Unit \ Assert =
        let v = Vector.dropLeft(0, Vector#{1});
        assertEq(expected = Vector#{1}, v)

    @Test
    def dropLeft06(): Unit \ Assert =
        let v = Vector.dropLeft(1, Vector#{1});
        assertEq(expected = Vector#{}, v)

    @Test
    def dropLeft07(): Unit \ Assert =
        let v = Vector.dropLeft(2, Vector#{1});
        assertEq(expected = Vector#{}, v)

    @Test
    def dropLeft08(): Unit \ Assert =
        let v = Vector.dropLeft(0, Vector#{1, 2});
        assertEq(expected = Vector#{1, 2}, v)

    @Test
    def dropLeft09(): Unit \ Assert =
        let v = Vector.dropLeft(1, Vector#{1, 2});
        assertEq(expected = Vector#{2}, v)

    @Test
    def dropLeft10(): Unit \ Assert =
        let v = Vector.dropLeft(2, Vector#{1, 2});
        assertEq(expected = Vector#{}, v)

    @Test
    def dropLeft11(): Unit \ Assert =
        let v = Vector.dropLeft(2, Vector#{1, 2, 3, 4, 5, 6});
        assertEq(expected = Vector#{3, 4, 5, 6}, v)

    @Test
    def dropLeft12(): Unit \ Assert =
        let v = Vector.dropLeft(4, Vector#{1, 2, 3, 4, 5, 6});
        assertEq(expected = Vector#{5, 6}, v)

    /////////////////////////////////////////////////////////////////////////////
    // dropRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropRight01(): Unit \ Assert =
        let v = Vector.dropRight(-1, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def dropRight02(): Unit \ Assert =
        let v = Vector.dropRight(0, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def dropRight03(): Unit \ Assert =
        let v = Vector.dropRight(1, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def dropRight04(): Unit \ Assert =
        let v = Vector.dropRight(-1, Vector#{1});
        assertEq(expected = Vector#{1}, v)

    @Test
    def dropRight05(): Unit \ Assert =
        let v = Vector.dropRight(0, Vector#{1});
        assertEq(expected = Vector#{1}, v)

    @Test
    def dropRight06(): Unit \ Assert =
        let v = Vector.dropRight(1, Vector#{1});
        assertEq(expected = Vector#{}, v)

    @Test
    def dropRight07(): Unit \ Assert =
        let v = Vector.dropRight(2, Vector#{1});
        assertEq(expected = Vector#{}, v)

    @Test
    def dropRight08(): Unit \ Assert =
        let v = Vector.dropRight(0, Vector#{1, 2});
        assertEq(expected = Vector#{1, 2}, v)

    @Test
    def dropRight09(): Unit \ Assert =
        let v = Vector.dropRight(1, Vector#{1, 2});
        assertEq(expected = Vector#{1}, v)

    @Test
    def dropRight10(): Unit \ Assert =
        let v = Vector.dropRight(2, Vector#{1, 2});
        assertEq(expected = Vector#{}, v)

    @Test
    def dropRight11(): Unit \ Assert =
        let v = Vector.dropRight(2, Vector#{1, 2, 3, 4, 5, 6});
        assertEq(expected = Vector#{1, 2, 3, 4}, v)

    @Test
    def dropRight12(): Unit \ Assert =
        let v = Vector.dropRight(4, Vector#{1, 2, 3, 4, 5, 6});
        assertEq(expected = Vector#{1, 2}, v)

    /////////////////////////////////////////////////////////////////////////////
    // dropWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropWhile01(): Unit \ Assert =
        let v = Vector.dropWhile(i -> i > 3, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def dropWhile02(): Unit \ Assert =
        let v = Vector.dropWhile(i -> i > 3, Vector#{1});
        assertEq(expected = Vector#{1}, v)

    @Test
    def dropWhile03(): Unit \ Assert =
        let v = Vector.dropWhile(i -> i > 3, Vector#{4});
        assertEq(expected = Vector#{}, v)

    @Test
    def dropWhile04(): Unit \ Assert =
        let v = Vector.dropWhile(i -> i > 3, Vector#{1, 2});
        assertEq(expected = Vector#{1, 2}, v)

    @Test
    def dropWhile05(): Unit \ Assert =
        let v = Vector.dropWhile(i -> i > 3, Vector#{1, 5});
        assertEq(expected = Vector#{1, 5}, v)

    @Test
    def dropWhile06(): Unit \ Assert =
        let v = Vector.dropWhile(i -> i > 3, Vector#{5, 1});
        assertEq(expected = Vector#{1}, v)

    @Test
    def dropWhile07(): Unit \ Assert =
        let v = Vector.dropWhile(i -> i > 3, Vector#{5, 8});
        assertEq(expected = Vector#{}, v)

    @Test
    def dropWhile08(): Unit \ Assert =
        let v = Vector.dropWhile(i -> i > 3, Vector#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7});
        assertEq(expected = Vector#{-3, 11, -5, 1, 2, 16, 7, 1, 7}, v)

    /////////////////////////////////////////////////////////////////////////////
    // dropWhileLeft                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropWhileLeft01(): Unit \ Assert =
        let v = Vector.dropWhileLeft(i -> i > 3, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def dropWhileLeft02(): Unit \ Assert =
        let v = Vector.dropWhileLeft(i -> i > 3, Vector#{1});
        assertEq(expected = Vector#{1}, v)

    @Test
    def dropWhileLeft03(): Unit \ Assert =
        let v = Vector.dropWhileLeft(i -> i > 3, Vector#{4});
        assertEq(expected = Vector#{}, v)

    @Test
    def dropWhileLeft04(): Unit \ Assert =
        let v = Vector.dropWhileLeft(i -> i > 3, Vector#{1, 2});
        assertEq(expected = Vector#{1, 2}, v)

    @Test
    def dropWhileLeft05(): Unit \ Assert =
        let v = Vector.dropWhileLeft(i -> i > 3, Vector#{1, 5});
        assertEq(expected = Vector#{1, 5}, v)

    @Test
    def dropWhileLeft06(): Unit \ Assert =
        let v = Vector.dropWhileLeft(i -> i > 3, Vector#{5, 1});
        assertEq(expected = Vector#{1}, v)

    @Test
    def dropWhileLeft07(): Unit \ Assert =
        let v = Vector.dropWhileLeft(i -> i > 3, Vector#{5, 8});
        assertEq(expected = Vector#{}, v)

    @Test
    def dropWhileLeft08(): Unit \ Assert =
        let v = Vector.dropWhileLeft(i -> i > 3, Vector#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7});
        assertEq(expected = Vector#{-3, 11, -5, 1, 2, 16, 7, 1, 7}, v)

    /////////////////////////////////////////////////////////////////////////////
    // dropWhileRight                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropWhileRight01(): Unit \ Assert =
        let v = Vector.dropWhileRight(i -> i > 3, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def dropWhileRight02(): Unit \ Assert =
        let v = Vector.dropWhileRight(i -> i > 3, Vector#{1});
        assertEq(expected = Vector#{1}, v)

    @Test
    def dropWhileRight03(): Unit \ Assert =
        let v = Vector.dropWhileRight(i -> i > 3, Vector#{4});
        assertEq(expected = Vector#{}, v)

    @Test
    def dropWhileRight04(): Unit \ Assert =
        let v = Vector.dropWhileRight(i -> i > 3, Vector#{1, 2});
        assertEq(expected = Vector#{1, 2}, v)

    @Test
    def dropWhileRight05(): Unit \ Assert =
        let v = Vector.dropWhileRight(i -> i > 3, Vector#{1, 5});
        assertEq(expected = Vector#{1}, v)

    @Test
    def dropWhileRight06(): Unit \ Assert =
        let v = Vector.dropWhileRight(i -> i > 3, Vector#{5, 1});
        assertEq(expected = Vector#{5, 1}, v)

    @Test
    def dropWhileRight07(): Unit \ Assert =
        let v = Vector.dropWhileRight(i -> i > 3, Vector#{5, 8});
        assertEq(expected = Vector#{}, v)

    @Test
    def dropWhileRight08(): Unit \ Assert =
        let v = Vector.dropWhileRight(i -> i > 3, Vector#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7});
        assertEq(expected = Vector#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1}, v)

    /////////////////////////////////////////////////////////////////////////////
    // take                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def take01(): Unit \ Assert =
        let v = Vector.take(-1, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def take02(): Unit \ Assert =
        let v = Vector.take(0, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def take03(): Unit \ Assert =
        let v = Vector.take(1, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def take04(): Unit \ Assert =
        let v = Vector.take(-1, Vector#{1});
        assertEq(expected = Vector#{}, v)

    @Test
    def take05(): Unit \ Assert =
        let v = Vector.take(0, Vector#{1});
        assertEq(expected = Vector#{}, v)

    @Test
    def take06(): Unit \ Assert =
        let v = Vector.take(1, Vector#{1});
        assertEq(expected = Vector#{1}, v)

    @Test
    def take07(): Unit \ Assert =
        let v = Vector.take(2, Vector#{1});
        assertEq(expected = Vector#{1}, v)

    @Test
    def take08(): Unit \ Assert =
        let v = Vector.take(0, Vector#{1, 2});
        assertEq(expected = Vector#{}, v)

    @Test
    def take09(): Unit \ Assert =
        let v = Vector.take(1, Vector#{1, 2});
        assertEq(expected = Vector#{1}, v)

    @Test
    def take10(): Unit \ Assert =
        let v = Vector.take(2, Vector#{1, 2});
        assertEq(expected = Vector#{1, 2}, v)

    @Test
    def take11(): Unit \ Assert =
        let v = Vector.take(2, Vector#{1, 2, 3, 4, 5, 6});
        assertEq(expected = Vector#{1, 2}, v)

    @Test
    def take12(): Unit \ Assert =
        let v = Vector.take(4, Vector#{1, 2, 3, 4, 5, 6});
        assertEq(expected = Vector#{1, 2, 3, 4}, v)

    /////////////////////////////////////////////////////////////////////////////
    // takeLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeLeft01(): Unit \ Assert =
        let v = Vector.takeLeft(-1, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def takeLeft02(): Unit \ Assert =
        let v = Vector.takeLeft(0, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def takeLeft03(): Unit \ Assert =
        let v = Vector.takeLeft(1, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def takeLeft04(): Unit \ Assert =
        let v = Vector.takeLeft(-1, Vector#{1});
        assertEq(expected = Vector#{}, v)

    @Test
    def takeLeft05(): Unit \ Assert =
        let v = Vector.takeLeft(0, Vector#{1});
        assertEq(expected = Vector#{}, v)

    @Test
    def takeLeft06(): Unit \ Assert =
        let v = Vector.takeLeft(1, Vector#{1});
        assertEq(expected = Vector#{1}, v)

    @Test
    def takeLeft07(): Unit \ Assert =
        let v = Vector.takeLeft(2, Vector#{1});
        assertEq(expected = Vector#{1}, v)

    @Test
    def takeLeft08(): Unit \ Assert =
        let v = Vector.takeLeft(0, Vector#{1, 2});
        assertEq(expected = Vector#{}, v)

    @Test
    def takeLeft09(): Unit \ Assert =
        let v = Vector.takeLeft(1, Vector#{1, 2});
        assertEq(expected = Vector#{1}, v)

    @Test
    def takeLeft10(): Unit \ Assert =
        let v = Vector.takeLeft(2, Vector#{1, 2});
        assertEq(expected = Vector#{1, 2}, v)

    @Test
    def takeLeft11(): Unit \ Assert =
        let v = Vector.takeLeft(2, Vector#{1, 2, 3, 4, 5, 6});
        assertEq(expected = Vector#{1, 2}, v)

    @Test
    def takeLeft12(): Unit \ Assert =
        let v = Vector.takeLeft(4, Vector#{1, 2, 3, 4, 5, 6});
        assertEq(expected = Vector#{1, 2, 3, 4}, v)

    /////////////////////////////////////////////////////////////////////////////
    // takeRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeRight01(): Unit \ Assert =
        let v = Vector.takeRight(-1, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def takeRight02(): Unit \ Assert =
        let v = Vector.takeRight(0, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def takeRight03(): Unit \ Assert =
        let v = Vector.takeRight(1, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def takeRight04(): Unit \ Assert =
        let v = Vector.takeRight(-1, Vector#{1});
        assertEq(expected = Vector#{}, v)

    @Test
    def takeRight05(): Unit \ Assert =
        let v = Vector.takeRight(0, Vector#{1});
        assertEq(expected = Vector#{}, v)

    @Test
    def takeRight06(): Unit \ Assert =
        let v = Vector.takeRight(1, Vector#{1});
        assertEq(expected = Vector#{1}, v)

    @Test
    def takeRight07(): Unit \ Assert =
        let v = Vector.takeRight(2, Vector#{1});
        assertEq(expected = Vector#{1}, v)

    @Test
    def takeRight08(): Unit \ Assert =
        let v = Vector.takeRight(0, Vector#{1, 2});
        assertEq(expected = Vector#{}, v)

    @Test
    def takeRight09(): Unit \ Assert =
        let v = Vector.takeRight(1, Vector#{1, 2});
        assertEq(expected = Vector#{2}, v)

    @Test
    def takeRight10(): Unit \ Assert =
        let v = Vector.takeRight(2, Vector#{1, 2});
        assertEq(expected = Vector#{1, 2}, v)

    @Test
    def takeRight11(): Unit \ Assert =
        let v = Vector.takeRight(2, Vector#{1, 2, 3, 4, 5, 6});
        assertEq(expected = Vector#{5, 6}, v)

    @Test
    def takeRight12(): Unit \ Assert =
        let v = Vector.takeRight(4, Vector#{1, 2, 3, 4, 5, 6});
        assertEq(expected = Vector#{3, 4, 5, 6}, v)

    /////////////////////////////////////////////////////////////////////////////
    // takeWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeWhile01(): Unit \ Assert =
        let v = Vector.takeWhile(i -> i > 3, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def takeWhile02(): Unit \ Assert =
        let v = Vector.takeWhile(i -> i > 3, Vector#{1});
        assertEq(expected = Vector#{}, v)

    @Test
    def takeWhile03(): Unit \ Assert =
        let v = Vector.takeWhile(i -> i > 3, Vector#{4});
        assertEq(expected = Vector#{4}, v)

    @Test
    def takeWhile04(): Unit \ Assert =
        let v = Vector.takeWhile(i -> i > 3, Vector#{1, 2});
        assertEq(expected = Vector#{}, v)

    @Test
    def takeWhile05(): Unit \ Assert =
        let v = Vector.takeWhile(i -> i > 3, Vector#{1, 5});
        assertEq(expected = Vector#{}, v)

    @Test
    def takeWhile06(): Unit \ Assert =
        let v = Vector.takeWhile(i -> i > 3, Vector#{5, 1});
        assertEq(expected = Vector#{5}, v)

    @Test
    def takeWhile07(): Unit \ Assert =
        let v = Vector.takeWhile(i -> i > 3, Vector#{5, 8});
        assertEq(expected = Vector#{5, 8}, v)

    @Test
    def takeWhile08(): Unit \ Assert =
        let v = Vector.takeWhile(i -> i > 3, Vector#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7});
        assertEq(expected = Vector#{4, 6}, v)

    /////////////////////////////////////////////////////////////////////////////
    // takeWhileLeft                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeWhileLeft01(): Unit \ Assert =
        let v = Vector.takeWhileLeft(i -> i > 3, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def takeWhileLeft02(): Unit \ Assert =
        let v = Vector.takeWhileLeft(i -> i > 3, Vector#{1});
        assertEq(expected = Vector#{}, v)

    @Test
    def takeWhileLeft03(): Unit \ Assert =
        let v = Vector.takeWhileLeft(i -> i > 3, Vector#{4});
        assertEq(expected = Vector#{4}, v)

    @Test
    def takeWhileLeft04(): Unit \ Assert =
        let v = Vector.takeWhileLeft(i -> i > 3, Vector#{1, 2});
        assertEq(expected = Vector#{}, v)

    @Test
    def takeWhileLeft05(): Unit \ Assert =
        let v = Vector.takeWhileLeft(i -> i > 3, Vector#{1, 5});
        assertEq(expected = Vector#{}, v)

    @Test
    def takeWhileLeft06(): Unit \ Assert =
        let v = Vector.takeWhileLeft(i -> i > 3, Vector#{5, 1});
        assertEq(expected = Vector#{5}, v)

    @Test
    def takeWhileLeft07(): Unit \ Assert =
        let v = Vector.takeWhileLeft(i -> i > 3, Vector#{5, 8});
        assertEq(expected = Vector#{5, 8}, v)

    @Test
    def takeWhileLeft08(): Unit \ Assert =
        let v = Vector.takeWhileLeft(i -> i > 3, Vector#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7});
        assertEq(expected = Vector#{4, 6}, v)

    /////////////////////////////////////////////////////////////////////////////
    // takeWhileRight                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeWhileRight01(): Unit \ Assert =
        let v = Vector.takeWhileRight(i -> i > 3, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def takeWhileRight02(): Unit \ Assert =
        let v = Vector.takeWhileRight(i -> i > 3, Vector#{1});
        assertEq(expected = Vector#{}, v)

    @Test
    def takeWhileRight03(): Unit \ Assert =
        let v = Vector.takeWhileRight(i -> i > 3, Vector#{4});
        assertEq(expected = Vector#{4}, v)

    @Test
    def takeWhileRight04(): Unit \ Assert =
        let v = Vector.takeWhileRight(i -> i > 3, Vector#{1, 2});
        assertEq(expected = Vector#{}, v)

    @Test
    def takeWhileRight05(): Unit \ Assert =
        let v = Vector.takeWhileRight(i -> i > 3, Vector#{1, 5});
        assertEq(expected = Vector#{5}, v)

    @Test
    def takeWhileRight06(): Unit \ Assert =
        let v = Vector.takeWhileRight(i -> i > 3, Vector#{5, 1});
        assertEq(expected = Vector#{}, v)

    @Test
    def takeWhileRight07(): Unit \ Assert =
        let v = Vector.takeWhileRight(i -> i > 3, Vector#{5, 8});
        assertEq(expected = Vector#{5, 8}, v)

    @Test
    def takeWhileRight08(): Unit \ Assert =
        let v = Vector.takeWhileRight(i -> i > 3, Vector#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7});
        assertEq(expected = Vector#{7}, v)

    /////////////////////////////////////////////////////////////////////////////
    // splitAt                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def splitAt01(): Unit \ Assert =
        let v = (Vector#{}: Vector[Int32]);
        assertEq(expected = (Vector#{}, Vector#{}), Vector.splitAt(0, v))

    @Test
    def splitAt02(): Unit \ Assert =
        let v = Vector#{1, 5, 19, 3};
        assertEq(expected = (Vector#{1, 5, 19}, Vector#{3}), Vector.splitAt(3, v))

    @Test
    def splitAt03(): Unit \ Assert =
        let v = Vector#{1, 5, 19, 3};
        assertEq(expected = (Vector#{}, v), Vector.splitAt(-3, v))

    @Test
    def splitAt04(): Unit \ Assert =
        let v = Vector#{1, 5, 19, 3};
        assertEq(expected = (v, Vector#{}), Vector.splitAt(5, v))

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
    def groupBy01(): Unit \ Assert = assertEq(expected = Vector#{}, Vector.groupBy(identity, (Vector#{}: Vector[A])))

    @Test
    def groupBy02(): Unit \ Assert = assertEq(expected = Vector#{Vector#{A.A(1, 1)}}, Vector.groupBy(identity, Vector#{A.A(1, 1)}))

    @Test
    def groupBy03(): Unit \ Assert = assertEq(expected = Vector#{Vector#{A.A(1, 1), A.A(1, 2), A.A(1, 1)}, Vector#{A.A(2, 1), A.A(2, 4)}}, Vector.groupBy(identity, Vector#{A.A(1, 1), A.A(2, 1), A.A(1, 2), A.A(1, 1), A.A(2, 4)}))

    /////////////////////////////////////////////////////////////////////////////
    // groupWith                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def groupWith01(): Unit \ Assert = assertEq(expected = Vector#{}, Vector.groupWith((a, b) -> a > 3 and b > 3, (Vector#{}: Vector[Int32])))

    @Test
    def groupWith02(): Unit \ Assert = assertEq(expected = Vector#{Vector#{1}}, Vector.groupWith((a, b) -> a > 3 and b > 3, Vector#{1}))

    @Test
    def groupWith03(): Unit \ Assert = assertEq(expected = Vector#{Vector#{1}, Vector#{4}}, Vector.groupWith((a, b) -> a > 3 and b > 3, Vector#{1, 4}))

    @Test
    def groupWith04(): Unit \ Assert = assertEq(expected = Vector#{Vector#{4, 9}}, Vector.groupWith((a, b) -> a > 3 and b > 3, Vector#{4, 9}))

    @Test
    def groupWith05(): Unit \ Assert =
        assertEq(expected = Vector#{Vector#{1, 2}, Vector#{4, 7, 6, 9, 4, 4, 8, 16}, Vector#{-1, -5, 0}}, Vector.groupWith((a, b) -> (a > 3 and b > 3) or (a < 1 and b < 1) or ((a >= 1 and a <= 3) and (b >= 1 and b <= 3)), Vector#{1, 4, 7, 6, 9, 2, 4, -1, 4, -5, 8, 16, 0}))

    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zip01(): Unit \ Assert =
        let v = Vector.zip((Vector#{}: Vector[Int32]), (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def zip02(): Unit \ Assert =
        let v = Vector.zip(Vector#{1}, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def zip03(): Unit \ Assert =
        let v = Vector.zip((Vector#{}: Vector[Int32]), Vector#{2});
        assertEq(expected = Vector#{}, v)

    @Test
    def zip04(): Unit \ Assert =
        let v = Vector.zip(Vector#{1}, Vector#{2});
        assertEq(expected = Vector#{(1, 2)}, v)

    @Test
    def zip05(): Unit \ Assert =
        let v = Vector.zip(Vector#{1, 3}, Vector#{2, 4});
        assertEq(expected = Vector#{(1, 2), (3, 4)}, v)

    @Test
    def zip06(): Unit \ Assert =
        let v = Vector.zip(Vector#{1, 3, 5}, Vector#{2, 4, 6});
        assertEq(expected = Vector#{(1, 2), (3, 4), (5, 6)}, v)

    @Test
    def zip07(): Unit \ Assert =
        let v = Vector.zip(Vector#{1, 3, 5, 7}, Vector#{2, 4, 6, 8});
        assertEq(expected = Vector#{(1, 2), (3, 4), (5, 6), (7, 8)}, v)

    /////////////////////////////////////////////////////////////////////////////
    // zipWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWith01(): Unit \ Assert =
        let v = Vector.zipWith((a, b) -> if (b) a + 1 else a, (Vector#{}: Vector[Int32]), (Vector#{}: Vector[Bool]));
        assertEq(expected = Vector#{}, v)

    @Test
    def zipWith02(): Unit \ Assert =
        let v = Vector.zipWith((a, b) -> if (b) a + 1 else a, Vector#{1}, (Vector#{}: Vector[Bool]));
        assertEq(expected = Vector#{}, v)

    @Test
    def zipWith03(): Unit \ Assert =
        let v = Vector.zipWith((a, b) -> if (b) a + 1 else a, (Vector#{}: Vector[Int32]), Vector#{true});
        assertEq(expected = Vector#{}, v)

    @Test
    def zipWith04(): Unit \ Assert =
        let v = Vector.zipWith((a, b) -> if (b) a + 1 else a, Vector#{1}, Vector#{true});
        assertEq(expected = Vector#{2}, v)

    @Test
    def zipWith05(): Unit \ Assert =
        let v = Vector.zipWith((a, b) -> if (b) a + 1 else a, Vector#{1}, Vector#{false});
        assertEq(expected = Vector#{1}, v)

    @Test
    def zipWith06(): Unit \ Assert =
        let v = Vector.zipWith((a, b) -> if (b) a + 1 else a,
                        Vector#{1, 2, 3, 4, 5, 6, 7, 8},
                        Vector#{false, true, true, false, false, true, true, true});
        assertEq(expected = Vector#{1, 3, 4, 4, 5, 7, 8, 9}, v)

    /////////////////////////////////////////////////////////////////////////////
    // unzip                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unzip01(): Unit \ Assert =
        let (a, b) = Vector.unzip((Vector#{}: Vector[(Unit, Unit)]));
        assertEq(expected = Vector#{}, a);
        assertEq(expected = Vector#{}, b)

    @Test
    def unzip02(): Unit \ Assert =
        let (a, b) = Vector.unzip(Vector#{(1, true)});
        assertEq(expected = Vector#{1}, a);
        assertEq(expected = Vector#{true}, b)

    @Test
    def unzip03(): Unit \ Assert =
        let (a,b) = Vector.unzip(Vector#{(1, true), (2, true)});
        assertEq(expected = Vector#{1, 2}, a);
        assertEq(expected = Vector#{true, true}, b)

    @Test
    def unzip04(): Unit \ Assert =
        let (a,b) = Vector.unzip(Vector#{(1, true), (2, true), (3, false)});
        assertEq(expected = Vector#{1, 2, 3}, a);
        assertEq(expected = Vector#{true, true, false}, b)

    @Test
    def unzip05(): Unit \ Assert =
        assertEq(expected = Vector#{1, 2, 3}, fst(Vector.unzip(Vector#{(1, "1"), (2, "2"), (3, "3")})))


    /////////////////////////////////////////////////////////////////////////////
    // fold2                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fold201(): Unit \ Assert =
        assertEq(expected = 4, Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, (Vector#{}: Vector[Int32]), (Vector#{}: Vector[Bool])))

    @Test
    def fold202(): Unit \ Assert =
        assertEq(expected = 4, Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{1}, Vector#{}))

    @Test
    def fold203(): Unit \ Assert =
        assertEq(expected = 4, Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{}, Vector#{true}))

    @Test
    def fold204(): Unit \ Assert =
        assertEq(expected = 6, Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{2}, Vector#{true}))

    @Test
    def fold205(): Unit \ Assert =
        assertEq(expected = 8, Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{2}, Vector#{false}))

    @Test
    def fold206(): Unit \ Assert =
        assertEq(expected = 9, Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{3, 2}, Vector#{true, true}))

    @Test
    def fold207(): Unit \ Assert =
        assertEq(expected = 14, Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{3, 2}, Vector#{true, false}))

    @Test
    def fold208(): Unit \ Assert =
        assertEq(expected = 14, Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{3, 2}, Vector#{false, true}))

    @Test
    def fold209(): Unit \ Assert =
        assertEq(expected = 24, Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{3, 2}, Vector#{false, false}))

    @Test
    def fold210(): Unit \ Assert =
        assertEq(expected = -118, Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{6, -4, 3, 2},
            Vector#{true, false, false, true}))

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft2                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeft201(): Unit \ Assert =
        assertEq(expected = 4, Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, (Vector#{}: Vector[Int32]), (Vector#{}: Vector[Bool])))

    @Test
    def foldLeft202(): Unit \ Assert =
        assertEq(expected = 4, Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{1}, (Vector#{}: Vector[Bool])))

    @Test
    def foldLeft203(): Unit \ Assert =
        assertEq(expected = 4, Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, (Vector#{}: Vector[Int32]), Vector#{true}))

    @Test
    def foldLeft204(): Unit \ Assert =
        assertEq(expected = 6, Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{2}, Vector#{true}))

    @Test
    def foldLeft205(): Unit \ Assert =
        assertEq(expected = 8, Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{2}, Vector#{false}))

    @Test
    def foldLeft206(): Unit \ Assert =
        assertEq(expected = 9, Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{3, 2}, Vector#{true, true}))

    @Test
    def foldLeft207(): Unit \ Assert =
        assertEq(expected = 14, Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{3, 2}, Vector#{true, false}))

    @Test
    def foldLeft208(): Unit \ Assert =
        assertEq(expected = 14, Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{3, 2}, Vector#{false, true}))

    @Test
    def foldLeft209(): Unit \ Assert =
        assertEq(expected = 24, Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{3, 2}, Vector#{false, false}))

    @Test
    def foldLeft210(): Unit \ Assert =
        assertEq(expected = -118, Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{6, -4, 3, 2},
            Vector#{true, false, false, true}))

    /////////////////////////////////////////////////////////////////////////////
    // foldRight2                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRight201(): Unit \ Assert =
        assertEq(expected = 4, Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, (Vector#{}: Vector[Int32]), (Vector#{}: Vector[Bool])))

    @Test
    def foldRight202(): Unit \ Assert =
        assertEq(expected = 4, Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Vector#{1}, (Vector#{}: Vector[Bool])))

    @Test
    def foldRight203(): Unit \ Assert =
        assertEq(expected = 4, Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, (Vector#{}: Vector[Int32]), Vector#{true}))

    @Test
    def foldRight204(): Unit \ Assert =
        assertEq(expected = 6, Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Vector#{2}, Vector#{true}))

    @Test
    def foldRight205(): Unit \ Assert =
        assertEq(expected = 8, Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Vector#{2}, Vector#{false}))

    @Test
    def foldRight206(): Unit \ Assert =
        assertEq(expected = 9, Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Vector#{3, 2}, Vector#{true, true}))

    @Test
    def foldRight207(): Unit \ Assert =
        assertEq(expected = 11, Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Vector#{3, 2}, Vector#{true, false}))

    @Test
    def foldRight208(): Unit \ Assert =
        assertEq(expected = 18, Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Vector#{3, 2}, Vector#{false, true}))

    @Test
    def foldRight209(): Unit \ Assert =
        assertEq(expected = 24, Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Vector#{3, 2}, Vector#{false, false}))

    @Test
    def foldRight210(): Unit \ Assert =
        assertEq(expected = -66, Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Vector#{6, -4, 3, 2},
            Vector#{true, false, false, true}))

    /////////////////////////////////////////////////////////////////////////////
    // filterMap                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterMap01(): Unit \ Assert =
        let v = Vector.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def filterMap02(): Unit \ Assert =
        let v = Vector.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Vector#{1});
        assertEq(expected = Vector#{}, v)

    @Test
    def filterMap03(): Unit \ Assert =
        let v = Vector.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Vector#{2});
        assertEq(expected = Vector#{1}, v)

    @Test
    def filterMap04(): Unit \ Assert =
        let v = Vector.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Vector#{1, 3});
        assertEq(expected = Vector#{}, v)

    @Test
    def filterMap05(): Unit \ Assert =
        let v = Vector.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Vector#{1, 4});
        assertEq(expected = Vector#{2}, v)

    @Test
    def filterMap06(): Unit \ Assert =
        let v = Vector.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Vector#{6, -1});
        assertEq(expected = Vector#{3}, v)

    @Test
    def filterMap07(): Unit \ Assert =
        let v = Vector.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Vector#{8, 6});
        assertEq(expected = Vector#{4, 3}, v)

    @Test
    def filterMap08(): Unit \ Assert =
        let v = Vector.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Vector#{0, 1, 2, 3, 4, 5, 10});
        assertEq(expected = Vector#{0, 1, 2, 5}, v)

    /////////////////////////////////////////////////////////////////////////////
    // findMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findMap01(): Unit \ Assert =
        assertEq(expected = None, Vector.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, (Vector#{}: Vector[Int32])))

    @Test
    def findMap02(): Unit \ Assert =
        assertEq(expected = None, Vector.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Vector#{1}))

    @Test
    def findMap03(): Unit \ Assert =
        assertEq(expected = Some(1), Vector.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Vector#{2}))

    @Test
    def findMap04(): Unit \ Assert =
        assertEq(expected = None, Vector.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Vector#{1, 3}))

    @Test
    def findMap05(): Unit \ Assert =
        assertEq(expected = Some(2), Vector.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Vector#{1, 4}))

    @Test
    def findMap06(): Unit \ Assert =
        assertEq(expected = Some(3), Vector.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Vector#{6, -1}))

    @Test
    def findMap07(): Unit \ Assert =
        assertEq(expected = Some(4), Vector.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Vector#{8, 6}))

    @Test
    def findMap08(): Unit \ Assert =
        assertEq(expected = Some(0), Vector.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Vector#{0, 1, 2, 3, 4, 5, 10}))

    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toSet01(): Unit \ Assert =
        assertEq(expected = Set#{}, Vector.toSet((Vector#{}: Vector[Int32])))

    @Test
    def toSet02(): Unit \ Assert =
        assertEq(expected = Set#{1}, Vector.toSet(Vector#{1}))

    @Test
    def toSet03(): Unit \ Assert =
        assertEq(expected = Set#{1, 2}, Vector.toSet(Vector#{1, 2}))

    @Test
    def toSet04(): Unit \ Assert =
        assertEq(expected = Set#{1}, Vector.toSet(Vector#{1, 1}))

    @Test
    def toSet05(): Unit \ Assert =
        assertEq(expected = Set#{1, 2, 3}, Vector.toSet(Vector#{1, 2, 3}))

    @Test
    def toSet06(): Unit \ Assert =
        assertEq(expected = Set#{1, 2}, Vector.toSet(Vector#{1, 2, 1}))

    @Test
    def toSet07(): Unit \ Assert =
        assertEq(expected = Set#{1, 2}, Vector.toSet(Vector#{1, 1, 2}))

    @Test
    def toSet08(): Unit \ Assert =
        assertEq(expected = Set#{1, 2, 3, 4, 5, 6}, Vector.toSet(Vector#{2, 1, 2, 3, 4, 5, 6, 3, 3}))

    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMap01(): Unit \ Assert =
        assertEq(expected = Map#{}, Vector.toMap((Vector#{}: Vector[(Int32, Bool)])))

    @Test
    def toMap02(): Unit \ Assert =
        assertEq(expected = Map#{1 => true}, Vector.toMap(Vector#{(1, true)}))

    @Test
    def toMap03(): Unit \ Assert =
        assertEq(expected = Map#{1 => true, 2 => false}, Vector.toMap(Vector#{(1, true), (2, false)}))

    @Test
    def toMap04(): Unit \ Assert =
        assertEq(expected = Map#{1 => true}, Vector.toMap(Vector#{(1, true), (1, false)}))

    /////////////////////////////////////////////////////////////////////////////
    // findIndexOf                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findIndexOf01(): Unit \ Assert =
        assertEq(expected = None, Vector.findIndexOf(i -> i > 2, (Vector#{}: Vector[Int32])))

    @Test
    def findIndexOf02(): Unit \ Assert =
        assertEq(expected = None, Vector.findIndexOf(i -> i > 2, Vector#{1}))

    @Test
    def findIndexOf03(): Unit \ Assert =
        assertEq(expected = Some(0), Vector.findIndexOf(i -> i > 2, Vector#{3}))

    @Test
    def findIndexOf04(): Unit \ Assert =
        assertEq(expected = None, Vector.findIndexOf(i -> i > 2, Vector#{1, 2}))

    @Test
    def findIndexOf05(): Unit \ Assert =
        assertEq(expected = Some(0), Vector.findIndexOf(i -> i > 2, Vector#{6, -6}))

    @Test
    def findIndexOf06(): Unit \ Assert =
        assertEq(expected = Some(1), Vector.findIndexOf(i -> i > 2, Vector#{-6, 6}))

    @Test
    def findIndexOf07(): Unit \ Assert =
        assertEq(expected = Some(0), Vector.findIndexOf(i -> i > 2, Vector#{6, 7}))

    /////////////////////////////////////////////////////////////////////////////
    // findIndexOfLeft                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findIndexOfLeft01(): Unit \ Assert =
        assertEq(expected = None, Vector.findIndexOfLeft(i -> i > 2, (Vector#{}: Vector[Int32])))

    @Test
    def findIndexOfLeft02(): Unit \ Assert =
        assertEq(expected = None, Vector.findIndexOfLeft(i -> i > 2, Vector#{1}))

    @Test
    def findIndexOfLeft03(): Unit \ Assert =
        assertEq(expected = Some(0), Vector.findIndexOfLeft(i -> i > 2, Vector#{3}))

    @Test
    def findIndexOfLeft04(): Unit \ Assert =
        assertEq(expected = None, Vector.findIndexOfLeft(i -> i > 2, Vector#{1, 2}))

    @Test
    def findIndexOfLeft05(): Unit \ Assert =
        assertEq(expected = Some(0), Vector.findIndexOfLeft(i -> i > 2, Vector#{6, -6}))

    @Test
    def findIndexOfLeft06(): Unit \ Assert =
        assertEq(expected = Some(1), Vector.findIndexOfLeft(i -> i > 2, Vector#{-6, 6}))

    @Test
    def findIndexOfLeft07(): Unit \ Assert =
        assertEq(expected = Some(0), Vector.findIndexOfLeft(i -> i > 2, Vector#{6, 7}))

    /////////////////////////////////////////////////////////////////////////////
    // findIndexOfRight                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findIndexOfRight01(): Unit \ Assert =
        assertEq(expected = None, Vector.findIndexOfRight(i -> i > 2, (Vector#{}: Vector[Int32])))

    @Test
    def findIndexOfRight02(): Unit \ Assert =
        assertEq(expected = None, Vector.findIndexOfRight(i -> i > 2, Vector#{1}))

    @Test
    def findIndexOfRight03(): Unit \ Assert =
        assertEq(expected = Some(0), Vector.findIndexOfRight(i -> i > 2, Vector#{3}))

    @Test
    def findIndexOfRight04(): Unit \ Assert =
        assertEq(expected = None, Vector.findIndexOfRight(i -> i > 2, Vector#{1, 2}))

    @Test
    def findIndexOfRight05(): Unit \ Assert =
        assertEq(expected = Some(0), Vector.findIndexOfRight(i -> i > 2, Vector#{6, -6}))

    @Test
    def findIndexOfRight06(): Unit \ Assert =
        assertEq(expected = Some(1), Vector.findIndexOfRight(i -> i > 2, Vector#{-6, 6}))

    @Test
    def findIndexOfRight07(): Unit \ Assert =
        assertEq(expected = Some(1), Vector.findIndexOfRight(i -> i > 2, Vector#{6, 7}))

    /////////////////////////////////////////////////////////////////////////////
    // findIndices                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findIndices01(): Unit \ Assert =
        let v = Vector.findIndices(i -> i > 2, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def findIndices02(): Unit \ Assert =
        let v = Vector.findIndices(i -> i > 2, Vector#{1});
        assertEq(expected = Vector#{}, v)

    @Test
    def findIndices03(): Unit \ Assert =
        let v = Vector.findIndices(i -> i > 2, Vector#{3});
        assertEq(expected = Vector#{0}, v)

    @Test
    def findIndices04(): Unit \ Assert =
        let v = Vector.findIndices(i -> i > 2, Vector#{1, 2});
        assertEq(expected = Vector#{}, v)

    @Test
    def findIndices05(): Unit \ Assert =
        let v = Vector.findIndices(i -> i > 2, Vector#{6, -6});
        assertEq(expected = Vector#{0}, v)

    @Test
    def findIndices06(): Unit \ Assert =
        let v = Vector.findIndices(i -> i > 2, Vector#{-6, 6});
        assertEq(expected = Vector#{1}, v)

    @Test
    def findIndices07(): Unit \ Assert =
        let v = Vector.findIndices(i -> i > 2, Vector#{6, 7});
        assertEq(expected = Vector#{0, 1}, v)

    /////////////////////////////////////////////////////////////////////////////
    // init                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def init01(): Unit \ Assert =
        let v = Vector.init(x -> x, 0);
        assertEq(expected = (Vector#{}: Vector[Int32]), v)

    @Test
    def init02(): Unit \ Assert =
        let v = Vector.init(x -> x, -1);
        assertEq(expected = Vector#{}, v)

    @Test
    def init03(): Unit \ Assert =
        let v = Vector.init(x -> x, 1);
        assertEq(expected = Vector#{0}, v)

    @Test
    def init04(): Unit \ Assert =
        let v = Vector.init(x -> x, 2);
        assertEq(expected = Vector#{0, 1}, v)

    @Test
    def init05(): Unit \ Assert =
        let v = Vector.init(x -> x, 50);
        assertEq(expected = 50, Vector.length(v))

    /////////////////////////////////////////////////////////////////////////////
    // equals                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def equals01(): Unit \ Assert =
        let a = (Vector#{}: Vector[Int32]);
        let b = (Vector#{}: Vector[Int32]);
        assertTrue(Vector.equals(a, b))

    @Test
    def equals02(): Unit \ Assert =
        let a = (Vector#{1});
        let b = (Vector#{}: Vector[Int32]);
        assertFalse(Vector.equals(a, b))

    @Test
    def equals03(): Unit \ Assert =
        let a = (Vector#{}: Vector[Int32]);
        let b = (Vector#{1});
        assertFalse(Vector.equals(a, b))

    @Test
    def equals04(): Unit \ Assert =
        let a = (Vector#{1});
        let b = (Vector#{1});
        assertTrue(Vector.equals(a, b))

    @Test
    def equals05(): Unit \ Assert =
        let a = (Vector#{1});
        let b = (Vector#{}: Vector[Int32]);
        assertFalse(Vector.equals(a, b))

    @Test
    def equals06(): Unit \ Assert =
        let a = (Vector#{1, 2});
        let b = (Vector#{1, 2});
        assertTrue(Vector.equals(a, b))

    @Test
    def equals07(): Unit \ Assert =
        let a = (Vector#{1, 2});
        let b = (Vector#{2, 1});
        assertFalse(Vector.equals(a, b))

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Unit \ Assert = region rc {
        let v = (Vector#{}: Vector[Int32]);
        let sb = StringBuilder.empty(rc);
        let fn = x -> if (x > 0) StringBuilder.append('T', sb) else StringBuilder.append('F', sb);
        Vector.forEach(fn, v);
        assertEq(expected = "", StringBuilder.toString(sb))
    }

    @Test
    def forEach02(): Unit \ Assert = region rc {
        let v = Vector#{0};
        let sb = StringBuilder.empty(rc);
        let fn = x -> if (x > 0) StringBuilder.append('T', sb) else StringBuilder.append('F', sb);
        Vector.forEach(fn, v);
        assertEq(expected = "F", StringBuilder.toString(sb))
    }

    @Test
    def forEach03(): Unit \ Assert = region rc {
        let v = Vector#{1};
        let sb = StringBuilder.empty(rc);
        let fn = x -> if (x > 0) StringBuilder.append('T', sb) else StringBuilder.append('F', sb);
        Vector.forEach(fn, v);
        assertEq(expected = "T", StringBuilder.toString(sb))
    }

    @Test
    def forEach04(): Unit \ Assert = region rc {
        let v = Vector#{0, 1};
        let sb = StringBuilder.empty(rc);
        let fn = x -> if (x > 0) StringBuilder.append('T', sb) else StringBuilder.append('F', sb);
        Vector.forEach(fn, v);
        assertEq(expected = "FT", StringBuilder.toString(sb))
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEachWithIndex01(): Unit \ Assert = region rc {
        let v = (Vector#{}: Vector[Float32]);
        let sb = StringBuilder.empty(rc);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append('T', sb) else StringBuilder.append('F', sb);
        Vector.forEachWithIndex(fn, v);
        assertEq(expected = "", StringBuilder.toString(sb))
    }

    @Test
    def forEachWithIndex02(): Unit \ Assert = region rc {
        let v = Vector#{0.0f32};
        let sb = StringBuilder.empty(rc);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append('T', sb) else StringBuilder.append('F', sb);
        Vector.forEachWithIndex(fn, v);
        assertEq(expected = "F", StringBuilder.toString(sb))
    }

    @Test
    def forEachWithIndex03(): Unit \ Assert = region rc {
        let v = Vector#{0.0f32, 0.1f32};
        let sb = StringBuilder.empty(rc);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append('T', sb) else StringBuilder.append('F', sb);
        Vector.forEachWithIndex(fn, v);
        assertEq(expected = "FT", StringBuilder.toString(sb))
    }

    @Test
    def forEachWithIndex04(): Unit \ Assert = region rc {
        let v = Vector#{0.0f32, 0.1f32, 0.2f32};
        let sb = StringBuilder.empty(rc);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append('T', sb) else StringBuilder.append('F', sb);
        Vector.forEachWithIndex(fn, v);
        assertEq(expected = "FTT", StringBuilder.toString(sb))
    }

    @Test
    def forEachWithIndex05(): Unit \ Assert = region rc {
        let v = Vector#{0.0f32, 0.1f32, 0.2f32, 0.3f32};
        let sb = StringBuilder.empty(rc);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append('T', sb) else StringBuilder.append('F', sb);
        Vector.forEachWithIndex(fn, v);
        assertEq(expected = "FTTT", StringBuilder.toString(sb))
    }

    /////////////////////////////////////////////////////////////////////////////
    // updateSequence                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def updateSequence01(): Unit \ Assert =
         let v = Vector.updateSequence(0, (Vector#{}: Vector[Int32]), (Vector#{}: Vector[Int32]));
         assertEq(expected = Vector#{}, v)

    @Test
    def updateSequence02(): Unit \ Assert =
        let v = Vector.updateSequence(0, Vector#{1, 2}, Vector#{});
        assertEq(expected = Vector#{}, v)

    @Test
    def updateSequence03(): Unit \ Assert =
        let v = Vector.updateSequence(0, Vector#{}, Vector#{1, 2});
        assertEq(expected = Vector#{1, 2}, v)

    @Test
    def updateSequence04(): Unit \ Assert =
        let v = Vector.updateSequence(-3, Vector#{1, 2, 4}, Vector#{1, 2});
        assertEq(expected = Vector#{1, 2}, v)

    @Test
    def updateSequence05(): Unit \ Assert =
        let v = Vector.updateSequence(2, Vector#{1, 2, 4}, Vector#{1, 2});
        assertEq(expected = Vector#{1, 2}, v)

    @Test
    def updateSequence06(): Unit \ Assert =
        let v = Vector.updateSequence(0, Vector#{}, Vector#{1});
        assertEq(expected = Vector#{1}, v)

    @Test
    def updateSequence07(): Unit \ Assert =
        let v = Vector.updateSequence(1, Vector#{2}, Vector#{1});
        assertEq(expected = Vector#{1}, v)

    @Test
    def updateSequence08(): Unit \ Assert =
        let v = Vector.updateSequence(0, Vector#{2}, Vector#{1});
        assertEq(expected = Vector#{2}, v)

    @Test
    def updateSequence09(): Unit \ Assert =
        let v = Vector.updateSequence(0, Vector#{2, 4}, Vector#{1});
        assertEq(expected = Vector#{2}, v)

    @Test
    def updateSequence10(): Unit \ Assert =
        let v = Vector.updateSequence(-1, Vector#{2, 4}, Vector#{1});
        assertEq(expected = Vector#{4}, v)

    @Test
    def updateSequence11(): Unit \ Assert =
        let v = Vector.updateSequence(-1, Vector#{3, 4}, Vector#{1, 2});
        assertEq(expected = Vector#{4, 2}, v)

    @Test
    def updateSequence12(): Unit \ Assert =
        let v = Vector.updateSequence(1, Vector#{3, 4}, Vector#{1, 2});
        assertEq(expected = Vector#{1, 3}, v)

    @Test
    def updateSequence13(): Unit \ Assert =
        let v = Vector.updateSequence(-2, Vector#{3, 4}, Vector#{1, 2});
        assertEq(expected = Vector#{1, 2}, v)

    @Test
    def updateSequence14(): Unit \ Assert =
        let v = Vector.updateSequence(2, Vector#{3, 4}, Vector#{1, 2});
        assertEq(expected = Vector#{1, 2}, v)

    @Test
    def updateSequence15(): Unit \ Assert =
        let v = Vector.updateSequence(1, Vector#{3}, Vector#{1, 2});
        assertEq(expected = Vector#{1, 3}, v)

    @Test
    def updateSequence16(): Unit \ Assert =
        let v = Vector.updateSequence(0, Vector#{3, 4}, Vector#{1, 2});
        assertEq(expected = Vector#{3, 4}, v)

    @Test
    def updateSequence17(): Unit \ Assert =
        let v = Vector.updateSequence(0, Vector#{4}, Vector#{1, 2, 3});
        assertEq(expected = Vector#{4, 2, 3}, v)

    @Test
    def updateSequence18(): Unit \ Assert =
        let v = Vector.updateSequence(1, Vector#{4}, Vector#{1, 2, 3});
        assertEq(expected = Vector#{1, 4, 3}, v)

    @Test
    def updateSequence19(): Unit \ Assert =
        let v = Vector.updateSequence(2, Vector#{4}, Vector#{1, 2, 3});
        assertEq(expected = Vector#{1, 2, 4}, v)

    @Test
    def updateSequence20(): Unit \ Assert =
        let v = Vector.updateSequence(0, Vector#{4, 5}, Vector#{1, 2, 3});
        assertEq(expected = Vector#{4, 5, 3}, v)

    @Test
    def updateSequence21(): Unit \ Assert =
        let v = Vector.updateSequence(1, Vector#{4, 5}, Vector#{1, 2, 3});
        assertEq(expected = Vector#{1, 4, 5}, v)

    @Test
    def updateSequence22(): Unit \ Assert =
        let v = Vector.updateSequence(-1, Vector#{4, 5, 6}, Vector#{1, 2, 3});
        assertEq(expected = Vector#{5, 6, 3}, v)

    @Test
    def updateSequence23(): Unit \ Assert =
        let v = Vector.updateSequence(0, Vector#{4, 5, 6}, Vector#{1, 2, 3});
        assertEq(expected = Vector#{4, 5, 6}, v)

    @Test
    def updateSequence24(): Unit \ Assert =
        let v = Vector.updateSequence(2, Vector#{14, 15, 16, 17}, Vector#{1, 2, 3, 4, 5, 6, 7});
        assertEq(expected = Vector#{1, 2, 14, 15, 16, 17, 7}, v)

    @Test
    def updateSequence25(): Unit \ Assert =
        let v = Vector.updateSequence(-2, Vector#{14, 15, 16, 17}, Vector#{1, 2, 3, 4, 5, 6, 7});
        assertEq(expected = Vector#{16, 17, 3, 4, 5, 6, 7}, v)

    @Test
    def updateSequence26(): Unit \ Assert =
        let v = Vector.updateSequence(4, Vector#{14, 15, 16, 17}, Vector#{1, 2, 3, 4, 5, 6, 7});
        assertEq(expected = Vector#{1, 2, 3, 4, 14, 15, 16}, v)

    @Test
    def updateSequence27(): Unit \ Assert =
        let v = Vector.updateSequence(4, Vector#{14, 15}, Vector#{1, 2, 3, 4, 5, 6, 7});
        assertEq(expected = Vector#{1, 2, 3, 4, 14, 15, 7}, v)

    @Test
    def updateSequence28(): Unit \ Assert =
        let v = Vector.updateSequence(-1, Vector#{-1, -2, -3, -4, -5, -6, -7, -8}, Vector#{1, 2, 3, 4, 5, 6, 7});
        assertEq(expected = Vector#{-2, -3, -4, -5, -6, -7, -8}, v)

    /////////////////////////////////////////////////////////////////////////////
    // sortWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    def cmp(x: Int32, y: Int32): Comparison =
        if (x < y) Comparison.LessThan
        else if (x == y) Comparison.EqualTo
        else Comparison.GreaterThan

    @Test
    def sortWith01(): Unit \ Assert =
        let v = Vector.sortWith(cmp, (Vector#{}: Vector[Int32]));
        assertEq(expected = Vector#{}, v)

    @Test
    def sortWith02(): Unit \ Assert =
        let v = Vector.sortWith(cmp, Vector#{0});
        assertEq(expected = Vector#{0}, v)

    @Test
    def sortWith03(): Unit \ Assert =
        let v = Vector.sortWith(cmp, Vector#{0, 1});
        assertEq(expected = Vector#{0, 1}, v)

    @Test
    def sortWith04(): Unit \ Assert =
        let v = Vector.sortWith(cmp, Vector#{1, 0});
        assertEq(expected = Vector#{0, 1}, v)

    @Test
    def sortWith05(): Unit \ Assert =
        let v = Vector.sortWith(cmp, Vector#{1, 1});
        assertEq(expected = Vector#{1, 1}, v)

    @Test
    def sortWith06(): Unit \ Assert =
        let v = Vector.sortWith(cmp, Vector#{0, 1, 2, 3, 4, 5});
        assertEq(expected = Vector#{0, 1, 2, 3, 4, 5}, v)

    @Test
    def sortWith07(): Unit \ Assert =
        let v = Vector.sortWith(cmp, Vector#{5, 4, 3, 2, 1, 0});
        assertEq(expected = Vector#{0, 1, 2, 3, 4, 5}, v)

    @Test
    def sortWith08(): Unit \ Assert =
        let v = Vector.sortWith(cmp, Vector#{5, 3, 0, 4, 1, 2});
        assertEq(expected = Vector#{0, 1, 2, 3, 4, 5}, v)

    @Test
    def sortWith09(): Unit \ Assert =
        let v = Vector.sortWith(cmp, Vector#{2, 3, 0, 4, 1, 2});
        assertEq(expected = Vector#{0, 1, 2, 2, 3, 4}, v)

    @Test
    def sortWith10(): Unit \ Assert =
        let v = Vector.sortWith(flip(cmp), Vector#{0, 1, 2, 3, 4, 5});
        assertEq(expected = Vector#{5, 4, 3, 2, 1, 0}, v)

    @Test
    def sortWith11(): Unit \ Assert =
        let v = Vector.sortWith(flip(cmp), Vector#{5, 4, 3, 2, 1, 0});
        assertEq(expected = Vector#{5, 4, 3, 2, 1, 0}, v)

    @Test
    def sortWith12(): Unit \ Assert =
        let v = Vector.sortWith(flip(cmp), Vector#{5, 3, 0, 4, 1, 2});
        assertEq(expected = Vector#{5, 4, 3, 2, 1, 0}, v)

    @Test
    def sortWith13(): Unit \ Assert =
        let v = Vector.sortWith(flip(cmp), Vector#{2, 3, 0, 4, 1, 2});
        assertEq(expected = Vector#{4, 3, 2, 2, 1, 0}, v)

    /////////////////////////////////////////////////////////////////////////////
    // sort                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    def testSortVsSortWith(a: Vector[Int32]): Bool =
        Vector.sort(a) == Vector.sortWith(cmp, a)


    @Test
    def sort01(): Unit \ Assert =
        assertTrue(testSortVsSortWith((Vector#{}: Vector[Int32])))

    @Test
    def sort02(): Unit \ Assert =
        assertTrue(testSortVsSortWith(Vector#{0}))

    @Test
    def sort03(): Unit \ Assert =
        assertTrue(testSortVsSortWith(Vector#{0, 1}))

    @Test
    def sort04(): Unit \ Assert =
        assertTrue(testSortVsSortWith(Vector#{1, 0}))

    @Test
    def sort05(): Unit \ Assert =
        assertTrue(testSortVsSortWith(Vector#{1, 1}))

    @Test
    def sort06(): Unit \ Assert =
        assertTrue(testSortVsSortWith(Vector#{0, 1, 2, 3, 4, 5}))

    @Test
    def sort07(): Unit \ Assert =
        assertTrue(testSortVsSortWith(Vector#{5, 4, 3, 2, 1, 0}))

    @Test
    def sort08(): Unit \ Assert =
        assertTrue(testSortVsSortWith(Vector#{5, 3, 0, 4, 1, 2}))

    @Test
    def sort09(): Unit \ Assert =
        assertTrue(testSortVsSortWith(Vector#{2, 3, 0, 4, 1, 2}))

    /////////////////////////////////////////////////////////////////////////////
    // sortBy                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    def testSortByVsSort(a: Vector[Int32]): Bool =
        (Vector.sortBy(identity, a) == Vector.sort(a)) and
        (Vector.sortBy(x -> 4 * x + 7, a) == Vector.sort(a)) and
        (Vector.sortBy(x -> -x, a) == Vector.sortWith(flip(cmp), a))

    @Test
    def sortBy01(): Unit \ Assert =
        assertTrue(testSortByVsSort((Vector#{}: Vector[Int32])))

    @Test
    def sortBy02(): Unit \ Assert =
        assertTrue(testSortByVsSort(Vector#{0}))

    @Test
    def sortBy03(): Unit \ Assert =
        assertTrue(testSortByVsSort(Vector#{0, 1}))

    @Test
    def sortBy04(): Unit \ Assert =
        assertTrue(testSortByVsSort(Vector#{1, 0}))

    @Test
    def sortBy05(): Unit \ Assert =
        assertTrue(testSortByVsSort(Vector#{1, 1}))

    @Test
    def sortBy06(): Unit \ Assert =
        assertTrue(testSortByVsSort(Vector#{0, 1, 2, 3, 4, 5}))

    @Test
    def sortBy07(): Unit \ Assert =
        assertTrue(testSortByVsSort(Vector#{5, 4, 3, 2, 1, 0}))

    @Test
    def sortBy08(): Unit \ Assert =
        assertTrue(testSortByVsSort(Vector#{5, 3, 0, 4, 1, 2}))

    @Test
    def sortBy09(): Unit \ Assert =
        assertTrue(testSortByVsSort(Vector#{2, 3, 0, 4, 1, 2}))

    enum R {
        case R({i = Int32, s = String})
    }

    instance Eq[R] {
        pub def eq(a: R, b: R): Bool =
            let R.R(x) = a;
            let R.R(y) = b;
            x#i == y#i and x#s == y#s
    }

    instance ToString[R] {
        pub def toString(r: R): String =
            let R.R(x) = r;
            "R({i = ${x#i}, s = \"${x#s}\"})"
    }

    @Test
    def sortBy10(): Unit \ Assert =
        let geti = r -> {let R.R(x) = r; x#i};
        assertEq(expected = Vector#{R.R({i = 1, s = "B"}), R.R({i = 2, s = "A"}), R.R({i = 3, s = "C"})}, Vector.sortBy(geti, Vector#{R.R({i = 2, s = "A"}), R.R({i = 1, s = "B"}), R.R({i = 3, s = "C"})}))

    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimumBy01(): Unit \ Assert =
        assertEq(expected = None, Vector.minimumBy((x, y) -> x <=> y, (Vector#{}: Vector[Int32])))

    @Test
    def minimumBy02(): Unit \ Assert =
        assertEq(expected = Some(0), Vector.minimumBy((x, y) -> x <=> y, Vector.range(0, 5)))

    @Test
    def minimumBy03(): Unit \ Assert =
        assertEq(expected = Some(4), Vector.minimumBy((x, y) -> x <=> y, Vector.range(4, 10)))

    @Test
    def minimumBy04(): Unit \ Assert =
        assertEq(expected = Some(10), Vector.minimumBy((x, y) -> x <=> y, Vector.range(10, 20)))

    /////////////////////////////////////////////////////////////////////////////
    // maximumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maximumBy01(): Unit \ Assert =
        assertEq(expected = None, Vector.maximumBy((x, y) -> x <=> y, (Vector#{}: Vector[Int32])))

    @Test
    def maximumBy02(): Unit \ Assert =
        assertEq(expected = Some(4), Vector.maximumBy((x, y) -> x <=> y, Vector.range(0, 5)))

    @Test
    def maximumBy03(): Unit \ Assert =
        assertEq(expected = Some(9), Vector.maximumBy((x, y) -> x <=> y, Vector.range(4, 10)))

    @Test
    def maximumBy04(): Unit \ Assert =
        assertEq(expected = Some(19), Vector.maximumBy((x, y) -> x <=> y, Vector.range(10, 20)))

    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sum01(): Unit \ Assert =
        assertEq(expected = 0, Vector.sum(Vector#{}))

    @Test
    def sum02(): Unit \ Assert =
        assertEq(expected = 1, Vector.sum(Vector#{1}))

    @Test
    def sum03(): Unit \ Assert =
        assertEq(expected = 6, Vector.sum(Vector#{1, 2, 3}))

    @Test
    def sum04(): Unit \ Assert =
        assertEq(expected = 3, Vector.sum(Vector#{1, 2, 3, -3}))

    @Test
    def sum05(): Unit \ Assert =
        assertEq(expected = -10, Vector.sum(Vector#{-1, -2, -3, -4}))

    @Test
    def sum06(): Unit \ Assert =
        assertEq(expected = 0, Vector.sum(Vector#{10, -10}))

    @Test
    def sum07(): Unit \ Assert =
        assertEq(expected = 5050, Vector.range(1, 101) |> Vector.sum)

    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumWith01(): Unit \ Assert =
        assertEq(expected = 0, Vector.sumWith(x -> x + 1, Vector#{}))

    @Test
    def sumWith02(): Unit \ Assert =
        assertEq(expected = 2, Vector.sumWith(x -> x + 1, Vector#{1}))

    @Test
    def sumWith03(): Unit \ Assert =
        assertEq(expected = 9, Vector.sumWith(x -> x + 1, Vector#{1, 2, 3}))

    @Test
    def sumWith04(): Unit \ Assert =
        assertEq(expected = 7, Vector.sumWith(x -> x + 1, Vector#{1, 2, 3, -3}))

    @Test
    def sumWith05(): Unit \ Assert =
        assertEq(expected = -6, Vector.sumWith(x -> x + 1, Vector#{-1, -2, -3, -4}))

    @Test
    def sumWith06(): Unit \ Assert =
        assertEq(expected = 2, Vector.sumWith(x -> x + 1, Vector#{10, -10}))


    /////////////////////////////////////////////////////////////////////////////
    // toChain                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toChain01(): Unit \ Assert =
        assertEq(expected = (Chain.empty(): Chain[Unit]), Vector.toChain((Vector#{}: Vector[Unit])))

    @Test
    def toChain02(): Unit \ Assert =
        assertEq(expected = Chain.singleton(1), Vector.toChain(Vector#{1}))

    @Test
    def toChain03(): Unit \ Assert =
        assertEq(expected = List.toChain(1 :: 2 :: Nil), Vector.toChain(Vector#{1, 2}))

    @Test
    def toChain04(): Unit \ Assert =
        assertEq(expected = List.toChain(1 :: 2 :: 3 :: Nil), Vector.toChain(Vector#{1, 2, 3}))

    /////////////////////////////////////////////////////////////////////////////
    // toNec                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toNec01(): Unit \ Assert =
        assertEq(expected = None, Vector.toNec((Vector#{}: Vector[Unit])))

    @Test
    def toNec02(): Unit \ Assert =
        assertEq(expected = Some(Nec.singleton(1)), Vector.toNec(Vector#{1}))

    @Test
    def toNec03(): Unit \ Assert =
        assertEq(expected = List.toNec(1 :: 2 :: Nil), Vector.toNec(Vector#{1, 2}))

    @Test
    def toNec04(): Unit \ Assert =
        assertEq(expected = List.toNec(1 :: 2 :: 3 :: Nil), Vector.toNec(Vector#{1, 2, 3}))

    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterator01(): Unit \ Assert = region rc {
        assertEq(expected = Nil, (Vector#{}: Vector[Int32]) |> Vector.iterator(rc) |> Iterator.toList)
    }

    @Test
    def iterator02(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: 2 :: 3 :: Nil, (Vector#{1, 2, 3}) |> Vector.iterator(rc) |> Iterator.toList)
    }

    @Test
    def iterator03(): Unit \ Assert = region rc {
        assertEq(expected = Vector#{1, 2, 3}, (Vector#{1, 2, 3}) |> Vector.iterator(rc) |> Iterator.toVector)
    }

    @Test
    def iterator04(): Unit \ Assert = region rc {
        assertEq(expected = Vector.range(-100, 100), Vector.range(-100, 100) |> Vector.iterator(rc) |> Iterator.toVector)
    }

    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def join01(): Unit \ Assert =
        assertEq(expected = "", Vector.join(",", (Vector#{}: Vector[Int32])))

    @Test
    def join02(): Unit \ Assert =
        assertEq(expected = "1", Vector.join(",", Vector#{1}))

    @Test
    def join03(): Unit \ Assert =
        assertEq(expected = "1,2,3", Vector.join(",", Vector#{1, 2, 3}))

    @Test
    def join04(): Unit \ Assert =
        assertEq(expected = "1,2,3", Vector.join(",", Vector#{"1", "2", "3"}))

    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinWith01(): Unit \ Assert =
        assertEq(expected = "", Vector.joinWith(x -> "${x + 1}", ",", Vector#{}))

    @Test
    def joinWith02(): Unit \ Assert =
        assertEq(expected = "2", Vector.joinWith(x -> "${x + 1}", ",", Vector#{1}))

    @Test
    def joinWith03(): Unit \ Assert =
        assertEq(expected = "2,3,4", Vector.joinWith(x -> "${x + 1}", ",", Vector#{1, 2, 3}))

    @Test
    def joinWith04(): Unit \ Assert =
        assertEq(expected = "11,22,33", Vector.joinWith(x -> x + x, ",", Vector#{"1", "2", "3"}))

    @Test
    def joinWith05(): Unit \ Assert =
        let v: Vector[Int32] = Vector#{};
        let s = Vector.joinWith(Int32.toString, ",", v);
        assertEq(expected = "", s)

    @Test
    def joinWith06(): Unit \ Assert =
        let v = Vector#{1};
        let s = Vector.joinWith(Int32.toString, ",", v);
        assertEq(expected = "1", s)

    @Test
    def joinWith07(): Unit \ Assert =
        let v = Vector#{1, 2};
        let s = Vector.joinWith(Int32.toString, ",", v);
        assertEq(expected = "1,2", s)

    @Test
    def joinWith08(): Unit \ Assert =
        let v = Vector#{1, 2, 3};
        let s = Vector.joinWith(Int32.toString, ",", v);
        assertEq(expected = "1,2,3", s)

    @Test
    def joinWith09(): Unit \ Assert =
        let v = Vector#{1, 2, 3};
        let s = Vector.joinWith(Int32.toString, "", v);
        assertEq(expected = "123", s)

    @Test
    def joinWith10(): Unit \ Assert =
        let v = Vector#{1, 2, 3};
        let s = Vector.joinWith(Int32.toString, "..", v);
        assertEq(expected = "1..2..3", s)


    /////////////////////////////////////////////////////////////////////////////
    // shuffle                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def shuffle01(): Unit \ Assert + NonDet =
        run {
            let v = (Vector#{}: Vector[Int32]);
            let v1 = Vector.shuffle(v);
            assertTrue(Vector.length(v1) == 0);
            assertTrue(Vector.toSet(v1) == Set#{})
        } with Shuffle.runWithIO

    @Test
    def shuffle02(): Unit \ Assert + NonDet =
        run {
            let v = Vector#{0};
            let v1 = Vector.shuffle(v);
            assertTrue(Vector.length(v1) == 1);
            assertTrue(Vector.toSet(v1) == Set#{0})
        } with Shuffle.runWithIO

    @Test
    def shuffle03(): Unit \ Assert + NonDet =
        run {
            let v = Vector#{0, 1, 2, 3};
            let v1 = Vector.shuffle(v);
            assertTrue(Vector.length(v1) == 4);
            assertTrue(Vector.toSet(v1) == Set#{0, 1, 2, 3})
        } with Shuffle.runWithIO

    @Test
    def shuffle04(): Unit \ Assert + NonDet =
        run {
            let v = Vector#{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
            let v1 = Vector.shuffle(v);
            assertTrue(Vector.length(v1) == 10);
            assertTrue(Vector.toSet(v1) == Set#{0, 1, 2, 3, 4, 5, 6, 7, 8, 9})
        } with Shuffle.runWithIO

}
