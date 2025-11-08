mod TestList {

    use Assert.{assertEq, assertNeq, assertTrue, assertFalse};
    use ToString.toString;
    use Hash.hash;

    /////////////////////////////////////////////////////////////////////////////
    // Collectable.collect                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def collect01(): Unit \ Assert = region rc {
        assertEq(expected = Nil, Collectable.collect((Iterator.empty(rc): Iterator[Int32, rc, rc])))
    }

    @Test
    def collect02(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: Nil, Collectable.collect(Iterator.singleton(rc, 1)))
    }

    @Test
    def collect03(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: 2 :: Nil, Collectable.collect(Iterator.range(rc, 1, 3)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // empty                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def empty01(): Unit \ Assert = assertEq(expected = (Nil: List[Int32]), List.empty())


    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def isEmpty01(): Unit \ Assert = assertTrue(List.isEmpty(Nil))

    @Test
    def isEmpty02(): Unit \ Assert = assertEq(expected = false, List.isEmpty(1 :: Nil))

    @Test
    def isEmpty03(): Unit \ Assert = assertEq(expected = false, List.isEmpty(1 :: 2 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // nonEmpty                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def nonEmpty01(): Unit \ Assert = assertEq(expected = false, List.nonEmpty(Nil))

    @Test
    def nonEmpty02(): Unit \ Assert = assertEq(expected = true, List.nonEmpty(1 :: Nil))

    @Test
    def nonEmpty03(): Unit \ Assert = assertEq(expected = true, List.nonEmpty(1 :: 2 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def head01(): Unit \ Assert = assertEq(expected = None, List.head((Nil: List[Unit])))

    @Test
    def head02(): Unit \ Assert = assertEq(expected = Some(1), List.head(1 :: Nil))

    @Test
    def head03(): Unit \ Assert = assertEq(expected = Some(2), List.head(2 :: 1 :: Nil))

    @Test
    def head04(): Unit \ Assert = assertEq(expected = Some(3), List.head(3 :: 2 :: 1 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def last01(): Unit \ Assert = assertEq(expected = None, List.last((Nil: List[Unit])))

    @Test
    def last02(): Unit \ Assert = assertEq(expected = Some(1), List.last(1 :: Nil))

    @Test
    def last03(): Unit \ Assert = assertEq(expected = Some(2), List.last(1 :: 2 :: Nil))

    @Test
    def last04(): Unit \ Assert = assertEq(expected = Some(3), List.last(1 :: 2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def length01(): Unit \ Assert = assertEq(expected = 0, List.length(Nil))

    @Test
    def length02(): Unit \ Assert = assertEq(expected = 1, List.length(1 :: Nil))

    @Test
    def length03(): Unit \ Assert = assertEq(expected = 2, List.length(1 :: 2 :: Nil))

    @Test
    def length04(): Unit \ Assert = assertEq(expected = 3, List.length(1 :: 2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def size01(): Unit \ Assert = assertEq(expected = 0, List.size(Nil))

    @Test
    def size02(): Unit \ Assert = assertEq(expected = 1, List.size(1 :: Nil))

    @Test
    def size03(): Unit \ Assert = assertEq(expected = 2, List.size(1 :: 2 :: Nil))

    @Test
    def size04(): Unit \ Assert = assertEq(expected = 3, List.size(1 :: 2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def append01(): Unit \ Assert = assertEq(expected = Nil, List.append((Nil: List[Unit]), (Nil: List[Unit])))

    @Test
    def append02(): Unit \ Assert = assertEq(expected = 1 :: Nil, List.append(Nil, 1 :: Nil))

    @Test
    def append03(): Unit \ Assert = assertEq(expected = 1 :: 2 :: Nil, List.append(Nil, 1 :: 2 :: Nil))

    @Test
    def append04(): Unit \ Assert = assertEq(expected = 1 :: Nil, List.append(1 :: Nil, Nil))

    @Test
    def append05(): Unit \ Assert = assertEq(expected = 1 :: 2 :: Nil, List.append(1 :: 2 :: Nil, Nil))

    @Test
    def append06(): Unit \ Assert = assertEq(expected = 1 :: 2 :: Nil, List.append(1 :: Nil, 2 :: Nil))

    @Test
    def append07(): Unit \ Assert = assertEq(expected = 1 :: 2 :: 3 :: Nil, List.append(1 :: 2 :: Nil, 3 :: Nil))

    @Test
    def append08(): Unit \ Assert = assertEq(expected = 1 :: 2 :: 3 :: Nil, List.append(1 :: Nil, 2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def memberOf01(): Unit \ Assert = assertEq(expected = false, List.memberOf(0, Nil))

    @Test
    def memberOf02(): Unit \ Assert = assertEq(expected = false, List.memberOf(0, 1 :: Nil))

    @Test
    def memberOf03(): Unit \ Assert = assertEq(expected = true, List.memberOf(0, 0 :: Nil))

    @Test
    def memberOf04(): Unit \ Assert = assertEq(expected = false, List.memberOf(0, 1 :: 2 :: Nil))

    @Test
    def memberOf05(): Unit \ Assert = assertEq(expected = true, List.memberOf(1, 1 :: 2 :: Nil))

    @Test
    def memberOf06(): Unit \ Assert = assertEq(expected = true, List.memberOf(2, 1 :: 2 :: Nil))

    @Test
    def memberOf07(): Unit \ Assert = assertEq(expected = false, List.memberOf(3, 1 :: 2 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // indexOf                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def indexOf01(): Unit \ Assert = assertEq(expected = None, List.indexOf(0, Nil))

    @Test
    def indexOf02(): Unit \ Assert = assertEq(expected = None, List.indexOf(0, 1 :: Nil))

    @Test
    def indexOf03(): Unit \ Assert = assertEq(expected = Some(0), List.indexOf(1, 1 :: Nil))

    @Test
    def indexOf04(): Unit \ Assert = assertEq(expected = None, List.indexOf(0, 1 :: 2 :: Nil))

    @Test
    def indexOf05(): Unit \ Assert = assertEq(expected = Some(0), List.indexOf(1, 1 :: 2 :: Nil))

    @Test
    def indexOf06(): Unit \ Assert = assertEq(expected = Some(1), List.indexOf(2, 1 :: 2 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // indicesOf                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indicesOf01(): Unit \ Assert = assertEq(expected = Vector#{}, List.indicesOf(0, Nil))

    @Test
    def indicesOf02(): Unit \ Assert = assertEq(expected = Vector#{}, List.indicesOf(0, 1 :: Nil))

    @Test
    def indicesOf03(): Unit \ Assert = assertEq(expected = Vector#{0}, List.indicesOf(1, 1 :: Nil))

    @Test
    def indicesOf04(): Unit \ Assert = assertEq(expected = Vector#{}, List.indicesOf(0, 1 :: 2 :: Nil))

    @Test
    def indicesOf05(): Unit \ Assert = assertEq(expected = Vector#{0}, List.indicesOf(1, 1 :: 2 :: Nil))

    @Test
    def indicesOf06(): Unit \ Assert = assertEq(expected = Vector#{1}, List.indicesOf(2, 1 :: 2 :: Nil))

    @Test
    def indicesOf07(): Unit \ Assert = assertEq(expected = Vector#{0, 1}, List.indicesOf(1, 1 :: 1 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // indices                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indices01(): Unit \ Assert = assertEq(expected = Range.Range(0, 0), List.indices(Nil))

    @Test
    def indices02(): Unit \ Assert = assertEq(expected = Range.Range(0, 3), List.indices(1 :: 2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // find                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def find01(): Unit \ Assert = assertEq(expected = None, List.find(i -> i > 2, Nil))

    @Test
    def find02(): Unit \ Assert = assertEq(expected = None, List.find(i -> i > 2, 1 :: Nil))

    @Test
    def find03(): Unit \ Assert = assertEq(expected = Some(3), List.find(i -> i > 2, 3 :: Nil))

    @Test
    def find04(): Unit \ Assert = assertEq(expected = None, List.find(i -> i > 2, 1 :: 2 :: Nil))

    @Test
    def find05(): Unit \ Assert = assertEq(expected = Some(6), List.find(i -> i > 2, 6 :: -6 :: Nil))

    @Test
    def find06(): Unit \ Assert = assertEq(expected = Some(6), List.find(i -> i > 2, -6 :: 6 :: Nil))

    @Test
    def find07(): Unit \ Assert = assertEq(expected = Some(6), List.find(i -> i > 2, 6 :: 7 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def findLeft01(): Unit \ Assert = assertEq(expected = None, List.findLeft(i -> i > 2, Nil))

    @Test
    def findLeft02(): Unit \ Assert = assertEq(expected = None, List.findLeft(i -> i > 2, 1 :: Nil))

    @Test
    def findLeft03(): Unit \ Assert = assertEq(expected = Some(3), List.findLeft(i -> i > 2, 3 :: Nil))

    @Test
    def findLeft04(): Unit \ Assert = assertEq(expected = None, List.findLeft(i -> i > 2, 1 :: 2 :: Nil))

    @Test
    def findLeft05(): Unit \ Assert = assertEq(expected = Some(6), List.findLeft(i -> i > 2, 6 :: -6 :: Nil))

    @Test
    def findLeft06(): Unit \ Assert = assertEq(expected = Some(6), List.findLeft(i -> i > 2, -6 :: 6 :: Nil))

    @Test
    def findLeft07(): Unit \ Assert = assertEq(expected = Some(6), List.findLeft(i -> i > 2, 6 :: 7 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def findRight01(): Unit \ Assert = assertEq(expected = None, List.findRight(i -> i > 2, Nil))

    @Test
    def findRight02(): Unit \ Assert = assertEq(expected = None, List.findRight(i -> i > 2, 1 :: Nil))

    @Test
    def findRight03(): Unit \ Assert = assertEq(expected = Some(3), List.findRight(i -> i > 2, 3 :: Nil))

    @Test
    def findRight04(): Unit \ Assert = assertEq(expected = None, List.findRight(i -> i > 2, 1 :: 2 :: Nil))

    @Test
    def findRight05(): Unit \ Assert = assertEq(expected = Some(6), List.findRight(i -> i > 2, 6 :: -6 :: Nil))

    @Test
    def findRight06(): Unit \ Assert = assertEq(expected = Some(6), List.findRight(i -> i > 2, -6 :: 6 :: Nil))

    @Test
    def findRight07(): Unit \ Assert = assertEq(expected = Some(7), List.findRight(i -> i > 2, 6 :: 7 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def range01(): Unit \ Assert = assertEq(expected = Nil, List.range(1, 0))

    @Test
    def range02(): Unit \ Assert = assertEq(expected = Nil, List.range(1, 1))

    @Test
    def range03(): Unit \ Assert = assertEq(expected = 1 :: Nil, List.range(1, 2))

    @Test
    def range04(): Unit \ Assert = assertEq(expected = 1 :: 2 :: Nil, List.range(1, 3))

    @Test
    def range05(): Unit \ Assert = assertEq(expected = 1 :: 2 :: 3 :: Nil, List.range(1, 4))

    /////////////////////////////////////////////////////////////////////////////
    // removeAdjDups                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def removeAdjDups01(): Unit \ Assert = {
        let list: List[Int32] = Nil;
        assertEq(expected = list, List.removeAdjDups(list))
    }

    @Test
    def removeAdjDups02(): Unit \ Assert = {
        let list: List[Int32] = 1 :: Nil;
        assertEq(expected = list, List.removeAdjDups(list))
    }

    @Test
    def removeAdjDups03(): Unit \ Assert = {
        let list: List[Int32] = 1 :: 1 :: Nil;
        assertEq(expected = 1 :: Nil, List.removeAdjDups(list))
    }

    @Test
    def removeAdjDups04(): Unit \ Assert = {
        let list: List[Int32] = 3 :: 3 :: 3 :: 2 :: 2 :: 1 :: 2 :: 1 :: 1 :: Nil;
        assertEq(expected = 3 :: 2 :: 1 :: 2 :: 1 :: Nil, List.removeAdjDups(list))
    }

    @Test
    def removeAdjDups05(): Unit \ Assert = {
        let list: List[(Int32, String)] = (1, "a") :: (2, "b") :: (2, "b") :: (3, "c") :: (3, "a") :: (3, "a") :: (3, "c") :: Nil;
        assertEq(expected = (1, "a") :: (2, "b") :: (3, "c") :: (3, "a") :: (3, "c") :: Nil, List.removeAdjDups(list))
    }

    /////////////////////////////////////////////////////////////////////////////
    // removeAdjDupsWith                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def removeAdjDupsWith01(): Unit \ Assert = {
        let list: List[Int32] = Nil;
        assertEq(expected = list, List.removeAdjDupsWith(x -> y -> x == y, list))
    }

    @Test
    def removeAdjDupsWith02(): Unit \ Assert = {
        let list: List[Int32] = 1 :: Nil;
        assertEq(expected = list, List.removeAdjDupsWith(x -> y -> x == y, list))
    }

    @Test
    def removeAdjDupsWith03(): Unit \ Assert = {
        let list: List[Int32] = 1 :: 1 :: Nil;
        assertEq(expected = 1 :: Nil, List.removeAdjDupsWith(x -> y -> x == y, list))
    }

    @Test
    def removeAdjDupsWith04(): Unit \ Assert = {
        let list: List[Int32] = 3 :: 3 :: 3 :: 2 :: 2 :: 1 :: 2 :: 1 :: 1 :: Nil;
        assertEq(expected = 3 :: 2 :: 1 :: 2 :: 1 :: Nil, List.removeAdjDupsWith(x -> y -> x == y, list))
    }

    @Test
    def removeAdjDupsWith05(): Unit \ Assert = {
        let list: List[(Int32, String)] = (1, "a") :: (2, "b") :: (2, "c") :: (3, "c") :: (3, "a") :: (2, "d") :: Nil;
        assertEq(expected = (1, "a") :: (2, "b") :: (3, "c") :: (2, "d") :: Nil, List.removeAdjDupsWith(x -> y -> fst(x) == fst(y), list))
    }

    /////////////////////////////////////////////////////////////////////////////
    // repeat                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def repeat01(): Unit \ Assert = assertEq(expected = Nil, List.repeat(-1, 1))

    @Test
    def repeat02(): Unit \ Assert = assertEq(expected = Nil, List.repeat(0, 1))

    @Test
    def repeat03(): Unit \ Assert = assertEq(expected = 1 :: Nil, List.repeat(1, 1))

    @Test
    def repeat04(): Unit \ Assert = assertEq(expected = 1 :: 1 :: Nil, List.repeat(2, 1))

    @Test
    def repeat05(): Unit \ Assert = assertEq(expected = 1 :: 1 :: 1 :: Nil, List.repeat(3, 1))

    /////////////////////////////////////////////////////////////////////////////
    // scan                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def scan01(): Unit \ Assert = assertEq(expected = 1 :: Nil, List.scan((i, b) -> if (b) i+1 else i+2, 1, Nil))

    @Test
    def scan02(): Unit \ Assert = assertEq(expected = 1 :: 3 :: Nil, List.scan((i, b) -> if (b) i+1 else i+2, 1, false :: Nil))

    @Test
    def scan03(): Unit \ Assert = assertEq(expected = 1 :: 2 :: Nil, List.scan((i, b) -> if (b) i+1 else i+2, 1, true :: Nil))

    @Test
    def scan04(): Unit \ Assert = assertEq(expected = 1 :: 3 :: 5 :: Nil, List.scan((i, b) -> if (b) i+1 else i+2, 1, false :: false :: Nil))

    @Test
    def scan05(): Unit \ Assert = assertEq(expected = 1 :: 3 :: 4 :: Nil, List.scan((i, b) -> if (b) i+1 else i+2, 1, false :: true :: Nil))

    @Test
    def scan06(): Unit \ Assert = assertEq(expected = 1 :: 2 :: 4 :: Nil, List.scan((i, b) -> if (b) i+1 else i+2, 1, true :: false :: Nil))

    @Test
    def scan07(): Unit \ Assert = assertEq(expected = 1 :: 2 :: 3 :: Nil, List.scan((i, b) -> if (b) i+1 else i+2, 1, true :: true :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // scanLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def scanLeft01(): Unit \ Assert = assertEq(expected = 1 :: Nil, List.scanLeft((i, b) -> if (b) i+1 else i+2, 1, Nil))

    @Test
    def scanLeft02(): Unit \ Assert = assertEq(expected = 1 :: 3 :: Nil, List.scanLeft((i, b) -> if (b) i+1 else i+2, 1, false :: Nil))

    @Test
    def scanLeft03(): Unit \ Assert = assertEq(expected = 1 :: 2 :: Nil, List.scanLeft((i, b) -> if (b) i+1 else i+2, 1, true :: Nil))

    @Test
    def scanLeft04(): Unit \ Assert = assertEq(expected = 1 :: 3 :: 5 :: Nil, List.scanLeft((i, b) -> if (b) i+1 else i+2, 1, false :: false :: Nil))

    @Test
    def scanLeft05(): Unit \ Assert = assertEq(expected = 1 :: 3 :: 4 :: Nil, List.scanLeft((i, b) -> if (b) i+1 else i+2, 1, false :: true :: Nil))

    @Test
    def scanLeft06(): Unit \ Assert = assertEq(expected = 1 :: 2 :: 4 :: Nil, List.scanLeft((i, b) -> if (b) i+1 else i+2, 1, true :: false :: Nil))

    @Test
    def scanLeft07(): Unit \ Assert = assertEq(expected = 1 :: 2 :: 3 :: Nil, List.scanLeft((i, b) -> if (b) i+1 else i+2, 1, true :: true :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // scanRight                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def scanRight01(): Unit \ Assert = assertEq(expected = 1 :: Nil, List.scanRight((b, i) -> if (b) i+1 else i+2, 1, Nil))

    @Test
    def scanRight02(): Unit \ Assert = assertEq(expected = 3 :: 1 :: Nil, List.scanRight((b, i) -> if (b) i+1 else i+2, 1, false :: Nil))

    @Test
    def scanRight03(): Unit \ Assert = assertEq(expected = 2 :: 1 :: Nil, List.scanRight((b, i) -> if (b) i+1 else i+2, 1, true :: Nil))

    @Test
    def scanRight04(): Unit \ Assert = assertEq(expected = 5 :: 3 :: 1 :: Nil, List.scanRight((b, i) -> if (b) i+1 else i+2, 1, false :: false :: Nil))

    @Test
    def scanRight05(): Unit \ Assert = assertEq(expected = 4 :: 2 :: 1 :: Nil, List.scanRight((b, i) -> if (b) i+1 else i+2, 1, false :: true :: Nil))

    @Test
    def scanRight06(): Unit \ Assert = assertEq(expected = 4 :: 3 :: 1 :: Nil, List.scanRight((b, i) -> if (b) i+1 else i+2, 1, true :: false :: Nil))

    @Test
    def scanRight07(): Unit \ Assert = assertEq(expected = 3 :: 2 :: 1 :: Nil, List.scanRight((b, i) -> if (b) i+1 else i+2, 1, true :: true :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def map01(): Unit \ Assert = assertEq(expected = Nil, List.map(i -> i > 2, Nil))

    @Test
    def map02(): Unit \ Assert = assertEq(expected = false :: Nil, List.map(i -> i > 2, 1 :: Nil))

    @Test
    def map03(): Unit \ Assert = assertEq(expected = true :: Nil, List.map(i -> i > 2, 3 :: Nil))

    @Test
    def map04(): Unit \ Assert = assertEq(expected = false :: false :: Nil, List.map(i -> i > 2, 1 :: 2 :: Nil))

    @Test
    def map05(): Unit \ Assert = assertEq(expected = false :: true :: Nil, List.map(i -> i > 2, 1 :: 8 :: Nil))

    @Test
    def map06(): Unit \ Assert = assertEq(expected = true :: false :: Nil, List.map(i -> i > 2, 8 :: 1 :: Nil))

    @Test
    def map07(): Unit \ Assert = assertEq(expected = true :: true :: Nil, List.map(i -> i > 2, 7 :: 8 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // point                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def point01(): Unit \ Assert = assertEq(expected = 0 :: Nil, List.point(0))

    @Test
    def point02(): Unit \ Assert = assertEq(expected = 123 :: Nil, List.point(123))

    @Test
    def point03(): Unit \ Assert = assertEq(expected = true :: Nil, List.point(true))

    @Test
    def point04(): Unit \ Assert = assertEq(expected = Some(0) :: Nil, List.point(Some(0)))

    @Test
    def point05(): Unit \ Assert = assertEq(expected = Nil :: Nil, List.point((Nil : List[Bool])))

    @Test
    def point06(): Unit \ Assert = assertEq(expected = (42 :: Nil) :: Nil, List.point(42::Nil))

    @Test
    def point07(): Unit \ Assert = assertEq(expected = (1:: Nil) :: Nil, List.point(List.point(1)))

    /////////////////////////////////////////////////////////////////////////////
    // ap                                                                      //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def ap01(): Unit \ Assert = assertEq(expected = (Nil: List[Int32]), List.ap(Nil, Nil))

    @Test
    def ap02(): Unit \ Assert = assertEq(expected = Nil, List.ap((x -> x+1) :: Nil, Nil))

    @Test
    def ap03(): Unit \ Assert = assertEq(expected = (Nil: List[Int32]), List.ap(Nil, 5 :: Nil))

    @Test
    def ap04(): Unit \ Assert = assertEq(expected = 6 :: Nil, List.ap((x -> x+1) :: Nil, 5 :: Nil))

    @Test
    def ap05(): Unit \ Assert = assertEq(expected = 1 :: 6 :: Nil, List.ap((x -> x+1) :: Nil, 0 :: 5 :: Nil))

    @Test
    def ap06(): Unit \ Assert = assertEq(expected = 1 :: 5 :: 0 :: 8 :: Nil, List.ap((x -> x+1) :: (x -> x*2) :: Nil, 0 :: 4 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // map2                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map201(): Unit \ Assert = assertEq(expected = (Nil: List[Int32]), List.map2((x,y) -> x+y, Nil, Nil))

    @Test
    def map202(): Unit \ Assert = assertEq(expected = Nil, List.map2((x,y) -> x+y, 10 :: Nil, Nil))

    @Test
    def map203(): Unit \ Assert = assertEq(expected = Nil, List.map2((x,y) -> x+y, Nil, 1 :: Nil))

    @Test
    def map204(): Unit \ Assert = assertEq(expected = 11 :: Nil, List.map2((x,y) -> x+y, 10 :: Nil, 1 :: Nil))

    @Test
    def map205(): Unit \ Assert = assertEq(expected = 11 :: 21 :: Nil, List.map2((x,y) -> x+y, 10 :: 20 :: Nil, 1 :: Nil))

    @Test
    def map206(): Unit \ Assert = assertEq(expected = 11 :: 12 :: 21 :: 22 :: Nil, List.map2((x,y) -> x+y, 10 :: 20 :: Nil, 1 :: 2 :: Nil))

    @Test
    def map207(): Unit \ Assert = assertEq(expected = (11 :: Nil) :: (12 :: Nil) :: Nil, List.map2((x,y) -> (x+y) :: Nil, 10 :: Nil, 1 :: 2 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // map3                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map301(): Unit \ Assert = assertEq(expected = (Nil : List[(Int32, String, String)]), List.map3((x,y,z) -> (x,y,z), Nil, Nil, Nil))

    @Test
    def map302(): Unit \ Assert = assertEq(expected = (1, "a", "A") :: (1, "a", "B") :: (1, "a", "C") :: (1, "b", "A") :: (1, "b", "B") :: (1, "b", "C") :: (1, "c", "A") :: (1, "c", "B") :: (1, "c", "C") :: (2, "a", "A") :: (2, "a", "B") :: (2, "a", "C") :: (2, "b", "A") :: (2, "b", "B") :: (2, "b", "C") :: (2, "c", "A") :: (2, "c", "B") :: (2, "c", "C") :: (3, "a", "A") :: (3, "a", "B") :: (3, "a", "C") :: (3, "b", "A") :: (3, "b", "B") :: (3, "b", "C") :: (3, "c", "A") :: (3, "c", "B") :: (3, "c", "C") :: Nil, List.map3((x,y,z) -> (x,y,z), 1 :: 2 :: 3 :: Nil, "a" :: "b" :: "c" :: Nil, "A" :: "B" :: "C" :: Nil))


    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def mapWithIndex01(): Unit \ Assert = assertEq(expected = Nil, List.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, Nil))

    @Test
    def mapWithIndex02(): Unit \ Assert = assertEq(expected = false :: Nil, List.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, 1 :: Nil))

    @Test
    def mapWithIndex03(): Unit \ Assert = assertEq(expected = true :: Nil, List.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, 3 :: Nil))

    @Test
    def mapWithIndex04(): Unit \ Assert = assertEq(expected = false :: true :: Nil, List.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, 1 :: 2 :: Nil))

    @Test
    def mapWithIndex05(): Unit \ Assert = assertEq(expected = false :: false :: Nil, List.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, 1 :: 8 :: Nil))

    @Test
    def mapWithIndex06(): Unit \ Assert = assertEq(expected = true :: true :: Nil, List.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, 8 :: 1 :: Nil))

    @Test
    def mapWithIndex07(): Unit \ Assert = assertEq(expected = true :: false :: Nil, List.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, 7 :: 8 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // flatMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def flatMap01(): Unit \ Assert = assertEq(expected = Nil, List.flatMap(i -> List.repeat(i, i), Nil))

    @Test
    def flatMap02(): Unit \ Assert = assertEq(expected = Nil, List.flatMap(i -> List.repeat(i, i), 0 :: Nil))

    @Test
    def flatMap03(): Unit \ Assert = assertEq(expected = 1 :: Nil, List.flatMap(i -> List.repeat(i, i), 1 :: Nil))

    @Test
    def flatMap04(): Unit \ Assert = assertEq(expected = 2 :: 2 :: Nil, List.flatMap(i -> List.repeat(i, i), 2 :: Nil))

    @Test
    def flatMap05(): Unit \ Assert = assertEq(expected = 1 :: 2 :: 2 :: Nil, List.flatMap(i -> List.repeat(i, i), 1 :: 2 :: Nil))

    @Test
    def flatMap06(): Unit \ Assert = assertEq(expected = 2 :: 2 :: 3 :: 3 :: 3 :: Nil, List.flatMap(i -> List.repeat(i, i), 2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def reverse01(): Unit \ Assert = assertEq(expected = Nil, List.reverse((Nil: List[Unit])))

    @Test
    def reverse02(): Unit \ Assert = assertEq(expected = 1 :: Nil, List.reverse(1 :: Nil))

    @Test
    def reverse03(): Unit \ Assert = assertEq(expected = 2 :: 1 :: Nil, List.reverse(1 :: 2 :: Nil))

    @Test
    def reverse04(): Unit \ Assert = assertEq(expected = 1 :: 1 :: Nil, List.reverse(1 :: 1 :: Nil))

    @Test
    def reverse05(): Unit \ Assert = assertEq(expected = 3 :: 2 :: 1 :: Nil, List.reverse(1 :: 2 :: 3 :: Nil))

    @Test
    def reverse06(): Unit \ Assert = assertEq(expected = 4 :: 3 :: 2 :: 1 :: Nil, List.reverse(1 :: 2 :: 3 :: 4 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // rotateLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def rotateLeft01(): Unit \ Assert = assertEq(expected = Nil, List.rotateLeft(0, (Nil: List[Unit])))

    @Test
    def rotateLeft02(): Unit \ Assert = assertEq(expected = Nil, List.rotateLeft(1, (Nil: List[Unit])))

    @Test
    def rotateLeft03(): Unit \ Assert = assertEq(expected = 1 :: Nil, List.rotateLeft(0, 1 :: Nil))

    @Test
    def rotateLeft04(): Unit \ Assert = assertEq(expected = 1 :: 2 :: Nil, List.rotateLeft(0, 1 :: 2 :: Nil))

    @Test
    def rotateLeft05(): Unit \ Assert = assertEq(expected = 2 :: 1 :: Nil, List.rotateLeft(1, 1 :: 2 :: Nil))

    @Test
    def rotateLeft06(): Unit \ Assert = assertEq(expected = 1 :: 2 :: Nil, List.rotateLeft(2, 1 :: 2 :: Nil))

    @Test
    def rotateLeft07(): Unit \ Assert = assertEq(expected = 2 :: 1 :: Nil, List.rotateLeft(3, 1 :: 2 :: Nil))

    @Test
    def rotateLeft08(): Unit \ Assert = assertEq(expected = 2 :: 1 :: Nil, List.rotateLeft(-1, 1 :: 2 :: Nil))

    @Test
    def rotateLeft09(): Unit \ Assert = assertEq(expected = 1 :: 2 :: 3 :: Nil, List.rotateLeft(0, 1 :: 2 :: 3 :: Nil))

    @Test
    def rotateLeft10(): Unit \ Assert = assertEq(expected = 2 :: 3 :: 1 :: Nil, List.rotateLeft(1, 1 :: 2 :: 3 :: Nil))

    @Test
    def rotateLeft11(): Unit \ Assert = assertEq(expected = 3 :: 1 :: 2 :: Nil, List.rotateLeft(2, 1 :: 2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // rotateRight                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def rotateRight01(): Unit \ Assert = assertEq(expected = Nil, List.rotateRight(0, (Nil: List[Unit])))

    @Test
    def rotateRight02(): Unit \ Assert = assertEq(expected = Nil, List.rotateRight(1, (Nil: List[Unit])))

    @Test
    def rotateRight03(): Unit \ Assert = assertEq(expected = 1 :: Nil, List.rotateRight(0, 1 :: Nil))

    @Test
    def rotateRight04(): Unit \ Assert = assertEq(expected = 1 :: 2 :: Nil, List.rotateRight(0, 1 :: 2 :: Nil))

    @Test
    def rotateRight05(): Unit \ Assert = assertEq(expected = 2 :: 1 :: Nil, List.rotateRight(1, 1 :: 2 :: Nil))

    @Test
    def rotateRight06(): Unit \ Assert = assertEq(expected = 1 :: 2 :: Nil, List.rotateRight(2, 1 :: 2 :: Nil))

    @Test
    def rotateRight07(): Unit \ Assert = assertEq(expected = 2 :: 1 :: Nil, List.rotateRight(3, 1 :: 2 :: Nil))

    @Test
    def rotateRight08(): Unit \ Assert = assertEq(expected = 2 :: 1 :: Nil, List.rotateRight(-1, 1 :: 2 :: Nil))

    @Test
    def rotateRight09(): Unit \ Assert = assertEq(expected = 1 :: 2 :: 3 :: Nil, List.rotateRight(0, 1 :: 2 :: 3 :: Nil))

    @Test
    def rotateRight10(): Unit \ Assert = assertEq(expected = 3 :: 1 :: 2 :: Nil, List.rotateRight(1, 1 :: 2 :: 3 :: Nil))

    @Test
    def rotateRight11(): Unit \ Assert = assertEq(expected = 2 :: 3 :: 1 :: Nil, List.rotateRight(2, 1 :: 2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // update                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def update01(): Unit \ Assert = assertEq(expected = Nil, List.update(0, 2, Nil))

    @Test
    def update02(): Unit \ Assert = assertEq(expected = 1 :: Nil, List.update(-1, 2, 1 :: Nil))

    @Test
    def update03(): Unit \ Assert = assertEq(expected = 2 :: Nil, List.update(0, 2, 1 :: Nil))

    @Test
    def update04(): Unit \ Assert = assertEq(expected = 1 :: Nil, List.update(1, 2, 1 :: Nil))

    @Test
    def update05(): Unit \ Assert = assertEq(expected = 5 :: 2 :: Nil, List.update(0, 5, 1 :: 2 :: Nil))

    @Test
    def update06(): Unit \ Assert = assertEq(expected = 1 :: 5 :: Nil, List.update(1, 5, 1 :: 2 :: Nil))

    @Test
    def update07(): Unit \ Assert = assertEq(expected = 1 :: 2 :: Nil, List.update(2, 5, 1 :: 2 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def replace01(): Unit \ Assert = assertEq(expected = Nil, List.replace(src = 3, dst = 4, Nil))

    @Test
    def replace02(): Unit \ Assert = assertEq(expected = 1 :: Nil, List.replace(src = 3, dst = 4, 1 :: Nil))

    @Test
    def replace03(): Unit \ Assert = assertEq(expected = 4 :: Nil, List.replace(src = 3, dst = 4, 3 :: Nil))

    @Test
    def replace04(): Unit \ Assert = assertEq(expected = 4 :: Nil, List.replace(src = 3, dst = 4, 4 :: Nil))

    @Test
    def replace05(): Unit \ Assert = assertEq(expected = 1 :: 2 :: Nil, List.replace(src = 3, dst = 4, 1 :: 2 :: Nil))

    @Test
    def replace06(): Unit \ Assert = assertEq(expected = 1 :: 4 :: Nil, List.replace(src = 3, dst = 4, 1 :: 3 :: Nil))

    @Test
    def replace07(): Unit \ Assert = assertEq(expected = 4 :: 4 :: Nil, List.replace(src = 3, dst = 4, 3 :: 4 :: Nil))

    @Test
    def replace08(): Unit \ Assert = assertEq(expected = 4 :: 4 :: Nil, List.replace(src = 3, dst = 4, 3 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // patch                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def patch01(): Unit \ Assert = assertEq(expected = Nil, List.patch(0, 0, (Nil: List[Int32]), Nil))

    @Test
    def patch02(): Unit \ Assert = assertEq(expected = Nil, List.patch(0, 2, 1 :: 2 :: Nil, Nil))

    @Test
    def patch03(): Unit \ Assert = assertEq(expected = 1 :: 2 :: Nil, List.patch(0, 2, Nil, 1 :: 2 :: Nil))

    @Test
    def patch04(): Unit \ Assert = assertEq(expected = 1 :: 2 :: Nil, List.patch(-3, 3, 1 :: 2 :: 4 :: Nil, 1 :: 2 :: Nil))

    @Test
    def patch05(): Unit \ Assert = assertEq(expected = 1 :: 2 :: Nil, List.patch(2, 3, 1 :: 2 :: 4 :: Nil, 1 :: 2 :: Nil))

    @Test
    def patch06(): Unit \ Assert = assertEq(expected = 1 :: Nil, List.patch(0, 0, Nil, 1 :: Nil))

    @Test
    def patch07(): Unit \ Assert = assertEq(expected = 1 :: Nil, List.patch(1, 0, 2 :: Nil, 1 :: Nil))

    @Test
    def patch08(): Unit \ Assert = assertEq(expected = 2 :: Nil, List.patch(0, 1, 2 :: Nil, 1 :: Nil))

    @Test
    def patch09(): Unit \ Assert = assertEq(expected = 2 :: Nil, List.patch(0, 2, 2 :: 4 :: Nil, 1 :: Nil))

    @Test
    def patch10(): Unit \ Assert = assertEq(expected = 4 :: Nil, List.patch(-1, 2, 2 :: 4 :: Nil, 1 :: Nil))

    @Test
    def patch11(): Unit \ Assert = assertEq(expected = 4 :: 2 :: Nil, List.patch(-1, 2, 3 :: 4 :: Nil, 1 :: 2 :: Nil))

    @Test
    def patch12(): Unit \ Assert = assertEq(expected = 1 :: 3 :: Nil, List.patch(1, 2, 3 :: 4 :: Nil, 1 :: 2 :: Nil))

    @Test
    def patch13(): Unit \ Assert = assertEq(expected = 1 :: 2 :: Nil, List.patch(-2, 2, 3 :: 4 :: Nil, 1 :: 2 :: Nil))

    @Test
    def patch14(): Unit \ Assert = assertEq(expected = 1 :: 2 :: Nil, List.patch(2, 2, 3 :: 4 :: Nil, 1 :: 2 :: Nil))

    @Test
    def patch15(): Unit \ Assert = assertEq(expected = 1 :: 3 :: Nil, List.patch(1, 1, 3 :: Nil, 1 :: 2 :: Nil))

    @Test
    def patch16(): Unit \ Assert = assertEq(expected = 3 :: 4 :: Nil, List.patch(0, 2, 3 :: 4 :: Nil, 1 :: 2 :: Nil))

    @Test
    def patch17(): Unit \ Assert = assertEq(expected = 4 :: 2 :: 3 :: Nil, List.patch(0, 1, 4 :: Nil, 1 :: 2 :: 3 :: Nil))

    @Test
    def patch18(): Unit \ Assert = assertEq(expected = 1 :: 4 :: 3 :: Nil, List.patch(1, 1, 4 :: Nil, 1 :: 2 :: 3 :: Nil))

    @Test
    def patch19(): Unit \ Assert = assertEq(expected = 1 :: 2 :: 4 :: Nil, List.patch(2, 1, 4 :: Nil, 1 :: 2 :: 3 :: Nil))

    @Test
    def patch20(): Unit \ Assert = assertEq(expected = 4 :: 5 :: 3 :: Nil, List.patch(0, 2, 4 :: 5 :: Nil, 1 :: 2 :: 3 :: Nil))

    @Test
    def patch21(): Unit \ Assert = assertEq(expected = 1 :: 4 :: 5 :: Nil, List.patch(1, 2, 4 :: 5 :: Nil, 1 :: 2 :: 3 :: Nil))

    @Test
    def patch22(): Unit \ Assert = assertEq(expected = 4 :: 5 :: 3 :: Nil, List.patch(0, 2, 4 :: 5 :: 6 :: Nil, 1 :: 2 :: 3 :: Nil))

    @Test
    def patch23(): Unit \ Assert = assertEq(expected = 4 :: 5 :: 6 :: Nil, List.patch(0, 3, 4 :: 5 :: 6 :: Nil, 1 :: 2 :: 3 :: Nil))

    @Test
    def patch24(): Unit \ Assert = assertEq(expected = 1 :: 2 :: 14 :: 15 :: 16 :: 17 :: 7 :: Nil, List.patch(2, 4, 14 :: 15 :: 16 :: 17 :: Nil, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil))

    @Test
    def patch25(): Unit \ Assert = assertEq(expected = 16 :: 17 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil, List.patch(-2, 4, 14 :: 15 :: 16 :: 17 :: Nil, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil))

    @Test
    def patch26(): Unit \ Assert = assertEq(expected = 1 :: 2 :: 3 :: 4 :: 14 :: 15 :: 16 :: Nil, List.patch(4, 5, 14 :: 15 :: 16 :: 17 :: Nil, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil))

    @Test
    def patch27(): Unit \ Assert = assertEq(expected = 1 :: 2 :: 3 :: 4 :: 14 :: 15 :: 7 :: Nil, List.patch(4, 2, 14 :: 15 :: 16 :: 17 :: Nil, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil))

    @Test
    def patch28(): Unit \ Assert = assertEq(expected = -2 :: -3 :: -4 :: -5 :: -6 :: -7 :: -8 :: Nil, List.patch(-1, 10, -1 :: -2 :: -3 :: -4 :: -5 :: -6 :: -7 :: -8 :: Nil, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // permutations                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def permutations01(): Unit \ Assert = assertEq(expected = Nil :: Nil, List.permutations((Nil: List[Unit])))

    @Test
    def permutations02(): Unit \ Assert = assertEq(expected = (1 :: Nil) :: Nil, List.permutations(1 :: Nil))

    @Test
    def permutations03(): Unit \ Assert = assertEq(expected = (1 :: 2 :: Nil) :: (2 :: 1 :: Nil) :: Nil, List.permutations(1 :: 2 :: Nil))

    @Test
    def permutations04(): Unit \ Assert = assertEq(expected = (1 :: 2 :: 3 :: Nil) :: (1 :: 3 :: 2 :: Nil) ::
                                                                                 (2 :: 1 :: 3 :: Nil) :: (2 :: 3 :: 1 :: Nil) ::
                                                                                 (3 :: 1 :: 2 :: Nil) :: (3 :: 2 :: 1 :: Nil) :: Nil, List.permutations(1 :: 2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // subsequences                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def subsequences01(): Unit \ Assert = assertEq(expected = Nil :: Nil, List.subsequences((Nil: List[Unit])))

    @Test
    def subsequences02(): Unit \ Assert = assertEq(expected = (1 :: Nil) :: Nil :: Nil, List.subsequences(1 :: Nil))

    @Test
    def subsequences03(): Unit \ Assert = assertEq(expected = (1 :: 2 :: Nil) :: (1 :: Nil) ::
                                                                            (2 :: Nil) :: Nil :: Nil, List.subsequences(1 :: 2 :: Nil))

    @Test
    def subsequences04(): Unit \ Assert = assertEq(expected = (1 :: 2 :: 3 :: Nil) :: (1 :: 2 :: Nil) ::
                                                                                 (1 :: 3 :: Nil) :: (1 :: Nil) ::
                                                                                 (2 :: 3 :: Nil) :: (2 :: Nil) ::
                                                                                 (3 :: Nil) :: Nil :: Nil, List.subsequences(1 :: 2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // intersperse                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def intersperse01(): Unit \ Assert = assertEq(expected = Nil, List.intersperse(11, Nil))

    @Test
    def intersperse02(): Unit \ Assert = assertEq(expected = 1 :: Nil, List.intersperse(11, 1 :: Nil))

    @Test
    def intersperse03(): Unit \ Assert = assertEq(expected = 1 :: 11 :: 2 :: Nil, List.intersperse(11, 1 :: 2 :: Nil))

    @Test
    def intersperse04(): Unit \ Assert = assertEq(expected = 1 :: 11 :: 2 :: 11 :: 3 :: Nil, List.intersperse(11, 1 :: 2 :: 3 :: Nil))

    @Test
    def intersperse05(): Unit \ Assert = assertEq(expected = 1 :: 11 :: 2 :: 11 :: 3 :: 11 :: 4 :: Nil, List.intersperse(11, 1 :: 2 :: 3 :: 4 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // transpose                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def transpose01(): Unit \ Assert = assertEq(expected = Nil, List.transpose((Nil: List[List[Unit]])))

    @Test
    def transpose02(): Unit \ Assert = assertEq(expected = Nil :: Nil, (List.transpose(Nil :: Nil): List[List[Unit]]))

    @Test
    def transpose03(): Unit \ Assert = assertEq(expected = Nil :: Nil :: Nil, (List.transpose(Nil :: Nil :: Nil): List[List[Unit]]))

    @Test
    def transpose04(): Unit \ Assert = assertEq(expected = Nil :: Nil :: Nil :: Nil, (List.transpose(Nil :: Nil :: Nil :: Nil): List[List[Unit]]))

    @Test
    def transpose05(): Unit \ Assert = assertEq(expected = (1 :: Nil) :: Nil, List.transpose((1 :: Nil) :: Nil))

    @Test
    def transpose06(): Unit \ Assert = assertEq(expected = (1 :: Nil) :: (2 :: Nil) :: Nil, List.transpose((1 :: 2 :: Nil) :: Nil))

    @Test
    def transpose07(): Unit \ Assert = assertEq(expected = (1 :: Nil) :: (2 :: Nil) :: (3 :: Nil) :: Nil, List.transpose((1 :: 2 :: 3 :: Nil) :: Nil))

    @Test
    def transpose08(): Unit \ Assert = assertEq(expected = (1 :: Nil) :: (2 :: Nil) :: (3 :: Nil) :: (4 :: Nil) :: Nil, List.transpose((1 :: 2 :: 3 :: 4 :: Nil) :: Nil))

    @Test
    def transpose09(): Unit \ Assert = assertEq(expected = (1 :: 2 :: Nil) :: Nil, List.transpose((1 :: Nil) :: (2 :: Nil) :: Nil))

    @Test
    def transpose10(): Unit \ Assert = assertEq(expected = (1 :: 2 :: 3 :: Nil) :: Nil, List.transpose((1 :: Nil) :: (2 :: Nil) :: (3 :: Nil) :: Nil))

    @Test
    def transpose11(): Unit \ Assert = assertEq(expected = (1 :: 2 :: 3 :: 4 :: Nil) :: Nil, List.transpose((1 :: Nil) :: (2 :: Nil) :: (3 :: Nil) :: (4 :: Nil) :: Nil))

    @Test
    def transpose12(): Unit \ Assert = assertEq(expected = (1 :: 3 :: Nil) :: (2 :: 4 :: Nil) :: Nil, List.transpose((1 :: 2 :: Nil) :: (3 :: 4 :: Nil) :: Nil))

    @Test
    def transpose13(): Unit \ Assert = assertEq(expected = (1 :: 4 :: Nil) :: (2 :: 5 :: Nil) :: (3 :: 6 :: Nil) :: Nil, List.transpose((1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: 6 :: Nil) :: Nil))

    @Test
    def transpose14(): Unit \ Assert = assertEq(expected = (1 :: 5 :: Nil) :: (2 :: 6 :: Nil) :: (3 :: 7 :: Nil) :: (4 :: 8 :: Nil) :: Nil, List.transpose((1 :: 2 :: 3 :: 4 :: Nil) :: (5 :: 6 :: 7 :: 8 :: Nil) :: Nil))

    @Test
    def transpose15(): Unit \ Assert = assertEq(expected = (1 :: 6 :: Nil) :: (2 :: 7 :: Nil) :: (3 :: 8 :: Nil) :: (4 :: 9 :: Nil) :: (5 :: 10 :: Nil) :: Nil, List.transpose((1 :: 2 :: 3 :: 4 :: 5 :: Nil) :: (6 :: 7 :: 8 :: 9 :: 10 :: Nil) :: Nil))

    @Test
    def transpose16(): Unit \ Assert = assertEq(expected = (1 :: 3 :: 5 :: Nil) :: (2 :: 4 :: 6 :: Nil) :: Nil, List.transpose((1 :: 2 :: Nil) :: (3 :: 4 :: Nil) :: (5 :: 6 :: Nil) :: Nil))

    @Test
    def transpose17(): Unit \ Assert = assertEq(expected = (1 :: 3 :: 5 :: 7 :: Nil) :: (2 :: 4 :: 6 :: 8 :: Nil) :: Nil, List.transpose((1 :: 2 :: Nil) :: (3 :: 4 :: Nil) :: (5 :: 6 :: Nil) :: (7 :: 8 :: Nil) :: Nil))

    @Test
    def transpose18(): Unit \ Assert = assertEq(expected = (1 :: 3 :: 5 :: 7 :: 9 :: Nil) :: (2 :: 4 :: 6 :: 8 :: 10 :: Nil) :: Nil, List.transpose((1 :: 2 :: Nil) :: (3 :: 4 :: Nil) :: (5 :: 6 :: Nil) :: (7 :: 8 :: Nil) :: (9 :: 10 :: Nil) :: Nil))

    @Test
    def transpose19(): Unit \ Assert = assertEq(expected = (1 :: 4 :: 7 :: Nil) :: (2 :: 5 :: 8 :: Nil) :: (3 :: 6 :: 9 :: Nil) :: Nil, List.transpose((1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: 6 :: Nil) :: (7 :: 8 :: 9 :: Nil) :: Nil))

    @Test
    def transpose20(): Unit \ Assert = assertEq(expected = (1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: Nil) :: (7 :: 8 :: 9 :: Nil) :: Nil, List.transpose((1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: Nil) :: (7 :: 8 :: 9 :: Nil) :: Nil))

    @Test
    def transpose21(): Unit \ Assert = assertEq(expected = (1 :: 2 :: 3 :: Nil) :: Nil :: (7 :: 8 :: 9 :: Nil) :: Nil, List.transpose((1 :: 2 :: 3 :: Nil) :: Nil :: (7 :: 8 :: 9 :: Nil) :: Nil))

    @Test
    def transpose22(): Unit \ Assert = assertEq(expected = (1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: 6 :: Nil) :: (7 :: 8 :: 9 :: 10 :: Nil) :: Nil, List.transpose((1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: 6 :: Nil) :: (7 :: 8 :: 9 :: 10 :: Nil) :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // isPrefixOf                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def isPrefixOf01(): Unit \ Assert = assertEq(expected = true, List.isPrefixOf((Nil: List[Unit]), Nil))

    @Test
    def isPrefixOf02(): Unit \ Assert = assertEq(expected = false, List.isPrefixOf(1 :: Nil, Nil))

    @Test
    def isPrefixOf03(): Unit \ Assert = assertEq(expected = true, List.isPrefixOf(Nil, 1 :: Nil))

    @Test
    def isPrefixOf04(): Unit \ Assert = assertEq(expected = true, List.isPrefixOf(1 :: Nil, 1 :: Nil))

    @Test
    def isPrefixOf05(): Unit \ Assert = assertEq(expected = false, List.isPrefixOf(2 :: Nil, 1 :: Nil))

    @Test
    def isPrefixOf06(): Unit \ Assert = assertEq(expected = false, List.isPrefixOf(1 :: 2 :: Nil, 1 :: Nil))

    @Test
    def isPrefixOf07(): Unit \ Assert = assertEq(expected = true, List.isPrefixOf(Nil, 1 :: 2 :: Nil))

    @Test
    def isPrefixOf08(): Unit \ Assert = assertEq(expected = true, List.isPrefixOf(1 :: Nil, 1 :: 2 :: Nil))

    @Test
    def isPrefixOf09(): Unit \ Assert = assertEq(expected = false, List.isPrefixOf(2 :: Nil, 1 :: 2 :: Nil))

    @Test
    def isPrefixOf10(): Unit \ Assert = assertEq(expected = true, List.isPrefixOf(1 :: 2 :: Nil, 1 :: 2 :: Nil))

    @Test
    def isPrefixOf11(): Unit \ Assert = assertEq(expected = false, List.isPrefixOf(1 :: 3 :: Nil, 1 :: 2 :: Nil))

    @Test
    def isPrefixOf12(): Unit \ Assert = assertEq(expected = false, List.isPrefixOf(1 :: 2 :: 3 :: Nil, 1 :: 2 :: Nil))

    @Test
    def isPrefixOf13(): Unit \ Assert = assertEq(expected = false, List.isPrefixOf(1 :: 2 :: 3 :: Nil, 89 :: 11 :: 1 :: 2 :: 3 :: 4 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // isInfixOf                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def isInfixOf01(): Unit \ Assert = assertEq(expected = true, List.isInfixOf((Nil: List[Unit]), Nil))

    @Test
    def isInfixOf02(): Unit \ Assert = assertEq(expected = false, List.isInfixOf(1 :: Nil, Nil))

    @Test
    def isInfixOf03(): Unit \ Assert = assertEq(expected = true, List.isInfixOf(Nil, 1 :: Nil))

    @Test
    def isInfixOf04(): Unit \ Assert = assertEq(expected = true, List.isInfixOf(1 :: Nil, 1 :: Nil))

    @Test
    def isInfixOf05(): Unit \ Assert = assertEq(expected = false, List.isInfixOf(2 :: Nil, 1 :: Nil))

    @Test
    def isInfixOf06(): Unit \ Assert = assertEq(expected = false, List.isInfixOf(1 :: 2 :: Nil, 1 :: Nil))

    @Test
    def isInfixOf07(): Unit \ Assert = assertEq(expected = true, List.isInfixOf(Nil, 1 :: 2 :: Nil))

    @Test
    def isInfixOf08(): Unit \ Assert = assertEq(expected = true, List.isInfixOf(1 :: Nil, 1 :: 2 :: Nil))

    @Test
    def isInfixOf09(): Unit \ Assert = assertEq(expected = true, List.isInfixOf(2 :: Nil, 1 :: 2 :: Nil))

    @Test
    def isInfixOf10(): Unit \ Assert = assertEq(expected = true, List.isInfixOf(1 :: 2 :: Nil, 1 :: 2 :: Nil))

    @Test
    def isInfixOf11(): Unit \ Assert = assertEq(expected = false, List.isInfixOf(1 :: 3 :: Nil, 1 :: 2 :: Nil))

    @Test
    def isInfixOf12(): Unit \ Assert = assertEq(expected = false, List.isInfixOf(1 :: 2 :: 3 :: Nil, 1 :: 2 :: Nil))

    @Test
    def isInfixOf13(): Unit \ Assert = assertEq(expected = true, List.isInfixOf(1 :: 2 :: 3 :: Nil, 89 :: 11 :: 1 :: 2 :: 3 :: 4 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // isSuffixOf                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def isSuffixOf01(): Unit \ Assert = assertEq(expected = true, List.isSuffixOf((Nil: List[Unit]), (Nil: List[Unit])))

    @Test
    def isSuffixOf02(): Unit \ Assert = assertEq(expected = false, List.isSuffixOf(1 :: Nil, Nil))

    @Test
    def isSuffixOf03(): Unit \ Assert = assertEq(expected = true, List.isSuffixOf(Nil, 1 :: Nil))

    @Test
    def isSuffixOf04(): Unit \ Assert = assertEq(expected = true, List.isSuffixOf(1 :: Nil, 1 :: Nil))

    @Test
    def isSuffixOf05(): Unit \ Assert = assertEq(expected = false, List.isSuffixOf(2 :: Nil, 1 :: Nil))

    @Test
    def isSuffixOf06(): Unit \ Assert = assertEq(expected = false, List.isSuffixOf(1 :: 2 :: Nil, 1 :: Nil))

    @Test
    def isSuffixOf07(): Unit \ Assert = assertEq(expected = true, List.isSuffixOf(Nil, 1 :: 2 :: Nil))

    @Test
    def isSuffixOf08(): Unit \ Assert = assertEq(expected = false, List.isSuffixOf(1 :: Nil, 1 :: 2 :: Nil))

    @Test
    def isSuffixOf09(): Unit \ Assert = assertEq(expected = true, List.isSuffixOf(2 :: Nil, 1 :: 2 :: Nil))

    @Test
    def isSuffixOf10(): Unit \ Assert = assertEq(expected = true, List.isSuffixOf(1 :: 2 :: Nil, 1 :: 2 :: Nil))

    @Test
    def isSuffixOf11(): Unit \ Assert = assertEq(expected = false, List.isSuffixOf(1 :: 3 :: Nil, 1 :: 2 :: Nil))

    @Test
    def isSuffixOf12(): Unit \ Assert = assertEq(expected = false, List.isSuffixOf(1 :: 2 :: 3 :: Nil, 1 :: 2 :: Nil))

    @Test
    def isSuffixOf13(): Unit \ Assert = assertEq(expected = true, List.isSuffixOf(1 :: 2 :: 3 :: Nil, 89 :: 11 :: 1 :: 2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // fold                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def fold01(): Unit \ Assert = assertEq(expected = "", List.fold((Nil: List[String])))

    @Test
    def fold02(): Unit \ Assert = assertEq(expected = "a", List.fold("a" :: Nil))

    @Test
    def fold03(): Unit \ Assert = assertEq(expected = "abc", List.fold("a" :: "b" :: "c" :: Nil))

    @Test
    def fold04(): Unit \ Assert = assertEq(expected = ("ace", "bdf"), List.fold(("a", "b") :: ("c", "d") :: ("e", "f") :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def foldLeft01(): Unit \ Assert = assertEq(expected = 100, List.foldLeft((i, e) -> (i - e)*(e `Int32.remainder` 2 + 1), 100, Nil))

    @Test
    def foldLeft02(): Unit \ Assert = assertEq(expected = 198, List.foldLeft((i, e) -> (i - e)*(e `Int32.remainder` 2 + 1), 100, 1 :: Nil))

    @Test
    def foldLeft03(): Unit \ Assert = assertEq(expected = 196, List.foldLeft((i, e) -> (i - e)*(e `Int32.remainder` 2 + 1), 100, 1 :: 2 :: Nil))

    @Test
    def foldLeft04(): Unit \ Assert = assertEq(expected = 386, List.foldLeft((i, e) -> (i - e)*(e `Int32.remainder` 2 + 1), 100, 1 :: 2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def foldRight01(): Unit \ Assert = assertEq(expected = 100, List.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, Nil))

    @Test
    def foldRight02(): Unit \ Assert = assertEq(expected = 198, List.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, 1 :: Nil))

    @Test
    def foldRight03(): Unit \ Assert = assertEq(expected = 194, List.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, 1 :: 2 :: Nil))

    @Test
    def foldRight04(): Unit \ Assert = assertEq(expected = 382, List.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, 1 :: 2 :: 3 :: Nil))

    @Test
    def foldRight05(): Unit \ Assert = assertEq(expected = 100, List.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, Nil))

    @Test
    def foldRight06(): Unit \ Assert = assertEq(expected = 198, List.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, 1 :: Nil))

    @Test
    def foldRight07(): Unit \ Assert = assertEq(expected = 194, List.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, 1 :: 2 :: Nil))

    @Test
    def foldRight08(): Unit \ Assert = assertEq(expected = 382, List.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, 1 :: 2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def reduceLeft01(): Unit \ Assert = assertEq(expected = None, List.reduceLeft((a, b) -> a-b, (Nil: List[Int32])))

    @Test
    def reduceLeft02(): Unit \ Assert = assertEq(expected = Some(1), List.reduceLeft((a, b) -> a-b, 1 :: Nil))

    @Test
    def reduceLeft03(): Unit \ Assert = assertEq(expected = Some(-1), List.reduceLeft((a, b) -> a-b, 1 :: 2 :: Nil))

    @Test
    def reduceLeft04(): Unit \ Assert = assertEq(expected = Some(-4), List.reduceLeft((a, b) -> a-b, 1 :: 2 :: 3 :: Nil))

    @Test
    def reduceLeft05(): Unit \ Assert = assertEq(expected = Some(-8), List.reduceLeft((a, b) -> a-b, 1 :: 2 :: 3 :: 4 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def reduceRight01(): Unit \ Assert = assertEq(expected = None, List.reduceRight((a, b) -> a-b, (Nil: List[Int32])))

    @Test
    def reduceRight02(): Unit \ Assert = assertEq(expected = Some(1), List.reduceRight((a, b) -> a-b, 1 :: Nil))

    @Test
    def reduceRight03(): Unit \ Assert = assertEq(expected = Some(-1), List.reduceRight((a, b) -> a-b, 1 :: 2 :: Nil))

    @Test
    def reduceRight04(): Unit \ Assert = assertEq(expected = Some(2), List.reduceRight((a, b) -> a-b, 1 :: 2 :: 3 :: Nil))

    @Test
    def reduceRight05(): Unit \ Assert = assertEq(expected = Some(-2), List.reduceRight((a, b) -> a-b, 1 :: 2 :: 3 :: 4 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def count01(): Unit \ Assert = assertEq(expected = 0, List.count(i -> i > 3, Nil))

    @Test
    def count02(): Unit \ Assert = assertEq(expected = 0, List.count(i -> i > 3, 1 :: Nil))

    @Test
    def count03(): Unit \ Assert = assertEq(expected = 1, List.count(i -> i > 3, 4 :: Nil))

    @Test
    def count04(): Unit \ Assert = assertEq(expected = 0, List.count(i -> i > 3, 1 :: 2 :: Nil))

    @Test
    def count05(): Unit \ Assert = assertEq(expected = 1, List.count(i -> i > 3, 1 :: 8 :: Nil))

    @Test
    def count06(): Unit \ Assert = assertEq(expected = 1, List.count(i -> i > 3, 8 :: 1 :: Nil))

    @Test
    def count07(): Unit \ Assert = assertEq(expected = 2, List.count(i -> i > 3, 6 :: 7 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def flatten01(): Unit \ Assert = assertEq(expected = Nil, List.flatten((Nil: List[List[Unit]])))

    @Test
    def flatten02(): Unit \ Assert = assertEq(expected = Nil, (List.flatten(Nil :: Nil): List[Unit]))

    @Test
    def flatten03(): Unit \ Assert = assertEq(expected = 1 :: Nil, List.flatten((1 :: Nil) :: Nil))

    @Test
    def flatten04(): Unit \ Assert = assertEq(expected = 1 :: 2 :: Nil, List.flatten((1 :: 2 :: Nil) :: Nil))

    @Test
    def flatten05(): Unit \ Assert = assertEq(expected = Nil, (List.flatten(Nil :: Nil :: Nil): List[Unit]))

    @Test
    def flatten06(): Unit \ Assert = assertEq(expected = 1 :: Nil, List.flatten((1 :: Nil) :: Nil :: Nil))

    @Test
    def flatten07(): Unit \ Assert = assertEq(expected = 1 :: Nil, List.flatten(Nil :: (1 :: Nil) :: Nil))

    @Test
    def flatten08(): Unit \ Assert = assertEq(expected = 1 :: 2 :: Nil, List.flatten((1 :: Nil) :: (2 :: Nil) :: Nil))

    @Test
    def flatten09(): Unit \ Assert = assertEq(expected = 1 :: 2 :: 3 :: 4 :: 5 :: Nil, List.flatten((1 :: 2 :: Nil) :: (3 :: 4 :: 5 :: Nil) :: Nil))

    @Test
    def flatten10(): Unit \ Assert = assertEq(expected = 1 :: 2 :: 3 :: 4 :: Nil, List.flatten((1 :: Nil) :: (2 :: 3 :: Nil) :: (4 :: Nil) :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def exists01(): Unit \ Assert = assertEq(expected = false, List.exists(i -> i > 3, Nil))

    @Test
    def exists02(): Unit \ Assert = assertEq(expected = false, List.exists(i -> i > 3, 1 :: Nil))

    @Test
    def exists03(): Unit \ Assert = assertEq(expected = true, List.exists(i -> i > 3, 5 :: Nil))

    @Test
    def exists04(): Unit \ Assert = assertEq(expected = false, List.exists(i -> i > 3, 1 :: 2 :: Nil))

    @Test
    def exists05(): Unit \ Assert = assertEq(expected = true, List.exists(i -> i > 3, 1 :: 6 :: Nil))

    @Test
    def exists06(): Unit \ Assert = assertEq(expected = true, List.exists(i -> i > 3, 6 :: 1 :: Nil))

    @Test
    def exists07(): Unit \ Assert = assertEq(expected = true, List.exists(i -> i > 3, 16 :: 6 :: Nil))

    @Test
    def exists08(): Unit \ Assert = assertEq(expected = false, List.exists(i -> i > 3, 1 :: -9 :: 3 :: Nil))

    @Test
    def exists09(): Unit \ Assert = assertEq(expected = true, List.exists(i -> i > 3, 1 :: 9 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def forAll01(): Unit \ Assert = assertEq(expected = true, List.forAll(i -> i > 3, Nil))

    @Test
    def forAll02(): Unit \ Assert = assertEq(expected = false, List.forAll(i -> i > 3, 1 :: Nil))

    @Test
    def forAll03(): Unit \ Assert = assertEq(expected = true, List.forAll(i -> i > 3, 5 :: Nil))

    @Test
    def forAll04(): Unit \ Assert = assertEq(expected = false, List.forAll(i -> i > 3, 1 :: 2 :: Nil))

    @Test
    def forAll05(): Unit \ Assert = assertEq(expected = false, List.forAll(i -> i > 3, 1 :: 6 :: Nil))

    @Test
    def forAll06(): Unit \ Assert = assertEq(expected = false, List.forAll(i -> i > 3, 6 :: 1 :: Nil))

    @Test
    def forAll07(): Unit \ Assert = assertEq(expected = true, List.forAll(i -> i > 3, 16 :: 6 :: Nil))

    @Test
    def forAll08(): Unit \ Assert = assertEq(expected = false, List.forAll(i -> i > 3, 1 :: -9 :: 3 :: Nil))

    @Test
    def forAll09(): Unit \ Assert = assertEq(expected = false, List.forAll(i -> i > 3, 1 :: 9 :: 3 :: Nil))

    @Test
    def forAll10(): Unit \ Assert = assertEq(expected = true, List.forAll(i -> i > 3, 11 :: 9 :: 31 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def filter01(): Unit \ Assert = assertEq(expected = Nil, List.filter(i -> i > 3, Nil))

    @Test
    def filter02(): Unit \ Assert = assertEq(expected = Nil, List.filter(i -> i > 3, 2 :: Nil))

    @Test
    def filter03(): Unit \ Assert = assertEq(expected = 4 :: Nil, List.filter(i -> i > 3, 4 :: Nil))

    @Test
    def filter04(): Unit \ Assert = assertEq(expected = Nil, List.filter(i -> i > 3, 1 :: 3 :: Nil))

    @Test
    def filter05(): Unit \ Assert = assertEq(expected = 8 :: Nil, List.filter(i -> i > 3, 1 :: 8 :: Nil))

    @Test
    def filter06(): Unit \ Assert = assertEq(expected = 8 :: Nil, List.filter(i -> i > 3, 8 :: 1 :: Nil))

    @Test
    def filter07(): Unit \ Assert = assertEq(expected = 8 :: 9 :: Nil, List.filter(i -> i > 3, 8 :: 9 :: Nil))

    @Test
    def filter08(): Unit \ Assert = assertEq(expected = 4 :: 11 :: 17 :: Nil, List.filter(i -> i > 3, 1 :: 4 :: 11 :: 2 :: -22 :: 17 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // init                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def init01(): Unit \ Assert = assertEq(expected = None, List.init((Nil: List[Unit])))

    @Test
    def init02(): Unit \ Assert = assertEq(expected = Some(Nil), List.init(0 :: Nil))

    @Test
    def init03(): Unit \ Assert = assertEq(expected = Some(0 :: Nil), List.init(0 :: 1 :: Nil))

    @Test
    def init04(): Unit \ Assert = assertEq(expected = Some(2 :: 1 :: Nil), List.init(2 :: 1 :: 0 :: Nil))

    @Test
    def init05(): Unit \ Assert = assertEq(expected = Some('a' :: 'b' :: 'c' :: Nil), List.init('a' :: 'b' :: 'c' :: 'd' :: Nil))

    @Test
    def init06(): Unit \ Assert = assertEq(expected = Some("test1" :: "test2" :: Nil), List.init("test1" :: "test2" :: "test3" :: Nil))

    @Test
    def init07(): Unit \ Assert = assertEq(expected = Some(2 :: 1 :: 0 :: Nil), List.init(2 :: 1 :: 0 :: -1 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // slice                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def slice01(): Unit \ Assert = assertEq(expected = Nil, List.slice(start = 0, end = 0, (Nil: List[Unit])))

    @Test
    def slice02(): Unit \ Assert = assertEq(expected = Nil, List.slice(start = -1, end = 1, (Nil: List[Unit])))

    @Test
    def slice03(): Unit \ Assert = assertEq(expected = Nil, List.slice(start = 0, end = 0, 1 :: Nil))

    @Test
    def slice04(): Unit \ Assert = assertEq(expected = 1 :: Nil, List.slice(start = 0, end = 1, 1 :: Nil))

    @Test
    def slice05(): Unit \ Assert = assertEq(expected = 1 :: Nil, List.slice(start = 0, end = 2, 1 :: Nil))

    @Test
    def slice06(): Unit \ Assert = assertEq(expected = Nil, List.slice(start = 2, end = 5, 1 :: Nil))

    @Test
    def slice07(): Unit \ Assert = assertEq(expected = 1 :: Nil, List.slice(start = -1, end = 1, 1 :: Nil))

    @Test
    def slice08(): Unit \ Assert = assertEq(expected = 1 :: Nil, List.slice(start = 0, end = 1, 1 :: 2 :: Nil))

    @Test
    def slice09(): Unit \ Assert = assertEq(expected = 1 :: 2 :: Nil, List.slice(start = 0, end = 2, 1 :: 2 :: Nil))

    @Test
    def slice10(): Unit \ Assert = assertEq(expected = 2 :: Nil, List.slice(start = 1, end = 2, 1 :: 2 :: Nil))

    @Test
    def slice11(): Unit \ Assert = assertEq(expected = 1 :: 2 :: 3 :: Nil, List.slice(start = 0, end = 3, 1 :: 2 :: 3 :: Nil))

    @Test
    def slice12(): Unit \ Assert = assertEq(expected = 1 :: 2 :: Nil, List.slice(start = 0, end = 2, 1 :: 2 :: 3 :: Nil))

    @Test
    def slice13(): Unit \ Assert = assertEq(expected = 2 :: 3 :: Nil, List.slice(start = 1, end = 3, 1 :: 2 :: 3 :: Nil))

    @Test
    def slice14(): Unit \ Assert = assertEq(expected = 2 :: Nil, List.slice(start = 1, end = 2, 1 :: 2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // partition                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def partition01(): Unit \ Assert = assertEq(expected = (Nil, Nil), List.partition(i -> i > 3, Nil))

    @Test
    def partition02(): Unit \ Assert = assertEq(expected = (Nil, 1 :: Nil), List.partition(i -> i > 3, 1 :: Nil))

    @Test
    def partition03(): Unit \ Assert = assertEq(expected = (4 :: Nil, Nil), List.partition(i -> i > 3, 4 :: Nil))

    @Test
    def partition04(): Unit \ Assert = assertEq(expected = (Nil, 1 :: 2 :: Nil), List.partition(i -> i > 3, 1 :: 2 :: Nil))

    @Test
    def partition05(): Unit \ Assert = assertEq(expected = (5 :: Nil, 1 :: Nil), List.partition(i -> i > 3, 1 :: 5 :: Nil))

    @Test
    def partition06(): Unit \ Assert = assertEq(expected = (5 :: Nil, 1 :: Nil), List.partition(i -> i > 3, 5 :: 1 :: Nil))

    @Test
    def partition07(): Unit \ Assert = assertEq(expected = (5 :: 8 :: Nil, Nil), List.partition(i -> i > 3, 5 :: 8 :: Nil))

    @Test
    def partition08(): Unit \ Assert = assertEq(expected = (4 :: 16 :: 7 :: 7 :: Nil, -3 :: -5 :: 1 :: 2 :: 1 :: Nil), List.partition(i -> i > 3, 4 :: -3 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // span                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def span01(): Unit \ Assert = assertEq(expected = (Nil, Nil), List.span(i -> i > 3, Nil))

    @Test
    def span02(): Unit \ Assert = assertEq(expected = (Nil, 1 :: Nil), List.span(i -> i > 3, 1 :: Nil))

    @Test
    def span03(): Unit \ Assert = assertEq(expected = (4 :: Nil, Nil), List.span(i -> i > 3, 4 :: Nil))

    @Test
    def span04(): Unit \ Assert = assertEq(expected = (Nil, 1 :: 2 :: Nil), List.span(i -> i > 3, 1 :: 2 :: Nil))

    @Test
    def span05(): Unit \ Assert = assertEq(expected = (Nil, 1 :: 5 :: Nil), List.span(i -> i > 3, 1 :: 5 :: Nil))

    @Test
    def span06(): Unit \ Assert = assertEq(expected = (5 :: Nil, 1 :: Nil), List.span(i -> i > 3, 5 :: 1 :: Nil))

    @Test
    def span07(): Unit \ Assert = assertEq(expected = (5 :: 8 :: Nil, Nil), List.span(i -> i > 3, 5 :: 8 :: Nil))

    @Test
    def span08(): Unit \ Assert = assertEq(expected = (4 :: 6 :: Nil, -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil), List.span(i -> i > 3, 4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // drop                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def drop01(): Unit \ Assert = assertEq(expected = Nil, List.drop(-1, (Nil: List[Unit])))

    @Test
    def drop02(): Unit \ Assert = assertEq(expected = Nil, List.drop(0, (Nil: List[Unit])))

    @Test
    def drop03(): Unit \ Assert = assertEq(expected = Nil, List.drop(1, (Nil: List[Unit])))

    @Test
    def drop04(): Unit \ Assert = assertEq(expected = 1 :: Nil, List.drop(-1, 1 :: Nil))

    @Test
    def drop05(): Unit \ Assert = assertEq(expected = 1 :: Nil, List.drop(0, 1 :: Nil))

    @Test
    def drop06(): Unit \ Assert = assertEq(expected = Nil, List.drop(1, 1 :: Nil))

    @Test
    def drop07(): Unit \ Assert = assertEq(expected = Nil, List.drop(2, 1 :: Nil))

    @Test
    def drop08(): Unit \ Assert = assertEq(expected = 1 :: 2 :: Nil, List.drop(0, 1 :: 2 :: Nil))

    @Test
    def drop09(): Unit \ Assert = assertEq(expected = 2 :: Nil, List.drop(1, 1 :: 2 :: Nil))

    @Test
    def drop10(): Unit \ Assert = assertEq(expected = Nil, List.drop(2, 1 :: 2 :: Nil))

    @Test
    def drop11(): Unit \ Assert = assertEq(expected = 3 :: 4 :: 5 :: 6 :: Nil, List.drop(2, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil))

    @Test
    def drop12(): Unit \ Assert = assertEq(expected = 5 :: 6 :: Nil, List.drop(4, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // dropWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def dropWhile01(): Unit \ Assert = assertEq(expected = Nil, List.dropWhile(i -> i > 3, Nil))

    @Test
    def dropWhile02(): Unit \ Assert = assertEq(expected = 1 :: Nil, List.dropWhile(i -> i > 3, 1 :: Nil))

    @Test
    def dropWhile03(): Unit \ Assert = assertEq(expected = Nil, List.dropWhile(i -> i > 3, 4 :: Nil))

    @Test
    def dropWhile04(): Unit \ Assert = assertEq(expected = 1 :: 2 :: Nil, List.dropWhile(i -> i > 3, 1 :: 2 :: Nil))

    @Test
    def dropWhile05(): Unit \ Assert = assertEq(expected = 1 :: 5 :: Nil, List.dropWhile(i -> i > 3, 1 :: 5 :: Nil))

    @Test
    def dropWhile06(): Unit \ Assert = assertEq(expected = 1 :: Nil, List.dropWhile(i -> i > 3, 5 :: 1 :: Nil))

    @Test
    def dropWhile07(): Unit \ Assert = assertEq(expected = Nil, List.dropWhile(i -> i > 3, 5 :: 8 :: Nil))

    @Test
    def dropWhile08(): Unit \ Assert = assertEq(expected = -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil, List.dropWhile(i -> i > 3, 4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // take                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def take01(): Unit \ Assert = assertEq(expected = Nil, List.take(-1, (Nil: List[Unit])))

    @Test
    def take02(): Unit \ Assert = assertEq(expected = Nil, List.take(0, (Nil: List[Unit])))

    @Test
    def take03(): Unit \ Assert = assertEq(expected = Nil, List.take(1, (Nil: List[Unit])))

    @Test
    def take04(): Unit \ Assert = assertEq(expected = Nil, List.take(-1, 1 :: Nil))

    @Test
    def take05(): Unit \ Assert = assertEq(expected = Nil, List.take(0, 1 :: Nil))

    @Test
    def take06(): Unit \ Assert = assertEq(expected = 1 :: Nil, List.take(1, 1 :: Nil))

    @Test
    def take07(): Unit \ Assert = assertEq(expected = 1 :: Nil, List.take(2, 1 :: Nil))

    @Test
    def take08(): Unit \ Assert = assertEq(expected = Nil, List.take(0, 1 :: 2 :: Nil))

    @Test
    def take09(): Unit \ Assert = assertEq(expected = 1 :: Nil, List.take(1, 1 :: 2 :: Nil))

    @Test
    def take10(): Unit \ Assert = assertEq(expected = 1 :: 2 :: Nil, List.take(2, 1 :: 2 :: Nil))

    @Test
    def take11(): Unit \ Assert = assertEq(expected = 1 :: 2 :: Nil, List.take(2, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil))

    @Test
    def take12(): Unit \ Assert = assertEq(expected = 1 :: 2 :: 3 :: 4 :: Nil, List.take(4, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // takeWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def takeWhile01(): Unit \ Assert = assertEq(expected = Nil, List.takeWhile(i -> i > 3, (Nil: List[Int32])))

    @Test
    def takeWhile02(): Unit \ Assert = assertEq(expected = Nil, List.takeWhile(i -> i > 3, 1 :: Nil))

    @Test
    def takeWhile03(): Unit \ Assert = assertEq(expected = 4 :: Nil, List.takeWhile(i -> i > 3, 4 :: Nil))

    @Test
    def takeWhile04(): Unit \ Assert = assertEq(expected = Nil, List.takeWhile(i -> i > 3, 1 :: 2 :: Nil))

    @Test
    def takeWhile05(): Unit \ Assert = assertEq(expected = Nil, List.takeWhile(i -> i > 3, 1 :: 5 :: Nil))

    @Test
    def takeWhile06(): Unit \ Assert = assertEq(expected = 5 :: Nil, List.takeWhile(i -> i > 3, 5 :: 1 :: Nil))

    @Test
    def takeWhile07(): Unit \ Assert = assertEq(expected = 5 :: 8 :: Nil, List.takeWhile(i -> i > 3, 5 :: 8 :: Nil))

    @Test
    def takeWhile08(): Unit \ Assert = assertEq(expected = 4 :: 6 :: Nil, List.takeWhile(i -> i > 3, 4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // splitAt                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def splitAt01(): Unit \ Assert =
        let xs = (Nil: List[Int32]);
        assertEq(expected = (Nil, Nil), List.splitAt(0, xs))

    @Test
    def splitAt02(): Unit \ Assert =
        let xs = 1 :: 5 :: 19 :: 3 :: Nil;
        assertEq(expected = (1 :: 5 :: 19 :: Nil, 3 :: Nil), List.splitAt(3, xs))

    @Test
    def splitAt03(): Unit \ Assert =
        let xs = 1 :: 5 :: 19 :: 3 :: Nil;
        assertEq(expected = (Nil, xs), List.splitAt(-3, xs))

    @Test
    def splitAt04(): Unit \ Assert =
        let xs = 1 :: 5 :: 19 :: 3 :: Nil;
        assertEq(expected = (xs, Nil), List.splitAt(5, xs))

    /////////////////////////////////////////////////////////////////////////////
    // groupBy                                                                 //
    /////////////////////////////////////////////////////////////////////////////
     def listToNel(head: v, list: List[v]): Nel[v] =
        (Nel.singleton(head), list) ||> List.foldLeft(acc -> cur -> Nel.cons(cur, acc)) |> Nel.reverse

    @Test
    def groupBy01(): Unit \ Assert = assertEq(expected = Nil, List.groupBy((a, b) -> a > 3 or b > 8, (Nil: List[Int32])))

    @Test
    def groupBy02(): Unit \ Assert = assertEq(expected = (Nel.singleton(1)) :: Nil, List.groupBy((a, b) -> a > 3 or b > 8, 1 :: Nil))

    @Test
    def groupBy03(): Unit \ Assert = assertEq(expected = Nel.singleton(1) :: Nel.singleton(4) :: Nil, List.groupBy((a, b) -> a > 3 or b > 8, 1 :: 4 :: Nil))

    @Test
    def groupBy04(): Unit \ Assert = assertEq(expected = (listToNel(1, 9 :: Nil)) :: Nil, List.groupBy((a, b) -> a > 3 or b > 8, 1 :: 9 :: Nil))

    @Test
    def groupBy05(): Unit \ Assert =
        assertEq(expected = listToNel(1, 9 :: 16 :: Nil) :: listToNel(4, 7 :: 6 :: 4 :: 4 :: 8 :: Nil) :: listToNel(2, Nil) :: Nil, List.groupBy((a, b) -> a > 3 or b > 8, 1 :: 4 :: 7 :: 6 :: 9 :: 2 :: 4 :: 4 :: 8 :: 16 :: Nil))

    @Test
    def groupBy06(): Unit \ Assert =
        assertEq(expected = listToNel(-1, -11 :: -11 :: 0 :: -3 :: -24 :: Nil) :: listToNel(4, 8 :: 2 :: 1 :: Nil) :: Nil, List.groupBy((a, b) -> a > -6 or a*b >= 0, -1 :: -11 :: 4 :: -11 :: 0 :: 8 :: 2 :: 1 :: -3 :: -24 :: Nil))

    @Test
    def groupBy07(): Unit \ Assert =
        assertEq(expected = listToNel(-5, 11 :: -11 :: -1 :: -1 :: Nil) :: listToNel(6, Nil) :: listToNel(8, 8 :: Nil) :: listToNel(0, Nil) :: listToNel(4, Nil) :: Nil, List.groupBy((a, b) -> a < 0 or (a > 10 or (b > 10 or a == b)), -5 :: 6 :: 11 :: 8 :: 8 :: -11 :: -1 :: 0 :: 4 :: -1 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def zip01(): Unit \ Assert = assertEq(expected = Nil, List.zip((Nil: List[Unit]), (Nil: List[Unit])))

    @Test
    def zip02(): Unit \ Assert = assertEq(expected = Nil, List.zip(1 :: Nil, (Nil: List[Unit])))

    @Test
    def zip03(): Unit \ Assert = assertEq(expected = Nil, List.zip((Nil: List[Unit]), 2 :: Nil))

    @Test
    def zip04(): Unit \ Assert = assertEq(expected = (1, 2) :: Nil, List.zip(1 :: Nil, 2 :: Nil))

    @Test
    def zip05(): Unit \ Assert = assertEq(expected = (1, 2) :: (3, 4) :: Nil, List.zip(1 :: 3 :: Nil, 2 :: 4 :: Nil))

    @Test
    def zip06(): Unit \ Assert = assertEq(expected = (1, 2) :: (3, 4) :: (5, 6) :: Nil, List.zip(1 :: 3 :: 5 :: Nil, 2 :: 4 :: 6 :: Nil))

    @Test
    def zip07(): Unit \ Assert = assertEq(expected = (1, 2) :: (3, 4) :: (5, 6) :: (7, 8) :: Nil, List.zip(1 :: 3 :: 5 :: 7 :: Nil, 2 :: 4 :: 6 :: 8 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // zipWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def zipWith01(): Unit \ Assert = assertEq(expected = Nil, List.zipWith((a, b) -> if (b) a+1 else a, Nil, Nil))

    @Test
    def zipWith02(): Unit \ Assert = assertEq(expected = Nil, List.zipWith((a, b) -> if (b) a+1 else a, 1 :: Nil, Nil))

    @Test
    def zipWith03(): Unit \ Assert = assertEq(expected = Nil, List.zipWith((a, b) -> if (b) a+1 else a, Nil, true :: Nil))

    @Test
    def zipWith04(): Unit \ Assert = assertEq(expected = 2 :: Nil, List.zipWith((a, b) -> if (b) a+1 else a, 1 :: Nil, true :: Nil))

    @Test
    def zipWith05(): Unit \ Assert = assertEq(expected = 1 :: Nil, List.zipWith((a, b) -> if (b) a+1 else a, 1 :: Nil, false :: Nil))

    @Test
    def zipWith06(): Unit \ Assert = assertEq(expected = 1 :: 3 :: 4 :: 4 :: 5 :: 7 :: 8 :: 9 :: Nil, List.zipWith((a, b) -> if (b) a+1 else a, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil,
                          false :: true :: true :: false :: false :: true :: true :: true ::Nil))

    /////////////////////////////////////////////////////////////////////////////
    // zipWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def zipWithIndex01(): Unit \ Assert = assertEq(expected = Nil, List.zipWithIndex((Nil : List[Char])))

    @Test
    def zipWithIndex02(): Unit \ Assert = assertEq(expected = (0, 1) :: Nil, List.zipWithIndex(1 :: Nil))

    @Test
    def zipWithIndex03(): Unit \ Assert = assertEq(expected = (0, "a") :: (1, "b") :: Nil, List.zipWithIndex("a" :: "b" :: Nil))

    @Test
    def zipWithIndex04(): Unit \ Assert = assertEq(expected = (0, (0, true)) :: (1, (1, false)) :: Nil, List.zipWithIndex(List.zipWithIndex(true :: false :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // zipWithA                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def zipWithA01(): Unit \ Assert = assertEq(expected = Identity.Identity(Nil), List.zipWithA((a, b) -> {if (b) a+1 else a} |> Identity.Identity, Nil, Nil))

    @Test
    def zipWithA02(): Unit \ Assert = assertEq(expected = Identity.Identity(Nil), List.zipWithA((a, b) -> {if (b) a+1 else a} |> Identity.Identity, 1 :: Nil, Nil))

    @Test
    def zipWithA03(): Unit \ Assert = assertEq(expected = Identity.Identity(Nil), List.zipWithA((a, b) -> {if (b) a+1 else a} |> Identity.Identity, Nil, true :: Nil))

    @Test
    def zipWithA04(): Unit \ Assert = assertEq(expected = Identity.Identity(2 :: Nil), List.zipWithA((a, b) -> {if (b) a+1 else a} |> Identity.Identity, 1 :: Nil, true :: Nil))

    @Test
    def zipWithA05(): Unit \ Assert = assertEq(expected = Identity.Identity(1 :: Nil), List.zipWithA((a, b) -> {if (b) a+1 else a} |> Identity.Identity, 1 :: Nil, false :: Nil))

    @Test
    def zipWithA06(): Unit \ Assert = assertEq(expected = Identity.Identity(1 :: 3 :: 4 :: 4 :: 5 :: 7 :: 8 :: 9 :: Nil), List.zipWithA((a, b) -> {if (b) a+1 else a} |> Identity.Identity, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil,
                          false :: true :: true :: false :: false :: true :: true :: true :: Nil))


    /////////////////////////////////////////////////////////////////////////////
    // unzip                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def unzip01(): Unit \ Assert = assertEq(expected = (Nil, Nil), List.unzip((Nil: List[(Unit, Unit)])))

    @Test
    def unzip02(): Unit \ Assert = assertEq(expected = (1 :: Nil, true :: Nil), List.unzip((1, true) :: Nil))

    @Test
    def unzip03(): Unit \ Assert = assertEq(expected = (1 :: 2 :: Nil, true :: true :: Nil), List.unzip((1, true) :: (2, true) :: Nil))

    @Test
    def unzip04(): Unit \ Assert = assertEq(expected = (1 :: 2 :: 3 :: Nil, true :: true :: false :: Nil), List.unzip((1, true) :: (2, true) :: (3, false) :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // zip3                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def zip301(): Unit \ Assert = assertEq(expected = Nil, List.zip3((Nil: List[Unit]),(Nil: List[Unit]), (Nil: List[Unit])))

    @Test
    def zip302(): Unit \ Assert = assertEq(expected = Nil, List.zip3(1 :: Nil, (Nil: List[Unit]), (Nil: List[Unit])))

    @Test
    def zip303(): Unit \ Assert = assertEq(expected = Nil, List.zip3((Nil: List[Unit]), 2 :: Nil, (Nil: List[Unit])))

    @Test
    def zip304(): Unit \ Assert = assertEq(expected = Nil, List.zip3((Nil: List[Unit]), (Nil: List[Unit]), 3 :: Nil))

    @Test
    def zip305(): Unit \ Assert = assertEq(expected = (1, 2, 3) :: Nil, List.zip3(1 :: Nil, 2 :: Nil, 3 :: Nil))

    @Test
    def zip306(): Unit \ Assert = assertEq(expected = (1, 2, 3) :: (4, 5, 6) :: Nil, List.zip3(1 :: 4 :: Nil, 2 :: 5 :: Nil, 3 :: 6 :: Nil))

    @Test
    def zip307(): Unit \ Assert = assertEq(expected = (1, 2, 3) :: (4, 5, 6) :: (7, 8, 9) :: Nil, List.zip3(1 :: 4 :: 7 :: Nil, 2 :: 5 :: 8 :: Nil, 3 :: 6 :: 9 :: Nil))

    @Test
    def zip308(): Unit \ Assert = assertEq(expected = (1, 2, 3) :: (4, 5, 6) :: (7, 8, 9) :: (10, 11, 12) :: Nil, List.zip3(1 :: 4 :: 7 :: 10 :: Nil, 2 :: 5 :: 8 :: 11 :: Nil, 3 :: 6 :: 9 :: 12 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // zipWith3                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def zipWith301(): Unit \ Assert = assertEq(expected = Nil, List.zipWith3((a, b, c) -> if (c) a else b, (Nil: List[Int32]), Nil, Nil))

    @Test
    def zipWith302(): Unit \ Assert = assertEq(expected = Nil, List.zipWith3((a, b, c) -> if (c) a else b, 1 :: Nil, Nil, Nil))

    @Test
    def zipWith303(): Unit \ Assert = assertEq(expected = Nil, List.zipWith3((a, b, c) -> if (c) a else b, Nil, 10 :: Nil, Nil))

    @Test
    def zipWith304(): Unit \ Assert = assertEq(expected = Nil, List.zipWith3((a, b, c) -> if (c) a else b, (Nil: List[Int32]), Nil, true :: Nil))

    @Test
    def zipWith305(): Unit \ Assert = assertEq(expected = 1 :: Nil, List.zipWith3((a, b, c) -> if (c) a else b, 1 :: Nil, 10 :: Nil, true :: Nil))

    @Test
    def zipWith306(): Unit \ Assert = assertEq(expected = 10 :: Nil, List.zipWith3((a, b, c) -> if (c) a else b, 1 :: Nil, 10 :: Nil, false :: Nil))

    @Test
    def zipWith307(): Unit \ Assert = assertEq(expected = 10 :: 2 :: 3 :: 13 :: 14 :: 6 :: 7 :: 8 :: Nil, List.zipWith3((a, b, c) -> if (c) a else b,
                                1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil,
                                10 :: 11 :: 12 :: 13 :: 14 :: 15 :: 16 :: 17 :: Nil,
                                false :: true :: true :: false :: false :: true :: true :: true ::Nil))

    /////////////////////////////////////////////////////////////////////////////
    // unzip3                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def unzip301(): Unit \ Assert = assertEq(expected = (Nil, Nil, Nil), List.unzip3((Nil: List[(Unit, Unit, Unit)])))

    @Test
    def unzip302(): Unit \ Assert = assertEq(expected = (1 :: Nil, 'a' :: Nil, true :: Nil), List.unzip3((1, 'a', true) :: Nil))

    @Test
    def unzip303(): Unit \ Assert = assertEq(expected = (1 :: 2 :: Nil, 'a' :: 'b' :: Nil, true :: true :: Nil), List.unzip3((1, 'a', true) :: (2, 'b', true) :: Nil))

    @Test
    def unzip304(): Unit \ Assert = assertEq(expected = (1 :: 2 :: 3 :: Nil, 'a' :: 'b' :: 'c' :: Nil, true :: true :: false :: Nil), List.unzip3((1, 'a', true) :: (2, 'b', true) :: (3, 'c', false) :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // fold2                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def fold201(): Unit \ Assert = assertEq(expected = 4, List.fold2((c, a, b) -> if (b) a+c else a*c, 4, Nil, Nil))

    @Test
    def fold202(): Unit \ Assert = assertEq(expected = 4, List.fold2((c, a, b) -> if (b) a+c else a*c, 4, 1 :: Nil, Nil))

    @Test
    def fold203(): Unit \ Assert = assertEq(expected = 4, List.fold2((c, a, b) -> if (b) a+c else a*c, 4, Nil, true :: Nil))

    @Test
    def fold204(): Unit \ Assert = assertEq(expected = 6, List.fold2((c, a, b) -> if (b) a+c else a*c, 4, 2 :: Nil, true :: Nil))

    @Test
    def fold205(): Unit \ Assert = assertEq(expected = 8, List.fold2((c, a, b) -> if (b) a+c else a*c, 4, 2 :: Nil, false :: Nil))

    @Test
    def fold206(): Unit \ Assert = assertEq(expected = 9, List.fold2((c, a, b) -> if (b) a+c else a*c, 4, 3 :: 2 :: Nil, true :: true :: Nil))

    @Test
    def fold207(): Unit \ Assert = assertEq(expected = 14, List.fold2((c, a, b) -> if (b) a+c else a*c, 4, 3 :: 2 :: Nil, true :: false :: Nil))

    @Test
    def fold208(): Unit \ Assert = assertEq(expected = 14, List.fold2((c, a, b) -> if (b) a+c else a*c, 4, 3 :: 2 :: Nil, false :: true :: Nil))

    @Test
    def fold209(): Unit \ Assert = assertEq(expected = 24, List.fold2((c, a, b) -> if (b) a+c else a*c, 4, 3 :: 2 :: Nil, false :: false :: Nil))

    @Test
    def fold210(): Unit \ Assert = assertEq(expected = -118, List.fold2((c, a, b) -> if (b) a+c else a*c, 4, 6 :: -4 :: 3 :: 2 :: Nil,
                        true :: false :: false :: true :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft2                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def foldLeft201(): Unit \ Assert = assertEq(expected = 4, List.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, Nil, Nil))

    @Test
    def foldLeft202(): Unit \ Assert = assertEq(expected = 4, List.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, 1 :: Nil, Nil))

    @Test
    def foldLeft203(): Unit \ Assert = assertEq(expected = 4, List.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, Nil, true :: Nil))

    @Test
    def foldLeft204(): Unit \ Assert = assertEq(expected = 6, List.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, 2 :: Nil, true :: Nil))

    @Test
    def foldLeft205(): Unit \ Assert = assertEq(expected = 8, List.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, 2 :: Nil, false :: Nil))

    @Test
    def foldLeft206(): Unit \ Assert = assertEq(expected = 9, List.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, 3 :: 2 :: Nil, true :: true :: Nil))

    @Test
    def foldLeft207(): Unit \ Assert = assertEq(expected = 14, List.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, 3 :: 2 :: Nil, true :: false :: Nil))

    @Test
    def foldLeft208(): Unit \ Assert = assertEq(expected = 14, List.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, 3 :: 2 :: Nil, false :: true :: Nil))

    @Test
    def foldLeft209(): Unit \ Assert = assertEq(expected = 24, List.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, 3 :: 2 :: Nil, false :: false :: Nil))

    @Test
    def foldLeft210(): Unit \ Assert = assertEq(expected = -118, List.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, 6 :: -4 :: 3 :: 2 :: Nil,
                            true :: false :: false :: true :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // foldRight2                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def foldRight201(): Unit \ Assert = assertEq(expected = 4, List.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, Nil, Nil))

    @Test
    def foldRight202(): Unit \ Assert = assertEq(expected = 4, List.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, 1 :: Nil, Nil))

    @Test
    def foldRight203(): Unit \ Assert = assertEq(expected = 4, List.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, Nil, true :: Nil))

    @Test
    def foldRight204(): Unit \ Assert = assertEq(expected = 6, List.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, 2 :: Nil, true :: Nil))

    @Test
    def foldRight205(): Unit \ Assert = assertEq(expected = 8, List.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, 2 :: Nil, false :: Nil))

    @Test
    def foldRight206(): Unit \ Assert = assertEq(expected = 9, List.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, 3 :: 2 :: Nil, true :: true :: Nil))

    @Test
    def foldRight207(): Unit \ Assert = assertEq(expected = 11, List.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, 3 :: 2 :: Nil, true :: false :: Nil))

    @Test
    def foldRight208(): Unit \ Assert = assertEq(expected = 18, List.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, 3 :: 2 :: Nil, false :: true :: Nil))

    @Test
    def foldRight209(): Unit \ Assert = assertEq(expected = 24, List.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, 3 :: 2 :: Nil, false :: false :: Nil))

    @Test
    def foldRight210(): Unit \ Assert = assertEq(expected = -66, List.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, 6 :: -4 :: 3 :: 2 :: Nil,
                             true :: false :: false :: true :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldMap01(): Unit \ Assert =
        assertEq(expected = 0, List.foldMap(x -> 2 * x, Nil))

    @Test
    def foldMap02(): Unit \ Assert =
        assertEq(expected = 6, List.foldMap(x -> 2 * x, 1 :: 2 :: Nil))

    @Test
    def foldMap03(): Unit \ Assert =
        assertEq(expected = "b", List.foldMap(x -> if (x == "a") "b" else x, "a" :: Nil))

    @Test
    def foldMap04(): Unit \ Assert =
        assertEq(expected = "abb", List.foldMap(x -> if (x == "c") "b" else x, "a" :: "b" :: "c" :: Nil))

    @Test
    def foldMap05(): Unit \ Assert =
        assertEq(expected = "123", List.foldMap(Int32.toString, 1 :: 2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // filterMap                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def filterMap01(): Unit \ Assert = assertEq(expected = Nil, List.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Nil))

    @Test
    def filterMap02(): Unit \ Assert = assertEq(expected = Nil, List.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, 1 :: Nil))

    @Test
    def filterMap03(): Unit \ Assert = assertEq(expected = 1 :: Nil, List.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, 2 :: Nil))

    @Test
    def filterMap04(): Unit \ Assert = assertEq(expected = Nil, List.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, 1 :: 3 :: Nil))

    @Test
    def filterMap05(): Unit \ Assert = assertEq(expected = 2 :: Nil, List.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, 1 :: 4 :: Nil))

    @Test
    def filterMap06(): Unit \ Assert = assertEq(expected = 3 :: Nil, List.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, 6 :: -1 :: Nil))

    @Test
    def filterMap07(): Unit \ Assert = assertEq(expected = 4 :: 3 :: Nil, List.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, 8 :: 6 :: Nil))

    @Test
    def filterMap08(): Unit \ Assert = assertEq(expected = 0 :: 1 :: 2 :: 5 :: Nil, List.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 10 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // findMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def findMap01(): Unit \ Assert = assertEq(expected = None, List.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Nil))

    @Test
    def findMap02(): Unit \ Assert = assertEq(expected = None, List.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, 1 :: Nil))

    @Test
    def findMap03(): Unit \ Assert = assertEq(expected = Some(1), List.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, 2 :: Nil))

    @Test
    def findMap04(): Unit \ Assert = assertEq(expected = None, List.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, 1 :: 3 :: Nil))

    @Test
    def findMap05(): Unit \ Assert = assertEq(expected = Some(2), List.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, 1 :: 4 :: Nil))

    @Test
    def findMap06(): Unit \ Assert = assertEq(expected = Some(3), List.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, 6 :: -1 :: Nil))

    @Test
    def findMap07(): Unit \ Assert = assertEq(expected = Some(4), List.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, 8 :: 6 :: Nil))

    @Test
    def findMap08(): Unit \ Assert = assertEq(expected = Some(0), List.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 10 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toSet01(): Unit \ Assert = assertEq(expected = Set#{}, List.toSet((Nil: List[Unit])))

    @Test
    def toSet02(): Unit \ Assert = assertEq(expected = Set#{1}, List.toSet(1 :: Nil))

    @Test
    def toSet03(): Unit \ Assert = assertEq(expected = Set#{1, 2}, List.toSet(1 :: 2 :: Nil))

    @Test
    def toSet04(): Unit \ Assert = assertEq(expected = Set#{1}, List.toSet(1 :: 1 :: Nil))

    @Test
    def toSet05(): Unit \ Assert = assertEq(expected = Set#{1, 2, 3}, List.toSet(1 :: 2 :: 3 :: Nil))

    @Test
    def toSet06(): Unit \ Assert = assertEq(expected = Set#{1, 2}, List.toSet(1 :: 2 :: 1 :: Nil))

    @Test
    def toSet07(): Unit \ Assert = assertEq(expected = Set#{1, 2}, List.toSet(1 :: 1 :: 2 :: Nil))

    @Test
    def toSet08(): Unit \ Assert = assertEq(expected = Set#{1, 2, 3, 4, 5, 6}, List.toSet(2 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 3 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toMap01(): Unit \ Assert = assertEq(expected = Map#{}, List.toMap((Nil: List[(Unit, Unit)])))

    @Test
    def toMap02(): Unit \ Assert = assertEq(expected = Map#{1 => true}, List.toMap((1, true) :: Nil))

    @Test
    def toMap03(): Unit \ Assert = assertEq(expected = Map#{1 => true, 2 => false}, List.toMap((1, true) :: (2, false) :: Nil))

    @Test
    def toMap04(): Unit \ Assert = assertEq(expected = Map#{1 => false}, List.toMap((1, true) :: (1, false) :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // toMapWith                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMapWith01(): Unit \ Assert =
        assertEq(expected = Map#{}, List.toMapWith(x -> (x, 2 * x), Nil))

    @Test
    def toMapWith02(): Unit \ Assert =
        assertEq(expected = Map#{1 => 0, 3 => 0, 6 => 0}, List.toMapWith(x -> (x, 0 * x), 1 :: 3 :: 6 :: Nil))

    @Test
    def toMapWith03(): Unit \ Assert =
        assertEq(expected = Map#{1 => 2, 3 => 6, 6 => 12}, List.toMapWith(x -> (x, 2 * x), 1 :: 3 :: 6 :: Nil))

    @Test
    def toMapWith04(): Unit \ Assert =
        assertEq(expected = Map#{0 => 11, 2 => 13, 5 => 16}, List.toMapWith(x -> (x - 1, x + 10), 1 :: 3 :: 6 :: Nil))

    @Test
    def toMapWith05(): Unit \ Assert =
        assertEq(expected = Map#{2 => 11, 4 => 13, 7 => 16}, List.toMapWith(x -> (x + 1, x + 10), 3 :: 6 :: 1 :: Nil))

    @Test
    def toMapWith06(): Unit \ Assert =
        assertEq(expected = Map#{3 => 5}, List.toMapWith(match (_, x) -> (3, x), (3, 4) :: (4, 5) :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Unit \ Assert = region rc {
        let ri = Ref.fresh(rc, 21);
        List.forEach(x -> Ref.put(x, ri), Nil);
        assertEq(expected = 21, Ref.get(ri))
    }

    @Test
    def forEach02(): Unit \ Assert = region rc {
        let ri = Ref.fresh(rc, 21);
        List.forEach(x -> Ref.put(x, ri), 1 :: 2 :: 3 :: Nil);
        assertEq(expected = 3, Ref.get(ri))
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEachWithIndex01(): Unit \ Assert = region rc {
        let ri = Ref.fresh(rc, 21);
        List.forEachWithIndex((i, _) -> Ref.put(i, ri), Nil);
        assertEq(expected = 21, Ref.get(ri))
    }

    @Test
    def forEachWithIndex02(): Unit \ Assert = region rc {
        let ri = Ref.fresh(rc, 21);
        List.forEachWithIndex((i, _) -> Ref.put(i, ri), 1 :: 2 :: 3 :: Nil);
        assertEq(expected = 2, Ref.get(ri))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toArray01(): Unit \ Assert = region rc {
        let a = (List.toArray(rc, Nil): Array[Int32, rc]);
        assertTrue(Array.sameElements(a, Array#{} @ rc))
    }

    @Test
    def toArray02(): Unit \ Assert = region rc {
        let a = List.toArray(rc, 1 :: Nil);
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def toArray03(): Unit \ Assert = region rc {
        let a = List.toArray(rc, 1 :: 2 :: Nil);
        assertTrue(Array.sameElements(a, Array#{1, 2} @ rc))
    }

    @Test
    def toArray04(): Unit \ Assert = region rc {
        let a = List.toArray(rc, 1 :: 2 :: 3 :: Nil);
        assertTrue(Array.sameElements(a, Array#{1, 2, 3} @ rc))
    }


    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toVector01(): Unit \ Assert =
        assertTrue((List.toVector(Nil) : Vector[Int32]) `Vector.equals` Vector.empty())

    @Test
    def toVector02(): Unit \ Assert =
        assertTrue(List.toVector(0 :: Nil) `Vector.equals` Vector.singleton(0))

    @Test
    def toVector03(): Unit \ Assert =
        assertTrue(List.toVector(1 :: 2 :: 3 :: Nil) `Vector.equals` Vector.range(1, 4))


    /////////////////////////////////////////////////////////////////////////////
    // toNel                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toNel01(): Unit \ Assert = assertEq(expected = None, List.toNel((Nil: List[Unit])))

    @Test
    def toNel02(): Unit \ Assert = assertEq(expected = Some(Nel.Nel(1, Nil)), List.toNel(1 :: Nil))

    @Test
    def toNel03(): Unit \ Assert = assertEq(expected = Some(Nel.Nel(1, 2 :: Nil)), List.toNel(1 :: 2 :: Nil))

    @Test
    def toNel04(): Unit \ Assert = assertEq(expected = Some(Nel.Nel(1, 1 :: Nil)), List.toNel(1 :: 1 :: Nil))

    @Test
    def toNel05(): Unit \ Assert = assertEq(expected = Some(Nel.Nel(2, 1 :: Nil)), List.toNel(2 :: 1 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // toNec                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toNec01(): Unit \ Assert = assertEq(expected = None, List.toNec((Nil: List[Unit])))

    @Test
    def toNec02(): Unit \ Assert = assertEq(expected = Some(Nec.singleton(1)), List.toNec(1 :: Nil))

    @Test
    def toNec03(): Unit \ Assert = assertEq(expected = Some(Nec.cons(1, Nec.singleton(2))), List.toNec(1 :: 2 :: Nil))

    @Test
    def toNec04(): Unit \ Assert = assertEq(expected = Some(Nec.cons(1, Nec.singleton(1))), List.toNec(1 :: 1 :: Nil))

    @Test
    def toNec05(): Unit \ Assert = assertEq(expected = Some(Nec.cons(2, Nec.singleton(1))), List.toNec(2 :: 1 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // sortWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    def cmp(x: Int32, y: Int32): Comparison =
        if (x < y) Comparison.LessThan
        else if (x == y) Comparison.EqualTo
        else Comparison.GreaterThan

    @Test
    def sortWith01(): Unit \ Assert =
        assertEq(expected = Nil, List.sortWith(cmp, (Nil: List[Int32])))

    @Test
    def sortWith02(): Unit \ Assert =
        assertEq(expected = 0 :: Nil, List.sortWith(cmp, 0 :: Nil))

    @Test
    def sortWith03(): Unit \ Assert =
        assertEq(expected = 0 :: 1 :: Nil, List.sortWith(cmp, 0 :: 1 :: Nil))

    @Test
    def sortWith04(): Unit \ Assert =
        assertEq(expected = 0 :: 1 :: Nil, List.sortWith(cmp, 1 :: 0 :: Nil))

    @Test
    def sortWith05(): Unit \ Assert =
        assertEq(expected = 1 :: 1 :: Nil, List.sortWith(cmp, 1 :: 1 :: Nil))

    @Test
    def sortWith06(): Unit \ Assert =
        assertEq(expected = 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil, List.sortWith(cmp, 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil))

    @Test
    def sortWith07(): Unit \ Assert =
        assertEq(expected = 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil, List.sortWith(cmp, 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil))

    @Test
    def sortWith08(): Unit \ Assert =
        assertEq(expected = 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil, List.sortWith(cmp, 5 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil))

    @Test
    def sortWith09(): Unit \ Assert =
        assertEq(expected = 0 :: 1 :: 2 :: 2 :: 3 :: 4 :: Nil, List.sortWith(cmp, 2 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil))

    @Test
    def sortWith10(): Unit \ Assert =
        assertEq(expected = 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil, List.sortWith(flip(cmp), 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil))

    @Test
    def sortWith11(): Unit \ Assert =
        assertEq(expected = 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil, List.sortWith(flip(cmp), 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil))

    @Test
    def sortWith12(): Unit \ Assert =
        assertEq(expected = 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil, List.sortWith(flip(cmp), 5 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil))

    @Test
    def sortWith13(): Unit \ Assert =
        assertEq(expected = 4 :: 3 :: 2 :: 2 :: 1 :: 0 :: Nil, List.sortWith(flip(cmp), 2 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // sort                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    def testSortVsSortWith(xs: List[Int32]) : Unit \ Assert =
        assertEq(expected = List.sortWith(cmp, xs), List.sort(xs))

    @Test
    def sort01(): Unit \ Assert = testSortVsSortWith((Nil: List[Int32]))

    @Test
    def sort02(): Unit \ Assert = testSortVsSortWith(0 :: Nil)

    @Test
    def sort03(): Unit \ Assert = testSortVsSortWith(0 :: 1 :: Nil)

    @Test
    def sort04(): Unit \ Assert = testSortVsSortWith(1 :: 0 :: Nil)

    @Test
    def sort05(): Unit \ Assert = testSortVsSortWith(1 :: 1 :: Nil)

    @Test
    def sort06(): Unit \ Assert = testSortVsSortWith(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil)

    @Test
    def sort07(): Unit \ Assert = testSortVsSortWith(5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil)

    @Test
    def sort08(): Unit \ Assert = testSortVsSortWith(5 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil)

    @Test
    def sort09(): Unit \ Assert = testSortVsSortWith(2 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // sortBy                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    def testSortByVsSort(a: List[Int32]) : Bool =
        (List.sortBy(identity, a) == List.sort(a)) and
        (List.sortBy(x -> 4*x+7, a) == List.sort(a)) and
        (List.sortBy(x -> -x, a) == List.sortWith(flip(cmp),a))

    @Test
    def sortBy01(): Unit \ Assert = assertTrue(testSortByVsSort((Nil: List[Int32])))

    @Test
    def sortBy02(): Unit \ Assert = assertTrue(testSortByVsSort(0 :: Nil))

    @Test
    def sortBy03(): Unit \ Assert = assertTrue(testSortByVsSort(0 :: 1 :: Nil))

    @Test
    def sortBy04(): Unit \ Assert = assertTrue(testSortByVsSort(1 :: 0 :: Nil))

    @Test
    def sortBy05(): Unit \ Assert = assertTrue(testSortByVsSort(1 :: 1 :: Nil))

    @Test
    def sortBy06(): Unit \ Assert = assertTrue(testSortByVsSort(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil))

    @Test
    def sortBy07(): Unit \ Assert = assertTrue(testSortByVsSort(5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil))

    @Test
    def sortBy08(): Unit \ Assert = assertTrue(testSortByVsSort(5 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil))

    @Test
    def sortBy09(): Unit \ Assert = assertTrue(testSortByVsSort(2 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil))


    /////////////////////////////////////////////////////////////////////////////
    // unfold                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unfold01(): Unit \ Assert =
        assertEq(expected = Nil, List.unfold(s -> if (true) None else Some((s + 48, s + 1)), 0))

    @Test
    def unfold02(): Unit \ Assert =
        assertEq(expected = 48 :: Nil, List.unfold(s -> if (s > 0) None else Some((s + 48, s + 1)), 0))

    @Test
    def unfold03(): Unit \ Assert =
        assertEq(expected = 48 :: 49 :: Nil, List.unfold(s -> if (s > 1) None else Some((s + 48, s + 1)), 0))

    @Test
    def unfold04(): Unit \ Assert =
        assertEq(expected = 48 :: 49 :: 50 :: 51 :: 52 :: 53 :: 54 :: 55 :: 56 :: 57 :: Nil, List.unfold(s -> if (s >= 10) None else Some((s + 48, s + 1)), 0))

    @Test
    def unfold05(): Unit \ Assert =
        assertEq(expected = 53 :: 54 :: 55 :: 56 :: 57 :: Nil, List.unfold(s -> if (s >= 10) None else Some((s + 48, s + 1)), 5))

    @Test
    def unfold06(): Unit \ Assert =
        assertEq(expected = 48 :: 50 :: 52 :: 54 :: 56 :: Nil, List.unfold(s -> if (s >= 10) None else Some((s + 48, s + 2)), 0))

    /////////////////////////////////////////////////////////////////////////////
    // unfoldWithIter                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unfoldWithIter01(): Unit \ Assert = region rc {
        let x = Ref.fresh(rc, 0);
        let step = () ->
            if (true)
                None
            else {
                let c = Some(Ref.get(x) + 48);
                Ref.put(Ref.get(x) + 1, x);
                c
            };
        assertEq(expected = Nil, List.unfoldWithIter(step))
    }

    @Test
    def unfoldWithIter02(): Unit \ Assert = region rc {
        let x = Ref.fresh(rc, 0);
        let step = () ->
            if (Ref.get(x) > 0)
                None
            else {
                let c = Some(Ref.get(x) + 48);
                Ref.put(Ref.get(x) + 1, x);
                c
            };
        assertEq(expected = 48 :: Nil, List.unfoldWithIter(step))
    }

    @Test
    def unfoldWithIter03(): Unit \ Assert = region rc {
        let x = Ref.fresh(rc, 0);
        let step = () ->
            if (Ref.get(x) > 1)
                None
            else {
                let c = Some(Ref.get(x) + 48);
                Ref.put(Ref.get(x) + 1, x);
                c
            };
        assertEq(expected = 48 :: 49 :: Nil, List.unfoldWithIter(step))
    }

    @Test
    def unfoldWithIter04(): Unit \ Assert = region rc {
        let x = Ref.fresh(rc, 0);
        let step = () ->
            if (Ref.get(x) >= 10)
                None
            else {
                let c = Some(Ref.get(x) + 48);
                Ref.put(Ref.get(x) + 1, x);
                c
            };
        assertEq(expected = 48 :: 49 :: 50 :: 51 :: 52 :: 53 :: 54 :: 55 :: 56 :: 57 :: Nil, List.unfoldWithIter(step))
    }

    @Test
    def unfoldWithIter05(): Unit \ Assert = region rc {
        let x = Ref.fresh(rc, 5);
        let step = () ->
            if (Ref.get(x) >= 10)
                None
            else {
                let c = Some(Ref.get(x) + 48);
                Ref.put(Ref.get(x) + 1, x);
                c
            };
        assertEq(expected = 53 :: 54 :: 55 :: 56 :: 57 :: Nil, List.unfoldWithIter(step))
    }

    @Test
    def unfoldWithIter06(): Unit \ Assert = region rc {
        let x = Ref.fresh(rc, 0);
        let step = () ->
            if (Ref.get(x) >= 10)
                None
            else {
                let c = Some(Ref.get(x) + 48);
                Ref.put(Ref.get(x) + 2, x);
                c
            };
        assertEq(expected = 48 :: 50 :: 52 :: 54 :: 56 :: Nil, List.unfoldWithIter(step))
    }

    /////////////////////////////////////////////////////////////////////////////
    // unfoldWithOkIter                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unfoldWithOkIter01(): Unit \ Assert = region rc {
        let x = Ref.fresh(rc, 0);
        let step = () ->
            if (true) {
                Ok(None)
            } else {
                Ok(Some(Ref.get(x)))
            };
        assertEq(expected = Ok(Nil), (List.unfoldWithOkIter(step): Result[Unit, _]))
    }

    @Test
    def unfoldWithOkIter02(): Unit \ Assert = region rc {
        let x = Ref.fresh(rc, 0);
        let step = () ->
            if (true) {
                Err(Ref.get(x))
            } else {
                Ok(Some(Ref.get(x)))
            };
        assertEq(expected = Err(0), List.unfoldWithOkIter(step))
    }

    @Test
    def unfoldWithOkIter03(): Unit \ Assert = region rc {
        let x = Ref.fresh(rc, 1);
        let step = () ->
            if (Ref.get(x) `Int32.remainder` 5 > 0) {
                let c = Ref.get(x);
                Ref.put(Ref.get(x) + 1, x);
                Ok(Some(c))
            } else {
                Ok(None)
            };
        assertEq(expected = Ok(1 :: 2 :: 3 :: 4 :: Nil), (List.unfoldWithOkIter(step): Result[Unit, _]))
    }

    /////////////////////////////////////////////////////////////////////////////
    // distinct                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def distinct01(): Unit \ Assert =
        assertEq(expected = Nil, List.distinct((Nil: List[Unit])))

    @Test
    def distinct02(): Unit \ Assert =
        assertEq(expected = 1 :: Nil, List.distinct(1 :: Nil))

    @Test
    def distinct03(): Unit \ Assert =
        assertEq(expected = 1 :: 2 :: Nil, List.distinct(1 :: 2 :: Nil))

    @Test
    def distinct04(): Unit \ Assert =
        assertEq(expected = 1 :: 2 :: Nil, List.distinct(1 :: 1 :: 2 :: Nil))

    @Test
    def distinct05(): Unit \ Assert =
        assertEq(expected = 1 :: 2 :: Nil, List.distinct(1 :: 2 :: 1 :: Nil))

    @Test
    def distinct06(): Unit \ Assert =
        assertEq(expected = 1 :: 2 :: Nil, List.distinct(1 :: 2 :: 2 :: Nil))

    @Test
    def distinct07(): Unit \ Assert =
        assertEq(expected = 1 :: 2 :: 3 :: Nil, List.distinct(1 :: 2 :: 2 :: 3 :: Nil))

    @Test
    def distinct08(): Unit \ Assert =
        assertEq(expected = 1 :: 2 :: 3 :: 4 :: Nil, List.distinct(1 :: 2 :: 2 :: 3 :: 1 :: 4 :: Nil))

    @Test
    def distinct09(): Unit \ Assert =
        assertEq(expected = 4 :: 3 :: 2 :: 1 :: Nil, List.distinct(4 :: 4 :: 3  :: 4 :: 2 :: 1 :: 1 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // distinctWith                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def distinctWith01(): Unit \ Assert =
        assertEq(expected = Nil, List.distinctWith((x,y) -> x == y, (Nil: List[Unit])))

    @Test
    def distinctWith02(): Unit \ Assert =
        assertEq(expected = 1 :: Nil, List.distinctWith((x,y) -> x == y, 1 :: Nil))

    @Test
    def distinctWith03(): Unit \ Assert =
        assertEq(expected = 1 :: 2 :: Nil, List.distinctWith((x,y) -> x == y, 1 :: 2 :: Nil))

    @Test
    def distinctWith04(): Unit \ Assert =
        assertEq(expected = 1 :: 2 :: Nil, List.distinctWith((x,y) -> x == y, 1 :: 1 :: 2 :: Nil))

    @Test
    def distinctWith05(): Unit \ Assert =
        assertEq(expected = 1 :: 2 :: Nil, List.distinctWith((x,y) -> x == y, 1 :: 2 :: 1 :: Nil))

    @Test
    def distinctWith06(): Unit \ Assert =
        assertEq(expected = 1 :: 2 :: Nil, List.distinctWith((x,y) -> x == y, 1 :: 2 :: 2 :: Nil))

    @Test
    def distinctWith07(): Unit \ Assert =
        assertEq(expected = 1 :: 2 :: 3 :: Nil, List.distinctWith((x,y) -> x == y, 1 :: 2 :: 2 :: 3 :: Nil))

    @Test
    def distinctWith08(): Unit \ Assert =
        assertEq(expected = 1 :: 2 :: 3 :: 4 :: Nil, List.distinctWith((x,y) -> x == y, 1 :: 2 :: 2 :: 3 :: 1 :: 4 :: Nil))

    @Test
    def distinctWith09(): Unit \ Assert =
        assertEq(expected = 4 :: 3 :: 2 :: 1 :: Nil, List.distinctWith((x,y) -> x == y, 4 :: 4 :: 3  :: 4 :: 2 :: 1 :: 1 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toString01(): Unit \ Assert =
        assertEq(expected = "1 :: Nil", toString(1 :: Nil))

    @Test
    def toString02(): Unit \ Assert =
        assertEq(expected = "1 :: 2 :: Nil", toString(1 :: 2 :: Nil))

    @Test
    def toString03(): Unit \ Assert =
        assertEq(expected = "93 :: 3 :: 4 :: Nil", toString(93 :: 3 :: 4 :: Nil))

    @Test
    def toString04(): Unit \ Assert =
        assertEq(expected = "a :: b :: c :: Nil", toString('a' :: 'b' :: 'c' :: Nil))

    @Test
    def toString05(): Unit \ Assert =
        assertEq(expected = "true :: false :: true :: true :: Nil", toString(true :: false :: true :: true :: Nil))

    @Test
    def toString06(): Unit \ Assert =
        assertEq(expected = "1 :: 2 :: Nil :: 2 :: 3 :: Nil :: 4 :: 7 :: Nil :: Nil", toString((1 :: 2 :: Nil) :: (2 :: 3 :: Nil) :: (4 :: 7 :: Nil) :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // hash                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def hash01(): Unit \ Assert = assertEq(expected = hash((Nil: List[Unit])), hash((Nil: List[Unit])))

    @Test
    def hash02(): Unit \ Assert = assertEq(expected = hash(1 :: Nil), hash(1 :: Nil))

    @Test
    def hash03(): Unit \ Assert = assertEq(expected = hash(1 :: 2 :: Nil), hash(1 :: 2 :: Nil))

    @Test
    def hash04(): Unit \ Assert = assertNeq(unexpected = hash(2 :: 1 :: Nil), hash(1 :: 2 :: Nil))

    @Test
    def hash05(): Unit \ Assert = assertNeq(unexpected = hash("c" :: "b" :: "a" :: Nil), hash("c" :: "a" :: "b" :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // mimimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimumBy01(): Unit \ Assert = assertEq(expected = None, List.minimumBy((x, y) -> x <=> y, (Nil: List[Int32])))

    @Test
    def minimumBy02(): Unit \ Assert = assertEq(expected = Some(1), List.minimumBy((x, y) -> x <=> y, 1 :: Nil))

    @Test
    def minimumBy03(): Unit \ Assert = assertEq(expected = Some(1), List.minimumBy((x, y) -> x <=> y, 3 :: 2 :: 1 :: Nil))

    @Test
    def minimumBy04(): Unit \ Assert = assertEq(expected = Some(0), List.minimumBy((x, y) -> x <=> y, 0 :: 3 :: 2 :: 1 :: Nil))

    @Test
    def minimumBy05(): Unit \ Assert = assertEq(expected = Some(0), List.minimumBy((_, _) -> Comparison.LessThan, List.range(0, 99)))

    /////////////////////////////////////////////////////////////////////////////
    // maximumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maximumBy01(): Unit \ Assert = assertEq(expected = None, List.maximumBy((x, y) -> x <=> y, (Nil: List[Int32])))

    @Test
    def maximumBy02(): Unit \ Assert = assertEq(expected = Some(1), List.maximumBy((x, y) -> x <=> y, 1 :: Nil))

    @Test
    def maximumBy03(): Unit \ Assert = assertEq(expected = Some(3), List.maximumBy((x, y) -> x <=> y, 3 :: 2 :: 1 :: Nil))

    @Test
    def maximumBy04(): Unit \ Assert = assertEq(expected = Some(3), List.maximumBy((x, y) -> x <=> y, 0 :: 3 :: 2 :: 1 :: Nil))

    @Test
    def maximumBy05(): Unit \ Assert = assertEq(expected = Some(98), List.maximumBy((_, _) -> Comparison.LessThan, List.range(0, 99)))


    /////////////////////////////////////////////////////////////////////////////
    // toChain                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toChain01(): Unit \ Assert =
        assertEq(expected = (Chain.empty(): Chain[Int32]), List.toChain((Nil: List[Int32])))

    @Test
    def toChain02(): Unit \ Assert =
        assertEq(expected = Chain.singleton(1), List.toChain(1 :: Nil))

    @Test
    def toChain03(): Unit \ Assert =
        assertEq(expected = (1 `Chain.cons` (2 `Chain.cons` Chain.empty())), List.toChain(1 :: 2 :: Nil))

    @Test
    def toChain04(): Unit \ Assert =
        assertEq(expected = (1 `Chain.cons` (2 `Chain.cons` (3 `Chain.cons` Chain.empty()))), List.toChain(1 :: 2 :: 3 :: Nil))


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sum01(): Unit \ Assert =
        assertEq(expected = 0, List.sum(Nil))

    @Test
    def sum02(): Unit \ Assert =
        assertEq(expected = 1, List.sum(1 :: Nil))

    @Test
    def sum03(): Unit \ Assert =
        assertEq(expected = 6, List.sum(1 :: 2 :: 3 :: Nil))

    @Test
    def sum04(): Unit \ Assert =
        assertEq(expected = 3, List.sum(1 :: 2 :: 3 :: -3 :: Nil))

    @Test
    def sum05(): Unit \ Assert =
        assertEq(expected = -10, List.sum(-1 :: -2 :: -3 :: -4 :: Nil))

    @Test
    def sum06(): Unit \ Assert =
        assertEq(expected = 0, List.sum(10 :: -10 :: Nil))

    @Test
    def sum07(): Unit \ Assert =
        assertEq(expected = 5050, List.range(1, 101) |> List.sum)


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumWith01(): Unit \ Assert =
        assertEq(expected = 0, List.sumWith(x -> x + 1, (Nil: List[Int32])))

    @Test
    def sumWith02(): Unit \ Assert =
        assertEq(expected = 2, List.sumWith(x -> x + 1, 1 :: Nil))

    @Test
    def sumWith03(): Unit \ Assert =
        assertEq(expected = 9, List.sumWith(x -> x + 1, 1 :: 2 :: 3 :: Nil))

    @Test
    def sumWith04(): Unit \ Assert =
        assertEq(expected = 7, List.sumWith(x -> x + 1, 1 :: 2 :: 3 :: -3 :: Nil))

    @Test
    def sumWith05(): Unit \ Assert =
        assertEq(expected = -6, List.sumWith(x -> x + 1, -1 :: -2 :: -3 :: -4 :: Nil))

    @Test
    def sumWith06(): Unit \ Assert =
        assertEq(expected = 2, List.sumWith(x -> x + 1, 10 :: -10 :: Nil))


    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterator01(): Unit \ Assert = region rc {
        assertEq(expected = Nil, (Nil: List[Int32]) |> List.iterator(rc) |> Iterator.toList)
    }

    @Test
    def iterator02(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: 2 :: 3 :: Nil, (1 :: 2 :: 3 :: Nil) |> List.iterator(rc) |> Iterator.toList)
    }

    @Test
    def iterator03(): Unit \ Assert = region rc {
        assertEq(expected = List.range(-100, 100), List.range(-100, 100) |> List.iterator(rc) |> Iterator.toList)
    }


    /////////////////////////////////////////////////////////////////////////////
    // sequence                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sequence01(): Unit \ Assert =
        let l: List[Identity[Int32]] = Nil;
        assertEq(expected = Identity.Identity(Nil), List.sequence(l))

    @Test
    def sequence02(): Unit \ Assert =
        let l = Identity.Identity(1) :: Nil;
        assertEq(expected = Identity.Identity(1 :: Nil), List.sequence(l))

    @Test
    def sequence03(): Unit \ Assert =
        let l = Identity.Identity(1) :: Identity.Identity(2) :: Nil;
        assertEq(expected = Identity.Identity(1 :: 2 :: Nil), List.sequence(l))

    @Test
    def sequence04(): Unit \ Assert =
        let l = Identity.Identity(1) :: Identity.Identity(2) :: Identity.Identity(3) :: Nil;
        assertEq(expected = Identity.Identity(1 :: 2 :: 3 :: Nil), List.sequence(l))

    /////////////////////////////////////////////////////////////////////////////
    // traverse                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def traverse01(): Unit \ Assert = region rc {
        let st = Ref.fresh(rc, 0);
        let l = Nil;
        let ans = List.traverse(x -> {Ref.put(x, st); Identity.Identity(x)}, l);
        assertEq(expected = Identity.Identity(Nil), ans);
        assertEq(expected = 0, Ref.get(st))
    }

    @Test
    def traverse02(): Unit \ Assert = region rc {
        let st = Ref.fresh(rc, 0);
        let l = 1 :: Nil;
        let ans = List.traverse(x -> {Ref.put(x, st); Identity.Identity(x)}, l);
        assertEq(expected = Identity.Identity(1 :: Nil), ans);
        assertEq(expected = 1, Ref.get(st))
    }

    @Test
    def traverse03(): Unit \ Assert = region rc {
        let st = Ref.fresh(rc, 0);
        let l = 1 :: 2 :: Nil;
        let ans = List.traverse(x -> {Ref.put(x, st); Identity.Identity(x)}, l);
        assertEq(expected = Identity.Identity(1 :: 2 :: Nil), ans);
        assertEq(expected = 2, Ref.get(st))
    }

    @Test
    def traverse04(): Unit \ Assert = region rc {
        let st = Ref.fresh(rc, 0);
        let l = 1 :: 2 :: 3 :: Nil;
        let ans = List.traverse(x -> {Ref.put(x, st); Identity.Identity(x)}, l);
        assertEq(expected = Identity.Identity(1 :: 2 :: 3 :: Nil), ans);
        assertEq(expected = 3, Ref.get(st))
    }

    /////////////////////////////////////////////////////////////////////////////
    // merge                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def merge01(): Unit \ Assert =
        let l1 = (Nil: List[Int32]);
        let l2 = (Nil: List[Int32]);
        assertEq(expected = Nil, List.merge(l1, l2))

    @Test
    def merge02(): Unit \ Assert =
        let l1 = 1 :: Nil;
        let l2 = (Nil: List[Int32]);
        assertEq(expected = 1 :: Nil, List.merge(l1, l2))

    @Test
    def merge03(): Unit \ Assert =
        let l1 = 1 :: Nil;
        let l2 = 2 :: Nil;
        assertEq(expected = 1 :: 2 :: Nil, List.merge(l1, l2))

    @Test
    def merge04(): Unit \ Assert =
        let l1 = 2 :: Nil;
        let l2 = 1 :: Nil;
        assertEq(expected = 1 :: 2 :: Nil, List.merge(l1, l2))

    @Test
    def merge05(): Unit \ Assert =
        let l1 = 1 :: 4 :: 8 :: 10 :: Nil;
        let l2 = 2 :: 3 :: 8 :: 9 :: Nil;
        assertEq(expected = 1 :: 2 :: 3 :: 4 :: 8 :: 8 :: 9 :: 10 :: Nil, List.merge(l1, l2))

    @Test
    def merge06(): Unit \ Assert =
        let l1 = 4 :: 5 :: Nil;
        let l2 = 2 :: 3 :: 6 :: 7 :: Nil;
        assertEq(expected = 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil, List.merge(l1, l2))


    /////////////////////////////////////////////////////////////////////////////
    // shuffle                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def shuffle01(): Unit \ {Assert, NonDet} =
        run {
            let l1: List[Int32] = Nil;
            let l2 = List.shuffle(l1);

            assertEq(expected = 0, List.length(l2));
            assertEq(expected = Set#{}, List.toSet(l2))
        } with Shuffle.runWithIO

    @Test
    def shuffle02(): Unit \ {Assert, NonDet} =
        run {
            let l1 = 1 :: Nil;
            let l2 = List.shuffle(l1);

            assertEq(expected = 1, List.length(l2));
            assertEq(expected = Set#{1}, List.toSet(l2))
        } with Shuffle.runWithIO

    @Test
    def shuffle03(): Unit \ {Assert, NonDet} =
        run {
            let l1 = 1 :: 2 :: 3 :: Nil;
            let l2 = List.shuffle(l1);

            assertEq(expected = 3, List.length(l2));
            assertEq(expected = Set#{1, 2, 3}, List.toSet(l2))
        } with Shuffle.runWithIO

    @Test
    def shuffle04(): Unit \ {Assert, NonDet} =
        run {
            let l1 = 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: Nil;
            let l2 = List.shuffle(l1);

            assertEq(expected = 10, List.length(l2));
            assertEq(expected = Set#{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}, List.toSet(l2))
        } with Shuffle.runWithIO

    /////////////////////////////////////////////////////////////////////////////
    // Literals                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def literal01(): Unit \ Assert =
        assertEq(expected = (Nil: List[String]), List#{})

    @Test
    def literal02(): Unit \ Assert =
        assertEq(expected = 1 :: Nil, List#{1})

    @Test
    def literal03(): Unit \ Assert =
        assertEq(expected = 1 :: 2 :: 3 :: Nil, List#{1, 2, 3})

    @Test
    def literal04(): Unit \ Assert =
        assertEq(expected = 'a' :: 'b' :: 'c' :: Nil, List#{'a', 'b', 'c'})

    @Test
    def literal05(): Unit \ Assert =
        assertEq(expected = (1 :: 2 :: Nil) :: (3 :: Nil) :: (4 :: 5 :: Nil) :: Nil, List#{List#{1, 2}, List#{3}, List#{4, 5}})

    @Test
    def literal06(): Unit \ Assert =
        assertFalse(List.isEmpty(List#{{}}))

    /////////////////////////////////////////////////////////////////////////////
    // frequency                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def frequency01(): Unit \ Assert =
        let l = List#{"a"};
        let m = Map#{"a" => 1};
        assertEq(expected = m, List.frequency(l))

    @Test
    def frequency02(): Unit \ Assert =
        let l = List#{"a", "b", "c", "a", "b", "a"};
        let m = Map#{"a" => 3, "b" => 2, "c" => 1};
        assertEq(expected = m, List.frequency(l))

    @Test
    def frequency03(): Unit \ Assert =
        let l = List#{"b", "c", "a", "a", "b", "a", "c", "a"};
        let m = Map#{"a" => 4, "b" => 2, "c" => 2};
        assertEq(expected = m, List.frequency(l))

    @Test
    def frequency04(): Unit \ Assert =
        let l = (List#{}:List[Int32]);
        let m = Map#{};
        assertEq(expected = m, List.frequency(l))
}
