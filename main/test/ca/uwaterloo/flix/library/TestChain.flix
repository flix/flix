mod TestChain {

    use Assert.{assertEq, assertTrue, assertFalse};

    /////////////////////////////////////////////////////////////////////////////
    // Collectable.collect                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def collect01(): Unit \ Assert = region rc {
        assertEq(expected = Chain.empty(), Collectable.collect((Iterator.empty(rc): Iterator[Int32, rc, rc])))
    }

    @Test
    def collect02(): Unit \ Assert = region rc {
        assertEq(expected = List.toChain(1 :: Nil), Collectable.collect(Iterator.singleton(rc, 1)))
    }

    @Test
    def collect03(): Unit \ Assert = region rc {
        assertEq(expected = List.toChain(1 :: 2 :: Nil), Collectable.collect(Iterator.range(rc, 1, 3)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // empty                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def empty01(): Unit \ Assert =
        let c: Chain[Int32] = Chain.empty();
        assertEq(expected = Nil, Chain.toList(c))

    /////////////////////////////////////////////////////////////////////////////
    // singleton                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def singleton01(): Unit \ Assert =
        let c = Chain.singleton(1);
        assertEq(expected = 1 :: Nil, Chain.toList(c))

    @Test
    def singleton02(): Unit \ Assert =
        let c: Chain[Option[Int32]] = Chain.singleton(None);
        assertEq(expected = None :: Nil, Chain.toList(c))

    /////////////////////////////////////////////////////////////////////////////
    // ap                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def ap01(): Unit \ Assert = assertEq(expected = (Chain.empty() : Chain[Int32]), Chain.ap(Chain.empty(), Chain.empty()))

    @Test
    def ap02(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.ap(Chain.singleton(x -> x+1), Chain.empty()))

    @Test
    def ap03(): Unit \ Assert = assertEq(expected = (Chain.empty() : Chain[Int32]), Chain.ap(Chain.empty(), Chain.singleton(5)))

    @Test
    def ap04(): Unit \ Assert = assertEq(expected = Chain.singleton(6), Chain.ap(Chain.singleton(x -> x+1), Chain.singleton(5)))

    @Test
    def ap05(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 6 :: Nil), Chain.ap(Chain.singleton(x -> x+1), List.toChain(0 :: 5 :: Nil)))

    @Test
    def ap06(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 5 :: 0 :: 8 :: Nil), Chain.ap(List.toChain((x -> x+1) :: (x -> x*2) :: Nil), List.toChain(0 :: 4 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isEmpty01(): Unit \ Assert = assertTrue(Chain.isEmpty(Chain.empty()))

    @Test
    def isEmpty02(): Unit \ Assert = assertFalse(Chain.isEmpty(Chain.singleton(1)))

    @Test
    def isEmpty03(): Unit \ Assert = assertFalse(Chain.isEmpty(List.toChain(1 :: 2 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // nonEmpty                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def nonEmpty01(): Unit \ Assert = assertFalse(Chain.nonEmpty(Chain.empty()))

    @Test
    def nonEmpty02(): Unit \ Assert = assertTrue(Chain.nonEmpty(Chain.singleton(1)))

    @Test
    def nonEmpty03(): Unit \ Assert = assertTrue(Chain.nonEmpty(List.toChain(1 :: 2 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // cons                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def cons01(): Unit \ Assert = assertEq(expected = Chain.singleton(1), Chain.cons(1, Chain.empty()))

    @Test
    def cons02(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 2 :: Nil), Chain.cons(1, Chain.singleton(2)))

    /////////////////////////////////////////////////////////////////////////////
    // snoc                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def snoc01(): Unit \ Assert = assertEq(expected = Chain.singleton(1), Chain.snoc(Chain.empty(), 1))

    @Test
    def snoc02(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 2 :: Nil), Chain.snoc(Chain.singleton(1), 2))

    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def head01(): Unit \ Assert = assertEq(expected = None, Chain.head((Chain.empty(): Chain[Unit])))

    @Test
    def head02(): Unit \ Assert = assertEq(expected = Some(1), Chain.head(Chain.singleton(1)))

    @Test
    def head03(): Unit \ Assert = assertEq(expected = Some(2), Chain.head(List.toChain(2 :: 1 :: Nil)))

    @Test
    def head04(): Unit \ Assert = assertEq(expected = Some(3), Chain.head(List.toChain(3 :: 2 :: 1 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def last01(): Unit \ Assert = assertEq(expected = None, Chain.last((Chain.empty(): Chain[Unit])))

    @Test
    def last02(): Unit \ Assert = assertEq(expected = Some(1), Chain.last(Chain.singleton(1)))

    @Test
    def last03(): Unit \ Assert = assertEq(expected = Some(2), Chain.last(List.toChain(1 :: 2 :: Nil)))

    @Test
    def last04(): Unit \ Assert = assertEq(expected = Some(3), Chain.last(List.toChain(1 :: 2 :: 3 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // init                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def init01(): Unit \ Assert = assertEq(expected = None, Chain.init((Chain.empty(): Chain[Unit])))

    @Test
    def init02(): Unit \ Assert = assertEq(expected = Some(Chain.empty()), Chain.init(Chain.singleton(0)))

    @Test
    def init03(): Unit \ Assert = assertEq(expected = Some(Chain.singleton(0)), Chain.init(List.toChain(0 :: 1 :: Nil)))

    @Test
    def init04(): Unit \ Assert = assertEq(expected = Some(List.toChain(2 :: 1 :: Nil)), Chain.init(List.toChain(2 :: 1 :: 0 :: Nil)))

    @Test
    def init05(): Unit \ Assert = assertEq(expected = Some(List.toChain('a' :: 'b' :: 'c' :: Nil)), Chain.init(List.toChain('a' :: 'b' :: 'c' :: 'd' :: Nil)))

    @Test
    def init06(): Unit \ Assert = assertEq(expected = Some(List.toChain("test1" :: "test2" :: Nil)), Chain.init(List.toChain("test1" :: "test2" :: "test3" :: Nil)))

    @Test
    def init07(): Unit \ Assert = assertEq(expected = Some(List.toChain(2 :: 1 :: 0 :: Nil)), Chain.init(List.toChain(2 :: 1 :: 0 :: -1 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def length01(): Unit \ Assert = assertEq(expected = 0, Chain.length(Chain.empty()))

    @Test
    def length02(): Unit \ Assert = assertEq(expected = 1, Chain.length(Chain.singleton(1)))

    @Test
    def length03(): Unit \ Assert = assertEq(expected = 2, Chain.length(List.toChain(1 :: 2 :: Nil)))

    @Test
    def length04(): Unit \ Assert = assertEq(expected = 3, Chain.length(List.toChain(1 :: 2 :: 3 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def size01(): Unit \ Assert = assertEq(expected = 0, Chain.size(Chain.empty()))

    @Test
    def size02(): Unit \ Assert = assertEq(expected = 1, Chain.size(Chain.singleton(1)))

    @Test
    def size03(): Unit \ Assert = assertEq(expected = 2, Chain.size(List.toChain(1 :: 2 :: Nil)))

    @Test
    def size04(): Unit \ Assert = assertEq(expected = 3, Chain.size(List.toChain(1 :: 2 :: 3 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def append01(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.append((Chain.empty(): Chain[Unit]), (Chain.empty(): Chain[Unit])))

    @Test
    def append02(): Unit \ Assert = assertEq(expected = Chain.singleton(1), Chain.append(Chain.empty(), Chain.singleton(1)))

    @Test
    def append03(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 2 :: Nil), Chain.append(Chain.empty(), List.toChain(1 :: 2 :: Nil)))

    @Test
    def append04(): Unit \ Assert = assertEq(expected = Chain.singleton(1), Chain.append(Chain.singleton(1), Chain.empty()))

    @Test
    def append05(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 2 :: Nil), Chain.append(List.toChain(1 :: 2 :: Nil), Chain.empty()))

    @Test
    def append06(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 2 :: Nil), Chain.append(Chain.singleton(1), Chain.singleton(2)))

    @Test
    def append07(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 2 :: 3 :: Nil), Chain.append(List.toChain(1 :: 2 :: Nil), Chain.singleton(3)))

    @Test
    def append08(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 2 :: 3 :: Nil), Chain.append(Chain.singleton(1), List.toChain(2 :: 3 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // viewLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def viewLeft01(): Unit \ Assert = {
        match Chain.viewLeft((Chain.empty(): Chain[Int32])) {
            case Chain.ViewLeft.NoneLeft => assertTrue(true)
            case _ => assertTrue(false)
        }
    }

    @Test
    def viewLeft02(): Unit \ Assert = {
        match Chain.viewLeft(Chain.singleton(1)) {
            case Chain.ViewLeft.SomeLeft(x, xs) => assertEq(expected = (1, Chain.empty()), (x, xs))
            case _ => assertTrue(false)
        }
    }

    @Test
    def viewLeft03(): Unit \ Assert = {
        match Chain.viewLeft(List.toChain(1 :: 2 :: Nil)) {
            case Chain.ViewLeft.SomeLeft(x, xs) => assertEq(expected = (1, Chain.singleton(2)), (x, xs))
            case _ => assertTrue(false)
        }
    }

    @Test
    def viewLeft04(): Unit \ Assert = {
        match Chain.viewLeft(List.toChain(1 :: 2 :: 3 :: Nil)) {
            case Chain.ViewLeft.SomeLeft(x, xs) => assertEq(expected = (1, List.toChain(2 :: 3 :: Nil)), (x, xs))
            case _ => assertTrue(false)
        }
    }

    /////////////////////////////////////////////////////////////////////////////
    // viewRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def viewRight01(): Unit \ Assert = {
        match Chain.viewRight((Chain.empty(): Chain[Int32])) {
            case Chain.ViewRight.NoneRight => assertTrue(true)
            case _ => assertTrue(false)
        }
    }

    @Test
    def viewRight02(): Unit \ Assert = {
        match Chain.viewRight(Chain.singleton(1)) {
            case Chain.ViewRight.SomeRight(xs, x) => assertEq(expected = (Chain.empty(), 1), (xs, x))
            case _ => assertTrue(false)
        }
    }

    @Test
    def viewRight03(): Unit \ Assert = {
        match Chain.viewRight(List.toChain(1 :: 2 :: Nil)) {
            case Chain.ViewRight.SomeRight(xs, x) => assertEq(expected = (Chain.singleton(1), 2), (xs, x))
            case _ => assertTrue(false)
        }
    }

    @Test
    def viewRight04(): Unit \ Assert = {
        match Chain.viewRight(List.toChain(1 :: 2 :: 3 :: Nil)) {
            case Chain.ViewRight.SomeRight(xs, x) => assertEq(expected = (List.toChain(1 :: 2 :: Nil), 3), (xs, x))
            case _ => assertTrue(false)
        }
    }

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def memberOf01(): Unit \ Assert = assertFalse(Chain.memberOf(0, Chain.empty()))

    @Test
    def memberOf02(): Unit \ Assert = assertFalse(Chain.memberOf(0, Chain.singleton(1)))

    @Test
    def memberOf03(): Unit \ Assert = assertTrue(Chain.memberOf(0, Chain.singleton(0)))

    @Test
    def memberOf04(): Unit \ Assert = assertFalse(Chain.memberOf(0, List.toChain(1 :: 2 :: Nil)))

    @Test
    def memberOf05(): Unit \ Assert = assertTrue(Chain.memberOf(1, List.toChain(1 :: 2 :: Nil)))

    @Test
    def memberOf06(): Unit \ Assert = assertTrue(Chain.memberOf(2, List.toChain(1 :: 2 :: Nil)))

    @Test
    def memberOf07(): Unit \ Assert = assertFalse(Chain.memberOf(3, List.toChain(1 :: 2 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // indexOf                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indexOf01(): Unit \ Assert = assertEq(expected = None, Chain.indexOf(0, Chain.empty()))

    @Test
    def indexOf02(): Unit \ Assert = assertEq(expected = None, Chain.indexOf(0, Chain.singleton(1)))

    @Test
    def indexOf03(): Unit \ Assert = assertEq(expected = Some(0), Chain.indexOf(1, Chain.singleton(1)))

    @Test
    def indexOf04(): Unit \ Assert = assertEq(expected = None, Chain.indexOf(0, List.toChain(1 :: 2 :: Nil)))

    @Test
    def indexOf05(): Unit \ Assert = assertEq(expected = Some(0), Chain.indexOf(1, List.toChain(1 :: 2 :: Nil)))

    @Test
    def indexOf06(): Unit \ Assert = assertEq(expected = Some(1), Chain.indexOf(2, List.toChain(1 :: 2 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // find                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def find01(): Unit \ Assert = assertEq(expected = None, Chain.find(i -> i > 2, Chain.empty()))

    @Test
    def find02(): Unit \ Assert = assertEq(expected = None, Chain.find(i -> i > 2, Chain.singleton(1)))

    @Test
    def find03(): Unit \ Assert = assertEq(expected = Some(3), Chain.find(i -> i > 2, Chain.singleton(3)))

    @Test
    def find04(): Unit \ Assert = assertEq(expected = None, Chain.find(i -> i > 2, List.toChain(1 :: 2 :: Nil)))

    @Test
    def find05(): Unit \ Assert = assertEq(expected = Some(6), Chain.find(i -> i > 2, List.toChain(6 :: -6 :: Nil)))

    @Test
    def find06(): Unit \ Assert = assertEq(expected = Some(6), Chain.find(i -> i > 2, List.toChain(-6 :: 6 :: Nil)))

    @Test
    def find07(): Unit \ Assert = assertEq(expected = Some(6), Chain.find(i -> i > 2, List.toChain(6 :: 7 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findLeft01(): Unit \ Assert = assertEq(expected = None, Chain.findLeft(i -> i > 2, Chain.empty()))

    @Test
    def findLeft02(): Unit \ Assert = assertEq(expected = None, Chain.findLeft(i -> i > 2, Chain.singleton(1)))

    @Test
    def findLeft03(): Unit \ Assert = assertEq(expected = Some(3), Chain.findLeft(i -> i > 2, Chain.singleton(3)))

    @Test
    def findLeft04(): Unit \ Assert = assertEq(expected = None, Chain.findLeft(i -> i > 2, List.toChain(1 :: 2 :: Nil)))

    @Test
    def findLeft05(): Unit \ Assert = assertEq(expected = Some(6), Chain.findLeft(i -> i > 2, List.toChain(6 :: -6 :: Nil)))

    @Test
    def findLeft06(): Unit \ Assert = assertEq(expected = Some(6), Chain.findLeft(i -> i > 2, List.toChain(-6 :: 6 :: Nil)))

    @Test
    def findLeft07(): Unit \ Assert = assertEq(expected = Some(6), Chain.findLeft(i -> i > 2, List.toChain(6 :: 7 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findRight01(): Unit \ Assert = assertEq(expected = None, Chain.findRight(i -> i > 2, Chain.empty()))

    @Test
    def findRight02(): Unit \ Assert = assertEq(expected = None, Chain.findRight(i -> i > 2, Chain.singleton(1)))

    @Test
    def findRight03(): Unit \ Assert = assertEq(expected = Some(3), Chain.findRight(i -> i > 2, Chain.singleton(3)))

    @Test
    def findRight04(): Unit \ Assert = assertEq(expected = None, Chain.findRight(i -> i > 2, List.toChain(1 :: 2 :: Nil)))

    @Test
    def findRight05(): Unit \ Assert = assertEq(expected = Some(6), Chain.findRight(i -> i > 2, List.toChain(6 :: -6 :: Nil)))

    @Test
    def findRight06(): Unit \ Assert = assertEq(expected = Some(6), Chain.findRight(i -> i > 2, List.toChain(-6 :: 6 :: Nil)))

    @Test
    def findRight07(): Unit \ Assert = assertEq(expected = Some(7), Chain.findRight(i -> i > 2, List.toChain(6 :: 7 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def range01(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.range(1, 0))

    @Test
    def range02(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.range(1, 1))

    @Test
    def range03(): Unit \ Assert = assertEq(expected = Chain.singleton(1), Chain.range(1, 2))

    @Test
    def range04(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 2 :: Nil), Chain.range(1, 3))

    @Test
    def range05(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 2 :: 3 :: Nil), Chain.range(1, 4))

    /////////////////////////////////////////////////////////////////////////////
    // repeat                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def repeat01(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.repeat(-1, 1))

    @Test
    def repeat02(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.repeat(0, 1))

    @Test
    def repeat03(): Unit \ Assert = assertEq(expected = Chain.singleton(1), Chain.repeat(1, 1))

    @Test
    def repeat04(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 1 :: Nil), Chain.repeat(2, 1))

    @Test
    def repeat05(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 1 :: 1 :: Nil), Chain.repeat(3, 1))

    /////////////////////////////////////////////////////////////////////////////
    // scan                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def scan01(): Unit \ Assert = assertEq(expected = Chain.singleton(1), Chain.scan((i, b) -> if (b) i+1 else i+2, 1, Chain.empty()))

    @Test
    def scan02(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 3 :: Nil), Chain.scan((i, b) -> if (b) i+1 else i+2, 1, Chain.singleton(false)))

    @Test
    def scan03(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 2 :: Nil), Chain.scan((i, b) -> if (b) i+1 else i+2, 1, Chain.singleton(true)))

    @Test
    def scan04(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 3 :: 5 :: Nil), Chain.scan((i, b) -> if (b) i+1 else i+2, 1, List.toChain(false :: false :: Nil)))

    @Test
    def scan05(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 3 :: 4 :: Nil), Chain.scan((i, b) -> if (b) i+1 else i+2, 1, List.toChain(false :: true :: Nil)))

    @Test
    def scan06(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 2 :: 4 :: Nil), Chain.scan((i, b) -> if (b) i+1 else i+2, 1, List.toChain(true :: false :: Nil)))

    @Test
    def scan07(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 2 :: 3 :: Nil), Chain.scan((i, b) -> if (b) i+1 else i+2, 1, List.toChain(true :: true :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // scanLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def scanLeft01(): Unit \ Assert = assertEq(expected = Chain.singleton(1), Chain.scanLeft((i, b) -> if (b) i+1 else i+2, 1, Chain.empty()))

    @Test
    def scanLeft02(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 3 :: Nil), Chain.scanLeft((i, b) -> if (b) i+1 else i+2, 1, Chain.singleton(false)))

    @Test
    def scanLeft03(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 2 :: Nil), Chain.scanLeft((i, b) -> if (b) i+1 else i+2, 1, Chain.singleton(true)))

    @Test
    def scanLeft04(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 3 :: 5 :: Nil), Chain.scanLeft((i, b) -> if (b) i+1 else i+2, 1, List.toChain(false :: false :: Nil)))

    @Test
    def scanLeft05(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 3 :: 4 :: Nil), Chain.scanLeft((i, b) -> if (b) i+1 else i+2, 1, List.toChain(false :: true :: Nil)))

    @Test
    def scanLeft06(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 2 :: 4 :: Nil), Chain.scanLeft((i, b) -> if (b) i+1 else i+2, 1, List.toChain(true :: false :: Nil)))

    @Test
    def scanLeft07(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 2 :: 3 :: Nil), Chain.scanLeft((i, b) -> if (b) i+1 else i+2, 1, List.toChain(true :: true :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // scanRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def scanRight01(): Unit \ Assert = assertEq(expected = Chain.singleton(1), Chain.scanRight((b, i) -> if (b) i+1 else i+2, 1, Chain.empty()))

    @Test
    def scanRight02(): Unit \ Assert = assertEq(expected = List.toChain(3 :: 1 :: Nil), Chain.scanRight((b, i) -> if (b) i+1 else i+2, 1, Chain.singleton(false)))

    @Test
    def scanRight03(): Unit \ Assert = assertEq(expected = List.toChain(2 :: 1 :: Nil), Chain.scanRight((b, i) -> if (b) i+1 else i+2, 1, Chain.singleton(true)))

    @Test
    def scanRight04(): Unit \ Assert = assertEq(expected = List.toChain(5 :: 3 :: 1 :: Nil), Chain.scanRight((b, i) -> if (b) i+1 else i+2, 1, List.toChain(false :: false :: Nil)))

    @Test
    def scanRight05(): Unit \ Assert = assertEq(expected = List.toChain(4 :: 2 :: 1 :: Nil), Chain.scanRight((b, i) -> if (b) i+1 else i+2, 1, List.toChain(false :: true :: Nil)))

    @Test
    def scanRight06(): Unit \ Assert = assertEq(expected = List.toChain(4 :: 3 :: 1 :: Nil), Chain.scanRight((b, i) -> if (b) i+1 else i+2, 1, List.toChain(true :: false :: Nil)))

    @Test
    def scanRight07(): Unit \ Assert = assertEq(expected = List.toChain(3 :: 2 :: 1 :: Nil), Chain.scanRight((b, i) -> if (b) i+1 else i+2, 1, List.toChain(true :: true :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map01(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.map(i -> i > 2, Chain.empty()))

    @Test
    def map02(): Unit \ Assert = assertEq(expected = Chain.singleton(false), Chain.map(i -> i > 2, Chain.singleton(1)))

    @Test
    def map03(): Unit \ Assert = assertEq(expected = Chain.singleton(true), Chain.map(i -> i > 2, Chain.singleton(3)))

    @Test
    def map04(): Unit \ Assert = assertEq(expected = List.toChain(false :: false :: Nil), Chain.map(i -> i > 2, List.toChain(1 :: 2 :: Nil)))

    @Test
    def map05(): Unit \ Assert = assertEq(expected = List.toChain(false :: true :: Nil), Chain.map(i -> i > 2, List.toChain(1 :: 8 :: Nil)))

    @Test
    def map06(): Unit \ Assert = assertEq(expected = List.toChain(true :: false :: Nil), Chain.map(i -> i > 2, List.toChain(8 :: 1 :: Nil)))

    @Test
    def map07(): Unit \ Assert = assertEq(expected = List.toChain(true :: true :: Nil), Chain.map(i -> i > 2, List.toChain(7 :: 8 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapWithIndex01(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, Chain.empty()))

    @Test
    def mapWithIndex02(): Unit \ Assert = assertEq(expected = Chain.singleton(false), Chain.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, Chain.singleton(1)))

    @Test
    def mapWithIndex03(): Unit \ Assert = assertEq(expected = Chain.singleton(true), Chain.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, Chain.singleton(3)))

    @Test
    def mapWithIndex04(): Unit \ Assert = assertEq(expected = List.toChain(false :: true :: Nil), Chain.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, List.toChain(1 :: 2 :: Nil)))

    @Test
    def mapWithIndex05(): Unit \ Assert = assertEq(expected = List.toChain(false :: false :: Nil), Chain.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, List.toChain(1 :: 8 :: Nil)))

    @Test
    def mapWithIndex06(): Unit \ Assert = assertEq(expected = List.toChain(true :: true :: Nil), Chain.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, List.toChain(8 :: 1 :: Nil)))

    @Test
    def mapWithIndex07(): Unit \ Assert = assertEq(expected = List.toChain(true :: false :: Nil), Chain.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, List.toChain(7 :: 8 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // flatMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatMap01(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.flatMap(i -> Chain.repeat(i, i), Chain.empty()))

    @Test
    def flatMap02(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.flatMap(i -> Chain.repeat(i, i), Chain.singleton(0)))

    @Test
    def flatMap03(): Unit \ Assert = assertEq(expected = Chain.singleton(1), Chain.flatMap(i -> Chain.repeat(i, i), Chain.singleton(1)))

    @Test
    def flatMap04(): Unit \ Assert = assertEq(expected = List.toChain(2 :: 2 :: Nil), Chain.flatMap(i -> Chain.repeat(i, i), Chain.singleton(2)))

    @Test
    def flatMap05(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 2 :: 2 :: Nil), Chain.flatMap(i -> Chain.repeat(i, i), List.toChain(1 :: 2 :: Nil)))

    @Test
    def flatMap06(): Unit \ Assert = assertEq(expected = List.toChain(2 :: 2 :: 3 :: 3 :: 3 :: Nil), Chain.flatMap(i -> Chain.repeat(i, i), List.toChain(2 :: 3 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reverse01(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.reverse((Chain.empty(): Chain[Unit])))

    @Test
    def reverse02(): Unit \ Assert = assertEq(expected = Chain.singleton(1), Chain.reverse(Chain.singleton(1)))

    @Test
    def reverse03(): Unit \ Assert = assertEq(expected = List.toChain(2 :: 1 :: Nil), Chain.reverse(List.toChain(1 :: 2 :: Nil)))

    @Test
    def reverse04(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 1 :: Nil), Chain.reverse(List.toChain(1 :: 1 :: Nil)))

    @Test
    def reverse05(): Unit \ Assert = assertEq(expected = List.toChain(3 :: 2 :: 1 :: Nil), Chain.reverse(List.toChain(1 :: 2 :: 3 :: Nil)))

    @Test
    def reverse06(): Unit \ Assert = assertEq(expected = List.toChain(4 :: 3 :: 2 :: 1 :: Nil), Chain.reverse(List.toChain(1 :: 2 :: 3 :: 4 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // intersperse                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def intersperse01(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.intersperse(11, Chain.empty()))

    @Test
    def intersperse02(): Unit \ Assert = assertEq(expected = Chain.singleton(1), Chain.intersperse(11, Chain.singleton(1)))

    @Test
    def intersperse03(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 11 :: 2 :: Nil), Chain.intersperse(11, List.toChain(1 :: 2 :: Nil)))

    @Test
    def intersperse04(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 11 :: 2 :: 11 :: 3 :: Nil), Chain.intersperse(11, List.toChain(1 :: 2 :: 3 :: Nil)))

    @Test
    def intersperse05(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 11 :: 2 :: 11 :: 3 :: 11 :: 4 :: Nil), Chain.intersperse(11, List.toChain(1 :: 2 :: 3 :: 4 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def count01(): Unit \ Assert = assertEq(expected = 0, Chain.count(i -> i > 3, Chain.empty()))

    @Test
    def count02(): Unit \ Assert = assertEq(expected = 0, Chain.count(i -> i > 3, Chain.singleton(1)))

    @Test
    def count03(): Unit \ Assert = assertEq(expected = 1, Chain.count(i -> i > 3, Chain.singleton(4)))

    @Test
    def count04(): Unit \ Assert = assertEq(expected = 0, Chain.count(i -> i > 3, List.toChain(1 :: 2 :: Nil)))

    @Test
    def count05(): Unit \ Assert = assertEq(expected = 1, Chain.count(i -> i > 3, List.toChain(1 :: 8 :: Nil)))

    @Test
    def count06(): Unit \ Assert = assertEq(expected = 1, Chain.count(i -> i > 3, List.toChain(8 :: 1 :: Nil)))

    @Test
    def count07(): Unit \ Assert = assertEq(expected = 2, Chain.count(i -> i > 3, List.toChain(6 :: 7 :: Nil)))


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sum01(): Unit \ Assert =
        assertEq(expected = 0, List.toChain(Nil) |> Chain.sum)

    @Test
    def sum02(): Unit \ Assert =
        assertEq(expected = 1, List.toChain(1 :: Nil) |> Chain.sum)

    @Test
    def sum03(): Unit \ Assert =
        assertEq(expected = 6, List.toChain(1 :: 2 :: 3 :: Nil) |> Chain.sum)

    @Test
    def sum04(): Unit \ Assert =
        assertEq(expected = 3, List.toChain(1 :: 2 :: 3 :: -3 :: Nil) |> Chain.sum)

    @Test
    def sum05(): Unit \ Assert =
        assertEq(expected = -10, List.toChain(-1 :: -2 :: -3 :: -4 :: Nil) |> Chain.sum)

    @Test
    def sum06(): Unit \ Assert =
        assertEq(expected = 0, List.toChain(10 :: -10 :: Nil) |> Chain.sum)

    @Test
    def sum07(): Unit \ Assert =
        assertEq(expected = 5050, List.range(1, 101) |> List.toChain |> Chain.sum)


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumWith01(): Unit \ Assert =
        assertEq(expected = 0, List.toChain(Nil) |> Chain.sumWith(x -> x + 1))

    @Test
    def sumWith02(): Unit \ Assert =
        assertEq(expected = 2, List.toChain(1 :: Nil) |> Chain.sumWith(x -> x + 1))

    @Test
    def sumWith03(): Unit \ Assert =
        assertEq(expected = 9, List.toChain(1 :: 2 :: 3 :: Nil) |> Chain.sumWith(x -> x + 1))

    @Test
    def sumWith04(): Unit \ Assert =
        assertEq(expected = 7, List.toChain(1 :: 2 :: 3 :: -3 :: Nil) |> Chain.sumWith(x -> x + 1))

    @Test
    def sumWith05(): Unit \ Assert =
        assertEq(expected = -6, List.toChain(-1 :: -2 :: -3 :: -4 :: Nil) |> Chain.sumWith(x -> x + 1))

    @Test
    def sumWith06(): Unit \ Assert =
        assertEq(expected = 2, List.toChain(10 :: -10 :: Nil) |> Chain.sumWith(x -> x + 1))


    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatten01(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.flatten((Chain.empty(): Chain[Chain[Unit]])))

    @Test
    def flatten02(): Unit \ Assert = assertEq(expected = (Chain.empty(): Chain[Unit]), (Chain.flatten(Chain.singleton(Chain.empty())): Chain[Unit]))

    @Test
    def flatten03(): Unit \ Assert = assertEq(expected = Chain.singleton(1), Chain.flatten(Chain.singleton(Chain.singleton(1))))

    @Test
    def flatten04(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 2 :: Nil), Chain.flatten(Chain.singleton(List.toChain(1 :: 2 :: Nil))))

    @Test
    def flatten05(): Unit \ Assert = assertEq(expected = (Chain.empty(): Chain[Unit]), (Chain.flatten(List.toChain(Chain.empty() :: Chain.empty() :: Nil)): Chain[Unit]))

    @Test
    def flatten06(): Unit \ Assert = assertEq(expected = Chain.singleton(1), Chain.flatten(List.toChain(Chain.singleton(1) :: Chain.empty() :: Nil)))

    @Test
    def flatten07(): Unit \ Assert = assertEq(expected = Chain.singleton(1), Chain.flatten(List.toChain(Chain.empty() :: Chain.singleton(1) :: Nil)))

    @Test
    def flatten08(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 2 :: Nil), Chain.flatten(List.toChain(Chain.singleton(1) :: Chain.singleton(2) :: Nil)))

    @Test
    def flatten09(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: Nil), Chain.flatten(List.toChain(List.toChain(1 :: 2 :: Nil) :: List.toChain(3 :: 4 :: 5 :: Nil) :: Nil)))

    @Test
    def flatten10(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 2 :: 3 :: 4 :: Nil), Chain.flatten(List.toChain(Chain.singleton(1) :: List.toChain(2 :: 3 :: Nil) :: Chain.singleton(4) :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def exists01(): Unit \ Assert = assertFalse(Chain.exists(i -> i > 3, Chain.empty()))

    @Test
    def exists02(): Unit \ Assert = assertFalse(Chain.exists(i -> i > 3, Chain.singleton(1)))

    @Test
    def exists03(): Unit \ Assert = assertTrue(Chain.exists(i -> i > 3, Chain.singleton(5)))

    @Test
    def exists04(): Unit \ Assert = assertFalse(Chain.exists(i -> i > 3, List.toChain(1 :: 2 :: Nil)))

    @Test
    def exists05(): Unit \ Assert = assertTrue(Chain.exists(i -> i > 3, List.toChain(1 :: 6 :: Nil)))

    @Test
    def exists06(): Unit \ Assert = assertTrue(Chain.exists(i -> i > 3, List.toChain(6 :: 1 :: Nil)))

    @Test
    def exists07(): Unit \ Assert = assertTrue(Chain.exists(i -> i > 3, List.toChain(16 :: 6 :: Nil)))

    @Test
    def exists08(): Unit \ Assert = assertFalse(Chain.exists(i -> i > 3, List.toChain(1 :: -9 :: 3 :: Nil)))

    @Test
    def exists09(): Unit \ Assert = assertTrue(Chain.exists(i -> i > 3, List.toChain(1 :: 9 :: 3 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forAll01(): Unit \ Assert = assertTrue(Chain.forAll(i -> i > 3, Chain.empty()))

    @Test
    def forAll02(): Unit \ Assert = assertFalse(Chain.forAll(i -> i > 3, Chain.singleton(1)))

    @Test
    def forAll03(): Unit \ Assert = assertTrue(Chain.forAll(i -> i > 3, Chain.singleton(5)))

    @Test
    def forAll04(): Unit \ Assert = assertFalse(Chain.forAll(i -> i > 3, List.toChain(1 :: 2 :: Nil)))

    @Test
    def forAll05(): Unit \ Assert = assertFalse(Chain.forAll(i -> i > 3, List.toChain(1 :: 6 :: Nil)))

    @Test
    def forAll06(): Unit \ Assert = assertFalse(Chain.forAll(i -> i > 3, List.toChain(6 :: 1 :: Nil)))

    @Test
    def forAll07(): Unit \ Assert = assertTrue(Chain.forAll(i -> i > 3, List.toChain(16 :: 6 :: Nil)))

    @Test
    def forAll08(): Unit \ Assert = assertFalse(Chain.forAll(i -> i > 3, List.toChain(1 :: -9 :: 3 :: Nil)))

    @Test
    def forAll09(): Unit \ Assert = assertFalse(Chain.forAll(i -> i > 3, List.toChain(1 :: 9 :: 3 :: Nil)))

    @Test
    def forAll10(): Unit \ Assert = assertTrue(Chain.forAll(i -> i > 3, List.toChain(11 :: 9 :: 31 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filter01(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.filter(i -> i > 3, Chain.empty()))

    @Test
    def filter02(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.filter(i -> i > 3, Chain.singleton(2)))

    @Test
    def filter03(): Unit \ Assert = assertEq(expected = Chain.singleton(4), Chain.filter(i -> i > 3, Chain.singleton(4)))

    @Test
    def filter04(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.filter(i -> i > 3, List.toChain(1 :: 3 :: Nil)))

    @Test
    def filter05(): Unit \ Assert = assertEq(expected = Chain.singleton(8), Chain.filter(i -> i > 3, List.toChain(1 :: 8 :: Nil)))

    @Test
    def filter06(): Unit \ Assert = assertEq(expected = Chain.singleton(8), Chain.filter(i -> i > 3, List.toChain(8 :: 1 :: Nil)))

    @Test
    def filter07(): Unit \ Assert = assertEq(expected = List.toChain(8 :: 9 :: Nil), Chain.filter(i -> i > 3, List.toChain(8 :: 9 :: Nil)))

    @Test
    def filter08(): Unit \ Assert = assertEq(expected = List.toChain(4 :: 11 :: 17 :: Nil), Chain.filter(i -> i > 3, List.toChain(1 :: 4 :: 11 :: 2 :: -22 :: 17 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeft01(): Unit \ Assert = assertEq(expected = 100, Chain.foldLeft((i, e) -> (i - e)*(e `Int32.remainder` 2 + 1), 100, Chain.empty()))

    @Test
    def foldLeft02(): Unit \ Assert = assertEq(expected = 198, Chain.foldLeft((i, e) -> (i - e)*(e `Int32.remainder` 2 + 1), 100, Chain.singleton(1)))

    @Test
    def foldLeft03(): Unit \ Assert = assertEq(expected = 196, Chain.foldLeft((i, e) -> (i - e)*(e `Int32.remainder` 2 + 1), 100, List.toChain(1 :: 2 :: Nil)))

    @Test
    def foldLeft04(): Unit \ Assert = assertEq(expected = 386, Chain.foldLeft((i, e) -> (i - e)*(e `Int32.remainder` 2 + 1), 100, List.toChain(1 :: 2 :: 3 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRight01(): Unit \ Assert = assertEq(expected = 100, Chain.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, Chain.empty()))

    @Test
    def foldRight02(): Unit \ Assert = assertEq(expected = 198, Chain.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, Chain.singleton(1)))

    @Test
    def foldRight03(): Unit \ Assert = assertEq(expected = 194, Chain.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, List.toChain(1 :: 2 :: Nil)))

    @Test
    def foldRight04(): Unit \ Assert = assertEq(expected = 382, Chain.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, List.toChain(1 :: 2 :: 3 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRightWithCont01(): Unit \ Assert = assertEq(expected = 100, Chain.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, Chain.empty()))

    @Test
    def foldRightWithCont02(): Unit \ Assert = assertEq(expected = 198, Chain.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, Chain.singleton(1)))

    @Test
    def foldRightWithCont03(): Unit \ Assert = assertEq(expected = 194, Chain.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, List.toChain(1 :: 2 :: Nil)))

    @Test
    def foldRightWithCont04(): Unit \ Assert = assertEq(expected = 382, Chain.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, List.toChain(1 :: 2 :: 3 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldMap01(): Unit \ Assert =
        assertEq(expected = 0, Chain.foldMap(x -> 2 * x, List.toChain(Nil)))

    @Test
    def foldMap02(): Unit \ Assert =
        assertEq(expected = 6, Chain.foldMap(x -> 2 * x, List.toChain(1 :: 2 :: Nil)))

    @Test
    def foldMap03(): Unit \ Assert =
        assertEq(expected = "b", Chain.foldMap(x -> if(x == "a") "b" else x, List.toChain("a" :: Nil)))

    @Test
    def foldMap04(): Unit \ Assert =
        assertEq(expected = "abb", Chain.foldMap(x -> if(x == "c") "b" else x, List.toChain("a" :: "b" :: "c" :: Nil)))

    @Test
    def foldMap05(): Unit \ Assert =
        assertEq(expected = "123", Chain.foldMap(Int32.toString, List.toChain(1 :: 2 :: 3 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // dropLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropLeft01(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.dropLeft(-1, (Chain.empty(): Chain[Unit])))

    @Test
    def dropLeft02(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.dropLeft(0, (Chain.empty(): Chain[Unit])))

    @Test
    def dropLeft03(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.dropLeft(1, (Chain.empty(): Chain[Unit])))

    @Test
    def dropLeft04(): Unit \ Assert = assertEq(expected = Chain.singleton(1), Chain.dropLeft(-1, Chain.singleton(1)))

    @Test
    def dropLeft05(): Unit \ Assert = assertEq(expected = Chain.singleton(1), Chain.dropLeft(0, Chain.singleton(1)))

    @Test
    def dropLeft06(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.dropLeft(1, Chain.singleton(1)))

    @Test
    def dropLeft07(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.dropLeft(2, Chain.singleton(1)))

    @Test
    def dropLeft08(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 2 :: Nil), Chain.dropLeft(0, List.toChain(1 :: 2 :: Nil)))

    @Test
    def dropLeft09(): Unit \ Assert = assertEq(expected = Chain.singleton(2), Chain.dropLeft(1, List.toChain(1 :: 2 :: Nil)))

    @Test
    def dropLeft10(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.dropLeft(2, List.toChain(1 :: 2 :: Nil)))

    @Test
    def dropLeft11(): Unit \ Assert = assertEq(expected = List.toChain(3 :: 4 :: 5 :: 6 :: Nil), Chain.dropLeft(2, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)))

    @Test
    def dropLeft12(): Unit \ Assert = assertEq(expected = List.toChain(5 :: 6 :: Nil), Chain.dropLeft(4, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // dropRight                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropRight01(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.dropRight(-1, (Chain.empty(): Chain[Unit])))

    @Test
    def dropRight02(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.dropRight(0, (Chain.empty(): Chain[Unit])))

    @Test
    def dropRight03(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.dropRight(1, (Chain.empty(): Chain[Unit])))

    @Test
    def dropRight04(): Unit \ Assert = assertEq(expected = Chain.singleton(1), Chain.dropRight(-1, Chain.singleton(1)))

    @Test
    def dropRight05(): Unit \ Assert = assertEq(expected = Chain.singleton(1), Chain.dropRight(0, Chain.singleton(1)))

    @Test
    def dropRight06(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.dropRight(1, Chain.singleton(1)))

    @Test
    def dropRight07(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.dropRight(2, Chain.singleton(1)))

    @Test
    def dropRight08(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 2 :: Nil), Chain.dropRight(0, List.toChain(1 :: 2 :: Nil)))

    @Test
    def dropRight09(): Unit \ Assert = assertEq(expected = Chain.singleton(1), Chain.dropRight(1, List.toChain(1 :: 2 :: Nil)))

    @Test
    def dropRight10(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.dropRight(2, List.toChain(1 :: 2 :: Nil)))

    @Test
    def dropRight11(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 2 :: 3 :: 4 :: Nil), Chain.dropRight(2, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)))

    @Test
    def dropRight12(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 2 :: Nil), Chain.dropRight(4, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // dropWhileLeft                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropWhileLeft01(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.dropWhileLeft(i -> i > 3, Chain.empty()))

    @Test
    def dropWhileLeft02(): Unit \ Assert = assertEq(expected = Chain.singleton(1), Chain.dropWhileLeft(i -> i > 3, Chain.singleton(1)))

    @Test
    def dropWhileLeft03(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.dropWhileLeft(i -> i > 3, Chain.singleton(4)))

    @Test
    def dropWhileLeft04(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 2 :: Nil), Chain.dropWhileLeft(i -> i > 3, List.toChain(1 :: 2 :: Nil)))

    @Test
    def dropWhileLeft05(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 5 :: Nil), Chain.dropWhileLeft(i -> i > 3, List.toChain(1 :: 5 :: Nil)))

    @Test
    def dropWhileLeft06(): Unit \ Assert = assertEq(expected = Chain.singleton(1), Chain.dropWhileLeft(i -> i > 3, List.toChain(5 :: 1 :: Nil)))

    @Test
    def dropWhileLeft07(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.dropWhileLeft(i -> i > 3, List.toChain(5 :: 8 :: Nil)))

    @Test
    def dropWhileLeft08(): Unit \ Assert = assertEq(expected = List.toChain(-3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil), Chain.dropWhileLeft(i -> i > 3, List.toChain(4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // dropWhileRight                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropWhileRight01(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.dropWhileRight(i -> i > 3, Chain.empty()))

    @Test
    def dropWhileRight02(): Unit \ Assert = assertEq(expected = Chain.singleton(1), Chain.dropWhileRight(i -> i > 3, Chain.singleton(1)))

    @Test
    def dropWhileRight03(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.dropWhileRight(i -> i > 3, Chain.singleton(4)))

    @Test
    def dropWhileRight04(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 2 :: Nil), Chain.dropWhileRight(i -> i > 3, List.toChain(1 :: 2 :: Nil)))

    @Test
    def dropWhileRight05(): Unit \ Assert = assertEq(expected = Chain.singleton(1), Chain.dropWhileRight(i -> i > 3, List.toChain(1 :: 5 :: Nil)))

    @Test
    def dropWhileRight06(): Unit \ Assert = assertEq(expected = List.toChain(5 :: 1 :: Nil), Chain.dropWhileRight(i -> i > 3, List.toChain(5 :: 1 :: Nil)))

    @Test
    def dropWhileRight07(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.dropWhileRight(i -> i > 3, List.toChain(5 :: 8 :: Nil)))

    @Test
    def dropWhileRight08(): Unit \ Assert = assertEq(expected = List.toChain(4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: Nil), Chain.dropWhileRight(i -> i > 3, List.toChain(4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // takeLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeLeft01(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.takeLeft(-1, (Chain.empty(): Chain[Unit])))

    @Test
    def takeLeft02(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.takeLeft(0, (Chain.empty(): Chain[Unit])))

    @Test
    def takeLeft03(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.takeLeft(1, (Chain.empty(): Chain[Unit])))

    @Test
    def takeLeft04(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.takeLeft(-1, Chain.singleton(1)))

    @Test
    def takeLeft05(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.takeLeft(0, Chain.singleton(1)))

    @Test
    def takeLeft06(): Unit \ Assert = assertEq(expected = Chain.singleton(1), Chain.takeLeft(1, Chain.singleton(1)))

    @Test
    def takeLeft07(): Unit \ Assert = assertEq(expected = Chain.singleton(1), Chain.takeLeft(2, Chain.singleton(1)))

    @Test
    def takeLeft08(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.takeLeft(0, List.toChain(1 :: 2 :: Nil)))

    @Test
    def takeLeft09(): Unit \ Assert = assertEq(expected = Chain.singleton(1), Chain.takeLeft(1, List.toChain(1 :: 2 :: Nil)))

    @Test
    def takeLeft10(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 2 :: Nil), Chain.takeLeft(2, List.toChain(1 :: 2 :: Nil)))

    @Test
    def takeLeft11(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 2 :: Nil), Chain.takeLeft(2, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)))

    @Test
    def takeLeft12(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 2 :: 3 :: 4 :: Nil), Chain.takeLeft(4, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)))


    /////////////////////////////////////////////////////////////////////////////
    // takeRight                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeRight01(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.takeRight(-1, (Chain.empty(): Chain[Unit])))

    @Test
    def takeRight02(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.takeRight(0, (Chain.empty(): Chain[Unit])))

    @Test
    def takeRight03(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.takeRight(1, (Chain.empty(): Chain[Unit])))

    @Test
    def takeRight04(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.takeRight(-1, Chain.singleton(1)))

    @Test
    def takeRight05(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.takeRight(0, Chain.singleton(1)))

    @Test
    def takeRight06(): Unit \ Assert = assertEq(expected = Chain.singleton(1), Chain.takeRight(1, Chain.singleton(1)))

    @Test
    def takeRight07(): Unit \ Assert = assertEq(expected = Chain.singleton(1), Chain.takeRight(2, Chain.singleton(1)))

    @Test
    def takeRight08(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.takeRight(0, List.toChain(1 :: 2 :: Nil)))

    @Test
    def takeRight09(): Unit \ Assert = assertEq(expected = Chain.singleton(2), Chain.takeRight(1, List.toChain(1 :: 2 :: Nil)))

    @Test
    def takeRight10(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 2 :: Nil), Chain.takeRight(2, List.toChain(1 :: 2 :: Nil)))

    @Test
    def takeRight11(): Unit \ Assert = assertEq(expected = List.toChain(5 :: 6 :: Nil), Chain.takeRight(2, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)))

    @Test
    def takeRight12(): Unit \ Assert = assertEq(expected = List.toChain(3 :: 4 :: 5 :: 6 :: Nil), Chain.takeRight(4, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // takeWhileLeft                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeWhileLeft01(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.takeWhileLeft(i -> i > 3, (Chain.empty(): Chain[Int32])))

    @Test
    def takeWhileLeft02(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.takeWhileLeft(i -> i > 3, Chain.singleton(1)))

    @Test
    def takeWhileLeft03(): Unit \ Assert = assertEq(expected = Chain.singleton(4), Chain.takeWhileLeft(i -> i > 3, Chain.singleton(4)))

    @Test
    def takeWhileLeft04(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.takeWhileLeft(i -> i > 3, List.toChain(1 :: 2 :: Nil)))

    @Test
    def takeWhileLeft05(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.takeWhileLeft(i -> i > 3, List.toChain(1 :: 5 :: Nil)))

    @Test
    def takeWhileLeft06(): Unit \ Assert = assertEq(expected = Chain.singleton(5), Chain.takeWhileLeft(i -> i > 3, List.toChain(5 :: 1 :: Nil)))

    @Test
    def takeWhileLeft07(): Unit \ Assert = assertEq(expected = List.toChain(5 :: 8 :: Nil), Chain.takeWhileLeft(i -> i > 3, List.toChain(5 :: 8 :: Nil)))

    @Test
    def takeWhileLeft08(): Unit \ Assert = assertEq(expected = List.toChain(4 :: 6 :: Nil), Chain.takeWhileLeft(i -> i > 3, List.toChain(4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // takeWhileRight                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeWhileRight01(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.takeWhileRight(i -> i > 3, (Chain.empty(): Chain[Int32])))

    @Test
    def takeWhileRight02(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.takeWhileRight(i -> i > 3, Chain.singleton(1)))

    @Test
    def takeWhileRight03(): Unit \ Assert = assertEq(expected = Chain.singleton(4), Chain.takeWhileRight(i -> i > 3, Chain.singleton(4)))

    @Test
    def takeWhileRight04(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.takeWhileRight(i -> i > 3, List.toChain(1 :: 2 :: Nil)))

    @Test
    def takeWhileRight05(): Unit \ Assert = assertEq(expected = Chain.singleton(5), Chain.takeWhileRight(i -> i > 3, List.toChain(1 :: 5 :: Nil)))

    @Test
    def takeWhileRight06(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.takeWhileRight(i -> i > 3, List.toChain(5 :: 1 :: Nil)))

    @Test
    def takeWhileRight07(): Unit \ Assert = assertEq(expected = List.toChain(5 :: 8 :: Nil), Chain.takeWhileRight(i -> i > 3, List.toChain(5 :: 8 :: Nil)))

    @Test
    def takeWhileRight08(): Unit \ Assert = assertEq(expected = Chain.singleton(7), Chain.takeWhileRight(i -> i > 3, List.toChain(4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // filterMap                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterMap01(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Chain.empty()))

    @Test
    def filterMap02(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Chain.singleton(1)))

    @Test
    def filterMap03(): Unit \ Assert = assertEq(expected = Chain.singleton(1), Chain.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Chain.singleton(2)))

    @Test
    def filterMap04(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, List.toChain(1 :: 3 :: Nil)))

    @Test
    def filterMap05(): Unit \ Assert = assertEq(expected = Chain.singleton(2), Chain.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, List.toChain(1 :: 4 :: Nil)))

    @Test
    def filterMap06(): Unit \ Assert = assertEq(expected = Chain.singleton(3), Chain.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, List.toChain(6 :: -1 :: Nil)))

    @Test
    def filterMap07(): Unit \ Assert = assertEq(expected = List.toChain(4 :: 3 :: Nil), Chain.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, List.toChain(8 :: 6 :: Nil)))

    @Test
    def filterMap08(): Unit \ Assert = assertEq(expected = List.toChain(0 :: 1 :: 2 :: 5 :: Nil), Chain.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 10 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // findMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findMap01(): Unit \ Assert = assertEq(expected = None, Chain.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Chain.empty()))

    @Test
    def findMap02(): Unit \ Assert = assertEq(expected = None, Chain.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Chain.singleton(1)))

    @Test
    def findMap03(): Unit \ Assert = assertEq(expected = Some(1), Chain.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Chain.singleton(2)))

    @Test
    def findMap04(): Unit \ Assert = assertEq(expected = None, Chain.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, List.toChain(1 :: 3 :: Nil)))

    @Test
    def findMap05(): Unit \ Assert = assertEq(expected = Some(2), Chain.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, List.toChain(1 :: 4 :: Nil)))

    @Test
    def findMap06(): Unit \ Assert = assertEq(expected = Some(3), Chain.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, List.toChain(6 :: -1 :: Nil)))

    @Test
    def findMap07(): Unit \ Assert = assertEq(expected = Some(4), Chain.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, List.toChain(8 :: 6 :: Nil)))

    @Test
    def findMap08(): Unit \ Assert = assertEq(expected = Some(0), Chain.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 10 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zip01(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.zip((Chain.empty(): Chain[Unit]), (Chain.empty(): Chain[Unit])))

    @Test
    def zip02(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.zip(Chain.singleton(1), (Chain.empty(): Chain[Unit])))

    @Test
    def zip03(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.zip((Chain.empty(): Chain[Unit]), Chain.singleton(2)))

    @Test
    def zip04(): Unit \ Assert = assertEq(expected = Chain.singleton((1, 2)), Chain.zip(Chain.singleton(1), Chain.singleton(2)))

    @Test
    def zip05(): Unit \ Assert = assertEq(expected = List.toChain((1, 2) :: (3, 4) :: Nil), Chain.zip(List.toChain(1 :: 3 :: Nil), List.toChain(2 :: 4 :: Nil)))

    @Test
    def zip06(): Unit \ Assert = assertEq(expected = List.toChain((1, 2) :: (3, 4) :: (5, 6) :: Nil), Chain.zip(List.toChain(1 :: 3 :: 5 :: Nil), List.toChain(2 :: 4 :: 6 :: Nil)))

    @Test
    def zip07(): Unit \ Assert = assertEq(expected = List.toChain((1, 2) :: (3, 4) :: (5, 6) :: (7, 8) :: Nil), Chain.zip(List.toChain(1 :: 3 :: 5 :: 7 :: Nil), List.toChain(2 :: 4 :: 6 :: 8 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // zipWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWith01(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.zipWith((a, b) -> if (b) a+1 else a, Chain.empty(), Chain.empty()))

    @Test
    def zipWith02(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.zipWith((a, b) -> if (b) a+1 else a, Chain.singleton(1), Chain.empty()))

    @Test
    def zipWith03(): Unit \ Assert = assertEq(expected = Chain.empty(), Chain.zipWith((a, b) -> if (b) a+1 else a, Chain.empty(), Chain.singleton(true)))

    @Test
    def zipWith04(): Unit \ Assert = assertEq(expected = Chain.singleton(2), Chain.zipWith((a, b) -> if (b) a+1 else a, Chain.singleton(1), Chain.singleton(true)))

    @Test
    def zipWith05(): Unit \ Assert = assertEq(expected = Chain.singleton(1), Chain.zipWith((a, b) -> if (b) a+1 else a, Chain.singleton(1), Chain.singleton(false)))

    @Test
    def zipWith06(): Unit \ Assert = assertEq(expected = List.toChain(1 :: 3 :: 4 :: 4 :: 5 :: 7 :: 8 :: 9 :: Nil), Chain.zipWith((a, b) -> if (b) a+1 else a, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil), List.toChain(false :: true :: true :: false :: false :: true :: true :: true :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // zipWithA                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithA01(): Unit \ Assert = assertEq(expected = Identity.Identity(Chain.empty()), Chain.zipWithA((a, b) -> {if (b) a+1 else a} |> Identity.Identity, Chain.empty(), Chain.empty()))

    @Test
    def zipWithA02(): Unit \ Assert = assertEq(expected = Identity.Identity(Chain.empty()), Chain.zipWithA((a, b) -> {if (b) a+1 else a} |> Identity.Identity, Chain.singleton(1), Chain.empty()))

    @Test
    def zipWithA03(): Unit \ Assert = assertEq(expected = Identity.Identity(Chain.empty()), Chain.zipWithA((a, b) -> {if (b) a+1 else a} |> Identity.Identity, Chain.empty(), Chain.singleton(true)))

    @Test
    def zipWithA04(): Unit \ Assert = assertEq(expected = Identity.Identity(Chain.singleton(2)), Chain.zipWithA((a, b) -> {if (b) a+1 else a} |> Identity.Identity, Chain.singleton(1), Chain.singleton(true)))

    @Test
    def zipWithA05(): Unit \ Assert = assertEq(expected = Identity.Identity(Chain.singleton(1)), Chain.zipWithA((a, b) -> {if (b) a+1 else a} |> Identity.Identity, Chain.singleton(1), Chain.singleton(false)))

    @Test
    def zipWith06A(): Unit \ Assert = assertEq(expected = Identity.Identity(List.toChain(1 :: 3 :: 4 :: 4 :: 5 :: 7 :: 8 :: 9 :: Nil)), Chain.zipWithA((a, b) -> {if (b) a+1 else a} |> Identity.Identity, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil), List.toChain(false :: true :: true :: false :: false :: true :: true :: true :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // unzip                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unzip01(): Unit \ Assert = assertEq(expected = (Chain.empty(), Chain.empty()), Chain.unzip((Chain.empty(): Chain[(Unit, Unit)])))

    @Test
    def unzip02(): Unit \ Assert = assertEq(expected = (Chain.singleton(1), Chain.singleton(true)), Chain.unzip(Chain.singleton((1, true))))

    @Test
    def unzip03(): Unit \ Assert = assertEq(expected = (List.toChain(1 :: 2 :: Nil), List.toChain(true :: true :: Nil)), Chain.unzip(List.toChain((1, true) :: (2, true) :: Nil)))

    @Test
    def unzip04(): Unit \ Assert = assertEq(expected = (List.toChain(1 :: 2 :: 3 :: Nil), List.toChain(true :: true :: false :: Nil)), Chain.unzip(List.toChain((1, true) :: (2, true) :: (3, false) :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // mapAccumLeft                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapAccumLeft01(): Unit \ Assert = assertEq(expected = (0, Chain.empty()), Chain.mapAccumLeft((s, x) -> (s+1, x+s), 0, Chain.empty()))

    @Test
    def mapAccumLeft02(): Unit \ Assert = assertEq(expected = (1, Chain.singleton(1)), Chain.mapAccumLeft((s, x) -> (s+1, x+s), 0, Chain.singleton(1)))

    @Test
    def mapAccumLeft03(): Unit \ Assert = assertEq(expected = (2, List.toChain(1 :: 2 :: Nil)), Chain.mapAccumLeft((s, x) -> (s+1, x+s), 0, List.toChain(1 :: 1 :: Nil)))

    @Test
    def mapAccumLeft04(): Unit \ Assert = assertEq(expected = (3, List.toChain(1 :: 2 :: 3 :: Nil)), Chain.mapAccumLeft((s, x) -> (s+1, x+s), 0, List.toChain(1 :: 1 :: 1 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // mapAccumRight                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapAccumRight01(): Unit \ Assert = assertEq(expected = (0, Chain.empty()), Chain.mapAccumRight((s, x) -> (s+1, x+s), 0, Chain.empty()))

    @Test
    def mapAccumRight02(): Unit \ Assert = assertEq(expected = (1, Chain.singleton(1)), Chain.mapAccumRight((s, x) -> (s+1, x+s), 0, Chain.singleton(1)))

    @Test
    def mapAccumRight03(): Unit \ Assert = assertEq(expected = (2, List.toChain(2 :: 1 :: Nil)), Chain.mapAccumRight((s, x) -> (s+1, x+s), 0, List.toChain(1 :: 1 :: Nil)))

    @Test
    def mapAccumRight04(): Unit \ Assert = assertEq(expected = (3, List.toChain(3 :: 2 :: 1 :: Nil)), Chain.mapAccumRight((s, x) -> (s+1, x+s), 0, List.toChain(1 :: 1 :: 1 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Unit \ Assert = region rc {
        let ri = Ref.fresh(rc, 21);
        Chain.forEach(x -> Ref.put(x, ri), Chain.empty());
        assertEq(expected = 21, Ref.get(ri))
    }

    @Test
    def forEach02(): Unit \ Assert = region rc {
        let ri = Ref.fresh(rc, 21);
        Chain.forEach(x -> Ref.put(x, ri), List.toChain(1 :: 2 :: 3 :: Nil));
        assertEq(expected = 3, Ref.get(ri))
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEachWithIndex01(): Unit \ Assert = region rc {
        let ri = Ref.fresh(rc, 21);
        Chain.forEachWithIndex((i, _) -> Ref.put(i, ri), Chain.empty());
        assertEq(expected = 21, Ref.get(ri))
    }

    @Test
    def forEachWithIndex02(): Unit \ Assert = region rc {
        let ri = Ref.fresh(rc, 21);
        Chain.forEachWithIndex((i, _) -> Ref.put(i, ri), List.toChain(1 :: 2 :: 3 :: Nil));
        assertEq(expected = 2, Ref.get(ri))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toList01(): Unit \ Assert =
        assertEq(expected = (Nil: List[Int32]), Chain.toList((Chain.empty(): Chain[Int32])))

    @Test
    def toList02(): Unit \ Assert =
        assertEq(expected = 1 :: Nil, Chain.toList(Chain.singleton(1)))

    @Test
    def toList03(): Unit \ Assert =
        assertEq(expected = 1 :: 2 :: Nil, Chain.toList(List.toChain(1 :: 2 :: Nil)))

    @Test
    def toList04(): Unit \ Assert =
        assertEq(expected = 1 :: 2 :: 3 :: Nil, Chain.toList(List.toChain(1 :: 2 :: 3 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // toMutList                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMutList01(): Unit \ Assert = region rc {
        assertTrue(Chain.toMutList(rc, (Chain.empty(): Chain[Int32])) `MutList.sameElements` (List.toMutList(rc, Nil): MutList[Int32, rc]))
    }

    @Test
    def toMutList02(): Unit \ Assert = region rc {
        assertTrue(Chain.toMutList(rc, Chain.singleton(1)) `MutList.sameElements` List.toMutList(rc, 1 :: Nil))
    }

    @Test
    def toMutList03(): Unit \ Assert = region rc {
        assertTrue(Chain.toMutList(rc, List.toChain(1 :: 2 :: Nil)) `MutList.sameElements` List.toMutList(rc, 1 :: 2 :: Nil))
    }

    @Test
    def toMutList04(): Unit \ Assert = region rc {
        assertTrue(Chain.toMutList(rc, List.toChain(1 :: 2 :: 3 :: Nil)) `MutList.sameElements` List.toMutList(rc, 1 :: 2 :: 3 :: Nil))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMutDeque01(): Unit \ Assert = region rc {
        let c = (Chain.empty(): Chain[Int32]);
        let d1 = Chain.toMutDeque(rc, c);

        let d2 = MutDeque.empty(rc);

        assertTrue(d1 `MutDeque.sameElements` d2)
    }

    @Test
    def toMutDeque02(): Unit \ Assert = region rc {
        let c = Chain.singleton(1);
        let d1 = Chain.toMutDeque(rc, c);

        let d2 = MutDeque.empty(rc);
        MutDeque.pushBack(1, d2);

        assertTrue(d1 `MutDeque.sameElements` d2)
    }

    @Test
    def toMutDeque03(): Unit \ Assert = region rc {
        let c = List.toChain(1 :: 2 :: Nil);
        let d1 = Chain.toMutDeque(rc, c);

        let d2 = MutDeque.empty(rc);
        MutDeque.pushFront(1, d2);
        MutDeque.pushBack(2, d2);

        assertTrue(d1 `MutDeque.sameElements` d2)
    }

    @Test
    def toMutDeque04(): Unit \ Assert = region rc {
        let c = List.toChain('a' :: 'b' :: 'c' :: Nil);
        let d1 = Chain.toMutDeque(rc, c);

        let d2 = MutDeque.empty(rc);
        MutDeque.pushFront('b', d2);
        MutDeque.pushBack('c', d2);
        MutDeque.pushFront('a', d2);

        assertTrue(d1 `MutDeque.sameElements` d2)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toSet01(): Unit \ Assert =
        assertEq(expected = (Set#{}: Set[Int32]), Chain.toSet((Chain.empty(): Chain[Int32])))

    @Test
    def toSet02(): Unit \ Assert =
        assertEq(expected = Set#{1}, Chain.toSet(Chain.singleton(1)))

    @Test
    def toSet03(): Unit \ Assert =
        assertEq(expected = Set#{1, 2}, Chain.toSet(List.toChain(1 :: 2 :: Nil)))

    @Test
    def toSet04(): Unit \ Assert =
        assertEq(expected = Set#{1, 2, 3}, Chain.toSet(List.toChain(1 :: 2 :: 3 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMap01(): Unit \ Assert =
        assertEq(expected = (Map#{}: Map[Int32, Int32]), Chain.toMap((Chain.empty(): Chain[(Int32, Int32)])))

    @Test
    def toMap02(): Unit \ Assert =
        assertEq(expected = Map#{1 => 1}, Chain.toMap(Chain.singleton((1, 1))))

    @Test
    def toMap03(): Unit \ Assert =
        assertEq(expected = Map#{1 => 1, 2 => 2}, Chain.toMap(List.toChain((1, 1) :: (2, 2) :: Nil)))

    @Test
    def toMap04(): Unit \ Assert =
        assertEq(expected = Map#{1 => 1, 2 => 2, 3 => 3}, Chain.toMap(List.toChain((1, 1) :: (2, 2) :: (3, 3) :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toArray01(): Unit \ Assert = region rc {
        let c: Chain[Int32] = Chain.empty();
        assertTrue(Array.sameElements(Chain.toArray(rc, c), Array#{} @ rc))
    }

    @Test
    def toArray02(): Unit \ Assert = region rc {
        let a = Chain.toArray(rc, Chain.singleton(1));
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def toArray03(): Unit \ Assert = region rc {
        let a = Chain.toArray(rc, List.toChain(1 :: 2 :: Nil));
        assertTrue(Array.sameElements(a, Array#{1, 2} @ rc))
    }

    @Test
    def toArray04(): Unit \ Assert = region rc {
        let a = Chain.toArray(rc, List.toChain(1 :: 2 :: 3 :: Nil));
        assertTrue(Array.sameElements(a, Array#{1, 2, 3} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toVector01(): Unit \ Assert =
        let c: Chain[Int32] = Chain.empty();
        assertEq(expected = Vector#{}, Chain.toVector(c))

    @Test
    def toVector02(): Unit \ Assert =
        let c = Chain.singleton(1);
        assertEq(expected = Vector#{1}, Chain.toVector(c))

    @Test
    def toVector03(): Unit \ Assert =
        let c = List.toChain(1 :: 2 :: Nil);
        assertEq(expected = Vector#{1, 2}, Chain.toVector(c))

    @Test
    def toVector04(): Unit \ Assert =
        let c = List.toChain(1 :: 2 :: 3 :: Nil);
        assertEq(expected = Vector#{1, 2, 3}, Chain.toVector(c))

    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterator01(): Unit \ Assert = region rc {
        assertEq(expected = (Nil: List[Int32]), Chain.empty() |> Chain.iterator(rc) |> Iterator.toList)
    }

    @Test
    def iterator02(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: 2 :: 3 :: Nil, List.toChain(1 :: 2 :: 3 :: Nil) |> Chain.iterator(rc) |> Iterator.toList)
    }

    @Test
    def iterator03(): Unit \ Assert = region rc {
        assertEq(expected = List.range(-100, 100), Chain.range(-100, 100) |> Chain.iterator(rc) |> Iterator.toList)
    }


    /////////////////////////////////////////////////////////////////////////////
    // enumerator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def enumerator01(): Unit \ Assert = region rc {
        assertEq(expected = (Nil: List[(Int32, Int32)]), Chain.empty() |> Chain.enumerator(rc) |> Iterator.toList)
    }

    @Test
    def enumerator02(): Unit \ Assert = region rc {
        assertEq(expected = (0, 1) :: (1, 2) :: (2, 3) :: Nil, List.toChain(1 :: 2 :: 3 :: Nil) |> Chain.enumerator(rc) |> Iterator.toList)
    }

    @Test
    def enumerator03(): Unit \ Assert = region rc {
        assertEq(expected = List.range(-100, 100) |> List.zipWithIndex, Chain.range(-100, 100) |> Chain.enumerator(rc) |> Iterator.toList)
    }


    /////////////////////////////////////////////////////////////////////////////
    // toNel.Nel                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toNel01(): Unit \ Assert = assertEq(expected = None, Chain.toNel((Chain.empty(): Chain[Int32])))

    @Test
    def toNel02(): Unit \ Assert = assertEq(expected = Some(Nel.Nel(1, Nil)), Chain.toNel(Chain.singleton(1)))

    @Test
    def toNel03(): Unit \ Assert =
        assertEq(expected = Some(Nel.Nel(1, 2 :: Nil)), Chain.toNel(List.toChain(1 :: 2 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // toNec                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toNec01(): Unit \ Assert = assertEq(expected = None, Chain.toNec((Chain.empty(): Chain[Int32])))

    @Test
    def toNec02(): Unit \ Assert = assertEq(expected = Some(Nec.singleton(1)), Chain.toNec(Chain.singleton(1)))

    @Test
    def toNec03(): Unit \ Assert =
        assertEq(expected = Some(Nec.cons(1, Nec.singleton(2))), Chain.toNec(List.toChain(1 :: 2 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // equals                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def equals01(): Unit \ Assert =
        assertTrue((Chain.empty(): Chain[Int32]) `Chain.equals` (Chain.empty(): Chain[Int32]))

    @Test
    def equals02(): Unit \ Assert =
        assertTrue(Chain.singleton(1) `Chain.equals` Chain.singleton(1))

    @Test
    def equals03(): Unit \ Assert =
        assertTrue((Chain.empty() |> Chain.cons(1)) `Chain.equals` Chain.singleton(1))

    @Test
    def equals04(): Bool =
        (Chain.empty() `Chain.snoc` 1) `Chain.equals` Chain.singleton(1)

    @Test
    def equals05(): Bool =
        (Chain.empty() `Chain.snoc` 1) `Chain.equals` (Chain.empty() |> Chain.cons(1))

    /////////////////////////////////////////////////////////////////////////////
    // compare                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def compare01(): Bool =
        (Chain.empty(): Chain[Int32]) `Chain.compare` (Chain.empty(): Chain[Int32]) == Comparison.EqualTo

    @Test
    def compare02(): Bool =
        Chain.singleton(1) `Chain.compare` Chain.empty() == Comparison.GreaterThan

    @Test
    def compare03(): Bool =
        Chain.empty() `Chain.compare` Chain.singleton(1) == Comparison.LessThan

    @Test
    def compare04(): Bool =
        Chain.singleton(1) `Chain.compare` Chain.singleton(1) == Comparison.EqualTo

    @Test
    def compare05(): Bool =
        Chain.singleton(1) `Chain.compare` Chain.singleton(2) == Comparison.LessThan

    @Test
    def compare06(): Bool =
        Chain.singleton(2) `Chain.compare` Chain.singleton(1) == Comparison.GreaterThan

    @Test
    def compare07(): Bool =
        Chain.singleton(1) `Chain.compare` List.toChain(1 :: 2 :: Nil) == Comparison.LessThan

    @Test
    def compare08(): Bool =
        List.toChain(1 :: 2 :: Nil) `Chain.compare` Chain.singleton(1) == Comparison.GreaterThan

    @Test
    def compare09(): Bool =
        Chain.singleton(1) `Chain.compare` List.toChain(0 :: 0 :: Nil) == Comparison.GreaterThan

    @Test
    def compare10(): Bool =
        List.toChain(0 :: 0 :: Nil)  `Chain.compare` Chain.singleton(1) == Comparison.LessThan

    @Test
    def compare11(): Bool =
        List.toChain(1 :: 2 :: Nil) `Chain.compare` List.toChain(1 :: 1 :: Nil) == Comparison.GreaterThan

    @Test
    def compare12(): Bool =
        List.toChain(1 :: 2 :: Nil) `Chain.compare` List.toChain(1 :: 3 :: Nil) == Comparison.LessThan

    @Test
    def compare13(): Bool =
        List.toChain(1 :: 2 :: 3 :: 4 :: Nil) `Chain.compare` List.toChain(1 :: 2 :: 3 :: 4 :: Nil) == Comparison.EqualTo

    /////////////////////////////////////////////////////////////////////////////
    // sort                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    def testSortVsSortWith(c: Chain[Int32]) : Bool =
        Chain.sort(c) == Chain.sortWith(cmp, c)

    @Test
    def sort01(): Bool = testSortVsSortWith((Chain.empty(): Chain[Int32]))

    @Test
    def sort02(): Bool = testSortVsSortWith(Chain.singleton(0))

    @Test
    def sort03(): Bool = testSortVsSortWith(List.toChain(0 :: 1 :: Nil))

    @Test
    def sort04(): Bool = testSortVsSortWith(List.toChain(1 :: 0 :: Nil))

    @Test
    def sort05(): Bool = testSortVsSortWith(List.toChain(1 :: 1 :: Nil))

    @Test
    def sort06(): Bool = testSortVsSortWith(List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil))

    @Test
    def sort07(): Bool = testSortVsSortWith(List.toChain(5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil))

    @Test
    def sort08(): Bool = testSortVsSortWith(List.toChain(5 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil))

    @Test
    def sort09(): Bool = testSortVsSortWith(List.toChain(2 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // sortBy                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    def testSortByVsSort(a: Chain[Int32]) : Bool =
        (Chain.sortBy(identity, a) == Chain.sort(a)) and
        (Chain.sortBy(x -> 4*x+7, a) == Chain.sort(a)) and
        (Chain.sortBy(x -> -x, a) == Chain.sortWith(flip(cmp),a))

    @Test
    def sortBy01(): Bool = testSortByVsSort((Chain.empty(): Chain[Int32]))

    @Test
    def sortBy02(): Bool = testSortByVsSort(Chain.singleton(0))

    @Test
    def sortBy03(): Bool = testSortByVsSort(List.toChain(0 :: 1 :: Nil))

    @Test
    def sortBy04(): Bool = testSortByVsSort(List.toChain(1 :: 0 :: Nil))

    @Test
    def sortBy05(): Bool = testSortByVsSort(List.toChain(1 :: 1 :: Nil))

    @Test
    def sortBy06(): Bool = testSortByVsSort(List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil))

    @Test
    def sortBy07(): Bool = testSortByVsSort(List.toChain(5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil))

    @Test
    def sortBy08(): Bool = testSortByVsSort(List.toChain(5 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil))

    @Test
    def sortBy09(): Bool = testSortByVsSort(List.toChain(2 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // sortWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    def cmp(x: Int32, y: Int32): Comparison =
        if (x < y) Comparison.LessThan
        else if (x == y) Comparison.EqualTo
        else Comparison.GreaterThan

    @Test
    def sortWith01(): Bool =
        Chain.sortWith(cmp, (Chain.empty(): Chain[Int32])) == Chain.empty()

    @Test
    def sortWith02(): Bool =
        Chain.sortWith(cmp, Chain.singleton(0)) == Chain.singleton(0)

    @Test
    def sortWith03(): Bool =
        Chain.sortWith(cmp, List.toChain(0 :: 1 :: Nil)) == List.toChain(0 :: 1 :: Nil)

    @Test
    def sortWith04(): Bool =
        Chain.sortWith(cmp, List.toChain(1 :: 0 :: Nil)) == List.toChain(0 :: 1 :: Nil)

    @Test
    def sortWith05(): Bool =
        Chain.sortWith(cmp, List.toChain(1 :: 1 :: Nil)) == List.toChain(1 :: 1 :: Nil)

    @Test
    def sortWith06(): Bool =
        Chain.sortWith(cmp, List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil)) == List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil)

    @Test
    def sortWith07(): Bool =
        Chain.sortWith(cmp, List.toChain(5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil)) == List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil)

    @Test
    def sortWith08(): Bool =
        Chain.sortWith(cmp, List.toChain(5 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil)) == List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil)

    @Test
    def sortWith09(): Bool =
        Chain.sortWith(cmp, List.toChain(2 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil)) == List.toChain(0 :: 1 :: 2 :: 2 :: 3 :: 4 :: Nil)

    @Test
    def sortWith10(): Bool =
        Chain.sortWith(flip(cmp), List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil)) == List.toChain(5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil)

    @Test
    def sortWith11(): Bool =
        Chain.sortWith(flip(cmp), List.toChain(5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil)) == List.toChain(5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil)

    @Test
    def sortWith12(): Bool =
        Chain.sortWith(flip(cmp), List.toChain(5 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil)) == List.toChain(5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil)

    @Test
    def sortWith13(): Bool =
        Chain.sortWith(flip(cmp), List.toChain(2 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil)) == List.toChain(4 :: 3 :: 2 :: 2 :: 1 :: 0 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // sequence                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sequence01(): Bool =
        let c: Chain[Identity[Int32]] = Chain.empty();
        Chain.sequence(c) == Identity.Identity(Chain.empty())

    @Test
    def sequence02(): Bool =
        let c = Chain.singleton(Identity.Identity(1));
        Chain.sequence(c) == Identity.Identity(Chain.singleton(1))

    @Test
    def sequence03(): Bool =
        let c = List.toChain(Identity.Identity(1) :: Identity.Identity(2) :: Nil);
        Chain.sequence(c) == Identity.Identity(List.toChain(1 :: 2 :: Nil))

    @Test
    def sequence04(): Bool =
        let c = List.toChain(Identity.Identity(1) :: Identity.Identity(2) :: Identity.Identity(3) :: Nil);
        Chain.sequence(c) == Identity.Identity(List.toChain(1 :: 2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // traverse                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def traverse01(): Bool = region rc {
        let st = Ref.fresh(rc, 0);
        let c = Chain.empty();
        let ans = Chain.traverse(x -> {Ref.put(x, st); Identity.Identity(x)}, c);
        ans == Identity.Identity(Chain.empty()) and Ref.get(st) == 0
    }

    @Test
    def traverse02(): Bool = region rc {
        let st = Ref.fresh(rc, 0);
        let c = Chain.singleton(1);
        let ans = Chain.traverse(x -> {Ref.put(x, st); Identity.Identity(x)}, c);
        ans == Identity.Identity(Chain.singleton(1)) and Ref.get(st) == 1
    }

    @Test
    def traverse03(): Bool = region rc {
        let st = Ref.fresh(rc, 0);
        let c = List.toChain(1 :: 2 :: Nil);
        let ans = Chain.traverse(x -> {Ref.put(x, st); Identity.Identity(x)}, c);
        ans == Identity.Identity(List.toChain(1 :: 2 :: Nil)) and Ref.get(st) == 2
    }

    @Test
    def traverse04(): Bool = region rc {
        let st = Ref.fresh(rc, 0);
        let c = List.toChain(1 :: 2 :: 3 :: Nil);
        let ans = Chain.traverse(x -> {Ref.put(x, st); Identity.Identity(x)}, c);
        ans == Identity.Identity(List.toChain(1 :: 2 :: 3 :: Nil)) and Ref.get(st) == 3
    }

    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def join01(): Bool =
        (Nil: List[Int32]) |> List.toChain |> Chain.join(",") == ""

    @Test
    def join02(): Bool =
        (1 :: Nil) |> List.toChain |> Chain.join(",") == "1"

    @Test
    def join03(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toChain |> Chain.join(",") == "1,2,3"

    @Test
    def join04(): Bool =
        ("1" :: "2" :: "3" :: Nil) |> List.toChain |> Chain.join(",") == "1,2,3"


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinWith01(): Bool =
        (Nil: List[Int32]) |> List.toChain |>
            Chain.joinWith(x -> "${x + 1}", ",") == ""

    @Test
    def joinWith02(): Bool =
        (1 :: Nil) |> List.toChain |>
            Chain.joinWith(x -> "${x + 1}", ",") == "2"

    @Test
    def joinWith03(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toChain |>
            Chain.joinWith(x -> "${x + 1}", ",") == "2,3,4"

    @Test
    def joinWith04(): Bool =
        ("1" :: "2" :: "3" :: Nil) |> List.toChain |>
            Chain.joinWith(x -> x + x, ",") == "11,22,33"

    /////////////////////////////////////////////////////////////////////////////
    // zipWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithIndex01(): Bool = Chain.zipWithIndex((Chain.empty(): Chain[Int32])) == Chain.empty()

    @Test
    def zipWithIndex02(): Bool = Chain.zipWithIndex(Chain.singleton(2)) == List.toChain((0, 2) :: Nil)

    @Test
    def zipWithIndex03(): Bool = Chain.zipWithIndex(List.toChain(1 :: 8 :: Nil)) == List.toChain((0, 1) :: (1, 8) :: Nil)

    @Test
    def zipWithIndex04(): Bool = Chain.zipWithIndex(List.toChain(8 :: 1 :: Nil)) == List.toChain((0, 8) :: (1, 1) :: Nil)

    @Test
    def zipWithIndex05(): Bool = Chain.zipWithIndex(List.toChain(1 :: 4 :: 11 :: 2 :: -22 :: 17 :: Nil)) == List.toChain((0, 1) :: (1, 4) :: (2, 11) :: (3, 2) :: (4, -22) :: (5, 17) :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // shuffle                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def shuffle01(): Bool \ NonDet =
        let l: List[Int32] = Nil;
        let c = List.toChain(l) |> Chain.shuffle;

        Chain.length(c) == 0 and Chain.toSet(c) == Set#{}

    @Test
    def shuffle02(): Bool \ NonDet =
        let l = 1 :: Nil;
        let c = List.toChain(l) |> Chain.shuffle;

        Chain.length(c) == 1 and Chain.toSet(c) == Set#{1}

    @Test
    def shuffle03(): Bool \ NonDet =
        let l = 1 :: 2 :: 3 :: Nil;
        let c = List.toChain(l) |> Chain.shuffle;

        Chain.length(c) == 3 and Chain.toSet(c) == Set#{1, 2, 3}

    @Test
    def shuffle04(): Bool \ NonDet =
        let l = 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: Nil;
        let c = List.toChain(l) |> Chain.shuffle;

        Chain.length(c) == 10 and Chain.toSet(c) == Set#{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}

}

