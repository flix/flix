mod TestReducible {

    use Assert.{assertEq, assertTrue, assertFalse}

    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceLeft01(): Unit \ Assert =
        assertEq(expected = "a", Reducible.reduceLeft((acc, a) -> acc + a, Nel.Nel("a", Nil)))

    @Test
    def reduceLeft02(): Unit \ Assert =
        assertEq(expected = "ab", Reducible.reduceLeft((acc, a) -> acc + a, Nel.Nel("a", "b" :: Nil)))

    @Test
    def reduceLeft03(): Unit \ Assert =
        assertEq(expected = "abc", Reducible.reduceLeft((acc, a) -> acc + a, Nel.Nel("a", "b" :: "c" :: Nil)))

    @Test
    def reduceLeft04(): Unit \ Assert =
        assertEq(expected = 100, Reducible.reduceLeft((acc, a) -> (acc - a) * ((a `Int32.remainder` 2) + 1), Nel.Nel(100, Nil)))

    @Test
    def reduceLeft05(): Unit \ Assert =
        assertEq(expected = 198, Reducible.reduceLeft((acc, a) -> (acc - a) * ((a `Int32.remainder` 2) + 1), Nel.Nel(100, 1 :: Nil)))

    @Test
    def reduceLeft06(): Unit \ Assert =
        assertEq(expected = 196, Reducible.reduceLeft((acc, a) -> (acc - a) * ((a `Int32.remainder` 2) + 1), Nel.Nel(100, 1 :: 2 :: Nil)))

    @Test
    def reduceLeft07(): Unit \ Assert =
        assertEq(expected = 386, Reducible.reduceLeft((acc, a) -> (acc - a) * ((a `Int32.remainder` 2) + 1), Nel.Nel(100, 1 :: 2 :: 3 :: Nil)))

    @Test
    def reduceLeft08(): Unit \ Assert = region rc {
        let l = Ref.fresh(rc, Nil);
        // `f` uses 0 as a control value, since the first element of a `reduce` is located in `acc`.
        let f = (acc, a) -> {
            if (acc == 0)
                Ref.put(a :: Ref.get(l), l)
            else
                Ref.put(a :: acc :: Ref.get(l), l);
            0
        };
        discard Reducible.reduceLeft(f, Nel.Nel(3, 2 :: 1 :: Nil));
        assertEq(expected = 1 :: 2 :: 3 :: Nil, Ref.get(l))
    }

    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceRight01(): Unit \ Assert =
        assertEq(expected = "a", Reducible.reduceRight((a, acc) -> a + acc, Nel.Nel("a", Nil)))

    @Test
    def reduceRight02(): Unit \ Assert =
        assertEq(expected = "ab", Reducible.reduceRight((a, acc) -> a + acc, Nel.Nel("a", "b" :: Nil)))

    @Test
    def reduceRight03(): Unit \ Assert =
        assertEq(expected = "abc", Reducible.reduceRight((a, acc) -> a + acc, Nel.Nel("a", "b" :: "c" :: Nil)))

    @Test
    def reduceRight04(): Unit \ Assert =
        assertEq(expected = 100, Reducible.reduceRight((a, acc) -> (acc - a) * ((a `Int32.remainder` 2) + 1), Nel.Nel(100, Nil)))

    @Test
    def reduceRight05(): Unit \ Assert =
        assertEq(expected = 198, Reducible.reduceRight((a, acc) -> (acc - a) * ((a `Int32.remainder` 2) + 1), Nel.Nel(1, 100 :: Nil)))

    @Test
    def reduceRight06(): Unit \ Assert =
        assertEq(expected = 194, Reducible.reduceRight((a, acc) -> (acc - a) * ((a `Int32.remainder` 2) + 1), Nel.Nel(1, 2 :: 100 :: Nil)))

    @Test
    def reduceRight07(): Unit \ Assert =
        assertEq(expected = 382, Reducible.reduceRight((a, acc) -> (acc - a) * ((a `Int32.remainder` 2) + 1), Nel.Nel(1, 2 :: 3 :: 100 :: Nil)))

    @Test
    def reduceRight08(): Unit \ Assert = region rc {
        let l = Ref.fresh(rc, Nil);
        // `f` uses 0 as a control value, since the first element of a `reduce` is located in `acc`.
        let f = (a, acc) -> {
            if (acc == 0)
                Ref.put(a :: Ref.get(l), l)
            else
                Ref.put(a :: acc :: Ref.get(l), l);
            0
        };
        discard Reducible.reduceRight(f, Nel.Nel(1, 2 :: 3 :: Nil));
        assertEq(expected = 1 :: 2 :: 3 :: Nil, Ref.get(l))
    }

    /////////////////////////////////////////////////////////////////////////////
    // reduce                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduce01(): Unit \ Assert =
        assertEq(expected = "a", Reducible.reduce(Nel.Nel("a", Nil)))

    @Test
    def reduce02(): Unit \ Assert =
        assertEq(expected = "ab", Reducible.reduce(Nel.Nel("a", "b" :: Nil)))

    @Test
    def reduce03(): Unit \ Assert =
        assertEq(expected = "abc", Reducible.reduce(Nel.Nel("a", "b" :: "c" :: Nil)))


    /////////////////////////////////////////////////////////////////////////////
    // reduceMap                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceMap01(): Unit \ Assert =
        assertEq(expected = 1, Reducible.reduceMap(a -> Int16.toInt32(a), Nel.Nel(1i16, Nil)))

    @Test
    def reduceMap02(): Unit \ Assert =
        assertEq(expected = 3, Reducible.reduceMap(a -> Int16.toInt32(a), Nel.Nel(1i16, 2i16 :: Nil)))

    @Test
    def reduceMap03(): Unit \ Assert =
        assertEq(expected = 6, Reducible.reduceMap(a -> Int16.toInt32(a), Nel.Nel(1i16, 2i16 :: 3i16 :: Nil)))


    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeft01(): Unit \ Assert =
        assertEq(expected = "a", Reducible.foldLeft((acc, a) -> acc + a, "", Nel.Nel("a", Nil)))

    @Test
    def foldLeft02(): Unit \ Assert =
        assertEq(expected = "ab", Reducible.foldLeft((acc, a) -> acc + a, "", Nel.Nel("a", "b" :: Nil)))

    @Test
    def foldLeft03(): Unit \ Assert =
        assertEq(expected = "abc", Reducible.foldLeft((acc, a) -> acc + a, "", Nel.Nel("a", "b" :: "c" :: Nil)))

    @Test
    def foldLeft04(): Unit \ Assert =
        assertEq(expected = 198, Reducible.foldLeft((acc, a) -> (acc - a) * ((a `Int32.remainder` 2) + 1), 100, Nel.Nel(1, Nil)))

    @Test
    def foldLeft05(): Unit \ Assert =
        assertEq(expected = 196, Reducible.foldLeft((acc, a) -> (acc - a) * ((a `Int32.remainder` 2) + 1), 100, Nel.Nel(1, 2 :: Nil)))

    @Test
    def foldLeft06(): Unit \ Assert =
        assertEq(expected = 386, Reducible.foldLeft((acc, a) -> (acc - a) * ((a `Int32.remainder` 2) + 1), 100, Nel.Nel(1, 2 :: 3 :: Nil)))

    @Test
    def foldLeft07(): Unit \ Assert = region rc {
        let l = Ref.fresh(rc, Nil);
        discard Reducible.foldLeft((_, x) -> { Ref.put(x :: Ref.get(l), l); x == 1 }, false, Nel.Nel(3, 2 :: 1 :: Nil));
        assertEq(expected = 1 :: 2 :: 3 :: Nil, Ref.get(l))
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRight01(): Unit \ Assert =
        assertEq(expected = "a", Reducible.foldRight((a, acc) -> a + acc, "", Nel.Nel("a", Nil)))

    @Test
    def foldRight02(): Unit \ Assert =
        assertEq(expected = "ab", Reducible.foldRight((a, acc) -> a + acc, "", Nel.Nel("a", "b" :: Nil)))

    @Test
    def foldRight03(): Unit \ Assert =
        assertEq(expected = "abc", Reducible.foldRight((a, acc) -> a + acc, "", Nel.Nel("a", "b" :: "c" :: Nil)))

    @Test
    def foldRight04(): Unit \ Assert =
        assertEq(expected = 198, Reducible.foldRight((a, acc) -> (acc - a) * ((a `Int32.remainder` 2) + 1), 100, Nel.Nel(1, Nil)))

    @Test
    def foldRight05(): Unit \ Assert =
        assertEq(expected = 194, Reducible.foldRight((a, acc) -> (acc - a) * ((a `Int32.remainder` 2) + 1), 100, Nel.Nel(1, 2 :: Nil)))

    @Test
    def foldRight06(): Unit \ Assert =
        assertEq(expected = 382, Reducible.foldRight((a, acc) -> (acc - a) * ((a `Int32.remainder` 2) + 1), 100, Nel.Nel(1, 2 :: 3 :: Nil)))

    @Test
    def foldRight07(): Unit \ Assert = region rc {
        let l = Ref.fresh(rc, Nil);
        discard Reducible.foldRight((x, _) -> { Ref.put(x :: Ref.get(l), l); x == 1 }, false, Nel.Nel(1, 2 :: 3 :: Nil));
        assertEq(expected = 1 :: 2 :: 3 :: Nil, Ref.get(l))
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRightWithCont01(): Unit \ Assert =
        assertEq(expected = "a", Reducible.foldRightWithCont((a, k) -> a + k(), "", Nel.Nel("a", Nil)))

    @Test
    def foldRightWithCont02(): Unit \ Assert =
        assertEq(expected = "ab", Reducible.foldRightWithCont((a, k) -> a + k(), "", Nel.Nel("a", "b" :: Nil)))

    @Test
    def foldRightWithCont03(): Unit \ Assert =
        assertEq(expected = "abc", Reducible.foldRightWithCont((a, k) -> a + k(), "", Nel.Nel("a", "b" :: "c" :: Nil)))

    @Test
    def foldRightWithCont04(): Unit \ Assert =
        assertEq(expected = 198, Reducible.foldRightWithCont((a, k) -> (k() - a) * ((a `Int32.remainder` 2) + 1), 100, Nel.Nel(1, Nil)))

    @Test
    def foldRightWithCont05(): Unit \ Assert =
        assertEq(expected = 194, Reducible.foldRightWithCont((a, k) -> (k() - a) * ((a `Int32.remainder` 2) + 1), 100, Nel.Nel(1, 2 :: Nil)))

    @Test
    def foldRightWithCont06(): Unit \ Assert =
        assertEq(expected = 382, Reducible.foldRightWithCont((a, k) -> (k() - a) * ((a `Int32.remainder` 2) + 1), 100, Nel.Nel(1, 2 :: 3 :: Nil)))

    @Test
    def foldRightWithCont07(): Unit \ Assert = region rc {
        let l = Ref.fresh(rc, Nil);
        discard Reducible.foldRightWithCont((x, _) -> { Ref.put(x :: Ref.get(l), l); x == 1 }, false, Nel.Nel(1, 2 :: 3 :: Nil));
        assertEq(expected = 1 :: 2 :: 3 :: Nil, Ref.get(l))
    }

    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def head01(): Unit \ Assert =
        assertEq(expected = 1, Reducible.head(Nel.Nel(1, Nil)))

    @Test
    def head02(): Unit \ Assert =
        assertEq(expected = 2, Reducible.head(Nel.Nel(2, 1 :: Nil)))

    @Test
    def head03(): Unit \ Assert =
        assertEq(expected = 3, Reducible.head(Nel.Nel(3, 2 :: 1 :: Nil)))


    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def last01(): Unit \ Assert =
        assertEq(expected = 1, Reducible.last(Nel.Nel(1, Nil)))

    @Test
    def last02(): Unit \ Assert =
        assertEq(expected = 2, Reducible.last(Nel.Nel(1, 2 :: Nil)))

    @Test
    def last03(): Unit \ Assert =
        assertEq(expected = 3, Reducible.last(Nel.Nel(1, 2 :: 3 :: Nil)))


    /////////////////////////////////////////////////////////////////////////////
    // init                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def init01(): Unit \ Assert =
        assertEq(expected = Nil, Reducible.init(Nel.Nel(1, Nil)))

    @Test
    def init02(): Unit \ Assert =
        assertEq(expected = Nil, Reducible.init(Nel.Nel(2, Nil)))

    @Test
    def init03(): Unit \ Assert =
        assertEq(expected = 1 :: Nil, Reducible.init(Nel.Nel(1, 2 :: Nil)))

    @Test
    def init04(): Unit \ Assert =
        assertEq(expected = 1 :: 2 :: Nil, Reducible.init(Nel.Nel(1, 2 :: 3 :: Nil)))

    @Test
    def init05(): Unit \ Assert =
        assertEq(expected = 3 :: 2 :: Nil, Reducible.init(Nel.Nel(3, 2 :: 1 :: Nil)))

    @Test
    def init06(): Unit \ Assert =
        assertEq(expected = 1 :: 2 :: 3 :: Nil, Reducible.init(Nel.Nel(1, 2 :: 3 :: 4 :: Nil)))


    /////////////////////////////////////////////////////////////////////////////
    // tail                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tail01(): Unit \ Assert =
        assertEq(expected = Nil, Reducible.tail(Nel.Nel(1, Nil)))

    @Test
    def tail02(): Unit \ Assert =
        assertEq(expected = Nil, Reducible.tail(Nel.Nel(2, Nil)))

    @Test
    def tail03(): Unit \ Assert =
        assertEq(expected = 2 :: Nil, Reducible.tail(Nel.Nel(1, 2 :: Nil)))

    @Test
    def tail04(): Unit \ Assert =
        assertEq(expected = 2 :: 3 :: Nil, Reducible.tail(Nel.Nel(1, 2 :: 3 :: Nil)))

    @Test
    def tail05(): Unit \ Assert =
        assertEq(expected = 2 :: 1 :: Nil, Reducible.tail(Nel.Nel(3, 2 :: 1 :: Nil)))


    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reverse01(): Unit \ Assert =
        assertEq(expected = 1 :: Nil, Reducible.reverse(Nel.Nel(1, Nil)))

    @Test
    def reverse02(): Unit \ Assert =
        assertEq(expected = 2 :: 1 :: Nil, Reducible.reverse(Nel.Nel(1, 2 :: Nil)))

    @Test
    def reverse03(): Unit \ Assert =
        assertEq(expected = 3 :: 2 :: 1 :: Nil, Reducible.reverse(Nel.Nel(1, 2 :: 3 :: Nil)))


    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def count01(): Unit \ Assert =
        assertEq(expected = 0, Reducible.count(i -> i > 3, Nel.Nel(1, Nil)))

    @Test
    def count02(): Unit \ Assert =
        assertEq(expected = 1, Reducible.count(i -> i > 3, Nel.Nel(4, Nil)))

    @Test
    def count03(): Unit \ Assert =
        assertEq(expected = 0, Reducible.count(i -> i > 3, Nel.Nel(1, 2 :: Nil)))

    @Test
    def count04(): Unit \ Assert =
        assertEq(expected = 1, Reducible.count(i -> i > 3, Nel.Nel(1, 8 :: Nil)))

    @Test
    def count05(): Unit \ Assert =
        assertEq(expected = 1, Reducible.count(i -> i > 3, Nel.Nel(8, 1 :: Nil)))

    @Test
    def count06(): Unit \ Assert =
        assertEq(expected = 2, Reducible.count(i -> i > 3, Nel.Nel(6, 7 :: Nil)))


    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def length01(): Unit \ Assert =
        assertEq(expected = 1, Reducible.length(Nel.Nel(1, Nil)))

    @Test
    def length02(): Unit \ Assert =
        assertEq(expected = 2, Reducible.length(Nel.Nel(1, 2 :: Nil)))

    @Test
    def length03(): Unit \ Assert =
        assertEq(expected = 3, Reducible.length(Nel.Nel(1, 2 :: 3 :: Nil)))


    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def size01(): Unit \ Assert =
        assertEq(expected = 1, Reducible.size(Nel.Nel(1, Nil)))

    @Test
    def size02(): Unit \ Assert =
        assertEq(expected = 2, Reducible.size(Nel.Nel(1, 2 :: Nil)))

    @Test
    def size03(): Unit \ Assert =
        assertEq(expected = 3, Reducible.size(Nel.Nel(1, 2 :: 3 :: Nil)))


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sum01(): Unit \ Assert =
        assertEq(expected = 1, Reducible.sum(Nel.Nel(1, Nil)))

    @Test
    def sum02(): Unit \ Assert =
        assertEq(expected = 6, Reducible.sum(Nel.Nel(1, 2 :: 3 :: Nil)))

    @Test
    def sum03(): Unit \ Assert =
        assertEq(expected = 3, Reducible.sum(Nel.Nel(1, 2 :: 3 :: -3 :: Nil)))

    @Test
    def sum04(): Unit \ Assert =
        assertEq(expected = -10, Reducible.sum(Nel.Nel(-1, -2 :: -3 :: -4 :: Nil)))

    @Test
    def sum05(): Unit \ Assert =
        assertEq(expected = 0, Reducible.sum(Nel.Nel(10, -10 :: Nil)))

    @Test
    def sum06(): Unit \ Assert = match List.range(1, 101) {
        case x :: xs => assertEq(expected = 5050, Reducible.sum(Nel.Nel(x, xs)))
        case _       => unreachable!()
    }


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumWith01(): Unit \ Assert =
        assertEq(expected = 2, Reducible.sumWith(x -> x + 1, Nel.Nel(1, Nil)))

    @Test
    def sumWith02(): Unit \ Assert =
        assertEq(expected = 9, Reducible.sumWith(x -> x + 1, Nel.Nel(1, 2 :: 3 :: Nil)))

    @Test
    def sumWith03(): Unit \ Assert =
        assertEq(expected = 7, Reducible.sumWith(x -> x + 1, Nel.Nel(1, 2 :: 3 :: -3 :: Nil)))

    @Test
    def sumWith04(): Unit \ Assert =
        assertEq(expected = -6, Reducible.sumWith(x -> x + 1, Nel.Nel(-1, -2 :: -3 :: -4 :: Nil)))

    @Test
    def sumWith05(): Unit \ Assert =
        assertEq(expected = 2, Reducible.sumWith(x -> x + 1, Nel.Nel(10, -10 :: Nil)))


    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def exists01(): Unit \ Assert =
        assertTrue(Reducible.exists(x -> x == 1, Nel.Nel(1, Nil)))

    @Test
    def exists02(): Unit \ Assert =
        assertFalse(Reducible.exists(x -> x == 2, Nel.Nel(1, Nil)))

    @Test
    def exists03(): Unit \ Assert =
        assertTrue(Reducible.exists(x -> x == 1, Nel.Nel(1, 2 :: Nil)))

    @Test
    def exists04(): Unit \ Assert =
        assertTrue(Reducible.exists(x -> x == 2, Nel.Nel(1, 2 :: Nil)))


    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forAll01(): Unit \ Assert =
        assertTrue(Reducible.forAll(x -> x == 1, Nel.Nel(1, Nil)))

    @Test
    def forAll02(): Unit \ Assert =
        assertFalse(Reducible.forAll(x -> x == 2, Nel.Nel(1, Nil)))

    @Test
    def forAll03(): Unit \ Assert =
        assertTrue(Reducible.forAll(x -> x == 1, Nel.Nel(1, 1 :: Nil)))

    @Test
    def forAll04(): Unit \ Assert =
        assertFalse(Reducible.forAll(x -> x == 1, Nel.Nel(1, 2 :: Nil)))

    @Test
    def forAll05(): Unit \ Assert =
        assertFalse(Reducible.forAll(x -> x == 1, Nel.Nel(2, 1 :: Nil)))


    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Unit \ Assert = region rc {
        let ri = Ref.fresh(rc, 21);
        Reducible.forEach(x -> Ref.put(x, ri), Nel.Nel(1, Nil));
        assertEq(expected = 1, Ref.get(ri))
    }

    @Test
    def forEach02(): Unit \ Assert = region rc {
        let ri = Ref.fresh(rc, 21);
        Reducible.forEach(x -> Ref.put(x, ri), Nel.Nel(1, 2 :: 3 :: Nil));
        assertEq(expected = 3, Ref.get(ri))
    }

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findLeft01(): Unit \ Assert =
        assertEq(expected = None, Reducible.findLeft(x -> x == 0, Nel.Nel(1, Nil)))

    @Test
    def findLeft02(): Unit \ Assert =
        assertEq(expected = None, Reducible.findLeft(x -> x == 0, Nel.Nel(1, 2 :: Nil)))

    @Test
    def findLeft03(): Unit \ Assert =
        assertEq(expected = None, Reducible.findLeft(x -> x == 0, Nel.Nel(1, 2 :: 3 :: Nil)))

    @Test
    def findLeft04(): Unit \ Assert =
        assertEq(expected = Some(1), Reducible.findLeft(x -> x == 1, Nel.Nel(1, 2 :: 3 :: Nil)))

    @Test
    def findLeft05(): Unit \ Assert =
        assertEq(expected = Some(2), Reducible.findLeft(x -> x == 2, Nel.Nel(1, 2 :: 3 :: Nil)))

    @Test
    def findLeft06(): Unit \ Assert =
        assertEq(expected = Some(3), Reducible.findLeft(x -> x == 3, Nel.Nel(1, 2 :: 3 :: Nil)))

    @Test
    def findLeft07(): Unit \ Assert = region rc {
        let l = Ref.fresh(rc, Nil);
        discard Reducible.findLeft(x -> { Ref.put(x :: Ref.get(l), l); x == 1 }, Nel.Nel(3, 2 :: 1 :: Nil));
        assertEq(expected = 1 :: 2 :: 3 :: Nil, Ref.get(l))
    }

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findRight01(): Unit \ Assert =
        assertEq(expected = None, Reducible.findRight(x -> x == 0, Nel.Nel(1, Nil)))

    @Test
    def findRight02(): Unit \ Assert =
        assertEq(expected = None, Reducible.findRight(x -> x == 0, Nel.Nel(1, 2 :: Nil)))

    @Test
    def findRight03(): Unit \ Assert =
        assertEq(expected = None, Reducible.findRight(x -> x == 0, Nel.Nel(1, 2 :: 3 :: Nil)))

    @Test
    def findRight04(): Unit \ Assert =
        assertEq(expected = Some(1), Reducible.findRight(x -> x == 1, Nel.Nel(1, 2 :: 3 :: Nil)))

    @Test
    def findRight05(): Unit \ Assert =
        assertEq(expected = Some(2), Reducible.findRight(x -> x == 2, Nel.Nel(1, 2 :: 3 :: Nil)))

    @Test
    def findRight06(): Unit \ Assert =
        assertEq(expected = Some(3), Reducible.findRight(x -> x == 3, Nel.Nel(1, 2 :: 3 :: Nil)))

    @Test
    def findRight07(): Unit \ Assert = region rc {
        let l = Ref.fresh(rc, Nil);
        discard Reducible.findRight(x -> { Ref.put(x :: Ref.get(l), l); x == 1 }, Nel.Nel(1, 2 :: 3 :: Nil));
        assertEq(expected = 1 :: 2 :: 3 :: Nil, Ref.get(l))
    }

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def memberOf01(): Unit \ Assert =
        assertFalse(Reducible.memberOf(0, Nel.Nel(1, Nil)))

    @Test
    def memberOf02(): Unit \ Assert =
        assertFalse(Reducible.memberOf(0, Nel.Nel(1, 2 :: Nil)))

    @Test
    def memberOf03(): Unit \ Assert =
        assertFalse(Reducible.memberOf(0, Nel.Nel(1, 2 :: 3 :: Nil)))

    @Test
    def memberOf04(): Unit \ Assert =
        assertTrue(Reducible.memberOf(1, Nel.Nel(1, 2 :: 3 :: Nil)))

    @Test
    def memberOf05(): Unit \ Assert =
        assertTrue(Reducible.memberOf(2, Nel.Nel(1, 2 :: 3 :: Nil)))

    @Test
    def memberOf06(): Unit \ Assert =
        assertTrue(Reducible.memberOf(3, Nel.Nel(1, 2 :: 3 :: Nil)))


    /////////////////////////////////////////////////////////////////////////////
    // minimum                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimum01(): Unit \ Assert =
        assertEq(expected = 1, Reducible.minimum(Nel.Nel(1, Nil)))

    @Test
    def minimum02(): Unit \ Assert =
        assertEq(expected = 1, Reducible.minimum(Nel.Nel(1, 2 :: 3 :: Nil)))

    @Test
    def minimum03(): Unit \ Assert =
        assertEq(expected = 0, Reducible.minimum(Nel.Nel(1, 2 :: 0 :: 3 :: Nil)))

    @Test
    def minimum04(): Unit \ Assert =
        assertEq(expected = 3, Reducible.minimum(Nel.Nel(9, 5 :: 4 :: 6 :: 7 :: 3 :: Nil)))

    @Test
    def minimum05(): Unit \ Assert =
        assertEq(expected = 2, Reducible.minimum(Nel.Nel(9, 5 :: 4 :: 2 :: 7 :: 10 :: Nil)))


    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimumBy01(): Unit \ Assert =
        assertEq(expected = 1, Reducible.minimumBy((x, y) -> x <=> y, Nel.Nel(1, Nil)))

    @Test
    def minimumBy02(): Unit \ Assert =
        assertEq(expected = 1, Reducible.minimumBy((x, y) -> x <=> y, Nel.Nel(1, 2 :: 3 :: Nil)))

    @Test
    def minimumBy03(): Unit \ Assert =
        assertEq(expected = 0, Reducible.minimumBy((x, y) -> x <=> y, Nel.Nel(1, 2 :: 0 :: 3 :: Nil)))

    @Test
    def minimumBy04(): Unit \ Assert =
        assertEq(expected = 3, Reducible.minimumBy((x, y) -> x <=> y, Nel.Nel(9, 5 :: 4 :: 6 :: 7 :: 3 :: Nil)))

    @Test
    def minimumBy05(): Unit \ Assert =
        assertEq(expected = 2, Reducible.minimumBy((x, y) -> x <=> y, Nel.Nel(9, 5 :: 4 :: 2 :: 7 :: 10 :: Nil)))


    /////////////////////////////////////////////////////////////////////////////
    // maximum                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maximum01(): Unit \ Assert =
        assertEq(expected = 1, Reducible.maximum(Nel.Nel(1, Nil)))

    @Test
    def maximum02(): Unit \ Assert =
        assertEq(expected = 3, Reducible.maximum(Nel.Nel(1, 2 :: 3 :: Nil)))

    @Test
    def maximum03(): Unit \ Assert =
        assertEq(expected = 3, Reducible.maximum(Nel.Nel(1, 2 :: 0 :: 3 :: Nil)))

    @Test
    def maximum04(): Unit \ Assert =
        assertEq(expected = 9, Reducible.maximum(Nel.Nel(9, 5 :: 4 :: 6 :: 7 :: 3 :: Nil)))

    @Test
    def maximum05(): Unit \ Assert =
        assertEq(expected = 10, Reducible.maximum(Nel.Nel(3, 5 :: 4 :: 9 :: 7 :: 10 :: Nil)))


    /////////////////////////////////////////////////////////////////////////////
    // maximumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maximumBy01(): Unit \ Assert =
        assertEq(expected = 1, Reducible.maximumBy((x, y) -> x <=> y, Nel.Nel(1, Nil)))

    @Test
    def maximumBy02(): Unit \ Assert =
        assertEq(expected = 3, Reducible.maximumBy((x, y) -> x <=> y, Nel.Nel(1, 2 :: 3 :: Nil)))

    @Test
    def maximumBy03(): Unit \ Assert =
        assertEq(expected = 3, Reducible.maximumBy((x, y) -> x <=> y, Nel.Nel(1, 2 :: 0 :: 3 :: Nil)))

    @Test
    def maximumBy04(): Unit \ Assert =
        assertEq(expected = 9, Reducible.maximumBy((x, y) -> x <=> y, Nel.Nel(9, 5 :: 4 :: 6 :: 7 :: 3 :: Nil)))

    @Test
    def maximumBy05(): Unit \ Assert =
        assertEq(expected = 10, Reducible.maximumBy((x, y) -> x <=> y, Nel.Nel(3, 5 :: 4 :: 9 :: 7 :: 10 :: Nil)))


    /////////////////////////////////////////////////////////////////////////////
    // dropWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropWhile01(): Unit \ Assert =
        assertEq(expected = 1 :: Nil, Reducible.dropWhile(a -> a > 3, Nel.Nel(1, Nil)))

    @Test
    def dropWhile02(): Unit \ Assert =
        assertEq(expected = Nil, Reducible.dropWhile(a -> a > 3, Nel.Nel(4, Nil)))

    @Test
    def dropWhile03(): Unit \ Assert =
        assertEq(expected = 1 :: 2 :: Nil, Reducible.dropWhile(a -> a > 3, Nel.Nel(1, 2 :: Nil)))

    @Test
    def dropWhile04(): Unit \ Assert =
        assertEq(expected = 1 :: 5 :: Nil, Reducible.dropWhile(a -> a > 3, Nel.Nel(1, 5 :: Nil)))

    @Test
    def dropWhile05(): Unit \ Assert =
        assertEq(expected = 1 :: Nil, Reducible.dropWhile(a -> a > 3, Nel.Nel(5, 1 :: Nil)))

    @Test
    def dropWhile06(): Unit \ Assert =
        assertEq(expected = Nil, Reducible.dropWhile(a -> a > 3, Nel.Nel(5, 8 :: Nil)))

    @Test
    def dropWhile07(): Unit \ Assert =
        assertEq(expected = -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil, Nel.Nel(4, 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil) |>
            Reducible.dropWhile(a -> a > 3))


    /////////////////////////////////////////////////////////////////////////////
    // takeWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeWhile01(): Unit \ Assert =
        assertEq(expected = Nil, Reducible.takeWhile(a -> a > 3, Nel.Nel(1, Nil)))

    @Test
    def takeWhile02(): Unit \ Assert =
        assertEq(expected = 4 :: Nil, Reducible.takeWhile(a -> a > 3, Nel.Nel(4, Nil)))

    @Test
    def takeWhile03(): Unit \ Assert =
        assertEq(expected = Nil, Reducible.takeWhile(a -> a > 3, Nel.Nel(1, 2 :: Nil)))

    @Test
    def takeWhile04(): Unit \ Assert =
        assertEq(expected = Nil, Reducible.takeWhile(a -> a > 3, Nel.Nel(1, 5 :: Nil)))

    @Test
    def takeWhile05(): Unit \ Assert =
        assertEq(expected = 5 :: Nil, Reducible.takeWhile(a -> a > 3, Nel.Nel(5, 1 :: Nil)))

    @Test
    def takeWhile06(): Unit \ Assert =
        assertEq(expected = 5 :: 8 :: Nil, Reducible.takeWhile(a -> a > 3, Nel.Nel(5, 8 :: Nil)))

    @Test
    def takeWhile07(): Unit \ Assert =
        assertEq(expected = 4 :: 6 :: Nil, Nel.Nel(4, 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil) |>
            Reducible.takeWhile(a -> a > 3))


    /////////////////////////////////////////////////////////////////////////////
    // intersperse                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def intersperse01(): Unit \ Assert =
        assertEq(expected = 1 :: Nil, Reducible.intersperse(7, Nel.Nel(1, Nil)))

    @Test
    def intersperse02(): Unit \ Assert =
        assertEq(expected = 1 :: 7 :: 2 :: Nil, Reducible.intersperse(7, Nel.Nel(1, 2 :: Nil)))

    @Test
    def intersperse03(): Unit \ Assert =
        assertEq(expected = 1 :: 7 :: 2 :: 7 :: 3 :: Nil, Reducible.intersperse(7, Nel.Nel(1, 2 :: 3 :: Nil)))


    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toArray01(): Unit \ Assert = region rc {
        assertTrue(Array.sameElements(Reducible.toArray(rc, Nel.Nel(1, Nil)), Array#{1} @ rc))
    }

    @Test
    def toArray02(): Unit \ Assert = region rc {
        assertTrue(Array.sameElements(Reducible.toArray(rc, Nel.Nel(1, 2 :: 3 :: Nil)), Array#{1, 2, 3} @ rc))
    }

    @Test
    def toArray03(): Unit \ Assert = region rc {
        assertTrue(Array.sameElements(Reducible.toArray(rc, Nel.Nel(1, 1 :: 2 :: 3 :: Nil)), Array#{1, 1, 2, 3} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toVector01(): Unit \ Assert =
        assertEq(expected = Vector#{1}, Reducible.toVector(Nel.Nel(1, Nil)))

    @Test
    def toVector02(): Unit \ Assert =
        assertEq(expected = Vector#{1, 2, 3}, Reducible.toVector(Nel.Nel(1, 2 :: 3 :: Nil)))

    @Test
    def toVector03(): Unit \ Assert =
        assertEq(expected = Vector#{1, 1, 2, 3}, Reducible.toVector(Nel.Nel(1, 1 :: 2 :: 3 :: Nil)))


    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toList01(): Unit \ Assert =
        assertEq(expected = 1 :: Nil, Reducible.toList(Nel.Nel(1, Nil)))

    @Test
    def toList02(): Unit \ Assert =
        assertEq(expected = 1 :: 2 :: 3 :: Nil, Reducible.toList(Nel.Nel(1, 2 :: 3 :: Nil)))

    @Test
    def toList03(): Unit \ Assert =
        assertEq(expected = 1 :: 1 :: 2 :: 3 :: Nil, Reducible.toList(Nel.Nel(1, 1 :: 2 :: 3 :: Nil)))


    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMap01(): Unit \ Assert =
        assertEq(expected = Map#{1 => 1}, Reducible.toMap(Nel.Nel((1, 1), Nil)))

    @Test
    def toMap02(): Unit \ Assert =
        assertEq(expected = Map#{"a" => 1, "b" => 2, "c" => 3}, Reducible.toMap(Nel.Nel(("a", 1), ("b", 2) :: ("c", 3) :: Nil)))

    @Test
    def toMap03(): Unit \ Assert =
        assertEq(expected = Map#{1 => 2, 2 => 2, 3 => 3, 4 => 3}, Reducible.toMap(Nel.Nel((1, 2), (4, 3) :: (2, 2) :: (3, 3) :: Nil)))


    /////////////////////////////////////////////////////////////////////////////
    // toNel                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toNel01(): Unit \ Assert =
        assertEq(expected = Nel.Nel(1, Nil), Reducible.toNel(Nel.Nel(1, Nil)))

    @Test
    def toNel02(): Unit \ Assert =
        assertEq(expected = Nel.Nel(1, 2 :: 3 :: Nil), Reducible.toNel(Nel.Nel(1, 2 :: 3 :: Nil)))

    @Test
    def toNel03(): Unit \ Assert =
        assertEq(expected = Nel.Nel(1, 1 :: 2 :: 3 :: Nil), Reducible.toNel(Nel.Nel(1, 1 :: 2 :: 3 :: Nil)))

    @Test
    def toNel04(): Unit \ Assert =
        match List.range(0, 100) {
            case x :: xs => assertEq(expected = List.range(0, 100), Reducible.toNel(Nel.Nel(x, xs)) |> Nel.toList)
            case _       => unreachable!()
        }


    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toSet01(): Unit \ Assert =
        assertEq(expected = Set#{1}, Reducible.toSet(Nel.Nel(1, Nil)))

    @Test
    def toSet02(): Unit \ Assert =
        assertEq(expected = Set#{1, 2, 3}, Reducible.toSet(Nel.Nel(1, 2 :: 3 :: Nil)))

    @Test
    def toSet03(): Unit \ Assert =
        assertEq(expected = Set#{1, 2, 3}, Reducible.toSet(Nel.Nel(1, 1 :: 2 :: 3 :: Nil)))

}
