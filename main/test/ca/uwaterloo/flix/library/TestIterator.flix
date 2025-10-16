mod TestIterator {
    use Iterator.Iterator
    use Assert.{assertEq, assertTrue, assertFalse};

    /////////////////////////////////////////////////////////////////////////////
    // next                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def next01(): Unit \ Assert = region rc {
        assertEq(expected = (None: Option[Int32]), Iterator.empty(rc) |> Iterator.next)
    }

    @Test
    def next02(): Unit \ Assert = region rc {
        let iter = List.iterator(rc, 1 :: Nil);
        assertEq(expected = Some(1), Iterator.next(iter))
    }

    @Test
    def next03(): Unit \ Assert = region rc {
        let iter = List.iterator(rc, 1 :: Nil);
        discard Iterator.next(iter);
        assertEq(expected = None, Iterator.next(iter))
    }

    @Test
    def next04(): Unit \ Assert = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: Nil);
        discard Iterator.next(iter);
        assertEq(expected = Some(2), Iterator.next(iter))
    }


    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def range01(): Unit \ Assert = region rc {
        assertEq(expected = Nil, Iterator.range(rc, 0, 0) |> Iterator.toList)
    }

    @Test
    def range02(): Unit \ Assert = region rc {
        assertEq(expected = Nil, Iterator.range(rc, 10, 0) |> Iterator.toList)
    }

    @Test
    def range03(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: 2 :: 3 :: Nil, Iterator.range(rc, 1, 4) |> Iterator.toList)
    }

    @Test
    def range04(): Unit \ Assert = region rc {
        assertEq(expected = -1 :: 0 :: 1 :: Nil, Iterator.range(rc, -1, 2) |> Iterator.toList)
    }

    @Test
    def range05(): Unit \ Assert = region rc {
        assertEq(expected = List.range(-100, 200), Iterator.range(rc, -100, 200) |> Iterator.toList)
    }


    /////////////////////////////////////////////////////////////////////////////
    // repeat                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def repeat01(): Unit \ Assert = region rc {
        assertEq(expected = Nil, Iterator.repeat(rc, 0, 10) |> Iterator.toList)
    }

    @Test
    def repeat02(): Unit \ Assert = region rc {
        assertEq(expected = Nil, Iterator.repeat(rc, -100, 2) |> Iterator.toList)
    }

    @Test
    def repeat03(): Unit \ Assert = region rc {
        assertEq(expected = List.repeat(100, 2), Iterator.repeat(rc, 100, 2) |> Iterator.toList)
    }

    @Test
    def repeat04(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: 1 :: 1 :: Nil, Iterator.repeat(rc, 3, 1) |> Iterator.toList)
    }


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sum01(): Unit \ Assert = region rc {
        assertEq(expected = 0, List.iterator(rc, Nil) |> Iterator.sum)
    }

    @Test
    def sum02(): Unit \ Assert = region rc {
        assertEq(expected = 1, List.iterator(rc, 1 :: Nil) |> Iterator.sum)
    }

    @Test
    def sum03(): Unit \ Assert = region rc {
        assertEq(expected = 6, List.iterator(rc, 1 :: 2 :: 3 :: Nil) |> Iterator.sum)
    }

    @Test
    def sum04(): Unit \ Assert = region rc {
        assertEq(expected = 3, List.iterator(rc, 1 :: 2 :: 3 :: -3 :: Nil) |> Iterator.sum)
    }

    @Test
    def sum05(): Unit \ Assert = region rc {
        assertEq(expected = -10, List.iterator(rc, -1 :: -2 :: -3 :: -4 :: Nil) |> Iterator.sum)
    }

    @Test
    def sum06(): Unit \ Assert = region rc {
        assertEq(expected = 0, List.iterator(rc, 10 :: -10 :: Nil) |> Iterator.sum)
    }

    @Test
    def sum07(): Unit \ Assert = region rc {
        assertEq(expected = 5050, Iterator.range(rc, 1, 101) |> Iterator.sum)
    }


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumWith01(): Unit \ Assert = region rc {
        assertEq(expected = 0, List.iterator(rc, Nil) |> Iterator.sumWith(x -> x + 1))
    }

    @Test
    def sumWith02(): Unit \ Assert = region rc {
        assertEq(expected = 2, List.iterator(rc, 1 :: Nil) |> Iterator.sumWith(x -> x + 1))
    }

    @Test
    def sumWith03(): Unit \ Assert = region rc {
        assertEq(expected = 9, List.iterator(rc, 1 :: 2 :: 3 :: Nil) |> Iterator.sumWith(x -> x + 1))
    }

    @Test
    def sumWith04(): Unit \ Assert = region rc {
        assertEq(expected = 7, List.iterator(rc, 1 :: 2 :: 3 :: -3 :: Nil) |> Iterator.sumWith(x -> x + 1))
    }

    @Test
    def sumWith05(): Unit \ Assert = region rc {
        assertEq(expected = -6, List.iterator(rc, -1 :: -2 :: -3 :: -4 :: Nil) |> Iterator.sumWith(x -> x + 1))
    }

    @Test
    def sumWith06(): Unit \ Assert = region rc {
        assertEq(expected = 2, List.iterator(rc, 10 :: -10 :: Nil) |> Iterator.sumWith(x -> x + 1))
    }


    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toList01(): Unit \ Assert = region rc {
        assertEq(expected = Nil, (Nil: List[Int32]) |> List.iterator(rc) |> Iterator.toList)
    }

    @Test
    def toList02(): Unit \ Assert = region rc {
        assertEq(expected = -10 :: -9 :: -8 :: -7 :: -6 :: Nil, Iterator.range(rc, -10, -5) |> Iterator.toList)
    }

    @Test
    def toList03(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: 1 ::  1 :: Nil, Iterator.repeat(rc, 3, 1) |> Iterator.toList)
    }

    @Test
    def toList04(): Unit \ Assert = region rc {
        assertEq(expected = List.range(-100, 200), Iterator.range(rc, -100, 200) |> Iterator.toList)
    }


    /////////////////////////////////////////////////////////////////////////////
    // toChain                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toChain01(): Unit \ Assert = region rc {
        assertEq(expected = Chain.empty(), (Nil: List[Int32]) |> List.iterator(rc) |> Iterator.toChain)
    }

    @Test
    def toChain02(): Unit \ Assert = region rc {
        assertEq(expected = List.toChain(-10 :: -9 :: -8 :: -7 :: -6 :: Nil), Iterator.range(rc, -10, -5) |> Iterator.toChain)
    }

    @Test
    def toChain03(): Unit \ Assert = region rc {
        assertEq(expected = List.toChain(1 :: 1 ::  1 :: Nil), Iterator.repeat(rc, 3, 1) |> Iterator.toChain)
    }

    @Test
    def toChain04(): Unit \ Assert = region rc {
        assertEq(expected = Chain.range(-100, 200), Iterator.range(rc, -100, 200) |> Iterator.toChain)
    }


    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toArray01(): Unit \ Assert = region rc {
        assertTrue((Nil: List[Int32]) |> List.iterator(rc) |> Iterator.toArray(rc) `Array.sameElements` Array#{} @ rc)
    }

    @Test
    def toArray02(): Unit \ Assert = region rc {
        assertTrue(Iterator.range(rc, -10, -5) |> Iterator.toArray(rc) `Array.sameElements` Array#{-10, -9, -8, -7, -6} @ rc)
    }

    @Test
    def toArray03(): Unit \ Assert = region rc {
        assertTrue(Iterator.repeat(rc, 3, 1) |> Iterator.toArray(rc) `Array.sameElements` Array#{1, 1, 1} @ rc)
    }

    @Test
    def toArray04(): Unit \ Assert = region rc {
        assertTrue(Iterator.range(rc, -100, 200) |> Iterator.toArray(rc) `Array.sameElements` Array.range(rc, -100, 200))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toVector01(): Unit \ Assert = region rc {
        assertTrue((Iterator.empty(rc) |> Iterator.toVector) `Vector.equals` (Vector.empty() : Vector[Int32]))
    }

    @Test
    def toVector02(): Unit \ Assert = region rc {
        assertTrue((Iterator.singleton(rc, 0) |> Iterator.toVector) `Vector.equals` Vector.singleton(0))
    }

    @Test
    def toVector03(): Unit \ Assert = region rc {
        assertTrue((Iterator.range(rc, 1, 4) |> Iterator.toVector) `Vector.equals` Vector.range(1, 4))
    }


    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMap01(): Unit \ Assert = region rc {
        assertEq(expected = (Map.empty(): Map[Int32, Int32]), Map.empty() |> Map.iterator(rc) |> Iterator.toMap)
    }

    @Test
    def toMap02(): Unit \ Assert = region rc {
        assertEq(expected = Map#{"A" => 1, "B" => 2, "C" => 3, "D" => 4}, Map#{"A" => 1, "B" => 2, "C" => 3, "D" => 4} |> Map.iterator(rc) |> Iterator.toMap)
    }

    @Test
    def toMap03(): Unit \ Assert = region rc {
        assertEq(expected = Map#{1 => 2, 2 => 3, 3 => 4}, Map#{1 => 2, 2 => 3, 3 => 4} |> Map.iterator(rc) |> Iterator.toMap)
    }


    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////////////////
    // toNel                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toNel01(): Unit \ Assert = region rc {
        assertEq(expected = (None: Option[Nel[Int32]]), Nil |> List.iterator(rc) |> Iterator.toNel)
    }

    @Test
    def toNel02(): Unit \ Assert = region rc {
        assertEq(expected = Some(Nel.Nel(1, Nil)), (1 :: Nil) |> List.iterator(rc) |> Iterator.toNel)
    }

    @Test
    def toNel03(): Unit \ Assert = region rc {
        assertEq(expected = Some(Nel.Nel(0, List.range(1, 1000))), Iterator.range(rc, 0, 1000) |> Iterator.toNel)
    }


    /////////////////////////////////////////////////////////////////////////////
    // toNec                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toNec01(): Unit \ Assert = region rc {
        let iter = List.iterator(rc, (Nil: List[Unit]));
        assertEq(expected = None, Iterator.toNec(iter))
    }

    @Test
    def toNec02(): Unit \ Assert = region rc {
        let iter = List.iterator(rc, 1 :: Nil);
        assertEq(expected = Some(Nec.singleton(1)), Iterator.toNec(iter))
    }

    @Test
    def toNec03(): Unit \ Assert = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: Nil);
        assertEq(expected = Some(Nec.cons(1, Nec.singleton(2))), Iterator.toNec(iter))
    }

    @Test
    def toNec04(): Unit \ Assert = region rc {
        let iter = List.iterator(rc, 1 :: 1 :: Nil);
        assertEq(expected = Some(Nec.cons(1, Nec.singleton(1))), Iterator.toNec(iter))
    }

    @Test
    def toNec05(): Unit \ Assert = region rc {
        let iter = List.iterator(rc, 2 :: 1 :: Nil);
        assertEq(expected = Some(Nec.cons(2, Nec.singleton(1))), Iterator.toNec(iter))
    }


    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toSet01(): Unit \ Assert = region rc {
        assertEq(expected = (Set.empty(): Set[Int32]), Set.empty() |> Set.iterator(rc) |> Iterator.toSet)
    }

    @Test
    def toSet02(): Unit \ Assert = region rc {
        assertEq(expected = Set#{"a", "b", "c", "d"}, Set#{"a", "b", "c", "d"} |> Set.iterator(rc) |> Iterator.toSet)
    }

    @Test
    def toSet03(): Unit \ Assert = region rc {
        assertEq(expected = Set.range(0, 10000), Set.range(0, 10000) |> Set.iterator(rc) |> Iterator.toSet)
    }


    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filter01(): Unit \ Assert = region rc {
        assertEq(expected = Nil, Nil |> List.iterator(rc) |> Iterator.filter(i -> i > 3) |> Iterator.toList)
    }

    @Test
    def filter02(): Unit \ Assert = region rc {
        assertEq(expected = Nil, (2 :: Nil) |> List.iterator(rc) |> Iterator.filter(i -> i > 3) |> Iterator.toList)
    }

    @Test
    def filter03(): Unit \ Assert = region rc {
        assertEq(expected = 4 :: Nil, (4 :: Nil) |> List.iterator(rc) |> Iterator.filter(i -> i > 3) |> Iterator.toList)
    }

    @Test
    def filter04(): Unit \ Assert = region rc {
        assertEq(expected = Nil, (1 :: 3 :: Nil) |> List.iterator(rc) |> Iterator.filter(i -> i > 3) |> Iterator.toList)
    }

    @Test
    def filter05(): Unit \ Assert = region rc {
        assertEq(expected = 8 :: Nil, (1 :: 8 :: Nil) |> List.iterator(rc) |> Iterator.filter(i -> i > 3) |> Iterator.toList)
    }

    @Test
    def filter06(): Unit \ Assert = region rc {
        assertEq(expected = 8 :: Nil, (8 :: 1 :: Nil) |> List.iterator(rc) |> Iterator.filter(i -> i > 3) |> Iterator.toList)
    }

    @Test
    def filter07(): Unit \ Assert = region rc {
        assertEq(expected = 8 :: 9 :: Nil, (8 :: 9 :: Nil) |> List.iterator(rc) |> Iterator.filter(i -> i > 3) |> Iterator.toList)
    }

    @Test
    def filter08(): Unit \ Assert = region rc {
        assertEq(expected = 4 :: 11 :: 17 :: Nil, (1 :: 4 :: 11 :: 2 :: -22 :: 17 :: Nil) |> List.iterator(rc) |> Iterator.filter(i -> i > 3) |> Iterator.toList)
    }


    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map01(): Unit \ Assert = region rc {
        assertEq(expected = Nil, Nil |> List.iterator(rc) |>
            Iterator.map(i -> i > 2) |> Iterator.toList)
    }

    @Test
    def map02(): Unit \ Assert = region rc {
        assertEq(expected = false :: Nil, (1 :: Nil) |> List.iterator(rc) |>
            Iterator.map(i -> i > 2) |> Iterator.toList)
    }

    @Test
    def map03(): Unit \ Assert = region rc {
        assertEq(expected = true :: Nil, (3 :: Nil) |> List.iterator(rc) |>
            Iterator.map(i -> i > 2) |> Iterator.toList)
    }

    @Test
    def map04(): Unit \ Assert = region rc {
        assertEq(expected = false :: false :: Nil, (1 :: 2 :: Nil) |> List.iterator(rc) |>
            Iterator.map(i -> i > 2) |> Iterator.toList)
    }

    @Test
    def map05(): Unit \ Assert = region rc {
        assertEq(expected = false :: true :: Nil, (1 :: 8 :: Nil) |> List.iterator(rc) |>
            Iterator.map(i -> i > 2) |> Iterator.toList)
    }

    @Test
    def map06(): Unit \ Assert = region rc {
        assertEq(expected = true :: false :: Nil, (8 :: 1 :: Nil) |> List.iterator(rc) |>
            Iterator.map(i -> i > 2) |> Iterator.toList)
    }

    @Test
    def map07(): Unit \ Assert = region rc {
        assertEq(expected = true :: true :: Nil, (7 :: 8 :: Nil) |> List.iterator(rc) |>
            Iterator.map(i -> i > 2) |> Iterator.toList)
    }


    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapWithIndex01(): Unit \ Assert = region rc {
        assertEq(expected = Nil, Nil |> List.iterator(rc) |>
            Iterator.mapWithIndex((ix, i) -> if (ix `Int32.remainder` 2 == 0) 0 else i) |> Iterator.toList)
    }

    @Test
    def mapWithIndex02(): Unit \ Assert = region rc {
        assertEq(expected = 0 :: Nil, (1 :: Nil) |> List.iterator(rc) |>
            Iterator.mapWithIndex((ix, i) -> if (ix `Int32.remainder` 2 == 0) 0 else i) |> Iterator.toList)
    }

    @Test
    def mapWithIndex03(): Unit \ Assert = region rc {
        assertEq(expected = 0 :: Nil, (3 :: Nil) |> List.iterator(rc) |>
            Iterator.mapWithIndex((ix, i) -> if (ix `Int32.remainder` 2 == 0) 0 else i) |> Iterator.toList)
    }

    @Test
    def mapWithIndex04(): Unit \ Assert = region rc {
        assertEq(expected = 0 :: 2 :: Nil, (1 :: 2 :: Nil) |> List.iterator(rc) |>
            Iterator.mapWithIndex((ix, i) -> if (ix `Int32.remainder` 2 == 0) 0 else i) |> Iterator.toList)
    }

    @Test
    def mapWithIndex05(): Unit \ Assert = region rc {
        assertEq(expected = 0 :: 8 :: 0 :: Nil, (1 :: 8 :: 10 :: Nil) |> List.iterator(rc) |>
            Iterator.mapWithIndex((ix, i) -> if (ix `Int32.remainder` 2 == 0) 0 else i) |> Iterator.toList)
    }

    @Test
    def mapWithIndex06(): Unit \ Assert = region rc {
        assertEq(expected = 0 :: 8 :: 0 :: Nil, (10 :: 8 :: 1 :: Nil) |> List.iterator(rc) |>
            Iterator.mapWithIndex((ix, i) -> if (ix `Int32.remainder` 2 == 0) 0 else i) |> Iterator.toList)
    }

    @Test
    def mapWithIndex07(): Unit \ Assert = region rc {
        assertEq(expected = 0 :: 8 :: 0 :: 14 :: Nil, (1 :: 8 :: 10 :: 14 :: Nil) |> List.iterator(rc) |>
            Iterator.mapWithIndex((ix, i) -> if (ix `Int32.remainder` 2 == 0) 0 else i) |> Iterator.toList)
    }


    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Unit \ Assert = region rc {
        let res = Ref.fresh(rc, 21);
        Nil |> List.iterator(rc) |>
            Iterator.forEach(x -> Ref.put(x, res));
        assertEq(expected = 21, Ref.get(res))
    }

    @Test
    def forEach02(): Unit \ Assert = region rc {
        let res = Ref.fresh(rc, 21);
        (1 :: 2 :: 3 :: Nil) |> List.iterator(rc) |>
            Iterator.forEach(x -> Ref.put(x, res));
        assertEq(expected = 3, Ref.get(res))
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEachWithIndex01(): Unit \ Assert = region rc {
        let res = Ref.fresh(rc, 21);
        Nil |> List.iterator(rc) |>
            Iterator.forEachWithIndex((i, _) -> Ref.put(i, res));
        assertEq(expected = 21, Ref.get(res))
    }

    @Test
    def forEachWithIndex02(): Unit \ Assert = region rc {
        let res = Ref.fresh(rc, 21);
        (1 :: 2 :: 3 :: Nil) |> List.iterator(rc) |>
            Iterator.forEachWithIndex((i, _) -> Ref.put(i, res));
        assertEq(expected = 2, Ref.get(res))
    }

    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def replace01(): Unit \ Assert = region rc {
        assertEq(expected = Nil, List.iterator(rc, Nil) |>
            Iterator.replace(src =  3, dst =  4) |> Iterator.toList)
    }

    @Test
    def replace02(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: Nil, List.iterator(rc, 1 :: Nil) |>
            Iterator.replace(src =  3, dst =  4) |> Iterator.toList)
    }

    @Test
    def replace03(): Unit \ Assert = region rc {
        assertEq(expected = 4 :: Nil, List.iterator(rc, 3 :: Nil) |>
            Iterator.replace(src =  3, dst =  4) |> Iterator.toList)
    }

    @Test
    def replace04(): Unit \ Assert = region rc {
        assertEq(expected = 4 :: Nil, List.iterator(rc, 4 :: Nil) |>
            Iterator.replace(src =  3, dst =  4) |> Iterator.toList)
    }

    @Test
    def replace05(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: 2 :: Nil, List.iterator(rc, 1 :: 2 :: Nil) |>
            Iterator.replace(src =  3, dst =  4) |> Iterator.toList)
    }

    @Test
    def replace06(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: 4 :: Nil, List.iterator(rc, 1 :: 3 :: Nil) |>
            Iterator.replace(src =  3, dst =  4) |> Iterator.toList)
    }

    @Test
    def replace07(): Unit \ Assert = region rc {
        assertEq(expected = 4 :: 4 :: Nil, List.iterator(rc, 3 :: 4 :: Nil) |>
            Iterator.replace(src =  3, dst =  4) |> Iterator.toList)
    }

    @Test
    def replace08(): Unit \ Assert = region rc {
        assertEq(expected = 4 :: 4 :: Nil, List.iterator(rc, 3 :: 3 :: Nil) |>
            Iterator.replace(src =  3, dst =  4) |> Iterator.toList)
    }

    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def append01(): Unit \ Assert = region rc {
        let iter1 = List.iterator(rc, (Nil: List[Unit]));
        let iter2 = List.iterator(rc, (Nil: List[Unit]));
        assertEq(expected = Nil, Iterator.append(iter1, iter2) |> Iterator.toList)
    }

    @Test
    def append02(): Unit \ Assert = region rc {
        let iter1 = List.iterator(rc, Nil);
        let iter2 = List.iterator(rc, 1 :: Nil);
        assertEq(expected = 1 :: Nil, Iterator.append(iter1, iter2) |> Iterator.toList)
    }

    @Test
    def append03(): Unit \ Assert = region rc {
        let iter1 = List.iterator(rc, Nil);
        let iter2 = List.iterator(rc, 1 :: 2 :: Nil);
        assertEq(expected = 1 :: 2 :: Nil, Iterator.append(iter1, iter2) |> Iterator.toList)
    }

    @Test
    def append04(): Unit \ Assert = region rc {
        let iter1 = List.iterator(rc, 1 :: Nil);
        let iter2 = List.iterator(rc, Nil);
        assertEq(expected = 1 :: Nil, Iterator.append(iter1, iter2) |> Iterator.toList)
    }

    @Test
    def append05(): Unit \ Assert = region rc {
        let iter1 = List.iterator(rc, 1 :: 2 :: Nil);
        let iter2 = List.iterator(rc, Nil);
        assertEq(expected = 1 :: 2 :: Nil, Iterator.append(iter1, iter2) |> Iterator.toList)
    }

    @Test
    def append06(): Unit \ Assert = region rc {
        let iter1 = List.iterator(rc, 1 :: Nil);
        let iter2 = List.iterator(rc, 2 :: Nil);
        assertEq(expected = 1 :: 2 :: Nil, Iterator.append(iter1, iter2) |> Iterator.toList)
    }

    @Test
    def append07(): Unit \ Assert = region rc {
        let iter1 = List.iterator(rc, 1 :: 2 :: Nil);
        let iter2 = List.iterator(rc, 3 :: Nil);
        assertEq(expected = 1 :: 2 :: 3 :: Nil, Iterator.append(iter1, iter2) |> Iterator.toList)
    }

    @Test
    def append08(): Unit \ Assert = region rc {
        let iter1 = List.iterator(rc, 1 :: Nil);
        let iter2 = List.iterator(rc, 2 :: 3 :: Nil);
        assertEq(expected = 1 :: 2 :: 3 :: Nil, Iterator.append(iter1, iter2) |> Iterator.toList)
    }

    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zip01(): Unit \ Assert = region rc {
        let iterA = (Nil: List[Int32]) |> List.iterator(rc);
        let iterB = Iterator.repeat(rc, 1000, "b");
        assertEq(expected = Nil, Iterator.zip(iterA, iterB) |> Iterator.toList)
    }

    @Test
    def zip02(): Unit \ Assert = region rc {
        let iterA = (Nil: List[Int32]) |> List.iterator(rc);
        let iterB = Iterator.repeat(rc, 1000, "b");
        assertEq(expected = Nil, Iterator.zip(iterB, iterA) |> Iterator.toList)
    }

    @Test
    def zip03(): Unit \ Assert = region rc {
        let iterA = Iterator.repeat(rc, 500,  "a");
        let iterB = Iterator.repeat(rc, 1000, "b");
        assertEq(expected = List.repeat(500, ("a", "b")), Iterator.zip(iterA, iterB) |> Iterator.toList)
    }

    @Test
    def zip04(): Unit \ Assert = region rc {
        let iterA = Iterator.repeat(rc, 1000, "a");
        let iterB = Iterator.repeat(rc, 500,  "b");
        assertEq(expected = List.repeat(500, ("a", "b")), Iterator.zip(iterA, iterB) |> Iterator.toList)
    }

    @Test
    def zip05(): Unit \ Assert = region rc {
        let iterA = Iterator.repeat(rc, 1000, "a");
        let iterB = Iterator.repeat(rc, 1000, "b");
        assertEq(expected = List.repeat(1000, ("a", "b")), Iterator.zip(iterA, iterB) |> Iterator.toList)
    }

    @Test
    def zip06(): Unit \ Assert = region rc {
        let iterA = Iterator.range(rc, 0, 500);
        let iterB = Iterator.repeat(rc, 1000, "b");
        assertEq(expected = List.zip(List.range(0, 500), List.repeat(500, "b")), Iterator.zip(iterA, iterB) |> Iterator.toList)
    }

    /////////////////////////////////////////////////////////////////////////////
    // zipWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWith01(): Unit \ Assert = region rc {
        let iterA = List.iterator(rc, Nil);
        let iterB = List.iterator(rc, Nil);
        assertEq(expected = Nil, Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) |> Iterator.toList)
    }

    @Test
    def zipWith02(): Unit \ Assert = region rc {
        let iterA = List.iterator(rc, 1 :: Nil);
        let iterB = List.iterator(rc, Nil);
        assertEq(expected = Nil, Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) |> Iterator.toList)
    }

    @Test
    def zipWith03(): Unit \ Assert = region rc {
        let iterA = List.iterator(rc, Nil);
        let iterB = List.iterator(rc, true :: Nil);
        assertEq(expected = Nil, Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) |> Iterator.toList)
    }

    @Test
    def zipWith04(): Unit \ Assert = region rc {
        let iterA = List.iterator(rc, 1 :: Nil);
        let iterB = List.iterator(rc, true :: Nil);
        assertEq(expected = 2 :: Nil, Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) |> Iterator.toList)
    }

    @Test
    def zipWith05(): Unit \ Assert = region rc {
        let iterA = List.iterator(rc, 1 :: Nil);
        let iterB = List.iterator(rc, false :: Nil);
        assertEq(expected = 1 :: Nil, Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) |> Iterator.toList)
    }

    @Test
    def zipWith06(): Unit \ Assert = region rc {
        let iterA = List.iterator(rc, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        let iterB = List.iterator(rc, false :: true :: true :: false :: false :: true :: true :: true :: Nil);
        assertEq(expected = 1 :: 3 :: 4 :: 4 :: 5 :: 7 :: 8 :: 9 :: Nil, Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) |> Iterator.toList)
    }

    @Test
    def zipWith07(): Unit \ Assert = region rc1 {
        let iterA = List.iterator(rc1, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        region rc2 {
            let iterB = List.iterator(rc2, false :: true :: true :: false :: false :: true :: true :: true :: Nil);
            assertEq(expected = 1 :: 3 :: 4 :: 4 :: 5 :: 7 :: 8 :: 9 :: Nil, Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) |> Iterator.toList)
        }
    }


    /////////////////////////////////////////////////////////////////////////////
    // zipWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithIndex01(): Unit \ Assert = region rc {
        assertEq(expected = Nil, Iterator.zipWithIndex((Iterator.empty(rc): Iterator[Int32, rc, rc])) |> Iterator.toList)
    }

    @Test
    def zipWithIndex02(): Unit \ Assert = region rc {
        assertEq(expected = (0, 2) :: Nil, Iterator.zipWithIndex(Iterator.singleton(rc, 2)) |> Iterator.toList)
    }

    @Test
    def zipWithIndex03(): Unit \ Assert = region rc {
        assertEq(expected = (0, 1) :: (1, 8) :: Nil, Iterator.zipWithIndex(List.iterator(rc, 1 :: 8 :: Nil)) |> Iterator.toList)
    }

    @Test
    def zipWithIndex04(): Unit \ Assert = region rc {
        assertEq(expected = (0, 8) :: (1, 1) :: Nil, Iterator.zipWithIndex(List.iterator(rc, 8 :: 1 :: Nil)) |> Iterator.toList)
    }

    @Test
    def zipWithIndex05(): Unit \ Assert = region rc {
        assertEq(expected = (0, 1) :: (1, 4) :: (2, 11) :: (3, 2) :: (4, -22) :: (5, 17) :: Nil, Iterator.zipWithIndex(List.iterator(rc, 1 :: 4 :: 11 :: 2 :: -22 :: 17 :: Nil)) |> Iterator.toList)
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeft01(): Unit \ Assert = region rc {
        let iter = List.iterator(rc, Nil);
        assertEq(expected = 100, Iterator.foldLeft((i, e) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, iter))
    }

    @Test
    def foldLeft02(): Unit \ Assert = region rc {
        let iter = List.iterator(rc, 1 :: Nil);
        assertEq(expected = 198, Iterator.foldLeft((i, e) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, iter))
    }

    @Test
    def foldLeft03(): Unit \ Assert = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: Nil);
        assertEq(expected = 196, Iterator.foldLeft((i, e) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, iter))
    }

    @Test
    def foldLeft04(): Unit \ Assert = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: 3 :: Nil);
        assertEq(expected = 386, Iterator.foldLeft((i, e) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, iter))
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRight01(): Unit \ Assert = region rc {
        let iter = List.iterator(rc, Nil);
        assertEq(expected = 100, Iterator.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, iter))
    }

    @Test
    def foldRight02(): Unit \ Assert = region rc {
        let iter = List.iterator(rc, 1 :: Nil);
        assertEq(expected = 198, Iterator.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, iter))
    }

    @Test
    def foldRight03(): Unit \ Assert = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: Nil);
        assertEq(expected = 194, Iterator.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, iter))
    }

    @Test
    def foldRight04(): Unit \ Assert = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: 3 :: Nil);
        assertEq(expected = 382, Iterator.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, iter))
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRightWithCont01(): Unit \ Assert = region rc {
        let iter = List.iterator(rc, Nil);
        assertEq(expected = 100, Iterator.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, iter))
    }

    @Test
    def foldRightWithCont02(): Unit \ Assert = region rc {
        let iter = List.iterator(rc, 1 :: Nil);
        assertEq(expected = 198, Iterator.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, iter))
    }

    @Test
    def foldRightWithCont03(): Unit \ Assert = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: Nil);
        assertEq(expected = 194, Iterator.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, iter))
    }

    @Test
    def foldRightWithCont04(): Unit \ Assert = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: 3 :: Nil);
        assertEq(expected = 382, Iterator.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, iter))
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldMap01(): Unit \ Assert = region rc {
        assertEq(expected = 0, Iterator.foldMap(x -> 2 * x, List.iterator(rc, Nil)))
    }

    @Test
    def foldMap02(): Unit \ Assert = region rc {
        assertEq(expected = 6, Iterator.foldMap(x -> 2 * x, List.iterator(rc, 1 :: 2 :: Nil)))
    }

    @Test
    def foldMap03(): Unit \ Assert = region rc {
        assertEq(expected = "b", Iterator.foldMap(x -> if (x == "a") "b" else x, List.iterator(rc, "a" :: Nil)))
    }

    @Test
    def foldMap04(): Unit \ Assert = region rc {
        assertEq(expected = "abb", Iterator.foldMap(x -> if (x == "c") "b" else x, List.iterator(rc, "a" :: "b" :: "c" :: Nil)))
    }

    @Test
    def foldMap05(): Unit \ Assert = region rc {
        assertEq(expected = "123", Iterator.foldMap(Int32.toString, List.iterator(rc, 1 :: 2 :: 3 :: Nil)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceLeft01(): Unit \ Assert = region rc {
        let iter = List.iterator(rc, (Nil: List[Int32]));
        assertEq(expected = None, Iterator.reduceLeft((a, b) -> a - b, iter))
    }

    @Test
    def reduceLeft02(): Unit \ Assert = region rc {
        let iter = List.iterator(rc, 1 :: Nil);
        assertEq(expected = Some(1), Iterator.reduceLeft((a, b) -> a - b, iter))
    }

    @Test
    def reduceLeft03(): Unit \ Assert = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: Nil);
        assertEq(expected = Some(-1), Iterator.reduceLeft((a, b) -> a - b, iter))
    }

    @Test
    def reduceLeft04(): Unit \ Assert = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: 3 :: Nil);
        assertEq(expected = Some(-4), Iterator.reduceLeft((a, b) -> a - b, iter))
    }

    @Test
    def reduceLeft05(): Unit \ Assert = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: 3 :: 4 :: Nil);
        assertEq(expected = Some(-8), Iterator.reduceLeft((a, b) -> a - b, iter))
    }


    /////////////////////////////////////////////////////////////////////////////
    // takeWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeWhile01(): Unit \ Assert = region rc {
        assertEq(expected = Nil, List.iterator(rc, (Nil: List[Int32])) |>
            Iterator.takeWhile(i -> i > 3) |> Iterator.toList)
    }

    @Test
    def takeWhile02(): Unit \ Assert = region rc {
        assertEq(expected = Nil, List.iterator(rc, 1 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) |> Iterator.toList)
    }

    @Test
    def takeWhile03(): Unit \ Assert = region rc {
        assertEq(expected = 4 :: Nil, List.iterator(rc, 4 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) |> Iterator.toList)
    }

    @Test
    def takeWhile04(): Unit \ Assert = region rc {
        assertEq(expected = Nil, List.iterator(rc, 1 :: 2 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) |> Iterator.toList)
    }

    @Test
    def takeWhile05(): Unit \ Assert = region rc {
        assertEq(expected = Nil, List.iterator(rc, 1 :: 5 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) |> Iterator.toList)
    }

    @Test
    def takeWhile06(): Unit \ Assert = region rc {
        assertEq(expected = 5 :: Nil, List.iterator(rc, 5 :: 1 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) |> Iterator.toList)
    }

    @Test
    def takeWhile07(): Unit \ Assert = region rc {
        assertEq(expected = 5 :: 8 :: Nil, List.iterator(rc, 5 :: 8 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) |> Iterator.toList)
    }

    @Test
    def takeWhile08(): Unit \ Assert = region rc {
        assertEq(expected = 4 :: 6 :: Nil, List.iterator(rc, 4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) |> Iterator.toList)
    }


    /////////////////////////////////////////////////////////////////////////////
    // dropWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropWhile01(): Unit \ Assert = region rc {
        assertEq(expected = Nil, List.iterator(rc, Nil) |>
            Iterator.dropWhile(i -> i > 3) |> Iterator.toList)
    }

    @Test
    def dropWhile02(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: Nil, List.iterator(rc, 1 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) |> Iterator.toList)
    }

    @Test
    def dropWhile03(): Unit \ Assert = region rc {
        assertEq(expected = Nil, List.iterator(rc, 4 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) |> Iterator.toList)
    }

    @Test
    def dropWhile04(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: 2 :: Nil, List.iterator(rc, 1 :: 2 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) |> Iterator.toList)
    }

    @Test
    def dropWhile05(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: 5 :: Nil, List.iterator(rc, 1 :: 5 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) |> Iterator.toList)
    }

    @Test
    def dropWhile06(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: Nil, List.iterator(rc, 5 :: 1 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) |> Iterator.toList)
    }

    @Test
    def dropWhile07(): Unit \ Assert = region rc {
        assertEq(expected = Nil, List.iterator(rc, 5 :: 8 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) |> Iterator.toList)
    }

    @Test
    def dropWhile08(): Unit \ Assert = region rc {
        assertEq(expected = -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil, List.iterator(rc, 4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) |> Iterator.toList)
    }


    /////////////////////////////////////////////////////////////////////////////
    // flatMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatMap01(): Unit \ Assert = region rc {
        assertEq(expected = Nil, (List.iterator(rc, Nil): Iterator[Int32, _, _]) |>
            Iterator.flatMap(i -> Iterator.repeat(rc, i, i)) |>
            Iterator.toList)
    }

    @Test
    def flatMap02(): Unit \ Assert = region rc {
        assertEq(expected = Nil, List.iterator(rc, 0 :: Nil) |>
            Iterator.flatMap(i -> Iterator.repeat(rc, i, i)) |>
            Iterator.toList)
    }

    @Test
    def flatMap03(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: Nil, List.iterator(rc, 1 :: Nil) |>
            Iterator.flatMap(i -> Iterator.repeat(rc, i, i)) |>
            Iterator.toList)
    }

    @Test
    def flatMap04(): Unit \ Assert = region rc {
        assertEq(expected = 2 :: 2 :: Nil, List.iterator(rc, 2 :: Nil) |>
            Iterator.flatMap(i -> Iterator.repeat(rc, i, i)) |>
            Iterator.toList)
    }

    @Test
    def flatMap05(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: 2 :: 2 :: Nil, List.iterator(rc, 1 :: 2 :: Nil) |>
            Iterator.flatMap(i -> Iterator.repeat(rc, i, i)) |>
            Iterator.toList)
    }

    @Test
    def flatMap06(): Unit \ Assert = region rc {
        assertEq(expected = 2 :: 2 :: 3 :: 3 :: 3 :: Nil, List.iterator(rc, 2 :: 3 :: Nil) |>
            Iterator.flatMap(i -> Iterator.repeat(rc, i, i)) |>
            Iterator.toList)
    }

    @Test
    def flatMap07(): Unit \ Assert = region rc {
        assertEq(expected = 2 :: 2 :: 4 :: 4 :: 4 :: 4 :: Nil, List.iterator(rc, 1 :: 2 :: 3 :: 4 :: Nil) |>
            Iterator.flatMap(i -> if (i `Int32.remainder` 2 == 0) Iterator.repeat(rc, i, i) else Iterator.empty(rc)) |>
            Iterator.toList)
    }

    /////////////////////////////////////////////////////////////////////////////
    // intersperse                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def intersperse01(): Unit \ Assert = region rc {
        assertEq(expected = Nil, List.iterator(rc, Nil) |>
            Iterator.intersperse(11) |>
            Iterator.toList)
    }

    @Test
    def intersperse02(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: Nil, List.iterator(rc, 1 :: Nil) |>
            Iterator.intersperse(11) |>
            Iterator.toList)
    }

    @Test
    def intersperse03(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: 11 :: 2 :: Nil, List.iterator(rc, 1 :: 2 :: Nil) |>
            Iterator.intersperse(11) |>
            Iterator.toList)
    }

    @Test
    def intersperse04(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: 11 :: 2 :: 11 :: 3 :: Nil, List.iterator(rc, 1 :: 2 :: 3 :: Nil) |>
            Iterator.intersperse(11) |>
            Iterator.toList)
    }

    @Test
    def intersperse05(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: 11 :: 2 :: 11 :: 3 :: 11 :: 4 :: Nil, List.iterator(rc, 1 :: 2 :: 3 :: 4 :: Nil) |>
            Iterator.intersperse(11) |>
            Iterator.toList)
    }


    /////////////////////////////////////////////////////////////////////////////
    // intercalate                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def intercalate01(): Unit \ Assert = region rc {
        let sep: List[Int32] = Nil;
        let iter = Nil |> List.iterator(rc);
        assertEq(expected = (Nil: List[Int32]), Iterator.intercalate(sep, iter) |> Iterator.toList)
    }

    @Test
    def intercalate02(): Unit \ Assert = region rc {
        let sep: List[Int32] = Nil;
        let iterA1 = (1 :: Nil) |> List.iterator(rc);
        let iter  = (iterA1 :: Nil) |> List.iterator(rc);
        assertEq(expected = 1 :: Nil, Iterator.intercalate(sep, iter) |>
            Iterator.toList)
    }

    @Test
    def intercalate03(): Unit \ Assert = region rc {
        let sep = 11 :: 12 :: 13 :: Nil;
        let iter = Nil |> List.iterator(rc);
        assertEq(expected = Nil, Iterator.intercalate(sep, iter) |>
            Iterator.toList)
    }

    @Test
    def intercalate04(): Unit \ Assert = region rc {
        let sep: List[Int32] = Nil;
        let iterA1 = (1 :: Nil) |> List.iterator(rc);
        let iterA2 = (2 :: 3 :: Nil) |> List.iterator(rc);
        let iter = (iterA1 :: iterA2 :: Nil) |> List.iterator(rc);
        assertEq(expected = 1 :: 2 :: 3 :: Nil, Iterator.intercalate(sep, iter) |>
            Iterator.toList)
    }

    @Test
    def intercalate05(): Unit \ Assert = region rc {
        let sep  = 11 :: 12 :: 13 :: Nil;
        let iterA1 = (1 :: Nil) |> List.iterator(rc);
        let iterA2 = (2 :: 3 :: Nil) |> List.iterator(rc);
        let iter = (iterA1 :: iterA2 :: Nil) |> List.iterator(rc);
        assertEq(expected = 1 :: 11 :: 12 :: 13 :: 2 :: 3 :: Nil, Iterator.intercalate(sep, iter) |>
            Iterator.toList)
    }

    @Test
    def intercalate06(): Unit \ Assert = region rc {
        let sep = Nil;
        let iterA1 = (1 :: Nil) |> List.iterator(rc);
        let iterA2 = (2 :: 3 :: Nil) |> List.iterator(rc);
        let iterA3 = (4 :: Nil) |> List.iterator(rc);
        let iter = (iterA1 :: iterA2 :: iterA3 :: Nil) |> List.iterator(rc);
        assertEq(expected = 1 :: 2 :: 3 :: 4 :: Nil, Iterator.intercalate(sep, iter) |>
            Iterator.toList)
    }

    /////////////////////////////////////////////////////////////////////////////
    // unfoldWithOk                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unfoldWithOk01(): Unit \ Assert = region rc {
        let iter = Iterator.unfoldWithOk(rc, () -> Err(0));
        assertEq(expected = (Nil: List[Int32]), Iterator.toList(iter))
    }

    @Test
    def unfoldWithOk02(): Unit \ Assert = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: 3 :: 4 :: 5 :: Nil);
        assertEq(expected = 1 :: 2 :: 3 :: 4 :: 5 :: Nil, Iterator.unfoldWithOk(rc, () -> {Iterator.next(iter) |> Option.toOk("Done")}) |>
            Iterator.toList)
        }

    @Test
    def unfoldWithOk03(): Unit \ Assert = region rc {
        let iter = (1 :: 2 :: 3 :: 4 :: 5 :: Nil) |>
            List.map(x -> if (x == 4) Err("Done") else Ok(x)) |>
            List.iterator(rc);
        assertEq(expected = 1 :: 2 :: 3 :: Nil, Iterator.unfoldWithOk(rc, () -> {Iterator.next(iter) |> Option.getWithDefault(Err("Done"))}) |>
            Iterator.toList)
        }


    /////////////////////////////////////////////////////////////////////////////
    // drop                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def drop01(): Unit \ Assert = region rc {
        assertEq(expected = Nil, (Nil: List[Int32]) |> List.iterator(rc) |>
            Iterator.drop(1) |> Iterator.toList)
    }
    @Test
    def drop02(): Unit \ Assert = region rc {
        assertEq(expected = Nil, (1 :: Nil) |> List.iterator(rc) |>
            Iterator.drop(1) |> Iterator.toList)
    }

    @Test
    def drop03(): Unit \ Assert = region rc {
        assertEq(expected = 3 :: Nil, (1 :: 2 :: 3 :: Nil) |> List.iterator(rc) |>
            Iterator.drop(2) |> Iterator.toList)
    }

    @Test
    def drop04(): Unit \ Assert = region rc {
        assertEq(expected = Nil, (1 :: 2 :: 3 :: Nil) |> List.iterator(rc) |>
            Iterator.drop(3) |> Iterator.toList)
    }

    @Test
    def drop05(): Unit \ Assert = region rc {
        assertEq(expected = Nil, (1 :: 2 :: 3 :: Nil) |> List.iterator(rc) |>
            Iterator.drop(4) |> Iterator.toList)
    }

    @Test
    def drop06(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: 2 :: 3 :: Nil, (1 :: 2 :: 3 :: Nil) |> List.iterator(rc) |>
            Iterator.drop(0) |> Iterator.toList)
    }


    /////////////////////////////////////////////////////////////////////////////
    // take                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def take01(): Unit \ Assert = region rc {
        assertEq(expected = Nil, List.iterator(rc, (Nil: List[Unit])) |> Iterator.take(-1) |> Iterator.toList)
    }

    @Test
    def take02(): Unit \ Assert = region rc {
        assertEq(expected = Nil, List.iterator(rc, (Nil: List[Unit])) |> Iterator.take(0) |> Iterator.toList)
    }

    @Test
    def take03(): Unit \ Assert = region rc {
        assertEq(expected = Nil, List.iterator(rc, (Nil: List[Unit])) |> Iterator.take(1) |> Iterator.toList)
    }

    @Test
    def take04(): Unit \ Assert = region rc {
        assertEq(expected = Nil, List.iterator(rc, 1 :: Nil) |> Iterator.take(-1) |> Iterator.toList)
    }

    @Test
    def take05(): Unit \ Assert = region rc {
        assertEq(expected = Nil, List.iterator(rc, 1 :: Nil) |> Iterator.take(0) |> Iterator.toList)
    }

    @Test
    def take06(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: Nil, List.iterator(rc, 1 :: Nil) |> Iterator.take(1) |> Iterator.toList)
    }

    @Test
    def take07(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: Nil, List.iterator(rc, 1 :: Nil) |> Iterator.take(2) |> Iterator.toList)
    }

    @Test
    def take08(): Unit \ Assert = region rc {
        assertEq(expected = Nil, List.iterator(rc, 1 :: 2 :: Nil) |> Iterator.take(0) |> Iterator.toList)
    }

    @Test
    def take09(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: Nil, List.iterator(rc, 1 :: 2 :: Nil) |> Iterator.take(1) |> Iterator.toList)
    }

    @Test
    def take10(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: 2 :: Nil, List.iterator(rc, 1 :: 2 :: Nil) |> Iterator.take(2) |> Iterator.toList)
    }

    @Test
    def take11(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: 2 :: Nil, List.iterator(rc, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil) |> Iterator.take(2) |> Iterator.toList)
    }

    @Test
    def take12(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: 2 :: 3 :: 4 :: Nil, List.iterator(rc, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil) |> Iterator.take(4) |> Iterator.toList)
    }

    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def join01(): Unit \ Assert = region rc {
        assertEq(expected = "", (Nil: List[Int32]) |> List.iterator(rc) |>
            Iterator.join(","))
    }

    @Test
    def join02(): Unit \ Assert = region rc {
        assertEq(expected = "1", (1 :: Nil) |> List.iterator(rc) |>
            Iterator.join(","))
    }

    @Test
    def join03(): Unit \ Assert = region rc {
        assertEq(expected = "1,2,3", (1 :: 2 :: 3 :: Nil) |> List.iterator(rc) |>
            Iterator.join(","))
    }

    @Test
    def join04(): Unit \ Assert = region rc {
        assertEq(expected = "1,2,3", ("1" :: "2" :: "3" :: Nil) |> List.iterator(rc) |>
            Iterator.join(","))
    }


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinWith01(): Unit \ Assert = region rc {
        assertEq(expected = "", (Nil: List[Int32]) |> List.iterator(rc) |>
            Iterator.joinWith(x -> "${x + 1}", ","))
    }

    @Test
    def joinWith02(): Unit \ Assert = region rc {
        assertEq(expected = "2", (1 :: Nil) |> List.iterator(rc) |>
            Iterator.joinWith(x -> "${x + 1}", ","))
    }

    @Test
    def joinWith03(): Unit \ Assert = region rc {
        assertEq(expected = "2,3,4", (1 :: 2 :: 3 :: Nil) |> List.iterator(rc) |>
            Iterator.joinWith(x -> "${x + 1}", ","))
    }

    @Test
    def joinWith04(): Unit \ Assert = region rc {
        assertEq(expected = "11,22,33", ("1" :: "2" :: "3" :: Nil) |> List.iterator(rc) |>
            Iterator.joinWith(x -> x + x, ","))
    }


    /////////////////////////////////////////////////////////////////////////////
    // filterMap                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterMap01(): Unit \ Assert = region rc {
        assertEq(expected = Nil, (Nil: List[Int32]) |> List.iterator(rc) |>
            Iterator.filterMap(x -> if (x `Int32.remainder` 2 == 0) Some(x) else None) |> Iterator.toList)
    }
    @Test
    def filterMap02(): Unit \ Assert = region rc {
        assertEq(expected = Nil, (1 :: Nil) |> List.iterator(rc) |>
            Iterator.filterMap(x -> if (x `Int32.remainder` 2 == 0) Some(x) else None) |> Iterator.toList)
    }

    @Test
    def filterMap03(): Unit \ Assert = region rc {
        assertEq(expected = 2 :: Nil, (1 :: 2 :: 3 :: Nil) |> List.iterator(rc) |>
            Iterator.filterMap(x -> if (x `Int32.remainder` 2 == 0) Some(x) else None) |> Iterator.toList)
    }

    @Test
    def filterMap04(): Unit \ Assert = region rc {
        assertEq(expected = 2 :: 4 :: 6 :: Nil, (1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil) |> List.iterator(rc) |>
            Iterator.filterMap(x -> if (x `Int32.remainder` 2 == 0) Some(x) else None) |> Iterator.toList)
    }

    /////////////////////////////////////////////////////////////////////////////
    // cons                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def cons01(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: Nil, (Nil: List[Int32]) |> List.iterator(rc) |>
            Iterator.cons(1) |> Iterator.toList)
    }

    @Test
    def cons02(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: 2 :: Nil, (2 :: (Nil: List[Int32])) |> List.iterator(rc) |>
            Iterator.cons(1) |> Iterator.toList)
    }

    @Test
    def cons03(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: 2 :: 3 :: Nil, (3 :: (Nil: List[Int32])) |> List.iterator(rc) |>
            Iterator.cons(2) |> Iterator.cons(1) |> Iterator.toList)
    }

    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatten01(): Unit \ Assert = region rc {
        assertEq(expected = Nil, (Nil: List[Int32]) |> List.iterator(rc) |> Iterator.map(i -> Iterator.repeat(rc, 2, i)) |>
            Iterator.flatten |> Iterator.toList)
    }

    @Test
    def flatten02(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: 1 :: Nil, (1 :: Nil) |> List.iterator(rc) |> Iterator.map(i -> Iterator.repeat(rc, 2, i)) |>
            Iterator.flatten |> Iterator.toList)
    }

    @Test
    def flatten03(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: 1 :: 2 :: 2 :: Nil, (1 :: 2 :: Nil) |> List.iterator(rc) |> Iterator.map(i -> Iterator.repeat(rc, 2, i)) |>
            Iterator.flatten |> Iterator.toList)
    }

    @Test
    def flatten04(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: 1 :: 2 :: 2 :: 3 :: 3 :: Nil, (1 :: 2 :: 3 :: Nil) |> List.iterator(rc) |> Iterator.map(i -> Iterator.repeat(rc, 2, i)) |>
            Iterator.flatten |> Iterator.toList)
    }

}
