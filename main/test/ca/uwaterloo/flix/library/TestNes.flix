mod TestNes {

    use Assert.{assertEq, assertNeq, assertTrue, assertFalse, assertNone};
    use Hash.hash

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toString01(): Unit \ Assert = assertEq(expected = "Nes#{1}", Nes.toString(Nes.singleton(1)))

    @Test
    def toString02(): Unit \ Assert = assertEq(expected = "Nes#{1, 2, 3}", Nes.toString(Nes.insert(3, Nes.insert(2, Nes.singleton(1)))))

    @Test
    def toString03(): Unit \ Assert = assertEq(expected = "Nes#{1, 2}", Nes.toString(Nes.Nes(1, Set#{1, 2})))

    /////////////////////////////////////////////////////////////////////////////
    // singleton                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def singleton01(): Unit \ Assert = assertEq(expected = 1, Nes.size(Nes.singleton(1)))

    @Test
    def singleton02(): Unit \ Assert = assertEq(expected = 1, Nes.minimum(Nes.singleton(1)))

    @Test
    def singleton03(): Unit \ Assert = assertEq(expected = 1, Nes.maximum(Nes.singleton(1)))

    /////////////////////////////////////////////////////////////////////////////
    // insert                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def insert01(): Unit \ Assert = assertEq(expected = Set#{1}, Nes.toSet(Nes.insert(1, Nes.singleton(1))))

    @Test
    def insert02(): Unit \ Assert = assertEq(expected = Set#{1, 2}, Nes.toSet(Nes.insert(2, Nes.singleton(1))))

    @Test
    def insert03(): Unit \ Assert = assertEq(expected = 3, Nes.size(Nes.insert(3, Nes.insert(2, Nes.singleton(1)))))

    /////////////////////////////////////////////////////////////////////////////
    // minimum / maximum                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimum01(): Unit \ Assert = assertEq(expected = 1, Nes.minimum(Nes.insert(3, Nes.insert(2, Nes.singleton(1)))))

    @Test
    def minimum02(): Unit \ Assert = assertEq(expected = 1, Nes.minimum(Nes.insert(1, Nes.insert(2, Nes.singleton(3)))))

    @Test
    def maximum01(): Unit \ Assert = assertEq(expected = 3, Nes.maximum(Nes.insert(3, Nes.insert(2, Nes.singleton(1)))))

    @Test
    def maximum02(): Unit \ Assert = assertEq(expected = 3, Nes.maximum(Nes.insert(1, Nes.insert(2, Nes.singleton(3)))))

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def size01(): Unit \ Assert = assertEq(expected = 1, Nes.size(Nes.singleton(1)))

    @Test
    def size02(): Unit \ Assert = assertEq(expected = 3, Nes.size(Nes.insert(3, Nes.insert(2, Nes.singleton(1)))))

    @Test
    def size03(): Unit \ Assert = assertEq(expected = 1, Nes.size(Nes.insert(1, Nes.insert(1, Nes.singleton(1)))))

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def memberOf01(): Unit \ Assert = assertTrue(Nes.memberOf(1, Nes.singleton(1)))

    @Test
    def memberOf02(): Unit \ Assert = assertFalse(Nes.memberOf(2, Nes.singleton(1)))

    @Test
    def memberOf03(): Unit \ Assert = assertTrue(Nes.memberOf(2, Nes.insert(3, Nes.insert(2, Nes.singleton(1)))))

    /////////////////////////////////////////////////////////////////////////////
    // union                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def union01(): Unit \ Assert = assertEq(expected = Set#{1, 2}, Nes.toSet(Nes.union(Nes.singleton(1), Nes.singleton(2))))

    @Test
    def union02(): Unit \ Assert = assertEq(expected = Set#{1, 2, 3}, Nes.toSet(Nes.union(Nes.insert(2, Nes.singleton(1)), Nes.insert(3, Nes.singleton(2)))))

    /////////////////////////////////////////////////////////////////////////////
    // intersection                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def intersection01(): Unit \ Assert = assertEq(expected = Set#{2}, Nes.intersection(Nes.insert(2, Nes.singleton(1)), Nes.insert(3, Nes.singleton(2))))

    @Test
    def intersection02(): Unit \ Assert = assertEq(expected = (Set#{}: Set[Int32]), Nes.intersection(Nes.singleton(1), Nes.singleton(2)))

    /////////////////////////////////////////////////////////////////////////////
    // difference                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def difference01(): Unit \ Assert = assertEq(expected = Set#{1}, Nes.difference(Nes.insert(2, Nes.singleton(1)), Nes.singleton(2)))

    @Test
    def difference02(): Unit \ Assert = assertEq(expected = (Set#{}: Set[Int32]), Nes.difference(Nes.singleton(1), Nes.insert(2, Nes.singleton(1))))

    /////////////////////////////////////////////////////////////////////////////
    // isSubsetOf / isProperSubsetOf                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isSubsetOf01(): Unit \ Assert = assertTrue(Nes.isSubsetOf(Nes.singleton(1), Nes.insert(2, Nes.singleton(1))))

    @Test
    def isSubsetOf02(): Unit \ Assert = assertTrue(Nes.isSubsetOf(Nes.singleton(1), Nes.singleton(1)))

    @Test
    def isSubsetOf03(): Unit \ Assert = assertFalse(Nes.isSubsetOf(Nes.insert(2, Nes.singleton(1)), Nes.singleton(1)))

    @Test
    def isProperSubsetOf01(): Unit \ Assert = assertTrue(Nes.isProperSubsetOf(Nes.singleton(1), Nes.insert(2, Nes.singleton(1))))

    @Test
    def isProperSubsetOf02(): Unit \ Assert = assertFalse(Nes.isProperSubsetOf(Nes.singleton(1), Nes.singleton(1)))

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map01(): Unit \ Assert = assertEq(expected = Set#{2, 4, 6}, Nes.toSet(Nes.map(x -> x * 2, Nes.insert(3, Nes.insert(2, Nes.singleton(1))))))

    @Test
    def map02(): Unit \ Assert = assertEq(expected = 1, Nes.size(Nes.map(_ -> 1, Nes.insert(3, Nes.insert(2, Nes.singleton(1))))))

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filter01(): Unit \ Assert = assertEq(expected = Set#{2, 3}, Nes.filter(x -> x > 1, Nes.insert(3, Nes.insert(2, Nes.singleton(1)))))

    @Test
    def filter02(): Unit \ Assert = assertEq(expected = (Set#{}: Set[Int32]), Nes.filter(x -> x > 10, Nes.singleton(1)))

    /////////////////////////////////////////////////////////////////////////////
    // flatMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatMap01(): Unit \ Assert = assertEq(expected = Set#{10, 20}, Nes.toSet(Nes.flatMap(x -> Nes.singleton(x * 10), Nes.insert(2, Nes.singleton(1)))))

    /////////////////////////////////////////////////////////////////////////////
    // fold / reduce                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeft01(): Unit \ Assert = assertEq(expected = 6, Nes.foldLeft((acc, x) -> acc + x, 0, Nes.insert(3, Nes.insert(2, Nes.singleton(1)))))

    @Test
    def foldRight01(): Unit \ Assert = assertEq(expected = 6, Nes.foldRight((x, acc) -> x + acc, 0, Nes.insert(3, Nes.insert(2, Nes.singleton(1)))))

    @Test
    def reduce01(): Unit \ Assert = assertEq(expected = 6, Nes.reduce(Nes.insert(3, Nes.insert(2, Nes.singleton(1)))))

    @Test
    def reduce02(): Unit \ Assert = assertEq(expected = 1, Nes.reduce(Nes.singleton(1)))

    /////////////////////////////////////////////////////////////////////////////
    // exists / forAll / count                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def exists01(): Unit \ Assert = assertTrue(Nes.exists(x -> x == 2, Nes.insert(3, Nes.insert(2, Nes.singleton(1)))))

    @Test
    def exists02(): Unit \ Assert = assertFalse(Nes.exists(x -> x == 5, Nes.insert(3, Nes.insert(2, Nes.singleton(1)))))

    @Test
    def forAll01(): Unit \ Assert = assertTrue(Nes.forAll(x -> x > 0, Nes.insert(3, Nes.insert(2, Nes.singleton(1)))))

    @Test
    def forAll02(): Unit \ Assert = assertFalse(Nes.forAll(x -> x > 1, Nes.insert(3, Nes.insert(2, Nes.singleton(1)))))

    @Test
    def count01(): Unit \ Assert = assertEq(expected = 2, Nes.count(x -> x > 1, Nes.insert(3, Nes.insert(2, Nes.singleton(1)))))

    /////////////////////////////////////////////////////////////////////////////
    // toList / toSet / toArray / toVector                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toList01(): Unit \ Assert = assertEq(expected = 1 :: 2 :: 3 :: Nil, Nes.toList(Nes.insert(3, Nes.insert(2, Nes.singleton(1)))))

    @Test
    def toSet01(): Unit \ Assert = assertEq(expected = Set#{1, 2, 3}, Nes.toSet(Nes.insert(3, Nes.insert(2, Nes.singleton(1)))))

    @Test
    def toVector01(): Unit \ Assert = assertEq(expected = Vector#{1, 2, 3}, Nes.toVector(Nes.insert(3, Nes.insert(2, Nes.singleton(1)))))

    /////////////////////////////////////////////////////////////////////////////
    // fromSet / fromList                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fromSet01(): Unit \ Assert = assertNone(Nes.fromSet((Set#{}: Set[Int32])))

    @Test
    def fromSet02(): Unit \ Assert = assertEq(expected = Some(Nes.insert(2, Nes.singleton(1))), Nes.fromSet(Set#{1, 2}))

    @Test
    def fromList01(): Unit \ Assert = assertNone(Nes.fromList((Nil: List[Int32])))

    @Test
    def fromList02(): Unit \ Assert = assertEq(expected = Some(Nes.insert(2, Nes.singleton(1))), Nes.fromList(1 :: 2 :: 1 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // Eq / Order / Hash                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def eq01(): Unit \ Assert = assertEq(expected = Nes.insert(2, Nes.singleton(1)), Nes.insert(1, Nes.singleton(2)))

    @Test
    def eq02(): Unit \ Assert = assertNeq(unexpected = Nes.singleton(1), Nes.singleton(2))

    @Test
    def order01(): Unit \ Assert = assertEq(expected = Comparison.LessThan, Nes.singleton(1) <=> Nes.singleton(2))

    @Test
    def hash01(): Unit \ Assert = assertEq(expected = hash(Nes.insert(2, Nes.singleton(1))), hash(Nes.insert(1, Nes.singleton(2))))

    /////////////////////////////////////////////////////////////////////////////
    // join / joinWith                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def join01(): Unit \ Assert = assertEq(expected = "1, 2, 3", Nes.join(", ", Nes.insert(3, Nes.insert(2, Nes.singleton(1)))))

    @Test
    def joinWith01(): Unit \ Assert = assertEq(expected = "<1>, <2>, <3>", Nes.joinWith(x -> "<${x}>", ", ", Nes.insert(3, Nes.insert(2, Nes.singleton(1)))))

    /////////////////////////////////////////////////////////////////////////////
    // find / findLeft / findRight                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def find01(): Unit \ Assert = assertEq(expected = Some(2), Nes.find(x -> x > 1, Nes.insert(3, Nes.insert(2, Nes.singleton(1)))))

    @Test
    def findLeft01(): Unit \ Assert = assertEq(expected = Some(2), Nes.findLeft(x -> x > 1, Nes.insert(3, Nes.insert(2, Nes.singleton(1)))))

    @Test
    def findLeft02(): Unit \ Assert = assertNone(Nes.findLeft(x -> x > 10, Nes.insert(3, Nes.insert(2, Nes.singleton(1)))))

    @Test
    def findRight01(): Unit \ Assert = assertEq(expected = Some(3), Nes.findRight(x -> x > 1, Nes.insert(3, Nes.insert(2, Nes.singleton(1)))))

    /////////////////////////////////////////////////////////////////////////////
    // fold / foldMap                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fold01(): Unit \ Assert = assertEq(expected = 6, Nes.fold(Nes.insert(3, Nes.insert(2, Nes.singleton(1)))))

    @Test
    def foldMap01(): Unit \ Assert = assertEq(expected = 6, Nes.foldMap(x -> x, Nes.insert(3, Nes.insert(2, Nes.singleton(1)))))

    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toArray01(): Unit \ Assert = region rc {
        assertTrue(Array.sameElements(Nes.toArray(rc, Nes.insert(3, Nes.insert(2, Nes.singleton(1)))), Array#{1, 2, 3} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEach / forEachWithIndex                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Unit \ Assert = region rc {
        let acc = Ref.fresh(rc, 0);
        Nes.forEach(x -> Ref.put(Ref.get(acc) + x, acc), Nes.insert(3, Nes.insert(2, Nes.singleton(1))));
        assertEq(expected = 6, Ref.get(acc))
    }

    @Test
    def forEachWithIndex01(): Unit \ Assert = region rc {
        let acc = Ref.fresh(rc, Nil);
        Nes.forEachWithIndex((i, x) -> Ref.put((i, x) :: Ref.get(acc), acc), Nes.insert(3, Nes.insert(2, Nes.singleton(1))));
        assertEq(expected = (0, 1) :: (1, 2) :: (2, 3) :: Nil, List.reverse(Ref.get(acc)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterator01(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: 2 :: 3 :: Nil, Nes.iterator(rc, Nes.insert(3, Nes.insert(2, Nes.singleton(1)))) |> Iterator.toList)
    }

    /////////////////////////////////////////////////////////////////////////////
    // non-canonical representations                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def rawHeadInRest01(): Unit \ Assert = assertEq(expected = 2, Nes.size(Nes.Nes(1, Set#{1, 2})))

    @Test
    def rawHeadInRest02(): Unit \ Assert = assertEq(expected = Set#{1, 2}, Nes.toSet(Nes.Nes(1, Set#{1, 2})))

    @Test
    def rawHeadInRest03(): Unit \ Assert = assertEq(expected = 2, Nes.count(_ -> true, Nes.Nes(1, Set#{1, 2})))

    @Test
    def rawHeadGreaterThanRest01(): Unit \ Assert = assertEq(expected = 1, Nes.minimum(Nes.Nes(2, Set#{1})))

    @Test
    def rawHeadGreaterThanRest02(): Unit \ Assert = assertEq(expected = 2, Nes.maximum(Nes.Nes(2, Set#{1})))

    @Test
    def rawHeadGreaterThanRest03(): Unit \ Assert = assertEq(expected = 1 :: 2 :: Nil, Nes.toList(Nes.Nes(2, Set#{1})))

    @Test
    def rawDifferentRepresentationsEq01(): Unit \ Assert = assertEq(expected = Nes.Nes(2, Set#{1}), Nes.Nes(1, Set#{2}))

    @Test
    def rawDifferentRepresentationsOrder01(): Unit \ Assert = assertEq(expected = Comparison.EqualTo, Nes.Nes(2, Set#{1}) <=> Nes.Nes(1, Set#{2}))

    @Test
    def rawDifferentRepresentationsHash01(): Unit \ Assert = assertEq(expected = hash(Nes.Nes(2, Set#{1})), hash(Nes.Nes(1, Set#{2})))

    @Test
    def rawExactlyOnceMap01(): Unit \ Assert = region rc {
        let calls = Ref.fresh(rc, 0);
        let mapped = Nes.map(x -> { Ref.put(Ref.get(calls) + 1, calls); x * 10 }, Nes.Nes(1, Set#{1, 2}));
        assertEq(expected = Set#{10, 20}, Nes.toSet(mapped));
        assertEq(expected = 2, Ref.get(calls))
    }

    @Test
    def rawExactlyOnceForEach01(): Unit \ Assert = region rc {
        let calls = Ref.fresh(rc, 0);
        Nes.forEach(_ -> Ref.put(Ref.get(calls) + 1, calls), Nes.Nes(1, Set#{1, 2}));
        assertEq(expected = 2, Ref.get(calls))
    }

    @Test
    def rawExactlyOnceFlatMap01(): Unit \ Assert = region rc {
        let calls = Ref.fresh(rc, 0);
        let result = Nes.flatMap(x -> { Ref.put(Ref.get(calls) + 1, calls); Nes.singleton(x * 10) }, Nes.Nes(1, Set#{1, 2}));
        assertEq(expected = Set#{10, 20}, Nes.toSet(result));
        assertEq(expected = 2, Ref.get(calls))
    }

}
