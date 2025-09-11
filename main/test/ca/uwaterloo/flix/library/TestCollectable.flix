mod TestCollectable {

    use Assert.assertEq;

    /////////////////////////////////////////////////////////////////////////////
    // Chain instance                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def collectChain01(): Unit \ Assert = region rc {
        let iter: Iterator[Int32, rc, rc] = Iterator.empty(rc);
        assertEq(expected = Chain.empty(), (Collectable.collect(iter) : Chain[Int32]))
    }

    @Test
    def collectChain02(): Unit \ Assert = region rc {
        let iter = Iterator.singleton(rc, 1);
        assertEq(expected = Chain.singleton(1), Collectable.collect(iter))
    }

    @Test
    def collectChain03(): Unit \ Assert = region rc {
        let iter = List.iterator(rc, List#{1, 2, 3});
        assertEq(expected = List.toChain(List#{1, 2, 3}), Collectable.collect(iter))
    }

    @Test
    def collectChain04(): Unit \ Assert = region rc {
        let iter = List.iterator(rc, List#{'C', 'B', 'A'});
        assertEq(expected = List.toChain(List#{'C', 'B', 'A'}), Collectable.collect(iter))
    }

    /////////////////////////////////////////////////////////////////////////////
    // List instance                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def collectList01(): Unit \ Assert = region rc {
        let iter: Iterator[Int32, rc, rc] = Iterator.empty(rc);
        assertEq(expected = Nil, (Collectable.collect(iter) : List[Int32]))
    }

    @Test
    def collectList02(): Unit \ Assert = region rc {
        let iter = Iterator.singleton(rc, 1);
        assertEq(expected = List#{1}, Collectable.collect(iter))
    }

    @Test
    def collectList03(): Unit \ Assert = region rc {
        let iter = List.iterator(rc, List#{1, 2, 3});
        assertEq(expected = List#{1, 2, 3}, Collectable.collect(iter))
    }

    @Test
    def collectList04(): Unit \ Assert = region rc {
        let iter = List.iterator(rc, List#{'C', 'B', 'A'});
        assertEq(expected = List#{'C', 'B', 'A'}, Collectable.collect(iter))
    }

    /////////////////////////////////////////////////////////////////////////////
    // Set instance                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def collectSet01(): Unit \ Assert = region rc {
        let iter: Iterator[Int32, rc, rc] = Iterator.empty(rc);
        assertEq(expected = Set.empty(), (Collectable.collect(iter) : Set[Int32]))
    }

    @Test
    def collectSet02(): Unit \ Assert = region rc {
        let iter = Iterator.singleton(rc, 1);
        assertEq(expected = Set#{1}, Collectable.collect(iter))
    }

    @Test
    def collectSet03(): Unit \ Assert = region rc {
        let iter = List.iterator(rc, List#{1, 2, 3});
        assertEq(expected = Set#{1, 2, 3}, Collectable.collect(iter))
    }

    @Test
    def collectSet04(): Unit \ Assert = region rc {
        let iter = List.iterator(rc, List#{'C', 'B', 'A'});
        assertEq(expected = Set#{'A', 'B', 'C'}, Collectable.collect(iter))
    }

    /////////////////////////////////////////////////////////////////////////////
    // Vector instance                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def collectVector01(): Unit \ Assert = region rc {
        let iter: Iterator[Int32, rc, rc] = Iterator.empty(rc);
        assertEq(expected = Vector#{}, (Collectable.collect(iter) : Vector[Int32]))
    }

    @Test
    def collectVector02(): Unit \ Assert = region rc {
        let iter = Iterator.singleton(rc, 1);
        assertEq(expected = Vector#{1}, Collectable.collect(iter))
    }

    @Test
    def collectVector03(): Unit \ Assert = region rc {
        let iter = List.iterator(rc, List#{1, 2, 3});
        assertEq(expected = Vector#{1, 2, 3}, Collectable.collect(iter))
    }

    @Test
    def collectVector04(): Unit \ Assert = region rc {
        let iter = List.iterator(rc, List#{'C', 'B', 'A'});
        assertEq(expected = Vector#{'C', 'B', 'A'}, Collectable.collect(iter))
    }

}