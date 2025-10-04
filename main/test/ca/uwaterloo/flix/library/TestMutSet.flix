mod TestMutSet {

    use Assert.{assertEq, assertTrue, assertFalse};

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toString01(): Unit \ Assert = region rc {
        let s: MutSet[Int32, rc] = MutSet.empty(rc);
        assertEq(expected = "MutSet#{}", MutSet.toString(s))
    }

    @Test
    def toString02(): Unit \ Assert = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(1, s);
        assertEq(expected = "MutSet#{1}", MutSet.toString(s))
    }

    @Test
    def toString03(): Unit \ Assert = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(1, s);
        MutSet.add(2, s);
        MutSet.add(3, s);
        MutSet.add(4, s);
        MutSet.add(5, s);
        assertEq(expected = "MutSet#{1, 2, 3, 4, 5}", MutSet.toString(s))
    }

    /////////////////////////////////////////////////////////////////////////////
    // sameElements                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sameElements01(): Unit \ Assert = region rc {
        let s1 = (MutSet.empty(rc): MutSet[Int32, _]);
        let s2 = (MutSet.empty(rc): MutSet[Int32, _]);
        assertTrue(MutSet.sameElements(s1, s2))
    }

    @Test
    def sameElements02(): Unit \ Assert = region rc {
        let s1 = MutSet.empty(rc);
        MutSet.add(1, s1);

        let s2 = MutSet.empty(rc);
        MutSet.add(1, s2);

        assertTrue(MutSet.sameElements(s1, s2))
    }

    @Test
    def sameElements03(): Unit \ Assert = region rc {
        let s1 = MutSet.empty(rc);
        MutSet.add(1, s1);

        let s2 = MutSet.empty(rc);
        MutSet.add(2, s2);

        assertFalse(MutSet.sameElements(s1, s2))
    }

    @Test
    def sameElements04(): Unit \ Assert = region rc {
        let s1 = MutSet.empty(rc);
        MutSet.add(1, s1);
        MutSet.add(2, s1);
        MutSet.add(3, s1);

        let s2 = MutSet.empty(rc);
        MutSet.add(2, s2);
        MutSet.add(3, s2);
        MutSet.add(1, s2);

        assertTrue(MutSet.sameElements(s1, s2))
    }

    @Test
    def sameElements05(): Unit \ Assert = region rc {
        let s1 = MutSet.empty(rc);
        MutSet.add("a", s1);
        MutSet.add("b", s1);
        MutSet.add("c", s1);

        let s2 = MutSet.empty(rc);
        MutSet.add("c", s2);
        MutSet.add("a", s2);
        MutSet.add("b", s2);

        assertTrue(MutSet.sameElements(s1, s2))
    }

    @Test
    def setSameElements06(): Unit \ Assert = region rc {
        let s1 = MutSet.empty(rc);
        MutSet.add("a", s1);
        MutSet.add("b", s1);
        MutSet.add("c", s1);

        let s2 = MutSet.empty(rc);
        MutSet.add("c", s2);
        MutSet.add("a", s2);

        assertFalse(MutSet.sameElements(s1, s2))
    }


    /////////////////////////////////////////////////////////////////////////////
    // addAll! and mulitple regions                                            //
    /////////////////////////////////////////////////////////////////////////////

    // This section of tests serve two purposes:
    //  1. to test `addAll`
    //  2. to test the use of mulitple regions.
    // Thus not all tests end with a simple comparison of two sets.

    @Test
    def addAll01(): Unit \ Assert = region rc {
        let s1 = (MutSet.empty(rc): MutSet[Int32, _]);
        let s2 = (MutSet.empty(rc): MutSet[Int32, _]);
        MutSet.addAll(MutSet.toSet(s1), s2);
        assertTrue(MutSet.isEmpty(s2))
    }

    @Test
    def addAll02(): Unit \ Assert = region rc {
        let s1 = MutSet.empty(rc);
        let s2 = MutSet.empty(rc);
        MutSet.add(1, s1);
        MutSet.add(1, s1);
        MutSet.addAll(MutSet.toSet(s1), s2);
        assertTrue(MutSet.sameElements(s1, s2))
    }

    @Test
    def addAll03(): Unit \ Assert = region rc {
        let s1 = MutSet.empty(rc);
        let s2 = MutSet.empty(rc);
        MutSet.add(1, s1);
        MutSet.add(2, s1);
        MutSet.addAll(MutSet.toSet(s1), s2);
        assertEq(expected = 3, MutSet.foldLeft((acc, a) -> acc + a, 0, s2))
    }

    @Test
    def addAll04(): Unit \ Assert = region rc {
        let s1 = MutSet.empty(rc);
        let s2 = MutSet.empty(rc);
        MutSet.add(1, s1);
        MutSet.add(2, s1);
        MutSet.add(3, s1);
        MutSet.addAll(MutSet.toSet(s1), s2);
        assertEq(expected = 6, MutSet.foldLeft((acc, a) -> acc + a, 0, s2))
    }

    @Test
    def addAll05(): Unit \ Assert = region rc1 {
        let s1 = MutSet.empty(rc1);
        MutSet.add("a", s1);
        MutSet.add("b", s1);
        MutSet.add("c", s1);

        region rc2 {
            let s2 = MutSet.empty(rc2);
            MutSet.addAll(MutSet.toSet(s1), s2);
            assertEq(expected = "a" :: "b" :: "c" :: Nil, MutSet.toList(s2))
        }
    }

    @Test
    def addAll06(): Unit \ Assert = region rc {
        let s1 = MutSet.empty(rc);
        MutSet.add("a", s1);
        MutSet.add("b", s1);
        MutSet.add("c", s1);

        region rc2 {
            let s2 = MutSet.empty(rc2);
            MutSet.addAll(MutSet.toSet(s1), s2);
            assertEq(expected = MutSet.toSet(s1), MutSet.toSet(s2))
            }
        }


    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def join01(): Unit \ Assert = region rc {
        let s: MutSet[Int32, _] = MutSet.empty(rc);
        assertEq(expected = "", MutSet.join(",", s))
    }

    @Test
    def join02(): Unit \ Assert = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(1, s);
        assertEq(expected = "1", MutSet.join(",", s))
    }

    @Test
    def join03(): Unit \ Assert = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(1, s);
        MutSet.add(2, s);
        MutSet.add(3, s);
        assertEq(expected = "1,2,3", MutSet.join(",", s))
    }

    @Test
    def join04(): Unit \ Assert = region rc {
        let s = MutSet.empty(rc);
        MutSet.add("1", s);
        MutSet.add("2", s);
        MutSet.add("3", s);
        assertEq(expected = "1,2,3", MutSet.join(",", s))
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRight01(): Unit \ Assert = region rc {
        assertEq(expected = 100, MutSet.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, MutSet.empty(rc)))
    }

    @Test
    def foldRight02(): Unit \ Assert = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(1, s);
        assertEq(expected = 198, MutSet.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, s))
    }

    @Test
    def foldRight03(): Unit \ Assert = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(2, s);
        MutSet.add(1, s);
        assertEq(expected = 194, MutSet.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, s))
    }

    @Test
    def foldRight04(): Unit \ Assert = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(3, s);
        MutSet.add(2, s);
        MutSet.add(1, s);
        assertEq(expected = 382, MutSet.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, s))
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRightWithCont01(): Unit \ Assert = region rc {
        assertEq(expected = 100, MutSet.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, MutSet.empty(rc)))
    }

    @Test
    def foldRightWithCont02(): Unit \ Assert = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(1, s);
        assertEq(expected = 198, MutSet.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, s))
    }

    @Test
    def foldRightWithCont03(): Unit \ Assert = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(2, s);
        MutSet.add(1, s);
        assertEq(expected = 194, MutSet.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, s))
    }

    @Test
    def foldRightWithCont04(): Unit \ Assert = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(3, s);
        MutSet.add(2, s);
        MutSet.add(1, s);
        assertEq(expected = 382, MutSet.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, s))
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldMap01(): Unit \ Assert = region rc {
        assertEq(expected = 0, MutSet.foldMap(x -> 2 * x, (MutSet.empty(rc): MutSet[Int32, _])))
    }

    @Test
    def foldMap02(): Unit \ Assert = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(1, s);
        MutSet.add(2, s);
        assertEq(expected = 6, MutSet.foldMap(x -> 2 * x, s))
    }

    @Test
    def foldMap03(): Unit \ Assert = region rc {
        let s = MutSet.empty(rc);
        MutSet.add("a", s);
        assertEq(expected = "b", MutSet.foldMap(x -> if (x == "a") "b" else x, s))
    }

    @Test
    def foldMap04(): Unit \ Assert = region rc {
        let s = MutSet.empty(rc);
        MutSet.add("a", s);
        MutSet.add("b", s);
        MutSet.add("c", s);
        assertEq(expected = "abb", MutSet.foldMap(x -> if (x == "c") "b" else x, s))
    }

    @Test
    def foldMap05(): Unit \ Assert = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(1, s);
        MutSet.add(2, s);
        MutSet.add(3, s);
        assertEq(expected = "123", MutSet.foldMap(Int32.toString, s))
    }


    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toArray01(): Unit \ Assert = region rc {
        let s: MutSet[Int32, _] = MutSet.empty(rc);
        let arr = MutSet.toArray(rc, s);
        assertEq(expected = 0, Array.length(arr))
    }

    @Test
    def toArray02(): Unit \ Assert = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(1, s);
        let arr = MutSet.toArray(rc, s);
        assertEq(expected = 1, Array.length(arr));
        assertEq(expected = 1, arr[0])
    }

    @Test
    def toArray03(): Unit \ Assert = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(1, s);
        MutSet.add(2, s);
        MutSet.add(3, s);
        let arr = MutSet.toArray(rc, s);
        assertEq(expected = 3, Array.length(arr));
        assertEq(expected = Set#{1, 2, 3}, Array.toSet(arr))
    }

    @Test
    def toArray04(): Unit \ Assert = region rc {
        let s = MutSet.empty(rc);
        MutSet.add("a", s);
        MutSet.add("b", s);
        MutSet.add("c", s);
        let arr = MutSet.toArray(rc, s);
        assertEq(expected = 3, Array.length(arr));
        assertEq(expected = Set#{"a", "b", "c"}, Array.toSet(arr))
    }


    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toVector01(): Unit \ Assert = region rc {
        let s: MutSet[Int32, _] = MutSet.empty(rc);
        let vec = MutSet.toVector(s);
        assertEq(expected = 0, Vector.length(vec))
    }

    @Test
    def toVector02(): Unit \ Assert = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(1, s);
        let vec = MutSet.toVector(s);
        assertEq(expected = 1, Vector.length(vec));
        assertEq(expected = 1, vec[0])
    }

    @Test
    def toVector03(): Unit \ Assert = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(1, s);
        MutSet.add(2, s);
        MutSet.add(3, s);
        let vec = MutSet.toVector(s);
        assertEq(expected = 3, Vector.length(vec));
        assertEq(expected = Set#{1, 2, 3}, Vector.toSet(vec))
    }

    @Test
    def toVector04(): Unit \ Assert = region rc {
        let s = MutSet.empty(rc);
        MutSet.add("a", s);
        MutSet.add("b", s);
        MutSet.add("c", s);
        let vec = MutSet.toVector(s);
        assertEq(expected = 3, Vector.length(vec));
        assertEq(expected = Set#{"a", "b", "c"}, Vector.toSet(vec))
    }


    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Unit \ Assert = region rc {
        let s = MutSet.empty(rc);
        let ri = Ref.fresh(rc, 21);
        MutSet.forEach(x -> Ref.put(x, ri), s);
        assertEq(expected = 21, Ref.get(ri))
    }

    @Test
    def forEach02(): Unit \ Assert = region rc {
        let s = MutSet.empty(rc);
        let ri = Ref.fresh(rc, 21);
        MutSet.add(0, s);
        MutSet.forEach(x -> Ref.put(x, ri), s);
        assertEq(expected = 0, Ref.get(ri))
    }

    @Test
    def forEach03(): Unit \ Assert = region rc {
        let s = MutSet.empty(rc);
        let ri = Ref.fresh(rc, 21);
        MutSet.add(0, s);
        MutSet.add(1, s);
        MutSet.add(2, s);
        MutSet.forEach(x -> Ref.put(x, ri), s);
        assertEq(expected = 2, Ref.get(ri))
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEachWithIndex01(): Unit \ Assert = region rc {
        let s = MutSet.empty(rc);
        let ri = Ref.fresh(rc, 21);
        MutSet.forEachWithIndex((i, _) -> Ref.put(i, ri), s);
        assertEq(expected = 21, Ref.get(ri))
    }

    @Test
    def forEachWithIndex02(): Unit \ Assert = region rc {
        let s = MutSet.empty(rc);
        let ri = Ref.fresh(rc, 21);
        MutSet.add(100, s);
        MutSet.forEachWithIndex((i, _) -> Ref.put(i, ri), s);
        assertEq(expected = 0, Ref.get(ri))
    }

    @Test
    def forEachWithIndex03(): Unit \ Assert = region rc {
        let s = MutSet.empty(rc);
        let ri = Ref.fresh(rc, 21);
        MutSet.add(100, s);
        MutSet.add(101, s);
        MutSet.add(102, s);
        MutSet.forEachWithIndex((i, _) -> Ref.put(i, ri), s);
        assertEq(expected = 2, Ref.get(ri))
    }

}

