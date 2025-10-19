mod TestMutMap {

    use Assert.{assertEq, assertTrue, assertFalse};

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toString01(): Unit \ Assert = region rc {
        let m: MutMap[Int32, Int32, _] = MutMap.empty(rc);
        assertEq(expected = "MutMap#{}", MutMap.toString(m))
    }

    @Test
    def toString02(): Unit \ Assert = region rc {
        let m = MutMap.empty(rc);
        MutMap.put(1, 101, m);
        assertEq(expected = "MutMap#{1 => 101}", MutMap.toString(m))
    }

    @Test
    def toString03(): Unit \ Assert = region rc {
        let m = MutMap.empty(rc);
        MutMap.put(1, 101, m);
        MutMap.put(2, 102, m);
        MutMap.put(3, 103, m);
        MutMap.put(4, 104, m);
        MutMap.put(5, 105, m);
        assertEq(expected = "MutMap#{1 => 101, 2 => 102, 3 => 103, 4 => 104, 5 => 105}", MutMap.toString(m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // sameElements                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sameElements01(): Unit \ Assert = region rc {
        let m1 = (MutMap.empty(rc): MutMap[Int32, Int32, _]);
        let m2 = (MutMap.empty(rc): MutMap[Int32, Int32, _]);
        assertTrue(MutMap.sameElements(m1, m2))
    }

    @Test
    def sameElements02(): Unit \ Assert = region rc {
        let m = MutMap.empty(rc);
        MutMap.put(1, "a", m);

        let m2 = MutMap.empty(rc);
        MutMap.put(1, "a", m2);

        assertTrue(MutMap.sameElements(m, m2))
    }

    @Test
    def sameElements03(): Unit \ Assert = region rc {
        let m = MutMap.empty(rc);
        MutMap.put(1, "a", m);
        MutMap.put(1, "b", m);

        let m2 = MutMap.empty(rc);
        MutMap.put(1, "a", m2);

        assertFalse(MutMap.sameElements(m, m2))
    }

    @Test
    def sameElements04(): Unit \ Assert = region rc {
        let m = MutMap.empty(rc);
        MutMap.put(1, "a", m);
        MutMap.put(2, "b", m);

        let m2 = MutMap.empty(rc);
        MutMap.put(1, "a", m2);

        assertFalse(MutMap.sameElements(m, m2))
    }

    @Test
    def sameElements05(): Unit \ Assert = region rc {
        let m = MutMap.empty(rc);
        MutMap.put(1, "a", m);

        let m2 = MutMap.empty(rc);
        MutMap.put(1, "a", m2);
        MutMap.put(2, "b", m2);

        assertFalse(MutMap.sameElements(m, m2))
    }

    @Test
    def sameElements06(): Unit \ Assert = region rc {
        let m = MutMap.empty(rc);
        MutMap.put(2, "b", m);
        MutMap.put(3, "c", m);
        MutMap.put(1, "a", m);

        let m2 = MutMap.empty(rc);
        MutMap.put(1, "a", m2);
        MutMap.put(2, "b", m2);
        MutMap.put(3, "c", m2);

        assertTrue(MutMap.sameElements(m, m2))
    }

    /////////////////////////////////////////////////////////////////////////////
    // mergeWithKey                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mergeWithKey01(): Unit \ Assert = region rc {
        let m1 = (MutMap.empty(rc): MutMap[Int32, Int32, _]);
        let m2 = (MutMap.empty(rc): MutMap[Int32, Int32, _]);
        MutMap.mergeWithKey((_, v, _) -> v, m1, m2);
        assertTrue(MutMap.isEmpty(m2))
    }

    @Test
    def mergeWithKey02(): Unit \ Assert = region rc {
        let m1 = MutMap.empty(rc);
        let m2 = MutMap.empty(rc);
        MutMap.put(1, 0, m1);
        MutMap.put(2, 0, m2);
        MutMap.mergeWithKey((_, v, _) -> v, m1, m2);
        assertTrue(MutMap.memberOf(1, m2));
        assertTrue(MutMap.memberOf(2, m2))
    }


    /////////////////////////////////////////////////////////////////////////////
    // joinKeys                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinKeys01(): Unit \ Assert = region rc {
        assertEq(expected = "", (MutMap.empty(rc): MutMap[Int32, Int32, _]) |> MutMap.joinKeys(","))
    }

    @Test
    def joinKeys02(): Unit \ Assert = region rc {
        assertEq(expected = "1", Map.toMutMap(rc, Map#{1 => 1}) |> MutMap.joinKeys(","))
    }

    @Test
    def joinKeys03(): Unit \ Assert = region rc {
        assertEq(expected = "0,1,2", Map.toMutMap(rc, Map#{0 => 1, 1 => 2, 2 => 2}) |> MutMap.joinKeys(","))
    }

    @Test
    def joinKeys04(): Unit \ Assert = region rc {
        assertEq(expected = "0,1,2", Map.toMutMap(rc, Map#{"0" => 1, "1" => 2, "2" => 2}) |> MutMap.joinKeys(","))
    }


    /////////////////////////////////////////////////////////////////////////////
    // joinValues                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinValues01(): Unit \ Assert = region rc {
        assertEq(expected = "", (MutMap.empty(rc): MutMap[Int32, Int32, _]) |> MutMap.joinValues(","))
    }

    @Test
    def joinValues02(): Unit \ Assert = region rc {
        assertEq(expected = "1", Map.toMutMap(rc, Map#{1 => 1}) |> MutMap.joinValues(","))
    }

    @Test
    def joinValues03(): Unit \ Assert = region rc {
        assertEq(expected = "1,2,2", Map.toMutMap(rc, Map#{0 => 1, 1 => 2, 2 => 2}) |> MutMap.joinValues(","))
    }

    @Test
    def joinValues04(): Unit \ Assert = region rc {
        assertEq(expected = "1,2,2", Map.toMutMap(rc, Map#{0 => "1", 1 => "2", 2 => "2"}) |> MutMap.joinValues(","))
    }


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinWith01(): Unit \ Assert = region rc {
        assertEq(expected = "", (MutMap.empty(rc): MutMap[Int32, Int32, _]) |>
            MutMap.joinWith((k, v) -> "${k} => ${v}", ","))
    }

    @Test
    def joinWith02(): Unit \ Assert = region rc {
        assertEq(expected = "1 => 1", Map.toMutMap(rc, Map#{1 => 1}) |>
            MutMap.joinWith((k, v) -> "${k} => ${v}", ", "))
    }

    @Test
    def joinWith03(): Unit \ Assert = region rc {
        assertEq(expected = "0 => 1, 1 => 2, 2 => 2", Map.toMutMap(rc, Map#{0 => 1, 1 => 2, 2 => 2}) |>
            MutMap.joinWith((k, v) -> "${k} => ${v}", ", "))
    }

    @Test
    def joinWith04(): Unit \ Assert = region rc {
        assertEq(expected = "0 => 1, 1 => 2, 2 => 2", Map.toMutMap(rc, Map#{0 => "1", 1 => "2", 2 => "2"}) |>
            MutMap.joinWith((k, v) -> "${k} => ${v}", ", "))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toList01(): Unit \ Assert = region rc {
        let m: MutMap[Int32, Int32, _] = MutMap.empty(rc);
        assertEq(expected = (List#{} : List[(Int32, Int32)]), MutMap.toList(m))
    }

    @Test
    def toList02(): Unit \ Assert = region rc {
        let m = Map.toMutMap(rc, Map#{1 => 2});
        assertEq(expected = List#{(1, 2)}, MutMap.toList(m))
    }

    @Test
    def toList03(): Unit \ Assert = region rc {
        let m = Map.toMutMap(rc, Map#{1 => 2, 3 => 4, 5 => 6});
        assertEq(expected = List#{(1, 2), (3, 4), (5, 6)}, MutMap.toList(m))
    }

    @Test
    def toList04(): Unit \ Assert = region rc {
        let m = Map.toMutMap(rc, Map#{1 => 'a', 2 => 'b', 3 => 'c'});
        assertEq(expected = List#{(1, 'a'), (2, 'b'), (3, 'c')}, MutMap.toList(m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toVector01(): Unit \ Assert = region rc {
        let m: MutMap[Int32, Int32, _] = MutMap.empty(rc);
        assertEq(expected = (Vector#{} : Vector[(Int32, Int32)]), MutMap.toVector(m))
    }

    @Test
    def toVector02(): Unit \ Assert = region rc {
        let m = Map.toMutMap(rc, Map#{1 => 2});
        assertEq(expected = Vector#{(1, 2)}, MutMap.toVector(m))
    }

    @Test
    def toVector03(): Unit \ Assert = region rc {
        let m = Map.toMutMap(rc, Map#{1 => 2, 3 => 4, 5 => 6});
        assertEq(expected = Vector#{(1, 2), (3, 4), (5, 6)}, MutMap.toVector(m))
    }

    @Test
    def toVector04(): Unit \ Assert = region rc {
        let m = Map.toMutMap(rc, Map#{1 => 'a', 2 => 'b', 3 => 'c'});
        assertEq(expected = Vector#{(1, 'a'), (2, 'b'), (3, 'c')}, MutMap.toVector(m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toArray01(): Unit \ Assert = region rc {
        let m: MutMap[Int32, Int32, _] = MutMap.empty(rc);
        assertTrue(Array.sameElements(MutMap.toArray(rc, m), Array#{} @ rc))
    }

    @Test
    def toArray02(): Unit \ Assert = region rc {
        let m = Map.toMutMap(rc, Map#{1 => 2});
        assertTrue(Array.sameElements(MutMap.toArray(rc, m), Array#{(1, 2)} @ rc))
    }

    @Test
    def toArray03(): Unit \ Assert = region rc {
        let m = Map.toMutMap(rc, Map#{1 => 2, 3 => 4, 5 => 6});
        assertTrue(Array.sameElements(MutMap.toArray(rc, m), Array#{(1, 2), (3, 4), (5, 6)} @ rc))
    }

    @Test
    def toArray04(): Unit \ Assert = region rc {
        let m = Map.toMutMap(rc, Map#{1 => 'a', 2 => 'b', 3 => 'c'});
        assertTrue(Array.sameElements(MutMap.toArray(rc, m), Array#{(1, 'a'), (2, 'b'), (3, 'c')} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toSet01(): Unit \ Assert = region rc {
        let m: MutMap[Int32, Int32, _] = MutMap.empty(rc);
        assertEq(expected = (Set#{} : Set[(Int32, Int32)]), MutMap.toSet(m))
    }

    @Test
    def toSet02(): Unit \ Assert = region rc {
        let m = Map.toMutMap(rc, Map#{1 => 2});
        assertEq(expected = Set#{(1, 2)}, MutMap.toSet(m))
    }

    @Test
    def toSet03(): Unit \ Assert = region rc {
        let m = Map.toMutMap(rc, Map#{1 => 2, 3 => 4, 5 => 6});
        assertEq(expected = Set#{(1, 2), (3, 4), (5, 6)}, MutMap.toSet(m))
    }

    @Test
    def toSet04(): Unit \ Assert = region rc {
        let m = Map.toMutMap(rc, Map#{1 => 'a', 2 => 'b', 3 => 'c'});
        assertEq(expected = Set#{(1, 'a'), (2, 'b'), (3, 'c')}, MutMap.toSet(m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Unit \ Assert = region rc {
        let m = MutMap.empty(rc);
        let ri = Ref.fresh(rc, 21);
        MutMap.forEach((k, v) -> Ref.put(k+v, ri), m);
        assertEq(expected = 21, Ref.get(ri))
    }

    @Test
    def forEach02(): Unit \ Assert = region rc {
        let m = MutMap.empty(rc);
        let ri = Ref.fresh(rc, 21);
        MutMap.put(0, 100, m);
        MutMap.forEach((k, v) -> Ref.put(k+v, ri), m);
        assertEq(expected = 100, Ref.get(ri))
    }

    @Test
    def forEach03(): Unit \ Assert = region rc {
        let m = MutMap.empty(rc);
        let ri = Ref.fresh(rc, 21);
        MutMap.put(0, 100, m);
        MutMap.put(1, 101, m);
        MutMap.put(2, 102, m);
        MutMap.forEach((k, v) -> Ref.put(k+v, ri), m);
        assertEq(expected = 104, Ref.get(ri))
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEachWithIndex01(): Unit \ Assert = region rc {
        let m = MutMap.empty(rc);
        let ri = Ref.fresh(rc, 21);
        MutMap.forEachWithIndex((i, _, _) -> Ref.put(i, ri), m);
        assertEq(expected = 21, Ref.get(ri))
    }

    @Test
    def forEachWithIndex02(): Unit \ Assert = region rc {
        let m = MutMap.empty(rc);
        let ri = Ref.fresh(rc, 21);
        MutMap.put(0, 100, m);
        MutMap.forEachWithIndex((i, _, _) -> Ref.put(i, ri), m);
        assertEq(expected = 0, Ref.get(ri))
    }

    @Test
    def forEachWithIndex03(): Unit \ Assert = region rc {
        let m = MutMap.empty(rc);
        let ri = Ref.fresh(rc, 21);
        MutMap.put(0, 100, m);
        MutMap.put(1, 101, m);
        MutMap.put(2, 102, m);
        MutMap.forEachWithIndex((i, _, _) -> Ref.put(i, ri), m);
        assertEq(expected = 2, Ref.get(ri))
    }

    /////////////////////////////////////////////////////////////////////////////
    // putWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def putWith01(): Unit \ Assert = region rc {
        let m = MutMap.empty(rc);
        MutMap.putWith((x, y) -> x + y, 1, 10, m);
        assertEq(expected = Some(10), MutMap.get(1, m))
    }

    @Test
    def putWith02(): Unit \ Assert = region rc {
        let m = MutMap.empty(rc);
        MutMap.put(1, 5, m);
        MutMap.putWith((x, y) -> x + y, 1, 10, m);
        assertEq(expected = Some(15), MutMap.get(1, m))
    }

    @Test
    def putWith03(): Unit \ Assert = region rc {
        let m = MutMap.empty(rc);
        MutMap.put(1, 5, m);
        MutMap.put(2, 10, m);
        MutMap.putWith((x, y) -> x + y, 1, 3, m);
        assertEq(expected = 2, MutMap.size(m));
        assertEq(expected = Some(8), MutMap.get(1, m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // putWithKey                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def putWithKey01(): Unit \ Assert = region rc {
        let m = MutMap.empty(rc);
        MutMap.putWithKey((k, x, y) -> k + x + y, 1, 10, m);
        // Key doesn't exist, so inserts 1 => 10 (function not applied)
        assertEq(expected = Some(10), MutMap.get(1, m))
    }

    @Test
    def putWithKey02(): Unit \ Assert = region rc {
        let m = MutMap.empty(rc);
        MutMap.put(1, 5, m);
        MutMap.putWithKey((k, x, y) -> k + x + y, 1, 10, m);
        assertEq(expected = Some(16), MutMap.get(1, m))
    }

    @Test
    def putWithKey03(): Unit \ Assert = region rc {
        let m = MutMap.empty(rc);
        MutMap.put(1, 5, m);
        MutMap.put(2, 10, m);
        MutMap.putWithKey((k, x, y) -> k + x + y, 2, 3, m);
        assertEq(expected = 2, MutMap.size(m));
        assertEq(expected = Some(15), MutMap.get(2, m))
    }

}

