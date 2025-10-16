mod TestDelayMap {

    use Assert.{assertEq, assertTrue};

    ///
    /// Returns `m` as a `DelayMap`.
    ///
    def toDelayMap(m: Map[k, v]): DelayMap[k, v] =
        let Map.Map(t) = m;
        let f = (_, v) -> lazy v;
        DelayMap.DMap(RedBlackTree.mapWithKey(f, t))

    /////////////////////////////////////////////////////////////////////////////
    // insertWith                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def insertWith01(): Unit \ Assert =
        assertEq(expected = (1, 3) :: Nil, DelayMap.insertWith((v1, v2) -> v1 + v2, 1, 3, DelayMap.empty()) |> DelayMap.toList)

    @Test
    def insertWith02(): Unit \ Assert =
        assertEq(expected = (1, 7) :: Nil, toDelayMap(List.toMap((1, 4) :: Nil)) |> DelayMap.insertWith((v1, v2) -> v1 + v2, 1, 3) |> DelayMap.toList)

    @Test
    def insertWith03(): Unit \ Assert =
        assertEq(expected = (1, 4) :: (2, 3) :: Nil, toDelayMap(List.toMap((1, 4) :: Nil)) |> DelayMap.insertWith((v1, v2) -> v1 + v2, 2, 3) |> DelayMap.toList)

    @Test
    def insertWith04(): Unit \ Assert =
        assertEq(expected = (1, 5) :: (5, -2) :: Nil, toDelayMap(List.toMap((1, 4) :: (5, -2) :: Nil)) |> DelayMap.insertWith((v1, v2) -> v1 + v2, 1, 1) |> DelayMap.toList)

    @Test
    def insertWith05(): Unit \ Assert =
        assertEq(expected = (1, 4) :: (5, -1) :: Nil, toDelayMap(List.toMap((1, 4) :: (5, -2) :: Nil)) |>
        DelayMap.insertWith((v1, v2) -> v1 + v2, 5, 1) |> DelayMap.toList)

    @Test
    def insertWith06(): Unit \ Assert =
        assertEq(expected = (1, 4) :: (4, -2) :: (5, -2):: Nil, toDelayMap(List.toMap((1, 4) :: (5, -2) :: Nil)) |>
        DelayMap.insertWith((v1, v2) -> v1 + v2, 4, -2) |>
        DelayMap.toList |> List.sortBy(t -> fst(t)))


    /////////////////////////////////////////////////////////////////////////////
    // insertWithKey insertWithKey                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def insertWithInsertWith01(): Unit \ Assert =
        assertEq(expected = ("a", 1) :: ("b", 4) :: Nil, (("a", 1) :: Nil)                                    |>
            List.toMap                                  |>
            toDelayMap                                  |>
            DelayMap.insertWith((v1, v2) -> v1 + v2, "b", 1) |>
            DelayMap.insertWith((v1, v2) -> v1 + v2, "b", 3) |>
            DelayMap.toList)

    @Test
    def insertWithInsertWith02(): Unit \ {Assert, IO} =
        assertEq(expected = ("a", 1) :: ("b", 4) :: Nil, (("a", 1) :: Nil)                                                        |>
            List.toMap                                                      |>
            toDelayMap                                                      |>
            DelayMap.insertWith((v1, v2) -> v1 + v2, unchecked_cast("b" as _ \ IO), 1) |>
            DelayMap.insertWith((v1, v2) -> v1 + v2, "b", 3)                     |>
            DelayMap.toList)
    @Test
    def insertWithInsertWith03(): Unit \ {Assert, IO} =
        assertEq(expected = ("a", 1) :: ("b", 4) :: Nil, (("a", 1) :: Nil)                                                        |>
            List.toMap                                                      |>
            toDelayMap                                                      |>
            DelayMap.insertWith((v1, v2) -> v1 + v2, "b", 1)                     |>
            DelayMap.insertWith((v1, v2) -> v1 + v2, unchecked_cast("b" as _ \ IO), 3) |>
            DelayMap.toList)

    @Test
    def insertWithInsertWith04(): Unit \ {Assert, IO} =
        assertEq(expected = ("a", 1) :: ("b", 4) :: Nil, (("a", 1) :: Nil)                                                        |>
            List.toMap                                                      |>
            toDelayMap                                                      |>
            DelayMap.insertWith((v1, v2) -> v1 + v2, unchecked_cast("b" as _ \ IO), 1) |>
            DelayMap.insertWith((v1, v2) -> v1 + v2, unchecked_cast("b" as _ \ IO), 3) |>
            DelayMap.toList)


    /////////////////////////////////////////////////////////////////////////////
    // insertWith insertWith fusion                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def insertWithFusion01(): Unit \ Assert = region rc {
        let l = Ref.fresh(rc, Nil);
        discard toDelayMap(List.toMap((1, 1) :: Nil)) |>
            DelayMap.insertWith((v, _) -> { Ref.put("a" :: Ref.get(l), l); v }, 1, 1) |>
            DelayMap.insertWith((v, _) -> { Ref.put("a" :: Ref.get(l), l); v }, 1, 1) |>
            DelayMap.insertWith((v, _) -> { Ref.put("a" :: Ref.get(l), l); v }, 1, 1) |>
            DelayMap.insertWith((v, _) -> { Ref.put("b" :: Ref.get(l), l); v }, 1, 1) |>
            DelayMap.insertWith((v, _) -> { Ref.put("b" :: Ref.get(l), l); v }, 1, 1) |>
            DelayMap.insertWith((v, _) -> { Ref.put("b" :: Ref.get(l), l); v }, 1, 1);
        assertEq(expected = "a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil, List.reverse(Ref.get(l)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def count01(): Unit \ Assert =
        let m = (Map#{}: Map[Unit, Unit]) |> toDelayMap;
        assertEq(expected = 0, DelayMap.count((k, v) -> k == v, m))

    @Test
    def count02(): Unit \ Assert =
        let m = Map#{1 => 2} |> toDelayMap;
        assertEq(expected = 0, DelayMap.count((k, v) -> k == v, m))

    @Test
    def count03(): Unit \ Assert =
        let m = Map#{1 => 1} |> toDelayMap;
        assertEq(expected = 1, DelayMap.count((k, v) -> k == v, m))

    @Test
    def count04(): Unit \ Assert =
        let m = Map#{1 => 2, 2 => 3} |> toDelayMap;
        assertEq(expected = 0, DelayMap.count((k, v) -> k == v, m))

    @Test
    def count05(): Unit \ Assert =
        let m = Map#{1 => 1, 2 => 3} |> toDelayMap;
        assertEq(expected = 1, DelayMap.count((k, v) -> k == v, m))

    @Test
    def count06(): Unit \ Assert =
        let m = Map#{1 => 2, 2 => 2} |> toDelayMap;
        assertEq(expected = 1, DelayMap.count((k, v) -> k == v, m))

    @Test
    def count07(): Unit \ Assert =
        let m = Map#{1 => 1, 2 => 2} |> toDelayMap;
        assertEq(expected = 2, DelayMap.count((k, v) -> k == v, m))

    @Test
    def count08(): Unit \ Assert = region rc {
        let range = List.range(0, 2000);
        let m = List.zip(range, range) |> List.toMap |> toDelayMap;
        let a = MutList.empty(rc);
        discard DelayMap.count((k, _) -> { let b = MutList.empty(rc); MutList.push(k, b); MutList.append(MutList.toList(b), a); true }, m);
        assertEq(expected = range, MutList.toList(a))
    }


    /////////////////////////////////////////////////////////////////////////////
    // map (pure)                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapPure01(): Unit \ Assert =
        assertEq(expected = Map#{}, (Map#{}: Map[Unit, Int32]) |> toDelayMap |> DelayMap.map(v -> 3 * v) |> DelayMap.toMap)

    @Test
    def mapPure02(): Unit \ Assert =
        assertEq(expected = Map#{1 => 12}, Map#{1 => 4} |> toDelayMap |> DelayMap.map(v -> 3 * v) |> DelayMap.toMap)

    @Test
    def mapPure03(): Unit \ Assert =
        assertEq(expected = Map#{2 => -3, 0 => 0}, Map#{2 => -1, 0 => 0} |> toDelayMap |>
            DelayMap.map(v -> 3 * v) |> DelayMap.toMap)

    @Test
    def mapPure04(): Unit \ Assert =
        assertEq(expected = Map#{2 => -3, 5 => 45, 11 => -27}, Map#{2 => -1, 5 => 15, 11 => -9} |> toDelayMap |>
            DelayMap.map(v -> 3 * v) |> DelayMap.toMap)

    @Test
    def mapPure05(): Unit \ Assert =
        assertEq(expected = Map#{2 => -3, 5 => 45, 11 => -27, 8 => 24}, Map#{2 => -1, 5 => 15, 11 => -9, 8 => 8} |> toDelayMap |>
            DelayMap.map(v -> 3 * v) |> DelayMap.toMap)


    /////////////////////////////////////////////////////////////////////////////
    // map (impure)                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapImpure01(): Unit \ {Assert, IO} =
        assertEq(expected = Map#{}, (Map#{}: Map[Unit, Int32]) |> toDelayMap |>
            DelayMap.map(v -> unchecked_cast(3 * v as _ \ IO)) |> DelayMap.toMap)

    @Test
    def mapImpure02(): Unit \ {Assert, IO} =
        assertEq(expected = Map#{1 => 12}, Map#{1 => 4} |> toDelayMap |>
            DelayMap.map(v -> unchecked_cast(3 * v as _ \ IO)) |> DelayMap.toMap)

    @Test
    def mapImpure03(): Unit \ {Assert, IO} =
        assertEq(expected = Map#{2 => -3, 0 => 0}, Map#{2 => -1, 0 => 0} |> toDelayMap |>
            DelayMap.map(v -> unchecked_cast(3 * v as _ \ IO)) |> DelayMap.toMap)

    @Test
    def mapImpure04(): Unit \ {Assert, IO} =
        assertEq(expected = Map#{2 => -3, 5 => 45, 11 => -27}, Map#{2 => -1, 5 => 15, 11 => -9} |> toDelayMap |>
            DelayMap.map(v -> unchecked_cast(3 * v as _ \ IO)) |> DelayMap.toMap)

    @Test
    def mapImpure05(): Unit \ {Assert, IO} =
        assertEq(expected = Map#{2 => -3, 5 => 45, 11 => -27, 8 => 24}, Map#{2 => -1, 5 => 15, 11 => -9, 8 => 8} |> toDelayMap |>
            DelayMap.map(v -> unchecked_cast(3 * v as _ \ IO)) |> DelayMap.toMap)


    /////////////////////////////////////////////////////////////////////////////
    // map map                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapMap01(): Unit \ Assert =
        assertEq(expected = Map#{2 => -9, 5 => 135, 11 => -81}, Map#{2 => -1, 5 => 15, 11 => -9} |>
            toDelayMap |>
            DelayMap.map(v -> 3 * v) |>
            DelayMap.map(v -> 3 * v) |>
            DelayMap.toMap)

    @Test
    def mapMap02(): Unit \ {Assert, IO} =
        assertEq(expected = Map#{2 => -9, 5 => 135, 11 => -81}, Map#{2 => -1, 5 => 15, 11 => -9} |>
            toDelayMap |>
            DelayMap.map(v -> unchecked_cast(3 * v as _ \ IO)) |>
            DelayMap.map(v -> 3 * v) |>
            DelayMap.toMap)

    @Test
    def mapMap03(): Unit \ {Assert, IO} =
        assertEq(expected = Map#{2 => -9, 5 => 135, 11 => -81}, Map#{2 => -1, 5 => 15, 11 => -9} |>
            toDelayMap |>
            DelayMap.map(v -> 3 * v) |>
            DelayMap.map(v -> unchecked_cast(3 * v as _ \ IO)) |>
            DelayMap.toMap)

    @Test
    def mapMap04(): Unit \ {Assert, IO} =
        assertEq(expected = Map#{2 => -9, 5 => 135, 11 => -81}, Map#{2 => -1, 5 => 15, 11 => -9} |>
            toDelayMap |>
            DelayMap.map(v -> unchecked_cast(3 * v as _ \ IO)) |>
            DelayMap.map(v -> unchecked_cast(3 * v as _ \ IO)) |>
            DelayMap.toMap)


    /////////////////////////////////////////////////////////////////////////////
    // map (fusion)                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapFusion01(): Unit \ Assert = region rc {
        let l = Ref.fresh(rc, Nil);
        discard ((1, 1) :: (2, 2) :: (3, 3) :: Nil) |> List.toMap |> toDelayMap |>
            DelayMap.map(v -> { Ref.put("a" :: Ref.get(l), l); v }) |>
            DelayMap.map(v -> { Ref.put("b" :: Ref.get(l), l); v });
        assertEq(expected = "a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil, List.reverse(Ref.get(l)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // mapWithKey                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapWithKey01(): Unit \ Assert =
        assertEq(expected = Map#{}, (Map#{}: Map[Unit, Unit]) |> toDelayMap |>
            DelayMap.mapWithKey((_, v) -> v) |> DelayMap.toMap)

    @Test
    def mapWithKey02(): Unit \ Assert =
        assertEq(expected = Map#{1 => 5}, Map#{1 => 4} |> toDelayMap |>
            DelayMap.mapWithKey((k, v) -> k + v) |> DelayMap.toMap)

    @Test
    def mapWithKey03(): Unit \ Assert =
        assertEq(expected = Map#{2 => 1, 0 => 0}, Map#{2 => -1, 0 => 0} |> toDelayMap |>
            DelayMap.mapWithKey((k, v) -> k + v) |> DelayMap.toMap)

    @Test
    def mapWithKey04(): Unit \ Assert =
        assertEq(expected = Map#{2 => 1, 5 => 20, 11 => 2}, Map#{2 => -1, 5 => 15, 11 => -9} |> toDelayMap |>
            DelayMap.mapWithKey((k, v) -> k + v) |> DelayMap.toMap)

    @Test
    def mapWithKey05(): Unit \ Assert =
        assertEq(expected = Map#{2 => 1, 5 => 20, 11 => 2, 8 => 16}, Map#{2 => -1, 5 => 15, 11 => -9, 8 => 8} |> toDelayMap |>
            DelayMap.mapWithKey((k, v) -> k + v) |> DelayMap.toMap)


    /////////////////////////////////////////////////////////////////////////////
    // union                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def union01(): Unit \ Assert =
        let m1 = toDelayMap((Map#{}: Map[Unit, Unit]));
        let m2 = toDelayMap(Map#{});
        assertEq(expected = Map#{}, DelayMap.union(m1, m2) |> DelayMap.toMap)

    @Test
    def union02(): Unit \ Assert =
        let m1 = toDelayMap(Map#{1 => 2});
        let m2 = toDelayMap(Map#{});
        assertEq(expected = Map#{1 => 2}, DelayMap.union(m1, m2) |> DelayMap.toMap)

    @Test
    def union03(): Unit \ Assert =
        let m1 = toDelayMap(Map#{});
        let m2 = toDelayMap(Map#{1 => 2});
        assertEq(expected = Map#{1 => 2}, DelayMap.union(m1, m2) |> DelayMap.toMap)

    @Test
    def union04(): Unit \ Assert =
        let m1 = toDelayMap(Map#{});
        let m2 = toDelayMap(Map#{1 => 2, 3 => 4});
        assertEq(expected = Map#{1 => 2, 3 => 4}, DelayMap.union(m1, m2) |> DelayMap.toMap)

    @Test
    def union05(): Unit \ Assert =
        let m1 = toDelayMap(Map#{1 => 2, 3 => 4});
        let m2 = toDelayMap(Map#{});
        assertEq(expected = Map#{1 => 2, 3 => 4}, DelayMap.union(m1, m2) |> DelayMap.toMap)

    @Test
    def union06(): Unit \ Assert =
        let m1 = toDelayMap(Map#{1 => 2});
        let m2 = toDelayMap(Map#{3 => 4});
        assertEq(expected = Map#{1 => 2, 3 => 4}, DelayMap.union(m1, m2) |> DelayMap.toMap)

    @Test
    def union07(): Unit \ Assert =
        let m1 = toDelayMap(Map#{1 => 2});
        let m2 = toDelayMap(Map#{1 => 5});
        assertEq(expected = Map#{1 => 2}, DelayMap.union(m1, m2) |> DelayMap.toMap)

    @Test
    def union08(): Unit \ Assert =
        let m1 = toDelayMap(Map#{});
        let m2 = toDelayMap(Map#{1 => 2, 2 => 3, 3 => 4});
        assertEq(expected = Map#{1 => 2, 2 => 3, 3 => 4}, DelayMap.union(m1, m2) |> DelayMap.toMap)

    @Test
    def union09(): Unit \ Assert =
        let m1 = toDelayMap(Map#{1 => 2, 2 => 3, 3 => 4});
        let m2 = toDelayMap(Map#{});
        assertEq(expected = Map#{1 => 2, 2 => 3, 3 => 4}, DelayMap.union(m1, m2) |> DelayMap.toMap)

    @Test
    def union10(): Unit \ Assert =
        let m1 = toDelayMap(Map#{1 => 2, 2 => 3});
        let m2 = toDelayMap(Map#{3 => 4});
        assertEq(expected = Map#{1 => 2, 2 => 3, 3 => 4}, DelayMap.union(m1, m2) |> DelayMap.toMap)

    @Test
    def union11(): Unit \ Assert =
        let m1 = toDelayMap(Map#{3 => 4});
        let m2 = toDelayMap(Map#{1 => 2, 2 => 3});
        assertEq(expected = Map#{1 => 2, 2 => 3, 3 => 4}, DelayMap.union(m1, m2) |> DelayMap.toMap)

    @Test
    def union12(): Unit \ Assert =
        let m1 = toDelayMap(Map#{1 => 2, 2 => 3});
        let m2 = toDelayMap(Map#{2 => 8});
        assertEq(expected = Map#{1 => 2, 2 => 3}, DelayMap.union(m1, m2) |> DelayMap.toMap)

    @Test
    def union13(): Unit \ Assert =
        let m1 = toDelayMap(Map#{1 => 2, 2 => 3});
        let m2 = toDelayMap(Map#{1 => 8});
        assertEq(expected = Map#{1 => 2, 2 => 3}, DelayMap.union(m1, m2) |> DelayMap.toMap)

    @Test
    def union14(): Unit \ Assert =
        let m1 = toDelayMap(Map#{1 => 4});
        let m2 = toDelayMap(Map#{1 => 2, 2 => 3});
        assertEq(expected = Map#{1 => 4, 2 => 3}, DelayMap.union(m1, m2) |> DelayMap.toMap)

    @Test
    def union15(): Unit \ Assert =
        let m1 = toDelayMap(Map#{2 => 7});
        let m2 = toDelayMap(Map#{1 => 2, 2 => 3});
        assertEq(expected = Map#{1 => 2, 2 => 7}, DelayMap.union(m1, m2) |> DelayMap.toMap)

    @Test
    def union16(): Unit \ Assert =
        let m1 = toDelayMap(Map#{2 => 7, 11 => 14, 9 => 8, 15 => 22});
        let m2 = toDelayMap(Map#{15 => 21, 1 => 2, 2 => 8, 44 => 33});
        assertEq(expected = Map#{2 => 7, 11 => 14, 9 => 8, 15 => 22, 1 => 2, 44 => 33}, DelayMap.union(m1, m2) |> DelayMap.toMap)


    /////////////////////////////////////////////////////////////////////////////
    // unionWith                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unionWith01(): Unit \ Assert =
        let m1 = toDelayMap(Map#{});
        let m2 = toDelayMap(Map#{});
        assertEq(expected = (Map#{}: Map[Unit, Int32]), DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap)

    @Test
    def unionWith02(): Unit \ Assert =
        let m1 = toDelayMap(Map#{1 => 2});
        let m2 = toDelayMap(Map#{});
        assertEq(expected = Map#{1 => 2}, DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap)

    @Test
    def unionWith03(): Unit \ Assert =
        let m1 = toDelayMap(Map#{});
        let m2 = toDelayMap(Map#{1 => 2});
        assertEq(expected = Map#{1 => 2}, DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap)

    @Test
    def unionWith04(): Unit \ Assert =
        let m1 = toDelayMap(Map#{});
        let m2 = toDelayMap(Map#{1 => 2, 3 => 4});
        assertEq(expected = Map#{1 => 2, 3 => 4}, DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap)

    @Test
    def unionWith05(): Unit \ Assert =
        let m1 = toDelayMap(Map#{1 => 2, 3 => 4});
        let m2 = toDelayMap(Map#{});
        assertEq(expected = Map#{1 => 2, 3 => 4}, DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap)

    @Test
    def unionWith06(): Unit \ Assert =
        let m1 = toDelayMap(Map#{1 => 2});
        let m2 = toDelayMap(Map#{3 => 4});
        assertEq(expected = Map#{1 => 2, 3 => 4}, DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap)

    @Test
    def unionWith07(): Unit \ Assert =
        let m1 = toDelayMap(Map#{1 => 2});
        let m2 = toDelayMap(Map#{1 => 5});
        assertEq(expected = Map#{1 => -3}, DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap)

    @Test
    def unionWith08(): Unit \ Assert =
        let m1 = toDelayMap(Map#{});
        let m2 = toDelayMap(Map#{1 => 2, 2 => 3, 3 => 4});
        assertEq(expected = Map#{1 => 2, 2 => 3, 3 => 4}, DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap)

    @Test
    def unionWith09(): Unit \ Assert =
        let m1 = toDelayMap(Map#{1 => 2, 2 => 3, 3 => 4});
        let m2 = toDelayMap(Map#{});
        assertEq(expected = Map#{1 => 2, 2 => 3, 3 => 4}, DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap)

    @Test
    def unionWith10(): Unit \ Assert =
        let m1 = toDelayMap(Map#{1 => 2, 2 => 3});
        let m2 = toDelayMap(Map#{3 => 4});
        assertEq(expected = Map#{1 => 2, 2 => 3, 3 => 4}, DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap)

    @Test
    def unionWith11(): Unit \ Assert =
        let m1 = toDelayMap(Map#{3 => 4});
        let m2 = toDelayMap(Map#{1 => 2, 2 => 3});
        assertEq(expected = Map#{1 => 2, 2 => 3, 3 => 4}, DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap)

    @Test
    def unionWith12(): Unit \ Assert =
        let m1 = toDelayMap(Map#{1 => 2, 2 => 3});
        let m2 = toDelayMap(Map#{2 => 8});
        assertEq(expected = Map#{1 => 2, 2 => -5}, DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap)

    @Test
    def unionWith13(): Unit \ Assert =
        let m1 = toDelayMap(Map#{1 => 2, 2 => 3});
        let m2 = toDelayMap(Map#{1 => 8});
        assertEq(expected = Map#{1 => -6, 2 => 3}, DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap)

    @Test
    def unionWith14(): Unit \ Assert =
        let m1 = toDelayMap(Map#{1 => 4});
        let m2 = toDelayMap(Map#{1 => 2, 2 => 3});
        assertEq(expected = Map#{1 => 2, 2 => 3}, DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap)

    @Test
    def unionWith15(): Unit \ Assert =
        let m1 = toDelayMap(Map#{2 => 7});
        let m2 = toDelayMap(Map#{1 => 2, 2 => 3});
        assertEq(expected = Map#{1 => 2, 2 => 4}, DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap)

    @Test
    def unionWith16(): Unit \ Assert =
        let m1 = toDelayMap(Map#{2 => 7, 11 => 14, 9 => 8, 15 => 22});
        let m2 = toDelayMap(Map#{15 => 21, 1 => 2, 2 => 8, 44 => 33});
        assertEq(expected = Map#{2 => -1, 11 => 14, 9 => 8, 15 => 1, 1 => 2, 44 => 33}, DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap)


    /////////////////////////////////////////////////////////////////////////////
    // unionWithKey (pure)                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unionWithKeyPure01(): Unit \ Assert =
        let m1 = toDelayMap(Map#{});
        let m2 = toDelayMap(Map#{});
        assertEq(expected = (Map#{}: Map[Int32, Int32]), DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap)

    @Test
    def unionWithKeyPure02(): Unit \ Assert =
        let m1 = toDelayMap(Map#{1 => 2});
        let m2 = toDelayMap(Map#{});
        assertEq(expected = Map#{1 => 2}, DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap)

    @Test
    def unionWithKeyPure03(): Unit \ Assert =
        let m1 = toDelayMap(Map#{});
        let m2 = toDelayMap(Map#{1 => 2});
        assertEq(expected = Map#{1 => 2}, DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap)

    @Test
    def unionWithKeyPure04(): Unit \ Assert =
        let m1 = toDelayMap(Map#{});
        let m2 = toDelayMap(Map#{1 => 2, 3 => 4});
        assertEq(expected = Map#{1 => 2, 3 => 4}, DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap)

    @Test
    def unionWithKeyPure05(): Unit \ Assert =
        let m1 = toDelayMap(Map#{1 => 2, 3 => 4});
        let m2 = toDelayMap(Map#{});
        assertEq(expected = Map#{1 => 2, 3 => 4}, DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap)

    @Test
    def unionWithKeyPure06(): Unit \ Assert =
        let m1 = toDelayMap(Map#{1 => 2});
        let m2 = toDelayMap(Map#{3 => 4});
        assertEq(expected = Map#{1 => 2, 3 => 4}, DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap)

    @Test
    def unionWithKeyPure07(): Unit \ Assert =
        let m1 = toDelayMap(Map#{1 => 2});
        let m2 = toDelayMap(Map#{1 => 5});
        assertEq(expected = Map#{1 => -2}, DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap)

    @Test
    def unionWithKeyPure08(): Unit \ Assert =
        let m1 = toDelayMap(Map#{});
        let m2 = toDelayMap(Map#{1 => 2, 2 => 3, 3 => 4});
        assertEq(expected = Map#{1 => 2, 2 => 3, 3 => 4}, DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap)

    @Test
    def unionWithKeyPure09(): Unit \ Assert =
        let m1 = toDelayMap(Map#{1 => 2, 2 => 3, 3 => 4});
        let m2 = toDelayMap(Map#{});
        assertEq(expected = Map#{1 => 2, 2 => 3, 3 => 4}, DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap)

    @Test
    def unionWithKeyPure10(): Unit \ Assert =
        let m1 = toDelayMap(Map#{1 => 2, 2 => 3});
        let m2 = toDelayMap(Map#{3 => 4});
        assertEq(expected = Map#{1 => 2, 2 => 3, 3 => 4}, DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap)

    @Test
    def unionWithKeyPure11(): Unit \ Assert =
        let m1 = toDelayMap(Map#{3 => 4});
        let m2 = toDelayMap(Map#{1 => 2, 2 => 3});
        assertEq(expected = Map#{1 => 2, 2 => 3, 3 => 4}, DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap)

    @Test
    def unionWithKeyPure12(): Unit \ Assert =
        let m1 = toDelayMap(Map#{1 => 2, 2 => 3});
        let m2 = toDelayMap(Map#{2 => 8});
        assertEq(expected = Map#{1 => 2, 2 => -3}, DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap)

    @Test
    def unionWithKeyPure13(): Unit \ Assert =
        let m1 = toDelayMap(Map#{1 => 2, 2 => 3});
        let m2 = toDelayMap(Map#{1 => 8});
        assertEq(expected = Map#{1 => -5, 2 => 3}, DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap)

    @Test
    def unionWithKeyPure14(): Unit \ Assert =
        let m1 = toDelayMap(Map#{1 => 4});
        let m2 = toDelayMap(Map#{1 => 2, 2 => 3});
        assertEq(expected = Map#{1 => 3, 2 => 3}, DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap)

    @Test
    def unionWithKeyPure15(): Unit \ Assert =
        let m1 = toDelayMap(Map#{2 => 7});
        let m2 = toDelayMap(Map#{1 => 2, 2 => 3});
        assertEq(expected = Map#{1 => 2, 2 => 6}, DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap)

    @Test
    def unionWithKeyPure16(): Unit \ Assert =
        let m1 = toDelayMap(Map#{2 => 7, 11 => 14, 9 => 8, 15 => 22});
        let m2 = toDelayMap(Map#{15 => 21, 1 => 2, 2 => 8, 44 => 33});
        assertEq(expected = Map#{2 => 1, 11 => 14, 9 => 8, 15 => 16, 1 => 2, 44 => 33}, DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap)


    /////////////////////////////////////////////////////////////////////////////
    // unionWithKey (impure)                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unionWithKeyImpure01(): Unit \ {Assert, IO} =
        let m1 = toDelayMap(Map#{});
        let m2 = toDelayMap(Map#{});
        assertEq(expected = (Map#{}: Map[Int32, Int32]), DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), m1, m2) |> DelayMap.toMap)

    @Test
    def unionWithKeyImpure02(): Unit \ {Assert, IO} =
        let m1 = toDelayMap(Map#{1 => 2});
        let m2 = toDelayMap(Map#{});
        assertEq(expected = Map#{1 => 2}, DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), m1, m2) |> DelayMap.toMap)

    @Test
    def unionWithKeyImpure03(): Unit \ {Assert, IO} =
        let m1 = toDelayMap(Map#{});
        let m2 = toDelayMap(Map#{1 => 2});
        assertEq(expected = Map#{1 => 2}, DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), m1, m2) |> DelayMap.toMap)

    @Test
    def unionWithKeyImpure04(): Unit \ {Assert, IO} =
        let m1 = toDelayMap(Map#{});
        let m2 = toDelayMap(Map#{1 => 2, 3 => 4});
        assertEq(expected = Map#{1 => 2, 3 => 4}, DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), m1, m2) |> DelayMap.toMap)

    @Test
    def unionWithKeyImpure05(): Unit \ {Assert, IO} =
        let m1 = toDelayMap(Map#{1 => 2, 3 => 4});
        let m2 = toDelayMap(Map#{});
        assertEq(expected = Map#{1 => 2, 3 => 4}, DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), m1, m2) |> DelayMap.toMap)

    @Test
    def unionWithKeyImpure06(): Unit \ {Assert, IO} =
        let m1 = toDelayMap(Map#{1 => 2});
        let m2 = toDelayMap(Map#{3 => 4});
        assertEq(expected = Map#{1 => 2, 3 => 4}, DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), m1, m2) |> DelayMap.toMap)

    @Test
    def unionWithKeyImpure07(): Unit \ {Assert, IO} =
        let m1 = toDelayMap(Map#{1 => 2});
        let m2 = toDelayMap(Map#{1 => 5});
        assertEq(expected = Map#{1 => -2}, DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), m1, m2) |> DelayMap.toMap)

    @Test
    def unionWithKeyImpure08(): Unit \ {Assert, IO} =
        let m1 = toDelayMap(Map#{});
        let m2 = toDelayMap(Map#{1 => 2, 2 => 3, 3 => 4});
        assertEq(expected = Map#{1 => 2, 2 => 3, 3 => 4}, DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), m1, m2) |> DelayMap.toMap)

    @Test
    def unionWithKeyImpure09(): Unit \ {Assert, IO} =
        let m1 = toDelayMap(Map#{1 => 2, 2 => 3, 3 => 4});
        let m2 = toDelayMap(Map#{});
        assertEq(expected = Map#{1 => 2, 2 => 3, 3 => 4}, DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), m1, m2) |> DelayMap.toMap)

    @Test
    def unionWithKeyImpure10(): Unit \ {Assert, IO} =
        let m1 = toDelayMap(Map#{1 => 2, 2 => 3});
        let m2 = toDelayMap(Map#{3 => 4});
        assertEq(expected = Map#{1 => 2, 2 => 3, 3 => 4}, DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), m1, m2) |> DelayMap.toMap)

    @Test
    def unionWithKeyImpure11(): Unit \ {Assert, IO} =
        let m1 = toDelayMap(Map#{3 => 4});
        let m2 = toDelayMap(Map#{1 => 2, 2 => 3});
        assertEq(expected = Map#{1 => 2, 2 => 3, 3 => 4}, DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), m1, m2) |> DelayMap.toMap)

    @Test
    def unionWithKeyImpure12(): Unit \ {Assert, IO} =
        let m1 = toDelayMap(Map#{1 => 2, 2 => 3});
        let m2 = toDelayMap(Map#{2 => 8});
        assertEq(expected = Map#{1 => 2, 2 => -3}, DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), m1, m2) |> DelayMap.toMap)

    @Test
    def unionWithKeyImpure13(): Unit \ {Assert, IO} =
        let m1 = toDelayMap(Map#{1 => 2, 2 => 3});
        let m2 = toDelayMap(Map#{1 => 8});
        assertEq(expected = Map#{1 => -5, 2 => 3}, DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), m1, m2) |> DelayMap.toMap)

    @Test
    def unionWithKeyImpure14(): Unit \ {Assert, IO} =
        let m1 = toDelayMap(Map#{1 => 4});
        let m2 = toDelayMap(Map#{1 => 2, 2 => 3});
        assertEq(expected = Map#{1 => 3, 2 => 3}, DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), m1, m2) |> DelayMap.toMap)

    @Test
    def unionWithKeyImpure15(): Unit \ {Assert, IO} =
        let m1 = toDelayMap(Map#{2 => 7});
        let m2 = toDelayMap(Map#{1 => 2, 2 => 3});
        assertEq(expected = Map#{1 => 2, 2 => 6}, DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), m1, m2) |> DelayMap.toMap)

    @Test
    def unionWithKeyImpure16(): Unit \ {Assert, IO} =
        let m1 = toDelayMap(Map#{2 => 7, 11 => 14, 9 => 8, 15 => 22});
        let m2 = toDelayMap(Map#{15 => 21, 1 => 2, 2 => 8, 44 => 33});
        assertEq(expected = Map#{2 => 1, 11 => 14, 9 => 8, 15 => 16, 1 => 2, 44 => 33}, DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), m1, m2) |> DelayMap.toMap)


    /////////////////////////////////////////////////////////////////////////////
    // unionWithKey unionWithKey                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unionWithKeyUnionWithKey01(): Unit \ Assert =
        assertEq(expected = Map#{1 => 6}, toDelayMap(Map#{1 => 5}) |>
            DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, toDelayMap(Map#{1 => 2})) |>
            DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, toDelayMap(Map#{1 => 3})) |>
            DelayMap.toMap)

    @Test
    def unionWithKeyUnionWithKey02(): Unit \ {Assert, IO} =
        assertEq(expected = Map#{1 => 6}, toDelayMap(Map#{1 => 5}) |>
            DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), toDelayMap(Map#{1 => 2})) |>
            DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, toDelayMap(Map#{1 => 3})) |>
            DelayMap.toMap)

    @Test
    def unionWithKeyUnionWithKey03(): Unit \ {Assert, IO} =
        assertEq(expected = Map#{1 => 6}, toDelayMap(Map#{1 => 5}) |>
            DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, toDelayMap(Map#{1 => 2})) |>
            DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), toDelayMap(Map#{1 => 3})) |>
            DelayMap.toMap)

    @Test
    def unionWithKeyUnionWithKey04(): Unit \ {Assert, IO} =
        assertEq(expected = Map#{1 => 6}, toDelayMap(Map#{1 => 5}) |>
            DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), toDelayMap(Map#{1 => 2})) |>
            DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), toDelayMap(Map#{1 => 3})) |>
            DelayMap.toMap)


    /////////////////////////////////////////////////////////////////////////////
    // unionWithKey (fusion)                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unionWithKeyFusion01(): Unit \ Assert = region rc {
        let l = Ref.fresh(rc, Nil);
        discard Map#{1 => 1, 2 => 2, 3 => 3} |> toDelayMap |>
            DelayMap.unionWithKey((_, _, v) -> { Ref.put("a" :: Ref.get(l), l); v }, toDelayMap(Map#{1 => 1, 2 => 2, 3 => 3})) |>
            DelayMap.unionWithKey((_, _, v) -> { Ref.put("b" :: Ref.get(l), l); v }, toDelayMap(Map#{1 => 1, 2 => 2, 3 => 3}));
        assertEq(expected = ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil), List.reverse(Ref.get(l)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // adjust                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def adjust01(): Unit \ Assert =
        let m = toDelayMap(Map#{});
        assertEq(expected = Map#{}, DelayMap.adjust(v -> 2 * v, 1, m) |> DelayMap.toMap)

    @Test
    def adjust02(): Unit \ Assert =
        let m = toDelayMap(Map#{1 => 4});
        assertEq(expected = Map#{1 => 8}, DelayMap.adjust(v -> 2 * v, 1, m) |> DelayMap.toMap)

    @Test
    def adjust03(): Unit \ Assert =
        let m = toDelayMap(Map#{1 => 4});
        assertEq(expected = Map#{1 => 4}, DelayMap.adjust(v -> 2 * v, 2, m) |> DelayMap.toMap)

    @Test
    def adjust04(): Unit \ Assert =
        let m = toDelayMap(Map#{1 => -14, 5 => -2});
        assertEq(expected = Map#{1 => -28, 5 => -2}, DelayMap.adjust(v -> 2 * v, 1, m) |> DelayMap.toMap)

    @Test
    def adjust05(): Unit \ Assert =
        let m = toDelayMap(Map#{1 => 4, 5 => -2});
        assertEq(expected = Map#{1 => 4, 5 => -4}, DelayMap.adjust(v -> 2 * v, 5, m) |> DelayMap.toMap)

    @Test
    def adjust06(): Unit \ Assert =
        let m = toDelayMap(Map#{1 => 4, 5 => -2});
        assertEq(expected = Map#{1 => 4, 5 => -2}, DelayMap.adjust(v -> 2 * v, 4, m) |> DelayMap.toMap)


    /////////////////////////////////////////////////////////////////////////////
    // adjustWithKey                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def adjustWithKey01(): Unit \ Assert =
        let m = toDelayMap(Map#{});
        assertEq(expected = Map#{}, DelayMap.adjustWithKey((k, v) -> k + v, 1, m) |> DelayMap.toMap)

    @Test
    def adjustWithKey02(): Unit \ Assert =
        let m = toDelayMap(Map#{1 => 4});
        assertEq(expected = Map#{1 => 5}, DelayMap.adjustWithKey((k, v) -> k + v, 1, m) |> DelayMap.toMap)

    @Test
    def adjustWithKey03(): Unit \ Assert =
        let m = toDelayMap(Map#{1 => 4});
        assertEq(expected = Map#{1 => 4}, DelayMap.adjustWithKey((k, v) -> k + v, 2, m) |> DelayMap.toMap)

    @Test
    def adjustWithKey04(): Unit \ Assert =
        let m = toDelayMap(Map#{1 => -14, 5 => -2});
        assertEq(expected = Map#{1 => -13, 5 => -2}, DelayMap.adjustWithKey((k, v) -> k + v, 1, m) |> DelayMap.toMap)

    @Test
    def adjustWithKey05(): Unit \ Assert =
        let m = toDelayMap(Map#{1 => 4, 5 => -2});
        assertEq(expected = Map#{1 => 4, 5 => 3}, DelayMap.adjustWithKey((k, v) -> k + v, 5, m) |> DelayMap.toMap)

    @Test
    def adjustWithKey06(): Unit \ Assert =
        let m = toDelayMap(Map#{1 => 4, 5 => -2});
        assertEq(expected = Map#{1 => 4, 5 => -2}, DelayMap.adjustWithKey((k, v) -> k + v, 4, m) |> DelayMap.toMap)


    /////////////////////////////////////////////////////////////////////////////
    // update                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def update01(): Unit \ Assert =
        let m = toDelayMap(Map#{});
        assertEq(expected = Map#{}, DelayMap.update(v -> if (v `Int32.remainder` 2 != 0) Some(2 * v) else None, 1, m) |> DelayMap.toMap)

    @Test
    def update02(): Unit \ Assert =
        let m = toDelayMap(Map#{1 => 3});
        assertEq(expected = Map#{1 => 6}, DelayMap.update(v -> if (v `Int32.remainder` 2 != 0) Some(2 * v) else None, 1, m) |> DelayMap.toMap)

    @Test
    def update03(): Unit \ Assert =
        let m = toDelayMap(Map#{1 => 4});
        assertEq(expected = Map#{1 => 4}, DelayMap.update(v -> if (v `Int32.remainder` 2 != 0) Some(2 * v) else None, 1, m) |> DelayMap.toMap)

    @Test
    def update04(): Unit \ Assert =
        let m = toDelayMap(Map#{1 => 4});
        assertEq(expected = Map#{1 => 4}, DelayMap.update(v -> if (v `Int32.remainder` 2 != 0) Some(2 * v) else None, 2, m) |> DelayMap.toMap)

    @Test
    def update05(): Unit \ Assert =
        let m = toDelayMap(Map#{1 => -14, 5 => -2});
        assertEq(expected = Map#{1 => -14, 5 => -2}, DelayMap.update(v -> if (v `Int32.remainder` 2 != 0) Some(2 * v) else None, 1, m) |> DelayMap.toMap)

    @Test
    def update06(): Unit \ Assert =
        let m = toDelayMap(Map#{1 => -13, 5 => -2});
        assertEq(expected = Map#{1 => -26, 5 => -2}, DelayMap.update(v -> if (v `Int32.remainder` 2 != 0) Some(2 * v) else None, 1, m) |> DelayMap.toMap)

    @Test
    def update07(): Unit \ Assert =
        let m = toDelayMap(Map#{1 => 4, 5 => -2});
        assertEq(expected = Map#{1 => 4, 5 => -2}, DelayMap.update(v -> if (v `Int32.remainder` 2 != 0) Some(2 * v) else None, 5, m) |> DelayMap.toMap)

    @Test
    def update08(): Unit \ Assert =
        let m = toDelayMap(Map#{1 => 4, 5 => -1});
        assertEq(expected = Map#{1 => 4, 5 => -2}, DelayMap.update(v -> if (v `Int32.remainder` 2 != 0) Some(2 * v) else None, 5, m) |> DelayMap.toMap)

    @Test
    def update09(): Unit \ Assert =
        let m = toDelayMap(Map#{1 => 4, 5 => -2});
        assertEq(expected = Map#{1 => 4, 5 => -2}, DelayMap.update(v -> if (v `Int32.remainder` 2 != 0) Some(2 * v) else None, 4, m) |> DelayMap.toMap)


    /////////////////////////////////////////////////////////////////////////////
    // updateWithKey (pure)                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def updateWithKeyPure01(): Unit \ Assert =
        let m = toDelayMap(Map#{});
        assertEq(expected = Map#{}, DelayMap.updateWithKey((k, v) -> if (v `Int32.remainder` 2 != 0) Some(k + 2 * v) else None, 1, m) |>
            DelayMap.toMap)

    @Test
    def updateWithKeyPure02(): Unit \ Assert =
        let m = toDelayMap(Map#{1 => 3});
        assertEq(expected = Map#{1 => 7}, DelayMap.updateWithKey((k, v) -> if (v `Int32.remainder` 2 != 0) Some(k + 2 * v) else None, 1, m) |>
            DelayMap.toMap)

    @Test
    def updateWithKeyPure03(): Unit \ Assert =
        let m = toDelayMap(Map#{1 => 4});
        assertEq(expected = Map#{1 => 4}, DelayMap.updateWithKey((k, v) -> if (v `Int32.remainder` 2 != 0) Some(k + 2 * v) else None, 1, m) |>
            DelayMap.toMap)

    @Test
    def updateWithKeyPure04(): Unit \ Assert =
        let m = toDelayMap(Map#{1 => 4});
        assertEq(expected = Map#{1 => 4}, DelayMap.updateWithKey((k, v) -> if (v `Int32.remainder` 2 != 0) Some(k + 2 * v) else None, 2, m) |>
            DelayMap.toMap)

    @Test
    def updateWithKeyPure05(): Unit \ Assert =
        let m = toDelayMap(Map#{1 => -14, 5 => -2});
        assertEq(expected = Map#{1 => -14, 5 => -2}, DelayMap.updateWithKey((k, v) -> if (v `Int32.remainder` 2 != 0) Some(k + 2 * v) else None, 1, m) |>
            DelayMap.toMap)

    @Test
    def updateWithKeyPure06(): Unit \ Assert =
        let m = toDelayMap(Map#{1 => -13, 5 => -2});
        assertEq(expected = Map#{1 => -25, 5 => -2}, DelayMap.updateWithKey((k, v) -> if (v `Int32.remainder` 2 != 0) Some(k + 2 * v) else None, 1, m) |>
            DelayMap.toMap)

    @Test
    def updateWithKeyPure07(): Unit \ Assert =
        let m = toDelayMap(Map#{1 => 4, 5 => -2});
        assertEq(expected = Map#{1 => 4, 5 => -2}, DelayMap.updateWithKey((k, v) -> if (v `Int32.remainder` 2 != 0) Some(k + 2 * v) else None, 5, m) |>
            DelayMap.toMap)

    @Test
    def updateWithKeyPure08(): Unit \ Assert =
        let m = toDelayMap(Map#{1 => 4, 5 => -1});
        assertEq(expected = Map#{1 => 4, 5 => 3}, DelayMap.updateWithKey((k, v) -> if (v `Int32.remainder` 2 != 0) Some(k + 2 * v) else None, 5, m) |>
            DelayMap.toMap)

    @Test
    def updateWithKeyPure09(): Unit \ Assert =
        let m = toDelayMap(Map#{1 => 4, 5 => -2});
        assertEq(expected = Map#{1 => 4, 5 => -2}, DelayMap.updateWithKey((k, v) -> if (v `Int32.remainder` 2 != 0) Some(k + 2 * v) else None, 4, m) |>
            DelayMap.toMap)


    /////////////////////////////////////////////////////////////////////////////
    // updateWithKey (pure)                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def updateWithKeyImpure01(): Unit \ {Assert, IO} =
        let m = toDelayMap(Map#{});
        assertEq(expected = Map#{}, DelayMap.updateWithKey((k, v) -> unchecked_cast(if (v `Int32.remainder` 2 != 0) Some(k + 2 * v) else None as _ \ IO), 1, m) |>
            DelayMap.toMap)

    @Test
    def updateWithKeyImpure02(): Unit \ {Assert, IO} =
        let m = toDelayMap(Map#{1 => 3});
        assertEq(expected = Map#{1 => 7}, DelayMap.updateWithKey((k, v) -> unchecked_cast(if (v `Int32.remainder` 2 != 0) Some(k + 2 * v) else None as _ \ IO), 1, m) |>
            DelayMap.toMap)

    @Test
    def updateWithKeyImpure03(): Unit \ {Assert, IO} =
        let m = toDelayMap(Map#{1 => 4});
        assertEq(expected = Map#{1 => 4}, DelayMap.updateWithKey((k, v) -> unchecked_cast(if (v `Int32.remainder` 2 != 0) Some(k + 2 * v) else None as _ \ IO), 1, m) |>
            DelayMap.toMap)

    @Test
    def updateWithKeyImpure04(): Unit \ {Assert, IO} =
        let m = toDelayMap(Map#{1 => 4});
        assertEq(expected = Map#{1 => 4}, DelayMap.updateWithKey((k, v) -> unchecked_cast(if (v `Int32.remainder` 2 != 0) Some(k + 2 * v) else None as _ \ IO), 2, m) |>
            DelayMap.toMap)

    @Test
    def updateWithKeyImpure05(): Unit \ {Assert, IO} =
        let m = toDelayMap(Map#{1 => -14, 5 => -2});
        assertEq(expected = Map#{1 => -14, 5 => -2}, DelayMap.updateWithKey((k, v) -> unchecked_cast(if (v `Int32.remainder` 2 != 0) Some(k + 2 * v) else None as _ \ IO), 1, m) |>
            DelayMap.toMap)

    @Test
    def updateWithKeyImpure06(): Unit \ {Assert, IO} =
        let m = toDelayMap(Map#{1 => -13, 5 => -2});
        assertEq(expected = Map#{1 => -25, 5 => -2}, DelayMap.updateWithKey((k, v) -> unchecked_cast(if (v `Int32.remainder` 2 != 0) Some(k + 2 * v) else None as _ \ IO), 1, m) |>
            DelayMap.toMap)

    @Test
    def updateWithKeyImpure07(): Unit \ {Assert, IO} =
        let m = toDelayMap(Map#{1 => 4, 5 => -2});
        assertEq(expected = Map#{1 => 4, 5 => -2}, DelayMap.updateWithKey((k, v) -> unchecked_cast(if (v `Int32.remainder` 2 != 0) Some(k + 2 * v) else None as _ \ IO), 5, m) |>
            DelayMap.toMap)

    @Test
    def updateWithKeyImpure08(): Unit \ {Assert, IO} =
        let m = toDelayMap(Map#{1 => 4, 5 => -1});
        assertEq(expected = Map#{1 => 4, 5 => 3}, DelayMap.updateWithKey((k, v) -> unchecked_cast(if (v `Int32.remainder` 2 != 0) Some(k + 2 * v) else None as _ \ IO), 5, m) |>
            DelayMap.toMap)

    @Test
    def updateWithKeyImpure09(): Unit \ {Assert, IO} =
        let m = toDelayMap(Map#{1 => 4, 5 => -2});
        assertEq(expected = Map#{1 => 4, 5 => -2}, DelayMap.updateWithKey((k, v) -> unchecked_cast(if (v `Int32.remainder` 2 != 0) Some(k + 2 * v) else None as _ \ IO), 4, m) |>
            DelayMap.toMap)


    /////////////////////////////////////////////////////////////////////////////
    // updateWithKey updateWithKey                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def updateWithKeyUpdateWithKey01(): Unit \ Assert =
        assertEq(expected = Map#{1 => 9, 5 => 1}, toDelayMap(Map#{1 => 4, 5 => -2})                                          |>
            DelayMap.updateWithKey((k, v) -> if (v == 4) Some(k + 2 * v) else None, 1) |>
            DelayMap.updateWithKey((k, v) -> if (k == 5) Some(k + 2 * v) else None, 5) |>
            DelayMap.toMap)

    @Test
    def updateWithKeyUpdateWithKey02(): Unit \ {Assert, IO} =
        assertEq(expected = Map#{1 => 9, 5 => 1}, toDelayMap(Map#{1 => 4, 5 => -2})                                                              |>
            DelayMap.updateWithKey((k, v) -> unchecked_cast(if (v == 4) Some(k + 2 * v) else None as _ \ IO), 1) |>
            DelayMap.updateWithKey((k, v) -> if (k == 5) Some(k + 2 * v) else None, 5)                     |>
            DelayMap.toMap)

    @Test
    def updateWithKeyUpdateWithKey03(): Unit \ {Assert, IO} =
        assertEq(expected = Map#{1 => 9, 5 => 1}, toDelayMap(Map#{1 => 4, 5 => -2})                                                              |>
            DelayMap.updateWithKey((k, v) -> if (v == 4) Some(k + 2 * v) else None, 1)                     |>
            DelayMap.updateWithKey((k, v) -> unchecked_cast(if (k == 5) Some(k + 2 * v) else None as _ \ IO), 5) |>
            DelayMap.toMap)

    @Test
    def updateWithKeyUpdateWithKey04(): Unit \ {Assert, IO} =
        assertEq(expected = Map#{1 => 9, 5 => 1}, toDelayMap(Map#{1 => 4, 5 => -2})                                                              |>
            DelayMap.updateWithKey((k, v) -> unchecked_cast(if (v == 4) Some(k + 2 * v) else None as _ \ IO), 1) |>
            DelayMap.updateWithKey((k, v) -> unchecked_cast(if (k == 5) Some(k + 2 * v) else None as _ \ IO), 5) |>
            DelayMap.toMap)


    /////////////////////////////////////////////////////////////////////////////
    // updateWithKey (fusion)                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def updateWithKeyFusion01(): Unit \ Assert = region rc {
        let l = Ref.fresh(rc, Nil);
        discard toDelayMap(Map#{1 => 1, 2 => 2, 3 => 3}) |>
            DelayMap.updateWithKey((_, v) -> { Ref.put("a" :: Ref.get(l), l); Some(v) }, 1) |>
            DelayMap.updateWithKey((_, v) -> { Ref.put("a" :: Ref.get(l), l); Some(v) }, 2) |>
            DelayMap.updateWithKey((_, v) -> { Ref.put("a" :: Ref.get(l), l); Some(v) }, 3) |>
            DelayMap.updateWithKey((_, v) -> { Ref.put("b" :: Ref.get(l), l); Some(v) }, 1) |>
            DelayMap.updateWithKey((_, v) -> { Ref.put("b" :: Ref.get(l), l); Some(v) }, 2) |>
            DelayMap.updateWithKey((_, v) -> { Ref.put("b" :: Ref.get(l), l); Some(v) }, 3);
        assertEq(expected = "a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil, List.reverse(Ref.get(l)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def size01(): Unit \ Assert =
        assertEq(expected = 0, Map#{} |> toDelayMap |> DelayMap.size)

    @Test
    def size02(): Unit \ Assert =
        assertEq(expected = 1, Map#{1 => 2} |> toDelayMap |> DelayMap.size)

    @Test
    def size03(): Unit \ Assert =
        assertEq(expected = 2, Map#{1 => 2, 2 => 4} |> toDelayMap |> DelayMap.size)

    @Test
    def size04(): Unit \ Assert =
        assertEq(expected = 3, Map#{1 => 2, 2 => 4, 3 => 6} |> toDelayMap |> DelayMap.size)

    @Test
    def size05(): Unit \ Assert =
        assertEq(expected = 4, Map#{1 => 2, 2 => 4, 3 => 6, 4 => 8} |> toDelayMap |> DelayMap.size)

    @Test
    def size06(): Unit \ Assert =
        assertEq(expected = 5, Map#{1 => 2, 2 => 4, 3 => 6, 4 => 8, 5 => 10} |> toDelayMap |> DelayMap.size)


    /////////////////////////////////////////////////////////////////////////////
    // singleton                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def singleton01(): Unit \ Assert =
        assertEq(expected = Map#{1 => 2}, DelayMap.singleton(1, 2) |> DelayMap.toMap)

    @Test
    def singleton02(): Unit \ Assert =
        assertEq(expected = Map#{3 => -1}, DelayMap.singleton(3, -1) |> DelayMap.toMap)

    @Test
    def singleton03(): Unit \ Assert =
        assertEq(expected = Map#{-99 => -11}, DelayMap.singleton(-99, -11) |> DelayMap.toMap)


    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isEmpty01(): Unit \ Assert =
        assertTrue((Map#{}: Map[Unit, Unit]) |> toDelayMap |> DelayMap.isEmpty)

    @Test
    def isEmpty02(): Unit \ Assert =
        assertTrue(not (Map#{1 => 2} |> toDelayMap |> DelayMap.isEmpty))

    @Test
    def isEmpty03(): Unit \ Assert =
        assertTrue(not (Map#{1 => 2, 2 => 4} |> toDelayMap |> DelayMap.isEmpty))

    @Test
    def isEmpty04(): Unit \ Assert =
        assertTrue(not (Map#{1 => 2, 2 => 4, 3 => 6} |> toDelayMap |> DelayMap.isEmpty))

    @Test
    def isEmpty05(): Unit \ Assert =
        assertTrue(not (Map#{1 => 2, 2 => 4, 3 => 6, 4 => 8} |> toDelayMap |> DelayMap.isEmpty))


    /////////////////////////////////////////////////////////////////////////////
    // nonEmpty                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def nonEmpty01(): Unit \ Assert =
        assertTrue(not ((Map#{}: Map[Unit, Unit]) |> toDelayMap |> DelayMap.nonEmpty))

    @Test
    def nonEmpty02(): Unit \ Assert =
        assertTrue(Map#{1 => 2} |> toDelayMap |> DelayMap.nonEmpty)

    @Test
    def nonEmpty03(): Unit \ Assert =
        assertTrue(Map#{1 => 2, 2 => 4} |> toDelayMap |> DelayMap.nonEmpty)

    @Test
    def nonEmpty04(): Unit \ Assert =
        assertTrue(Map#{1 => 2, 2 => 4, 3 => 6} |> toDelayMap |> DelayMap.nonEmpty)

    @Test
    def nonEmpty05(): Unit \ Assert =
        assertTrue(Map#{1 => 2, 2 => 4, 3 => 6, 4 => 8} |> toDelayMap |> DelayMap.nonEmpty)


    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeft01(): Unit \ Assert =
        assertEq(expected = 0, Map#{} |> toDelayMap |>
            DelayMap.foldLeft((s, v) -> s + v, 0))

    @Test
    def foldLeft02(): Unit \ Assert =
        assertEq(expected = 2, Map#{1 => 2} |> toDelayMap |>
            DelayMap.foldLeft((s, v) -> s + v, 0))

    @Test
    def foldLeft03(): Unit \ Assert =
        assertEq(expected = 6, Map#{1 => 2, 3 => 4} |> toDelayMap |>
            DelayMap.foldLeft((s, v) -> s + v, 0))

    @Test
    def foldLeft04(): Unit \ Assert =
        assertEq(expected = 12, Map#{1 => 2, 3 => 4, 5 => 6} |> toDelayMap |>
            DelayMap.foldLeft((s, v) -> s + v, 0))


    /////////////////////////////////////////////////////////////////////////////
    // foldLeftWithKey                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeftWithKey01(): Unit \ Assert =
        assertEq(expected = 0, Map#{} |> toDelayMap |>
            DelayMap.foldLeftWithKey((s, k, v) -> k + s + v, 0))

    @Test
    def foldLeftWithKey02(): Unit \ Assert =
        assertEq(expected = 3, Map#{1 => 2} |> toDelayMap |>
            DelayMap.foldLeftWithKey((s, k, v) -> k + s + v, 0))

    @Test
    def foldLeftWithKey03(): Unit \ Assert =
        assertEq(expected = 10, Map#{1 => 2, 3 => 4} |> toDelayMap |>
            DelayMap.foldLeftWithKey((s, k, v) -> k + s + v, 0))

    @Test
    def foldLeftWithKey04(): Unit \ Assert =
        assertEq(expected = 21, Map#{1 => 2, 3 => 4, 5 => 6} |> toDelayMap |>
            DelayMap.foldLeftWithKey((s, k, v) -> k + s + v, 0))


    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRight01(): Unit \ Assert =
        assertEq(expected = 0, Map#{} |> toDelayMap |>
            DelayMap.foldRight((v, acc) -> acc + v, 0))

    @Test
    def foldRight02(): Unit \ Assert =
        assertEq(expected = 2, Map#{1 => 2} |> toDelayMap |>
            DelayMap.foldRight((v, acc) -> acc + v, 0))

    @Test
    def foldRight03(): Unit \ Assert =
        assertEq(expected = 6, Map#{1 => 2, 3 => 4} |> toDelayMap |>
            DelayMap.foldRight((v, acc) -> acc + v, 0))

    @Test
    def foldRight04(): Unit \ Assert =
        assertEq(expected = 12, Map#{1 => 2, 3 => 4, 5 => 6} |> toDelayMap |>
            DelayMap.foldRight((v, acc) -> acc + v, 0))


    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithKey                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRightWithKey01(): Unit \ Assert =
        assertEq(expected = 0, Map#{} |> toDelayMap |>
            DelayMap.foldRightWithKey((k, v, acc) -> k + acc + v, 0))

    @Test
    def foldRightWithKey02(): Unit \ Assert =
        assertEq(expected = 3, Map#{1 => 2} |> toDelayMap |>
            DelayMap.foldRightWithKey((k, v, acc) -> k + acc + v, 0))

    @Test
    def foldRightWithKey03(): Unit \ Assert =
        assertEq(expected = 10, Map#{1 => 2, 3 => 4} |> toDelayMap |>
            DelayMap.foldRightWithKey((k, v, acc) -> k + acc + v, 0))

    @Test
    def foldRightWithKey04(): Unit \ Assert =
        assertEq(expected = 21, Map#{1 => 2, 3 => 4, 5 => 6} |> toDelayMap |>
            DelayMap.foldRightWithKey((k, v, acc) -> k + acc + v, 0))

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRightWithCont01(): Unit \ Assert =
        assertEq(expected = 0, Map#{} |> toDelayMap |>
            DelayMap.foldRightWithCont((v, c) -> c() + v, 0))

    @Test
    def foldRightWithCont02(): Unit \ Assert =
        assertEq(expected = 2, Map#{1 => 2} |> toDelayMap |>
            DelayMap.foldRightWithCont((v, c) -> c() + v, 0))

    @Test
    def foldRightWithCont03(): Unit \ Assert =
        assertEq(expected = 6, Map#{1 => 2, 3 => 4} |> toDelayMap |>
            DelayMap.foldRightWithCont((v, c) -> c() + v, 0))

    @Test
    def foldRightWithCont04(): Unit \ Assert =
        assertEq(expected = 12, Map#{1 => 2, 3 => 4, 5 => 6} |> toDelayMap |>
            DelayMap.foldRightWithCont((v, c) -> c() + v, 0))


    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithKeyCont                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRightWithKeyCont01(): Unit \ Assert =
        assertEq(expected = 0, Map#{} |> toDelayMap |>
            DelayMap.foldRightWithKeyCont((k, v, c) -> k + c() + v, 0))

    @Test
    def foldRightWithKeyCont02(): Unit \ Assert =
        assertEq(expected = 3, Map#{1 => 2} |> toDelayMap |>
            DelayMap.foldRightWithKeyCont((k, v, c) -> k + c() + v, 0))

    @Test
    def foldRightWithKeyCont03(): Unit \ Assert =
        assertEq(expected = 10, Map#{1 => 2, 3 => 4} |> toDelayMap |>
            DelayMap.foldRightWithKeyCont((k, v, c) -> k + c() + v, 0))

    @Test
    def foldRightWithKeyCont04(): Unit \ Assert =
        assertEq(expected = 21, Map#{1 => 2, 3 => 4, 5 => 6} |> toDelayMap |>
            DelayMap.foldRightWithKeyCont((k, v, c) -> k + c() + v, 0))


    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceLeft01(): Unit \ Assert =
        assertEq(expected = None, (Map#{}: Map[Unit, Int32]) |> toDelayMap |>
            DelayMap.reduceLeft((v1, v2) -> v1 - v2))

    @Test
    def reduceLeft02(): Unit \ Assert =
        assertEq(expected = Some(2), Map#{1 => 2} |> toDelayMap |>
            DelayMap.reduceLeft((v1, v2) -> v1 - v2))

    @Test
    def reduceLeft03(): Unit \ Assert =
        assertEq(expected = Some(-1), Map#{1 => 2, 2 => 3} |> toDelayMap |>
            DelayMap.reduceLeft((v1, v2) -> v1 - v2))

    @Test
    def reduceLeft04(): Unit \ Assert =
        assertEq(expected = Some(-5), Map#{1 => 2, 2 => 3, 3 => 4} |> toDelayMap |>
            DelayMap.reduceLeft((v1, v2) -> v1 - v2))


    /////////////////////////////////////////////////////////////////////////////
    // reduceLeftWithKey                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceLeftWithKey01(): Unit \ Assert =
        assertEq(expected = None, (Map#{}: Map[Int32, Int32]) |> toDelayMap |>
            DelayMap.reduceLeftWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2)))

    @Test
    def reduceLeftWithKey02(): Unit \ Assert =
        assertEq(expected = Some((1, 2)), Map#{1 => 2} |> toDelayMap |>
            DelayMap.reduceLeftWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2)))

    @Test
    def reduceLeftWithKey03(): Unit \ Assert =
        assertEq(expected = Some((-1, -1)), Map#{1 => 2, 2 => 3} |> toDelayMap |>
            DelayMap.reduceLeftWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2)))

    @Test
    def reduceLeftWithKey04(): Unit \ Assert =
        assertEq(expected = Some((-4, -5)), Map#{1 => 2, 2 => 3, 3 => 4} |> toDelayMap |>
            DelayMap.reduceLeftWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2)))


    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceRight01(): Unit \ Assert =
        assertEq(expected = None, (Map#{}: Map[Unit, Int32]) |> toDelayMap |>
            DelayMap.reduceRight((v1, v2) -> v1 - v2))

    @Test
    def reduceRight02(): Unit \ Assert =
        assertEq(expected = Some(2), Map#{1 => 2} |> toDelayMap |>
            DelayMap.reduceRight((v1, v2) -> v1 - v2))

    @Test
    def reduceRight03(): Unit \ Assert =
        assertEq(expected = Some(-1), Map#{1 => 2, 2 => 3} |> toDelayMap |>
            DelayMap.reduceRight((v1, v2) -> v1 - v2))

    @Test
    def reduceRight04(): Unit \ Assert =
        assertEq(expected = Some(3), Map#{1 => 2, 2 => 3, 3 => 4} |> toDelayMap |>
            DelayMap.reduceRight((v1, v2) -> v1 - v2))


    /////////////////////////////////////////////////////////////////////////////
    // reduceRightWithKey                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceRightWithKey01(): Unit \ Assert =
        assertEq(expected = None, (Map#{}: Map[Int32, Int32]) |> toDelayMap |>
            DelayMap.reduceRightWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2)))

    @Test
    def reduceRightWithKey02(): Unit \ Assert =
        assertEq(expected = Some((1, 2)), Map#{1 => 2} |> toDelayMap |>
            DelayMap.reduceRightWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2)))

    @Test
    def reduceRightWithKey03(): Unit \ Assert =
        assertEq(expected = Some((-1, -1)), Map#{1 => 2, 2 => 3} |> toDelayMap |>
            DelayMap.reduceRightWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2)))

    @Test
    def reduceRightWithKey04(): Unit \ Assert =
        assertEq(expected = Some((2 , 3)), Map#{1 => 2, 2 => 3, 3 => 4} |> toDelayMap |>
            DelayMap.reduceRightWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2)))


    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Unit \ Assert = region rc {
        let ri = Ref.fresh(rc, 21);
        Map#{} |> toDelayMap |>
            DelayMap.forEach((k, _) -> Ref.put(k, ri));
        assertEq(expected = 21, Ref.get(ri))
    }

    @Test
    def forEach02(): Unit \ Assert = region rc {
        let ri = Ref.fresh(rc, 21);
        Map#{1 => "Hello World!"} |> toDelayMap |>
            DelayMap.forEach((k, _) -> Ref.put(k, ri));
        assertEq(expected = 1, Ref.get(ri))
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEachWithIndex01(): Unit \ Assert = region rc {
        let ri = Ref.fresh(rc, 21);
        Map#{} |> toDelayMap |>
            DelayMap.forEachWithIndex((i, _, _) -> Ref.put(i, ri));
        assertEq(expected = 21, Ref.get(ri))
    }

    @Test
    def forEachWithIndex02(): Unit \ Assert = region rc {
        let ri = Ref.fresh(rc, 21);
        Map#{1 => "Hello World!"} |> toDelayMap |>
            DelayMap.forEachWithIndex((i, _, _) -> Ref.put(i, ri));
        assertEq(expected = 0, Ref.get(ri))
    }

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def memberOf01(): Unit \ Assert =
        assertTrue(not ((Map#{}: Map[_, Unit]) |> toDelayMap |>
            DelayMap.memberOf(2)))

    @Test
    def memberOf02(): Unit \ Assert =
        assertTrue(not (Map#{1 => 2} |> toDelayMap |>
            DelayMap.memberOf(2)))

    @Test
    def memberOf03(): Unit \ Assert =
        assertTrue(Map#{2 => 1} |> toDelayMap |>
            DelayMap.memberOf(2))

    @Test
    def memberOf04(): Unit \ Assert =
        assertTrue(not (Map#{2 => 1, 3 => 17} |> toDelayMap |>
            DelayMap.memberOf(5)))

    @Test
    def memberOf05(): Unit \ Assert =
        assertTrue(Map#{2 => 1, 5 => 17} |> toDelayMap |>
            DelayMap.memberOf(5))

    @Test
    def memberOf06(): Unit \ Assert =
        assertTrue(Map#{5 => 1, 3 => 17} |> toDelayMap |>
            DelayMap.memberOf(5))

    @Test
    def memberOf07(): Unit \ Assert =
        assertTrue(not (Map#{2 => 1, 3 => 17, -1 => -2} |> toDelayMap |>
            DelayMap.memberOf(-2)))

    @Test
    def memberOf08(): Unit \ Assert =
        assertTrue(Map#{-2 => 1, 3 => 17, -1 => -2} |> toDelayMap |>
            DelayMap.memberOf(-2))

    @Test
    def memberOf09(): Unit \ Assert =
        assertTrue(Map#{2 => 1, -2 => 17, -1 => -2} |> toDelayMap |>
            DelayMap.memberOf(-2))

    @Test
    def memberOf10(): Unit \ Assert =
        assertTrue(Map#{2 => 1, 3 => 17, -2 => -2} |> toDelayMap |>
            DelayMap.memberOf(-2))


    /////////////////////////////////////////////////////////////////////////////
    // keysOf                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def keysOf01(): Unit \ Assert =
        assertEq(expected = Set#{}, (Map#{}: Map[Unit, Unit]) |> toDelayMap |>
            DelayMap.keysOf)

    @Test
    def keysOf02(): Unit \ Assert =
        assertEq(expected = Set#{1}, Map#{1 => 2} |> toDelayMap |>
            DelayMap.keysOf)

    @Test
    def keysOf03(): Unit \ Assert =
        assertEq(expected = Set#{1, 2}, Map#{1 => 2, 2 => 4} |> toDelayMap |>
            DelayMap.keysOf)

    @Test
    def keysOf04(): Unit \ Assert =
        assertEq(expected = Set#{1, 2, 3}, Map#{1 => 2, 2 => 4, 3 => 6} |> toDelayMap |>
            DelayMap.keysOf)

    @Test
    def keysOf05(): Unit \ Assert =
        assertEq(expected = Set#{1, 2, 3, 4}, Map#{1 => 2, 2 => 4, 3 => 6, 4 => 8} |> toDelayMap |>
            DelayMap.keysOf)

    @Test
    def keysOf06(): Unit \ Assert =
        assertEq(expected = Set#{1, 2, 3, 4, 5}, Map#{1 => 2, 2 => 4, 3 => 6, 4 => 8, 5 => 10} |> toDelayMap |>
            DelayMap.keysOf)


    /////////////////////////////////////////////////////////////////////////////
    // valuesOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def valuesOf01(): Unit \ Assert =
        assertEq(expected = Nil, (Map#{}: Map[Unit, Unit]) |> toDelayMap |>
            DelayMap.valuesOf)

    @Test
    def valuesOf02(): Unit \ Assert =
        assertEq(expected = 2 :: Nil, Map#{1 => 2} |> toDelayMap |>
            DelayMap.valuesOf)

    @Test
    def valuesOf03(): Unit \ Assert =
        assertEq(expected = 2 :: 4 :: Nil, Map#{1 => 2, 2 => 4} |> toDelayMap |>
            DelayMap.valuesOf)

    @Test
    def valuesOf04(): Unit \ Assert =
        assertEq(expected = 2 :: 4 :: 6 :: Nil, Map#{1 => 2, 2 => 4, 3 => 6} |> toDelayMap |>
            DelayMap.valuesOf)

    @Test
    def valuesOf05(): Unit \ Assert =
        assertEq(expected = 2 :: 4 :: 6 :: 8 :: Nil, Map#{1 => 2, 2 => 4, 3 => 6, 4 => 8} |> toDelayMap |>
            DelayMap.valuesOf)

    @Test
    def valuesOf06(): Unit \ Assert =
        assertEq(expected = 2 :: 4 :: 6 :: 8 :: 10 :: Nil, Map#{1 => 2, 2 => 4, 3 => 6, 4 => 8, 5 => 10} |> toDelayMap |>
            DelayMap.valuesOf)

    @Test
    def valuesOf07(): Unit \ Assert =
        assertEq(expected = -11 :: 4 :: -5 :: 7 :: -5 :: Nil, Map#{1 => -11, 2 => 4, 3 => -5, 4 => 7, 5 => -5} |> toDelayMap |>
            DelayMap.valuesOf)


    /////////////////////////////////////////////////////////////////////////////
    // minimumKey                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimumKey01(): Unit \ Assert =
        assertEq(expected = None, (Map#{}: Map[Unit, Int32]) |> toDelayMap |>
            DelayMap.minimumKey)

    @Test
    def minimumKey02(): Unit \ Assert =
        assertEq(expected = Some((1, 2)), Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5} |> toDelayMap |>
            DelayMap.minimumKey)

    @Test
    def minimumKey03(): Unit \ Assert =
        assertEq(expected = Some((0, 2)), Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5, 0 => 2} |> toDelayMap |>
            DelayMap.minimumKey)

    @Test
    def minimumKey04(): Unit \ Assert =
        assertEq(expected = Some((-44, 4)), Map#{1 => 2, 2 => 3, -44 => 4, 5 => 5, 0 => 2} |> toDelayMap |>
            DelayMap.minimumKey)


    /////////////////////////////////////////////////////////////////////////////
    // minimumKeyBy                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimumKeyBy01(): Unit \ Assert =
        assertEq(expected = None, (Map#{}: Map[Unit, Int32]) |> toDelayMap |>
            DelayMap.minimumKeyBy((x, y) -> x <=> y))

    @Test
    def minimumKeyBy02(): Unit \ Assert =
        assertEq(expected = Some((1, 2)), Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5} |> toDelayMap |>
            DelayMap.minimumKeyBy((x, y) -> x <=> y))

    @Test
    def minimumKeyBy03(): Unit \ Assert =
        assertEq(expected = Some((0, 2)), Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5, 0 => 2} |> toDelayMap |>
            DelayMap.minimumKeyBy((x, y) -> x <=> y))

    @Test
    def minimumKeyBy04(): Unit \ Assert =
        assertEq(expected = Some((-44, 4)), Map#{1 => 2, 2 => 3, -44 => 4, 5 => 5, 0 => 2} |> toDelayMap |>
            DelayMap.minimumKeyBy((x, y) -> x <=> y))


    /////////////////////////////////////////////////////////////////////////////
    // maximumKey                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maximumKey01(): Unit \ Assert =
        assertEq(expected = None, (Map#{}: Map[Unit, Int32]) |> toDelayMap |>
            DelayMap.maximumKey)

    @Test
    def maximumKey02(): Unit \ Assert =
        assertEq(expected = Some((5, 5)), Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5} |> toDelayMap |>
            DelayMap.maximumKey)

    @Test
    def maximumKey03(): Unit \ Assert =
        assertEq(expected = Some((5, 5)), Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5, 0 => 2} |> toDelayMap |>
            DelayMap.maximumKey)

    @Test
    def maximumKey04(): Unit \ Assert =
        assertEq(expected = Some((107, -107)), Map#{1 => 2, 2 => 3, 107 => -107, 5 => 5, 0 => 2} |> toDelayMap |>
            DelayMap.maximumKey)


    /////////////////////////////////////////////////////////////////////////////
    // maximumKeyBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maximumKeyBy01(): Unit \ Assert =
        assertEq(expected = None, (Map#{}: Map[Unit, Int32]) |> toDelayMap |>
            DelayMap.maximumKeyBy((x, y) -> x <=> y))

    @Test
    def maximumKeyBy02(): Unit \ Assert =
        assertEq(expected = Some((5, 5)), Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5} |> toDelayMap |>
            DelayMap.maximumKeyBy((x, y) -> x <=> y))

    @Test
    def maximumKeyBy03(): Unit \ Assert =
        assertEq(expected = Some((5, 5)), Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5, 0 => 2} |> toDelayMap |>
            DelayMap.maximumKeyBy((x, y) -> x <=> y))

    @Test
    def maximumKeyBy04(): Unit \ Assert =
        assertEq(expected = Some((107, -107)), Map#{1 => 2, 2 => 3, 107 => -107, 5 => 5, 0 => 2} |> toDelayMap |>
            DelayMap.maximumKeyBy((x, y) -> x <=> y))


    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterator01(): Unit \ Assert = region rc {
        assertEq(expected = Map.empty(), (Map.empty(): Map[Int32, Int32]) |> toDelayMap |>
            DelayMap.iterator(rc) |> Iterator.toMap)
    }

    @Test
    def iterator02(): Unit \ Assert = region rc {
        assertEq(expected = Map#{"A" => 1, "B" => 2, "C" => 3, "D" => 4}, Map#{"A" => 1, "B" => 2, "C" => 3, "D" => 4} |> toDelayMap |>
            DelayMap.iterator(rc) |> Iterator.toMap)
    }

    @Test
    def iterator03(): Unit \ Assert = region rc {
        let l = List.range(0, 100);
        assertEq(expected = List.zip(List.range(0, 100), List.reverse(List.range(0, 100))), List.zip(l, List.reverse(l)) |> List.toMap |> toDelayMap |>
            DelayMap.iterator(rc) |> Iterator.toMap |> Map.toList)
    }

    /////////////////////////////////////////////////////////////////////////////
    // sumValues                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumValues01(): Unit \ Assert =
        assertEq(expected = 0, DelayMap.empty() |> DelayMap.sumValues)

    @Test
    def sumValues02(): Unit \ Assert =
        assertEq(expected = 1, Map#{1 => 1} |> toDelayMap |>
            DelayMap.sumValues)

    @Test
    def sumValues03(): Unit \ Assert =
        assertEq(expected = 6, Map#{1 => 1, 2 => 2, 3 => 3} |> toDelayMap |>
            DelayMap.sumValues)

    @Test
    def sumValues04(): Unit \ Assert =
        assertEq(expected = 3, Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3} |> toDelayMap |>
            DelayMap.sumValues)

    @Test
    def sumValues05(): Unit \ Assert =
        assertEq(expected = -10, Map#{-1 => -1, -2 => -2, -3 => -3, -4 => -4} |> toDelayMap |>
            DelayMap.sumValues)

    @Test
    def sumValues06(): Unit \ Assert =
        assertEq(expected = 0, Map#{10 => 10, -10 => -10} |> toDelayMap |>
            DelayMap.sumValues)

    @Test
    def sumValues07(): Unit \ Assert =
        assertEq(expected = 5050, List.range(1, 101) |> List.zip(List.range(1, 101)) |> List.toMap |> toDelayMap |>
            DelayMap.sumValues)


    /////////////////////////////////////////////////////////////////////////////
    // sumKeys                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumKeys01(): Unit \ Assert =
        assertEq(expected = 0, DelayMap.empty() |> DelayMap.sumKeys)

    @Test
    def sumKeys02(): Unit \ Assert =
        assertEq(expected = 1, Map#{1 => 1} |> toDelayMap |>
            DelayMap.sumKeys)

    @Test
    def sumKeys03(): Unit \ Assert =
        assertEq(expected = 6, Map#{1 => 1, 2 => 2, 3 => 3} |> toDelayMap |>
            DelayMap.sumKeys)

    @Test
    def sumKeys04(): Unit \ Assert =
        assertEq(expected = 3, Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3} |> toDelayMap |>
            DelayMap.sumKeys)

    @Test
    def sumKeys05(): Unit \ Assert =
        assertEq(expected = -12, Map#{-1 => -1, -2 => -2, -5 => -3, -4 => -4} |> toDelayMap |>
            DelayMap.sumKeys)

    @Test
    def sumKeys06(): Unit \ Assert =
        assertEq(expected = 0, Map#{10 => 10, -10 => -10} |> toDelayMap |>
            DelayMap.sumKeys)

    @Test
    def sumKeys07(): Unit \ Assert =
        assertEq(expected = 5050, List.range(1, 101) |> List.zip(List.range(1, 101)) |> List.toMap |> toDelayMap |>
            DelayMap.sumKeys)


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumWith01(): Unit \ Assert =
        assertEq(expected = 0, DelayMap.empty() |> DelayMap.sumWith((k, v) -> k + v))

    @Test
    def sumWith02(): Unit \ Assert =
        assertEq(expected = 2, Map#{1 => 1} |> toDelayMap |>
            DelayMap.sumWith((k, v) -> k + v))

    @Test
    def sumWith03(): Unit \ Assert =
        assertEq(expected = 12, Map#{1 => 1, 2 => 2, 3 => 3} |> toDelayMap |>
            DelayMap.sumWith((k, v) -> k + v))

    @Test
    def sumWith04(): Unit \ Assert =
        assertEq(expected = 6, Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3} |> toDelayMap |>
            DelayMap.sumWith((k, v) -> k + v))

    @Test
    def sumWith05(): Unit \ Assert =
        assertEq(expected = -20, Map#{-1 => -1, -2 => -2, -3 => -3, -4 => -4} |> toDelayMap |>
            DelayMap.sumWith((k, v) -> k + v))

    @Test
    def sumWith06(): Unit \ Assert =
        assertEq(expected = 0, Map#{10 => 10, -10 => -10} |> toDelayMap |>
            DelayMap.sumWith((k, v) -> k + v))

    @Test
    def sumWith07(): Unit \ Assert = region rc {
        let range = List.range(0, 2000);
        let m = List.zip(range, range) |> List.toMap |> toDelayMap;
        let a = MutList.empty(rc);
        discard DelayMap.sumWith((k, _) -> { let b = MutList.empty(rc); MutList.push(k, b); MutList.append(MutList.toList(b), a); 0 }, m);
        assertEq(expected = range, MutList.toList(a))
    }


    /////////////////////////////////////////////////////////////////////////////
    // joinKeys                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinKeys01(): Unit \ Assert =
        assertEq(expected = "", (DelayMap.empty(): DelayMap[Int32, Int32]) |> DelayMap.joinKeys(","))

    @Test
    def joinKeys02(): Unit \ Assert =
        assertEq(expected = "1", Map#{1 => 1} |> toDelayMap |> DelayMap.joinKeys(","))

    @Test
    def joinKeys03(): Unit \ Assert =
        assertEq(expected = "0,1,2", Map#{0 => 1, 1 => 2, 2 => 2} |> toDelayMap |> DelayMap.joinKeys(","))

    @Test
    def joinKeys04(): Unit \ Assert =
        assertEq(expected = "0,1,2", Map#{"0" => 1, "1" => 2, "2" => 2} |> toDelayMap |> DelayMap.joinKeys(","))


    /////////////////////////////////////////////////////////////////////////////
    // joinValues                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinValues01(): Unit \ Assert =
        assertEq(expected = "", (DelayMap.empty(): DelayMap[Int32, Int32]) |> DelayMap.joinValues(","))

    @Test
    def joinValues02(): Unit \ Assert =
        assertEq(expected = "1", Map#{1 => 1} |> toDelayMap |> DelayMap.joinValues(","))

    @Test
    def joinValues03(): Unit \ Assert =
        assertEq(expected = "1,2,2", Map#{0 => 1, 1 => 2, 2 => 2} |> toDelayMap |> DelayMap.joinValues(","))

    @Test
    def joinValues04(): Unit \ Assert =
        assertEq(expected = "1,2,2", Map#{0 => "1", 1 => "2", 2 => "2"} |> toDelayMap |> DelayMap.joinValues(","))


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinWith01(): Unit \ Assert =
        assertEq(expected = "", (Map.empty(): Map[Int32, Int32]) |> toDelayMap |>
            DelayMap.joinWith((k, v) -> "${k} => ${v}", ","))

    @Test
    def joinWith02(): Unit \ Assert =
        assertEq(expected = "1 => 1", Map#{1 => 1} |> toDelayMap |>
            DelayMap.joinWith((k, v) -> "${k} => ${v}", ", "))

    @Test
    def joinWith03(): Unit \ Assert =
        assertEq(expected = "0 => 1, 1 => 2, 2 => 2", Map#{0 => 1, 1 => 2, 2 => 2} |> toDelayMap |>
            DelayMap.joinWith((k, v) -> "${k} => ${v}", ", "))

    @Test
    def joinWith04(): Unit \ Assert =
        assertEq(expected = "0 => 1, 1 => 2, 2 => 2", Map#{0 => "1", 1 => "2", 2 => "2"} |> toDelayMap |>
            DelayMap.joinWith((k, v) -> "${k} => ${v}", ", "))


    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toString01(): Unit \ Assert =
        assertEq(expected = "DelayMap#{1 => 2}", ToString.toString(Map#{1 => 2} |> toDelayMap))

    @Test
    def toString02(): Unit \ Assert =
        assertEq(expected = "DelayMap#{1 => 0, 2 => 1, 3 => 2}", ToString.toString(Map#{1 => 0, 2 => 1, 3 => 2} |> toDelayMap))

    @Test
    def toString03(): Unit \ Assert =
        assertEq(expected = "DelayMap#{1 => b, 2 => a}", ToString.toString(Map#{1 => "b", 2 => "a"} |> toDelayMap))

    @Test
    def toString04(): Unit \ Assert =
        assertEq(expected = "DelayMap#{0 => true, 2 => true, 3 => false, 4 => true, 97 => false}", ToString.toString(Map#{97 => false, 2 => true, 3 => false, 4 => true, 0 => true} |> toDelayMap))

    @Test
    def toString05(): Unit \ Assert =
        assertEq(expected = "DelayMap#{2 => Map#{1 => 0, 2 => 1}, 3 => Map#{3 => 2, 4 => 92}}", ToString.toString(Map#{2 => Map#{1 => 0, 2 => 1}, 3 => Map#{3 => 2, 4 => 92}} |> toDelayMap))


}
