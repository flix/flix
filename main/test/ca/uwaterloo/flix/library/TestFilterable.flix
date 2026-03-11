mod TestFilterable {

    use Assert.{assertEq};
    use Filterable.{filter, filterMap};

    def isOdd(i: Int32): Bool = not (i `Int32.remainder` 2 == 0)

    def oddPlus10(i: Int32): Option[Int32] = if (isOdd(i)) Some(i + 10) else None

    /////////////////////////////////////////////////////////////////////////////
    // filterChain                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterChain01(): Unit \ Assert =
        assertEq(expected = Nil, filter(isOdd, (Chain.empty(): Chain[Int32])) |> Chain.toList)

    @Test
    def filterChain02(): Unit \ Assert =
        assertEq(expected = 1 :: Nil, filter(isOdd, Chain.singleton(1)) |> Chain.toList)

    @Test
    def filterChain03(): Unit \ Assert =
        assertEq(expected = 1 :: 3 :: Nil, filter(isOdd, List.toChain(1 :: 2 :: 3 :: 4 :: Nil)) |> Chain.toList)


    /////////////////////////////////////////////////////////////////////////////
    // filterMapChain                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterMapChain01(): Unit \ Assert =
        assertEq(expected = Nil, filterMap(oddPlus10, (Chain.empty(): Chain[Int32])) |> Chain.toList)

    @Test
    def filterMapChain02(): Unit \ Assert =
        assertEq(expected = 11 :: Nil, filterMap(oddPlus10, Chain.singleton(1)) |> Chain.toList)

    @Test
    def filterMapChain03(): Unit \ Assert =
        assertEq(expected = 11 :: 13 :: Nil, filterMap(oddPlus10, List.toChain(1 :: 2 :: 3 :: 4 :: Nil)) |> Chain.toList)


    /////////////////////////////////////////////////////////////////////////////
    // filterDelayList                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterDelayList01(): Unit \ Assert =
        assertEq(expected = Nil, filter(isOdd, (DelayList.empty(): DelayList[Int32])) |> DelayList.toList)

    @Test
    def filterDelayList02(): Unit \ Assert =
        assertEq(expected = 1 :: Nil, filter(isOdd, DelayList.range(1, 2)) |> DelayList.toList)

    @Test
    def filterDelayList03(): Unit \ Assert =
        assertEq(expected = 1 :: 3 :: Nil, filter(isOdd, DelayList.range(1, 5)) |> DelayList.toList)


    /////////////////////////////////////////////////////////////////////////////
    // filterMapDelayList                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterMapDelayList01(): Unit \ Assert =
        assertEq(expected = Nil, filterMap(oddPlus10, (DelayList.empty(): DelayList[Int32])) |> DelayList.toList)

    @Test
    def filterMapDelayList02(): Unit \ Assert =
        assertEq(expected = 11 :: Nil, filterMap(oddPlus10, DelayList.range(1, 2)) |> DelayList.toList)

    @Test
    def filterMapDelayList03(): Unit \ Assert =
        assertEq(expected = 11 :: 13 :: Nil, filterMap(oddPlus10, DelayList.range(1, 5)) |> DelayList.toList)


    /////////////////////////////////////////////////////////////////////////////
    // filterList                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterList01(): Unit \ Assert =
        assertEq(expected = Nil, filter(isOdd, (Nil: List[Int32])))

    @Test
    def filterList02(): Unit \ Assert =
        assertEq(expected = 1 :: Nil, filter(isOdd, 1 :: Nil))

    @Test
    def filterList03(): Unit \ Assert =
        assertEq(expected = 1 :: 3 :: Nil, filter(isOdd, 1 :: 2 :: 3 :: 4 :: Nil))


    /////////////////////////////////////////////////////////////////////////////
    // filterMapList                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterMapList01(): Unit \ Assert =
        assertEq(expected = Nil, filterMap(oddPlus10, (Nil: List[Int32])))

    @Test
    def filterMapList02(): Unit \ Assert =
        assertEq(expected = 11 :: Nil, filterMap(oddPlus10, 1 :: Nil))

    @Test
    def filterMapList03(): Unit \ Assert =
        assertEq(expected = 11 :: 13 :: Nil, filterMap(oddPlus10, 1 :: 2 :: 3 :: 4 :: Nil))


    /////////////////////////////////////////////////////////////////////////////
    // filterMap                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterMap01(): Unit \ Assert =
        assertEq(expected = Map#{}, filter(isOdd, (Map.empty(): Map[Int32, Int32])))

    @Test
    def filterMap02(): Unit \ Assert =
        assertEq(expected = Map#{1 => 1}, filter(isOdd, Map#{1 => 1}))

    @Test
    def filterMap03(): Unit \ Assert =
        assertEq(expected = Map#{1 => 1, 3 => 3}, filter(isOdd, Map#{1 => 1, 2 => 2, 3 => 3, 4 => 4}))


    /////////////////////////////////////////////////////////////////////////////
    // filterMapMap                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterMapMap01(): Unit \ Assert =
        assertEq(expected = Map#{}, filterMap(oddPlus10, (Map.empty(): Map[Int32, Int32])))

    @Test
    def filterMapMap02(): Unit \ Assert =
        assertEq(expected = Map#{1 => 11}, filterMap(oddPlus10, Map#{1 => 1}))

    @Test
    def filterMapMap03(): Unit \ Assert =
        assertEq(expected = Map#{1 => 11, 3 => 13}, filterMap(oddPlus10, Map#{1 => 1, 2 => 2, 3 => 3, 4 => 4}))


    /////////////////////////////////////////////////////////////////////////////
    // filterOption                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterOption01(): Unit \ Assert =
        assertEq(expected = None, filter(isOdd, (None: Option[Int32])))

    @Test
    def filterOption02(): Unit \ Assert =
        assertEq(expected = Some(1), filter(isOdd, Some(1)))

    @Test
    def filterOption03(): Unit \ Assert =
        assertEq(expected = None, filter(isOdd, Some(2)))


    /////////////////////////////////////////////////////////////////////////////
    // filterMapOption                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterMapOption01(): Unit \ Assert =
        assertEq(expected = None, filterMap(oddPlus10, (None: Option[Int32])))

    @Test
    def filterMapOption02(): Unit \ Assert =
        assertEq(expected = Some(11), filterMap(oddPlus10, Some(1)))

    @Test
    def filterMapOption03(): Unit \ Assert =
        assertEq(expected = None, filterMap(oddPlus10, Some(2)))


    /////////////////////////////////////////////////////////////////////////////
    // filterRedBlackTree                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterRedBlackTree01(): Unit \ Assert =
        assertEq(expected = (1, 1) :: Nil, filter(isOdd, RedBlackTree.empty() |> RedBlackTree.insert(1, 1)) |> RedBlackTree.toList)

    @Test
    def filterRedBlackTree02(): Unit \ Assert =
        assertEq(expected = (1, 1) :: (3, 3) :: Nil,
            filter(isOdd, RedBlackTree.empty() |> RedBlackTree.insert(1, 1) |> RedBlackTree.insert(2, 2) |> RedBlackTree.insert(3, 3) |> RedBlackTree.insert(4, 4))
            |> RedBlackTree.toList |> List.sort)


    /////////////////////////////////////////////////////////////////////////////
    // filterMapRedBlackTree                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterMapRedBlackTree01(): Unit \ Assert =
        assertEq(expected = (1, 11) :: Nil,
            filterMap(oddPlus10, RedBlackTree.empty() |> RedBlackTree.insert(1, 1)) |> RedBlackTree.toList |> List.sort)

    @Test
    def filterMapRedBlackTree02(): Unit \ Assert =
        assertEq(expected = (1, 11) :: (3, 13) :: Nil,
            filterMap(oddPlus10, RedBlackTree.empty() |> RedBlackTree.insert(1, 1) |> RedBlackTree.insert(2, 2) |> RedBlackTree.insert(3, 3) |> RedBlackTree.insert(4, 4))
            |> RedBlackTree.toList |> List.sort)

}
