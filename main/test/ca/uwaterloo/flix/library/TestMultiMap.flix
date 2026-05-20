mod TestMultiMap {

    use Assert.{assertEq, assertNeq, assertTrue, assertFalse};
    use MultiMap.{empty, singleton, insert, toAssocList, MultiMap}

    def isEven(i: Int32): Bool = i `Int32.remainder` 2 == 0

    def isOdd(i: Int32): Bool = not isEven(i)

    /////////////////////////////////////////////////////////////////////////////
    // ToString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toString01(): Unit \ Assert =
        assertEq(expected = "MultiMap#{}", ToString.toString((empty(): MultiMap[Int32, Int32])))

    @Test
    def toString02(): Unit \ Assert =
        assertEq(expected = "MultiMap#{1 => Set#{1}}", ToString.toString(singleton(1, 1)))

    @Test
    def toString03(): Unit \ Assert =
        assertEq(expected = "MultiMap#{1 => Set#{1, 2}}", ToString.toString(singleton(1, 1) |> insert(1, 2)))

    @Test
    def toString04(): Unit \ Assert =
        assertEq(expected = "MultiMap#{1 => Set#{1}, 2 => Set#{2}}", ToString.toString(singleton(1, 1) |> insert(2, 2)))

    /////////////////////////////////////////////////////////////////////////////
    // Eq                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def eq01(): Unit \ Assert =
        assertEq(expected = (empty(): MultiMap[Int32, Char]), (empty(): MultiMap[Int32, Char]))

    @Test
    def eq02(): Unit \ Assert =
        assertNeq(unexpected = empty(), singleton(1, 'a'))

    @Test
    def eq03(): Unit \ Assert =
        assertNeq(unexpected = singleton(1, 'a'), empty())

    @Test
    def eq04(): Unit \ Assert =
        assertNeq(unexpected = singleton(1, 'b'), singleton(1, 'a'))

    @Test
    def eq05(): Unit \ Assert =
        assertEq(expected = singleton(1, 'a'), singleton(1, 'a'))

    @Test
    def eq06(): Unit \ Assert =
        assertEq(expected = singleton(1, 'a') |> insert(1, 'b'), singleton(1, 'a') |> insert(1, 'b'))

    /// Insert order doesn't matter for MultiMap
    @Test
    def eq07(): Unit \ Assert =
        assertEq(expected = singleton(1, 'a') |> insert(1, 'b'), singleton(1, 'b') |> insert(1, 'a'))

    /////////////////////////////////////////////////////////////////////////////
    // Order (compare)                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def compare01(): Unit \ Assert =
        assertEq(expected = Comparison.EqualTo, Order.compare((empty(): MultiMap[Int32, Char]), (empty(): MultiMap[Int32, Char])))

    @Test
    def compare02(): Unit \ Assert =
        assertEq(expected = Comparison.GreaterThan, Order.compare(singleton(1, 'a'), empty()))

    @Test
    def compare03(): Unit \ Assert =
        assertEq(expected = Comparison.LessThan, Order.compare(empty(), singleton(1, 'a')))

    @Test
    def compare04(): Unit \ Assert =
        assertEq(expected = Comparison.GreaterThan, Order.compare(singleton(1, 'b'), singleton(1, 'a')))

    @Test
    def compare05(): Unit \ Assert =
        assertEq(expected = Comparison.LessThan, Order.compare(singleton(1, 'a'), singleton(1, 'b')))

    /////////////////////////////////////////////////////////////////////////////
    // empty                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def empty01(): Unit \ Assert =
        assertTrue(MultiMap.isEmpty(empty()))

    /////////////////////////////////////////////////////////////////////////////
    // singleton                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def singleton01(): Unit \ Assert =
        assertEq(expected = (1, 'a') :: Nil, MultiMap.singleton(1, 'a') |> MultiMap.toList |> List.sort)

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isEmpty01(): Unit \ Assert =
        assertTrue(MultiMap.isEmpty(empty()))

    @Test
    def isEmpty02(): Unit \ Assert =
        assertFalse(MultiMap.isEmpty(singleton(1, 'a')))

    /////////////////////////////////////////////////////////////////////////////
    // nonEmpty                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def nonEmpty01(): Unit \ Assert =
        assertFalse(MultiMap.nonEmpty(empty()))

    @Test
    def nonEmpty02(): Unit \ Assert =
        assertTrue(MultiMap.nonEmpty(singleton(1, 'a')))

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def size01(): Unit \ Assert =
        assertEq(expected = 0, MultiMap.empty() |> MultiMap.size)

    @Test
    def size02(): Unit \ Assert =
        assertEq(expected = 1, singleton("a", 1) |> MultiMap.size)

    @Test
    def size03(): Unit \ Assert =
        assertEq(expected = 2, singleton(1, 'a') |> insert(2, 'b') |> MultiMap.size)

    @Test
    def size04(): Unit \ Assert =
        assertEq(expected = 3, singleton(1, 'a') |> insert(2, 'b') |> insert(3, 'c') |> MultiMap.size)

    /////////////////////////////////////////////////////////////////////////////
    // get                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def get01(): Unit \ Assert =
        assertEq(expected = Set#{}, MultiMap.get(1, (empty(): MultiMap[Int32, Char])))

    @Test
    def get02(): Unit \ Assert =
        assertEq(expected = Set#{'a'}, MultiMap.get(1, singleton(1, 'a')))

    @Test
    def get03(): Unit \ Assert =
        assertEq(expected = Set#{'a'}, MultiMap.get(1, singleton(1, 'a') |> insert(2, 'b')))

    @Test
    def get04(): Unit \ Assert =
        assertEq(expected = Set#{'a', 'c'}, MultiMap.get(1, singleton(1, 'a') |> insert(2, 'b') |> insert(1, 'c')))

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def memberOf01(): Unit \ Assert =
        assertFalse(MultiMap.memberOf(1, (empty(): MultiMap[Int32, Char])))

    @Test
    def memberOf02(): Unit \ Assert =
        assertTrue(MultiMap.memberOf(1, singleton(1, 'a')))

    @Test
    def memberOf03(): Unit \ Assert =
        assertFalse(MultiMap.memberOf(1, singleton(2, 'b')))

    @Test
    def memberOf04(): Unit \ Assert =
        assertTrue(MultiMap.memberOf(1, singleton(1, 'a') |> insert(2, 'b') |> insert(1, 'c')))

    /////////////////////////////////////////////////////////////////////////////
    // keysOf                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def keysOf01(): Unit \ Assert =
        assertEq(expected = Set#{}, MultiMap.keysOf((empty(): MultiMap[Int32, Char])))

    @Test
    def keysOf02(): Unit \ Assert =
        assertEq(expected = Set#{1}, MultiMap.keysOf(singleton(1, 'a')))

    @Test
    def keysOf03(): Unit \ Assert =
        assertEq(expected = Set#{2}, MultiMap.keysOf(singleton(2, 'b')))

    @Test
    def keysOf04(): Unit \ Assert =
        assertEq(expected = Set#{1, 2}, MultiMap.keysOf(singleton(1, 'a') |> insert(2, 'b') |> insert(1, 'c')))


    /////////////////////////////////////////////////////////////////////////////
    // valuesOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def valuesOf01(): Unit \ Assert =
        assertEq(expected = Nil, MultiMap.valuesOf((empty(): MultiMap[Int32, Char])))

    @Test
    def valuesOf02(): Unit \ Assert =
        assertEq(expected = 'a' :: Nil, MultiMap.valuesOf(singleton(1, 'a')) |> List.sort)

    @Test
    def valuesOf03(): Unit \ Assert =
        assertEq(expected = 'b' :: Nil, MultiMap.valuesOf(singleton(2, 'b')) |> List.sort)

    @Test
    def valuesOf04(): Unit \ Assert =
        assertEq(expected = 'a' :: 'b' :: 'c' :: Nil, MultiMap.valuesOf(singleton(1, 'a') |> insert(2, 'b') |> insert(1, 'c')) |> List.sort)

    @Test
    def valuesOf05(): Unit \ Assert =
        assertEq(expected = 'a' :: 'a' :: 'b' :: 'c' :: Nil, MultiMap.valuesOf(singleton(1, 'a') |> insert(2, 'b') |> insert(1, 'c') |> insert(3, 'a')) |> List.sort)

    /////////////////////////////////////////////////////////////////////////////
    // insert                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def insert01(): Unit \ Assert =
        assertEq(expected = (1, Set#{'a'}) :: Nil, MultiMap.insert(1, 'a', empty()) |> toAssocList)

    @Test
    def insert02(): Unit \ Assert =
        assertEq(expected = (1, Set#{'a'}) :: (2, Set#{'b'}) :: Nil, empty() |> MultiMap.insert(1, 'a') |> MultiMap.insert(2, 'b') |> toAssocList)

    @Test
    def insert03(): Unit \ Assert =
        assertEq(expected = (1, Set#{'a', 'c'}) :: (2, Set#{'b'}) :: Nil, empty() |> MultiMap.insert(1, 'a') |> MultiMap.insert(2, 'b') |> MultiMap.insert(1, 'c') |> toAssocList)

    @Test
    def insert04(): Unit \ Assert =
        assertEq(expected = (1, Set#{'a', 'c'}) :: (2, Set#{'b'}) :: (3, Set#{'b'}) :: Nil, empty() |> MultiMap.insert(1, 'a') |> MultiMap.insert(2, 'b') |> MultiMap.insert(1, 'c') |> MultiMap.insert(3, 'b') |> toAssocList)

    /////////////////////////////////////////////////////////////////////////////
    // insertAll                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def insertAll01(): Unit \ Assert =
        assertEq(expected = empty(), MultiMap.insertAll(1, (Nil: List[Char]), empty()))

    @Test
    def insertAll02(): Unit \ Assert =
        assertEq(expected = (1, Set#{'a'}) :: Nil, empty() |> MultiMap.insertAll(1, 'a' :: Nil) |> toAssocList)

    @Test
    def insertAll03(): Unit \ Assert =
        assertEq(expected = (1, Set#{'a', 'c'}) :: (2, Set#{'b'}) :: Nil, empty() |> MultiMap.insertAll(1, 'a' :: 'c' :: Nil) |> MultiMap.insertAll(2, 'b' :: Nil) |> toAssocList)

    @Test
    def insertAll04(): Unit \ Assert =
        assertEq(expected = (1, Set#{'a', 'c'}) :: (2, Set#{'b'}) :: (3, Set#{'b'}) :: Nil, empty() |> MultiMap.insertAll(1, 'a' :: 'c' :: Nil) |> MultiMap.insertAll(2, 'b' :: Nil) |> MultiMap.insertAll(3, 'b' :: Nil) |> toAssocList)

    /////////////////////////////////////////////////////////////////////////////
    // adjust                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def adjust01(): Unit \ Assert =
        assertEq(expected = empty(), MultiMap.adjust(v -> 2*v, 1, empty()))

    @Test
    def adjust02(): Unit \ Assert =
        assertEq(expected = (1, Set#{8}) :: Nil, MultiMap.adjust(v -> 2*v, 1, MultiMap.singleton(1, 4)) |> toAssocList)

    @Test
    def adjust03(): Unit \ Assert =
        assertEq(expected = (1, Set#{4}) :: Nil, MultiMap.adjust(v -> 2*v, 3, singleton(1, 4)) |> toAssocList)

    @Test
    def adjust04(): Unit \ Assert =
        assertEq(expected = (1, Set#{8, 10}) :: (2, Set#{4}) :: Nil, MultiMap.adjust(v -> 2*v, 1, singleton(1, 4) |> insert(2, 4) |> insert(1, 5)) |> toAssocList)

    @Test
    def adjust05(): Unit \ Assert =
        assertEq(expected = (1, Set#{4, 5}) :: (2, Set#{8}) :: Nil, MultiMap.adjust(v -> 2*v, 2, singleton(1, 4) |> insert(2, 4) |> insert(1, 5)) |> toAssocList)

    /////////////////////////////////////////////////////////////////////////////
    // adjustWithKey                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def adjustWithKey01(): Unit \ Assert =
        assertEq(expected = empty(), MultiMap.adjustWithKey((k, v) -> k+v, 1, empty()))

    @Test
    def adjustWithKey02(): Unit \ Assert =
        assertEq(expected = (1, Set#{5}) :: Nil, MultiMap.adjustWithKey((k, v) -> k+v, 1, singleton(1, 4)) |> toAssocList)

    @Test
    def adjustWithKey03(): Unit \ Assert =
        assertEq(expected = (1, Set#{4}) :: Nil, MultiMap.adjustWithKey((k, v) -> k+v, 3, singleton(1, 4)) |> toAssocList)

    @Test
    def adjustWithKey04(): Unit \ Assert =
        assertEq(expected = (1, Set#{5, 6}) :: (2, Set#{4}) :: Nil, MultiMap.adjustWithKey((k, v) -> k+v, 1, singleton(1, 4) |> insert(2, 4) |> insert(1, 5)) |> toAssocList)

    @Test
    def adjustWithKey05(): Unit \ Assert =
        assertEq(expected = (1, Set#{4, 5}) :: (2, Set#{6}) :: Nil, MultiMap.adjustWithKey((k, v) -> k+v, 2, singleton(1, 4) |> insert(2, 4) |> insert(1, 5)) |> toAssocList)

    /////////////////////////////////////////////////////////////////////////////
    // update                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def update01(): Unit \ Assert =
        assertEq(expected = empty(), MultiMap.update(v -> if (isEven(v)) Some(2*v) else None, 1, empty()))

    @Test
    def update02(): Unit \ Assert =
        assertEq(expected = (1, Set#{8}) :: Nil, MultiMap.update(v -> if (isEven(v)) Some(2*v) else None, 1, singleton(1, 4)) |> toAssocList)

    @Test
    def update03(): Unit \ Assert =
        assertEq(expected = (1, Set#{4}) :: Nil, MultiMap.update(v -> if (isEven(v)) Some(2*v) else None, 3, singleton(1, 4)) |> toAssocList)

    @Test
    def update04(): Unit \ Assert =
        assertEq(expected = (1, Set#{5, 8}) :: (2, Set#{4}) :: Nil, MultiMap.update(v -> if (isEven(v)) Some(2*v) else None, 1, singleton(1, 4) |> insert(2, 4) |> insert(1, 5)) |> toAssocList)

    @Test
    def update05(): Unit \ Assert =
        assertEq(expected = (1, Set#{4, 5}) :: (2, Set#{8}) :: Nil, MultiMap.update(v -> if (isEven(v)) Some(2*v) else None, 2, singleton(1, 4) |> insert(2, 4) |> insert(1, 5)) |> toAssocList)

    /////////////////////////////////////////////////////////////////////////////
    // updateWithKey                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def updateWithKey01(): Unit \ Assert =
        assertEq(expected = empty(), MultiMap.updateWithKey((k, v) -> if (isEven(k)) Some(k+v) else None, 1, empty()))

    @Test
    def updateWithKey02(): Unit \ Assert =
        assertEq(expected = (1, Set#{4}) :: Nil, MultiMap.updateWithKey((k, v) -> if (isEven(k)) Some(k+v) else None, 1, singleton(1, 4)) |> toAssocList)

    @Test
    def updateWithKey03(): Unit \ Assert =
        assertEq(expected = (1, Set#{5}) :: Nil, MultiMap.updateWithKey((k, v) -> if (isOdd(k)) Some(k+v) else None, 1, singleton(1, 4)) |> toAssocList)

    @Test
    def updateWithKey04(): Unit \ Assert =
        assertEq(expected = (1, Set#{4}) :: Nil, MultiMap.updateWithKey((k, v) -> if (isEven(k)) Some(k+v) else None, 3, singleton(1, 4)) |> toAssocList)

    @Test
    def updateWithKey05(): Unit \ Assert =
        assertEq(expected = (1, Set#{4}) :: Nil, MultiMap.updateWithKey((k, v) -> if (isOdd(k)) Some(k+v) else None, 3, singleton(1, 4)) |> toAssocList)

    @Test
    def updateWithKey06(): Unit \ Assert =
        assertEq(expected = (1, Set#{4, 5}) :: (2, Set#{4}) :: Nil, MultiMap.updateWithKey((k, v) -> if (isEven(k)) Some(k+v) else None, 1, singleton(1, 4) |> insert(2, 4) |> insert(1, 5)) |> toAssocList)

    @Test
    def updateWithKey07(): Unit \ Assert =
        assertEq(expected = (1, Set#{5, 6}) :: (2, Set#{4}) :: Nil, MultiMap.updateWithKey((k, v) -> if (isOdd(k)) Some(k+v) else None, 1, singleton(1, 4) |> insert(2, 4) |> insert(1, 5)) |> toAssocList)

    @Test
    def updateWithKey08(): Unit \ Assert =
        assertEq(expected = (1, Set#{4, 5}) :: (2, Set#{6}) :: Nil, MultiMap.updateWithKey((k, v) -> if (isEven(k)) Some(k+v) else None, 2, singleton(1, 4) |> insert(2, 4) |> insert(1, 5)) |> toAssocList)

    @Test
    def updateWithKey09(): Unit \ Assert =
        assertEq(expected = (1, Set#{4, 5}) :: (2, Set#{4}) :: Nil, MultiMap.updateWithKey((k, v) -> if (isOdd(k)) Some(k+v) else None, 2, singleton(1, 4) |> insert(2, 4) |> insert(1, 5)) |> toAssocList)

    /////////////////////////////////////////////////////////////////////////////
    // remove                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def remove01(): Unit \ Assert =
        assertEq(expected = empty(), MultiMap.remove(1, (empty(): MultiMap[Int32, Int32])))

    @Test
    def remove02(): Unit \ Assert =
        assertEq(expected = Nil, MultiMap.remove(1, singleton(1, 4)) |> toAssocList)

    @Test
    def remove03(): Unit \ Assert =
        assertEq(expected = (1, Set#{4}) :: Nil, MultiMap.remove(3, singleton(1, 4)) |> toAssocList)

    @Test
    def remove04(): Unit \ Assert =
        assertEq(expected = (2, Set#{4}) :: Nil, MultiMap.remove(1, singleton(1, 4) |> insert(2, 4) |> insert(1, 5)) |> toAssocList)

    @Test
    def remove05(): Unit \ Assert =
        assertEq(expected = (1, Set#{4, 5}) :: Nil, MultiMap.remove(2, singleton(1, 4) |> insert(2, 4) |> insert(1, 5)) |> toAssocList)

    /////////////////////////////////////////////////////////////////////////////
    // removeWithValue                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def removeWithValue01(): Unit \ Assert =
        assertEq(expected = empty(), MultiMap.removeWithValue(1, 4, (empty(): MultiMap[Int32, Int32])))

    @Test
    def removeWithValue02(): Unit \ Assert =
        assertEq(expected = Nil, MultiMap.removeWithValue(1, 4, singleton(1, 4)) |> toAssocList)

    @Test
    def removeWithValue03(): Unit \ Assert =
        assertEq(expected = (1, Set#{4}) :: Nil, MultiMap.removeWithValue(1, 1, singleton(1, 4)) |> toAssocList)

    @Test
    def removeWithValue04(): Unit \ Assert =
        assertEq(expected = (1, Set#{5}) :: (2, Set#{4}) :: Nil, MultiMap.removeWithValue(1, 4, singleton(1, 4) |> insert(2, 4) |> insert(1, 5)) |> toAssocList)

    @Test
    def removeWithValue05(): Unit \ Assert =
        assertEq(expected = (1, Set#{4, 5}) :: Nil, MultiMap.removeWithValue(2, 4, singleton(1, 4) |> insert(2, 4) |> insert(1, 5)) |> toAssocList)

    @Test
    def removeWithValue06(): Unit \ Assert =
        assertEq(expected = (1, Set#{4, 5}) :: (2, Set#{4}) :: Nil, MultiMap.removeWithValue(2, 0, singleton(1, 4) |> insert(2, 4) |> insert(1, 5)) |> toAssocList)

    /////////////////////////////////////////////////////////////////////////////
    // find                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def find01(): Unit \ Assert =
        assertEq(expected = None, MultiMap.find((k, v) -> k == v, (empty(): MultiMap[Int32, Int32])))

    @Test
    def find02(): Unit \ Assert =
        assertEq(expected = None, MultiMap.find((k, v) -> k == v, singleton(1, 2)))

    @Test
    def find03(): Unit \ Assert =
        assertEq(expected = Some((1, 1)), MultiMap.find((k, v) -> k == v, singleton(1, 1)))

    @Test
    def find04(): Unit \ Assert =
        assertEq(expected = None, MultiMap.find((k, v) -> k == v, singleton(1, 4) |> insert(2, 4) |> insert(1, 5)))

    @Test
    def find05(): Unit \ Assert =
        assertEq(expected = Some((2, 2)), MultiMap.find((k, v) -> k == v, singleton(1, 4) |> insert(2, 2) |> insert(3, 3)))

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findLeft01(): Unit \ Assert =
        assertEq(expected = None, MultiMap.findLeft((k, v) -> k == v, (empty(): MultiMap[Int32, Int32])))

    @Test
    def findLeft02(): Unit \ Assert =
        assertEq(expected = None, MultiMap.findLeft((k, v) -> k == v, singleton(1, 2)))

    @Test
    def findLeft03(): Unit \ Assert =
        assertEq(expected = Some((1, 1)), MultiMap.findLeft((k, v) -> k == v, singleton(1, 1)))

    @Test
    def findLeft04(): Unit \ Assert =
        assertEq(expected = None, MultiMap.findLeft((k, v) -> k == v, singleton(1, 4) |> insert(2, 4) |> insert(1, 5)))

    @Test
    def findLeft05(): Unit \ Assert =
        assertEq(expected = Some((2, 2)), MultiMap.findLeft((k, v) -> k == v, singleton(1, 4) |> insert(2, 2) |> insert(3, 3)))

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findRight01(): Unit \ Assert =
        assertEq(expected = None, MultiMap.findRight((k, v) -> k == v, (empty(): MultiMap[Int32, Int32])))

    @Test
    def findRight02(): Unit \ Assert =
        assertEq(expected = None, MultiMap.findRight((k, v) -> k == v, singleton(1, 2)))

    @Test
    def findRight03(): Unit \ Assert =
        assertEq(expected = Some((1, 1)), MultiMap.findRight((k, v) -> k == v, singleton(1, 1)))

    @Test
    def findRight04(): Unit \ Assert =
        assertEq(expected = None, MultiMap.findRight((k, v) -> k == v, singleton(1, 4) |> insert(2, 4) |> insert(1, 5)))

    @Test
    def findRight05(): Unit \ Assert =
        assertEq(expected = Some((3, 3)), MultiMap.findRight((k, v) -> k == v, singleton(1, 4) |> insert(2, 2) |> insert(3, 3)))

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filter01(): Unit \ Assert =
        assertEq(expected = empty(), MultiMap.filter(isEven, (empty(): MultiMap[Int32, Int32])))

    @Test
    def filter02(): Unit \ Assert =
        assertEq(expected = (1, Set#{2}) :: Nil, MultiMap.filter(isEven, singleton(1, 2)) |> toAssocList)

    @Test
    def filter03(): Unit \ Assert =
        assertEq(expected = empty(), MultiMap.filter(isEven, singleton(1, 1)))

    @Test
    def filter04(): Unit \ Assert =
        assertEq(expected = (1, Set#{4}) :: (2, Set#{4}) :: Nil, MultiMap.filter(isEven, singleton(1, 4) |> insert(2, 4)) |> toAssocList)

    @Test
    def filter05(): Unit \ Assert =
        assertEq(expected = (1, Set#{4}) :: (2, Set#{2}) :: Nil, MultiMap.filter(isEven, singleton(1, 4) |> insert(2, 2) |> insert(1, 3)) |> toAssocList)


    /////////////////////////////////////////////////////////////////////////////
    // filterWithKey                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterWithKey01(): Unit \ Assert =
        assertEq(expected = empty(), MultiMap.filterWithKey((k, v) -> k == v, (empty(): MultiMap[Int32, Int32])))

    @Test
    def filterWithKey02(): Unit \ Assert =
        assertEq(expected = empty(), MultiMap.filterWithKey((k, v) -> k == v, singleton(1, 2)))

    @Test
    def filterWithKey03(): Unit \ Assert =
        assertEq(expected = (1, Set#{1}) :: Nil, MultiMap.filterWithKey((k, v) -> k == v, singleton(1, 1)) |> toAssocList)

    @Test
    def filterWithKey04(): Unit \ Assert =
        assertEq(expected = empty(), MultiMap.filterWithKey((k, v) -> k == v, singleton(1, 4) |> insert(2, 4)))

    @Test
    def filterWithKey05(): Unit \ Assert =
        assertEq(expected = (1, Set#{1}) :: (2, Set#{2}) :: Nil, MultiMap.filterWithKey((k, v) -> k == v, singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) |> toAssocList)

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map01(): Unit \ Assert =
        assertEq(expected = empty(), MultiMap.map(v -> v*3, (empty(): MultiMap[Int32, Int32])))

    @Test
    def map02(): Unit \ Assert =
        assertEq(expected = (1, Set#{6}) :: Nil, MultiMap.map(v -> v*3, singleton(1, 2)) |> toAssocList)

    @Test
    def map03(): Unit \ Assert =
        assertEq(expected = (1, Set#{3}) :: Nil, MultiMap.map(v -> v*3, singleton(1, 1)) |> toAssocList)

    @Test
    def map04(): Unit \ Assert =
        assertEq(expected = (1, Set#{12}) :: (2, Set#{12}) :: Nil, MultiMap.map(v -> v*3, singleton(1, 4) |> insert(2, 4)) |> toAssocList)

    @Test
    def map05(): Unit \ Assert =
        assertEq(expected = (1, Set#{3, 12}) :: (2, Set#{6}) :: Nil, MultiMap.map(v -> v*3, singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) |> toAssocList)


    /////////////////////////////////////////////////////////////////////////////
    // mapWithKey                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapWithKey01(): Unit \ Assert =
        assertEq(expected = empty(), MultiMap.mapWithKey((k, v) -> v*3+k, (empty(): MultiMap[Int32, Int32])))

    @Test
    def mapWithKey02(): Unit \ Assert =
        assertEq(expected = (1, Set#{7}) :: Nil, MultiMap.mapWithKey((k, v) -> v*3+k, singleton(1, 2)) |> toAssocList)

    @Test
    def mapWithKey03(): Unit \ Assert =
        assertEq(expected = (1, Set#{4}) :: Nil, MultiMap.mapWithKey((k, v) -> v*3+k, singleton(1, 1)) |> toAssocList)

    @Test
    def mapWithKey04(): Unit \ Assert =
        assertEq(expected = (1, Set#{13}) :: (2, Set#{14}) :: Nil, MultiMap.mapWithKey((k, v) -> v*3+k, singleton(1, 4) |> insert(2, 4)) |> toAssocList)

    @Test
    def mapWithKey05(): Unit \ Assert =
        assertEq(expected = (1, Set#{4, 13}) :: (2, Set#{8}) :: Nil, MultiMap.mapWithKey((k, v) -> v*3+k, singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) |> toAssocList)

    /////////////////////////////////////////////////////////////////////////////
    // foldWithKey                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldWithKey01(): Unit \ Assert =
        assertEq(expected = 0, MultiMap.foldWithKey((acc, k, v) -> acc + k + v, 0, (empty(): MultiMap[Int32, Int32])))

    @Test
    def foldWithKey02(): Unit \ Assert =
        assertEq(expected = 3, MultiMap.foldWithKey((acc, k, v) -> acc + k + v, 0, singleton(1, 2)))

    @Test
    def foldWithKey03(): Unit \ Assert =
        assertEq(expected = 11, MultiMap.foldWithKey((acc, k, v) -> acc + k + v, 0, singleton(1, 4) |> insert(2, 4)))

    @Test
    def foldWithKey04(): Unit \ Assert =
        assertEq(expected = 11, MultiMap.foldWithKey((acc, k, v) -> acc + k + v, 0, singleton(1, 4) |> insert(2, 2) |> insert(1, 1)))

    @Test
    def foldWithKey05(): Unit \ Assert =
        assertEq(expected = 17, MultiMap.foldWithKey((acc, k, v) -> acc + k + v, 0, singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)))

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeft01(): Unit \ Assert =
        assertEq(expected = Nil, MultiMap.foldLeft((acc, v) -> v :: acc, Nil, (empty(): MultiMap[Int32, Int32])))

    @Test
    def foldLeft02(): Unit \ Assert =
        assertEq(expected = 2 :: Nil, MultiMap.foldLeft((acc, v) -> v :: acc, Nil, singleton(1, 2)))

    @Test
    def foldLeft03(): Unit \ Assert =
        assertEq(expected = 5 :: 4 :: Nil, MultiMap.foldLeft((acc, v) -> v :: acc, Nil, singleton(1, 4) |> insert(2, 5)))

    @Test
    def foldLeft04(): Unit \ Assert =
        assertEq(expected = 2 :: 4 :: 1 :: Nil, MultiMap.foldLeft((acc, v) -> v :: acc, Nil, singleton(1, 4) |> insert(2, 2) |> insert(1, 1)))

    @Test
    def foldLeft05(): Unit \ Assert =
        assertEq(expected = 3 :: 2 :: 4 :: 1 :: Nil, MultiMap.foldLeft((acc, v) -> v :: acc, Nil, singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)))


    /////////////////////////////////////////////////////////////////////////////
    // foldLeftWithKey                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeftWithKey01(): Unit \ Assert =
        assertEq(expected = Nil, MultiMap.foldLeftWithKey((acc, k, v) -> (k, v) :: acc, Nil, (empty(): MultiMap[Int32, Int32])))

    @Test
    def foldLeftWithKey02(): Unit \ Assert =
        assertEq(expected = (1, 2) :: Nil, MultiMap.foldLeftWithKey((acc, k, v) -> (k, v) :: acc, Nil, singleton(1, 2)))

    @Test
    def foldLeftWithKey03(): Unit \ Assert =
        assertEq(expected = (2, 5) :: (1, 4) :: Nil, MultiMap.foldLeftWithKey((acc, k, v) -> (k, v) :: acc, Nil, singleton(1, 4) |> insert(2, 5)))

    @Test
    def foldLeftWithKey04(): Unit \ Assert =
        assertEq(expected = (2, 2) :: (1, 4) :: (1, 1) :: Nil, MultiMap.foldLeftWithKey((acc, k, v) -> (k, v) :: acc, Nil, singleton(1, 4) |> insert(2, 2) |> insert(1, 1)))

    @Test
    def foldLeftWithKey05(): Unit \ Assert =
        assertEq(expected = (3, 3) :: (2, 2) :: (1, 4) :: (1, 1) :: Nil, MultiMap.foldLeftWithKey((acc, k, v) -> (k, v) :: acc, Nil, singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)))

    @Test
    def foldLeftWithKey06(): Unit \ Assert =
        assertEq(expected = (3, 3) :: (2, 2) :: (1, 4) :: (1, 2) :: (1, 1) :: Nil, MultiMap.foldLeftWithKey((acc, k, v) -> (k, v) :: acc, Nil, singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3) |> insert(1, 2)))

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRight01(): Unit \ Assert =
        assertEq(expected = Nil, MultiMap.foldRight((v, acc) -> v :: acc, Nil, (empty(): MultiMap[Int32, Int32])))

    @Test
    def foldRight02(): Unit \ Assert =
        assertEq(expected = 2 :: Nil, MultiMap.foldRight((v, acc) -> v :: acc, Nil, singleton(1, 2)))

    @Test
    def foldRight03(): Unit \ Assert =
        assertEq(expected = 4 :: 5 :: Nil, MultiMap.foldRight((v, acc) -> v :: acc, Nil, singleton(1, 4) |> insert(2, 5)))

    @Test
    def foldRight04(): Unit \ Assert =
        assertEq(expected = 1 :: 4 :: 2 :: Nil, MultiMap.foldRight((v, acc) -> v :: acc, Nil, singleton(1, 4) |> insert(2, 2) |> insert(1, 1)))

    @Test
    def foldRight05(): Unit \ Assert =
        assertEq(expected = 1 :: 4 :: 2 :: 3 :: Nil, MultiMap.foldRight((v, acc) -> v :: acc, Nil, singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)))

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithKey                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRightWithKey01(): Unit \ Assert =
        assertEq(expected = Nil, MultiMap.foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil, (empty(): MultiMap[Int32, Int32])))

    @Test
    def foldRightWithKey02(): Unit \ Assert =
        assertEq(expected = (1, 2) :: Nil, MultiMap.foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil, singleton(1, 2)))

    @Test
    def foldRightWithKey03(): Unit \ Assert =
        assertEq(expected = (1, 4) :: (2, 5) :: Nil, MultiMap.foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil, singleton(1, 4) |> insert(2, 5)))

    @Test
    def foldRightWithKey04(): Unit \ Assert =
        assertEq(expected = (1, 1) :: (1, 4) :: (2, 2) :: Nil, MultiMap.foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil, singleton(1, 4) |> insert(2, 2) |> insert(1, 1)))

    @Test
    def foldRightWithKey05(): Unit \ Assert =
        assertEq(expected = (1, 1) :: (1, 4) :: (2, 2) :: (3, 3) :: Nil, MultiMap.foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil, singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)))

    @Test
    def foldRightWithKey06(): Unit \ Assert =
        assertEq(expected = (1, 1) :: (1, 2) :: (1, 4) :: (2, 2) :: (3, 3) :: Nil, MultiMap.foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil, singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3) |> insert(1, 2)))

    /////////////////////////////////////////////////////////////////////////////
    // foldMapWithKey                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldMapWithKey01(): Unit \ Assert =
        assertEq(expected = 0, MultiMap.foldMapWithKey((k, v) -> k + v, MultiMap(Map#{})))

    @Test
    def foldMapWithKey02(): Unit \ Assert =
        assertEq(expected = (1 + 2 + 3 + 5), MultiMap.foldMapWithKey((k, v) -> k + v, MultiMap(Map#{1 => Set#{2}, 3 => Set#{5}})))

    @Test
    def foldMapWithKey03(): Unit \ Assert =
        assertEq(expected = (1 + 2 + 1 + 3 + 4 + 5), MultiMap.foldMapWithKey((k, v) -> k + v, MultiMap(Map#{1 => Set#{2, 3}, 4 => Set#{5}})))

    @Test
    def foldMapWithKey04(): Unit \ Assert =
        assertEq(expected = (2 * (1 + 2) + 2 * (3 + 5)), MultiMap.foldMapWithKey((k, v) -> 2 * (k + v), MultiMap(Map#{1 => Set#{2}, 3 => Set#{5}})))

    @Test
    def foldMapWithKey05(): Unit \ Assert =
        assertEq(expected = ((2 * (1 + 2)) + (2 * (1 + 3)) + 2 * (4 + 5)), MultiMap.foldMapWithKey((k, v) -> 2 * (k + v), MultiMap(Map#{1 => Set#{2, 3}, 4 => Set#{5}})))


    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldMap01(): Unit \ Assert =
        assertEq(expected = 0, MultiMap.foldMap(v -> v + 1, MultiMap(Map#{})))

    @Test
    def foldMap02(): Unit \ Assert =
        assertEq(expected = (2 + 1) + (5 + 1), MultiMap.foldMap(v -> v + 1, MultiMap(Map#{1 => Set#{2}, 3 => Set#{5}})))

    @Test
    def foldMap03(): Unit \ Assert =
        assertEq(expected = (2 + 1) + (3 + 1) + (5 + 1), MultiMap.foldMap(v -> v + 1, MultiMap(Map#{1 => Set#{2, 3}, 4 => Set#{5}})))

    @Test
    def foldMap04(): Unit \ Assert =
        assertEq(expected = (2 * 2) + (2 * 5), MultiMap.foldMap(v -> 2 * v, MultiMap(Map#{1 => Set#{2}, 3 => Set#{5}})))

    @Test
    def foldMap05(): Unit \ Assert =
        assertEq(expected = (2 * 2) + (2 * 3) + (2 * 5), MultiMap.foldMap(v -> 2 * v, MultiMap(Map#{1 => Set#{2, 3}, 4 => Set#{5}})))


    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceLeft01(): Unit \ Assert =
        assertEq(expected = None, MultiMap.reduceLeft((acc, v) -> acc - v, (empty(): MultiMap[Int32, Int32])))

    @Test
    def reduceLeft02(): Unit \ Assert =
        assertEq(expected = Some(2), MultiMap.reduceLeft((acc, v) -> acc - v, singleton(1, 2)))

    @Test
    def reduceLeft03(): Unit \ Assert =
        assertEq(expected = Some(-1), MultiMap.reduceLeft((acc, v) -> acc - v, singleton(1, 4) |> insert(2, 5)))

    /// 1 - 4 - 2
    @Test
    def reduceLeft04(): Unit \ Assert =
        assertEq(expected = Some(-5), MultiMap.reduceLeft((acc, v) -> acc - v, singleton(1, 4) |> insert(2, 2) |> insert(1, 1)))

    /// 1 - 4 - 2 - 3
    @Test
    def reduceLeft05(): Unit \ Assert =
        assertEq(expected = Some(-8), MultiMap.reduceLeft((acc, v) -> acc - v, singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)))

    /// First value
    @Test
    def reduceLeft06(): Unit \ Assert =
        assertEq(expected = Some(1), MultiMap.reduceLeft((v1, _) -> v1, singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)))

    /// Last value
    @Test
    def reduceLeft07(): Unit \ Assert =
        assertEq(expected = Some(3), MultiMap.reduceLeft((_, v2) -> v2, singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)))


    /////////////////////////////////////////////////////////////////////////////
    // reduceLeftWithKey                                                       //
    /////////////////////////////////////////////////////////////////////////////

    /// Leftmost key, rightmost value

    @Test
    def reduceLeftWithKey01(): Unit \ Assert =
        assertEq(expected = None, MultiMap.reduceLeftWithKey((k1, _, _, v2) -> (k1, v2), (empty(): MultiMap[Int32, Int32])))

    @Test
    def reduceLeftWithKey02(): Unit \ Assert =
        assertEq(expected = Some((1, 2)), MultiMap.reduceLeftWithKey((k1, _, _, v2) -> (k1, v2), singleton(1, 2)))

    @Test
    def reduceLeftWithKey03(): Unit \ Assert =
        assertEq(expected = Some((1, 1)), MultiMap.reduceLeftWithKey((k1, _, _, v2) -> (k1, v2), singleton(1, 1)))

    @Test
    def reduceLeftWithKey04(): Unit \ Assert =
        assertEq(expected = Some((1, 5)), MultiMap.reduceLeftWithKey((k1, _, _, v2) -> (k1, v2), singleton(1, 4) |> insert(2, 5)))

    @Test
    def reduceLeftWithKey05(): Unit \ Assert =
        assertEq(expected = Some((1, 2)), MultiMap.reduceLeftWithKey((k1, _, _, v2) -> (k1, v2), singleton(1, 4) |> insert(2, 2) |> insert(1, 1)))

    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceRight01(): Unit \ Assert =
        assertEq(expected = None, MultiMap.reduceRight((v, acc) -> acc - v, (empty(): MultiMap[Int32, Int32])))

    @Test
    def reduceRight02(): Unit \ Assert =
        assertEq(expected = Some(2), MultiMap.reduceRight((v, acc) -> acc - v, singleton(1, 2)))

    @Test
    def reduceRight03(): Unit \ Assert =
        assertEq(expected = Some(1), MultiMap.reduceRight((v, acc) -> acc - v, singleton(1, 4) |> insert(2, 5)))

    @Test
    def reduceRight04(): Unit \ Assert =
        assertEq(expected = Some(-3), MultiMap.reduceRight((v, acc) -> acc - v, singleton(1, 4) |> insert(2, 2) |> insert(1, 1)))

    /// 3 - 2 - 4 - 1
    @Test
    def reduceRight05(): Unit \ Assert =
        assertEq(expected = Some(-4), MultiMap.reduceRight((v, acc) -> acc - v, singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)))

    /// First value
    @Test
    def reduceRight06(): Unit \ Assert =
        assertEq(expected = Some(3), MultiMap.reduceRight((_, acc) -> acc, singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)))

    /// Last value
    @Test
    def reduceRight07(): Unit \ Assert =
        assertEq(expected = Some(1), MultiMap.reduceRight((v, _) -> v, singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)))


    /////////////////////////////////////////////////////////////////////////////
    // reduceRightWithKey                                                      //
    /////////////////////////////////////////////////////////////////////////////

    /// Rightmost key, leftmost value

    @Test
    def reduceRightWithKey01(): Unit \ Assert =
        assertEq(expected = None, MultiMap.reduceRightWithKey((_, v1, k2, _) -> (k2, v1), (empty(): MultiMap[Int32, Int32])))

    @Test
    def reduceRightWithKey02(): Unit \ Assert =
        assertEq(expected = Some((1, 2)), MultiMap.reduceRightWithKey((_, v1, k2, _) -> (k2, v1), singleton(1, 2)))

    @Test
    def reduceRightWithKey03(): Unit \ Assert =
        assertEq(expected = Some((2, 4)), MultiMap.reduceRightWithKey((_, v1, k2, _) -> (k2, v1), singleton(1, 4) |> insert(2, 5)))

    @Test
    def reduceRightWithKey04(): Unit \ Assert =
        assertEq(expected = Some((2, 1)), MultiMap.reduceRightWithKey((_, v1, k2, _) -> (k2, v1), singleton(1, 4) |> insert(2, 2) |> insert(1, 1)))

    @Test
    def reduceRightWithKey05(): Unit \ Assert =
        assertEq(expected = Some((3, 1)), MultiMap.reduceRightWithKey((_, v1, k2, _) -> (k2, v1), singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)))

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def count01(): Unit \ Assert =
        assertEq(expected = 0, MultiMap.count((k, v) -> v > k, (empty(): MultiMap[Int32, Int32])))

    @Test
    def count02(): Unit \ Assert =
        assertEq(expected = 1, MultiMap.count((k, v) -> v > k, singleton(1, 2)))

    @Test
    def count03(): Unit \ Assert =
        assertEq(expected = 2, MultiMap.count((k, v) -> v > k, singleton(1, 4) |> insert(2, 4)))

    @Test
    def count04(): Unit \ Assert =
        assertEq(expected = 1, MultiMap.count((k, v) -> v > k, singleton(1, 4) |> insert(2, 2) |> insert(1, 1)))

    @Test
    def count05(): Unit \ Assert =
        assertEq(expected = 1, MultiMap.count((k, v) -> v > k, singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)))


    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def exists01(): Unit \ Assert =
        assertFalse(MultiMap.exists((k, v) -> v > k, (empty(): MultiMap[Int32, Int32])))

    @Test
    def exists02(): Unit \ Assert =
        assertTrue(MultiMap.exists((k, v) -> v > k, singleton(1, 2)))

    @Test
    def exists03(): Unit \ Assert =
        assertTrue(MultiMap.exists((k, v) -> v > k, singleton(1, 4) |> insert(2, 4)))

    @Test
    def exists04(): Unit \ Assert =
        assertTrue(MultiMap.exists((k, v) -> v > k, singleton(1, 4) |> insert(2, 2) |> insert(1, 1)))

    @Test
    def exists05(): Unit \ Assert =
        assertTrue(MultiMap.exists((k, v) -> v > k, singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)))

    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forAll01(): Unit \ Assert =
        assertTrue(MultiMap.forAll((k, v) -> v > k, (empty(): MultiMap[Int32, Int32])))

    @Test
    def forAll02(): Unit \ Assert =
        assertTrue(MultiMap.forAll((k, v) -> v > k, singleton(1, 2)))

    @Test
    def forAll03(): Unit \ Assert =
        assertTrue(MultiMap.forAll((k, v) -> v > k, singleton(1, 4) |> insert(2, 4)))

    @Test
    def forAll04(): Unit \ Assert =
        assertFalse(MultiMap.forAll((k, v) -> v > k, singleton(1, 4) |> insert(2, 2) |> insert(1, 1)))

    @Test
    def forAll05(): Unit \ Assert =
        assertFalse(MultiMap.forAll((k, v) -> v > k, singleton(1, 4) |> insert(2, 2) |> insert(1, 1)  |> insert(3, 3)))

    @Test
    def forAll06(): Unit \ Assert =
        assertTrue(MultiMap.forAll((k, v) -> v > k, singleton(1, 4) |> insert(2, 3) |> insert(1, 2)  |> insert(3, 4)))

    /////////////////////////////////////////////////////////////////////////////
    // union                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def union01(): Unit \ Assert =
        assertEq(expected = empty(), MultiMap.union((empty(): MultiMap[Int32, Int32]), empty()))

    @Test
    def union02(): Unit \ Assert =
        assertEq(expected = singleton(1, 2), MultiMap.union(singleton(1, 2), empty()))

    @Test
    def union03(): Unit \ Assert =
        assertEq(expected = singleton(1, 2), MultiMap.union(empty(), singleton(1, 2)))

    @Test
    def union04(): Unit \ Assert =
        assertEq(expected = (1, Set#{2}) :: (2, Set#{4}) :: Nil, MultiMap.union(empty(), singleton(1, 2) |> insert(2, 4)) |> toAssocList)

    @Test
    def union05(): Unit \ Assert =
        assertEq(expected = (1, Set#{2}) :: (2, Set#{4}) :: Nil, MultiMap.union(singleton(1, 2) |> insert(2, 4), empty()) |> toAssocList)

    @Test
    def union06(): Unit \ Assert =
        assertEq(expected = (1, Set#{2}) :: (2, Set#{4}) :: Nil, MultiMap.union(singleton(1, 2), singleton(2, 4)) |> toAssocList)

    @Test
    def union07(): Unit \ Assert =
        assertEq(expected = (1, Set#{2, 3}) :: Nil, MultiMap.union(singleton(1, 2), singleton(1, 3)) |> toAssocList)

    @Test
    def union08(): Unit \ Assert =
        assertEq(expected = (1, Set#{2, 4}) :: (2, Set#{3}) :: Nil, MultiMap.union(singleton(1, 4), singleton(1, 2) |> insert(2, 3)) |> toAssocList)

    /////////////////////////////////////////////////////////////////////////////
    // intersection                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def intersection01(): Unit \ Assert =
        assertEq(expected = empty(), MultiMap.intersection((empty(): MultiMap[Int32, Int32]), empty()))

    @Test
    def intersection02(): Unit \ Assert =
        assertEq(expected = empty(), MultiMap.intersection(singleton(1, 2), empty()))

    @Test
    def intersection03(): Unit \ Assert =
        assertEq(expected = empty(), MultiMap.intersection(empty(), singleton(1, 2)))

    @Test
    def intersection04(): Unit \ Assert =
        assertEq(expected = empty(), MultiMap.intersection(singleton(1, 2), singleton(1, 3)))

    @Test
    def intersection05(): Unit \ Assert =
        assertEq(expected = singleton(1, 2), MultiMap.intersection(singleton(1, 2), singleton(1, 2)))

    @Test
    def intersection06(): Unit \ Assert =
        assertEq(expected = singleton(1, 2), MultiMap.intersection(singleton(1, 2) |> insert(1, 3), singleton(1, 2) |> insert(1, 4)))

    @Test
    def intersection07(): Unit \ Assert =
        assertEq(expected = (1, Set#{2, 4}) :: Nil, MultiMap.intersection(singleton(1, 2) |> insert(1, 4), singleton(1, 2) |> insert(1, 4)) |> toAssocList)

    @Test
    def intersection08(): Unit \ Assert =
        assertEq(expected = (1, Set#{2, 4}) :: Nil, MultiMap.intersection(singleton(1, 2) |> insert(1, 4), singleton(1, 4) |> insert(1, 2) |> insert(1, 3)) |> toAssocList)

    /////////////////////////////////////////////////////////////////////////////
    // difference                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def difference01(): Unit \ Assert =
        assertEq(expected = empty(), MultiMap.difference((empty(): MultiMap[Int32, Int32]), empty()))

    @Test
    def difference02(): Unit \ Assert =
        assertEq(expected = singleton(1, 2), MultiMap.difference(singleton(1, 2), empty()))

    @Test
    def difference03(): Unit \ Assert =
        assertEq(expected = empty(), MultiMap.difference(empty(), singleton(1, 2)))

    @Test
    def difference04(): Unit \ Assert =
        assertEq(expected = (1, Set#{2}) :: Nil, MultiMap.difference(singleton(1, 2), singleton(1, 3)) |> toAssocList)

    @Test
    def difference05(): Unit \ Assert =
        assertEq(expected = empty(), MultiMap.difference(singleton(1, 2), singleton(1, 2)))

    @Test
    def difference06(): Unit \ Assert =
        assertEq(expected = singleton(1, 3), MultiMap.difference(singleton(1, 2) |> insert(1, 3), singleton(1, 2) |> insert(1, 4)))

    @Test
    def difference07(): Unit \ Assert =
        assertEq(expected = empty(), MultiMap.difference(singleton(1, 2) |> insert(1, 4), singleton(1, 2) |> insert(1, 4)))

    @Test
    def difference08(): Unit \ Assert =
        assertEq(expected = empty(), MultiMap.difference(singleton(1, 2) |> insert(1, 4), singleton(1, 4) |> insert(1, 2) |> insert(1, 3)))

    @Test
    def difference09(): Unit \ Assert =
        assertEq(expected = (1, Set#{3}) :: Nil, MultiMap.difference(singleton(1, 2) |> insert(1, 4) |> insert(1, 3), singleton(1, 4) |> insert(1, 2)) |> toAssocList)

    @Test
    def difference10(): Unit \ Assert =
        assertEq(expected = (1, Set#{3}) :: (2, Set#{2}) :: Nil, MultiMap.difference(singleton(1, 2) |> insert(1, 4) |> insert(1, 3) |> insert(2, 2), singleton(1, 4) |> insert(1, 2)) |> toAssocList)

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toList01(): Unit \ Assert =
        assertEq(expected = Nil, MultiMap.toList((empty(): MultiMap[Int32, Int32])))

    @Test
    def toList02(): Unit \ Assert =
        assertEq(expected = (1, 4) :: Nil, MultiMap.toList(singleton(1, 4)))

    @Test
    def toList03(): Unit \ Assert =
        assertEq(expected = (1, 4) :: (2, 4) :: Nil, MultiMap.toList(singleton(1, 4) |> insert(2, 4)))

    @Test
    def toList04(): Unit \ Assert =
        assertEq(expected = (1, 1) :: (1, 4) :: (2, 2) :: Nil, MultiMap.toList(singleton(1, 4) |> insert(2, 2) |> insert(1, 1)))

    @Test
    def toList05(): Unit \ Assert =
        assertEq(expected = (1, 1) :: (1, 4) :: (2, 2) :: (3, 3) :: Nil, MultiMap.toList(singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)))

    /////////////////////////////////////////////////////////////////////////////
    // toAscList                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toAscList01(): Unit \ Assert =
        assertEq(expected = Nil, MultiMap.toAscList((empty(): MultiMap[Int32, Int32])))

    @Test
    def toAscList02(): Unit \ Assert =
        assertEq(expected = (1, 4) :: Nil, MultiMap.toAscList(singleton(1, 4)))

    @Test
    def toAscList03(): Unit \ Assert =
        assertEq(expected = (1, 4) :: (2, 4) :: Nil, MultiMap.toAscList(singleton(1, 4) |> insert(2, 4)))

    @Test
    def toAscList04(): Unit \ Assert =
        assertEq(expected = (1, 1) :: (1, 4) :: (2, 4) :: Nil, MultiMap.toAscList(singleton(1, 4) |> insert(2, 4) |> insert(1, 1)))

    @Test
    def toAscList05(): Unit \ Assert =
        assertEq(expected = (1, 1) :: (1, 4) :: (2, 4) :: (3, 3) :: Nil, MultiMap.toAscList(singleton(1, 4) |> insert(2, 4) |> insert(1, 1) |> insert(3, 3)))

    /////////////////////////////////////////////////////////////////////////////
    // toDescList                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toDescList01(): Unit \ Assert =
        assertEq(expected = Nil, MultiMap.toDescList((empty(): MultiMap[Int32, Int32])))

    @Test
    def toDescList02(): Unit \ Assert =
        assertEq(expected = (1, 4) :: Nil, MultiMap.toDescList(singleton(1, 4)))

    @Test
    def toDescList03(): Unit \ Assert =
        assertEq(expected = (2, 4) :: (1, 4) :: Nil, MultiMap.toDescList(singleton(1, 4) |> insert(2, 4)))

    @Test
    def toDescList04(): Unit \ Assert =
        assertEq(expected = (2, 4) :: (1, 4) :: (1, 1) :: Nil, MultiMap.toDescList(singleton(1, 4) |> insert(2, 4) |> insert(1, 1)))

    @Test
    def toDescList05(): Unit \ Assert =
        assertEq(expected = (3, 3) :: (2, 4) :: (1, 4) :: (1, 1) :: Nil, MultiMap.toDescList(singleton(1, 4) |> insert(2, 4) |> insert(1, 1) |> insert(3, 3)))

    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMap01(): Unit \ Assert =
        assertEq(expected = Map#{}, MultiMap.toMap((empty(): MultiMap[Int32, Int32])))

    @Test
    def toMap02(): Unit \ Assert =
        assertEq(expected = Map#{1 => Set#{4}}, MultiMap.toMap(singleton(1, 4)))

    @Test
    def toMap03(): Unit \ Assert =
        assertEq(expected = Map#{1 => Set#{4}, 2 => Set#{4}}, MultiMap.toMap(singleton(1, 4) |> insert(2, 4)))

    @Test
    def toMap04(): Unit \ Assert =
        assertEq(expected = Map#{1 => Set#{1, 4}, 2 => Set#{4}}, MultiMap.toMap(singleton(1, 4) |> insert(2, 4) |> insert(1, 1)))

    @Test
    def toMap05(): Unit \ Assert =
        assertEq(expected = Map#{1 => Set#{1, 4}, 2 => Set#{4}, 3 => Set#{3}}, MultiMap.toMap(singleton(1, 4) |> insert(2, 4) |> insert(1, 1) |> insert(3, 3)))

    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Unit \ Assert =
        region rc {
            let rk = Ref.fresh(rc, 0);
            let rv = Ref.fresh(rc, 0);
            let m = (empty(): MultiMap[Int32, Int32]);
            MultiMap.forEach((k, v) -> { Ref.put(k, rk); Ref.put(v, rv) }, m);
            assertEq(expected = 0, Ref.get(rk));
            assertEq(expected = 0, Ref.get(rv))
        }

    @Test
    def forEach02(): Unit \ Assert =
        region rc {
            let rk = Ref.fresh(rc, 0);
            let rv = Ref.fresh(rc, 0);
            let m = singleton(1, 4);
            MultiMap.forEach((k, v) -> {Ref.put(k, rk); Ref.put(v, rv)}, m);
            assertEq(expected = 1, Ref.get(rk));
            assertEq(expected = 4, Ref.get(rv))
        }

    @Test
    def forEach03(): Unit \ Assert =
        region rc {
            let rk = Ref.fresh(rc, 0);
            let rv = Ref.fresh(rc, 0);
            let m = singleton(1, 4) |> insert(2, 4);
            MultiMap.forEach((k, v) -> {Ref.put(k, rk); Ref.put(v, rv)}, m);
            assertEq(expected = 2, Ref.get(rk));
            assertEq(expected = 4, Ref.get(rv))
        }

    @Test
    def forEach04(): Unit \ Assert =
        region rc {
            let rk = Ref.fresh(rc, 0);
            let rv = Ref.fresh(rc, 0);
            let m = singleton(1, 4) |> insert(2, 4) |> insert(1, 1);
            MultiMap.forEach((k, v) -> {Ref.put(k, rk); Ref.put(v, rv)}, m);
            assertEq(expected = 2, Ref.get(rk));
            assertEq(expected = 4, Ref.get(rv))
        }

    @Test
    def forEach05(): Unit \ Assert =
        region rc {
            let rk = Ref.fresh(rc, 0);
            let rv = Ref.fresh(rc, 0);
            let m = singleton(1, 4) |> insert(2, 4) |> insert(1, 1) |> insert(3, 3);
            MultiMap.forEach((k, v) -> {Ref.put(k, rk); Ref.put(v, rv)}, m);
            assertEq(expected = 3, Ref.get(rk));
            assertEq(expected = 3, Ref.get(rv))
        }

    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEachWithIndex01(): Unit \ Assert =
        region rc {
            let ri = Ref.fresh(rc, 0);
            let rk = Ref.fresh(rc, 0);
            let rv = Ref.fresh(rc, 0);
            let m = (empty(): MultiMap[Int32, Int32]);
            MultiMap.forEachWithIndex((i, k, v) -> {Ref.put(i, ri); Ref.put(k, rk); Ref.put(v, rv)}, m);
            assertEq(expected = 0, Ref.get(ri));
            assertEq(expected = 0, Ref.get(rk));
            assertEq(expected = 0, Ref.get(rv))
        }

    @Test
    def forEachWithIndex02(): Unit \ Assert =
        region rc {
            let ri = Ref.fresh(rc, 0);
            let rk = Ref.fresh(rc, 0);
            let rv = Ref.fresh(rc, 0);
            let m = singleton(1, 4);
            MultiMap.forEachWithIndex((i, k, v) -> {Ref.put(i, ri); Ref.put(k, rk); Ref.put(v, rv)}, m);
            assertEq(expected = 0, Ref.get(ri));
            assertEq(expected = 1, Ref.get(rk));
            assertEq(expected = 4, Ref.get(rv))
        }

    @Test
    def forEachWithIndex03(): Unit \ Assert =
        region rc {
            let ri = Ref.fresh(rc, 0);
            let rk = Ref.fresh(rc, 0);
            let rv = Ref.fresh(rc, 0);
            let m = singleton(1, 4) |> insert(2, 4);
            MultiMap.forEachWithIndex((i, k, v) -> {Ref.put(i, ri); Ref.put(k, rk); Ref.put(v, rv)}, m);
            assertEq(expected = 1, Ref.get(ri));
            assertEq(expected = 2, Ref.get(rk));
            assertEq(expected = 4, Ref.get(rv))
        }

    @Test
    def forEachWithIndex04(): Unit \ Assert =
        region rc {
            let ri = Ref.fresh(rc, 0);
            let rk = Ref.fresh(rc, 0);
            let rv = Ref.fresh(rc, 0);
            let m = singleton(1, 4) |> insert(2, 4) |> insert(1, 1);
            MultiMap.forEachWithIndex((i, k, v) -> {Ref.put(i, ri); Ref.put(k, rk); Ref.put(v, rv)}, m);
            assertEq(expected = 2, Ref.get(ri));
            assertEq(expected = 2, Ref.get(rk));
            assertEq(expected = 4, Ref.get(rv))
        }

    @Test
    def forEachWithIndex05(): Unit \ Assert =
        region rc {
            let ri = Ref.fresh(rc, 0);
            let rk = Ref.fresh(rc, 0);
            let rv = Ref.fresh(rc, 0);
            let m = singleton(1, 4) |> insert(2, 4) |> insert(1, 1) |> insert(3, 3);
            MultiMap.forEachWithIndex((i, k, v) -> {Ref.put(i, ri); Ref.put(k, rk); Ref.put(v, rv)}, m);
            assertEq(expected = 3, Ref.get(ri));
            assertEq(expected = 3, Ref.get(rk));
            assertEq(expected = 3, Ref.get(rv))
        }

}
