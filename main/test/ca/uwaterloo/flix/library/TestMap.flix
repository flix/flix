mod TestMap {

    use Assert.{assertEq, assertNeq, assertTrue, assertFalse, assertSome, assertNone};
    use ToString.toString;
    use Hash.hash;

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def size01(): Unit \ Assert = 
        assertEq(expected = 0, Map.size(Map#{}))

    @Test
    def size02(): Unit \ Assert = 
        assertEq(expected = 1, Map.size(Map#{1 => 2}))

    @Test
    def size03(): Unit \ Assert = 
        assertEq(expected = 2, Map.size(Map#{1 => 2, 2 => 4}))

    @Test
    def size04(): Unit \ Assert = 
        assertEq(expected = 3, Map.size(Map#{1 => 2, 2 => 4, 3 => 6}))

    @Test
    def size05(): Unit \ Assert = 
        assertEq(expected = 4, Map.size(Map#{1 => 2, 2 => 4, 3 => 6, 4 => 8}))

    @Test
    def size06(): Unit \ Assert = 
        assertEq(expected = 5, Map.size(Map#{1 => 2, 2 => 4, 3 => 6, 4 => 8, 5 => 10}))

    /////////////////////////////////////////////////////////////////////////////
    // empty                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def empty01(): Unit \ Assert = 
        assertEq(expected = Map#{}, (Map.empty(): Map[Unit, Unit]))

    /////////////////////////////////////////////////////////////////////////////
    // singleton                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def singleton01(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 2}, Map.singleton(1, 2))

    @Test
    def singleton02(): Unit \ Assert = 
        assertEq(expected = Map#{3 => -1}, Map.singleton(3, -1))

    @Test
    def singleton03(): Unit \ Assert = 
        assertEq(expected = Map#{-99 => -11}, Map.singleton(-99, -11))

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isEmpty01(): Unit \ Assert = 
        assertTrue(Map.isEmpty((Map#{}: Map[Unit, Unit])))

    @Test
    def isEmpty02(): Unit \ Assert = 
        assertFalse(Map.isEmpty(Map#{1 => 2}))

    @Test
    def isEmpty03(): Unit \ Assert = 
        assertFalse(Map.isEmpty(Map#{1 => 2, 2 => 4}))

    @Test
    def isEmpty04(): Unit \ Assert = 
        assertFalse(Map.isEmpty(Map#{1 => 2, 2 => 4, 3 => 6}))

    @Test
    def isEmpty05(): Unit \ Assert = 
        assertFalse(Map.isEmpty(Map#{1 => 2, 2 => 4, 3 => 6, 4 => 8}))

    /////////////////////////////////////////////////////////////////////////////
    // nonEmpty                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def nonEmpty01(): Unit \ Assert = 
        assertFalse(Map.nonEmpty((Map#{}: Map[Unit, Unit])))

    @Test
    def nonEmpty02(): Unit \ Assert = 
        assertTrue(Map.nonEmpty(Map#{1 => 2}))

    @Test
    def nonEmpty03(): Unit \ Assert = 
        assertTrue(Map.nonEmpty(Map#{1 => 2, 2 => 4}))

    @Test
    def nonEmpty04(): Unit \ Assert = 
        assertTrue(Map.nonEmpty(Map#{1 => 2, 2 => 4, 3 => 6}))

    @Test
    def nonEmpty05(): Unit \ Assert = 
        assertTrue(Map.nonEmpty(Map#{1 => 2, 2 => 4, 3 => 6, 4 => 8}))

    /////////////////////////////////////////////////////////////////////////////
    // get                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def get01(): Unit \ Assert = 
        assertEq(expected = None, Map.get(2, (Map#{}: Map[_, Unit])))

    @Test
    def get02(): Unit \ Assert = 
        assertEq(expected = None, Map.get(2, Map#{1 => 2}))

    @Test
    def get03(): Unit \ Assert = 
        assertEq(expected = Some(1), Map.get(2, Map#{2 => 1}))

    @Test
    def get04(): Unit \ Assert = 
        assertEq(expected = None, Map.get(5, Map#{2 => 1, 3 => 17}))

    @Test
    def get05(): Unit \ Assert = 
        assertEq(expected = Some(17), Map.get(5, Map#{2 => 1, 5 => 17}))

    @Test
    def get06(): Unit \ Assert = 
        assertEq(expected = Some(1), Map.get(5, Map#{5 => 1, 3 => 17}))

    @Test
    def get07(): Unit \ Assert = 
        assertEq(expected = None, Map.get(-2, Map#{2 => 1, 3 => 17, -1 => -2}))

    @Test
    def get08(): Unit \ Assert = 
        assertEq(expected = Some(1), Map.get(-2, Map#{-2 => 1, 3 => 17, -1 => -2}))

    @Test
    def get09(): Unit \ Assert = 
        assertEq(expected = Some(17), Map.get(-2, Map#{2 => 1, -2 => 17, -1 => -2}))

    @Test
    def get10(): Unit \ Assert = 
        assertEq(expected = Some(-2), Map.get(-2, Map#{2 => 1, 3 => 17, -2 => -2}))

    /////////////////////////////////////////////////////////////////////////////
    // getWithDefault                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def getWithDefault01(): Unit \ Assert = 
        assertEq(expected = 34, Map.getWithDefault(2, 34, Map#{}))

    @Test
    def getWithDefault02(): Unit \ Assert = 
        assertEq(expected = 34, Map.getWithDefault(2, 34, Map#{1 => 2}))

    @Test
    def getWithDefault03(): Unit \ Assert = 
        assertEq(expected = 1, Map.getWithDefault(2, 34, Map#{2 => 1}))

    @Test
    def getWithDefault04(): Unit \ Assert = 
        assertEq(expected = 34, Map.getWithDefault(5, 34, Map#{2 => 1, 3 => 17}))

    @Test
    def getWithDefault05(): Unit \ Assert = 
        assertEq(expected = 17, Map.getWithDefault(5, 34, Map#{2 => 1, 5 => 17}))

    @Test
    def getWithDefault06(): Unit \ Assert = 
        assertEq(expected = 1, Map.getWithDefault(5, 34, Map#{5 => 1, 3 => 17}))

    @Test
    def getWithDefault07(): Unit \ Assert = 
        assertEq(expected = 34, Map.getWithDefault(-2, 34, Map#{2 => 1, 3 => 17, -1 => -2}))

    @Test
    def getWithDefault08(): Unit \ Assert = 
        assertEq(expected = 1, Map.getWithDefault(-2, 34, Map#{-2 => 1, 3 => 17, -1 => -2}))

    @Test
    def getWithDefault09(): Unit \ Assert = 
        assertEq(expected = 17, Map.getWithDefault(-2, 34, Map#{2 => 1, -2 => 17, -1 => -2}))

    @Test
    def getWithDefault10(): Unit \ Assert = 
        assertEq(expected = -2, Map.getWithDefault(-2, 34, Map#{2 => 1, 3 => 17, -2 => -2}))

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def memberOf01(): Unit \ Assert = 
        assertFalse(Map.memberOf(2, (Map#{}: Map[_, Unit])))

    @Test
    def memberOf02(): Unit \ Assert = 
        assertFalse(Map.memberOf(2, Map#{1 => 2}))

    @Test
    def memberOf03(): Unit \ Assert = 
        assertTrue(Map.memberOf(2, Map#{2 => 1}))

    @Test
    def memberOf04(): Unit \ Assert = 
        assertFalse(Map.memberOf(5, Map#{2 => 1, 3 => 17}))

    @Test
    def memberOf05(): Unit \ Assert = 
        assertTrue(Map.memberOf(5, Map#{2 => 1, 5 => 17}))

    @Test
    def memberOf06(): Unit \ Assert = 
        assertTrue(Map.memberOf(5, Map#{5 => 1, 3 => 17}))

    @Test
    def memberOf07(): Unit \ Assert = 
        assertFalse(Map.memberOf(-2, Map#{2 => 1, 3 => 17, -1 => -2}))

    @Test
    def memberOf08(): Unit \ Assert = 
        assertTrue(Map.memberOf(-2, Map#{-2 => 1, 3 => 17, -1 => -2}))

    @Test
    def memberOf09(): Unit \ Assert = 
        assertTrue(Map.memberOf(-2, Map#{2 => 1, -2 => 17, -1 => -2}))

    @Test
    def memberOf10(): Unit \ Assert = 
        assertTrue(Map.memberOf(-2, Map#{2 => 1, 3 => 17, -2 => -2}))

    /////////////////////////////////////////////////////////////////////////////
    // keysOf                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def keysOf01(): Unit \ Assert = 
        assertEq(expected = Set#{}, Map.keysOf((Map#{}: Map[Unit, Unit])))

    @Test
    def keysOf02(): Unit \ Assert = 
        assertEq(expected = Set#{1}, Map.keysOf(Map#{1 => 2}))

    @Test
    def keysOf03(): Unit \ Assert = 
        assertEq(expected = Set#{1, 2}, Map.keysOf(Map#{1 => 2, 2 => 4}))

    @Test
    def keysOf04(): Unit \ Assert = 
        assertEq(expected = Set#{1, 2, 3}, Map.keysOf(Map#{1 => 2, 2 => 4, 3 => 6}))

    @Test
    def keysOf05(): Unit \ Assert = 
        assertEq(expected = Set#{1, 2, 3, 4}, Map.keysOf(Map#{1 => 2, 2 => 4, 3 => 6, 4 => 8}))

    @Test
    def keysOf06(): Unit \ Assert = 
        assertEq(expected = Set#{1, 2, 3, 4, 5}, Map.keysOf(Map#{1 => 2, 2 => 4, 3 => 6, 4 => 8, 5 => 10}))

    /////////////////////////////////////////////////////////////////////////////
    // valuesOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def valuesOf01(): Unit \ Assert = 
        assertEq(expected = Nil, Map.valuesOf((Map#{}: Map[Unit, Unit])))

    @Test
    def valuesOf02(): Unit \ Assert = 
        assertEq(expected = 2 :: Nil, Map.valuesOf(Map#{1 => 2}))

    @Test
    def valuesOf03(): Unit \ Assert = 
        assertEq(expected = 2 :: 4 :: Nil, Map.valuesOf(Map#{1 => 2, 2 => 4}))

    @Test
    def valuesOf04(): Unit \ Assert = 
        assertEq(expected = 2 :: 4 :: 6 :: Nil, Map.valuesOf(Map#{1 => 2, 2 => 4, 3 => 6}))

    @Test
    def valuesOf05(): Unit \ Assert = 
        assertEq(expected = 2 :: 4 :: 6 :: 8 :: Nil, Map.valuesOf(Map#{1 => 2, 2 => 4, 3 => 6, 4 => 8}))

    @Test
    def valuesOf06(): Unit \ Assert = 
        assertEq(expected = 2 :: 4 :: 6 :: 8 :: 10 :: Nil, Map.valuesOf(Map#{1 => 2, 2 => 4, 3 => 6, 4 => 8, 5 => 10}))

    @Test
    def valuesOf07(): Unit \ Assert = 
        assertEq(expected = -11 :: 4 :: -5 :: 7 :: -5 :: Nil, Map.valuesOf(Map#{1 => -11, 2 => 4, 3 => -5, 4 => 7, 5 => -5}))

    /////////////////////////////////////////////////////////////////////////////
    // insert                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def insert01(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 3}, Map.insert(1, 3, Map#{}))

    @Test
    def insert02(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 3}, Map.insert(1, 3, Map#{1 => 4}))

    @Test
    def insert03(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 4, 2 => 3}, Map.insert(2, 3, Map#{1 => 4}))

    @Test
    def insert04(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 1, 5 => -2}, Map.insert(1, 1, Map#{1 => 4, 5 => -2}))

    @Test
    def insert05(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 4, 5 => 1}, Map.insert(5, 1, Map#{1 => 4, 5 => -2}))

    @Test
    def insert06(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 4, 5 => -2, 4 => -2}, Map.insert(4, -2, Map#{1 => 4, 5 => -2}))

    /////////////////////////////////////////////////////////////////////////////
    // insertWith                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def insertWith01(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 3}, Map.insertWith((v1, v2) -> v1 + v2, 1, 3, Map#{}))

    @Test
    def insertWith02(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 7}, Map.insertWith((v1, v2) -> v1 + v2, 1, 3, Map#{1 => 4}))

    @Test
    def insertWith03(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 4, 2 => 3}, Map.insertWith((v1, v2) -> v1 + v2, 2, 3, Map#{1 => 4}))

    @Test
    def insertWith04(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 5, 5 => -2}, Map.insertWith((v1, v2) -> v1 + v2, 1, 1, Map#{1 => 4, 5 => -2}))

    @Test
    def insertWith05(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 4, 5 => -1}, Map.insertWith((v1, v2) -> v1 + v2, 5, 1, Map#{1 => 4, 5 => -2}))

    @Test
    def insertWith06(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 4, 5 => -2, 4 => -2}, Map.insertWith((v1, v2) -> v1 + v2, 4, -2, Map#{1 => 4, 5 => -2}))

    /////////////////////////////////////////////////////////////////////////////
    // insertWithKey                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def insertWithKey01(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 3}, Map.insertWithKey((k, v1, v2) -> k + v1 + v2, 1, 3, Map#{}))

    @Test
    def insertWithKey02(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 8}, Map.insertWithKey((k, v1, v2) -> k + v1 + v2, 1, 3, Map#{1 => 4}))

    @Test
    def insertWithKey03(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 4, 2 => 3}, Map.insertWithKey((k, v1, v2) -> k + v1 + v2, 2, 3, Map#{1 => 4}))

    @Test
    def insertWithKey04(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 6, 5 => -2}, Map.insertWithKey((k, v1, v2) -> k + v1 + v2, 1, 1, Map#{1 => 4, 5 => -2}))

    @Test
    def insertWithKey05(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 4, 5 => 4}, Map.insertWithKey((k, v1, v2) -> k + v1 + v2, 5, 1, Map#{1 => 4, 5 => -2}))

    @Test
    def insertWithKey06(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 4, 5 => -2, 4 => -2}, Map.insertWithKey((k, v1, v2) -> k + v1 + v2, 4, -2, Map#{1 => 4, 5 => -2}))

    /////////////////////////////////////////////////////////////////////////////
    // adjust                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def adjust01(): Unit \ Assert = 
        assertEq(expected = Map#{}, Map.adjust(v -> 2*v, 1, Map#{}))

    @Test
    def adjust02(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 8}, Map.adjust(v -> 2*v, 1, Map#{1 => 4}))

    @Test
    def adjust03(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 4}, Map.adjust(v -> 2*v, 2, Map#{1 => 4}))

    @Test
    def adjust04(): Unit \ Assert = 
        assertEq(expected = Map#{1 => -28, 5 => -2}, Map.adjust(v -> 2*v, 1, Map#{1 => -14, 5 => -2}))

    @Test
    def adjust05(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 4, 5 => -4}, Map.adjust(v -> 2*v, 5, Map#{1 => 4, 5 => -2}))

    @Test
    def adjust06(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 4, 5 => -2}, Map.adjust(v -> 2*v, 4, Map#{1 => 4, 5 => -2}))

    /////////////////////////////////////////////////////////////////////////////
    // adjustWithKey                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def adjustWithKey01(): Unit \ Assert = 
        assertEq(expected = Map#{}, Map.adjustWithKey((k, v) -> k+v, 1, Map#{}))

    @Test
    def adjustWithKey02(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 5}, Map.adjustWithKey((k, v) -> k+v, 1, Map#{1 => 4}))

    @Test
    def adjustWithKey03(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 4}, Map.adjustWithKey((k, v) -> k+v, 2, Map#{1 => 4}))

    @Test
    def adjustWithKey04(): Unit \ Assert = 
        assertEq(expected = Map#{1 => -13, 5 => -2}, Map.adjustWithKey((k, v) -> k+v, 1, Map#{1 => -14, 5 => -2}))

    @Test
    def adjustWithKey05(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 4, 5 => 3}, Map.adjustWithKey((k, v) -> k+v, 5, Map#{1 => 4, 5 => -2}))

    @Test
    def adjustWithKey06(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 4, 5 => -2}, Map.adjustWithKey((k, v) -> k+v, 4, Map#{1 => 4, 5 => -2}))

    /////////////////////////////////////////////////////////////////////////////
    // update                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def update01(): Unit \ Assert = 
        assertEq(expected = Map#{}, Map.update(v -> if (v `Int32.remainder` 2 != 0) Some(2*v) else None, 1, Map#{}))

    @Test
    def update02(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 6}, Map.update(v -> if (v `Int32.remainder` 2 != 0) Some(2*v) else None, 1, Map#{1 => 3}))

    @Test
    def update03(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 4}, Map.update(v -> if (v `Int32.remainder` 2 != 0) Some(2*v) else None, 1, Map#{1 => 4}))

    @Test
    def update04(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 4}, Map.update(v -> if (v `Int32.remainder` 2 != 0) Some(2*v) else None, 2, Map#{1 => 4}))

    @Test
    def update05(): Unit \ Assert = 
        assertEq(expected = Map#{1 => -14, 5 => -2}, Map.update(v -> if (v `Int32.remainder` 2 != 0) Some(2*v) else None, 1, Map#{1 => -14, 5 => -2}))

    @Test
    def update06(): Unit \ Assert = 
        assertEq(expected = Map#{1 => -26, 5 => -2}, Map.update(v -> if (v `Int32.remainder` 2 != 0) Some(2*v) else None, 1, Map#{1 => -13, 5 => -2}))

    @Test
    def update07(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 4, 5 => -2}, Map.update(v -> if (v `Int32.remainder` 2 != 0) Some(2*v) else None, 5, Map#{1 => 4, 5 => -2}))

    @Test
    def update08(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 4, 5 => -2}, Map.update(v -> if (v `Int32.remainder` 2 != 0) Some(2*v) else None, 5, Map#{1 => 4, 5 => -1}))

    @Test
    def update09(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 4, 5 => -2}, Map.update(v -> if (v `Int32.remainder` 2 != 0) Some(2*v) else None, 4, Map#{1 => 4, 5 => -2}))

    /////////////////////////////////////////////////////////////////////////////
    // updateWithKey                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def updateWithKey01(): Unit \ Assert = 
        assertEq(expected = Map#{}, Map.updateWithKey((k, v) -> if (v `Int32.remainder` 2 != 0) Some(k+2*v) else None, 1, Map#{}))

    @Test
    def updateWithKey02(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 7}, Map.updateWithKey((k, v) -> if (v `Int32.remainder` 2 != 0) Some(k+2*v) else None, 1, Map#{1 => 3}))

    @Test
    def updateWithKey03(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 4}, Map.updateWithKey((k, v) -> if (v `Int32.remainder` 2 != 0) Some(k+2*v) else None, 1, Map#{1 => 4}))

    @Test
    def updateWithKey04(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 4}, Map.updateWithKey((k, v) -> if (v `Int32.remainder` 2 != 0) Some(k+2*v) else None, 2, Map#{1 => 4}))

    @Test
    def updateWithKey05(): Unit \ Assert = 
        assertEq(expected = Map#{1 => -14, 5 => -2}, Map.updateWithKey((k, v) -> if (v `Int32.remainder` 2 != 0) Some(k+2*v) else None, 1, Map#{1 => -14, 5 => -2}))

    @Test
    def updateWithKey06(): Unit \ Assert = 
        assertEq(expected = Map#{1 => -25, 5 => -2}, Map.updateWithKey((k, v) -> if (v `Int32.remainder` 2 != 0) Some(k+2*v) else None, 1, Map#{1 => -13, 5 => -2}))

    @Test
    def updateWithKey07(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 4, 5 => -2}, Map.updateWithKey((k, v) -> if (v `Int32.remainder` 2 != 0) Some(k+2*v) else None, 5, Map#{1 => 4, 5 => -2}))

    @Test
    def updateWithKey08(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 4, 5 => 3}, Map.updateWithKey((k, v) -> if (v `Int32.remainder` 2 != 0) Some(k+2*v) else None, 5, Map#{1 => 4, 5 => -1}))

    @Test
    def updateWithKey09(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 4, 5 => -2}, Map.updateWithKey((k, v) -> if (v `Int32.remainder` 2 != 0) Some(k+2*v) else None, 4, Map#{1 => 4, 5 => -2}))

    /////////////////////////////////////////////////////////////////////////////
    // remove                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def remove01(): Unit \ Assert = 
        assertEq(expected = Map#{}, Map.remove(1, (Map#{}: Map[_, Unit])))

    @Test
    def remove02(): Unit \ Assert = 
        assertEq(expected = Map#{2 => 4}, Map.remove(1, Map#{2 => 4}))

    @Test
    def remove03(): Unit \ Assert = 
        assertEq(expected = Map#{}, Map.remove(1, Map#{1 => 4}))

    @Test
    def remove04(): Unit \ Assert = 
        assertEq(expected = Map#{6 => 1, 2 => 4}, Map.remove(1, Map#{6 => 1, 2 => 4}))

    @Test
    def remove05(): Unit \ Assert = 
        assertEq(expected = Map#{2 => 4}, Map.remove(6, Map#{6 => 1, 2 => 4}))

    @Test
    def remove06(): Unit \ Assert = 
        assertEq(expected = Map#{6 => 1}, Map.remove(2, Map#{6 => 1, 2 => 4}))

    @Test
    def remove07(): Unit \ Assert = 
        assertEq(expected = Map#{6 => 1, 2 => 4, 8 => 89}, Map.remove(1, Map#{6 => 1, 2 => 4, 8 => 89}))

    @Test
    def remove08(): Unit \ Assert = 
        assertEq(expected = Map#{2 => 4, 8 => 89}, Map.remove(6, Map#{6 => 1, 2 => 4, 8 => 89}))

    @Test
    def remove09(): Unit \ Assert = 
        assertEq(expected = Map#{6 => 1, 8 => 89}, Map.remove(2, Map#{6 => 1, 2 => 4, 8 => 89}))

    @Test
    def remove10(): Unit \ Assert = 
        assertEq(expected = Map#{6 => 1, 2 => 4}, Map.remove(8, Map#{6 => 1, 2 => 4, 8 => 89}))

    /////////////////////////////////////////////////////////////////////////////
    // isSubmapOf                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isSubmapOf01(): Unit \ Assert = 
        assertTrue(Map.isSubmapOf((Map#{}: Map[Unit, Unit]), Map#{}))

    @Test
    def isSubmapOf02(): Unit \ Assert = 
        assertFalse(Map.isSubmapOf(Map#{1 => 2}, Map#{}))

    @Test
    def isSubmapOf03(): Unit \ Assert = 
        assertFalse(Map.isSubmapOf(Map#{3 => 4}, Map#{}))

    @Test
    def isSubmapOf04(): Unit \ Assert = 
        assertTrue(Map.isSubmapOf(Map#{1 => 3}, Map#{1 => 3}))

    @Test
    def isSubmapOf05(): Unit \ Assert = 
        assertFalse(Map.isSubmapOf(Map#{2 => 3}, Map#{1 => 3}))

    @Test
    def isSubmapOf06(): Unit \ Assert = 
        assertFalse(Map.isSubmapOf(Map#{1 => 4}, Map#{1 => 3}))

    @Test
    def isSubmapOf07(): Unit \ Assert = 
        assertFalse(Map.isSubmapOf(Map#{1 => 3, 2 => 3}, Map#{1 => 3}))

    @Test
    def isSubmapOf08(): Unit \ Assert = 
        assertFalse(Map.isSubmapOf(Map#{1 => 3, 2 => 3}, Map#{2 => 3}))

    @Test
    def isSubmapOf09(): Unit \ Assert = 
        assertTrue(Map.isSubmapOf(Map#{}, Map#{2 => 4, 1 => 3}))

    @Test
    def isSubmapOf10(): Unit \ Assert = 
        assertTrue(Map.isSubmapOf(Map#{2 => 4}, Map#{2 => 4, 1 => 3}))

    @Test
    def isSubmapOf11(): Unit \ Assert = 
        assertTrue(Map.isSubmapOf(Map#{1 => 3}, Map#{2 => 4, 1 => 3}))

    @Test
    def isSubmapOf12(): Unit \ Assert = 
        assertTrue(Map.isSubmapOf(Map#{1 => 3, 2 => 4}, Map#{1 => 3, 2 => 4}))

    @Test
    def isSubmapOf13(): Unit \ Assert = 
        assertTrue(Map.isSubmapOf(Map#{2 => 4, 1 => 3}, Map#{1 => 3, 2 => 4}))

    @Test
    def isSubmapOf14(): Unit \ Assert = 
        assertFalse(Map.isSubmapOf(Map#{2 => 5, 1 => 3}, Map#{1 => 3, 2 => 4}))

    @Test
    def isSubmapOf15(): Unit \ Assert = 
        assertFalse(Map.isSubmapOf(Map#{2 => 4, 1 => -1}, Map#{1 => 3, 2 => 4}))

    @Test
    def isSubmapOf16(): Unit \ Assert = 
        assertFalse(Map.isSubmapOf(Map#{3 => 4, 1 => 3}, Map#{1 => 3, 2 => 4}))

    @Test
    def isSubmapOf17(): Unit \ Assert = 
        assertFalse(Map.isSubmapOf(Map#{2 => 4, 3 => 3}, Map#{1 => 3, 2 => 4}))

    @Test
    def isSubmapOf18(): Unit \ Assert = 
        assertFalse(Map.isSubmapOf(Map#{2 => 4, 1 => 3, 8 => 9}, Map#{1 => 3, 2 => 4}))

    /////////////////////////////////////////////////////////////////////////////
    // isProperSubmapOf                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isProperSubmapOf01(): Unit \ Assert = 
        assertFalse(Map.isProperSubmapOf((Map#{}: Map[Unit, Unit]), Map#{}))

    @Test
    def isProperSubmapOf02(): Unit \ Assert = 
        assertFalse(Map.isProperSubmapOf(Map#{1 => 2}, Map#{}))

    @Test
    def isProperSubmapOf03(): Unit \ Assert = 
        assertFalse(Map.isProperSubmapOf(Map#{3 => 4}, Map#{}))

    @Test
    def isProperSubmapOf04(): Unit \ Assert = 
        assertFalse(Map.isProperSubmapOf(Map#{1 => 3}, Map#{1 => 3}))

    @Test
    def isProperSubmapOf05(): Unit \ Assert = 
        assertFalse(Map.isProperSubmapOf(Map#{2 => 3}, Map#{1 => 3}))

    @Test
    def isProperSubmapOf06(): Unit \ Assert = 
        assertFalse(Map.isProperSubmapOf(Map#{1 => 4}, Map#{1 => 3}))

    @Test
    def isProperSubmapOf07(): Unit \ Assert = 
        assertFalse(Map.isProperSubmapOf(Map#{1 => 3, 2 => 3}, Map#{1 => 3}))

    @Test
    def isProperSubmapOf08(): Unit \ Assert = 
        assertFalse(Map.isProperSubmapOf(Map#{1 => 3, 2 => 3}, Map#{2 => 3}))

    @Test
    def isProperSubmapOf09(): Unit \ Assert = 
        assertTrue(Map.isProperSubmapOf(Map#{}, Map#{2 => 4, 1 => 3}))

    @Test
    def isProperSubmapOf10(): Unit \ Assert = 
        assertTrue(Map.isProperSubmapOf(Map#{2 => 4}, Map#{2 => 4, 1 => 3}))

    @Test
    def isProperSubmapOf11(): Unit \ Assert = 
        assertTrue(Map.isProperSubmapOf(Map#{1 => 3}, Map#{2 => 4, 1 => 3}))

    @Test
    def isProperSubmapOf12(): Unit \ Assert = 
        assertFalse(Map.isProperSubmapOf(Map#{1 => 3, 2 => 4}, Map#{1 => 3, 2 => 4}))

    @Test
    def isProperSubmapOf13(): Unit \ Assert = 
        assertFalse(Map.isProperSubmapOf(Map#{2 => 4, 1 => 3}, Map#{1 => 3, 2 => 4}))

    @Test
    def isProperSubmapOf14(): Unit \ Assert = 
        assertFalse(Map.isProperSubmapOf(Map#{2 => 5, 1 => 3}, Map#{1 => 3, 2 => 4}))

    @Test
    def isProperSubmapOf15(): Unit \ Assert = 
        assertFalse(Map.isProperSubmapOf(Map#{2 => 4, 1 => -1}, Map#{1 => 3, 2 => 4}))

    @Test
    def isProperSubmapOf16(): Unit \ Assert = 
        assertFalse(Map.isProperSubmapOf(Map#{3 => 4, 1 => 3}, Map#{1 => 3, 2 => 4}))

    @Test
    def isProperSubmapOf17(): Unit \ Assert = 
        assertFalse(Map.isProperSubmapOf(Map#{2 => 4, 3 => 3}, Map#{1 => 3, 2 => 4}))

    @Test
    def isProperSubmapOf18(): Unit \ Assert = 
        assertFalse(Map.isProperSubmapOf(Map#{2 => 4, 1 => 3, 8 => 9}, Map#{1 => 3, 2 => 4}))

    /////////////////////////////////////////////////////////////////////////////
    // find                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def find01(): Unit \ Assert = 
        assertEq(expected = None, Map.find((k, v) -> k == v, (Map#{}: Map[Unit, Unit])))

    @Test
    def find02(): Unit \ Assert = 
        assertEq(expected = None, Map.find((k, v) -> k == v, Map#{1 => 2}))

    @Test
    def find03(): Unit \ Assert = 
        assertEq(expected = Some((1, 1)), Map.find((k, v) -> k == v, Map#{1 => 1}))

    @Test
    def find04(): Unit \ Assert = 
        assertEq(expected = None, Map.find((k, v) -> k == v, Map#{1 => 2, 2 => 3}))

    @Test
    def find05(): Unit \ Assert = 
        assertEq(expected = Some((1, 1)), Map.find((k, v) -> k == v, Map#{1 => 1, 2 => 3}))

    @Test
    def find06(): Unit \ Assert = 
        assertEq(expected = Some((2, 2)), Map.find((k, v) -> k == v, Map#{1 => 2, 2 => 2}))

    @Test
    def find07(): Unit \ Assert = 
        assertEq(expected = Some((1, 1)), Map.find((k, v) -> k == v, Map#{1 => 1, 2 => 2}))

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findLeft01(): Unit \ Assert = 
        assertEq(expected = None, Map.findLeft((k, v) -> k == v, (Map#{}: Map[Unit, Unit])))

    @Test
    def findLeft02(): Unit \ Assert = 
        assertEq(expected = None, Map.findLeft((k, v) -> k == v, Map#{1 => 2}))

    @Test
    def findLeft03(): Unit \ Assert = 
        assertEq(expected = Some((1, 1)), Map.findLeft((k, v) -> k == v, Map#{1 => 1}))

    @Test
    def findLeft04(): Unit \ Assert = 
        assertEq(expected = None, Map.findLeft((k, v) -> k == v, Map#{1 => 2, 2 => 3}))

    @Test
    def findLeft05(): Unit \ Assert = 
        assertEq(expected = Some((1, 1)), Map.findLeft((k, v) -> k == v, Map#{1 => 1, 2 => 3}))

    @Test
    def findLeft06(): Unit \ Assert = 
        assertEq(expected = Some((2, 2)), Map.findLeft((k, v) -> k == v, Map#{1 => 2, 2 => 2}))

    @Test
    def findLeft07(): Unit \ Assert = 
        assertEq(expected = Some((1, 1)), Map.findLeft((k, v) -> k == v, Map#{1 => 1, 2 => 2}))

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findRight01(): Unit \ Assert = 
        assertEq(expected = None, Map.findRight((k, v) -> k == v, (Map#{}: Map[Unit, Unit])))

    @Test
    def findRight02(): Unit \ Assert = 
        assertEq(expected = None, Map.findRight((k, v) -> k == v, Map#{1 => 2}))

    @Test
    def findRight03(): Unit \ Assert = 
        assertEq(expected = Some((1, 1)), Map.findRight((k, v) -> k == v, Map#{1 => 1}))

    @Test
    def findRight04(): Unit \ Assert = 
        assertEq(expected = None, Map.findRight((k, v) -> k == v, Map#{1 => 2, 2 => 3}))

    @Test
    def findRight05(): Unit \ Assert = 
        assertEq(expected = Some((1, 1)), Map.findRight((k, v) -> k == v, Map#{1 => 1, 2 => 3}))

    @Test
    def findRight06(): Unit \ Assert = 
        assertEq(expected = Some((2, 2)), Map.findRight((k, v) -> k == v, Map#{1 => 2, 2 => 2}))

    @Test
    def findRight07(): Unit \ Assert = 
        assertEq(expected = Some((2, 2)), Map.findRight((k, v) -> k == v, Map#{1 => 1, 2 => 2}))

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filter01(): Unit \ Assert = 
        assertEq(expected = Map#{}, Map.filter(v -> v `Int32.remainder` 2 == 0, (Map#{}: Map[Unit, _])))

    @Test
    def filter02(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 2}, Map.filter(v -> v `Int32.remainder` 2 == 0, Map#{1 => 2}))

    @Test
    def filter03(): Unit \ Assert = 
        assertEq(expected = Map#{}, Map.filter(v -> v `Int32.remainder` 2 == 0, Map#{1 => 3}))

    @Test
    def filter04(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 8, 2 => 4}, Map.filter(v -> v `Int32.remainder` 2 == 0, Map#{1 => 8, 2 => 4}))

    @Test
    def filter05(): Unit \ Assert = 
        assertEq(expected = Map#{2 => 4}, Map.filter(v -> v `Int32.remainder` 2 == 0, Map#{1 => 7, 2 => 4}))

    @Test
    def filter06(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 8}, Map.filter(v -> v `Int32.remainder` 2 == 0, Map#{1 => 8, 2 => 3}))

    @Test
    def filter07(): Unit \ Assert = 
        assertEq(expected = Map#{}, Map.filter(v -> v `Int32.remainder` 2 == 0, Map#{1 => -1, 2 => -3}))

    /////////////////////////////////////////////////////////////////////////////
    // filterWithKey                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterWithKey01(): Unit \ Assert = 
        assertEq(expected = Map#{}, Map.filterWithKey((k, v) -> k == v, (Map#{}: Map[Unit, Unit])))

    @Test
    def filterWithKey02(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 1}, Map.filterWithKey((k, v) -> k == v, Map#{1 => 1}))

    @Test
    def filterWithKey03(): Unit \ Assert = 
        assertEq(expected = Map#{}, Map.filterWithKey((k, v) -> k == v, Map#{1 => 3}))

    @Test
    def filterWithKey04(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 1, 2 => 2}, Map.filterWithKey((k, v) -> k == v, Map#{1 => 1, 2 => 2}))

    @Test
    def filterWithKey05(): Unit \ Assert = 
        assertEq(expected = Map#{2 => 2}, Map.filterWithKey((k, v) -> k == v, Map#{1 => 7, 2 => 2}))

    @Test
    def filterWithKey06(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 1}, Map.filterWithKey((k, v) -> k == v, Map#{1 => 1, 2 => 3}))

    @Test
    def filterWithKey07(): Unit \ Assert = 
        assertEq(expected = Map#{}, Map.filterWithKey((k, v) -> k == v, Map#{1 => -1, 2 => -3}))

    /////////////////////////////////////////////////////////////////////////////
    // filterMap                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterMap01(): Unit \ Assert = 
        assertEq(expected = Map#{}, Map.filterMap(x -> if (x > 1) Some("${x}") else None, (Map#{}: Map[Unit, _])))

    @Test
    def filterMap02(): Unit \ Assert = 
        assertEq(expected = Map#{}, Map.filterMap(x -> if (x > 1) Some("${x}") else None, Map#{1 => 1}))

    @Test
    def filterMap03(): Unit \ Assert =
        assertEq(expected = Map#{2 => "2", 3 => "3", 4 => "4"}, Map.filterMap(x -> if (x > 1) Some("${x}") else None, Map#{1 => 1, 2 => 2, 3 => 3, 4 => 4}))

    /////////////////////////////////////////////////////////////////////////////
    // filterMapWithKey                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterMapWithKey01(): Unit \ Assert = 
        assertEq(expected = Map#{}, Map.filterMapWithKey((k, v) -> if (k == v and v > 1) Some("${v}") else None, (Map#{}: Map[Int32, Int32])))

    @Test
    def filterMapWithKey02(): Unit \ Assert = 
        assertEq(expected = Map#{}, Map.filterMapWithKey((k, v) -> if (k == v and v > 1) Some("${v}") else None, Map#{1 => 1}))

    @Test
    def filterMapWithKey03(): Unit \ Assert =
        assertEq(expected = Map#{2 => "2", 3 => "3", 4 => "4"}, Map.filterMapWithKey((k, v) -> if (k == v and v > 1) Some("${v}") else None, Map#{1 => 1, 2 => 2, 3 => 3, 4 => 4}))

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map01(): Unit \ Assert = 
        assertEq(expected = Map#{}, Map.map(v -> 3*v, (Map#{}: Map[Unit, Int32])))

    @Test
    def map02(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 12}, Map.map(v -> 3*v, Map#{1 => 4}))

    @Test
    def map03(): Unit \ Assert = 
        assertEq(expected = Map#{2 => -3, 0 => 0}, Map.map(v -> 3*v, Map#{2 => -1, 0 => 0}))

    @Test
    def map04(): Unit \ Assert = 
        assertEq(expected = Map#{2 => -3, 5 => 45, 11 => -27}, Map.map(v -> 3*v, Map#{2 => -1, 5 => 15, 11 => -9}))

    @Test
    def map05(): Unit \ Assert = 
        assertEq(expected = Map#{2 => -3, 5 => 45, 11 => -27, 8 => 24}, Map.map(v -> 3*v, Map#{2 => -1, 5 => 15, 11 => -9, 8 => 8}))

    /////////////////////////////////////////////////////////////////////////////
    // mapWithKey                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapWithKey01(): Unit \ Assert = 
        assertEq(expected = Map#{}, Map.mapWithKey((_, v) -> v, (Map#{}: Map[Unit, Unit])))

    @Test
    def mapWithKey02(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 5}, Map.mapWithKey((k, v) -> k + v, Map#{1 => 4}))

    @Test
    def mapWithKey03(): Unit \ Assert = 
        assertEq(expected = Map#{2 => 1, 0 => 0}, Map.mapWithKey((k, v) -> k + v, Map#{2 => -1, 0 => 0}))

    @Test
    def mapWithKey04(): Unit \ Assert = 
        assertEq(expected = Map#{2 => 1, 5 => 20, 11 => 2}, Map.mapWithKey((k, v) -> k + v, Map#{2 => -1, 5 => 15, 11 => -9}))

    @Test
    def mapWithKey05(): Unit \ Assert = 
        assertEq(expected = Map#{2 => 1, 5 => 20, 11 => 2, 8 => 16}, Map.mapWithKey((k, v) -> k + v, Map#{2 => -1, 5 => 15, 11 => -9, 8 => 8}))

    @Test
    def mapWithKey06(): Unit \ Assert = region rc {
        let range = List.range(0, 2000);
        let m = List.zip(range, range) |> List.toMap;
        let a = MutList.empty(rc);
        discard Map.mapWithKey((k, v) -> { let b = MutList.empty(rc); MutList.push(k, b); MutList.append(MutList.toList(b), a); v }, m);
        assertEq(expected = range, MutList.toList(a))
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldWithKey                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldWithKey01(): Unit \ Assert = 
        assertEq(expected = 0, Map.foldWithKey((k, s, v) -> k + s + v, 0, Map#{}))

    @Test
    def foldWithKey02(): Unit \ Assert = 
        assertEq(expected = 3, Map.foldWithKey((k, s, v) -> k + s + v, 0, Map#{1 => 2}))

    @Test
    def foldWithKey03(): Unit \ Assert = 
        assertEq(expected = 10, Map.foldWithKey((k, s, v) -> k + s + v, 0, Map#{1 => 2, 3 => 4}))

    @Test
    def foldWithKey04(): Unit \ Assert = 
        assertEq(expected = 21, Map.foldWithKey((k, s, v) -> k + s + v, 0, Map#{1 => 2, 3 => 4, 5 => 6}))

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeft01(): Unit \ Assert = 
        assertEq(expected = 0, Map.foldLeft((s, v) -> s + v, 0, Map#{}))

    @Test
    def foldLeft02(): Unit \ Assert = 
        assertEq(expected = 2, Map.foldLeft((s, v) -> s + v, 0, Map#{1 => 2}))

    @Test
    def foldLeft03(): Unit \ Assert = 
        assertEq(expected = 6, Map.foldLeft((s, v) -> s + v, 0, Map#{1 => 2, 3 => 4}))

    @Test
    def foldLeft04(): Unit \ Assert = 
        assertEq(expected = 12, Map.foldLeft((s, v) -> s + v, 0, Map#{1 => 2, 3 => 4, 5 => 6}))

    /////////////////////////////////////////////////////////////////////////////
    // foldLeftWithKey                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeftWithKey01(): Unit \ Assert = 
        assertEq(expected = 0, Map.foldLeftWithKey((s, k, v) -> k + s + v, 0, Map#{}))

    @Test
    def foldLeftWithKey02(): Unit \ Assert = 
        assertEq(expected = 3, Map.foldLeftWithKey((s, k, v) -> k + s + v, 0, Map#{1 => 2}))

    @Test
    def foldLeftWithKey03(): Unit \ Assert = 
        assertEq(expected = 10, Map.foldLeftWithKey((s, k, v) -> k + s + v, 0, Map#{1 => 2, 3 => 4}))

    @Test
    def foldLeftWithKey04(): Unit \ Assert = 
        assertEq(expected = 21, Map.foldLeftWithKey((s, k, v) -> k + s + v, 0, Map#{1 => 2, 3 => 4, 5 => 6}))

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRight01(): Unit \ Assert = 
        assertEq(expected = 0, Map.foldRight((v, acc) -> acc + v, 0, Map#{}))

    @Test
    def foldRight02(): Unit \ Assert = 
        assertEq(expected = 2, Map.foldRight((v, acc) -> acc + v, 0, Map#{1 => 2}))

    @Test
    def foldRight03(): Unit \ Assert = 
        assertEq(expected = 6, Map.foldRight((v, acc) -> acc + v, 0, Map#{1 => 2, 3 => 4}))

    @Test
    def foldRight04(): Unit \ Assert = 
        assertEq(expected = 12, Map.foldRight((v, acc) -> acc + v, 0, Map#{1 => 2, 3 => 4, 5 => 6}))

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithKey                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRightWithKey01(): Unit \ Assert = 
        assertEq(expected = 0, Map.foldRightWithKey((k, v, acc) -> k + acc + v, 0, Map#{}))

    @Test
    def foldRightWithKey02(): Unit \ Assert = 
        assertEq(expected = 3, Map.foldRightWithKey((k, v, acc) -> k + acc + v, 0, Map#{1 => 2}))

    @Test
    def foldRightWithKey03(): Unit \ Assert = 
        assertEq(expected = 10, Map.foldRightWithKey((k, v, acc) -> k + acc + v, 0, Map#{1 => 2, 3 => 4}))

    @Test
    def foldRightWithKey04(): Unit \ Assert = 
        assertEq(expected = 21, Map.foldRightWithKey((k, v, acc) -> k + acc + v, 0, Map#{1 => 2, 3 => 4, 5 => 6}))

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRightWithCont01(): Unit \ Assert = 
        assertEq(expected = 0, Map.foldRightWithCont((v, k) -> k() + v, 0, Map#{}))

    @Test
    def foldRightWithCont02(): Unit \ Assert = 
        assertEq(expected = 2, Map.foldRightWithCont((v, k) -> k() + v, 0, Map#{1 => 2}))

    @Test
    def foldRightWithCont03(): Unit \ Assert = 
        assertEq(expected = 6, Map.foldRightWithCont((v, k) -> k() + v, 0, Map#{1 => 2, 3 => 4}))

    @Test
    def foldRightWithCont04(): Unit \ Assert = 
        assertEq(expected = 12, Map.foldRightWithCont((v, k) -> k() + v, 0, Map#{1 => 2, 3 => 4, 5 => 6}))

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithKeyCont                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRightWithKeyCont01(): Unit \ Assert = 
        assertEq(expected = 0, Map.foldRightWithKeyCont((k, v, c) -> k + c() + v, 0, Map#{}))

    @Test
    def foldRightWithKeyCont02(): Unit \ Assert = 
        assertEq(expected = 3, Map.foldRightWithKeyCont((k, v, c) -> k + c() + v, 0, Map#{1 => 2}))

    @Test
    def foldRightWithKeyCont03(): Unit \ Assert = 
        assertEq(expected = 10, Map.foldRightWithKeyCont((k, v, c) -> k + c() + v, 0, Map#{1 => 2, 3 => 4}))

    @Test
    def foldRightWithKeyCont04(): Unit \ Assert = 
        assertEq(expected = 21, Map.foldRightWithKeyCont((k, v, c) -> k + c() + v, 0, Map#{1 => 2, 3 => 4, 5 => 6}))

    /////////////////////////////////////////////////////////////////////////////
    // foldMapWithKey                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldMapWithKey01(): Unit \ Assert = 
        assertEq(expected = 0, Map.foldMapWithKey((k, v) -> k + v, Map#{}))

    @Test
    def foldMapWithKey02(): Unit \ Assert = 
        assertEq(expected = (1 + 2 + 3 + 5), Map.foldMapWithKey((k, v) -> k + v, Map#{1 => 2, 3 => 5}))

    @Test
    def foldMapWithKey03(): Unit \ Assert =
        assertEq(expected = (2 * (1 + 2) + 2 * (3 + 5)), Map.foldMapWithKey((k, v) -> 2 * (k + v), Map#{1 => 2, 3 => 5}))

    @Test
    def foldMapWithKey04(): Unit \ Assert = 
        assertEq(expected = "1a2b", Map.foldMapWithKey((k, v) -> "${k}" + v, Map#{1 => "a", 2 => "b"}))

    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldMap01(): Unit \ Assert =
        assertEq(expected = 0, Map.foldMap(v -> v + 1, Map#{}))

    @Test
    def foldMap02(): Unit \ Assert =
        assertEq(expected = (2 + 1) + (5 + 1), Map.foldMap(v -> v + 1, Map#{1 => 2, 3 => 5}))

    @Test
    def foldMap03(): Unit \ Assert =
        assertEq(expected = (2 * 2) + (2 * 5), Map.foldMap(v -> 2 * v, Map#{1 => 2, 3 => 5}))

    @Test
    def foldMap04(): Unit \ Assert =
        assertEq(expected = "xaxb", Map.foldMap(v -> "x" + v, Map#{1 => "a", 2 => "b"}))

    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceLeft01(): Unit \ Assert = 
        assertEq(expected = None, Map.reduceLeft((v1, v2) -> v1 - v2, (Map#{}: Map[Int32, Int32])))

    @Test
    def reduceLeft02(): Unit \ Assert = 
        assertEq(expected = Some(2), Map.reduceLeft((v1, v2) -> v1 - v2, Map#{1 => 2}))

    @Test
    def reduceLeft03(): Unit \ Assert = 
        assertEq(expected = Some(-1), Map.reduceLeft((v1, v2) -> v1 - v2, Map#{1 => 2, 2 => 3}))

    @Test
    def reduceLeft04(): Unit \ Assert = 
        assertEq(expected = Some(-5), Map.reduceLeft((v1, v2) -> v1 - v2, Map#{1 => 2, 2 => 3, 3 => 4}))

    /////////////////////////////////////////////////////////////////////////////
    // reduceLeftWithKey                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceLeftWithKey01(): Unit \ Assert = 
        assertEq(expected = None, Map.reduceLeftWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), (Map#{}: Map[Int32, Int32])))

    @Test
    def reduceLeftWithKey02(): Unit \ Assert = 
        assertEq(expected = Some((1, 2)), Map.reduceLeftWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), Map#{1 => 2}))

    @Test
    def reduceLeftWithKey03(): Unit \ Assert = 
        assertEq(expected = Some((-1, -1)), Map.reduceLeftWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), Map#{1 => 2, 2 => 3}))

    @Test
    def reduceLeftWithKey04(): Unit \ Assert = 
        assertEq(expected = Some((-4, -5)), Map.reduceLeftWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), Map#{1 => 2, 2 => 3, 3 => 4}))

    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceRight01(): Unit \ Assert = 
        assertEq(expected = None, Map.reduceRight((v1, v2) -> v1 - v2, (Map#{}: Map[Int32, Int32])))

    @Test
    def reduceRight02(): Unit \ Assert = 
        assertEq(expected = Some(2), Map.reduceRight((v1, v2) -> v1 - v2, Map#{1 => 2}))

    @Test
    def reduceRight03(): Unit \ Assert = 
        assertEq(expected = Some(-1), Map.reduceRight((v1, v2) -> v1 - v2, Map#{1 => 2, 2 => 3}))

    @Test
    def reduceRight04(): Unit \ Assert = 
        assertEq(expected = Some(3), Map.reduceRight((v1, v2) -> v1 - v2, Map#{1 => 2, 2 => 3, 3 => 4}))

    /////////////////////////////////////////////////////////////////////////////
    // reduceRightWithKey                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceRightWithKey01(): Unit \ Assert = 
        assertEq(expected = None, Map.reduceRightWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), (Map#{}: Map[Int32, Int32])))

    @Test
    def reduceRightWithKey02(): Unit \ Assert = 
        assertEq(expected = Some((1, 2)), Map.reduceRightWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), Map#{1 => 2}))

    @Test
    def reduceRightWithKey03(): Unit \ Assert = 
        assertEq(expected = Some((-1, -1)), Map.reduceRightWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), Map#{1 => 2, 2 => 3}))

    @Test
    def reduceRightWithKey04(): Unit \ Assert = 
        assertEq(expected = Some((2 , 3)), Map.reduceRightWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), Map#{1 => 2, 2 => 3, 3 => 4}))

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def count01(): Unit \ Assert = 
        assertEq(expected = 0, Map.count((k, v) -> k == v, (Map#{}: Map[Unit, Unit])))

    @Test
    def count02(): Unit \ Assert = 
        assertEq(expected = 0, Map.count((k, v) -> k == v, Map#{1 => 2}))

    @Test
    def count03(): Unit \ Assert = 
        assertEq(expected = 1, Map.count((k, v) -> k == v, Map#{1 => 1}))

    @Test
    def count04(): Unit \ Assert = 
        assertEq(expected = 0, Map.count((k, v) -> k == v, Map#{1 => 2, 2 => 3}))

    @Test
    def count05(): Unit \ Assert = 
        assertEq(expected = 1, Map.count((k, v) -> k == v, Map#{1 => 1, 2 => 3}))

    @Test
    def count06(): Unit \ Assert = 
        assertEq(expected = 1, Map.count((k, v) -> k == v, Map#{1 => 2, 2 => 2}))

    @Test
    def count07(): Unit \ Assert = 
        assertEq(expected = 2, Map.count((k, v) -> k == v, Map#{1 => 1, 2 => 2}))

    @Test
    def count08(): Unit \ Assert = region rc {
        let range = List.range(0, 2000);
        let m = List.zip(range, range) |> List.toMap;
        let a = MutList.empty(rc);
        discard Map.count((k, _) -> { let b = MutList.empty(rc); MutList.push(k, b); MutList.append(MutList.toList(b), a); true }, m);
        assertEq(expected = range, MutList.toList(a))
    }


    /////////////////////////////////////////////////////////////////////////////
    // sumValues                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumValues01(): Unit \ Assert =
        assertEq(expected = 0, Map.empty() |> Map.sumValues)

    @Test
    def sumValues02(): Unit \ Assert =
        assertEq(expected = 1, Map#{1 => 1} |> Map.sumValues)

    @Test
    def sumValues03(): Unit \ Assert =
        assertEq(expected = 6, Map#{1 => 1, 2 => 2, 3 => 3} |> Map.sumValues)

    @Test
    def sumValues04(): Unit \ Assert =
        assertEq(expected = 3, Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3} |> Map.sumValues)

    @Test
    def sumValues05(): Unit \ Assert =
        assertEq(expected = -10, Map#{-1 => -1, -2 => -2, -3 => -3, -4 => -4} |> Map.sumValues)

    @Test
    def sumValues06(): Unit \ Assert =
        assertEq(expected = 0, Map#{10 => 10, -10 => -10} |> Map.sumValues)

    @Test
    def sumValues07(): Unit \ Assert =
        assertEq(expected = 5050, List.range(1, 101) |> List.zip(List.range(1, 101)) |> List.toMap |> Map.sumValues)


    /////////////////////////////////////////////////////////////////////////////
    // sumKeys                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumKeys01(): Unit \ Assert =
        assertEq(expected = 0, Map.empty() |> Map.sumKeys)

    @Test
    def sumKeys02(): Unit \ Assert =
        assertEq(expected = 1, Map#{1 => 1} |> Map.sumKeys)

    @Test
    def sumKeys03(): Unit \ Assert =
        assertEq(expected = 6, Map#{1 => 1, 2 => 2, 3 => 3} |> Map.sumKeys)

    @Test
    def sumKeys04(): Unit \ Assert =
        assertEq(expected = 3, Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3} |> Map.sumKeys)

    @Test
    def sumKeys05(): Unit \ Assert =
        assertEq(expected = -12, Map#{-1 => -1, -2 => -2, -5 => -3, -4 => -4} |> Map.sumKeys)

    @Test
    def sumKeys06(): Unit \ Assert =
        assertEq(expected = 0, Map#{10 => 10, -10 => -10} |> Map.sumKeys)

    @Test
    def sumKeys07(): Unit \ Assert =
        assertEq(expected = 5050, List.range(1, 101) |> List.zip(List.range(1, 101)) |> List.toMap |> Map.sumKeys)


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumWith01(): Unit \ Assert =
        assertEq(expected = 0, Map.empty() |> Map.sumWith((k, v) -> k + v))

    @Test
    def sumWith02(): Unit \ Assert =
        assertEq(expected = 2, Map#{1 => 1} |> Map.sumWith((k, v) -> k + v))

    @Test
    def sumWith03(): Unit \ Assert =
        assertEq(expected = 12, Map#{1 => 1, 2 => 2, 3 => 3} |> Map.sumWith((k, v) -> k + v))

    @Test
    def sumWith04(): Unit \ Assert =
        assertEq(expected = 6, Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3} |> Map.sumWith((k, v) -> k + v))

    @Test
    def sumWith05(): Unit \ Assert =
        assertEq(expected = -20, Map#{-1 => -1, -2 => -2, -3 => -3, -4 => -4} |> Map.sumWith((k, v) -> k + v))

    @Test
    def sumWith06(): Unit \ Assert =
        assertEq(expected = 0, Map#{10 => 10, -10 => -10} |> Map.sumWith((k, v) -> k + v))

    @Test
    def sumWith07(): Unit \ Assert = region rc {
        let range = List.range(0, 2000);
        let m = List.zip(range, range) |> List.toMap;
        let a = MutList.empty(rc);
        discard Map.sumWith((k, _) -> { let b = MutList.empty(rc); MutList.push(k, b); MutList.append(MutList.toList(b), a); 1 }, m);
        assertEq(expected = range, MutList.toList(a))
    }


    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def exists01(): Unit \ Assert = 
        assertFalse(Map.exists((k, v) -> k == v, (Map#{}: Map[Unit, Unit])))

    @Test
    def exists02(): Unit \ Assert = 
        assertFalse(Map.exists((k, v) -> k == v, Map#{1 => 2}))

    @Test
    def exists03(): Unit \ Assert = 
        assertTrue(Map.exists((k, v) -> k == v, Map#{1 => 1}))

    @Test
    def exists04(): Unit \ Assert = 
        assertFalse(Map.exists((k, v) -> k == v, Map#{1 => 2, 2 => 3}))

    @Test
    def exists05(): Unit \ Assert = 
        assertTrue(Map.exists((k, v) -> k == v, Map#{1 => 1, 2 => 3}))

    @Test
    def exists06(): Unit \ Assert = 
        assertTrue(Map.exists((k, v) -> k == v, Map#{1 => 2, 2 => 2}))

    @Test
    def exists07(): Unit \ Assert = 
        assertTrue(Map.exists((k, v) -> k == v, Map#{1 => 1, 2 => 2}))

    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forAll01(): Unit \ Assert = 
        assertTrue(Map.forAll((k, v) -> k == v, (Map#{}: Map[Unit, Unit])))

    @Test
    def forAll02(): Unit \ Assert = 
        assertFalse(Map.forAll((k, v) -> k == v, Map#{1 => 2}))

    @Test
    def forAll03(): Unit \ Assert = 
        assertTrue(Map.forAll((k, v) -> k == v, Map#{1 => 1}))

    @Test
    def forAll04(): Unit \ Assert = 
        assertFalse(Map.forAll((k, v) -> k == v, Map#{1 => 2, 2 => 3}))

    @Test
    def forAll05(): Unit \ Assert = 
        assertFalse(Map.forAll((k, v) -> k == v, Map#{1 => 1, 2 => 3}))

    @Test
    def forAll06(): Unit \ Assert = 
        assertFalse(Map.forAll((k, v) -> k == v, Map#{1 => 2, 2 => 2}))

    @Test
    def forAll07(): Unit \ Assert = 
        assertTrue(Map.forAll((k, v) -> k == v, Map#{1 => 1, 2 => 2}))

    /////////////////////////////////////////////////////////////////////////////
    // union                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def union01(): Unit \ Assert = 
        assertEq(expected = Map#{}, Map.union((Map#{}: Map[Unit, Unit]), Map#{}))

    @Test
    def union02(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 2}, Map.union(Map#{1 => 2}, Map#{}))

    @Test
    def union03(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 2}, Map.union(Map#{}, Map#{1 => 2}))

    @Test
    def union04(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 2, 3 => 4}, Map.union(Map#{}, Map#{1 => 2, 3 => 4}))

    @Test
    def union05(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 2, 3 => 4}, Map.union(Map#{1 => 2, 3 => 4}, Map#{}))

    @Test
    def union06(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 2, 3 => 4}, Map.union(Map#{1 => 2}, Map#{3 => 4}))

    @Test
    def union07(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 2}, Map.union(Map#{1 => 2}, Map#{1 => 5}))

    @Test
    def union08(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 2, 2 => 3, 3 => 4}, Map.union(Map#{}, Map#{1 => 2, 2 => 3, 3 => 4}))

    @Test
    def union09(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 2, 2 => 3, 3 => 4}, Map.union(Map#{1 => 2, 2 => 3, 3 => 4}, Map#{}))

    @Test
    def union10(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 2, 2 => 3, 3 => 4}, Map.union(Map#{1 => 2, 2 => 3}, Map#{3 => 4}))

    @Test
    def union11(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 2, 2 => 3, 3 => 4}, Map.union(Map#{3 => 4}, Map#{1 => 2, 2 => 3}))

    @Test
    def union12(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 2, 2 => 3}, Map.union(Map#{1 => 2, 2 => 3}, Map#{2 => 8}))

    @Test
    def union13(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 2, 2 => 3}, Map.union(Map#{1 => 2, 2 => 3}, Map#{1 => 8}))

    @Test
    def union14(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 4, 2 => 3}, Map.union(Map#{1 => 4}, Map#{1 => 2, 2 => 3}))

    @Test
    def union15(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 2, 2 => 7}, Map.union(Map#{2 => 7}, Map#{1 => 2, 2 => 3}))

    @Test
    def union16(): Unit \ Assert = 
        assertEq(expected = Map#{2 => 7, 11 => 14, 9 => 8, 15 => 22, 1 => 2, 44 => 33}, Map.union(Map#{2 => 7, 11 => 14, 9 => 8, 15 => 22}, Map#{15 => 21, 1 => 2, 2 => 8, 44 => 33}))

    /////////////////////////////////////////////////////////////////////////////
    // unionWith                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unionWith01(): Unit \ Assert = 
        assertEq(expected = (Map#{}: Map[Int32, Int32]), Map.unionWith((v1, v2) -> v1 - v2, Map#{}, Map#{}))

    @Test
    def unionWith02(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 2}, Map.unionWith((v1, v2) -> v1 - v2, Map#{1 => 2}, Map#{}))

    @Test
    def unionWith03(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 2}, Map.unionWith((v1, v2) -> v1 - v2, Map#{}, Map#{1 => 2}))

    @Test
    def unionWith04(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 2, 3 => 4}, Map.unionWith((v1, v2) -> v1 - v2, Map#{}, Map#{1 => 2, 3 => 4}))

    @Test
    def unionWith05(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 2, 3 => 4}, Map.unionWith((v1, v2) -> v1 - v2, Map#{1 => 2, 3 => 4}, Map#{}))

    @Test
    def unionWith06(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 2, 3 => 4}, Map.unionWith((v1, v2) -> v1 - v2, Map#{1 => 2}, Map#{3 => 4}))

    @Test
    def unionWith07(): Unit \ Assert = 
        assertEq(expected = Map#{1 => -3}, Map.unionWith((v1, v2) -> v1 - v2, Map#{1 => 2}, Map#{1 => 5}))

    @Test
    def unionWith08(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 2, 2 => 3, 3 => 4}, Map.unionWith((v1, v2) -> v1 - v2, Map#{}, Map#{1 => 2, 2 => 3, 3 => 4}))

    @Test
    def unionWith09(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 2, 2 => 3, 3 => 4}, Map.unionWith((v1, v2) -> v1 - v2, Map#{1 => 2, 2 => 3, 3 => 4}, Map#{}))

    @Test
    def unionWith10(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 2, 2 => 3, 3 => 4}, Map.unionWith((v1, v2) -> v1 - v2, Map#{1 => 2, 2 => 3}, Map#{3 => 4}))

    @Test
    def unionWith11(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 2, 2 => 3, 3 => 4}, Map.unionWith((v1, v2) -> v1 - v2, Map#{3 => 4}, Map#{1 => 2, 2 => 3}))

    @Test
    def unionWith12(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 2, 2 => -5}, Map.unionWith((v1, v2) -> v1 - v2, Map#{1 => 2, 2 => 3}, Map#{2 => 8}))

    @Test
    def unionWith13(): Bool = Map.unionWith((v1, v2) -> v1 - v2, Map#{1 => 2, 2 => 3}, Map#{1 => 8}) == Map#{1 => -6, 2 => 3}

    @Test
    def unionWith14(): Bool = Map.unionWith((v1, v2) -> v1 - v2, Map#{1 => 4}, Map#{1 => 2, 2 => 3}) == Map#{1 => 2, 2 => 3}

    @Test
    def unionWith15(): Bool = Map.unionWith((v1, v2) -> v1 - v2, Map#{2 => 7}, Map#{1 => 2, 2 => 3}) == Map#{1 => 2, 2 => 4}

    @Test
    def unionWith16(): Bool = Map.unionWith((v1, v2) -> v1 - v2, Map#{2 => 7, 11 => 14, 9 => 8, 15 => 22}, Map#{15 => 21, 1 => 2, 2 => 8, 44 => 33})
                                       == Map#{2 => -1, 11 => 14, 9 => 8, 15 => 1, 1 => 2, 44 => 33}

    /////////////////////////////////////////////////////////////////////////////
    // unionWithKey                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unionWithKey01(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{}, Map#{}) == (Map#{}: Map[Int32, Int32])

    @Test
    def unionWithKey02(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 => 2}, Map#{}) == Map#{1 => 2}

    @Test
    def unionWithKey03(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{}, Map#{1 => 2}) == Map#{1 => 2}

    @Test
    def unionWithKey04(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{}, Map#{1 => 2, 3 => 4}) == Map#{1 => 2, 3 => 4}

    @Test
    def unionWithKey05(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 => 2, 3 => 4}, Map#{}) == Map#{1 => 2, 3 => 4}

    @Test
    def unionWithKey06(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 => 2}, Map#{3 => 4}) == Map#{1 => 2, 3 => 4}

    @Test
    def unionWithKey07(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 => 2}, Map#{1 => 5}) == Map#{1 => -2}

    @Test
    def unionWithKey08(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{}, Map#{1 => 2, 2 => 3, 3 => 4}) == Map#{1 => 2, 2 => 3, 3 => 4}

    @Test
    def unionWithKey09(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 => 2, 2 => 3, 3 => 4}, Map#{}) == Map#{1 => 2, 2 => 3, 3 => 4}

    @Test
    def unionWithKey10(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 => 2, 2 => 3}, Map#{3 => 4}) == Map#{1 => 2, 2 => 3, 3 => 4}

    @Test
    def unionWithKey11(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{3 => 4}, Map#{1 => 2, 2 => 3}) == Map#{1 => 2, 2 => 3, 3 => 4}

    @Test
    def unionWithKey12(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 => 2, 2 => 3}, Map#{2 => 8}) == Map#{1 => 2, 2 => -3}

    @Test
    def unionWithKey13(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 => 2, 2 => 3}, Map#{1 => 8}) == Map#{1 => -5, 2 => 3}

    @Test
    def unionWithKey14(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 => 4}, Map#{1 => 2, 2 => 3}) == Map#{1 => 3, 2 => 3}

    @Test
    def unionWithKey15(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{2 => 7}, Map#{1 => 2, 2 => 3}) == Map#{1 => 2, 2 => 6}

    @Test
    def unionWithKey16(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{2 => 7, 11 => 14, 9 => 8, 15 => 22}, Map#{15 => 21, 1 => 2, 2 => 8, 44 => 33})
                                       == Map#{2 => 1, 11 => 14, 9 => 8, 15 => 16, 1 => 2, 44 => 33}

    /////////////////////////////////////////////////////////////////////////////
    // intersection                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def intersection01(): Bool = Map.intersection((Map#{}: Map[Unit, Unit]), Map#{}) == Map#{}

    @Test
    def intersection02(): Bool = Map.intersection(Map#{1 => 2}, Map#{}) == Map#{}

    @Test
    def intersection03(): Bool = Map.intersection(Map#{}, Map#{1 => 2}) == Map#{}

    @Test
    def intersection04(): Bool = Map.intersection(Map#{}, Map#{1 => 2, 3 => 4}) == Map#{}

    @Test
    def intersection05(): Bool = Map.intersection(Map#{1 => 2, 3 => 4}, Map#{}) == Map#{}

    @Test
    def intersection06(): Bool = Map.intersection(Map#{1 => 2}, Map#{3 => 4}) == Map#{}

    @Test
    def intersection07(): Bool = Map.intersection(Map#{1 => 2}, Map#{1 => 5}) == Map#{1 => 2}

    @Test
    def intersection08(): Bool = Map.intersection(Map#{}, Map#{1 => 2, 2 => 3, 3 => 4}) == Map#{}

    @Test
    def intersection09(): Bool = Map.intersection(Map#{1 => 2, 2 => 3, 3 => 4}, Map#{}) == Map#{}

    @Test
    def intersection10(): Bool = Map.intersection(Map#{1 => 2, 2 => 3}, Map#{3 => 4}) == Map#{}

    @Test
    def intersection11(): Bool = Map.intersection(Map#{3 => 4}, Map#{1 => 2, 2 => 3}) == Map#{}

    @Test
    def intersection12(): Bool = Map.intersection(Map#{1 => 2, 2 => 3}, Map#{2 => 8}) == Map#{2 => 3}

    @Test
    def intersection13(): Bool = Map.intersection(Map#{1 => 2, 2 => 3}, Map#{1 => 8}) == Map#{1 => 2}

    @Test
    def intersection14(): Bool = Map.intersection(Map#{1 => 4}, Map#{1 => 2, 2 => 3}) == Map#{1 => 4}

    @Test
    def intersection15(): Bool = Map.intersection(Map#{2 => 7}, Map#{1 => 2, 2 => 3}) == Map#{2 => 7}

    @Test
    def intersection16(): Bool = Map.intersection(Map#{2 => 7, 11 => 14, 9 => 8, 15 => 22}, Map#{15 => 21, 1 => 2, 2 => 8, 44 => 33, 11 => 1})
                                       == Map#{2 => 7, 15 => 22, 11 => 14}

    /////////////////////////////////////////////////////////////////////////////
    // intersectionWith                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def intersectionWith01(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{}, Map#{}) == (Map#{}: Map[Int32, Int32])

    @Test
    def intersectionWith02(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{1 => 2}, Map#{}) == Map#{}

    @Test
    def intersectionWith03(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{}, Map#{1 => 2}) == Map#{}

    @Test
    def intersectionWith04(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{}, Map#{1 => 2, 3 => 4}) == Map#{}

    @Test
    def intersectionWith05(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{1 => 2, 3 => 4}, Map#{}) == Map#{}

    @Test
    def intersectionWith06(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{1 => 2}, Map#{3 => 4}) == Map#{}

    @Test
    def intersectionWith07(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{1 => 2}, Map#{1 => 5}) == Map#{1 => -3}

    @Test
    def intersectionWith08(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{}, Map#{1 => 2, 2 => 3, 3 => 4}) == Map#{}

    @Test
    def intersectionWith09(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{1 => 2, 2 => 3, 3 => 4}, Map#{}) == Map#{}

    @Test
    def intersectionWith10(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{1 => 2, 2 => 3}, Map#{3 => 4}) == Map#{}

    @Test
    def intersectionWith11(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{3 => 4}, Map#{1 => 2, 2 => 3}) == Map#{}

    @Test
    def intersectionWith12(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{1 => 2, 2 => 3}, Map#{2 => 8}) == Map#{2 => -5}

    @Test
    def intersectionWith13(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{1 => 2, 2 => 3}, Map#{1 => 8}) == Map#{1 => -6}

    @Test
    def intersectionWith14(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{1 => 4}, Map#{1 => 2, 2 => 3}) == Map#{1 => 2}

    @Test
    def intersectionWith15(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{2 => 7}, Map#{1 => 2, 2 => 3}) == Map#{2 => 4}

    @Test
    def intersectionWith16(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{2 => 7, 11 => 14, 9 => 8, 15 => 22}, Map#{15 => 21, 1 => 2, 2 => 8, 44 => 33, 11 => 11})
                                       == Map#{2 => -1, 15 => 1, 11 => 3}
    @Test
    def intersectionWith17(): Bool = Map.intersectionWith((v1, v2) -> "${v1}|${v2}", Map#{3 => 'a', 5 => 'b'}, Map#{5 => "B", 7 => "C"})
                                       == Map#{5 => "b|B"}
    @Test
    def intersectionWith18(): Bool = Map.intersectionWith((v1, v2) -> "${v1}|${v2}", Map#{3 => 'a', 5 => 'b', 7 => 'c', 10 => 'd'}, Map#{5 => "B", 7 => "C"})
                                       == Map#{5 => "b|B", 7 => "c|C"}

    @Test
    def intersectionWith19(): Bool = Map.intersectionWith((v1, v2) -> "${v1}|${v2}", Map#{3 => '3'}, Map#{3 => 3})
                                       == Map#{3 => "3|3"}

    /////////////////////////////////////////////////////////////////////////////
    // intersectionWithKey                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def intersectionWithKey01(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{}, Map#{}) == (Map#{}: Map[Int32, Int32])

    @Test
    def intersectionWithKey02(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 => 2}, Map#{}) == Map#{}

    @Test
    def intersectionWithKey03(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{}, Map#{1 => 2}) == Map#{}

    @Test
    def intersectionWithKey04(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{}, Map#{1 => 2, 3 => 4}) == Map#{}

    @Test
    def intersectionWithKey05(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 => 2, 3 => 4}, Map#{}) == Map#{}

    @Test
    def intersectionWithKey06(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 => 2}, Map#{3 => 4}) == Map#{}

    @Test
    def intersectionWithKey07(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 => 2}, Map#{1 => 5}) == Map#{1 => -2}

    @Test
    def intersectionWithKey08(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{}, Map#{1 => 2, 2 => 3, 3 => 4}) == Map#{}

    @Test
    def intersectionWithKey09(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 => 2, 2 => 3, 3 => 4}, Map#{}) == Map#{}

    @Test
    def intersectionWithKey10(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 => 2, 2 => 3}, Map#{3 => 4}) == Map#{}

    @Test
    def intersectionWithKey11(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{3 => 4}, Map#{1 => 2, 2 => 3}) == Map#{}

    @Test
    def intersectionWithKey12(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 => 2, 2 => 3}, Map#{2 => 8}) == Map#{2 => -3}

    @Test
    def intersectionWithKey13(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 => 2, 2 => 3}, Map#{1 => 8}) == Map#{1 => -5}

    @Test
    def intersectionWithKey14(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 => 4}, Map#{1 => 2, 2 => 3}) == Map#{1 => 3}

    @Test
    def intersectionWithKey15(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{2 => 7}, Map#{1 => 2, 2 => 3}) == Map#{2 => 6}

    @Test
    def intersectionWithKey16(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{2 => 7, 11 => 14, 9 => 8, 15 => 22}, Map#{15 => 21, 1 => 2, 2 => 8, 44 => 33, 11 => 17})
                                      == Map#{2 => 1, 15 => 16, 11 => 8}
    @Test
    def intersectionWithKey17(): Bool = Map.intersectionWithKey((k, v1, v2) -> "${k}:${v1}|${v2}", Map#{3 => 'a', 5 => 'b'}, Map#{5 => "B", 7 => "C"})
                                       == Map#{5 => "5:b|B"}
    @Test
    def intersectionWithKey18(): Bool = Map.intersectionWithKey((k, v1, v2) -> "${k}:${v1}|${v2}", Map#{3 => 'a', 5 => 'b', 7 => 'c', 10 => 'd'}, Map#{5 => "B", 7 => "C"})
                                       == Map#{5 => "5:b|B", 7 => "7:c|C"}

    @Test
    def intersectionWithKey19(): Bool = Map.intersectionWithKey((k, v1, v2) -> "${k}:${v1}|${v2}", Map#{3 => '3'}, Map#{3 => 3})
                                       == Map#{3 => "3:3|3"}

    /////////////////////////////////////////////////////////////////////////////
    // difference                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def difference01(): Bool = Map.difference((Map#{}: Map[Unit, Unit]), (Map#{}: Map[Unit, Unit])) == Map#{}

    @Test
    def difference02(): Bool = Map.difference(Map#{1 => 2}, Map#{}) == Map#{1 => 2}

    @Test
    def difference03(): Bool = Map.difference(Map#{}, Map#{1 => 2}) == Map#{}

    @Test
    def difference04(): Bool = Map.difference(Map#{}, Map#{1 => 2, 3 => 4}) == Map#{}

    @Test
    def difference05(): Bool = Map.difference(Map#{1 => 2, 3 => 4}, Map#{}) == Map#{1 => 2, 3 => 4}

    @Test
    def difference06(): Bool = Map.difference(Map#{1 => 2}, Map#{3 => 4}) == Map#{1 => 2}

    @Test
    def difference07(): Bool = Map.difference(Map#{1 => 2}, Map#{1 => 5}) == Map#{}

    @Test
    def difference08(): Bool = Map.difference(Map#{}, Map#{1 => 2, 2 => 3, 3 => 4}) == Map#{}

    @Test
    def difference09(): Bool = Map.difference(Map#{1 => 2, 2 => 3, 3 => 4}, Map#{}) == Map#{1 => 2, 2 => 3, 3 => 4}

    @Test
    def difference10(): Bool = Map.difference(Map#{1 => 2, 2 => 3}, Map#{3 => 4}) == Map#{1 => 2, 2 => 3}

    @Test
    def difference11(): Bool = Map.difference(Map#{3 => 4}, Map#{1 => 2, 2 => 3}) == Map#{3 => 4}

    @Test
    def difference12(): Bool = Map.difference(Map#{1 => 2, 2 => 3}, Map#{2 => 8}) == Map#{1 => 2}

    @Test
    def difference13(): Bool = Map.difference(Map#{1 => 2, 2 => 3}, Map#{1 => 8}) == Map#{2 => 3}

    @Test
    def difference14(): Bool = Map.difference(Map#{1 => 4}, Map#{1 => 2, 2 => 3}) == Map#{}

    @Test
    def difference15(): Bool = Map.difference(Map#{2 => 7}, Map#{1 => 2, 2 => 3}) == Map#{}

    @Test
    def difference16(): Bool = Map.difference(Map#{2 => 7, 11 => 14, 9 => 8, 15 => 22}, Map#{15 => 21, 1 => 2, 2 => 8, 44 => 33})
                                      == Map#{11 => 14, 9 => 8}

    /////////////////////////////////////////////////////////////////////////////
    // differenceWith                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def differenceWith01(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{}, Map#{}) == (Map#{}: Map[Int32, Int32])

    @Test
    def differenceWith02(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 => 2}, Map#{}) == Map#{1 => 2}

    @Test
    def differenceWith03(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{}, Map#{1 => 2}) == Map#{}

    @Test
    def differenceWith04(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{}, Map#{1 => 2, 3 => 4}) == Map#{}

    @Test
    def differenceWith05(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 => 2, 3 => 4}, Map#{}) == Map#{1 => 2, 3 => 4}

    @Test
    def differenceWith06(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 => 2}, Map#{3 => 4}) == Map#{1 => 2}

    @Test
    def differenceWith07(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 => 2}, Map#{1 => 5}) == Map#{}

    @Test
    def differenceWith08(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 => 5}, Map#{1 => 4}) == Map#{1 => 5}

    @Test
    def differenceWith09(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{}, Map#{1 => 2, 2 => 3, 3 => 4}) == Map#{}

    @Test
    def differenceWith10(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 => 2, 2 => 3, 3 => 4}, Map#{}) == Map#{1 => 2, 2 => 3, 3 => 4}

    @Test
    def differenceWith11(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 => 2, 2 => 3}, Map#{3 => 4}) == Map#{1 => 2, 2 => 3}

    @Test
    def differenceWith12(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{3 => 4}, Map#{1 => 2, 2 => 3}) == Map#{3 => 4}

    @Test
    def differenceWith13(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 => 2, 2 => 3}, Map#{2 => 8}) == Map#{1 => 2}

    @Test
    def differenceWith14(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 => 2, 2 => 9}, Map#{2 => 8}) == Map#{1 => 2, 2 => 9}

    @Test
    def differenceWith15(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 => 2, 2 => 3}, Map#{1 => 8}) == Map#{2 => 3}

    @Test
    def differenceWith16(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 => 9, 2 => 3}, Map#{1 => 8}) == Map#{1 => 9, 2 => 3}

    @Test
    def differenceWith17(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 => 1}, Map#{1 => 2, 2 => 3}) == Map#{}

    @Test
    def differenceWith18(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 => 4}, Map#{1 => 2, 2 => 3}) == Map#{1 => 4}

    @Test
    def differenceWith19(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{2 => -4}, Map#{1 => 2, 2 => 3}) == Map#{}

    @Test
    def differenceWith20(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{2 => 7}, Map#{1 => 2, 2 => 3}) == Map#{2 => 7}

    @Test
    def differenceWith21(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{2 => 7, 11 => 14, 9 => 8, 15 => 22}, Map#{15 => 21, 1 => 2, 2 => 8, 44 => 33})
                                      == Map#{11 => 14, 9 => 8, 15 => 22}

    /////////////////////////////////////////////////////////////////////////////
    // differenceWithKey                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def differenceWithKey01(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{}, Map#{}) == (Map#{}: Map[Int32, Int32])

    @Test
    def differenceWithKey02(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 => 2}, Map#{}) == Map#{1 => 2}

    @Test
    def differenceWithKey03(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{}, Map#{1 => 2}) == Map#{}

    @Test
    def differenceWithKey04(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{}, Map#{1 => 2, 3 => 4}) == Map#{}

    @Test
    def differenceWithKey05(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 => 2, 3 => 4}, Map#{}) == Map#{1 => 2, 3 => 4}

    @Test
    def differenceWithKey06(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 => 2}, Map#{3 => 4}) == Map#{1 => 2}

    @Test
    def differenceWithKey07(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 => 2}, Map#{1 => 5}) == Map#{}

    @Test
    def differenceWithKey08(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 => 5}, Map#{1 => 4}) == Map#{1 => 6}

    @Test
    def differenceWithKey09(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{}, Map#{1 => 2, 2 => 3, 3 => 4}) == Map#{}

    @Test
    def differenceWithKey10(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 => 2, 2 => 3, 3 => 4}, Map#{}) == Map#{1 => 2, 2 => 3, 3 => 4}

    @Test
    def differenceWithKey11(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 => 2, 2 => 3}, Map#{3 => 4}) == Map#{1 => 2, 2 => 3}

    @Test
    def differenceWithKey12(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{3 => 4}, Map#{1 => 2, 2 => 3}) == Map#{3 => 4}

    @Test
    def differenceWithKey13(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 => 2, 2 => 3}, Map#{2 => 8}) == Map#{1 => 2}

    @Test
    def differenceWithKey14(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 => 2, 2 => 9}, Map#{2 => 8}) == Map#{1 => 2, 2 => 11}

    @Test
    def differenceWithKey15(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 => 2, 2 => 3}, Map#{1 => 8}) == Map#{2 => 3}

    @Test
    def differenceWithKey16(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 => 9, 2 => 3}, Map#{1 => 8}) == Map#{1 => 10, 2 => 3}

    @Test
    def differenceWithKey17(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 => 1}, Map#{1 => 2, 2 => 3}) == Map#{}

    @Test
    def differenceWithKey18(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 => 4}, Map#{1 => 2, 2 => 3}) == Map#{1 => 5}

    @Test
    def differenceWithKey19(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{2 => -4}, Map#{1 => 2, 2 => 3}) == Map#{}

    @Test
    def differenceWithKey20(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{2 => 7}, Map#{1 => 2, 2 => 3}) == Map#{2 => 9}

    @Test
    def differenceWithKey21(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{2 => 7, 11 => 14, 9 => 8, 15 => 22}, Map#{15 => 21, 1 => 2, 2 => 8, 44 => 33})
                                      == Map#{11 => 14, 9 => 8, 15 => 37}

    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toSet01(): Bool = Map.toSet((Map#{}: Map[Unit, Unit])) == Set#{}

    @Test
    def toSet02(): Bool = Map.toSet(Map#{1 => 2}) == Set#{(1, 2)}

    @Test
    def toSet03(): Bool = Map.toSet(Map#{1 => 2, 3 => 4}) == Set#{(1, 2), (3, 4)}

    @Test
    def toSet04(): Bool = Map.toSet(Map#{1 => 2, 2 => 2, 3 => 4}) == Set#{(1, 2), (2, 2), (3, 4)}

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toList01(): Bool = Map.toList((Map#{}: Map[Unit, Unit])) == Nil

    @Test
    def toList02(): Bool = Map.toList(Map#{1 => 2}) == (1, 2) :: Nil

    @Test
    def toList03(): Bool = Map.toList(Map#{1 => 2, 3 => 4}) == (1, 2) :: (3, 4) :: Nil

    @Test
    def toList04(): Bool = Map.toList(Map#{1 => 2, 2 => 2, 3 => 4}) == (1, 2) :: (2, 2) :: (3, 4) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toArray01(): Bool = region rc {
        Array.sameElements(Map.toArray(rc, (Map#{}: Map[Unit, Unit])), Array#{} @ rc)
    }

    @Test
    def toArray02(): Bool = region rc {
        Array.sameElements(Map.toArray(rc, Map#{1 => 2}), Array#{(1, 2)} @ rc)
    }

    @Test
    def toArray03(): Bool =  region rc {
        Array.sameElements(Map.toArray(rc, Map#{1 => 2, 3 => 4}), Array#{(1, 2), (3, 4)} @ rc)
    }

    @Test
    def toArray04(): Bool = region rc {
        Array.sameElements(Map.toArray(rc, Map#{1 => 2, 2 => 2, 3 => 4}), Array#{(1, 2), (2, 2), (3, 4)} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toVector01(): Bool = Map.toVector((Map#{}: Map[Unit, Unit])) == (Vector#{} : Vector[(Unit, Unit)])

    @Test
    def toVector02(): Bool = Map.toVector(Map#{1 => 2}) == Vector#{(1, 2)}

    @Test
    def toVector03(): Bool = Map.toVector(Map#{1 => 2, 3 => 4}) == Vector#{(1, 2), (3, 4)}

    @Test
    def toVector04(): Bool = Map.toVector(Map#{1 => 2, 2 => 2, 3 => 4}) == Vector#{(1, 2), (2, 2), (3, 4)}

    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMutDeque01(): Bool = region rc {
        let m: Map[Int32, Int32] = Map#{};
        let d1 = Map.toMutDeque(rc, m);

        let d2 = MutDeque.empty(rc);

        d1 `MutDeque.sameElements` d2
    }

    @Test
    def toMutDeque02(): Bool = region rc {
        let m = Map#{1 => 2};
        let d1 = Map.toMutDeque(rc, m);

        let d2 = MutDeque.empty(rc);
        MutDeque.pushBack((1, 2), d2);

        d1 `MutDeque.sameElements` d2
    }

    @Test
    def toMutDeque03(): Bool = region rc {
        let m = Map#{1 => 2, 3 => 4, 5 => 6};
        let d1 = Map.toMutDeque(rc, m);

        let d2 = MutDeque.empty(rc);
        MutDeque.pushBack((3, 4), d2);
        MutDeque.pushBack((5, 6), d2);
        MutDeque.pushFront((1, 2), d2);

        d1 `MutDeque.sameElements` d2
    }

    @Test
    def toMutDeque04(): Bool = region rc {
        let m = Map#{1 => 'a', 2 => 'b', 3 => 'c'};
        let d1 = Map.toMutDeque(rc, m);

        let d2 = MutDeque.empty(rc);
        MutDeque.pushFront((3, 'c'), d2);
        MutDeque.pushFront((2, 'b'), d2);
        MutDeque.pushFront((1, 'a'), d2);

        d1 `MutDeque.sameElements` d2
    }

    /////////////////////////////////////////////////////////////////////////////
    // eq                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def eq01(): Bool = (Map#{}: Map[Unit, Unit]) == Map#{}

    @Test
    def eq02(): Bool = Map#{1 => 2} != Map#{}

    @Test
    def eq03(): Bool = Map#{} != Map#{1 => 2}

    @Test
    def eq04(): Bool = Map#{1 => 2, 3 => 4} != Map#{}

    @Test
    def eq05(): Bool = Map#{} != Map#{1 => 2, 3 => 4}

    @Test
    def eq06(): Bool = Map#{1 => 2} == Map#{1 => 2}

    @Test
    def eq07(): Bool = Map#{1 => 3} != Map#{1 => 2}

    @Test
    def eq08(): Bool = Map#{1 => 2} != Map#{1 => 3}

    @Test
    def eq09(): Bool = Map#{2 => 2} != Map#{1 => 2}

    @Test
    def eq10(): Bool = Map#{1 => 2} != Map#{4 => 2}

    @Test
    def eq11(): Bool = Map#{1 => 2, 3 => 4} != Map#{1 => 2}

    @Test
    def eq12(): Bool = Map#{1 => 2} != Map#{1 => 2, 3 => 4}

    @Test
    def eq13(): Bool = Map#{1 => 2, 3 => 4} == Map#{1 => 2, 3 => 4}

    @Test
    def eq14(): Bool = Map#{1 => 2, 3 => 4} == Map#{3 => 4, 1 => 2}

    @Test
    def eq15(): Bool = Map#{1 => 1, 3 => 4} != Map#{1 => 2, 3 => 4}

    @Test
    def eq16(): Bool = Map#{4 => 2, 3 => 4} != Map#{1 => 2, 3 => 4}

    @Test
    def eq17(): Bool = Map#{1 => 2, 3 => 4} != Map#{1 => 2, 2 => 4}

    @Test
    def eq18(): Bool = Map#{1 => 2, 3 => 4} != Map#{1 => 2, 3 => 8}

    @Test
    def eq19(): Bool = Map#{1 => 2, 3 => 4, 5 => 6} != Map#{1 => 2, 3 => 4}

    @Test
    def eq20(): Bool = Map#{1 => 2, 3 => 4} != Map#{1 => 2, 3 => 4, 5 => 6}

    @Test
    def eq21(): Bool = Map#{1 => 2, 3 => 4, 5 => 6} == Map#{1 => 2, 3 => 4, 5 => 6}

    @Test
    def eq22(): Bool = Map#{1 => 2, 3 => 4, 5 => 6} == Map#{1 => 2, 5 => 6, 3 => 4}

    @Test
    def eq23(): Bool = Map#{1 => 2, 3 => 4, 5 => 6} == Map#{3 => 4, 5 => 6, 1 => 2}

    @Test
    def eq24(): Bool = Map#{1 => 2, 3 => 4, 5 => 6} == Map#{5 => 6, 3 => 4, 1 => 2}

    @Test
    def eq25(): Bool = Map#{1 => 2, 3 => 4, 5 => 6, 7 => 8, 9 => 10} == Map#{7 => 8, 1 => 2, 3 => 4, 9 => 10, 5 => 6}

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Bool = region rc {
        let ri = Ref.fresh(rc, 21);
        Map.forEach((k, _) -> Ref.put(k, ri), Map#{});
        21 == Ref.get(ri)
    }

    @Test
    def forEach02(): Bool = region rc {
        let ri = Ref.fresh(rc, 21);
        Map.forEach((k, _) -> Ref.put(k, ri), Map#{1 => "Hello World!"});
        1 == Ref.get(ri)
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEachWithIndex01(): Bool = region rc {
        let ri = Ref.fresh(rc, 21);
        Map.forEachWithIndex((i, _, _) -> Ref.put(i, ri), Map#{});
        21 == Ref.get(ri)
    }

    @Test
    def forEachWithIndex02(): Bool = region rc {
        let ri = Ref.fresh(rc, 21);
        Map.forEachWithIndex((i, _, _) -> Ref.put(i, ri), Map#{1 => "Hello World!"});
        0 == Ref.get(ri)
    }

    /////////////////////////////////////////////////////////////////////////////
    // unfold                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unfold01(): Bool =
        Map.unfold(s -> if (true) None else Some((s, s + 48, s + 1)), 0) == Map#{}

    @Test
    def unfold02(): Bool =
        Map.unfold(s -> if (s > 0) None else Some((s, s + 48, s + 1)), 0) == Map#{0 => 48}

    @Test
    def unfold03(): Bool =
        Map.unfold(s -> if (s > 1) None else Some((s, s + 48, s + 1)), 0) == Map#{0 => 48, 1 => 49}

    @Test
    def unfold04(): Bool =
        Map.unfold(s -> if (s >= 10) None else Some((s, s + 48, s + 1)), 0) == Map#{0 => 48, 1 => 49, 2 => 50, 3 => 51, 4 => 52, 5 => 53, 6 => 54, 7 => 55, 8 => 56, 9 => 57}

    @Test
    def unfold05(): Bool =
        Map.unfold(s -> if (s >= 10) None else Some((s, s + 48, s + 1)), 5) == Map#{5 => 53, 6 => 54, 7 => 55, 8 => 56, 9 => 57}

    @Test
    def unfold06(): Bool =
        Map.unfold(s -> if (s >= 10) None else Some((s, s + 48, s + 2)), 0) == Map#{0 => 48, 2 => 50, 4 => 52, 6 => 54, 8 => 56}

    /////////////////////////////////////////////////////////////////////////////
    // unfoldWithIter                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unfoldWithIter01(): Bool = region rc {
        let x = Ref.fresh(rc, 0);
        let step = () ->
            if (true)
                None
            else {
                let k = Ref.get(x);
                let v = k + 48;
                Ref.put(Ref.get(x) + 1, x);
                Some((k, v))
            };
        Map.unfoldWithIter(step) == Map#{}
    }

    @Test
    def unfoldWithIter02(): Bool = region rc {
        let x = Ref.fresh(rc, 0);
        let step = () ->
            if (Ref.get(x) > 0)
                None
            else {
                let k = Ref.get(x);
                let v = k + 48;
                Ref.put(Ref.get(x) + 1, x);
                Some((k, v))
            };
        Map.unfoldWithIter(step) == Map#{0 => 48}
    }

    @Test
    def unfoldWithIter03(): Bool = region rc {
        let x = Ref.fresh(rc, 0);
        let step = () ->
            if (Ref.get(x) > 1)
                None
            else {
                let k = Ref.get(x);
                let v = k + 48;
                Ref.put(Ref.get(x) + 1, x);
                Some((k, v))
            };
        Map.unfoldWithIter(step) == Map#{0 => 48, 1 => 49}
    }

    @Test
    def unfoldWithIter04(): Bool = region rc {
        let x = Ref.fresh(rc, 0);
        let step = () ->
            if (Ref.get(x) >= 10)
                None
            else {
                let k = Ref.get(x);
                let v = k + 48;
                Ref.put(Ref.get(x) + 1, x);
                Some((k, v))
            };
        Map.unfoldWithIter(step) == Map#{0 => 48, 1 => 49, 2 => 50, 3 => 51, 4 => 52, 5 => 53, 6 => 54, 7 => 55, 8 => 56, 9 => 57}
    }

    @Test
    def unfoldWithIter05(): Bool = region rc {
        let x = Ref.fresh(rc, 5);
        let step = () ->
            if (Ref.get(x) >= 10)
                None
            else {
                let k = Ref.get(x);
                let v = k + 48;
                Ref.put(Ref.get(x) + 1, x);
                Some((k, v))
            };
        Map.unfoldWithIter(step) == Map#{5 => 53, 6 => 54, 7 => 55, 8 => 56, 9 => 57}
    }

    @Test
    def unfoldWithIter06(): Bool = region rc {
        let x = Ref.fresh(rc, 0);
        let step = () ->
            if (Ref.get(x) >= 10)
                None
            else {
                let k = Ref.get(x);
                let v = k + 48;
                Ref.put(Ref.get(x) + 2, x);
                Some((k, v))
            };
        Map.unfoldWithIter(step) == Map#{0 => 48, 2 => 50, 4 => 52, 6 => 54, 8 => 56}
    }

    /////////////////////////////////////////////////////////////////////////////
    // inverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testInvert01(): Bool = Map.invert(Map#{1 => "a"}) == Map#{"a" => Set#{1}}

    @Test
    def testInvert02(): Bool = Map.invert(Map#{1 => "a", 2 => "b"}) == Map#{"a" => Set#{1}, "b" => Set#{2}}

    @Test
    def testInvert03(): Bool = Map.invert(Map#{1 => "a", 2 => "a"}) == Map#{"a" => Set#{1, 2}}

    @Test
    def testInvert04(): Bool = Map.invert(Map#{1 => "a", 2 => "b", 3 => "a"}) == Map#{"a" => Set#{1, 3}, "b" => Set#{2}}

    @Test
    def testInvert05(): Bool = Map.invert(Map#{1 => "a", 2 => "a", 3 => "a"}) == Map#{"a" => Set#{1, 2, 3}}

    @Test
    def testInvert06(): Bool = Map.invert(Map#{1 => "a", 2 => "b", 3 => "a", 4 => "b"}) == Map#{"a" => Set#{1, 3}, "b" => Set#{2, 4}}

    /////////////////////////////////////////////////////////////////////////////
    // query                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def rangeQuery01(): Bool =
        Map.rangeQuery(_ -> Comparison.EqualTo, (Map#{}: Map[Unit, Unit])) == Nil

    @Test
    def rangeQuery02(): Bool =
        Map.rangeQuery(_ -> Comparison.EqualTo, Map#{1 => 101, 2 => 42}) == (1, 101) :: (2, 42) :: Nil

    @Test
    def rangeQuery03(): Bool =
        Map.rangeQuery(x -> if (x < 2) Comparison.LessThan else if (x > 2) Comparison.GreaterThan else Comparison.EqualTo, Map#{1 => 1, 2 => 2, 3 => 3}) == (2, 2) :: Nil

    @Test
    def rangeQuery04(): Bool =
        Map.rangeQuery(x -> if (x < 'b') Comparison.LessThan else if (x > 'c') Comparison.GreaterThan else Comparison.EqualTo, Map#{'a' => 0, 'b' => 1, 'c' => 2, 'd' => 3, 'e' => 4}) == ('b', 1) :: ('c', 2) :: Nil

    @Test
    def rangeQuery05(): Bool =
        let m = Map.unfold(s -> if (s > 10) None else Some((s, s, s + 1)), 5);
        Map.rangeQuery(x -> if (x < 5) Comparison.LessThan else if (x >= 10) Comparison.GreaterThan else Comparison.EqualTo, m) == List.map(x -> (x, x), List.range(5, 10))

    @Test
    def rangeQuery06(): Bool =
        let m = Map.unfold(s -> if (s > 75) None else Some((s, Some(s + 48), s + 1)), 25);
        Map.rangeQuery(x -> if (x < 25) Comparison.LessThan else if (x >= 75) Comparison.GreaterThan else Comparison.EqualTo, m) == List.map(x -> (x, Some(x + 48)), List.range(25, 75))

    @Test
    def rangeQuery07(): Bool =
        Map.rangeQuery(x -> if (x < 42) Comparison.LessThan else if (x > 42) Comparison.GreaterThan else Comparison.EqualTo, Map#{42 => true}) == (42, true) :: Nil

    @Test
    def rangeQuery08(): Bool =
        Map.rangeQuery(x -> if (x < 1) Comparison.LessThan else if (x > 1) Comparison.GreaterThan else Comparison.EqualTo, Map#{1 => 2, 2 => 3, 3 => 4, 4 => 5}) == (1, 2) :: Nil

    @Test
    def rangeQuery09(): Bool =
        Map.rangeQuery(x -> if (x < 2) Comparison.LessThan else if (x > 3) Comparison.GreaterThan else Comparison.EqualTo, Map#{1 => 2, 2 => 3, 3 => 4, 4 => 5}) == (2, 3) :: (3, 4) :: Nil

    @Test
    def rangeQuery10(): Bool =
        Map.rangeQuery(x -> if (x < 4) Comparison.LessThan else if (x > 4) Comparison.GreaterThan else Comparison.EqualTo, Map#{1 => 2, 2 => 3, 3 => 4, 4 => 5}) == (4, 5) :: Nil

    @Test
    def rangeQuery11(): Bool =
        let m = Map#{(1, 2) => 0, (1, 3) => 1, (2, 1) => 2, (2, 2) => 3, (2, 3) => 4, (3, 1) => 5};
        Map.rangeQuery(x -> if (fst(x) < 1) Comparison.LessThan else if (fst(x) > 1) Comparison.GreaterThan else Comparison.EqualTo, m) == ((1, 2), 0) :: ((1, 3), 1) :: Nil

    @Test
    def rangeQuery12(): Bool =
        let m = Map#{(1, 2) => 0, (1, 3) => 1, (2, 1) => 2, (2, 2) => 3, (2, 3) => 4, (3, 1) => 5};
        Map.rangeQuery(x -> if (fst(x) < 3) Comparison.LessThan else if (fst(x) > 3) Comparison.GreaterThan else Comparison.EqualTo, m) == ((3, 1), 5) :: Nil

    @Test
    def rangeQuery13(): Bool =
        let m = Map#{(1, 2) => 0, (1, 3) => 1, (2, 1) => 2, (2, 2) => 3, (2, 3) => 4, (3, 1) => 5};
        Map.rangeQuery(x -> if (fst(x) > 2) Comparison.GreaterThan else Comparison.EqualTo, m) == ((1, 2), 0) :: ((1, 3), 1) :: ((2, 1), 2) :: ((2, 2), 3) :: ((2, 3), 4) :: Nil

    @Test
    def rangeQuery14(): Bool =
        let m = Map#{(1, 2) => 0, (1, 3) => 1, (2, 1) => 2, (2, 2) => 3, (2, 3) => 4, (3, 1) => 5};
        Map.rangeQuery(x -> if (fst(x) < 2) Comparison.LessThan else Comparison.EqualTo, m) == ((2, 1), 2) :: ((2, 2), 3) :: ((2, 3), 4) :: ((3, 1), 5) :: Nil

    @Test
    def rangeQuery15(): Bool =
        let m = Map#{(1, 2) => 0, (1, 3) => 1, (2, 1) => 2, (2, 2) => 3, (2, 3) => 4, (3, 1) => 5};
        let f = x -> {
            let cmp = x <=> (2, 2);
            if (cmp == Comparison.GreaterThan)
                cmp
            else
                Comparison.EqualTo
            };
        Map.rangeQuery(f, m) == ((1, 2), 0) :: ((1, 3), 1) :: ((2, 1), 2) :: ((2, 2), 3) :: Nil

    @Test
    def rangeQuery16(): Bool =
        let m = Map#{(1, 2) => 0, (1, 3) => 1, (2, 1) => 2, (2, 2) => 3, (2, 3) => 4, (3, 1) => 5};
        let f = x -> {
            let cmp = x <=> (2, 2);
            if (cmp == Comparison.LessThan)
                cmp
            else
                Comparison.EqualTo
            };
        Map.rangeQuery(f, m) == ((2, 2), 3) :: ((2, 3), 4) :: ((3, 1), 5) :: Nil

    @Test
    def rangeQuery17(): Bool =
        let m = Map#{(1, 2, 2) => 0, (1, 2, 3) => 1, (1, 2, 4) => 2, (2, 2, 4) => 3};
        let f = x -> {
            let cmp = x <=> (1, 2, 3);
            if (cmp == Comparison.LessThan)
                cmp
            else
                Comparison.EqualTo
            };
        Map.rangeQuery(f, m) == ((1, 2, 3), 1) :: ((1, 2, 4), 2) :: ((2, 2, 4), 3) :: Nil

    @Test
    def rangeQuery18(): Bool =
        let m = Map#{(1, 2, 2) => 0, (1, 2, 3) => 1, (1, 2, 4) => 2, (2, 2, 4) => 3};
        let f = x -> {
            let cmp = x <=> (1, 2, 3);
            if (cmp == Comparison.GreaterThan)
                cmp
            else
                Comparison.EqualTo
            };
        Map.rangeQuery(f, m) == ((1, 2, 2), 0) :: ((1, 2, 3), 1) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toString01(): Unit \ Assert =
        assertEq(expected = "Map#{1 => 2}", toString(Map#{1 => 2}))

    @Test
    def toString02(): Unit \ Assert =
        assertEq(expected = "Map#{1 => 0, 2 => 1, 3 => 2}", toString(Map#{1 => 0, 2 => 1, 3 => 2}))

    @Test
    def toString03(): Unit \ Assert =
        assertEq(expected = "Map#{1 => b, 2 => a}", toString(Map#{1 => "b", 2 => "a"}))

    @Test
    def toString04(): Unit \ Assert =
        assertEq(expected = "Map#{0 => true, 2 => true, 3 => false, 4 => true, 97 => false}", toString(Map#{97 => false, 2 => true, 3 => false, 4 => true, 0 => true}))

    @Test
    def toString05(): Unit \ Assert =
        assertEq(expected = "Map#{2 => Map#{1 => 0, 2 => 1}, 3 => Map#{3 => 2, 4 => 92}}", toString(Map#{2 => Map#{1 => 0, 2 => 1}, 3 => Map#{3 => 2, 4 => 92}}))

    /////////////////////////////////////////////////////////////////////////////
    // hash                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def hash01(): Unit \ Assert = 
        assertEq(expected = hash((Map#{}: Map[Unit, Unit])), hash((Map#{}: Map[Unit, Unit])))

    @Test
    def hash02(): Unit \ Assert = 
        assertEq(expected = hash(Map#{1 => true, 2 => false, 3 => false}), hash(Map#{1 => true, 2 => false, 3 => false}))

    @Test
    def hash03(): Unit \ Assert = 
        assertNeq(unexpected = hash(Map#{1 => 1, 2 => 2}), hash(Map#{1 => 2, 1 => 2}))

    @Test
    def hash04(): Unit \ Assert = 
        assertNeq(unexpected = hash(Map#{'a' => 1, 'b' => 2}), hash(Map#{1 => 'a', 2 => 'b'}))

    @Test
    def hash05(): Unit \ Assert = 
        assertEq(expected = hash(Map#{1 => 'a', 2 => 'b', 3 => 'z', 4 => 'x'}), hash(Map#{1 => 'a', 2 => 'b', 3 => 'z', 4 => 'x'}))

    /////////////////////////////////////////////////////////////////////////////
    // minimumKey                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimumKey01(): Bool = Map.minimumKey((Map#{}: Map[Int32, Int32])) == None

    @Test
    def minimumKey02(): Bool = Map.minimumKey(Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5}) == Some((1, 2))

    @Test
    def minimumKey03(): Bool = Map.minimumKey(Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5, 0 => 2}) == Some((0, 2))

    @Test
    def minimumKey04(): Bool = Map.minimumKey(Map#{1 => 2, 2 => 3, -44 => 4, 5 => 5, 0 => 2}) == Some((-44, 4))

    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimumKeyBy01(): Bool =
        Map.minimumKeyBy((x, y) -> x <=> y, (Map#{}: Map[Int32, Int32])) == None

    @Test
    def minimumKeyBy02(): Bool =
        Map.minimumKeyBy((x, y) -> x <=> y, Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5}) == Some((1, 2))

    @Test
    def minimumKeyBy03(): Bool =
        Map.minimumKeyBy((x, y) -> x <=> y, Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5, 0 => 2}) == Some((0, 2))

    @Test
    def minimumKeyBy04(): Bool =
        Map.minimumKeyBy((x, y) -> x <=> y, Map#{1 => 2, 2 => 3, -44 => 4, 5 => 5, 0 => 2}) == Some((-44, 4))

    @Test
    def minimumKeyBy05(): Bool = region rc {
        let range = List.range(0, 2000);
        let m = List.zip(range, range) |> List.toMap;
        let a = MutList.empty(rc);
        discard Map.minimumKeyBy((x, y) -> { let b = MutList.empty(rc); MutList.push(y, b); MutList.append(MutList.toList(b), a); x <=> y }, m);
        MutList.toList(a) == List.range(1, 2000) // The first / left most value is ignored
    }

    /////////////////////////////////////////////////////////////////////////////
    // minimumValue                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimumValue01(): Bool = Map.minimumValue((Map#{}: Map[Int32, Int32])) == None

    @Test
    def minimumValue02(): Bool = Map.minimumValue(Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5}) == Some((1, 2))

    @Test
    def minimumValue03(): Bool = Map.minimumValue(Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5, 0 => 2}) == Some((1, 2))

    @Test
    def minimumValue04(): Bool = Map.minimumValue(Map#{1 => 2, 2 => 3, -44 => 4, 5 => 5, 0 => 2}) == Some((1, 2))

    @Test
    def minimumValue05(): Bool = Map.minimumValue(Map#{1 => 2, 2 => 3, 4 => -44, 5 => 5, 0 => 2}) == Some((4, -44))

    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimumValueBy01(): Bool =
        Map.minimumValueBy((x, y) -> x <=> y, (Map#{}: Map[Int32, Int32])) == None

    @Test
    def minimumValueBy02(): Bool =
        Map.minimumValueBy((x, y) -> x <=> y, Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5}) == Some((1, 2))

    @Test
    def minimumValueBy03(): Bool =
        Map.minimumValueBy((x, y) -> x <=> y, Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5, 0 => 2}) == Some((1, 2))

    @Test
    def minimumValueBy04(): Bool =
        Map.minimumValueBy((x, y) -> x <=> y, Map#{1 => 2, 2 => 3, -44 => 4, 5 => 5, 0 => 2}) == Some((1, 2))

    @Test
    def minimumValueBy05(): Bool =
        Map.minimumValueBy((x, y) -> x <=> y, Map#{1 => 2, 2 => 3, 4 => -44, 5 => 5, 0 => 2}) == Some((4, -44))

    @Test
    def minimumValueBy06(): Bool = region rc {
        let range = List.range(0, 2000);
        let m = List.zip(range, List.reverse(range)) |> List.toMap;
        let a = MutList.empty(rc);
        discard Map.minimumValueBy((x, y) -> { let b = MutList.empty(rc); MutList.push(x, b); MutList.append(MutList.toList(b), a); x <=> y }, m);
        MutList.toList(a) == List.reverse(List.range(1, 2000)) // The first / left most value is ignored
    }


    /////////////////////////////////////////////////////////////////////////////
    // maximumKey                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maximumKey01(): Bool = Map.maximumKey((Map#{}: Map[Int32, Int32])) == None

    @Test
    def maximumKey02(): Bool = Map.maximumKey(Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5}) == Some((5, 5))

    @Test
    def maximumKey03(): Bool = Map.maximumKey(Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5, 0 => 2}) == Some((5, 5))

    @Test
    def maximumKey04(): Bool = Map.maximumKey(Map#{1 => 2, 2 => 3, 107 => -107, 5 => 5, 0 => 2}) == Some((107, -107))


    /////////////////////////////////////////////////////////////////////////////
    // maximumKeyBy                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maximumKeyBy01(): Bool = Map.maximumKeyBy((x, y) -> x <=> y, (Map#{}: Map[Int32, Int32])) == None

    @Test
    def maximumKeyBy02(): Bool = Map.maximumKeyBy((x, y) -> x <=> y, Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5}) == Some((5, 5))

    @Test
    def maximumKeyBy03(): Bool = Map.maximumKeyBy((x, y) -> x <=> y, Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5, 0 => 2}) == Some((5, 5))

    @Test
    def maximumKeyBy04(): Bool = Map.maximumKeyBy((x, y) -> x <=> y, Map#{1 => 2, 2 => 3, 107 => -107, 5 => 5, 0 => 2}) == Some((107, -107))

    @Test
    def maximumKeyBy05(): Bool = region rc {
        let range = List.range(0, 2000);
        let m = List.zip(range, range) |> List.toMap;
        let a = MutList.empty(rc);
        discard Map.maximumKeyBy((x, y) -> { let b = MutList.empty(rc); MutList.push(y, b); MutList.append(MutList.toList(b), a); x <=> y }, m);
        MutList.toList(a) == List.range(1, 2000) // The first / left most value is ignored
    }


    /////////////////////////////////////////////////////////////////////////////
    // maximumValue                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maximumValue01(): Bool = Map.maximumValue((Map#{}: Map[Int32, Int32])) == None

    @Test
    def maximumValue02(): Bool = Map.maximumValue(Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5}) == Some((5, 5))

    @Test
    def maximumValue03(): Bool = Map.maximumValue(Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5, 0 => 2}) == Some((5, 5))

    @Test
    def maximumValue04(): Bool = Map.maximumValue(Map#{1 => 2, 2 => 3, -107 => 107, 0 => 2}) == Some((-107, 107))


    /////////////////////////////////////////////////////////////////////////////
    // maximumValueBy                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maximumValueBy01(): Bool = Map.maximumValueBy((x, y) -> x <=> y, (Map#{}: Map[Int32, Int32])) == None

    @Test
    def maximumValueBy02(): Bool = Map.maximumValueBy((x, y) -> x <=> y, Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5}) == Some((5, 5))

    @Test
    def maximumValueBy03(): Bool = Map.maximumValueBy((x, y) -> x <=> y, Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5, 0 => 2}) == Some((5, 5))

    @Test
    def maximumValueBy04(): Bool = Map.maximumValueBy((x, y) -> x <=> y, Map#{1 => 2, 2 => 3, -107 => 107,5 => 5, 0 => 2}) == Some((-107, 107))

    @Test
    def maximumValueBy05(): Bool = region rc {
        let range = List.range(0, 2000);
        let m = List.zip(range, range) |> List.toMap;
        let a = MutList.empty(rc);
        discard Map.maximumValueBy((x, y) -> { let b = MutList.empty(rc); MutList.push(y, b); MutList.append(MutList.toList(b), a); x <=> y }, m);
        MutList.toList(a) == List.range(1, 2000) // The first / left most value is ignored
    }


    /////////////////////////////////////////////////////////////////////////////
    // toDelayList                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toDelayList01(): Bool =
        (Map.empty(): Map[Unit, Unit]) |> Map.toDelayList == DelayList.empty()

    @Test
    def toDelayList02(): Bool =
        Map.unfold(s -> if (s < 1000) Some((s, "a", s + 1)) else None, 0) |> Map.toDelayList == DelayList.repeat("a") |> DelayList.zip(DelayList.range(0, 1000))

    @Test
    def toDelayList03(): Bool =
        Map.unfold(s -> if (s < 1000) Some((s, "a", s + 1)) else None, 0) |> Map.toDelayList |> DelayList.toMap == Map.unfold(s -> if (s < 1000) Some((s, "a", s + 1)) else None, 0)

    /////////////////////////////////////////////////////////////////////////////
    // toChain                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toChain01(): Bool =
        Map.toChain((Map#{}: Map[Int32, Int32])) == (Chain.empty(): Chain[(Int32, Int32)])

    @Test
    def toChain02(): Bool =
        Map.toChain(Map#{1 => 1}) == Chain.singleton((1, 1))

    @Test
    def toChain03(): Bool =
        Map.toChain(Map#{1 => 1, 2 => 2}) == List.toChain((1, 1) :: (2, 2) :: Nil)

    @Test
    def toChain04(): Bool =
        Map.toChain(Map#{1 => 1, 2 => 2, 3 => 3}) == List.toChain((1, 1) :: (2, 2) :: (3, 3) :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // toMultiMap                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMultiMap01(): Bool =
        Map.toMultiMap((Map#{}: Map[Int32, Char])) |> MultiMap.toAscList == Nil

    @Test
    def toMultiMap02(): Bool =
        Map.toMultiMap(Map#{1 => 'a'}) |> MultiMap.toAscList == (1, 'a') :: Nil

    @Test
    def toMultiMap03(): Bool =
        Map.toMultiMap(Map#{1 => 'a', 2 => 'b'}) |> MultiMap.toAscList == (1, 'a') :: (2, 'b') :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterator01(): Bool = region rc {
        (Map.empty(): Map[Int32, Int32]) |> Map.iterator(rc) |> Iterator.toMap == Map.empty()
    }

    @Test
    def iterator02(): Bool = region rc {
        Map#{"a" => 1, "b" => 2, "c" => 3, "d" => 4} |> Map.iterator(rc) |> Iterator.toMap == Map#{"a" => 1, "b" => 2, "c" => 3, "d" => 4}
    }

    @Test
    def iterator03(): Bool = region rc {
        let l = List.range(0, 100);
        List.zip(l, List.reverse(l)) |> List.toMap |> Map.iterator(rc) |> Iterator.toMap |> Map.toList == List.zip(List.range(0, 100), List.reverse(List.range(0, 100)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // iteratorKeys                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iteratorKeys01(): Bool = region rc {
        (Map.empty(): Map[Int32, Int32]) |> Map.iteratorKeys(rc) |> Iterator.toList == Nil
    }

    @Test
    def iteratorKeys02(): Bool = region rc {
        Map#{"a" => 1, "b" => 2, "c" => 3, "d" => 4} |> Map.iteratorKeys(rc) |> Iterator.toList == "a" :: "b" :: "c" :: "d" :: Nil
    }

    @Test
    def iteratorKeys03(): Bool = region rc {
        let l = List.range(0, 100);
        List.zip(l, List.reverse(l)) |> List.toMap |> Map.iteratorKeys(rc) |> Iterator.toList == List.range(0, 100)
    }


    /////////////////////////////////////////////////////////////////////////////
    // iteratorValues                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iteratorValues01(): Bool = region rc {
        (Map.empty(): Map[Int32, Int32]) |> Map.iteratorValues(rc) |> Iterator.toList == Nil
    }

    @Test
    def iteratorValues02(): Bool = region rc {
        Map#{"a" => 1, "b" => 2, "c" => 3, "d" => 4} |> Map.iteratorValues(rc) |> Iterator.toList == 1 :: 2 :: 3 :: 4 :: Nil
    }

    @Test
    def iteratorValues03(): Bool = region rc {
        let l = List.range(0, 100);
        List.zip(l, List.reverse(l)) |> List.toMap |> Map.iteratorValues(rc) |> Iterator.toList == List.range(0, 100) |> List.reverse
    }

    /////////////////////////////////////////////////////////////////////////////
    // sequence                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sequence01(): Bool =
        let m: Map[Int32, Identity[Char]] = Map#{};
        Map.sequence(m) == Identity.Identity(Map#{})

    @Test
    def sequence02(): Bool =
        let m = Map#{1 => Identity.Identity('a')};
        Map.sequence(m) == Identity.Identity(Map#{1 => 'a'})

    @Test
    def sequence03(): Bool =
        let m = Map#{1 => Identity.Identity('a'), 2 => Identity.Identity('b')};
        Map.sequence(m) == Identity.Identity(Map#{1 => 'a', 2 => 'b'})

    @Test
    def sequence04(): Bool =
        let m = Map#{1 => Identity.Identity('a'), 2 => Identity.Identity('b'), 3 => Identity.Identity('c')};
        Map.sequence(m) == Identity.Identity(Map#{1 => 'a', 2 => 'b', 3 => 'c'})

    /////////////////////////////////////////////////////////////////////////////
    // traverse                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def traverse01(): Bool = region rc {
        let st = Ref.fresh(rc, '#');
        let m: Map[Int32, Char] = Map#{};
        let ans = Map.traverse(x -> {Ref.put(x, st); Identity.Identity(x)}, m);
        ans == Identity.Identity(Map#{}) and Ref.get(st) == '#'
    }

    @Test
    def traverse02(): Bool = region rc {
        let st = Ref.fresh(rc, '#');
        let m = Map#{1 => 'a'};
        let ans = Map.traverse(x -> {Ref.put(x, st); Identity.Identity(x)}, m);
        ans == Identity.Identity(Map#{1 => 'a'}) and Ref.get(st) == 'a'
    }

    @Test
    def traverse03(): Bool = region rc {
        let st = Ref.fresh(rc, '#');
        let m = Map#{1 => 'a', 2 => 'b'};
        let ans = Map.traverse(x -> {Ref.put(x, st); Identity.Identity(x)}, m);
        ans == Identity.Identity(Map#{1 => 'a', 2 => 'b'}) and Ref.get(st) == 'b'
    }

    @Test
    def traverse04(): Bool = region rc {
        let st = Ref.fresh(rc, '#');
        let m = Map#{1 => 'a', 2 => 'b', 3 => 'c'};
        let ans = Map.traverse(x -> {Ref.put(x, st); Identity.Identity(x)}, m);
        ans == Identity.Identity(Map#{1 => 'a', 2 => 'b', 3 => 'c'}) and Ref.get(st) == 'c'
    }

    /////////////////////////////////////////////////////////////////////////////
    // traverseWithKey                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def traverseWithKey01(): Bool = region rc {
        let st = Ref.fresh(rc, (0, '#'));
        let m: Map[Int32, Char] = Map#{};
        let ans = Map.traverseWithKey((k, v) -> {Ref.put((k, v), st); Identity.Identity(v)}, m);
        ans == Identity.Identity(Map#{}) and Ref.get(st) == (0, '#')
    }

    @Test
    def traverseWithKey02(): Bool = region rc {
        let st = Ref.fresh(rc, (0, '#'));
        let m = Map#{1 => 'a'};
        let ans = Map.traverseWithKey((k, v) -> {Ref.put((k, v), st); Identity.Identity(v)}, m);
        ans == Identity.Identity(Map#{1 => 'a'}) and Ref.get(st) == (1, 'a')
    }

    @Test
    def traverseWithKey03(): Bool = region rc {
        let st = Ref.fresh(rc, (0, '#'));
        let m = Map#{1 => 'a', 2 => 'b'};
        let ans = Map.traverseWithKey((k, v) -> {Ref.put((k, v), st); Identity.Identity(v)}, m);
        ans == Identity.Identity(Map#{1 => 'a', 2 => 'b'}) and Ref.get(st) == (2, 'b')
    }

    @Test
    def traverseWithKey04(): Bool = region rc {
        let st = Ref.fresh(rc, (0, '#'));
        let m = Map#{1 => 'a', 2 => 'b', 3 => 'c'};
        let ans = Map.traverseWithKey((k, v) -> {Ref.put((k, v), st); Identity.Identity(v)}, m);
        ans == Identity.Identity(Map#{1 => 'a', 2 => 'b', 3 => 'c'}) and Ref.get(st) == (3, 'c')
    }

    /////////////////////////////////////////////////////////////////////////////
    // joinKeys                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinKeys01(): Bool =
        (Map.empty(): Map[Int32, Int32]) |> Map.joinKeys(",") == ""

    @Test
    def joinKeys02(): Bool =
        Map#{1 => 1} |> Map.joinKeys(",") == "1"

    @Test
    def joinKeys03(): Bool =
        Map#{0 => 1, 1 => 2, 2 => 2} |> Map.joinKeys(",") == "0,1,2"

    @Test
    def joinKeys04(): Bool =
        Map#{"0" => 1, "1" => 2, "2" => 2} |> Map.joinKeys(",") == "0,1,2"


    /////////////////////////////////////////////////////////////////////////////
    // joinValues                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinValues01(): Bool =
        (Map.empty(): Map[Int32, Int32]) |> Map.joinValues(",") == ""

    @Test
    def joinValues02(): Bool =
        Map#{1 => 1} |> Map.joinValues(",") == "1"

    @Test
    def joinValues03(): Bool =
        Map#{0 => 1, 1 => 2, 2 => 2} |> Map.joinValues(",") == "1,2,2"

    @Test
    def joinValues04(): Bool =
        Map#{0 => "1", 1 => "2", 2 => "2"} |> Map.joinValues(",") == "1,2,2"


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinWith01(): Bool =
        (Map.empty(): Map[Int32, Int32]) |> Map.joinWith((k, v) -> "${k} => ${v}", ",") == ""

    @Test
    def joinWith02(): Bool =
        Map#{1 => 1} |>
            Map.joinWith((k, v) -> "${k} => ${v}", ", ") == "1 => 1"

    @Test
    def joinWith03(): Bool =
        Map#{0 => 1, 1 => 2, 2 => 2} |>
            Map.joinWith((k, v) -> "${k} => ${v}", ", ") == "0 => 1, 1 => 2, 2 => 2"

    @Test
    def joinWith04(): Bool =
        Map#{0 => "1", 1 => "2", 2 => "2"} |>
            Map.joinWith((k, v) -> "${k} => ${v}", ", ") == "0 => 1, 1 => 2, 2 => 2"


    /////////////////////////////////////////////////////////////////////////////
    // explode                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def explode01(): Bool =
        Map.explode(Map#{1 => Set#{2, 3}}) == Set#{(1, 2), (1, 3)}

    @Test
    def explode02(): Bool =
        Map.explode(Map#{1 => Set#{2, 3}, 2 => Set#{2, 3}}) == Set#{(1, 2), (1, 3), (2, 2), (2, 3)}

    @Test
    def explode03(): Bool =
        Map.explode(Map#{1 => Set#{2, 3}, 2 => Set#{5, 6, 7}}) == Set#{(1, 2), (1, 3), (2, 5), (2, 6), (2, 7)}

    @Test
    def explode04(): Bool =
        Map.explode(Map#{1 => Set#{2, 3}, 2 => Set#{5, 6, 7}, 3 => Set#{0}}) == Set#{(1, 2), (1, 3), (2, 5), (2, 6), (2, 7), (3, 0)}

    /////////////////////////////////////////////////////////////////////////////
    // LowerBound.minValue                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minValue01(): Bool =
        (LowerBound.minValue(): Map[Int32, Int32]) == Map.empty()

    /////////////////////////////////////////////////////////////////////////////
    // PartialOrder.lessEqual                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def lessEqual01(): Bool = PartialOrder.lessEqual((Map.empty(): Map[Int32, Int32]), (Map.empty(): Map[Int32, Int32]))

    @Test
    def lessEqual02(): Bool = PartialOrder.lessEqual((Map.empty(): Map[Int32, Int32]), Map.singleton(1, 1))

    @Test
    def lessEqual03(): Bool = not PartialOrder.lessEqual(Map.singleton(1, 1), (Map.empty(): Map[Int32, Int32]))

    @Test
    def lessEqual04(): Bool = PartialOrder.lessEqual(Map.singleton(1, 1), Map.singleton(1, 1))

    @Test
    def lessEqual05(): Bool = not PartialOrder.lessEqual(Map.singleton(1, 1), Map.singleton(1, 10))

    /////////////////////////////////////////////////////////////////////////////
    // JoinLattice.leastUpperBound                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def leastUpperBound01(): Bool =
        JoinLattice.leastUpperBound((Map.empty(): Map[Int32, Int32]), (Map.empty(): Map[Int32, Int32])) == (Map.empty(): Map[Int32, Int32])

    @Test
    def leastUpperBound02(): Bool =
        JoinLattice.leastUpperBound((Map.empty(): Map[Int32, Int32]), Map.singleton(1, 1)) == Map.singleton(1, 1)

    @Test
    def leastUpperBound03(): Bool =
        JoinLattice.leastUpperBound(Map.singleton(1, 1), (Map.empty(): Map[Int32, Int32])) == Map.singleton(1, 1)

    @Test
    def leastUpperBound04(): Bool =
        JoinLattice.leastUpperBound(Map.singleton(1, 1), Map.singleton(1, 1)) == Map.singleton(1, 1)

    @Test
    def leastUpperBound05(): Bool =
        JoinLattice.leastUpperBound(Map.singleton(1, 1), Map.singleton(1, 10)) == Map.singleton(1, 10)

    @Test
    def leastUpperBound06(): Bool =
        JoinLattice.leastUpperBound(Map.singleton(1, 1), Map.singleton(2, 2)) == Map#{1 => 1, 2 => 2}

    @Test
    def leastUpperBound07(): Bool =
        JoinLattice.leastUpperBound(Map.singleton(1, 1), Map#{1 => 10, 2 => 2}) == Map#{1 => 10, 2 => 2}

    /////////////////////////////////////////////////////////////////////////////
    // MeetLattice.greatestLowerBound                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def greatestLowerBound01(): Bool =
        MeetLattice.greatestLowerBound((Map.empty(): Map[Int32, Int32]), (Map.empty(): Map[Int32, Int32])) == (Map.empty(): Map[Int32, Int32])

    @Test
    def greatestLowerBound02(): Bool =
        MeetLattice.greatestLowerBound((Map.empty(): Map[Int32, Int32]), Map.singleton(1, 1)) == (Map.empty(): Map[Int32, Int32])

    @Test
    def greatestLowerBound03(): Bool =
        MeetLattice.greatestLowerBound(Map.singleton(1, 1), (Map.empty(): Map[Int32, Int32])) == (Map.empty(): Map[Int32, Int32])

    @Test
    def greatestLowerBound04(): Bool =
        MeetLattice.greatestLowerBound(Map.singleton(1, 1), Map.singleton(1, 1)) == Map.singleton(1, 1)

    @Test
    def greatestLowerBound05(): Bool =
        MeetLattice.greatestLowerBound(Map.singleton(1, 1), Map.singleton(1, 10)) == Map.singleton(1, 1)

    @Test
    def greatestLowerBound06(): Bool =
        MeetLattice.greatestLowerBound(Map.singleton(1, 1), Map.singleton(2, 2)) == (Map.empty(): Map[Int32, Int32])

    @Test
    def greatestLowerBound07(): Bool =
        MeetLattice.greatestLowerBound(Map.singleton(1, 1), Map#{1 => 10, 2 => 2}) == Map.singleton(1, 1)

}
