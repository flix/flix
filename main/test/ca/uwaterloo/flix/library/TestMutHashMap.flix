mod TestMutHashMap {

    use Assert.{assertEq, assertNeq, assertTrue, assertFalse, fail}

    /////////////////////////////////////////////////////////////////////////////
    // empty                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def empty01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, String, rc] = MutHashMap.empty(rc);
        assertTrue(MutHashMap.isEmpty(m))
    }

    @Test
    def empty02(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, String, rc] = MutHashMap.empty(rc);
        assertEq(expected = 0, MutHashMap.size(m))
    }

    @Test
    def empty03(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, String, _] = MutHashMap.empty(rc);
        assertEq(expected = None, MutHashMap.get(1, m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // emptyWithCapacity                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def emptyWithCapacity01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, String, rc] = MutHashMap.emptyWithCapacity(rc, 32);
        assertTrue(MutHashMap.isEmpty(m))
    }

    @Test
    def emptyWithCapacity02(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, String, rc] = MutHashMap.emptyWithCapacity(rc, -1);
        assertTrue(MutHashMap.isEmpty(m))
    }

    @Test
    def emptyWithCapacity03(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, String, rc] = MutHashMap.emptyWithCapacity(rc, 0);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        assertEq(expected = 2, MutHashMap.size(m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // singleton                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def singleton01(): Unit \ Assert = region rc {
        let m = MutHashMap.singleton(rc, 1, "one");
        assertEq(expected = 1, MutHashMap.size(m))
    }

    @Test
    def singleton02(): Unit \ Assert = region rc {
        let m = MutHashMap.singleton(rc, 1, "one");
        assertEq(expected = Some("one"), MutHashMap.get(1, m))
    }

    @Test
    def singleton03(): Unit \ Assert = region rc {
        let m = MutHashMap.singleton(rc, 42, "answer");
        assertFalse(MutHashMap.isEmpty(m));
        assertTrue(MutHashMap.nonEmpty(m));
        assertTrue(MutHashMap.memberOf(42, m));
        assertFalse(MutHashMap.memberOf(41, m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // copy                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def copy01(): Unit \ Assert = region rc {
        let m1: MutHashMap[Int32, String, _] = MutHashMap.empty(rc);
        let m2 = MutHashMap.copy(rc, m1);
        assertEq(expected = 0, MutHashMap.size(m2))
    }

    @Test
    def copy02(): Unit \ Assert = region rc {
        let m1 = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m1);
        MutHashMap.put(2, "two", m1);
        MutHashMap.put(3, "three", m1);
        let m2 = MutHashMap.copy(rc, m1);
        assertEq(expected = 3, MutHashMap.size(m2));
        assertEq(expected = Some("one"), MutHashMap.get(1, m2));
        assertEq(expected = Some("two"), MutHashMap.get(2, m2));
        assertEq(expected = Some("three"), MutHashMap.get(3, m2))
    }

    @Test
    def copy03(): Unit \ Assert = region rc {
        let m1 = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m1);
        MutHashMap.put(2, "two", m1);
        let m2 = MutHashMap.copy(rc, m1);
        // Mutating m1 should not affect m2
        MutHashMap.put(3, "three", m1);
        MutHashMap.put(1, "ONE", m1);
        assertEq(expected = 2, MutHashMap.size(m2));
        assertEq(expected = Some("one"), MutHashMap.get(1, m2));
        assertEq(expected = None, MutHashMap.get(3, m2))
    }

    @Test
    def copy04(): Unit \ Assert = region rc {
        let m1 = MutHashMap.empty(rc);
        MutHashMap.put(3, "three", m1);
        MutHashMap.put(1, "one", m1);
        MutHashMap.put(2, "two", m1);
        let m2 = MutHashMap.copy(rc, m1);
        // Verify insertion order is preserved
        let list1 = MutHashMap.toList(m1);
        let list2 = MutHashMap.toList(m2);
        assertEq(expected = list1, list2)
    }

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def size01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, String, rc] = MutHashMap.empty(rc);
        assertEq(expected = 0, MutHashMap.size(m))
    }

    @Test
    def size02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        assertEq(expected = 1, MutHashMap.size(m))
    }

    @Test
    def size03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(3, "three", m);
        assertEq(expected = 3, MutHashMap.size(m))
    }

    @Test
    def size04(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(1, "ONE", m);
        assertEq(expected = 1, MutHashMap.size(m))
    }

    @Test
    def size05(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.remove(1, m);
        assertEq(expected = 1, MutHashMap.size(m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isEmpty01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, String, rc] = MutHashMap.empty(rc);
        assertTrue(MutHashMap.isEmpty(m))
    }

    @Test
    def isEmpty02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        assertFalse(MutHashMap.isEmpty(m))
    }

    @Test
    def isEmpty03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.remove(1, m);
        assertTrue(MutHashMap.isEmpty(m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // nonEmpty                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def nonEmpty01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, String, rc] = MutHashMap.empty(rc);
        assertFalse(MutHashMap.nonEmpty(m))
    }

    @Test
    def nonEmpty02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        assertTrue(MutHashMap.nonEmpty(m))
    }

    @Test
    def nonEmpty03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.remove(1, m);
        assertFalse(MutHashMap.nonEmpty(m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // put                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def put01(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        assertEq(expected = Some("one"), MutHashMap.get(1, m))
    }

    @Test
    def put02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        assertEq(expected = Some("one"), MutHashMap.get(1, m));
        assertEq(expected = Some("two"), MutHashMap.get(2, m))
    }

    @Test
    def put03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(1, "ONE", m);
        assertEq(expected = Some("ONE"), MutHashMap.get(1, m));
        assertEq(expected = 1, MutHashMap.size(m))
    }

    @Test
    def put04(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(3, "three", m);
        MutHashMap.put(4, "four", m);
        MutHashMap.put(5, "five", m);
        assertEq(expected = 5, MutHashMap.size(m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // get                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def get01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, String, rc] = MutHashMap.empty(rc);
        assertEq(expected = (None: Option[String]), MutHashMap.get(1, m))
    }

    @Test
    def get02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        assertEq(expected = Some("one"), MutHashMap.get(1, m))
    }

    @Test
    def get03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        assertEq(expected = (None: Option[String]), MutHashMap.get(2, m))
    }

    @Test
    def get04(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(3, "three", m);
        assertEq(expected = Some("two"), MutHashMap.get(2, m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // getWithDefault                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def getWithDefault01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, String, rc] = MutHashMap.empty(rc);
        assertEq(expected = "default", MutHashMap.getWithDefault(1, "default", m))
    }

    @Test
    def getWithDefault02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        assertEq(expected = "one", MutHashMap.getWithDefault(1, "default", m))
    }

    @Test
    def getWithDefault03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        assertEq(expected = "default", MutHashMap.getWithDefault(2, "default", m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // putWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def putWith01(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.putWith((x, y) -> x + y, 1, 10, m);
        assertEq(expected = Some(10), MutHashMap.get(1, m))
    }

    @Test
    def putWith02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 5, m);
        MutHashMap.putWith((x, y) -> x + y, 1, 10, m);
        assertEq(expected = Some(15), MutHashMap.get(1, m))
    }

    @Test
    def putWith03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 5, m);
        MutHashMap.put(2, 10, m);
        MutHashMap.putWith((x, y) -> x + y, 1, 3, m);
        assertEq(expected = 2, MutHashMap.size(m));
        assertEq(expected = Some(8), MutHashMap.get(1, m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // putWithKey                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def putWithKey01(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.putWithKey((k, x, y) -> k + x + y, 1, 10, m);
        // Key doesn't exist, so inserts 1 => 10 (function not applied)
        assertEq(expected = Some(10), MutHashMap.get(1, m))
    }

    @Test
    def putWithKey02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 5, m);
        MutHashMap.putWithKey((k, x, y) -> k + x + y, 1, 10, m);
        assertEq(expected = Some(16), MutHashMap.get(1, m))
    }

    @Test
    def putWithKey03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 5, m);
        MutHashMap.put(2, 10, m);
        MutHashMap.putWithKey((k, x, y) -> k + x + y, 2, 3, m);
        assertEq(expected = 2, MutHashMap.size(m));
        assertEq(expected = Some(15), MutHashMap.get(2, m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // adjust                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def adjust01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, Int32, _] = MutHashMap.empty(rc);
        MutHashMap.adjust(x -> x * 2, 1, m);
        assertEq(expected = 0, MutHashMap.size(m))
    }

    @Test
    def adjust02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 10, m);
        MutHashMap.adjust(x -> x * 2, 1, m);
        assertEq(expected = Some(20), MutHashMap.get(1, m))
    }

    @Test
    def adjust03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 10, m);
        MutHashMap.put(2, 20, m);
        MutHashMap.adjust(x -> x + 5, 1, m);
        assertEq(expected = Some(15), MutHashMap.get(1, m));
        assertEq(expected = Some(20), MutHashMap.get(2, m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // adjustWithKey                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def adjustWithKey01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, Int32, _] = MutHashMap.empty(rc);
        MutHashMap.adjustWithKey((k, v) -> k + v, 1, m);
        assertEq(expected = 0, MutHashMap.size(m))
    }

    @Test
    def adjustWithKey02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 10, m);
        MutHashMap.adjustWithKey((k, v) -> k + v, 1, m);
        assertEq(expected = Some(11), MutHashMap.get(1, m))
    }

    @Test
    def adjustWithKey03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 10, m);
        MutHashMap.put(2, 20, m);
        MutHashMap.adjustWithKey((k, v) -> k + v, 2, m);
        assertEq(expected = Some(10), MutHashMap.get(1, m));
        assertEq(expected = Some(22), MutHashMap.get(2, m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // update                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def update01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, Int32, _] = MutHashMap.empty(rc);
        MutHashMap.update(v -> Some(v * 2), 1, m);
        assertEq(expected = 0, MutHashMap.size(m))
    }

    @Test
    def update02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 10, m);
        MutHashMap.update(v -> Some(v * 2), 1, m);
        assertEq(expected = Some(20), MutHashMap.get(1, m))
    }

    @Test
    def update03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 10, m);
        MutHashMap.update(_ -> None, 1, m);
        assertEq(expected = 0, MutHashMap.size(m))
    }

    @Test
    def update04(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 10, m);
        MutHashMap.put(2, 20, m);
        MutHashMap.update(v -> if (v > 15) Some(v) else None, 1, m);
        assertEq(expected = 1, MutHashMap.size(m));
        assertEq(expected = None, MutHashMap.get(1, m));
        assertEq(expected = Some(20), MutHashMap.get(2, m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // updateWithKey                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def updateWithKey01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, Int32, _] = MutHashMap.empty(rc);
        MutHashMap.updateWithKey((k, v) -> Some(k + v), 1, m);
        assertEq(expected = 0, MutHashMap.size(m))
    }

    @Test
    def updateWithKey02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 10, m);
        MutHashMap.updateWithKey((k, v) -> Some(k + v), 1, m);
        assertEq(expected = Some(11), MutHashMap.get(1, m))
    }

    @Test
    def updateWithKey03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 10, m);
        MutHashMap.updateWithKey((_, _) -> None, 1, m);
        assertEq(expected = 0, MutHashMap.size(m))
    }

    @Test
    def updateWithKey04(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 10, m);
        MutHashMap.put(2, 20, m);
        MutHashMap.updateWithKey((k, v) -> if (k + v > 15) Some(v) else None, 1, m);
        assertEq(expected = 1, MutHashMap.size(m));
        assertEq(expected = None, MutHashMap.get(1, m));
        assertEq(expected = Some(20), MutHashMap.get(2, m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def memberOf01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, String, rc] = MutHashMap.empty(rc);
        assertFalse(MutHashMap.memberOf(1, m))
    }

    @Test
    def memberOf02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        assertTrue(MutHashMap.memberOf(1, m))
    }

    @Test
    def memberOf03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        assertFalse(MutHashMap.memberOf(2, m))
    }

    @Test
    def memberOf04(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(3, "three", m);
        assertTrue(MutHashMap.memberOf(2, m));
        assertFalse(MutHashMap.memberOf(4, m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // remove                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def remove01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, String, rc] = MutHashMap.empty(rc);
        MutHashMap.remove(1, m);
        assertEq(expected = 0, MutHashMap.size(m))
    }

    @Test
    def remove02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.remove(1, m);
        assertEq(expected = 0, MutHashMap.size(m))
    }

    @Test
    def remove03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.remove(1, m);
        assertEq(expected = 1, MutHashMap.size(m));
        assertEq(expected = (None: Option[String]), MutHashMap.get(1, m));
        assertEq(expected = Some("two"), MutHashMap.get(2, m))
    }

    @Test
    def remove04(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(3, "three", m);
        MutHashMap.remove(2, m);
        assertEq(expected = 2, MutHashMap.size(m));
        assertEq(expected = Some("one"), MutHashMap.get(1, m));
        assertEq(expected = (None: Option[String]), MutHashMap.get(2, m));
        assertEq(expected = Some("three"), MutHashMap.get(3, m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // clear                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def clear01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, String, rc] = MutHashMap.empty(rc);
        MutHashMap.clear(m);
        assertTrue(MutHashMap.isEmpty(m))
    }

    @Test
    def clear02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.clear(m);
        assertTrue(MutHashMap.isEmpty(m))
    }

    @Test
    def clear03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(3, "three", m);
        MutHashMap.clear(m);
        assertEq(expected = 0, MutHashMap.size(m));
        assertFalse(MutHashMap.memberOf(1, m));
        assertFalse(MutHashMap.memberOf(2, m));
        assertFalse(MutHashMap.memberOf(3, m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // Multiple inserts (expansion test)                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def multipleInserts01(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(3, "three", m);
        MutHashMap.put(4, "four", m);
        MutHashMap.put(5, "five", m);
        MutHashMap.put(6, "six", m);
        MutHashMap.put(7, "seven", m);
        MutHashMap.put(8, "eight", m);
        MutHashMap.put(9, "nine", m);
        MutHashMap.put(10, "ten", m);
        assertEq(expected = 10, MutHashMap.size(m));
        assertEq(expected = Some("five"), MutHashMap.get(5, m))
    }

    @Test
    def multipleInserts02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(3, "three", m);
        MutHashMap.put(4, "four", m);
        MutHashMap.put(5, "five", m);
        MutHashMap.put(6, "six", m);
        MutHashMap.put(7, "seven", m);
        MutHashMap.put(8, "eight", m);
        MutHashMap.put(9, "nine", m);
        MutHashMap.put(10, "ten", m);
        MutHashMap.put(11, "eleven", m);
        MutHashMap.put(12, "twelve", m);
        MutHashMap.put(13, "thirteen", m);
        MutHashMap.put(14, "fourteen", m);
        MutHashMap.put(15, "fifteen", m);
        MutHashMap.put(16, "sixteen", m);
        MutHashMap.put(17, "seventeen", m);
        MutHashMap.put(18, "eighteen", m);
        MutHashMap.put(19, "nineteen", m);
        MutHashMap.put(20, "twenty", m);
        assertEq(expected = 20, MutHashMap.size(m));
        assertEq(expected = Some("fifteen"), MutHashMap.get(15, m));
        assertEq(expected = Some("twenty"), MutHashMap.get(20, m))
    }

    @Test
    def multipleInserts03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        // Insert 200 elements
        List.range(1, 201) |> List.forEach(i -> MutHashMap.put(i, "value${i}", m));
        assertEq(expected = 200, MutHashMap.size(m));
        // Check some random elements
        assertEq(expected = Some("value1"), MutHashMap.get(1, m));
        assertEq(expected = Some("value50"), MutHashMap.get(50, m));
        assertEq(expected = Some("value100"), MutHashMap.get(100, m));
        assertEq(expected = Some("value150"), MutHashMap.get(150, m));
        assertEq(expected = Some("value200"), MutHashMap.get(200, m));
        assertEq(expected = None, MutHashMap.get(201, m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // Hash collisions                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def hashCollisions01(): Unit \ Assert = region rc {
        // Test with keys that may collide in small buckets
        let m = MutHashMap.empty(rc);
        MutHashMap.put(0, "zero", m);
        MutHashMap.put(16, "sixteen", m);
        MutHashMap.put(32, "thirty-two", m);
        assertEq(expected = 3, MutHashMap.size(m));
        assertEq(expected = Some("zero"), MutHashMap.get(0, m));
        assertEq(expected = Some("sixteen"), MutHashMap.get(16, m));
        assertEq(expected = Some("thirty-two"), MutHashMap.get(32, m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // Shrinking test                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def shrinking01(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        // Insert many elements to expand
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(3, "three", m);
        MutHashMap.put(4, "four", m);
        MutHashMap.put(5, "five", m);
        MutHashMap.put(6, "six", m);
        MutHashMap.put(7, "seven", m);
        MutHashMap.put(8, "eight", m);
        MutHashMap.put(9, "nine", m);
        MutHashMap.put(10, "ten", m);
        MutHashMap.put(11, "eleven", m);
        MutHashMap.put(12, "twelve", m);
        MutHashMap.put(13, "thirteen", m);
        MutHashMap.put(14, "fourteen", m);
        MutHashMap.put(15, "fifteen", m);
        MutHashMap.put(16, "sixteen", m);
        // Remove most elements to trigger shrinking
        MutHashMap.remove(1, m);
        MutHashMap.remove(2, m);
        MutHashMap.remove(3, m);
        MutHashMap.remove(4, m);
        MutHashMap.remove(5, m);
        MutHashMap.remove(6, m);
        MutHashMap.remove(7, m);
        MutHashMap.remove(8, m);
        MutHashMap.remove(9, m);
        MutHashMap.remove(10, m);
        MutHashMap.remove(11, m);
        MutHashMap.remove(12, m);
        MutHashMap.remove(13, m);
        MutHashMap.remove(14, m);
        assertEq(expected = 2, MutHashMap.size(m));
        assertEq(expected = Some("fifteen"), MutHashMap.get(15, m));
        assertEq(expected = Some("sixteen"), MutHashMap.get(16, m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, String, rc] = MutHashMap.empty(rc);
        let l = Ref.fresh(rc, Nil);
        MutHashMap.forEach((k, _v) -> Ref.put(k :: Ref.get(l), l), m);
        assertEq(expected = "", List.reverse(Ref.get(l)) |> List.join(","))
    }

    @Test
    def forEach02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        let l = Ref.fresh(rc, Nil);
        MutHashMap.forEach((k, _v) -> Ref.put(k :: Ref.get(l), l), m);
        assertEq(expected = "1", List.reverse(Ref.get(l)) |> List.join(","))
    }

    @Test
    def forEach03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(3, "three", m);
        let l = Ref.fresh(rc, Nil);
        MutHashMap.forEach((k, _v) -> Ref.put(k :: Ref.get(l), l), m);
        assertEq(expected = "1,2,3", List.reverse(Ref.get(l)) |> List.join(","))
    }

    @Test
    def forEach04(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(3, "three", m);
        let l = Ref.fresh(rc, Nil);
        MutHashMap.forEach((_k, v) -> Ref.put(v :: Ref.get(l), l), m);
        assertEq(expected = "one,two,three", List.reverse(Ref.get(l)) |> List.join(","))
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEachWithIndex01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, String, rc] = MutHashMap.empty(rc);
        let ri = Ref.fresh(rc, 99);
        MutHashMap.forEachWithIndex((i, _k, _v) -> Ref.put(i, ri), m);
        assertEq(expected = 99, Ref.get(ri))
    }

    @Test
    def forEachWithIndex02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        let ri = Ref.fresh(rc, 99);
        MutHashMap.forEachWithIndex((i, _k, _v) -> Ref.put(i, ri), m);
        assertEq(expected = 0, Ref.get(ri))
    }

    @Test
    def forEachWithIndex03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(3, "three", m);
        let ri = Ref.fresh(rc, 99);
        MutHashMap.forEachWithIndex((i, _k, _v) -> Ref.put(i, ri), m);
        assertEq(expected = 2, Ref.get(ri))
    }

    @Test
    def forEachWithIndex04(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(3, "three", m);
        let l = Ref.fresh(rc, Nil);
        MutHashMap.forEachWithIndex((i, k, _v) -> Ref.put("${i}:${k}" :: Ref.get(l), l), m);
        assertEq(expected = "0:1,1:2,2:3", List.reverse(Ref.get(l)) |> List.join(","))
    }

    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterator01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, String, rc] = MutHashMap.empty(rc);
        let iter = MutHashMap.iterator(rc, m);
        match Iterator.next(iter) {
            case None => ()
            case _    => fail("Unexpected case")
        }
    }

    @Test
    def iterator02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        let iter = MutHashMap.iterator(rc, m);
        match Iterator.next(iter) {
            case Some(_) => ()
            case _       => fail("Unexpected case")
        }
    }

    @Test
    def iterator03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(3, "three", m);
        let iter = MutHashMap.iterator(rc, m);
        let result = iter |> Iterator.toList;
        assertEq(expected = (1, "one") :: (2, "two") :: (3, "three") :: Nil, result)
    }

    @Test
    def iterator04(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(3, "three", m);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        let iter = MutHashMap.iterator(rc, m);
        let result = iter |> Iterator.toList;
        assertEq(expected = (3, "three") :: (1, "one") :: (2, "two") :: Nil, result)
    }

    /////////////////////////////////////////////////////////////////////////////
    // iteratorKeys                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iteratorKeys01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, String, rc] = MutHashMap.empty(rc);
        let iter = MutHashMap.iteratorKeys(rc, m);
        match Iterator.next(iter) {
            case None => ()
            case _    => fail("Unexpected case")
        }
    }

    @Test
    def iteratorKeys02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(3, "three", m);
        let iter = MutHashMap.iteratorKeys(rc, m);
        let result = iter |> Iterator.toList;
        assertEq(expected = 1 :: 2 :: 3 :: Nil, result)
    }

    @Test
    def iteratorKeys03(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, String, rc] = MutHashMap.empty(rc);
        let result = MutHashMap.iteratorKeys(rc, m) |> Iterator.toList;
        assertEq(expected = Nil, result)
    }

    /////////////////////////////////////////////////////////////////////////////
    // iteratorValues                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iteratorValues01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, String, rc] = MutHashMap.empty(rc);
        let iter = MutHashMap.iteratorValues(rc, m);
        match Iterator.next(iter) {
            case None => ()
            case _    => fail("Unexpected case")
        }
    }

    @Test
    def iteratorValues02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(3, "three", m);
        let iter = MutHashMap.iteratorValues(rc, m);
        let result = iter |> Iterator.toList;
        assertEq(expected = "one" :: "two" :: "three" :: Nil, result)
    }

    @Test
    def iteratorValues03(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, String, rc] = MutHashMap.empty(rc);
        let result = MutHashMap.iteratorValues(rc, m) |> Iterator.toList;
        assertEq(expected = Nil, result)
    }

    /////////////////////////////////////////////////////////////////////////////
    // Insertion order preservation                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def insertionOrder01(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(5, "five", m);
        MutHashMap.put(3, "three", m);
        MutHashMap.put(7, "seven", m);
        MutHashMap.put(1, "one", m);
        let keys = MutHashMap.iteratorKeys(rc, m) |> Iterator.toList;
        assertEq(expected = 5 :: 3 :: 7 :: 1 :: Nil, keys)
    }

    @Test
    def insertionOrder02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(3, "three", m);
        MutHashMap.put(2, "TWO", m);  // Update existing key
        let result = MutHashMap.iterator(rc, m) |> Iterator.toList;
        // Order should remain 1, 2, 3 even though 2 was updated
        assertEq(expected = (1, "one") :: (2, "TWO") :: (3, "three") :: Nil, result)
    }

    @Test
    def insertionOrder03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(3, "three", m);
        MutHashMap.remove(2, m);  // Remove middle element
        let keys = MutHashMap.iteratorKeys(rc, m) |> Iterator.toList;
        assertEq(expected = 1 :: 3 :: Nil, keys)
    }

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, Int32, _] = MutHashMap.empty(rc);
        let result = MutHashMap.map(rc, x -> x * 2, m);
        assertTrue(MutHashMap.isEmpty(result))
    }

    @Test
    def map02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 10, m);
        MutHashMap.put(2, 20, m);
        MutHashMap.put(3, 30, m);
        let result = MutHashMap.map(rc, x -> x * 2, m);
        assertEq(expected = 3, MutHashMap.size(result));
        assertEq(expected = Some(20), MutHashMap.get(1, result));
        assertEq(expected = Some(40), MutHashMap.get(2, result));
        assertEq(expected = Some(60), MutHashMap.get(3, result))
    }

    @Test
    def map03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(3, 30, m);
        MutHashMap.put(1, 10, m);
        MutHashMap.put(2, 20, m);
        let result = MutHashMap.map(rc, x -> x / 10, m);
        // Verify insertion order is preserved
        let keys = MutHashMap.iteratorKeys(rc, result) |> Iterator.toList;
        assertEq(expected = 3 :: 1 :: 2 :: Nil, keys)
    }

    @Test
    def map04(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 10, m);
        MutHashMap.put(2, 20, m);
        let result = MutHashMap.map(rc, x -> x * 2, m);
        // Mutating original should not affect result
        MutHashMap.put(3, 30, m);
        MutHashMap.put(1, 100, m);
        assertEq(expected = 2, MutHashMap.size(result));
        assertEq(expected = Some(20), MutHashMap.get(1, result))
    }

    /////////////////////////////////////////////////////////////////////////////
    // mapWithKey                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapWithKey01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, Int32, _] = MutHashMap.empty(rc);
        let result = MutHashMap.mapWithKey(rc, (k, v) -> k + v, m);
        assertTrue(MutHashMap.isEmpty(result))
    }

    @Test
    def mapWithKey02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 10, m);
        MutHashMap.put(2, 20, m);
        MutHashMap.put(3, 30, m);
        let result = MutHashMap.mapWithKey(rc, (k, v) -> k + v, m);
        assertEq(expected = 3, MutHashMap.size(result));
        assertEq(expected = Some(11), MutHashMap.get(1, result));
        assertEq(expected = Some(22), MutHashMap.get(2, result));
        assertEq(expected = Some(33), MutHashMap.get(3, result))
    }

    @Test
    def mapWithKey03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(5, 10, m);
        MutHashMap.put(2, 20, m);
        MutHashMap.put(8, 30, m);
        let result = MutHashMap.mapWithKey(rc, (k, v) -> k * v, m);
        // Verify insertion order is preserved
        let keys = MutHashMap.iteratorKeys(rc, result) |> Iterator.toList;
        assertEq(expected = 5 :: 2 :: 8 :: Nil, keys);
        assertEq(expected = Some(50), MutHashMap.get(5, result));
        assertEq(expected = Some(40), MutHashMap.get(2, result));
        assertEq(expected = Some(240), MutHashMap.get(8, result))
    }

    /////////////////////////////////////////////////////////////////////////////
    // transform                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def transform01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, Int32, rc] = MutHashMap.empty(rc);
        MutHashMap.transform(x -> x * 2, m);
        assertTrue(MutHashMap.isEmpty(m))
    }

    @Test
    def transform02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 10, m);
        MutHashMap.put(2, 20, m);
        MutHashMap.put(3, 30, m);
        MutHashMap.transform(x -> x * 2, m);
        assertEq(expected = Some(20), MutHashMap.get(1, m));
        assertEq(expected = Some(40), MutHashMap.get(2, m));
        assertEq(expected = Some(60), MutHashMap.get(3, m))
    }

    @Test
    def transform03(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, Int32, rc] = MutHashMap.empty(rc);
        MutHashMap.transform(x -> x * 2, m);
        assertEq(expected = 0, MutHashMap.size(m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // transformWithKey                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def transformWithKey01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, Int32, rc] = MutHashMap.empty(rc);
        MutHashMap.transformWithKey((k, v) -> k + v, m);
        assertTrue(MutHashMap.isEmpty(m))
    }

    @Test
    def transformWithKey02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 10, m);
        MutHashMap.put(2, 20, m);
        MutHashMap.put(3, 30, m);
        MutHashMap.transformWithKey((k, v) -> k + v, m);
        assertEq(expected = Some(11), MutHashMap.get(1, m));
        assertEq(expected = Some(22), MutHashMap.get(2, m));
        assertEq(expected = Some(33), MutHashMap.get(3, m))
    }

    @Test
    def transformWithKey03(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, Int32, rc] = MutHashMap.empty(rc);
        MutHashMap.transformWithKey((k, v) -> k + v, m);
        assertEq(expected = 0, MutHashMap.size(m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // refine                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def refine01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, Int32, rc] = MutHashMap.empty(rc);
        MutHashMap.refine(x -> x > 0, m);
        assertTrue(MutHashMap.isEmpty(m))
    }

    @Test
    def refine02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 10, m);
        MutHashMap.put(2, 20, m);
        MutHashMap.put(3, 30, m);
        MutHashMap.refine(x -> x >= 20, m);
        assertEq(expected = 2, MutHashMap.size(m));
        assertEq(expected = (None: Option[Int32]), MutHashMap.get(1, m));
        assertEq(expected = Some(20), MutHashMap.get(2, m));
        assertEq(expected = Some(30), MutHashMap.get(3, m))
    }

    @Test
    def refine03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 10, m);
        MutHashMap.put(2, 20, m);
        MutHashMap.put(3, 5, m);
        MutHashMap.refine(v -> v >= 10, m);
        assertEq(expected = 2, MutHashMap.size(m));
        assertEq(expected = Some(10), MutHashMap.get(1, m));
        assertEq(expected = Some(20), MutHashMap.get(2, m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // refineWithKey                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def refineWithKey01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, Int32, rc] = MutHashMap.empty(rc);
        MutHashMap.refineWithKey((k, _v) -> k > 0, m);
        assertTrue(MutHashMap.isEmpty(m))
    }

    @Test
    def refineWithKey02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 10, m);
        MutHashMap.put(2, 20, m);
        MutHashMap.put(3, 30, m);
        MutHashMap.refineWithKey((k, v) -> k + v > 20, m);
        assertEq(expected = 2, MutHashMap.size(m));
        assertEq(expected = (None: Option[Int32]), MutHashMap.get(1, m));
        assertEq(expected = Some(20), MutHashMap.get(2, m));
        assertEq(expected = Some(30), MutHashMap.get(3, m))
    }

    @Test
    def refineWithKey03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 5, m);
        MutHashMap.put(2, 10, m);
        MutHashMap.put(3, 15, m);
        MutHashMap.refineWithKey((k, v) -> k * v > 20, m);
        assertEq(expected = 1, MutHashMap.size(m));
        assertEq(expected = Some(15), MutHashMap.get(3, m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeft01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, Int32, rc] = MutHashMap.empty(rc);
        let result = MutHashMap.foldLeft((acc, v) -> acc + v, 0, m);
        assertEq(expected = 0, result)
    }

    @Test
    def foldLeft02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 10, m);
        MutHashMap.put(2, 20, m);
        MutHashMap.put(3, 30, m);
        let result = MutHashMap.foldLeft((acc, v) -> acc + v, 0, m);
        assertEq(expected = 60, result)
    }

    @Test
    def foldLeft03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 10, m);
        MutHashMap.put(2, 20, m);
        let result = MutHashMap.foldLeft((acc, v) -> acc + v, 0, m);
        assertEq(expected = 30, result)
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldLeftWithKey                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeftWithKey01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, Int32, rc] = MutHashMap.empty(rc);
        let result = MutHashMap.foldLeftWithKey((acc, k, v) -> acc + k + v, 0, m);
        assertEq(expected = 0, result)
    }

    @Test
    def foldLeftWithKey02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 10, m);
        MutHashMap.put(2, 20, m);
        MutHashMap.put(3, 30, m);
        let result = MutHashMap.foldLeftWithKey((acc, k, v) -> acc + k + v, 0, m);
        assertEq(expected = 66, result)
    }

    @Test
    def foldLeftWithKey03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 10, m);
        MutHashMap.put(2, 20, m);
        let result = MutHashMap.foldLeftWithKey((acc, k, v) -> acc + k + v, 0, m);
        assertEq(expected = 33, result)
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRight01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, String, rc] = MutHashMap.empty(rc);
        let result = MutHashMap.foldRight((v, acc) -> v :: acc, Nil, m);
        assertEq(expected = (Nil: List[String]), result)
    }

    @Test
    def foldRight02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(3, "three", m);
        let result = MutHashMap.foldRight((v, acc) -> v :: acc, Nil, m);
        // foldRight starts from tail and moves to head, so with cons we get forward order
        assertEq(expected = "one" :: "two" :: "three" :: Nil, result)
    }

    @Test
    def foldRight03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 10, m);
        MutHashMap.put(2, 20, m);
        let result = MutHashMap.foldRight((v, acc) -> v + acc, 0, m);
        assertEq(expected = 30, result)
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithKey                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRightWithKey01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, String, rc] = MutHashMap.empty(rc);
        let result = MutHashMap.foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil, m);
        assertEq(expected = (Nil: List[(Int32, String)]), result)
    }

    @Test
    def foldRightWithKey02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(3, "three", m);
        let result = MutHashMap.foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil, m);
        // foldRightWithKey starts from tail and moves to head, so with cons we get forward order
        assertEq(expected = (1, "one") :: (2, "two") :: (3, "three") :: Nil, result)
    }

    @Test
    def foldRightWithKey03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 10, m);
        MutHashMap.put(2, 20, m);
        let result = MutHashMap.foldRightWithKey((k, v, acc) -> k + v + acc, 0, m);
        assertEq(expected = 33, result)
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldMap01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, Int32, rc] = MutHashMap.empty(rc);
        let result = MutHashMap.foldMap(x -> x * 2, m);
        assertEq(expected = 0, result)
    }

    @Test
    def foldMap02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 10, m);
        MutHashMap.put(2, 20, m);
        MutHashMap.put(3, 30, m);
        let result = MutHashMap.foldMap(x -> x * 2, m);
        assertEq(expected = 120, result)
    }

    @Test
    def foldMap03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 5, m);
        MutHashMap.put(2, 10, m);
        let result = MutHashMap.foldMap(v -> v * 2, m);
        assertEq(expected = 30, result)
    }

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def exists01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, Int32, rc] = MutHashMap.empty(rc);
        assertFalse(MutHashMap.exists((_k, v) -> v > 20, m))
    }

    @Test
    def exists02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 10, m);
        MutHashMap.put(2, 20, m);
        MutHashMap.put(3, 30, m);
        assertTrue(MutHashMap.exists((_k, v) -> v > 20, m))
    }

    @Test
    def exists03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 10, m);
        MutHashMap.put(2, 20, m);
        assertFalse(MutHashMap.exists((_k, v) -> v > 30, m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forAll01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, Int32, rc] = MutHashMap.empty(rc);
        assertTrue(MutHashMap.forAll((_k, v) -> v > 0, m))
    }

    @Test
    def forAll02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 10, m);
        MutHashMap.put(2, 20, m);
        MutHashMap.put(3, 30, m);
        assertTrue(MutHashMap.forAll((_k, v) -> v > 0, m))
    }

    @Test
    def forAll03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 10, m);
        MutHashMap.put(2, 20, m);
        MutHashMap.put(3, 30, m);
        assertFalse(MutHashMap.forAll((_k, v) -> v > 15, m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def count01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, Int32, rc] = MutHashMap.empty(rc);
        assertEq(expected = 0, MutHashMap.count((_k, v) -> v > 15, m))
    }

    @Test
    def count02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 10, m);
        MutHashMap.put(2, 20, m);
        MutHashMap.put(3, 30, m);
        assertEq(expected = 2, MutHashMap.count((_k, v) -> v > 15, m))
    }

    @Test
    def count03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 10, m);
        MutHashMap.put(2, 20, m);
        MutHashMap.put(3, 30, m);
        assertEq(expected = 3, MutHashMap.count((k, v) -> k + v > 10, m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findLeft01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, Int32, rc] = MutHashMap.empty(rc);
        assertEq(expected = (None: Option[(Int32, Int32)]), MutHashMap.findLeft((_k, v) -> v > 15, m))
    }

    @Test
    def findLeft02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 10, m);
        MutHashMap.put(2, 20, m);
        MutHashMap.put(3, 30, m);
        assertEq(expected = Some((2, 20)), MutHashMap.findLeft((_k, v) -> v > 15, m))
    }

    @Test
    def findLeft03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 10, m);
        MutHashMap.put(2, 5, m);
        MutHashMap.put(3, 20, m);
        // Should return first match in insertion order (from left)
        assertEq(expected = Some((1, 10)), MutHashMap.findLeft((k, v) -> k + v > 10, m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findRight01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, Int32, rc] = MutHashMap.empty(rc);
        assertEq(expected = (None: Option[(Int32, Int32)]), MutHashMap.findRight((_k, v) -> v > 15, m))
    }

    @Test
    def findRight02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 10, m);
        MutHashMap.put(2, 20, m);
        MutHashMap.put(3, 30, m);
        assertEq(expected = Some((3, 30)), MutHashMap.findRight((_k, v) -> v > 15, m))
    }

    @Test
    def findRight03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 10, m);
        MutHashMap.put(2, 5, m);
        MutHashMap.put(3, 20, m);
        // Should return first match in reverse insertion order (from right)
        assertEq(expected = Some((3, 20)), MutHashMap.findRight((k, v) -> k + v > 10, m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // find                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def find01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, Int32, rc] = MutHashMap.empty(rc);
        assertEq(expected = (None: Option[(Int32, Int32)]), MutHashMap.find((_k, v) -> v > 15, m))
    }

    @Test
    def find02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 10, m);
        MutHashMap.put(2, 20, m);
        MutHashMap.put(3, 30, m);
        assertEq(expected = Some((2, 20)), MutHashMap.find((_k, v) -> v > 15, m))
    }

    @Test
    def find03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, 10, m);
        MutHashMap.put(2, 5, m);
        MutHashMap.put(3, 20, m);
        // Should return first match in insertion order
        assertEq(expected = Some((1, 10)), MutHashMap.find((k, v) -> k + v > 10, m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // getOrElsePut                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def getOrElsePut01(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        let result = MutHashMap.getOrElsePut(1, "one", m);
        assertEq(expected = "one", result);
        assertEq(expected = 1, MutHashMap.size(m));
        assertEq(expected = Some("one"), MutHashMap.get(1, m))
    }

    @Test
    def getOrElsePut02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "ONE", m);
        let result = MutHashMap.getOrElsePut(1, "one", m);
        assertEq(expected = "ONE", result);
        assertEq(expected = 1, MutHashMap.size(m));
        assertEq(expected = Some("ONE"), MutHashMap.get(1, m))
    }

    @Test
    def getOrElsePut03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "ONE", m);
        MutHashMap.put(2, "TWO", m);
        let result = MutHashMap.getOrElsePut(3, "three", m);
        assertEq(expected = "three", result);
        assertEq(expected = 3, MutHashMap.size(m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toList01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, String, _] = MutHashMap.empty(rc);
        assertEq(expected = Nil, MutHashMap.toList(m))
    }

    @Test
    def toList02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(3, "three", m);
        assertEq(expected = (1, "one") :: (2, "two") :: (3, "three") :: Nil, MutHashMap.toList(m))
    }

    @Test
    def toList03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(1, "ONE", m);  // Update doesn't change order
        assertEq(expected = (1, "ONE") :: (2, "two") :: Nil, MutHashMap.toList(m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toArray01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, String, _] = MutHashMap.empty(rc);
        let arr = MutHashMap.toArray(rc, m);
        assertEq(expected = 0, Array.length(arr))
    }

    @Test
    def toArray02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(3, "three", m);
        let arr = MutHashMap.toArray(rc, m);
        assertEq(expected = 3, Array.length(arr));
        assertEq(expected = (1, "one"), Array.get(0, arr));
        assertEq(expected = (2, "two"), Array.get(1, arr));
        assertEq(expected = (3, "three"), Array.get(2, arr))
    }

    @Test
    def toArray03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        let arr = MutHashMap.toArray(rc, m);
        assertEq(expected = 1, Array.length(arr));
        assertEq(expected = (1, "one"), Array.get(0, arr))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toVector01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, String, _] = MutHashMap.empty(rc);
        assertEq(expected = Vector.empty(), MutHashMap.toVector(m))
    }

    @Test
    def toVector02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(3, "three", m);
        let expected = Vector#{(1, "one"), (2, "two"), (3, "three")};
        assertEq(expected = expected, MutHashMap.toVector(m))
    }

    @Test
    def toVector03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        let expected = Vector#{(1, "one")};
        assertEq(expected = expected, MutHashMap.toVector(m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toSet01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, String, _] = MutHashMap.empty(rc);
        assertEq(expected = Set.empty(), MutHashMap.toSet(m))
    }

    @Test
    def toSet02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(3, "three", m);
        let expected = Set#{(1, "one"), (2, "two"), (3, "three")};
        assertEq(expected = expected, MutHashMap.toSet(m))
    }

    @Test
    def toSet03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        let expected = Set#{(1, "one")};
        assertEq(expected = expected, MutHashMap.toSet(m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMap01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, String, _] = MutHashMap.empty(rc);
        assertEq(expected = Map.empty(), MutHashMap.toMap(m))
    }

    @Test
    def toMap02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(3, "three", m);
        let expected = Map#{ 1 => "one", 2 => "two", 3 => "three" };
        assertEq(expected = expected, MutHashMap.toMap(m))
    }

    @Test
    def toMap03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        let expected = Map#{ 1 => "one" };
        assertEq(expected = expected, MutHashMap.toMap(m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // fromMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fromMap01(): Unit \ Assert = region rc {
        let m: Map[Int32, String] = Map.empty();
        let result = MutHashMap.fromMap(rc, m);
        assertEq(expected = 0, MutHashMap.size(result))
    }

    @Test
    def fromMap02(): Unit \ Assert = region rc {
        let m = Map#{ 1 => "one", 2 => "two", 3 => "three" };
        let result = MutHashMap.fromMap(rc, m);
        assertEq(expected = 3, MutHashMap.size(result));
        assertEq(expected = Some("one"), MutHashMap.get(1, result));
        assertEq(expected = Some("two"), MutHashMap.get(2, result));
        assertEq(expected = Some("three"), MutHashMap.get(3, result))
    }

    @Test
    def fromMap03(): Unit \ Assert = region rc {
        let m = Map#{ 1 => "one" };
        let result = MutHashMap.fromMap(rc, m);
        assertEq(expected = 1, MutHashMap.size(result));
        assertEq(expected = Some("one"), MutHashMap.get(1, result))
    }

    /////////////////////////////////////////////////////////////////////////////
    // fromList                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fromList01(): Unit \ Assert = region rc {
        let l: List[(Int32, String)] = Nil;
        let result = MutHashMap.fromList(rc, l);
        assertEq(expected = 0, MutHashMap.size(result))
    }

    @Test
    def fromList02(): Unit \ Assert = region rc {
        let l = (1, "one") :: (2, "two") :: (3, "three") :: Nil;
        let result = MutHashMap.fromList(rc, l);
        assertEq(expected = 3, MutHashMap.size(result));
        assertEq(expected = Some("one"), MutHashMap.get(1, result));
        assertEq(expected = Some("two"), MutHashMap.get(2, result));
        assertEq(expected = Some("three"), MutHashMap.get(3, result))
    }

    @Test
    def fromList03(): Unit \ Assert = region rc {
        // Duplicate keys - last occurrence wins
        let l = (1, "one") :: (2, "two") :: (1, "ONE") :: Nil;
        let result = MutHashMap.fromList(rc, l);
        assertEq(expected = 2, MutHashMap.size(result));
        assertEq(expected = Some("ONE"), MutHashMap.get(1, result));
        assertEq(expected = Some("two"), MutHashMap.get(2, result))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toString01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, String, _] = MutHashMap.empty(rc);
        assertEq(expected = "MutHashMap#{}", MutHashMap.toString(m))
    }

    @Test
    def toString02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        assertEq(expected = "MutHashMap#{1 -> one}", MutHashMap.toString(m))
    }

    @Test
    def toString03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(3, "three", m);
        assertEq(expected = "MutHashMap#{1 -> one, 2 -> two, 3 -> three}", MutHashMap.toString(m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // joinKeys                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinKeys01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, String, _] = MutHashMap.empty(rc);
        assertEq(expected = "", MutHashMap.joinKeys(", ", m))
    }

    @Test
    def joinKeys02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        assertEq(expected = "1", MutHashMap.joinKeys(", ", m))
    }

    @Test
    def joinKeys03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(3, "three", m);
        assertEq(expected = "1, 2, 3", MutHashMap.joinKeys(", ", m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // joinValues                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinValues01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, String, _] = MutHashMap.empty(rc);
        assertEq(expected = "", MutHashMap.joinValues(", ", m))
    }

    @Test
    def joinValues02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        assertEq(expected = "one", MutHashMap.joinValues(", ", m))
    }

    @Test
    def joinValues03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(3, "three", m);
        assertEq(expected = "one, two, three", MutHashMap.joinValues(", ", m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinWith01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, String, _] = MutHashMap.empty(rc);
        assertEq(expected = "", MutHashMap.joinWith((k, v) -> "${k}=${v}", ", ", m))
    }

    @Test
    def joinWith02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        assertEq(expected = "1=one", MutHashMap.joinWith((k, v) -> "${k}=${v}", ", ", m))
    }

    @Test
    def joinWith03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(3, "three", m);
        assertEq(expected = "1=one, 2=two, 3=three", MutHashMap.joinWith((k, v) -> "${k}=${v}", ", ", m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // Iterable instance                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterable01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, String, _] = MutHashMap.empty(rc);
        let result = Iterable.iterator(rc, m) |> Iterator.toList;
        assertEq(expected = Nil, result)
    }

    @Test
    def iterable02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(3, "three", m);
        let result = Iterable.iterator(rc, m) |> Iterator.toList;
        assertEq(expected = (1, "one") :: (2, "two") :: (3, "three") :: Nil, result)
    }

    @Test
    def iterable03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(3, "three", m);
        // Test that we can use foreach syntax
        let acc = Ref.fresh(rc, 0);
        foreach ((k, _) <- m) {
            Ref.put(Ref.get(acc) + k, acc)
        };
        assertEq(expected = 6, Ref.get(acc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // ForEach instance                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEachInstance01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, String, _] = MutHashMap.empty(rc);
        let acc = Ref.fresh(rc, 0);
        ForEach.forEach(match (k, _) -> Ref.put(Ref.get(acc) + k, acc), m);
        assertEq(expected = 0, Ref.get(acc))
    }

    @Test
    def forEachInstance02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(3, "three", m);
        let acc = Ref.fresh(rc, 0);
        ForEach.forEach(match (k, _) -> Ref.put(Ref.get(acc) + k, acc), m);
        assertEq(expected = 6, Ref.get(acc))
    }

    @Test
    def forEachInstance03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(3, "three", m);
        let result = Ref.fresh(rc, "");
        ForEach.forEach(match (_, v) -> {
            let current = Ref.get(result);
            Ref.put(if (current == "") v else "${current},${v}", result)
        }, m);
        assertEq(expected = "one,two,three", Ref.get(result))
    }

    /////////////////////////////////////////////////////////////////////////////
    // keysOf                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def keysOf01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, String, _] = MutHashMap.empty(rc);
        assertEq(expected = Set.empty(), MutHashMap.keysOf(m))
    }

    @Test
    def keysOf02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(3, "three", m);
        assertEq(expected = Set#{1, 2, 3}, MutHashMap.keysOf(m))
    }

    @Test
    def keysOf03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(1, "ONE", m);  // Update doesn't change order
        assertEq(expected = Set#{1, 2}, MutHashMap.keysOf(m))
    }

    /////////////////////////////////////////////////////////////////////////////
    // valuesOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def valuesOf01(): Unit \ Assert = region rc {
        let m: MutHashMap[Int32, String, _] = MutHashMap.empty(rc);
        assertEq(expected = Nil, MutHashMap.valuesOf(m))
    }

    @Test
    def valuesOf02(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(3, "three", m);
        assertEq(expected = "one" :: "two" :: "three" :: Nil, MutHashMap.valuesOf(m))
    }

    @Test
    def valuesOf03(): Unit \ Assert = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        MutHashMap.put(1, "ONE", m);  // Update changes value but not order
        assertEq(expected = "ONE" :: "two" :: Nil, MutHashMap.valuesOf(m))
    }

}
