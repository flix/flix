mod Test.Exp.Jvm.Generic.InvokeMethod {

    use Assert.{assertEq, assertTrue}

    import java.lang.{Integer => JInteger}
    import java.lang.Object
    import java.util.ArrayList
    import java.util.HashMap
    import java.util.LinkedList
    import java.util.HashSet
    import java.util.TreeMap

    @Test
    def testArrayListAdd01(): Unit \ Assert + IO =
        let list: ArrayList[String] = new ArrayList();
        assertTrue(list.add("hello"))

    @Test
    def testArrayListSize01(): Unit \ Assert + IO =
        let list: ArrayList[String] = new ArrayList();
        list.add("a");
        list.add("b");
        list.add("c");
        assertEq(expected = 3, list.size())

    @Test
    def testArrayListIsEmpty01(): Unit \ Assert + IO =
        let list: ArrayList[String] = new ArrayList();
        assertTrue(list.isEmpty())

    @Test
    def testArrayListIsEmpty02(): Unit \ Assert + IO =
        let list: ArrayList[String] = new ArrayList();
        list.add("hello");
        assertTrue(not list.isEmpty())

    @Test
    def testArrayListGet01(): Unit \ Assert + IO =
        let list: ArrayList[String] = new ArrayList();
        list.add("hello");
        let result = unchecked_cast(list.get(0) as String);
        assertEq(expected = "hello", result)

    @Test
    def testArrayListContains01(): Unit \ Assert + IO =
        let list: ArrayList[String] = new ArrayList();
        list.add("hello");
        let obj: Object = checked_cast("hello");
        assertTrue(list.contains(obj))

    @Test
    def testArrayListContains02(): Unit \ Assert + IO =
        let list: ArrayList[String] = new ArrayList();
        list.add("hello");
        let obj: Object = checked_cast("missing");
        assertTrue(not list.contains(obj))

    @Test
    def testArrayListRemove01(): Unit \ Assert + IO =
        let list: ArrayList[String] = new ArrayList();
        list.add("hello");
        list.add("world");
        list.remove(0);
        assertEq(expected = 1, list.size())

    @Test
    def testArrayListClear01(): Unit \ Assert + IO =
        let list: ArrayList[String] = new ArrayList();
        list.add("hello");
        list.add("world");
        list.clear();
        assertTrue(list.isEmpty())

    @Test
    def testArrayListSet01(): Unit \ Assert + IO =
        let list: ArrayList[String] = new ArrayList();
        list.add("old");
        list.set(0, "new");
        let result = unchecked_cast(list.get(0) as String);
        assertEq(expected = "new", result)

    @Test
    def testHashMapPut01(): Unit \ Assert + IO =
        let map: HashMap[String, JInteger] = new HashMap();
        map.put("key", JInteger.valueOf(42i32));
        assertEq(expected = 1, map.size())

    @Test
    def testHashMapGet01(): Unit \ Assert + IO =
        let map: HashMap[String, JInteger] = new HashMap();
        map.put("key", JInteger.valueOf(42i32));
        let result = unchecked_cast(map.get("key") as JInteger);
        assertEq(expected = 42, result.intValue())

    @Test
    def testHashMapContainsKey01(): Unit \ Assert + IO =
        let map: HashMap[String, JInteger] = new HashMap();
        map.put("key", JInteger.valueOf(1i32));
        let obj: Object = checked_cast("key");
        assertTrue(map.containsKey(obj))

    @Test
    def testHashMapContainsKey02(): Unit \ Assert + IO =
        let map: HashMap[String, JInteger] = new HashMap();
        map.put("key", JInteger.valueOf(1i32));
        let obj: Object = checked_cast("missing");
        assertTrue(not map.containsKey(obj))

    @Test
    def testHashMapRemove01(): Unit \ Assert + IO =
        let map: HashMap[String, JInteger] = new HashMap();
        map.put("key", JInteger.valueOf(1i32));
        let obj: Object = checked_cast("key");
        map.remove(obj);
        assertEq(expected = 0, map.size())

    @Test
    def testHashMapIsEmpty01(): Unit \ Assert + IO =
        let map: HashMap[String, JInteger] = new HashMap();
        assertTrue(map.isEmpty())

    @Test
    def testHashMapSize01(): Unit \ Assert + IO =
        let map: HashMap[String, JInteger] = new HashMap();
        map.put("a", JInteger.valueOf(1i32));
        map.put("b", JInteger.valueOf(2i32));
        map.put("c", JInteger.valueOf(3i32));
        assertEq(expected = 3, map.size())

    @Test
    def testLinkedListAdd01(): Unit \ Assert + IO =
        let list: LinkedList[String] = new LinkedList();
        list.add("elem");
        assertEq(expected = 1, list.size())

    @Test
    def testLinkedListAddFirst01(): Unit \ Assert + IO =
        let list: LinkedList[String] = new LinkedList();
        list.add("second");
        list.addFirst("first");
        let result = unchecked_cast(list.getFirst() as String);
        assertEq(expected = "first", result)

    @Test
    def testHashSetAdd01(): Unit \ Assert + IO =
        let set: HashSet[String] = new HashSet();
        assertTrue(set.add("elem"));
        assertTrue(not set.add("elem"))

    @Test
    def testHashSetContains01(): Unit \ Assert + IO =
        let set: HashSet[String] = new HashSet();
        set.add("elem");
        let obj: Object = checked_cast("elem");
        assertTrue(set.contains(obj))

    @Test
    def testHashSetSize01(): Unit \ Assert + IO =
        let set: HashSet[String] = new HashSet();
        set.add("a");
        set.add("b");
        set.add("c");
        assertEq(expected = 3, set.size())

    @Test
    def testTreeMapPut01(): Unit \ Assert + IO =
        let map: TreeMap[String, JInteger] = new TreeMap();
        map.put("key", JInteger.valueOf(42i32));
        assertEq(expected = 1, map.size())

    @Test
    def testTreeMapGet01(): Unit \ Assert + IO =
        let map: TreeMap[String, JInteger] = new TreeMap();
        map.put("key", JInteger.valueOf(42i32));
        let result = unchecked_cast(map.get("key") as JInteger);
        assertEq(expected = 42, result.intValue())

    @Test
    def testArrayListToString01(): Unit \ Assert + IO =
        let list: ArrayList[String] = new ArrayList();
        list.add("hello");
        let str = list.toString();
        assertTrue(not str.isEmpty())

}
