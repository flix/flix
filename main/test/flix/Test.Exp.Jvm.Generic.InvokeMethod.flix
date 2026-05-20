mod Test.Exp.Jvm.Generic.InvokeMethod {

    use Assert.{assertEq, assertTrue}

    import java.lang.{Integer => JInteger}
    import java.lang.Object
    import java.util.ArrayList
    import java.util.HashMap
    import java.util.HashSet
    import java.util.LinkedList
    import java.util.TreeMap

    @Test
    def testArrayListAdd01(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        assertTrue(l.add("hello"))

    @Test
    def testArrayListSize01(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        l.add("a");
        l.add("b");
        l.add("c");
        assertEq(expected = 3, l.size())

    @Test
    def testArrayListIsEmpty01(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        assertTrue(l.isEmpty())

    @Test
    def testArrayListIsEmpty02(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        l.add("hello");
        assertTrue(not l.isEmpty())

    @Test
    def testArrayListGet01(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        l.add("hello");
        let r: String = l.get(0);
        assertEq(expected = "hello", r)

    @Test
    def testArrayListContains01(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        l.add("hello");
        let o: Object = checked_cast("hello");
        assertTrue(l.contains(o))

    @Test
    def testArrayListContains02(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        l.add("hello");
        let o: Object = checked_cast("missing");
        assertTrue(not l.contains(o))

    @Test
    def testArrayListRemove01(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        l.add("hello");
        l.add("world");
        l.remove(0);
        assertEq(expected = 1, l.size())

    @Test
    def testArrayListClear01(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        l.add("hello");
        l.add("world");
        l.clear();
        assertTrue(l.isEmpty())

    @Test
    def testArrayListSet01(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        l.add("old");
        l.set(0, "new");
        let r: String = l.get(0);
        assertEq(expected = "new", r)

    @Test
    def testHashMapPut01(): Unit \ {Assert, IO} =
        let m: HashMap[String, JInteger] = new HashMap();
        m.put("key", JInteger.valueOf(42i32));
        assertEq(expected = 1, m.size())

    @Test
    def testHashMapGet01(): Unit \ {Assert, IO} =
        let m: HashMap[String, JInteger] = new HashMap();
        m.put("key", JInteger.valueOf(42i32));
        let r: JInteger = m.get("key");
        assertEq(expected = 42, r.intValue())

    @Test
    def testHashMapContainsKey01(): Unit \ {Assert, IO} =
        let m: HashMap[String, JInteger] = new HashMap();
        m.put("key", JInteger.valueOf(1i32));
        let o: Object = checked_cast("key");
        assertTrue(m.containsKey(o))

    @Test
    def testHashMapContainsKey02(): Unit \ {Assert, IO} =
        let m: HashMap[String, JInteger] = new HashMap();
        m.put("key", JInteger.valueOf(1i32));
        let o: Object = checked_cast("missing");
        assertTrue(not m.containsKey(o))

    @Test
    def testHashMapRemove01(): Unit \ {Assert, IO} =
        let m: HashMap[String, JInteger] = new HashMap();
        m.put("key", JInteger.valueOf(1i32));
        let o: Object = checked_cast("key");
        m.remove(o);
        assertEq(expected = 0, m.size())

    @Test
    def testHashMapIsEmpty01(): Unit \ {Assert, IO} =
        let m: HashMap[String, JInteger] = new HashMap();
        assertTrue(m.isEmpty())

    @Test
    def testHashMapSize01(): Unit \ {Assert, IO} =
        let m: HashMap[String, JInteger] = new HashMap();
        m.put("a", JInteger.valueOf(1i32));
        m.put("b", JInteger.valueOf(2i32));
        m.put("c", JInteger.valueOf(3i32));
        assertEq(expected = 3, m.size())

    @Test
    def testLinkedListAdd01(): Unit \ {Assert, IO} =
        let l: LinkedList[String] = new LinkedList();
        l.add("elem");
        assertEq(expected = 1, l.size())

    @Test
    def testLinkedListAddFirst01(): Unit \ {Assert, IO} =
        let l: LinkedList[String] = new LinkedList();
        l.add("second");
        l.addFirst("first");
        let r: String = l.getFirst();
        assertEq(expected = "first", r)

    @Test
    def testHashSetAdd01(): Unit \ {Assert, IO} =
        let s: HashSet[String] = new HashSet();
        assertTrue(s.add("elem"));
        assertTrue(not s.add("elem"))

    @Test
    def testHashSetContains01(): Unit \ {Assert, IO} =
        let s: HashSet[String] = new HashSet();
        s.add("elem");
        let o: Object = checked_cast("elem");
        assertTrue(s.contains(o))

    @Test
    def testHashSetSize01(): Unit \ {Assert, IO} =
        let s: HashSet[String] = new HashSet();
        s.add("a");
        s.add("b");
        s.add("c");
        assertEq(expected = 3, s.size())

    @Test
    def testTreeMapPut01(): Unit \ {Assert, IO} =
        let m: TreeMap[String, JInteger] = new TreeMap();
        m.put("key", JInteger.valueOf(42i32));
        assertEq(expected = 1, m.size())

    @Test
    def testTreeMapGet01(): Unit \ {Assert, IO} =
        let m: TreeMap[String, JInteger] = new TreeMap();
        m.put("key", JInteger.valueOf(42i32));
        let r: JInteger = m.get("key");
        assertEq(expected = 42, r.intValue())

    @Test
    def testArrayListToString01(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        l.add("hello");
        let s = l.toString();
        assertTrue(not s.isEmpty())

}
