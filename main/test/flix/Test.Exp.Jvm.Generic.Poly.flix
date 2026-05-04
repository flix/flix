mod Test.Exp.Jvm.Generic.Poly {

    use Assert.{assertEq, assertTrue}

    import java.util.ArrayList
    import java.util.Collection
    import java.util.HashMap
    import java.util.HashSet
    import java.util.Iterator
    import java.util.LinkedList
    import java.util.{List => JList}
    import java.util.{Set => JSet}
    import java.util.Optional
    import java.util.Objects
    import java.util.TreeMap

    @Test
    def testAddStringToArrayList01(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        assertTrue(l.add("hello"))

    @Test
    def testAddInt32ToArrayList02(): Unit \ {Assert, IO} =
        let l: ArrayList[Int32] = new ArrayList();
        assertTrue(l.add(42))

    @Test
    def testAddFloat64ToArrayList03(): Unit \ {Assert, IO} =
        let l: ArrayList[Float64] = new ArrayList();
        assertTrue(l.add(1.0))

    @Test
    def testAddBoolToArrayList04(): Unit \ {Assert, IO} =
        let l: ArrayList[Bool] = new ArrayList();
        assertTrue(l.add(true))

    @Test
    def testAddCharToArrayList05(): Unit \ {Assert, IO} =
        let l: ArrayList[Char] = new ArrayList();
        assertTrue(l.add('a'))

    @Test
    def testHashMapPut06(): Unit \ {Assert, IO} =
        let m: HashMap[String, Int32] = new HashMap();
        m.put("k", 42);
        assertEq(expected = 1, m.size())

    @Test
    def testHashMapPutStringString07(): Unit \ {Assert, IO} =
        let m: HashMap[String, String] = new HashMap();
        m.put("k", "v");
        assertEq(expected = "v", m.get("k"))

    @Test
    def testHashSetAdd08(): Unit \ {Assert, IO} =
        let s: HashSet[String] = new HashSet();
        assertTrue(s.add("x"))

    @Test
    def testLinkedListAdd09(): Unit \ {Assert, IO} =
        let l: LinkedList[String] = new LinkedList();
        assertTrue(l.add("x"))

    @Test
    def testTreeMapPut10(): Unit \ {Assert, IO} =
        let m: TreeMap[String, Int32] = new TreeMap();
        m.put("k", 1);
        assertEq(expected = 1, m.size())

    @Test
    def testArrayListSet11(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        l.add("a");
        l.set(0, "x");
        assertEq(expected = "x", l.get(0))

    @Test
    def testArrayListGetReturnsString12(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        l.add("hello");
        assertEq(expected = "hello", l.get(0))

    @Test
    def testHashMapGetReturnsInt13(): Unit \ {Assert, IO} =
        let m: HashMap[String, Int32] = new HashMap();
        m.put("k", 42);
        assertEq(expected = 42, m.get("k"))

    @Test
    def testHashMapKeySetReturnsSetString14(): Unit \ {Assert, IO} =
        let m: HashMap[String, Int32] = new HashMap();
        m.put("hello", 1);
        let s: JSet[String] = m.keySet();
        assertEq(expected = 1, s.size())

    @Test
    def testHashMapValuesReturnsCollectionInt15(): Unit \ {Assert, IO} =
        let m: HashMap[String, Int32] = new HashMap();
        m.put("k", 99);
        let v: Collection[Int32] = m.values();
        assertEq(expected = 1, v.size())

    @Test
    def testArrayListIteratorReturnsIteratorString16(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        l.add("a");
        let it: Iterator[String] = l.iterator();
        assertTrue(it.hasNext())

    @Test
    def testArrayListSubListReturnsListString17(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        l.add("a");
        l.add("b");
        let sub: JList[String] = l.subList(0, 1);
        assertEq(expected = 1, sub.size())

    @Test
    def testArrayListSizeReturnsInt18(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        l.add("a");
        l.add("b");
        assertEq(expected = 2, l.size())

    @Test
    def testArrayListIsEmptyReturnsBool19(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        assertTrue(l.isEmpty())

    @Test
    def testLinkedListGetFirstReturnsString20(): Unit \ {Assert, IO} =
        let l: LinkedList[String] = new LinkedList();
        l.add("hello");
        assertEq(expected = "hello", l.getFirst())

    /// Polymorphic add to ArrayList.
    def polyAdd(l: ArrayList[a], x: a): Bool \ IO = l.add(x)

    @Test
    def testPolyAdd21(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        assertTrue(polyAdd(l, "hello"))

    @Test
    def testPolyAdd22(): Unit \ {Assert, IO} =
        let l: ArrayList[Int32] = new ArrayList();
        assertTrue(polyAdd(l, 42))

    /// Polymorphic get from ArrayList.
    def polyGet(l: ArrayList[a], i: Int32): a \ IO = l.get(i)

    @Test
    def testPolyGet23(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        discard l.add("world");
        assertEq(expected = "world", polyGet(l, 0))

    /// Polymorphic Optional.of.
    def polyOptionalOf(x: a): Optional[a] \ IO = Optional.of(x)

    @Test
    def testPolyOptionalOf24(): Unit \ {Assert, IO} =
        let opt = polyOptionalOf("test");
        assertTrue(opt.isPresent())

    @Test
    def testPolyOptionalOf25(): Unit \ {Assert, IO} =
        let opt = polyOptionalOf(99);
        assertTrue(opt.isPresent())

    /// Polymorphic Objects.isNull.
    def polyIsNull(x: a): Bool = Objects.isNull(x)

    @Test
    def testPolyIsNull26(): Unit \ Assert =
        assertTrue(not polyIsNull("hello"))

    /// Polymorphic HashMap put and get.
    def polyPut(m: HashMap[k, v], k: k, v: v): Unit \ IO =
        discard m.put(k, v)

    def polyGet2(m: HashMap[k, v], k: k): v \ IO = m.get(k)

    @Test
    def testPolyHashMap27(): Unit \ {Assert, IO} =
        let m: HashMap[String, Int32] = new HashMap();
        polyPut(m, "x", 1);
        assertEq(expected = 1, polyGet2(m, "x"))

}
