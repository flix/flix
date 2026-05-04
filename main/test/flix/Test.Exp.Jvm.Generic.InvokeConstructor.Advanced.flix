mod Test.Exp.Jvm.Generic.InvokeConstructor.Advanced {

    use Assert.{assertEq, assertTrue}

    import java.lang.{Boolean => JBoolean}
    import java.lang.{Double => JDouble}
    import java.lang.{Integer => JInteger}
    import java.lang.{Long => JLong}
    import java.util.ArrayList
    import java.util.HashMap
    import java.util.HashSet
    import java.util.LinkedHashMap
    import java.util.LinkedList
    import java.util.TreeMap

    // CompileTests

    @CompileTest
    def testNestedGenericConstructor01(): ArrayList[ArrayList[JInteger]] \ IO =
        new ArrayList()

    @CompileTest
    def testNestedGenericConstructor02(): HashMap[String, ArrayList[JInteger]] \ IO =
        new HashMap()

    @CompileTest
    def testTripleNestedConstructor01(): ArrayList[HashMap[String, ArrayList[JInteger]]] \ IO =
        new ArrayList()

    @CompileTest
    def testHashMapBooleanValue01(): HashMap[String, JBoolean] \ IO =
        new HashMap()

    @CompileTest
    def testHashMapDoubleValue01(): HashMap[String, JDouble] \ IO =
        new HashMap()

    @CompileTest
    def testHashMapLongValue01(): HashMap[JLong, String] \ IO =
        new HashMap()

    // Tests

    @Test
    def testNestedGenericUsage01(): Unit \ {Assert, IO} =
        let outer: ArrayList[ArrayList[String]] = new ArrayList();
        let inner: ArrayList[String] = new ArrayList();
        inner.add("hello");
        outer.add(inner);
        assertEq(expected = 1, outer.size());
        let retrieved: ArrayList[String] = outer.get(0);
        assertEq(expected = 1, retrieved.size());
        let s: String = retrieved.get(0);
        assertEq(expected = "hello", s)

    @Test
    def testNestedGenericUsage02(): Unit \ {Assert, IO} =
        let m: HashMap[String, ArrayList[JInteger]] = new HashMap();
        let l: ArrayList[JInteger] = new ArrayList();
        l.add(JInteger.valueOf(1i32));
        l.add(JInteger.valueOf(2i32));
        m.put("nums", l);
        let retrieved: ArrayList[JInteger] = m.get("nums");
        assertEq(expected = 2, retrieved.size())

    @Test
    def testHashMapBooleanUsage01(): Unit \ {Assert, IO} =
        let m: HashMap[String, JBoolean] = new HashMap();
        m.put("flag", JBoolean.valueOf(true));
        let r: JBoolean = m.get("flag");
        assertTrue(r.booleanValue())

    @Test
    def testHashMapDoubleUsage01(): Unit \ {Assert, IO} =
        let m: HashMap[String, JDouble] = new HashMap();
        m.put("pi", JDouble.valueOf(3.14f64));
        let r: JDouble = m.get("pi");
        assertTrue(r.doubleValue() > 3.0f64)

    @Test
    def testLinkedHashMapOrdering01(): Unit \ {Assert, IO} =
        let m: LinkedHashMap[String, JInteger] = new LinkedHashMap();
        m.put("c", JInteger.valueOf(3i32));
        m.put("a", JInteger.valueOf(1i32));
        m.put("b", JInteger.valueOf(2i32));
        let it = m.keySet().iterator();
        let first: String = it.next();
        assertEq(expected = "c", first)

    @Test
    def testTreeMapWithIntegerKeys01(): Unit \ {Assert, IO} =
        let m: TreeMap[JInteger, String] = new TreeMap();
        m.put(JInteger.valueOf(3i32), "three");
        m.put(JInteger.valueOf(1i32), "one");
        m.put(JInteger.valueOf(2i32), "two");
        let first: JInteger = m.firstKey();
        assertEq(expected = 1, first.intValue())

    @Test
    def testMultipleCollectionsSameType01(): Unit \ {Assert, IO} =
        let list: ArrayList[String] = new ArrayList();
        let set: HashSet[String] = new HashSet();
        let linked: LinkedList[String] = new LinkedList();
        list.add("a");
        set.add("a");
        linked.add("a");
        assertEq(expected = 1, list.size());
        assertEq(expected = 1, set.size());
        assertEq(expected = 1, linked.size())

    @Test
    def testArrayListWithCapacity01(): Unit \ {Assert, IO} =
        let l: ArrayList[JInteger] = new ArrayList(100);
        l.add(JInteger.valueOf(42i32));
        assertEq(expected = 1, l.size());
        let r: JInteger = l.get(0);
        assertEq(expected = 42, r.intValue())

    @Test
    def testHashMapWithCapacityAndLoadFactor01(): Unit \ {Assert, IO} =
        let m: HashMap[String, JInteger] = new HashMap(16, 0.75f32);
        m.put("key", JInteger.valueOf(1i32));
        assertEq(expected = 1, m.size())

}
