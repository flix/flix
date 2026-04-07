mod Test.Exp.Jvm.Generic.InvokeMethod.Inherited {

    use Assert.{assertEq, assertTrue}

    import java.lang.{Integer => JInteger}
    import java.util.ArrayList
    import java.util.LinkedList
    import java.util.{List => JList}
    import java.util.{Iterator => JIterator}
    import java.util.TreeMap

    // Tests for inherited methods through the generic type hierarchy

    @Test
    def testArrayListContainsAll01(): Unit \ {Assert, IO} =
        let l1: ArrayList[String] = new ArrayList();
        l1.add("a");
        l1.add("b");
        l1.add("c");
        let l2: ArrayList[String] = new ArrayList();
        l2.add("a");
        l2.add("c");
        assertTrue(l1.containsAll(l2))

    @Test
    def testArrayListContainsAll02(): Unit \ {Assert, IO} =
        let l1: ArrayList[String] = new ArrayList();
        l1.add("a");
        l1.add("b");
        let l2: ArrayList[String] = new ArrayList();
        l2.add("a");
        l2.add("c");
        assertTrue(not l1.containsAll(l2))

    @Test
    def testArrayListAddAll01(): Unit \ {Assert, IO} =
        let l1: ArrayList[String] = new ArrayList();
        l1.add("a");
        let l2: ArrayList[String] = new ArrayList();
        l2.add("b");
        l2.add("c");
        l1.addAll(l2);
        assertEq(expected = 3, l1.size())

    @Test
    def testLinkedListContains01(): Unit \ {Assert, IO} =
        let l: LinkedList[String] = new LinkedList();
        l.add("x");
        l.add("y");
        assertTrue(l.contains("x"))

    @Test
    def testTreeMapInheritedKeySet01(): Unit \ {Assert, IO} =
        let m: TreeMap[String, JInteger] = new TreeMap();
        m.put("a", JInteger.valueOf(1i32));
        m.put("b", JInteger.valueOf(2i32));
        m.put("c", JInteger.valueOf(3i32));
        assertEq(expected = 3, m.keySet().size())

    @Test
    def testTreeMapInheritedValues01(): Unit \ {Assert, IO} =
        let m: TreeMap[String, JInteger] = new TreeMap();
        m.put("x", JInteger.valueOf(10i32));
        m.put("y", JInteger.valueOf(20i32));
        assertEq(expected = 2, m.values().size())

    @Test
    def testTreeMapInheritedContainsKey01(): Unit \ {Assert, IO} =
        let m: TreeMap[String, JInteger] = new TreeMap();
        m.put("key1", JInteger.valueOf(1i32));
        assertTrue(m.containsKey("key1"))

    @Test
    def testTreeMapInheritedContainsValue01(): Unit \ {Assert, IO} =
        let m: TreeMap[String, JInteger] = new TreeMap();
        m.put("key", JInteger.valueOf(42i32));
        assertTrue(m.containsValue(JInteger.valueOf(42i32)))

    @Test
    def testArrayListIterator01(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        l.add("only");
        let it: JIterator[String] = l.iterator();
        assertTrue(it.hasNext());
        let r: String = it.next();
        assertEq(expected = "only", r);
        assertTrue(not it.hasNext())

    @Test
    def testArrayListSubList01(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        l.add("a");
        l.add("b");
        l.add("c");
        l.add("d");
        let sub: JList[String] = l.subList(1, 3);
        assertEq(expected = 2, sub.size());
        let r: String = sub.get(0);
        assertEq(expected = "b", r)

    @Test
    def testArrayListIndexOf01(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        l.add("x");
        l.add("y");
        l.add("z");
        assertEq(expected = 1, l.indexOf("y"))

    @Test
    def testArrayListLastIndexOf01(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        l.add("a");
        l.add("b");
        l.add("a");
        assertEq(expected = 2, l.lastIndexOf("a"))

    @Test
    def testTreeMapFirstKey01(): Unit \ {Assert, IO} =
        let m: TreeMap[String, JInteger] = new TreeMap();
        m.put("c", JInteger.valueOf(3i32));
        m.put("a", JInteger.valueOf(1i32));
        m.put("b", JInteger.valueOf(2i32));
        let r: String = m.firstKey();
        assertEq(expected = "a", r)

    @Test
    def testTreeMapLastKey01(): Unit \ {Assert, IO} =
        let m: TreeMap[String, JInteger] = new TreeMap();
        m.put("c", JInteger.valueOf(3i32));
        m.put("a", JInteger.valueOf(1i32));
        m.put("b", JInteger.valueOf(2i32));
        let r: String = m.lastKey();
        assertEq(expected = "c", r)

}
