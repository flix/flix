mod Test.Exp.Jvm.Generic.InvokeStaticMethod {

    use Assert.{assertEq, assertTrue}

    import java.lang.{Integer => JInteger}
    import java.lang.Object
    import java.util.ArrayList
    import java.util.{Collections => JCollections}
    import java.util.{List => JList}
    import java.util.{Map => JMap}
    import java.util.Optional
    import java.util.{Set => JSet}

    // CompileTests

    @CompileTest
    def testCollectionsEmptyList01(): JList[String] \ IO =
        JCollections.emptyList()

    @CompileTest
    def testCollectionsEmptyMap01(): JMap[String, JInteger] \ IO =
        JCollections.emptyMap()

    @CompileTest
    def testCollectionsEmptySet01(): JSet[String] \ IO =
        JCollections.emptySet()

    @CompileTest
    def testListOf01(): JList[String] \ IO =
        JList.of("a", "b")

    @CompileTest
    def testMapOf01(): JMap[String, String] \ IO =
        JMap.of("k", "v")

    // Tests

    @Test
    def testOptionalEmpty01(): Unit \ {Assert, IO} =
        let o: Optional[String] = Optional.empty();
        assertTrue(o.isEmpty())

    @Test
    def testOptionalOf01(): Unit \ {Assert, IO} =
        let o: Optional[String] = Optional.of("hello");
        assertTrue(o.isPresent())

    @Test
    def testOptionalOfGet01(): Unit \ {Assert, IO} =
        let o: Optional[String] = Optional.of("hello");
        let r: String = o.get();
        assertEq(expected = "hello", r)

    @Test
    def testCollectionsSingletonList01(): Unit \ {Assert, IO} =
        let l: JList[String] = JCollections.singletonList("hello");
        assertEq(expected = 1, l.size())

    @Test
    def testCollectionsSingletonMap01(): Unit \ {Assert, IO} =
        let m: JMap[String, String] = JCollections.singletonMap("k", "v");
        assertEq(expected = 1, m.size())

    @Test
    def testIntegerValueOf01(): Unit \ {Assert, IO} =
        let l: ArrayList[JInteger] = new ArrayList();
        l.add(JInteger.valueOf(42i32));
        assertEq(expected = 1, l.size())

    @Test
    def testCollectionsUnmodifiableList01(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        l.add("hello");
        let u: JList[String] = JCollections.unmodifiableList(l);
        assertEq(expected = 1, u.size())

    @Test
    def testCollectionsSort01(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        l.add("c");
        l.add("a");
        l.add("b");
        JCollections.sort(l);
        let r: String = l.get(0);
        assertEq(expected = "a", r)

    @Test
    def testCollectionsReverse01(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        l.add("a");
        l.add("b");
        l.add("c");
        JCollections.reverse(l);
        let r: String = l.get(0);
        assertEq(expected = "c", r)

    @Test
    def testCollectionsFrequency01(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        l.add("hello");
        l.add("world");
        l.add("hello");
        let o: Object = checked_cast("hello");
        assertEq(expected = 2, JCollections.frequency(l, o))

    @Test
    def testCollectionsMin01(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        l.add("b");
        l.add("a");
        l.add("c");
        let r: String = JCollections.min(l);
        assertEq(expected = "a", r)

    @Test
    def testCollectionsMax01(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        l.add("b");
        l.add("a");
        l.add("c");
        let r: String = JCollections.max(l);
        assertEq(expected = "c", r)

    @Test
    def testListOf02(): Unit \ {Assert, IO} =
        let l: JList[String] = JList.of("a", "b");
        assertEq(expected = 2, l.size())

    @Test
    def testOptionalOrElse01(): Unit \ {Assert, IO} =
        let o: Optional[String] = Optional.empty();
        let r: String = o.orElse("default");
        assertEq(expected = "default", r)

    @Test
    def testArraysAsList01(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        l.add("a");
        l.add("b");
        l.add("c");
        let u: JList[String] = JCollections.unmodifiableList(l);
        assertEq(expected = 3, u.size())

}
