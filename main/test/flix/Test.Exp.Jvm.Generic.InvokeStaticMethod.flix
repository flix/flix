mod Test.Exp.Jvm.Generic.InvokeStaticMethod {

    use Assert.{assertEq, assertTrue}

    import java.lang.{Integer => JInteger}
    import java.lang.Object
    import java.util.ArrayList
    import java.util.{Collections => JCollections}
    import java.util.{List => JList}
    import java.util.{Map => JMap}
    import java.util.{Set => JSet}
    import java.util.Optional

    @Test
    def testOptionalEmpty01(): Unit \ Assert + IO =
        let opt: Optional[String] = Optional.empty();
        assertTrue(opt.isEmpty())

    @Test
    def testOptionalOf01(): Unit \ Assert + IO =
        let opt: Optional[String] = Optional.of("hello");
        assertTrue(opt.isPresent())

    @Test
    def testOptionalOfGet01(): Unit \ Assert + IO =
        let opt: Optional[String] = Optional.of("hello");
        let result = unchecked_cast(opt.get() as String);
        assertEq(expected = "hello", result)

    @CompileTest
    def testCollectionsEmptyList01(): JList[String] \ IO =
        JCollections.emptyList()

    @CompileTest
    def testCollectionsEmptyMap01(): JMap[String, JInteger] \ IO =
        JCollections.emptyMap()

    @CompileTest
    def testCollectionsEmptySet01(): JSet[String] \ IO =
        JCollections.emptySet()

    @Test
    def testCollectionsSingletonList01(): Unit \ Assert + IO =
        let list: JList[String] = JCollections.singletonList("hello");
        assertEq(expected = 1, list.size())

    @Test
    def testCollectionsSingletonMap01(): Unit \ Assert + IO =
        let map: JMap[String, String] = JCollections.singletonMap("k", "v");
        assertEq(expected = 1, map.size())

    @Test
    def testIntegerValueOf01(): Unit \ Assert + IO =
        let list: ArrayList[JInteger] = new ArrayList();
        list.add(JInteger.valueOf(42i32));
        assertEq(expected = 1, list.size())

    @Test
    def testCollectionsUnmodifiableList01(): Unit \ Assert + IO =
        let list: ArrayList[String] = new ArrayList();
        list.add("hello");
        let unmod: JList[String] = JCollections.unmodifiableList(list);
        assertEq(expected = 1, unmod.size())

    @Test
    def testCollectionsSort01(): Unit \ Assert + IO =
        let list: ArrayList[String] = new ArrayList();
        list.add("c");
        list.add("a");
        list.add("b");
        JCollections.sort(list);
        let first = unchecked_cast(list.get(0) as String);
        assertEq(expected = "a", first)

    @Test
    def testCollectionsReverse01(): Unit \ Assert + IO =
        let list: ArrayList[String] = new ArrayList();
        list.add("a");
        list.add("b");
        list.add("c");
        JCollections.reverse(list);
        let first = unchecked_cast(list.get(0) as String);
        assertEq(expected = "c", first)

    @Test
    def testCollectionsFrequency01(): Unit \ Assert + IO =
        let list: ArrayList[String] = new ArrayList();
        list.add("hello");
        list.add("world");
        list.add("hello");
        let obj: Object = checked_cast("hello");
        assertEq(expected = 2, JCollections.frequency(list, obj))

    @Test
    def testCollectionsMin01(): Unit \ Assert + IO =
        let list: ArrayList[String] = new ArrayList();
        list.add("b");
        list.add("a");
        list.add("c");
        let result = unchecked_cast(JCollections.min(list) as String);
        assertEq(expected = "a", result)

    @Test
    def testCollectionsMax01(): Unit \ Assert + IO =
        let list: ArrayList[String] = new ArrayList();
        list.add("b");
        list.add("a");
        list.add("c");
        let result = unchecked_cast(JCollections.max(list) as String);
        assertEq(expected = "c", result)

    @CompileTest
    def testListOf01(): JList[String] \ IO =
        JList.of("a", "b")

    @Test
    def testListOf02(): Unit \ Assert + IO =
        let list: JList[String] = JList.of("a", "b");
        assertEq(expected = 2, list.size())

    @CompileTest
    def testMapOf01(): JMap[String, String] \ IO =
        JMap.of("k", "v")

    @Test
    def testOptionalOrElse01(): Unit \ Assert + IO =
        let opt: Optional[String] = Optional.empty();
        let result = unchecked_cast(opt.orElse("default") as String);
        assertEq(expected = "default", result)

    @Test
    def testArraysAsList01(): Unit \ Assert + IO =
        let list: ArrayList[String] = new ArrayList();
        list.add("a");
        list.add("b");
        list.add("c");
        let unmod: JList[String] = JCollections.unmodifiableList(list);
        assertEq(expected = 3, unmod.size())

}
