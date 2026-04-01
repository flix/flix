mod Test.Exp.Jvm.Generic.InvokeStaticMethod.Advanced {

    use Assert.{assertEq, assertTrue}

    import java.lang.{Integer => JInteger}
    import java.util.ArrayList
    import java.util.{Collections => JCollections}
    import java.util.{List => JList}
    import java.util.Optional

    // Tests for advanced generic static method patterns

    @Test
    def testOptionalOfNullable01(): Unit \ {Assert, IO} =
        let o: Optional[String] = Optional.ofNullable("hello");
        assertTrue(o.isPresent())

    @Test
    def testOptionalOfNullableGet01(): Unit \ {Assert, IO} =
        let o: Optional[String] = Optional.of("world");
        let r: String = o.get();
        assertEq(expected = "world", r)

    @Test
    def testOptionalOfInteger01(): Unit \ {Assert, IO} =
        let o: Optional[JInteger] = Optional.of(JInteger.valueOf(42i32));
        let r: JInteger = o.get();
        assertEq(expected = 42, r.intValue())

    @Test
    def testOptionalEmptyInteger01(): Unit \ {Assert, IO} =
        let o: Optional[JInteger] = Optional.empty();
        assertTrue(o.isEmpty())

    @Test
    def testOptionalOrElse02(): Unit \ {Assert, IO} =
        let o: Optional[String] = Optional.empty();
        let r: String = o.orElse("default");
        assertEq(expected = "default", r)

    @Test
    def testOptionalOrElsePresent01(): Unit \ {Assert, IO} =
        let o: Optional[String] = Optional.of("present");
        let r: String = o.orElse("default");
        assertEq(expected = "present", r)

    @Test
    def testCollectionsNcopies01(): Unit \ {Assert, IO} =
        let l: JList[String] = JCollections.nCopies(3, "abc");
        assertEq(expected = 3, l.size());
        let r: String = l.get(0);
        assertEq(expected = "abc", r)

    @Test
    def testCollectionsNcopiesInteger01(): Unit \ {Assert, IO} =
        let l: JList[JInteger] = JCollections.nCopies(5, JInteger.valueOf(7i32));
        assertEq(expected = 5, l.size());
        let r: JInteger = l.get(4);
        assertEq(expected = 7, r.intValue())

    @Test
    def testCollectionsSingletonList02(): Unit \ {Assert, IO} =
        let l: JList[JInteger] = JCollections.singletonList(JInteger.valueOf(99i32));
        assertEq(expected = 1, l.size());
        let r: JInteger = l.get(0);
        assertEq(expected = 99, r.intValue())

    @Test
    def testCollectionsEmptyListIsEmpty01(): Unit \ {Assert, IO} =
        let l: JList[String] = JCollections.emptyList();
        assertTrue(l.isEmpty())

    @Test
    def testCollectionsSwap01(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        l.add("a");
        l.add("b");
        l.add("c");
        JCollections.swap(l, 0, 2);
        let first: String = l.get(0);
        let last: String = l.get(2);
        assertEq(expected = "c", first);
        assertEq(expected = "a", last)

    @Test
    def testCollectionsFill01(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        l.add("x");
        l.add("y");
        l.add("z");
        JCollections.fill(l, "replaced");
        let r: String = l.get(1);
        assertEq(expected = "replaced", r)

}
