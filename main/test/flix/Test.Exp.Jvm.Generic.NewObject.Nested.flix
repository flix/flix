mod Test.Exp.Jvm.Generic.NewObject.Nested {

    use Assert.{assertEq, assertTrue}

    import dev.flix.test.TestGenericInterface2
    import java.lang.{Integer => JInteger}
    import java.util.ArrayList
    import java.util.concurrent.Callable

    // CompileTests

    @CompileTest
    def testNestedGenericCallableCompile01(): Callable[ArrayList[String]] \ IO =
        new Callable[ArrayList[String]] {
            def call(_this: Callable[ArrayList[String]]): ArrayList[String] \ IO =
                new ArrayList()
        }

    @CompileTest
    def testNestedGenericCallableCompile02(): Callable[ArrayList[JInteger]] \ IO =
        new Callable[ArrayList[JInteger]] {
            def call(_this: Callable[ArrayList[JInteger]]): ArrayList[JInteger] \ IO =
                new ArrayList()
        }

    // Tests

    @Test
    def testNestedGenericCallable01(): Unit \ {Assert, IO} =
        let c: Callable[ArrayList[String]] = new Callable[ArrayList[String]] {
            def call(_this: Callable[ArrayList[String]]): ArrayList[String] \ IO =
                let l: ArrayList[String] = new ArrayList();
                l.add("hello");
                l
        };
        let r: ArrayList[String] = c.call();
        assertEq(expected = 1, r.size())

    @Test
    def testNestedGenericCallable02(): Unit \ {Assert, IO} =
        let c: Callable[ArrayList[JInteger]] = new Callable[ArrayList[JInteger]] {
            def call(_this: Callable[ArrayList[JInteger]]): ArrayList[JInteger] \ IO =
                let l: ArrayList[JInteger] = new ArrayList();
                l.add(JInteger.valueOf(1i32));
                l.add(JInteger.valueOf(2i32));
                l
        };
        let r: ArrayList[JInteger] = c.call();
        assertEq(expected = 2, r.size())

    @Test
    def testNestedGenericCallable03(): Unit \ {Assert, IO} =
        let c: Callable[ArrayList[String]] = new Callable[ArrayList[String]] {
            def call(_this: Callable[ArrayList[String]]): ArrayList[String] \ IO =
                let l: ArrayList[String] = new ArrayList();
                l.add("a");
                l.add("b");
                l.add("c");
                l
        };
        let r: ArrayList[String] = c.call();
        let first: String = r.get(0);
        assertEq(expected = "a", first);
        assertEq(expected = 3, r.size())

    @Test
    def testNestedGenericInterface201(): Unit \ {Assert, IO} =
        let f = new TestGenericInterface2[ArrayList[String], JInteger] {
            def apply(_this: TestGenericInterface2[ArrayList[String], JInteger], a: ArrayList[String]): JInteger \ IO =
                JInteger.valueOf(a.size())
        };
        let l: ArrayList[String] = new ArrayList();
        l.add("a");
        l.add("b");
        l.add("c");
        let r: JInteger = f.apply(l);
        assertEq(expected = 3, r.intValue())

    @Test
    def testNestedGenericInterface202(): Unit \ {Assert, IO} =
        let f = new TestGenericInterface2[String, ArrayList[String]] {
            def apply(_this: TestGenericInterface2[String, ArrayList[String]], a: String): ArrayList[String] \ IO =
                let l: ArrayList[String] = new ArrayList();
                l.add(a);
                l.add(a);
                l
        };
        let r: ArrayList[String] = f.apply("hi");
        assertEq(expected = 2, r.size())

}
