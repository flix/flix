mod Test.Exp.Jvm.Generic.NewObject.Capture {

    use Assert.{assertEq, assertTrue}

    import dev.flix.test.TestGenericInterface
    import dev.flix.test.TestGenericInterface2
    import java.lang.{Integer => JInteger}
    import java.util.ArrayList
    import java.util.Comparator
    import java.util.concurrent.Callable

    // Tests for capturing generic-typed variables in NewObject closures

    @Test
    def testCaptureArrayList01(): Unit \ {Assert, IO} =
        let list: ArrayList[String] = new ArrayList();
        list.add("a");
        list.add("b");
        let c: Callable[JInteger] = new Callable[JInteger] {
            def call(_this: Callable[JInteger]): JInteger \ IO =
                JInteger.valueOf(list.size())
        };
        let r: JInteger = c.call();
        assertEq(expected = 2, r.intValue())

    @Test
    def testCaptureArrayListModify01(): Unit \ {Assert, IO} =
        let list: ArrayList[String] = new ArrayList();
        let c: Callable[JInteger] = new Callable[JInteger] {
            def call(_this: Callable[JInteger]): JInteger \ IO =
                list.add("added");
                JInteger.valueOf(list.size())
        };
        let r: JInteger = c.call();
        assertEq(expected = 1, r.intValue());
        assertEq(expected = 1, list.size())

    @Test
    def testCaptureMultipleGenericValues01(): Unit \ {Assert, IO} =
        let l1: ArrayList[String] = new ArrayList();
        let l2: ArrayList[JInteger] = new ArrayList();
        l1.add("hello");
        l2.add(JInteger.valueOf(42i32));
        let c: Callable[String] = new Callable[String] {
            def call(_this: Callable[String]): String \ IO =
                let s: String = l1.get(0);
                let i: JInteger = l2.get(0);
                s + "=" + i.toString()
        };
        let r: String = c.call();
        assertEq(expected = "hello=42", r)

    @Test
    def testCaptureInComparator01(): Unit \ {Assert, IO} =
        let offset = 10;
        let c = new Comparator[JInteger] {
            def compare(_this: Comparator[JInteger], t: JInteger, u: JInteger): Int32 =
                (t.intValue() + offset) - (u.intValue() + offset)
        };
        assertTrue(c.compare(JInteger.valueOf(1i32), JInteger.valueOf(2i32)) < 0)

    @Test
    def testCaptureInGenericInterface01(): Unit \ {Assert, IO} =
        let suffix = "!";
        let obj = new TestGenericInterface[String] {
            def testMethod(_this: TestGenericInterface[String], x: String): String =
                x + suffix
        };
        let r: String = obj.testMethod("hello");
        assertEq(expected = "hello!", r)

    @Test
    def testCaptureInGenericInterface201(): Unit \ {Assert, IO} =
        let multiplier = 3;
        let f = new TestGenericInterface2[JInteger, JInteger] {
            def apply(_this: TestGenericInterface2[JInteger, JInteger], a: JInteger): JInteger =
                JInteger.valueOf(a.intValue() * multiplier)
        };
        let r: JInteger = f.apply(JInteger.valueOf(7i32));
        assertEq(expected = 21, r.intValue())

}
