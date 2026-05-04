mod Test.Exp.Jvm.Generic.NewObject.MultiParam {

    use Assert.{assertEq, assertTrue}

    import dev.flix.test.TestGenericInterface2
    import dev.flix.test.TestGenericInterface3
    import java.lang.{Integer => JInteger}
    import java.lang.{Long => JLong}
    import java.lang.{Double => JDouble}

    // CompileTests

    @CompileTest
    def testGenericInterface2Compile01(): TestGenericInterface2[String, String] \ IO =
        new TestGenericInterface2[String, String] {
            def apply(_this: TestGenericInterface2[String, String], a: String): String =
                a
        }

    @CompileTest
    def testGenericInterface2Compile02(): TestGenericInterface2[String, JInteger] \ IO =
        new TestGenericInterface2[String, JInteger] {
            def apply(_this: TestGenericInterface2[String, JInteger], a: String): JInteger =
                JInteger.valueOf(a.length())
        }

    @CompileTest
    def testGenericInterface2Compile03(): TestGenericInterface2[JInteger, String] \ IO =
        new TestGenericInterface2[JInteger, String] {
            def apply(_this: TestGenericInterface2[JInteger, String], a: JInteger): String =
                a.toString()
        }

    @CompileTest
    def testGenericInterface3Compile01(): TestGenericInterface3[String, String, String] \ IO =
        new TestGenericInterface3[String, String, String] {
            def combine(_this: TestGenericInterface3[String, String, String], a: String, b: String): String =
                a + b
        }

    @CompileTest
    def testGenericInterface3Compile02(): TestGenericInterface3[String, JInteger, JLong] \ IO =
        new TestGenericInterface3[String, JInteger, JLong] {
            def combine(_this: TestGenericInterface3[String, JInteger, JLong], _a: String, b: JInteger): JLong =
                JLong.valueOf(Int32.toInt64(b.intValue()))
        }

    // Tests

    @Test
    def testGenericInterface2StringString01(): Unit \ {Assert, IO} =
        let f = new TestGenericInterface2[String, String] {
            def apply(_this: TestGenericInterface2[String, String], a: String): String =
                a + "!"
        };
        let r: String = f.apply("hello");
        assertEq(expected = "hello!", r)

    @Test
    def testGenericInterface2StringInteger01(): Unit \ {Assert, IO} =
        let f = new TestGenericInterface2[String, JInteger] {
            def apply(_this: TestGenericInterface2[String, JInteger], a: String): JInteger =
                JInteger.valueOf(a.length())
        };
        let r: JInteger = f.apply("hello");
        assertEq(expected = 5, r.intValue())

    @Test
    def testGenericInterface2IntegerString01(): Unit \ {Assert, IO} =
        let f = new TestGenericInterface2[JInteger, String] {
            def apply(_this: TestGenericInterface2[JInteger, String], a: JInteger): String =
                a.toString()
        };
        let r: String = f.apply(JInteger.valueOf(42i32));
        assertEq(expected = "42", r)

    @Test
    def testGenericInterface2IntegerInteger01(): Unit \ {Assert, IO} =
        let f = new TestGenericInterface2[JInteger, JInteger] {
            def apply(_this: TestGenericInterface2[JInteger, JInteger], a: JInteger): JInteger =
                JInteger.valueOf(a.intValue() * 2)
        };
        let r: JInteger = f.apply(JInteger.valueOf(21i32));
        assertEq(expected = 42, r.intValue())

    @Test
    def testGenericInterface3StringStringString01(): Unit \ {Assert, IO} =
        let f = new TestGenericInterface3[String, String, String] {
            def combine(_this: TestGenericInterface3[String, String, String], a: String, b: String): String =
                a + " " + b
        };
        let r: String = f.combine("hello", "world");
        assertEq(expected = "hello world", r)

    @Test
    def testGenericInterface3StringIntegerLong01(): Unit \ {Assert, IO} =
        let f = new TestGenericInterface3[String, JInteger, JLong] {
            def combine(_this: TestGenericInterface3[String, JInteger, JLong], _a: String, b: JInteger): JLong =
                JLong.valueOf(Int32.toInt64(b.intValue()))
        };
        let r: JLong = f.combine("hello", JInteger.valueOf(10i32));
        assertEq(expected = 10i64, r.longValue())

    @Test
    def testGenericInterface2WithCapture01(): Unit \ {Assert, IO} =
        let prefix = "prefix_";
        let f = new TestGenericInterface2[String, String] {
            def apply(_this: TestGenericInterface2[String, String], a: String): String =
                prefix + a
        };
        let r: String = f.apply("test");
        assertEq(expected = "prefix_test", r)

    @Test
    def testGenericInterface3WithCapture01(): Unit \ {Assert, IO} =
        let separator = "-";
        let f = new TestGenericInterface3[String, String, String] {
            def combine(_this: TestGenericInterface3[String, String, String], a: String, b: String): String =
                a + separator + b
        };
        let r: String = f.combine("hello", "world");
        assertEq(expected = "hello-world", r)

    @Test
    def testGenericInterface2SameType01(): Unit \ {Assert, IO} =
        let f = new TestGenericInterface2[JDouble, JDouble] {
            def apply(_this: TestGenericInterface2[JDouble, JDouble], a: JDouble): JDouble =
                JDouble.valueOf(a.doubleValue() * 2.0f64)
        };
        let r: JDouble = f.apply(JDouble.valueOf(3.14f64));
        assertTrue(r.doubleValue() > 6.27f64)

}
