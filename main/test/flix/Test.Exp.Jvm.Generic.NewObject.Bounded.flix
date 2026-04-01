mod Test.Exp.Jvm.Generic.NewObject.Bounded {

    use Assert.{assertEq, assertTrue}

    import dev.flix.test.TestBoundedGenericInterface
    import java.lang.{Integer => JInteger}

    // CompileTests

    @CompileTest
    def testBoundedGenericCompile01(): TestBoundedGenericInterface[String] \ IO =
        new TestBoundedGenericInterface[String] {
            def getMin(_this: TestBoundedGenericInterface[String], a: String, b: String): String =
                if (a.compareTo(b) <= 0) a else b
            def getMax(_this: TestBoundedGenericInterface[String], a: String, b: String): String =
                if (a.compareTo(b) >= 0) a else b
        }

    @CompileTest
    def testBoundedGenericCompile02(): TestBoundedGenericInterface[JInteger] \ IO =
        new TestBoundedGenericInterface[JInteger] {
            def getMin(_this: TestBoundedGenericInterface[JInteger], a: JInteger, b: JInteger): JInteger =
                if (a.compareTo(b) <= 0) a else b
            def getMax(_this: TestBoundedGenericInterface[JInteger], a: JInteger, b: JInteger): JInteger =
                if (a.compareTo(b) >= 0) a else b
        }

    // Tests

    @Test
    def testBoundedGenericMin01(): Unit \ {Assert, IO} =
        let obj = new TestBoundedGenericInterface[String] {
            def getMin(_this: TestBoundedGenericInterface[String], a: String, b: String): String =
                if (a.compareTo(b) <= 0) a else b
            def getMax(_this: TestBoundedGenericInterface[String], a: String, b: String): String =
                if (a.compareTo(b) >= 0) a else b
        };
        let r: String = obj.getMin("apple", "banana");
        assertEq(expected = "apple", r)

    @Test
    def testBoundedGenericMax01(): Unit \ {Assert, IO} =
        let obj = new TestBoundedGenericInterface[String] {
            def getMin(_this: TestBoundedGenericInterface[String], a: String, b: String): String =
                if (a.compareTo(b) <= 0) a else b
            def getMax(_this: TestBoundedGenericInterface[String], a: String, b: String): String =
                if (a.compareTo(b) >= 0) a else b
        };
        let r: String = obj.getMax("apple", "banana");
        assertEq(expected = "banana", r)

    @Test
    def testBoundedGenericMinEqual01(): Unit \ {Assert, IO} =
        let obj = new TestBoundedGenericInterface[String] {
            def getMin(_this: TestBoundedGenericInterface[String], a: String, b: String): String =
                if (a.compareTo(b) <= 0) a else b
            def getMax(_this: TestBoundedGenericInterface[String], a: String, b: String): String =
                if (a.compareTo(b) >= 0) a else b
        };
        let r: String = obj.getMin("same", "same");
        assertEq(expected = "same", r)

    @Test
    def testBoundedGenericIntegerMin01(): Unit \ {Assert, IO} =
        let obj = new TestBoundedGenericInterface[JInteger] {
            def getMin(_this: TestBoundedGenericInterface[JInteger], a: JInteger, b: JInteger): JInteger =
                if (a.compareTo(b) <= 0) a else b
            def getMax(_this: TestBoundedGenericInterface[JInteger], a: JInteger, b: JInteger): JInteger =
                if (a.compareTo(b) >= 0) a else b
        };
        let r: JInteger = obj.getMin(JInteger.valueOf(10i32), JInteger.valueOf(20i32));
        assertEq(expected = 10, r.intValue())

    @Test
    def testBoundedGenericIntegerMax01(): Unit \ {Assert, IO} =
        let obj = new TestBoundedGenericInterface[JInteger] {
            def getMin(_this: TestBoundedGenericInterface[JInteger], a: JInteger, b: JInteger): JInteger =
                if (a.compareTo(b) <= 0) a else b
            def getMax(_this: TestBoundedGenericInterface[JInteger], a: JInteger, b: JInteger): JInteger =
                if (a.compareTo(b) >= 0) a else b
        };
        let r: JInteger = obj.getMax(JInteger.valueOf(10i32), JInteger.valueOf(20i32));
        assertEq(expected = 20, r.intValue())

    @Test
    def testBoundedGenericWithCapture01(): Unit \ {Assert, IO} =
        let defaultVal = "fallback";
        let obj = new TestBoundedGenericInterface[String] {
            def getMin(_this: TestBoundedGenericInterface[String], a: String, b: String): String =
                if (a.compareTo(b) <= 0) a else b
            def getMax(_this: TestBoundedGenericInterface[String], a: String, b: String): String =
                if (a.length() == 0 and b.length() == 0) defaultVal
                else if (a.compareTo(b) >= 0) a else b
        };
        let r: String = obj.getMax("", "");
        assertEq(expected = "fallback", r)

}
