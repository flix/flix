mod Test.Exp.Jvm.Generic.NewObject.Inherited {

    use Assert.{assertEq, assertTrue}

    import dev.flix.test.TestGenericChildInterface
    import dev.flix.test.TestGenericSubInterface
    import java.lang.{Comparable => JComparable}
    import java.lang.{Integer => JInteger}

    // CompileTests

    @CompileTest
    def testGenericChildInterfaceCompile01(): TestGenericChildInterface[String] \ IO =
        new TestGenericChildInterface[String] {
            def testMethod(_this: TestGenericChildInterface[String], x: String): String =
                x + x
            def describe(_this: TestGenericChildInterface[String]): String =
                "child"
        }

    @CompileTest
    def testGenericSubInterfaceCompile01(): TestGenericSubInterface[String] \ IO =
        new TestGenericSubInterface[String] {
            def getValue(_this: TestGenericSubInterface[String]): String =
                "hello"
            def compareTo(_this: TestGenericSubInterface[String], o: String): Int32 =
                "hello".compareTo(o)
        }

    // Tests

    @Test
    def testGenericChildInterfaceInherited01(): Unit \ {Assert, IO} =
        let obj = new TestGenericChildInterface[String] {
            def testMethod(_this: TestGenericChildInterface[String], x: String): String =
                x + ", " + x
            def describe(_this: TestGenericChildInterface[String]): String =
                "string-child"
        };
        let r: String = obj.testMethod("hi");
        assertEq(expected = "hi, hi", r)

    @Test
    def testGenericChildInterfaceDescribe01(): Unit \ {Assert, IO} =
        let obj = new TestGenericChildInterface[String] {
            def testMethod(_this: TestGenericChildInterface[String], x: String): String =
                x
            def describe(_this: TestGenericChildInterface[String]): String =
                "my-description"
        };
        let r: String = obj.describe();
        assertEq(expected = "my-description", r)

    @Test
    def testGenericChildInterfaceInteger01(): Unit \ {Assert, IO} =
        let obj = new TestGenericChildInterface[JInteger] {
            def testMethod(_this: TestGenericChildInterface[JInteger], x: JInteger): JInteger =
                JInteger.valueOf(x.intValue() * 2)
            def describe(_this: TestGenericChildInterface[JInteger]): String =
                "integer-child"
        };
        let r: JInteger = obj.testMethod(JInteger.valueOf(21i32));
        assertEq(expected = 42, r.intValue())

    @Test
    def testGenericChildInterfaceWithCapture01(): Unit \ {Assert, IO} =
        let suffix = "!";
        let obj = new TestGenericChildInterface[String] {
            def testMethod(_this: TestGenericChildInterface[String], x: String): String =
                x + suffix
            def describe(_this: TestGenericChildInterface[String]): String =
                "captured" + suffix
        };
        let r1: String = obj.testMethod("hello");
        let r2: String = obj.describe();
        assertEq(expected = "hello!", r1);
        assertEq(expected = "captured!", r2)

    @Test
    def testGenericSubInterfaceGetValue01(): Unit \ {Assert, IO} =
        let obj = new TestGenericSubInterface[String] {
            def getValue(_this: TestGenericSubInterface[String]): String =
                "stored"
            def compareTo(_this: TestGenericSubInterface[String], o: String): Int32 =
                "stored".compareTo(o)
        };
        let r: String = obj.getValue();
        assertEq(expected = "stored", r)

    @Test
    def testGenericSubInterfaceCompareTo01(): Unit \ {Assert, IO} =
        let obj = new TestGenericSubInterface[String] {
            def getValue(_this: TestGenericSubInterface[String]): String =
                "hello"
            def compareTo(_this: TestGenericSubInterface[String], o: String): Int32 =
                "hello".compareTo(o)
        };
        assertTrue(obj.compareTo("aaa") > 0)

    @Test
    def testGenericSubInterfaceCompareTo02(): Unit \ {Assert, IO} =
        let obj = new TestGenericSubInterface[String] {
            def getValue(_this: TestGenericSubInterface[String]): String =
                "hello"
            def compareTo(_this: TestGenericSubInterface[String], o: String): Int32 =
                "hello".compareTo(o)
        };
        assertEq(expected = 0, obj.compareTo("hello"))

}
