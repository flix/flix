mod Test.Exp.Jvm.Generic.NewObject.DefaultMethods {

    use Assert.{assertEq}

    import dev.flix.test.TestGenericDefaultMethods
    import java.lang.{Integer => JInteger}

    // CompileTests

    @CompileTest
    def testGenericDefaultMethodsCompile01(): TestGenericDefaultMethods[String] \ IO =
        new TestGenericDefaultMethods[String] {
            def transform(_this: TestGenericDefaultMethods[String], input: String): String =
                input + "!"
        }

    @CompileTest
    def testGenericDefaultMethodsCompile02(): TestGenericDefaultMethods[JInteger] \ IO =
        new TestGenericDefaultMethods[JInteger] {
            def transform(_this: TestGenericDefaultMethods[JInteger], input: JInteger): JInteger =
                JInteger.valueOf(input.intValue() + 1)
        }

    // Tests

    @Test
    def testGenericDefaultTransform01(): Unit \ {Assert, IO} =
        let obj = new TestGenericDefaultMethods[String] {
            def transform(_this: TestGenericDefaultMethods[String], input: String): String =
                input + "!"
        };
        let r: String = obj.transform("hello");
        assertEq(expected = "hello!", r)

    @Test
    def testGenericDefaultTransformTwice01(): Unit \ {Assert, IO} =
        let obj = new TestGenericDefaultMethods[String] {
            def transform(_this: TestGenericDefaultMethods[String], input: String): String =
                input + "!"
        };
        let r: String = obj.transformTwice("hi");
        assertEq(expected = "hi!!", r)

    @Test
    def testGenericDefaultTransformTwiceInteger01(): Unit \ {Assert, IO} =
        let obj = new TestGenericDefaultMethods[JInteger] {
            def transform(_this: TestGenericDefaultMethods[JInteger], input: JInteger): JInteger =
                JInteger.valueOf(input.intValue() + 1)
        };
        let r: JInteger = obj.transformTwice(JInteger.valueOf(0i32));
        assertEq(expected = 2, r.intValue())

    @Test
    def testGenericDefaultAsString01(): Unit \ {Assert, IO} =
        let obj = new TestGenericDefaultMethods[String] {
            def transform(_this: TestGenericDefaultMethods[String], input: String): String =
                input
        };
        let r: String = obj.asString("test");
        assertEq(expected = "test", r)

    @Test
    def testGenericDefaultAsStringInteger01(): Unit \ {Assert, IO} =
        let obj = new TestGenericDefaultMethods[JInteger] {
            def transform(_this: TestGenericDefaultMethods[JInteger], input: JInteger): JInteger =
                input
        };
        let r: String = obj.asString(JInteger.valueOf(42i32));
        assertEq(expected = "42", r)

    @Test
    def testGenericDefaultOverrideDefault01(): Unit \ {Assert, IO} =
        let obj = new TestGenericDefaultMethods[String] {
            def transform(_this: TestGenericDefaultMethods[String], input: String): String =
                input + "!"
            def transformTwice(_this: TestGenericDefaultMethods[String], input: String): String =
                input + "!!"
        };
        let r: String = obj.transformTwice("hi");
        assertEq(expected = "hi!!", r)

    @Test
    def testGenericDefaultWithCapture01(): Unit \ {Assert, IO} =
        let prefix = "> ";
        let obj = new TestGenericDefaultMethods[String] {
            def transform(_this: TestGenericDefaultMethods[String], input: String): String =
                prefix + input
        };
        let r: String = obj.transformTwice("x");
        assertEq(expected = "> > x", r)

}
