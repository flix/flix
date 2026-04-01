mod Test.Exp.Jvm.Generic.NewObject.AbstractClass {

    use Assert.{assertEq, assertTrue}

    import dev.flix.test.TestGenericAbstractClass
    import dev.flix.test.TestGenericAbstractClass2
    import java.lang.{Integer => JInteger}

    // CompileTests

    @CompileTest
    def testGenericAbstractClassCompile01(): TestGenericAbstractClass[String] \ IO =
        new TestGenericAbstractClass[String] {
            def new(): TestGenericAbstractClass[String] \ IO = super("hello")
            def transform(_this: TestGenericAbstractClass[String], input: String): String =
                input
        }

    @CompileTest
    def testGenericAbstractClassCompile02(): TestGenericAbstractClass[JInteger] \ IO =
        new TestGenericAbstractClass[JInteger] {
            def new(): TestGenericAbstractClass[JInteger] \ IO = super(JInteger.valueOf(0i32))
            def transform(_this: TestGenericAbstractClass[JInteger], input: JInteger): JInteger =
                input
        }

    @CompileTest
    def testGenericAbstractClass2Compile01(): TestGenericAbstractClass2[String, JInteger] \ IO =
        new TestGenericAbstractClass2[String, JInteger] {
            def new(): TestGenericAbstractClass2[String, JInteger] \ IO = super("key", JInteger.valueOf(0i32))
            def process(_this: TestGenericAbstractClass2[String, JInteger], key: String): JInteger =
                JInteger.valueOf(key.length())
        }

    // Tests

    @Test
    def testGenericAbstractClassTransform01(): Unit \ {Assert, IO} =
        let obj = new TestGenericAbstractClass[String] {
            def new(): TestGenericAbstractClass[String] \ IO = super("hello")
            def transform(_this: TestGenericAbstractClass[String], input: String): String =
                input + "!"
        };
        let r: String = obj.transform("world");
        assertEq(expected = "world!", r)

    @Test
    def testGenericAbstractClassTransform02(): Unit \ {Assert, IO} =
        let obj = new TestGenericAbstractClass[JInteger] {
            def new(): TestGenericAbstractClass[JInteger] \ IO = super(JInteger.valueOf(10i32))
            def transform(_this: TestGenericAbstractClass[JInteger], input: JInteger): JInteger =
                JInteger.valueOf(input.intValue() + 1)
        };
        let r: JInteger = obj.transform(JInteger.valueOf(41i32));
        assertEq(expected = 42, r.intValue())

    @Test
    def testGenericAbstractClassGetValue01(): Unit \ {Assert, IO} =
        let obj = new TestGenericAbstractClass[String] {
            def new(): TestGenericAbstractClass[String] \ IO = super("stored")
            def transform(_this: TestGenericAbstractClass[String], input: String): String =
                input
        };
        let r: String = obj.getValue();
        assertEq(expected = "stored", r)

    @Test
    def testGenericAbstractClassGetValueInteger01(): Unit \ {Assert, IO} =
        let obj = new TestGenericAbstractClass[JInteger] {
            def new(): TestGenericAbstractClass[JInteger] \ IO = super(JInteger.valueOf(99i32))
            def transform(_this: TestGenericAbstractClass[JInteger], input: JInteger): JInteger =
                input
        };
        let r: JInteger = obj.getValue();
        assertEq(expected = 99, r.intValue())

    @Test
    def testGenericAbstractClassDescribe01(): Unit \ {Assert, IO} =
        let obj = new TestGenericAbstractClass[String] {
            def new(): TestGenericAbstractClass[String] \ IO = super("test")
            def transform(_this: TestGenericAbstractClass[String], input: String): String =
                input
        };
        let r: String = obj.describe();
        assertEq(expected = "value=test", r)

    @Test
    def testGenericAbstractClassWithCapture01(): Unit \ {Assert, IO} =
        let suffix = "!";
        let obj = new TestGenericAbstractClass[String] {
            def new(): TestGenericAbstractClass[String] \ IO = super("base")
            def transform(_this: TestGenericAbstractClass[String], input: String): String =
                input + suffix
        };
        let r: String = obj.transform("captured");
        assertEq(expected = "captured!", r)

    @Test
    def testGenericAbstractClass2Process01(): Unit \ {Assert, IO} =
        let obj = new TestGenericAbstractClass2[String, JInteger] {
            def new(): TestGenericAbstractClass2[String, JInteger] \ IO = super("key", JInteger.valueOf(0i32))
            def process(_this: TestGenericAbstractClass2[String, JInteger], key: String): JInteger =
                JInteger.valueOf(key.length())
        };
        let r: JInteger = obj.process("hello");
        assertEq(expected = 5, r.intValue())

    @Test
    def testGenericAbstractClass2GetKey01(): Unit \ {Assert, IO} =
        let obj = new TestGenericAbstractClass2[String, JInteger] {
            def new(): TestGenericAbstractClass2[String, JInteger] \ IO = super("mykey", JInteger.valueOf(42i32))
            def process(_this: TestGenericAbstractClass2[String, JInteger], key: String): JInteger =
                JInteger.valueOf(key.length())
        };
        let r: String = obj.getKey();
        assertEq(expected = "mykey", r)

    @Test
    def testGenericAbstractClass2GetVal01(): Unit \ {Assert, IO} =
        let obj = new TestGenericAbstractClass2[String, JInteger] {
            def new(): TestGenericAbstractClass2[String, JInteger] \ IO = super("mykey", JInteger.valueOf(42i32))
            def process(_this: TestGenericAbstractClass2[String, JInteger], key: String): JInteger =
                JInteger.valueOf(key.length())
        };
        let r: JInteger = obj.getVal();
        assertEq(expected = 42, r.intValue())

    @Test
    def testGenericAbstractClass2WithCapture01(): Unit \ {Assert, IO} =
        let offset = 100;
        let obj = new TestGenericAbstractClass2[String, JInteger] {
            def new(): TestGenericAbstractClass2[String, JInteger] \ IO = super("k", JInteger.valueOf(0i32))
            def process(_this: TestGenericAbstractClass2[String, JInteger], key: String): JInteger =
                JInteger.valueOf(key.length() + offset)
        };
        let r: JInteger = obj.process("abc");
        assertEq(expected = 103, r.intValue())

}
