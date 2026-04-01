mod Test.Exp.Jvm.Generic.NewObject.Super {

    use Assert.{assertEq, assertTrue}

    import dev.flix.test.TestGenericAbstractClass
    import java.lang.{Integer => JInteger}

    // Tests for calling super methods on generic abstract classes

    @Test
    def testGenericSuperGetValue01(): Unit \ {Assert, IO} =
        let obj = new TestGenericAbstractClass[String] {
            def new(): TestGenericAbstractClass[String] \ IO = super("original")
            def transform(_this: TestGenericAbstractClass[String], input: String): String \ IO =
                let v: String = super.getValue();
                v + "+" + input
        };
        let r: String = obj.transform("added");
        assertEq(expected = "original+added", r)

    @Test
    def testGenericSuperGetValue02(): Unit \ {Assert, IO} =
        let obj = new TestGenericAbstractClass[JInteger] {
            def new(): TestGenericAbstractClass[JInteger] \ IO = super(JInteger.valueOf(10i32))
            def transform(_this: TestGenericAbstractClass[JInteger], input: JInteger): JInteger \ IO =
                let v: JInteger = super.getValue();
                JInteger.valueOf(v.intValue() + input.intValue())
        };
        let r: JInteger = obj.transform(JInteger.valueOf(32i32));
        assertEq(expected = 42, r.intValue())

    @Test
    def testGenericSuperDescribe01(): Unit \ {Assert, IO} =
        let obj = new TestGenericAbstractClass[String] {
            def new(): TestGenericAbstractClass[String] \ IO = super("test")
            def transform(_this: TestGenericAbstractClass[String], input: String): String =
                input
            def describe(_this: TestGenericAbstractClass[String]): String \ IO =
                let orig: String = super.describe();
                "overridden(" + orig + ")"
        };
        let r: String = obj.describe();
        assertEq(expected = "overridden(value=test)", r)

    @Test
    def testGenericSuperWithCapture01(): Unit \ {Assert, IO} =
        let bonus = 100;
        let obj = new TestGenericAbstractClass[JInteger] {
            def new(): TestGenericAbstractClass[JInteger] \ IO = super(JInteger.valueOf(5i32))
            def transform(_this: TestGenericAbstractClass[JInteger], input: JInteger): JInteger \ IO =
                let v: JInteger = super.getValue();
                JInteger.valueOf(v.intValue() + input.intValue() + bonus)
        };
        let r: JInteger = obj.transform(JInteger.valueOf(1i32));
        assertEq(expected = 106, r.intValue())

}
