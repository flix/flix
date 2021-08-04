namespace Test/Exp/Jvm/GetFieldDoubleNestedClass {

    @test
    def testGetBoolField01(): Bool & Impure =
        import new flix.test.TestClass$_StaticNestedClass$DoubleNestedClass() as newObject;
        import get flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.boolField as getField;
        let o: ##flix.test.TestClass$_StaticNestedClass$DoubleNestedClass = newObject();
        getField(o) == true

    @test
    def testGetCharField01(): Bool & Impure =
        import new flix.test.TestClass$_StaticNestedClass$DoubleNestedClass() as newObject;
        import get flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.charField as getField;
        let o: ##flix.test.TestClass$_StaticNestedClass$DoubleNestedClass = newObject();
        getField(o) == 'A'

    @test
    def testGetFloat32Field01(): Bool & Impure =
        import new flix.test.TestClass$_StaticNestedClass$DoubleNestedClass() as newObject;
        import get flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.float32Field as getField;
        let o: ##flix.test.TestClass$_StaticNestedClass$DoubleNestedClass = newObject();
        getField(o) == 123.456f32

    @test
    def testGetFloat64Field01(): Bool & Impure =
        import new flix.test.TestClass$_StaticNestedClass$DoubleNestedClass() as newObject;
        import get flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.float64Field as getField;
        let o: ##flix.test.TestClass$_StaticNestedClass$DoubleNestedClass = newObject();
        getField(o) == 123.456f64

    @test
    def testGetInt8Field01(): Bool & Impure =
        import new flix.test.TestClass$_StaticNestedClass$DoubleNestedClass() as newObject;
        import get flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.int8Field as getField;
        let o: ##flix.test.TestClass$_StaticNestedClass$DoubleNestedClass = newObject();
        getField(o) == 127i8

    @test
    def testGetInt16Field01(): Bool & Impure =
        import new flix.test.TestClass$_StaticNestedClass$DoubleNestedClass() as newObject;
        import get flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.int16Field as getField;
        let o: ##flix.test.TestClass$_StaticNestedClass$DoubleNestedClass = newObject();
        getField(o) == 127i16

    @test
    def testGetInt32Field01(): Bool & Impure =
        import new flix.test.TestClass$_StaticNestedClass$DoubleNestedClass() as newObject;
        import get flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.int32Field as getField;
        let o: ##flix.test.TestClass$_StaticNestedClass$DoubleNestedClass = newObject();
        getField(o) == 127i32

    @test
    def testGetInt64Field01(): Bool & Impure =
        import new flix.test.TestClass$_StaticNestedClass$DoubleNestedClass() as newObject;
        import get flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.int64Field as getField;
        let o: ##flix.test.TestClass$_StaticNestedClass$DoubleNestedClass = newObject();
        getField(o) == 127i64

    @test
    def testGetStringField01(): Bool & Impure =
        import new flix.test.TestClass$_StaticNestedClass$DoubleNestedClass() as newObject;
        import get flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.stringField as getField;
        let o: ##flix.test.TestClass$_StaticNestedClass$DoubleNestedClass = newObject();
        getField(o) == "Hello World"

}
