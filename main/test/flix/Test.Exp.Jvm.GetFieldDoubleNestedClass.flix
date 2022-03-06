namespace Test/Exp/Jvm/GetFieldDoubleNestedClass {

    @test
    def testGetBoolField01(): Bool & Impure =
        import new flix.test.TestClass$_StaticNestedClass$DoubleNestedClass(): ##flix.test.TestClass$_StaticNestedClass$DoubleNestedClass & Impure as newObject;
        import get flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.boolField: Bool & Impure as getField;
        let o: ##flix.test.TestClass$_StaticNestedClass$DoubleNestedClass = newObject();
        getField(o) == true

    @test
    def testGetCharField01(): Bool & Impure =
        import new flix.test.TestClass$_StaticNestedClass$DoubleNestedClass(): ##flix.test.TestClass$_StaticNestedClass$DoubleNestedClass & Impure as newObject;
        import get flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.charField: Char & Impure as getField;
        let o: ##flix.test.TestClass$_StaticNestedClass$DoubleNestedClass = newObject();
        getField(o) == 'A'

    @test
    def testGetFloat32Field01(): Bool & Impure =
        import new flix.test.TestClass$_StaticNestedClass$DoubleNestedClass(): ##flix.test.TestClass$_StaticNestedClass$DoubleNestedClass & Impure as newObject;
        import get flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.float32Field: Float32 & Impure as getField;
        let o: ##flix.test.TestClass$_StaticNestedClass$DoubleNestedClass = newObject();
        getField(o) == 123.456f32

    @test
    def testGetFloat64Field01(): Bool & Impure =
        import new flix.test.TestClass$_StaticNestedClass$DoubleNestedClass(): ##flix.test.TestClass$_StaticNestedClass$DoubleNestedClass & Impure as newObject;
        import get flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.float64Field: Float64 & Impure as getField;
        let o: ##flix.test.TestClass$_StaticNestedClass$DoubleNestedClass = newObject();
        getField(o) == 123.456f64

    @test
    def testGetInt8Field01(): Bool & Impure =
        import new flix.test.TestClass$_StaticNestedClass$DoubleNestedClass(): ##flix.test.TestClass$_StaticNestedClass$DoubleNestedClass & Impure as newObject;
        import get flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.int8Field: Int8 & Impure as getField;
        let o: ##flix.test.TestClass$_StaticNestedClass$DoubleNestedClass = newObject();
        getField(o) == 127i8

    @test
    def testGetInt16Field01(): Bool & Impure =
        import new flix.test.TestClass$_StaticNestedClass$DoubleNestedClass(): ##flix.test.TestClass$_StaticNestedClass$DoubleNestedClass & Impure as newObject;
        import get flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.int16Field: Int16 & Impure as getField;
        let o: ##flix.test.TestClass$_StaticNestedClass$DoubleNestedClass = newObject();
        getField(o) == 127i16

    @test
    def testGetInt32Field01(): Bool & Impure =
        import new flix.test.TestClass$_StaticNestedClass$DoubleNestedClass(): ##flix.test.TestClass$_StaticNestedClass$DoubleNestedClass & Impure as newObject;
        import get flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.int32Field: Int32 & Impure as getField;
        let o: ##flix.test.TestClass$_StaticNestedClass$DoubleNestedClass = newObject();
        getField(o) == 127i32

    @test
    def testGetInt64Field01(): Bool & Impure =
        import new flix.test.TestClass$_StaticNestedClass$DoubleNestedClass(): ##flix.test.TestClass$_StaticNestedClass$DoubleNestedClass & Impure as newObject;
        import get flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.int64Field: Int64 & Impure as getField;
        let o: ##flix.test.TestClass$_StaticNestedClass$DoubleNestedClass = newObject();
        getField(o) == 127i64

    @test
    def testGetStringField01(): Bool & Impure =
        import new flix.test.TestClass$_StaticNestedClass$DoubleNestedClass(): ##flix.test.TestClass$_StaticNestedClass$DoubleNestedClass & Impure as newObject;
        import get flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.stringField: String & Impure as getField;
        let o: ##flix.test.TestClass$_StaticNestedClass$DoubleNestedClass = newObject();
        getField(o) == "Hello World"

    @test
    def testGetPureField01(): Bool =
        import new flix.test.TestClass$_StaticNestedClass$DoubleNestedClass(): _ & Pure as newObject;
        import get flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.stringField: String & Pure as getField;
        let o: ##flix.test.TestClass$_StaticNestedClass$DoubleNestedClass = newObject();
        getField(o) == "Hello World"

    @test
    def testGetObjectField01(): ##java.lang.Object & Impure =
        import new flix.test.TestClass$_StaticNestedClass$DoubleNestedClass(): ##flix.test.TestClass$_StaticNestedClass$DoubleNestedClass & Impure as newObject;
        import get flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.stringField: ##java.lang.Object & Impure as getField;
        let o: ##flix.test.TestClass$_StaticNestedClass$DoubleNestedClass = newObject();
        getField(o)
}
