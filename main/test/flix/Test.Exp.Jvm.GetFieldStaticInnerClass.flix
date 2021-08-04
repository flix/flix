namespace Test/Exp/Jvm/GetFieldStaticInnerClass {

    @test
    def testGetBoolField01(): Bool & Impure =
        import new flix.test.TestClass$_StaticNestedClass() as newObject;
        import get flix.test.TestClass$_StaticNestedClass.boolField as getField;
        let o: ##flix.test.TestClass$_StaticNestedClass = newObject();
        getField(o) == true

    @test
    def testGetCharField01(): Bool & Impure =
        import new flix.test.TestClass$_StaticNestedClass() as newObject;
        import get flix.test.TestClass$_StaticNestedClass.charField as getField;
        let o: ##flix.test.TestClass$_StaticNestedClass = newObject();
        getField(o) == 'A'

    @test
    def testGetFloat32Field01(): Bool & Impure =
        import new flix.test.TestClass$_StaticNestedClass() as newObject;
        import get flix.test.TestClass$_StaticNestedClass.float32Field as getField;
        let o: ##flix.test.TestClass$_StaticNestedClass = newObject();
        getField(o) == 123.456f32

    @test
    def testGetFloat64Field01(): Bool & Impure =
        import new flix.test.TestClass$_StaticNestedClass() as newObject;
        import get flix.test.TestClass$_StaticNestedClass.float64Field as getField;
        let o: ##flix.test.TestClass$_StaticNestedClass = newObject();
        getField(o) == 123.456f64

    @test
    def testGetInt8Field01(): Bool & Impure =
        import new flix.test.TestClass$_StaticNestedClass() as newObject;
        import get flix.test.TestClass$_StaticNestedClass.int8Field as getField;
        let o: ##flix.test.TestClass$_StaticNestedClass = newObject();
        getField(o) == 127i8

    @test
    def testGetInt16Field01(): Bool & Impure =
        import new flix.test.TestClass$_StaticNestedClass() as newObject;
        import get flix.test.TestClass$_StaticNestedClass.int16Field as getField;
        let o: ##flix.test.TestClass$_StaticNestedClass = newObject();
        getField(o) == 127i16

    @test
    def testGetInt32Field01(): Bool & Impure =
        import new flix.test.TestClass$_StaticNestedClass() as newObject;
        import get flix.test.TestClass$_StaticNestedClass.int32Field as getField;
        let o: ##flix.test.TestClass$_StaticNestedClass = newObject();
        getField(o) == 127i32

    @test
    def testGetInt64Field01(): Bool & Impure =
        import new flix.test.TestClass$_StaticNestedClass() as newObject;
        import get flix.test.TestClass$_StaticNestedClass.int64Field as getField;
        let o: ##flix.test.TestClass$_StaticNestedClass = newObject();
        getField(o) == 127i64

    @test
    def testGetStringField01(): Bool & Impure =
        import new flix.test.TestClass$_StaticNestedClass() as newObject;
        import get flix.test.TestClass$_StaticNestedClass.stringField as getField;
        let o: ##flix.test.TestClass$_StaticNestedClass = newObject();
        getField(o) == "Hello World"

}
