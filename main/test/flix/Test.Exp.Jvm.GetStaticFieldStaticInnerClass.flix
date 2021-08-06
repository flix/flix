namespace Test/Exp/Jvm/GetStaticFieldStaticInnerClass {

    @test
    def testGetStaticBoolField01(): Bool & Impure =
        import get flix.test.TestClass$_StaticNestedClass:BOOL_FIELD as getField;
        getField() == true

    @test
    def testGetStaticCharField01(): Bool & Impure =
        import get flix.test.TestClass$_StaticNestedClass:CHAR_FIELD as getField;
        getField() == 'A'

    @test
    def testGetStaticFloat32Field01(): Bool & Impure =
        import get flix.test.TestClass$_StaticNestedClass:FLOAT32_FIELD as getField;
        getField() == 123.456f32

    @test
    def testGetStaticFloat64Field01(): Bool & Impure =
        import get flix.test.TestClass$_StaticNestedClass:FLOAT64_FIELD as getField;
        getField() == 123.456f64

    @test
    def testGetStaticInt8Field01(): Bool & Impure =
        import get flix.test.TestClass$_StaticNestedClass:INT8_FIELD as getField;
        getField() == 127i8

    @test
    def testGetStaticInt16Field01(): Bool & Impure =
        import get flix.test.TestClass$_StaticNestedClass:INT16_FIELD as getField;
        getField() == 127i16

    @test
    def testGetStaticInt32Field01(): Bool & Impure =
        import get flix.test.TestClass$_StaticNestedClass:INT32_FIELD as getField;
        getField() == 127i32

    @test
    def testGetStaticInt64Field01(): Bool & Impure =
        import get flix.test.TestClass$_StaticNestedClass:INT64_FIELD as getField;
        getField() == 127i64

    @test
    def testGetStaticStringField01(): Bool & Impure =
        import get flix.test.TestClass$_StaticNestedClass:STRING_FIELD as getField;
        getField() == "Hello World"

}
