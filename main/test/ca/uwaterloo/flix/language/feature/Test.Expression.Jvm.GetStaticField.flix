namespace Test/Expression/Jvm/GetStaticField {

    ///
    /// Java Standard Library.
    ///

    @test
    def testGetStaticField01(): Bool =
        import get java.lang.Integer:MIN_VALUE as getMinValue;
        getMinValue() == Int32.minValue()

    @test
    def testGetStaticField02(): Bool =
        import get java.lang.Integer:MAX_VALUE as getMinValue;
        getMinValue() == Int32.maxValue()

    @test
    def testGetStaticField03(): ##java.nio.file.StandardOpenOption =
        import get java.nio.file.StandardOpenOption:APPEND as getAppendValue;
        getAppendValue()

    @test
    def testGetStaticField04(): ##java.nio.file.StandardOpenOption =
        import get java.nio.file.StandardOpenOption:READ as getReadValue;
        getReadValue()

    ///
    /// Tests for each Type.
    ///

    @test
    def testGetStaticBoolField01(): Bool =
        import get flix.test.TestClass:BOOL_FIELD as getField;
        getField() == true

    @test
    def testGetStaticCharField01(): Bool =
        import get flix.test.TestClass:CHAR_FIELD as getField;
        getField() == 'A'

    @test
    def testGetStaticFloat32Field01(): Bool =
        import get flix.test.TestClass:FLOAT32_FIELD as getField;
        getField() == 123.456f32

    @test
    def testGetStaticFloat64Field01(): Bool =
        import get flix.test.TestClass:FLOAT64_FIELD as getField;
        getField() == 123.456f64

    @test
    def testGetStaticInt8Field01(): Bool =
        import get flix.test.TestClass:INT8_FIELD as getField;
        getField() == 127i8

    @test
    def testGetStaticInt16Field01(): Bool =
        import get flix.test.TestClass:INT16_FIELD as getField;
        getField() == 127i16


    @test
    def testGetStaticInt32Field01(): Bool =
        import get flix.test.TestClass:INT32_FIELD as getField;
        getField() == 127i32

    @test
    def testGetStaticInt64Field01(): Bool =
        import get flix.test.TestClass:INT64_FIELD as getField;
        getField() == 127i64

    @test
    def testGetStaticStringField01(): Bool =
        import get flix.test.TestClass:STRING_FIELD as getField;
        getField() == "Hello World"

}