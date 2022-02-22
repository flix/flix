namespace Test/Exp/Jvm/GetStaticField {

    ///
    /// Java Standard Library.
    ///

    @test
    def testGetStaticField01(): Bool & Impure =
        import static get java.lang.Integer.MIN_VALUE: Int32 & Impure as getMinValue;
        getMinValue() == Int32.minValue()

    @test
    def testGetStaticField02(): Bool & Impure =
        import static get java.lang.Integer.MAX_VALUE: Int32 & Impure as getMinValue;
        getMinValue() == Int32.maxValue()

    @test
    def testGetStaticField03(): ##java.nio.file.StandardOpenOption & Impure =
        import static get java.nio.file.StandardOpenOption.APPEND: ##java.nio.file.StandardOpenOption & Impure as getAppendValue;
        getAppendValue()

    @test
    def testGetStaticField04(): ##java.nio.file.StandardOpenOption & Impure =
        import static get java.nio.file.StandardOpenOption.READ: ##java.nio.file.StandardOpenOption & Impure as getReadValue;
        getReadValue()

    @test
    def testGetStaticField05(): ##java.io.InputStream & Impure =
        import static get java.lang.System.in: ##java.io.InputStream & Impure as getIn;
        getIn()

    @test
    def testGetStaticField06(): ##java.io.PrintStream & Impure =
        import static get java.lang.System.out: ##java.io.PrintStream & Impure as getOut;
        getOut()

    @test
    def testGetStaticField07(): ##java.io.PrintStream & Impure =
        import static get java.lang.System.err: ##java.io.PrintStream & Impure as getErr;
        getErr()

    ///
    /// Tests for each Type.
    ///

    @test
    def testGetStaticBoolField01(): Bool & Impure =
        import static get flix.test.TestClass.BOOL_FIELD: Bool & Impure as getField;
        getField() == true

    @test
    def testGetStaticCharField01(): Bool & Impure =
        import static get flix.test.TestClass.CHAR_FIELD: Char & Impure as getField;
        getField() == 'A'

    @test
    def testGetStaticFloat32Field01(): Bool & Impure =
        import static get flix.test.TestClass.FLOAT32_FIELD: Float32 & Impure as getField;
        getField() == 123.456f32

    @test
    def testGetStaticFloat64Field01(): Bool & Impure =
        import static get flix.test.TestClass.FLOAT64_FIELD: Float64 & Impure as getField;
        getField() == 123.456f64

    @test
    def testGetStaticInt8Field01(): Bool & Impure =
        import static get flix.test.TestClass.INT8_FIELD: Int8 & Impure as getField;
        getField() == 127i8

    @test
    def testGetStaticInt16Field01(): Bool & Impure =
        import static get flix.test.TestClass.INT16_FIELD: Int16 & Impure as getField;
        getField() == 127i16

    @test
    def testGetStaticInt32Field01(): Bool & Impure =
        import static get flix.test.TestClass.INT32_FIELD: Int32 & Impure as getField;
        getField() == 127i32

    @test
    def testGetStaticInt64Field01(): Bool & Impure =
        import static get flix.test.TestClass.INT64_FIELD: Int64 & Impure as getField;
        getField() == 127i64

    @test
    def testGetStaticStringField01(): Bool & Impure =
        import static get flix.test.TestClass.STRING_FIELD: String & Impure as getField;
        getField() == "Hello World"

    @test
    def testGetStaticPureField01(): Bool =
        import static get flix.test.TestClass.STRING_FIELD: _ & Pure as getField;
        getField() == "Hello World"

    @test
    def testGetStaticObjectField01(): ##java.lang.Object & Impure =
        import static get flix.test.TestClass.STRING_FIELD: ##java.lang.Object & Impure as getField;
        getField()

}
