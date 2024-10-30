mod Test.Exp.Jvm.GetStaticField {

    import java.io.InputStream
    import java.io.PrintStream
    import java.lang.Object
    import java.nio.file.StandardOpenOption

    ///
    /// Java Standard Library.
    ///

    @test
    def testGetStaticField01(): Bool \ IO =
        import static java_get_field java.lang.Integer.MIN_VALUE: Int32 \ IO as getMinValue;
        getMinValue() == Int32.minValue()

    @test
    def testGetStaticField02(): Bool \ IO =
        import static java_get_field java.lang.Integer.MAX_VALUE: Int32 \ IO as getMinValue;
        getMinValue() == Int32.maxValue()

    @test
    def testGetStaticField03(): StandardOpenOption \ IO =
        import static java_get_field java.nio.file.StandardOpenOption.APPEND: StandardOpenOption \ IO as getAppendValue;
        getAppendValue()

    @test
    def testGetStaticField04(): StandardOpenOption \ IO =
        import static java_get_field java.nio.file.StandardOpenOption.READ: StandardOpenOption \ IO as getReadValue;
        getReadValue()

    @test
    def testGetStaticField05(): InputStream \ IO =
        import static java_get_field java.lang.System.in: InputStream \ IO as getIn;
        getIn()

    @test
    def testGetStaticField06(): PrintStream \ IO =
        import static java_get_field java.lang.System.out: PrintStream \ IO as getOut;
        getOut()

    @test
    def testGetStaticField07(): PrintStream \ IO =
        import static java_get_field java.lang.System.err: PrintStream \ IO as getErr;
        getErr()

    ///
    /// Tests for each Type.
    ///

    @test
    def testGetStaticBoolField01(): Bool \ IO =
        import static java_get_field dev.flix.test.TestClass.BOOL_FIELD: Bool \ IO as getField;
        getField() == true

    @test
    def testGetStaticCharField01(): Bool \ IO =
        import static java_get_field dev.flix.test.TestClass.CHAR_FIELD: Char \ IO as getField;
        getField() == 'A'

    @test
    def testGetStaticFloat32Field01(): Bool \ IO =
        import static java_get_field dev.flix.test.TestClass.FLOAT32_FIELD: Float32 \ IO as getField;
        getField() == 123.456f32

    @test
    def testGetStaticFloat64Field01(): Bool \ IO =
        import static java_get_field dev.flix.test.TestClass.FLOAT64_FIELD: Float64 \ IO as getField;
        getField() == 123.456f64

    @test
    def testGetStaticInt8Field01(): Bool \ IO =
        import static java_get_field dev.flix.test.TestClass.INT8_FIELD: Int8 \ IO as getField;
        getField() == 127i8

    @test
    def testGetStaticInt16Field01(): Bool \ IO =
        import static java_get_field dev.flix.test.TestClass.INT16_FIELD: Int16 \ IO as getField;
        getField() == 127i16

    @test
    def testGetStaticInt32Field01(): Bool \ IO =
        import static java_get_field dev.flix.test.TestClass.INT32_FIELD: Int32 \ IO as getField;
        getField() == 127i32

    @test
    def testGetStaticInt64Field01(): Bool \ IO =
        import static java_get_field dev.flix.test.TestClass.INT64_FIELD: Int64 \ IO as getField;
        getField() == 127i64

    @test
    def testGetStaticStringField01(): Bool \ IO =
        import static java_get_field dev.flix.test.TestClass.STRING_FIELD: String \ IO as getField;
        getField() == "Hello World"

    @test
    def testGetStaticPureField01(): Bool =
        import static java_get_field dev.flix.test.TestClass.STRING_FIELD: String \ {} as getField;
        getField() == "Hello World"

    @test
    def testGetStaticObjectField01(): Object \ IO =
        import static java_get_field dev.flix.test.TestClass.STRING_FIELD: Object \ IO as getField;
        getField()

    @test
    def testGetStaticInheritedField01(): Bool =
        import static java_get_field dev.flix.test.TestClassWithInheritedMethod.staticField: Int32 \ {} as getField;
        getField() == 123

}
