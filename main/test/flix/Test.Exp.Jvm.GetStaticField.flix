mod Test.Exp.Jvm.GetStaticField {

    ///
    /// Java Standard Library.
    ///

    @Test
    def testGetStaticField01(): Bool \ IO =
        import static get java.lang.Integer.MIN_VALUE: Int32 \ IO as getMinValue;
        getMinValue() == Int32.minValue()

    @Test
    def testGetStaticField02(): Bool \ IO =
        import static get java.lang.Integer.MAX_VALUE: Int32 \ IO as getMinValue;
        getMinValue() == Int32.maxValue()

    @Test
    def testGetStaticField03(): ##java.nio.file.StandardOpenOption \ IO =
        import static get java.nio.file.StandardOpenOption.APPEND: ##java.nio.file.StandardOpenOption \ IO as getAppendValue;
        getAppendValue()

    @Test
    def testGetStaticField04(): ##java.nio.file.StandardOpenOption \ IO =
        import static get java.nio.file.StandardOpenOption.READ: ##java.nio.file.StandardOpenOption \ IO as getReadValue;
        getReadValue()

    @Test
    def testGetStaticField05(): ##java.io.InputStream \ IO =
        import static get java.lang.System.in: ##java.io.InputStream \ IO as getIn;
        getIn()

    @Test
    def testGetStaticField06(): ##java.io.PrintStream \ IO =
        import static get java.lang.System.out: ##java.io.PrintStream \ IO as getOut;
        getOut()

    @Test
    def testGetStaticField07(): ##java.io.PrintStream \ IO =
        import static get java.lang.System.err: ##java.io.PrintStream \ IO as getErr;
        getErr()

    ///
    /// Tests for each Type.
    ///

    @Test
    def testGetStaticBoolField01(): Bool \ IO =
        import static get dev.flix.test.TestClass.BOOL_FIELD: Bool \ IO as getField;
        getField() == true

    @Test
    def testGetStaticCharField01(): Bool \ IO =
        import static get dev.flix.test.TestClass.CHAR_FIELD: Char \ IO as getField;
        getField() == 'A'

    @Test
    def testGetStaticFloat32Field01(): Bool \ IO =
        import static get dev.flix.test.TestClass.FLOAT32_FIELD: Float32 \ IO as getField;
        getField() == 123.456f32

    @Test
    def testGetStaticFloat64Field01(): Bool \ IO =
        import static get dev.flix.test.TestClass.FLOAT64_FIELD: Float64 \ IO as getField;
        getField() == 123.456f64

    @Test
    def testGetStaticInt8Field01(): Bool \ IO =
        import static get dev.flix.test.TestClass.INT8_FIELD: Int8 \ IO as getField;
        getField() == 127i8

    @Test
    def testGetStaticInt16Field01(): Bool \ IO =
        import static get dev.flix.test.TestClass.INT16_FIELD: Int16 \ IO as getField;
        getField() == 127i16

    @Test
    def testGetStaticInt32Field01(): Bool \ IO =
        import static get dev.flix.test.TestClass.INT32_FIELD: Int32 \ IO as getField;
        getField() == 127i32

    @Test
    def testGetStaticInt64Field01(): Bool \ IO =
        import static get dev.flix.test.TestClass.INT64_FIELD: Int64 \ IO as getField;
        getField() == 127i64

    @Test
    def testGetStaticStringField01(): Bool \ IO =
        import static get dev.flix.test.TestClass.STRING_FIELD: String \ IO as getField;
        getField() == "Hello World"

    @Test
    def testGetStaticPureField01(): Bool =
        import static get dev.flix.test.TestClass.STRING_FIELD: String \ {} as getField;
        getField() == "Hello World"

    @Test
    def testGetStaticObjectField01(): ##java.lang.Object \ IO =
        import static get dev.flix.test.TestClass.STRING_FIELD: ##java.lang.Object \ IO as getField;
        getField()

    @Test
    def testGetStaticInheritedField01(): Bool =
        import static get dev.flix.test.TestClassWithInheritedMethod.staticField: Int32 \ {} as getField;
        getField() == 123

}
