mod Test.Exp.Jvm.PutStaticField {

    @Test
    def testPutStaticBoolField01(): Bool \ IO =
        import static get dev.flix.test.TestClass.BOOL_FIELD: Bool \ IO as getField;
        import static set dev.flix.test.TestClass.BOOL_FIELD: Unit \ IO as setField;
        setField(true);
        getField() == true

    @Test
    def testPutStaticBoolField02(): Bool \ IO =
        import static get dev.flix.test.TestClass.BOOL_FIELD: Bool \ IO as getField;
        import static set dev.flix.test.TestClass.BOOL_FIELD: Unit \ IO as setField;
        setField(false);
        getField() == false

    @Test
    def testPutStaticCharField01(): Bool \ IO =
        import static get dev.flix.test.TestClass.CHAR_FIELD: Char \ IO as getField;
        import static set dev.flix.test.TestClass.CHAR_FIELD: Unit \ IO as setField;
        setField('a');
        getField() == 'a'

    @Test
    def testPutStaticCharField02(): Bool \ IO =
        import static get dev.flix.test.TestClass.CHAR_FIELD: Char \ IO as getField;
        import static set dev.flix.test.TestClass.CHAR_FIELD: Unit \ IO as setField;
        setField('z');
        getField() == 'z'

    @Test
    def testPutStaticFloat32Field01(): Bool \ IO =
        import static get dev.flix.test.TestClass.FLOAT32_FIELD: Float32 \ IO as getField;
        import static set dev.flix.test.TestClass.FLOAT32_FIELD: Unit \ IO as setField;
        setField(0.0f32);
        getField() == 0.0f32

    @Test
    def testPutStaticFloat32Field02(): Bool \ IO =
        import static get dev.flix.test.TestClass.FLOAT32_FIELD: Float32 \ IO as getField;
        import static set dev.flix.test.TestClass.FLOAT32_FIELD: Unit \ IO as setField;
        setField(21.42f32);
        getField() == 21.42f32

    @Test
    def testPutStaticFloat64Field01(): Bool \ IO =
        import static get dev.flix.test.TestClass.FLOAT64_FIELD: Float64 \ IO as getField;
        import static set dev.flix.test.TestClass.FLOAT64_FIELD: Unit \ IO as setField;
        setField(0.0f64);
        getField() == 0.0f64

    @Test
    def testPutStaticFloat64Field02(): Bool \ IO =
        import static get dev.flix.test.TestClass.FLOAT64_FIELD: Float64 \ IO as getField;
        import static set dev.flix.test.TestClass.FLOAT64_FIELD: Unit \ IO as setField;
        setField(21.42f64);
        getField() == 21.42f64

    @Test
    def testPutStaticInt8Field01(): Bool \ IO =
        import static get dev.flix.test.TestClass.INT8_FIELD: Int8 \ IO as getField;
        import static set dev.flix.test.TestClass.INT8_FIELD: Unit \ IO as setField;
        setField(0i8);
        getField() == 0i8

    @Test
    def testPutStaticInt8Field02(): Bool \ IO =
        import static get dev.flix.test.TestClass.INT8_FIELD: Int8 \ IO as getField;
        import static set dev.flix.test.TestClass.INT8_FIELD: Unit \ IO as setField;
        setField(42i8);
        getField() == 42i8

    @Test
    def testPutStaticInt16Field01(): Bool \ IO =
        import static get dev.flix.test.TestClass.INT16_FIELD: Int16 \ IO as getField;
        import static set dev.flix.test.TestClass.INT16_FIELD: Unit \ IO as setField;
        setField(0i16);
        getField() == 0i16

    @Test
    def testPutStaticInt16Field02(): Bool \ IO =
        import static get dev.flix.test.TestClass.INT16_FIELD: Int16 \ IO as getField;
        import static set dev.flix.test.TestClass.INT16_FIELD: Unit \ IO as setField;
        setField(42i16);
        getField() == 42i16

    @Test
    def testPutStaticInt32Field01(): Bool \ IO =
        import static get dev.flix.test.TestClass.INT32_FIELD: Int32 \ IO as getField;
        import static set dev.flix.test.TestClass.INT32_FIELD: Unit \ IO as setField;
        setField(0i32);
        getField() == 0i32

    @Test
    def testPutStaticInt32Field02(): Bool \ IO =
        import static get dev.flix.test.TestClass.INT32_FIELD: Int32 \ IO as getField;
        import static set dev.flix.test.TestClass.INT32_FIELD: Unit \ IO as setField;
        setField(42i32);
        getField() == 42i32

    @Test
    def testPutStaticInt64Field01(): Bool \ IO =
        import static get dev.flix.test.TestClass.INT64_FIELD: Int64 \ IO as getField;
        import static set dev.flix.test.TestClass.INT64_FIELD: Unit \ IO as setField;
        setField(0i64);
        getField() == 0i64

    @Test
    def testPutStaticInt64Field02(): Bool \ IO =
        import static get dev.flix.test.TestClass.INT64_FIELD: Int64 \ IO as getField;
        import static set dev.flix.test.TestClass.INT64_FIELD: Unit \ IO as setField;
        setField(42i64);
        getField() == 42i64

    @Test
    def testPutStaticStringField01(): Bool \ IO =
        import static get dev.flix.test.TestClass.STRING_FIELD: String \ IO as getField;
        import static set dev.flix.test.TestClass.STRING_FIELD: Unit \ IO as setField;
        setField("");
        getField() == ""

    @Test
    def testPutStaticStringField02(): Bool \ IO =
        import static get dev.flix.test.TestClass.STRING_FIELD: String \ IO as getField;
        import static set dev.flix.test.TestClass.STRING_FIELD: Unit \ IO as setField;
        setField("Goodbye World!");
        getField() == "Goodbye World!"

    @Test
    def testPutStaticInheritedField01(): Bool \ IO =
        import static get dev.flix.test.TestClassWithInheritedMethod.staticField: Int32 \ {} as getField;
        import static set dev.flix.test.TestClassWithInheritedMethod.staticField: Unit \ IO as setField;
        setField(456);
        getField() == 456

}
