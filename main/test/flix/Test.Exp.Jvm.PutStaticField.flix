mod Test.Exp.Jvm.PutStaticField {

    @test
    def testPutStaticBoolField01(): Bool \ IO =
        import static get dev.flix.test.TestClass.BOOL_FIELD: Bool \ IO as getField;
        import static set dev.flix.test.TestClass.BOOL_FIELD: Unit \ IO as setField;
        setField(true);
        getField() == true

    @test
    def testPutStaticBoolField02(): Bool \ IO =
        import static get dev.flix.test.TestClass.BOOL_FIELD: Bool \ IO as getField;
        import static set dev.flix.test.TestClass.BOOL_FIELD: Unit \ IO as setField;
        setField(false);
        getField() == false

    @test
    def testPutStaticCharField01(): Bool \ IO =
        import static get dev.flix.test.TestClass.CHAR_FIELD: Char \ IO as getField;
        import static set dev.flix.test.TestClass.CHAR_FIELD: Unit \ IO as setField;
        setField('a');
        getField() == 'a'

    @test
    def testPutStaticCharField02(): Bool \ IO =
        import static get dev.flix.test.TestClass.CHAR_FIELD: Char \ IO as getField;
        import static set dev.flix.test.TestClass.CHAR_FIELD: Unit \ IO as setField;
        setField('z');
        getField() == 'z'

    @test
    def testPutStaticFloat32Field01(): Bool \ IO =
        import static get dev.flix.test.TestClass.FLOAT32_FIELD: Float32 \ IO as getField;
        import static set dev.flix.test.TestClass.FLOAT32_FIELD: Unit \ IO as setField;
        setField(0.0f32);
        getField() == 0.0f32

    @test
    def testPutStaticFloat32Field02(): Bool \ IO =
        import static get dev.flix.test.TestClass.FLOAT32_FIELD: Float32 \ IO as getField;
        import static set dev.flix.test.TestClass.FLOAT32_FIELD: Unit \ IO as setField;
        setField(21.42f32);
        getField() == 21.42f32

    @test
    def testPutStaticFloat64Field01(): Bool \ IO =
        import static get dev.flix.test.TestClass.FLOAT64_FIELD: Float64 \ IO as getField;
        import static set dev.flix.test.TestClass.FLOAT64_FIELD: Unit \ IO as setField;
        setField(0.0f64);
        getField() == 0.0f64

    @test
    def testPutStaticFloat64Field02(): Bool \ IO =
        import static get dev.flix.test.TestClass.FLOAT64_FIELD: Float64 \ IO as getField;
        import static set dev.flix.test.TestClass.FLOAT64_FIELD: Unit \ IO as setField;
        setField(21.42f64);
        getField() == 21.42f64

    @test
    def testPutStaticInt8Field01(): Bool \ IO =
        import static get dev.flix.test.TestClass.INT8_FIELD: Int8 \ IO as getField;
        import static set dev.flix.test.TestClass.INT8_FIELD: Unit \ IO as setField;
        setField(0i8);
        getField() == 0i8

    @test
    def testPutStaticInt8Field02(): Bool \ IO =
        import static get dev.flix.test.TestClass.INT8_FIELD: Int8 \ IO as getField;
        import static set dev.flix.test.TestClass.INT8_FIELD: Unit \ IO as setField;
        setField(42i8);
        getField() == 42i8

    @test
    def testPutStaticInt16Field01(): Bool \ IO =
        import static get dev.flix.test.TestClass.INT16_FIELD: Int16 \ IO as getField;
        import static set dev.flix.test.TestClass.INT16_FIELD: Unit \ IO as setField;
        setField(0i16);
        getField() == 0i16

    @test
    def testPutStaticInt16Field02(): Bool \ IO =
        import static get dev.flix.test.TestClass.INT16_FIELD: Int16 \ IO as getField;
        import static set dev.flix.test.TestClass.INT16_FIELD: Unit \ IO as setField;
        setField(42i16);
        getField() == 42i16

    @test
    def testPutStaticInt32Field01(): Bool \ IO =
        import static get dev.flix.test.TestClass.INT32_FIELD: Int32 \ IO as getField;
        import static set dev.flix.test.TestClass.INT32_FIELD: Unit \ IO as setField;
        setField(0i32);
        getField() == 0i32

    @test
    def testPutStaticInt32Field02(): Bool \ IO =
        import static get dev.flix.test.TestClass.INT32_FIELD: Int32 \ IO as getField;
        import static set dev.flix.test.TestClass.INT32_FIELD: Unit \ IO as setField;
        setField(42i32);
        getField() == 42i32

    @test
    def testPutStaticInt64Field01(): Bool \ IO =
        import static get dev.flix.test.TestClass.INT64_FIELD: Int64 \ IO as getField;
        import static set dev.flix.test.TestClass.INT64_FIELD: Unit \ IO as setField;
        setField(0i64);
        getField() == 0i64

    @test
    def testPutStaticInt64Field02(): Bool \ IO =
        import static get dev.flix.test.TestClass.INT64_FIELD: Int64 \ IO as getField;
        import static set dev.flix.test.TestClass.INT64_FIELD: Unit \ IO as setField;
        setField(42i64);
        getField() == 42i64

    @test
    def testPutStaticStringField01(): Bool \ IO =
        import static get dev.flix.test.TestClass.STRING_FIELD: String \ IO as getField;
        import static set dev.flix.test.TestClass.STRING_FIELD: Unit \ IO as setField;
        setField("");
        getField() == ""

    @test
    def testPutStaticStringField02(): Bool \ IO =
        import static get dev.flix.test.TestClass.STRING_FIELD: String \ IO as getField;
        import static set dev.flix.test.TestClass.STRING_FIELD: Unit \ IO as setField;
        setField("Goodbye World!");
        getField() == "Goodbye World!"

    @test
    def testPutStaticInheritedField01(): Bool \ IO =
        import static get dev.flix.test.TestClassWithInheritedMethod.staticField: Int32 \ {} as getField;
        import static set dev.flix.test.TestClassWithInheritedMethod.staticField: Unit \ IO as setField;
        setField(456);
        getField() == 456

}
