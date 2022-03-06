namespace Test/Exp/Jvm/PutStaticField {

    @test
    def testPutStaticBoolField01(): Bool & Impure =
        import static get flix.test.TestClass.BOOL_FIELD: Bool & Impure as getField;
        import static set flix.test.TestClass.BOOL_FIELD: Unit & Impure as setField;
        setField(true);
        getField() == true

    @test
    def testPutStaticBoolField02(): Bool & Impure =
        import static get flix.test.TestClass.BOOL_FIELD: Bool & Impure as getField;
        import static set flix.test.TestClass.BOOL_FIELD: Unit & Impure as setField;
        setField(false);
        getField() == false

    @test
    def testPutStaticCharField01(): Bool & Impure =
        import static get flix.test.TestClass.CHAR_FIELD: Char & Impure as getField;
        import static set flix.test.TestClass.CHAR_FIELD: Unit & Impure as setField;
        setField('a');
        getField() == 'a'

    @test
    def testPutStaticCharField02(): Bool & Impure =
        import static get flix.test.TestClass.CHAR_FIELD: Char & Impure as getField;
        import static set flix.test.TestClass.CHAR_FIELD: Unit & Impure as setField;
        setField('z');
        getField() == 'z'

    @test
    def testPutStaticFloat32Field01(): Bool & Impure =
        import static get flix.test.TestClass.FLOAT32_FIELD: Float32 & Impure as getField;
        import static set flix.test.TestClass.FLOAT32_FIELD: Unit & Impure as setField;
        setField(0.0f32);
        getField() == 0.0f32

    @test
    def testPutStaticFloat32Field02(): Bool & Impure =
        import static get flix.test.TestClass.FLOAT32_FIELD: Float32 & Impure as getField;
        import static set flix.test.TestClass.FLOAT32_FIELD: Unit & Impure as setField;
        setField(21.42f32);
        getField() == 21.42f32

    @test
    def testPutStaticFloat64Field01(): Bool & Impure =
        import static get flix.test.TestClass.FLOAT64_FIELD: Float64 & Impure as getField;
        import static set flix.test.TestClass.FLOAT64_FIELD: Unit & Impure as setField;
        setField(0.0f64);
        getField() == 0.0f64

    @test
    def testPutStaticFloat64Field02(): Bool & Impure =
        import static get flix.test.TestClass.FLOAT64_FIELD: Float64 & Impure as getField;
        import static set flix.test.TestClass.FLOAT64_FIELD: Unit & Impure as setField;
        setField(21.42f64);
        getField() == 21.42f64

    @test
    def testPutStaticInt8Field01(): Bool & Impure =
        import static get flix.test.TestClass.INT8_FIELD: Int8 & Impure as getField;
        import static set flix.test.TestClass.INT8_FIELD: Unit & Impure as setField;
        setField(0i8);
        getField() == 0i8

    @test
    def testPutStaticInt8Field02(): Bool & Impure =
        import static get flix.test.TestClass.INT8_FIELD: Int8 & Impure as getField;
        import static set flix.test.TestClass.INT8_FIELD: Unit & Impure as setField;
        setField(42i8);
        getField() == 42i8

    @test
    def testPutStaticInt16Field01(): Bool & Impure =
        import static get flix.test.TestClass.INT16_FIELD: Int16 & Impure as getField;
        import static set flix.test.TestClass.INT16_FIELD: Unit & Impure as setField;
        setField(0i16);
        getField() == 0i16

    @test
    def testPutStaticInt16Field02(): Bool & Impure =
        import static get flix.test.TestClass.INT16_FIELD: Int16 & Impure as getField;
        import static set flix.test.TestClass.INT16_FIELD: Unit & Impure as setField;
        setField(42i16);
        getField() == 42i16

    @test
    def testPutStaticInt32Field01(): Bool & Impure =
        import static get flix.test.TestClass.INT32_FIELD: Int32 & Impure as getField;
        import static set flix.test.TestClass.INT32_FIELD: Unit & Impure as setField;
        setField(0i32);
        getField() == 0i32

    @test
    def testPutStaticInt32Field02(): Bool & Impure =
        import static get flix.test.TestClass.INT32_FIELD: Int32 & Impure as getField;
        import static set flix.test.TestClass.INT32_FIELD: Unit & Impure as setField;
        setField(42i32);
        getField() == 42i32

    @test
    def testPutStaticInt64Field01(): Bool & Impure =
        import static get flix.test.TestClass.INT64_FIELD: Int64 & Impure as getField;
        import static set flix.test.TestClass.INT64_FIELD: Unit & Impure as setField;
        setField(0i64);
        getField() == 0i64

    @test
    def testPutStaticInt64Field02(): Bool & Impure =
        import static get flix.test.TestClass.INT64_FIELD: Int64 & Impure as getField;
        import static set flix.test.TestClass.INT64_FIELD: Unit & Impure as setField;
        setField(42i64);
        getField() == 42i64

    @test
    def testPutStaticStringField01(): Bool & Impure =
        import static get flix.test.TestClass.STRING_FIELD: String & Impure as getField;
        import static set flix.test.TestClass.STRING_FIELD: Unit & Impure as setField;
        setField("");
        getField() == ""

    @test
    def testPutStaticStringField02(): Bool & Impure =
        import static get flix.test.TestClass.STRING_FIELD: String & Impure as getField;
        import static set flix.test.TestClass.STRING_FIELD: Unit & Impure as setField;
        setField("Goodbye World!");
        getField() == "Goodbye World!"

}
