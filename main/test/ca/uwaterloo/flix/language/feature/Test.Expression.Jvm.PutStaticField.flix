namespace Test/Expression/Jvm/PutStaticField {

    @test
    def testPutStaticBoolField01(): Bool =
        import get flix.test.TestClass:BOOL_FIELD as getField;
        import put flix.test.TestClass:BOOL_FIELD as putField;
        putField(true);
        getField() == true

    @test
    def testPutStaticBoolField02(): Bool =
        import get flix.test.TestClass:BOOL_FIELD as getField;
        import put flix.test.TestClass:BOOL_FIELD as putField;
        putField(false);
        getField() == false

    @test
    def testPutStaticCharField01(): Bool =
        import get flix.test.TestClass:CHAR_FIELD as getField;
        import put flix.test.TestClass:CHAR_FIELD as putField;
        putField('a');
        getField() == 'a'

    @test
    def testPutStaticCharField02(): Bool =
        import get flix.test.TestClass:CHAR_FIELD as getField;
        import put flix.test.TestClass:CHAR_FIELD as putField;
        putField('z');
        getField() == 'z'

    @test
    def testPutStaticFloat32Field01(): Bool =
        import get flix.test.TestClass:FLOAT32_FIELD as getField;
        import put flix.test.TestClass:FLOAT32_FIELD as putField;
        putField(0.0f32);
        getField() == 0.0f32

    @test
    def testPutStaticFloat32Field02(): Bool =
        import get flix.test.TestClass:FLOAT32_FIELD as getField;
        import put flix.test.TestClass:FLOAT32_FIELD as putField;
        putField(21.42f32);
        getField() == 21.42f32

    @test
    def testPutStaticFloat64Field01(): Bool =
        import get flix.test.TestClass:FLOAT64_FIELD as getField;
        import put flix.test.TestClass:FLOAT64_FIELD as putField;
        putField(0.0f64);
        getField() == 0.0f64

    @test
    def testPutStaticFloat64Field02(): Bool =
        import get flix.test.TestClass:FLOAT64_FIELD as getField;
        import put flix.test.TestClass:FLOAT64_FIELD as putField;
        putField(21.42f64);
        getField() == 21.42f64

    @test
    def testPutStaticInt8Field01(): Bool =
        import get flix.test.TestClass:INT8_FIELD as getField;
        import put flix.test.TestClass:INT8_FIELD as putField;
        putField(0i8);
        getField() == 0i8

    @test
    def testPutStaticInt8Field02(): Bool =
        import get flix.test.TestClass:INT8_FIELD as getField;
        import put flix.test.TestClass:INT8_FIELD as putField;
        putField(42i8);
        getField() == 42i8


}
