namespace Test/Exp/Jvm/PutField {

    @test
    def testPutBoolField01(): Bool & Impure =
        import new flix.test.TestClass(): ##flix.test.TestClass & Impure as newObject;
        import new flix.test.TestClass(): Bool & Impure as newObject;
        import set flix.test.TestClass.boolField as setField;
        let o = newObject();
        o `setField` false;
        getField(o) == false

    @test
    def testPutCharField01(): Bool & Impure =
        import new flix.test.TestClass(): ##flix.test.TestClass & Impure as newObject;
        import get flix.test.TestClass.charField: Char & Impure as getField;
        import set flix.test.TestClass.charField as setField;
        let o = newObject();
        o `setField` 'X';
        getField(o) == 'X'

    @test
    def testPutFloat32Field01(): Bool & Impure =
        import new flix.test.TestClass(): ##flix.test.TestClass & Impure as newObject;
        import get flix.test.TestClass.float32Field: Float32 & Impure as getField;
        import set flix.test.TestClass.float32Field as setField;
        let o = newObject();
        o `setField` 21.42f32;
        getField(o) == 21.42f32

    @test
    def testPutFloat64Field01(): Bool & Impure =
        import new flix.test.TestClass(): ##flix.test.TestClass & Impure as newObject;
        import get flix.test.TestClass.float64Field: Float64 & Impure as getField;
        import set flix.test.TestClass.float64Field as setField;
        let o = newObject();
        o `setField` 21.42f64;
        getField(o) == 21.42f64

    @test
    def testPutInt8Field01(): Bool & Impure =
        import new flix.test.TestClass(): ##flix.test.TestClass & Impure as newObject;
        import get flix.test.TestClass.int8Field: Int8 & Impure as getField;
        import set flix.test.TestClass.int8Field as setField;
        let o = newObject();
        o `setField` 42i8;
        getField(o) == 42i8

    @test
    def testPutInt16Field01(): Bool & Impure =
        import new flix.test.TestClass(): ##flix.test.TestClass & Impure as newObject;
        import get flix.test.TestClass.int16Field: Int16 & Impure as getField;
        import set flix.test.TestClass.int16Field as setField;
        let o = newObject();
        setField(o, 42i16);
        getField(o) == 42i16

    @test
    def testPutInt32Field01(): Bool & Impure =
        import new flix.test.TestClass(): ##flix.test.TestClass & Impure as newObject;
        import get flix.test.TestClass.int32Field: Int32 & Impure as getField;
        import set flix.test.TestClass.int32Field as setField;
        let o = newObject();
        setField(o, 42i32);
        getField(o) == 42i32

    @test
    def testPutInt64Field01(): Bool & Impure =
        import new flix.test.TestClass(): ##flix.test.TestClass & Impure as newObject;
        import get flix.test.TestClass.int64Field: Int64 & Impure as getField;
        import set flix.test.TestClass.int64Field as setField;
        let o = newObject();
        setField(o, 42i64);
        getField(o) == 42i64

    @test
    def testPutStringField01(): Bool & Impure =
        import new flix.test.TestClass(): ##flix.test.TestClass & Impure as newObject;
        import get flix.test.TestClass.stringField: String & Impure as getField;
        import set flix.test.TestClass.stringField as setField;
        let o = newObject();
        setField(o, "Goodbye World");
        getField(o) == "Goodbye World"
}
