namespace Test/Exp/Jvm/PutField {

    @test
    def testPutBoolField01(): Bool & Impure =
        import new flix.test.TestClass() as newObject;
        import get flix.test.TestClass.boolField as getField;
        import set flix.test.TestClass.boolField as setField;
        let o = newObject();
        o.setField(false);
        getField(o) == false

    @test
    def testPutCharField01(): Bool & Impure =
        import new flix.test.TestClass() as newObject;
        import get flix.test.TestClass.charField as getField;
        import set flix.test.TestClass.charField as setField;
        let o = newObject();
        o.setField('X');
        getField(o) == 'X'

    @test
    def testPutFloat32Field01(): Bool & Impure =
        import new flix.test.TestClass() as newObject;
        import get flix.test.TestClass.float32Field as getField;
        import set flix.test.TestClass.float32Field as setField;
        let o = newObject();
        o.setField(21.42f32);
        getField(o) == 21.42f32

    @test
    def testPutFloat64Field01(): Bool & Impure =
        import new flix.test.TestClass() as newObject;
        import get flix.test.TestClass.float64Field as getField;
        import set flix.test.TestClass.float64Field as setField;
        let o = newObject();
        o.setField(21.42f64);
        getField(o) == 21.42f64

    @test
    def testPutInt8Field01(): Bool & Impure =
        import new flix.test.TestClass() as newObject;
        import get flix.test.TestClass.int8Field as getField;
        import set flix.test.TestClass.int8Field as setField;
        let o = newObject();
        o.setField(42i8);
        getField(o) == 42i8

    @test
    def testPutInt16Field01(): Bool & Impure =
        import new flix.test.TestClass() as newObject;
        import get flix.test.TestClass.int16Field as getField;
        import set flix.test.TestClass.int16Field as setField;
        let o = newObject();
        o.setField(42i16);
        getField(o) == 42i16

    @test
    def testPutInt32Field01(): Bool & Impure =
        import new flix.test.TestClass() as newObject;
        import get flix.test.TestClass.int32Field as getField;
        import set flix.test.TestClass.int32Field as setField;
        let o = newObject();
        o.setField(42i32);
        getField(o) == 42i32

    @test
    def testPutInt64Field01(): Bool & Impure =
        import new flix.test.TestClass() as newObject;
        import get flix.test.TestClass.int64Field as getField;
        import set flix.test.TestClass.int64Field as setField;
        let o = newObject();
        o.setField(42i64);
        getField(o) == 42i64

    @test
    def testPutStringField01(): Bool & Impure =
        import new flix.test.TestClass() as newObject;
        import get flix.test.TestClass.stringField as getField;
        import set flix.test.TestClass.stringField as setField;
        let o = newObject();
        o.setField("Goodbye World");
        getField(o) == "Goodbye World"

}
