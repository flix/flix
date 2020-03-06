namespace Test/Exp/Jvm/GetField {

    @test
    def testGetBoolField01(): Bool & Impure =
        import new flix.test.TestClass() as newObject;
        import get flix.test.TestClass.boolField as getField;
        let o = newObject();
        getField(o) == true

    @test
    def testGetCharField01(): Bool & Impure =
        import new flix.test.TestClass() as newObject;
        import get flix.test.TestClass.charField as getField;
        let o = newObject();
        getField(o) == 'A'

    @test
    def testGetFloat32Field01(): Bool & Impure =
        import new flix.test.TestClass() as newObject;
        import get flix.test.TestClass.float32Field as getField;
        let o = newObject();
        getField(o) == 123.456f32

    @test
    def testGetFloat64Field01(): Bool & Impure =
        import new flix.test.TestClass() as newObject;
        import get flix.test.TestClass.float64Field as getField;
        let o = newObject();
        getField(o) == 123.456f64

    @test
    def testGetInt8Field01(): Bool & Impure =
        import new flix.test.TestClass() as newObject;
        import get flix.test.TestClass.int8Field as getField;
        let o = newObject();
        getField(o) == 127i8

    @test
    def testGetInt16Field01(): Bool & Impure =
        import new flix.test.TestClass() as newObject;
        import get flix.test.TestClass.int16Field as getField;
        let o = newObject();
        getField(o) == 127i16

    @test
    def testGetInt32Field01(): Bool & Impure =
        import new flix.test.TestClass() as newObject;
        import get flix.test.TestClass.int32Field as getField;
        let o = newObject();
        getField(o) == 127i32

    @test
    def testGetInt64Field01(): Bool & Impure =
        import new flix.test.TestClass() as newObject;
        import get flix.test.TestClass.int64Field as getField;
        let o = newObject();
        getField(o) == 127i64

    @test
    def testGetStringField01(): Bool & Impure =
        import new flix.test.TestClass() as newObject;
        import get flix.test.TestClass.stringField as getField;
        let o = newObject();
        getField(o) == "Hello World"

}
