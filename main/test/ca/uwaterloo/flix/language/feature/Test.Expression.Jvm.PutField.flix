namespace Test/Expression/Jvm/PutField {

    @test
    def testPutField01(): Bool =
        import flix.test.TestClass:__new__() as newObject;
        import get flix.test.TestClass.boolField as getField;
        import put flix.test.TestClass.boolField as setField;
        let o = newObject();
        o.setField(false);
        getField(o) == false

    @test
    def testPutField02(): Bool =
        import flix.test.TestClass:__new__() as newObject;
        import get flix.test.TestClass.charField as getField;
        import put flix.test.TestClass.charField as setField;
        let o = newObject();
        o.setField('X');
        getField(o) == 'X'

    @test
    def testPutField03(): Bool =
        import flix.test.TestClass:__new__() as newObject;
        import get flix.test.TestClass.float32Field as getField;
        import put flix.test.TestClass.float32Field as setField;
        let o = newObject();
        o.setField(21.42f32);
        getField(o) == 21.42f32

    @test
    def testPutField04(): Bool =
        import flix.test.TestClass:__new__() as newObject;
        import get flix.test.TestClass.float64Field as getField;
        import put flix.test.TestClass.float64Field as setField;
        let o = newObject();
        o.setField(21.42f64);
        getField(o) == 21.42f64

    @test
    def testPutField05(): Bool =
        import flix.test.TestClass:__new__() as newObject;
        import get flix.test.TestClass.int8Field as getField;
        import put flix.test.TestClass.int8Field as setField;
        let o = newObject();
        o.setField(42i8);
        getField(o) == 42i8

    @test
    def testPutField06(): Bool =
        import flix.test.TestClass:__new__() as newObject;
        import get flix.test.TestClass.int16Field as getField;
        import put flix.test.TestClass.int16Field as setField;
        let o = newObject();
        o.setField(42i16);
        getField(o) == 42i16



}