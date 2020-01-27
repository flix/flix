namespace Test/Expression/Jvm/GetField {

    @test
    def testGetField01(): Bool =
        import flix.test.TestClass:__new__() as newObject;
        import get flix.test.TestClass.boolField as getField;
        let o = newObject();
        getField(o) == true

    @test
    def testGetField02(): Bool =
        import flix.test.TestClass:__new__() as newObject;
        import get flix.test.TestClass.charField as getField;
        let o = newObject();
        getField(o) == 'A'

    @test
    def testGetField03(): Bool =
        import flix.test.TestClass:__new__() as newObject;
        import get flix.test.TestClass.float32Field as getField;
        let o = newObject();
        getField(o) == 123.456f32

    @test
    def testGetField04(): Bool =
        import flix.test.TestClass:__new__() as newObject;
        import get flix.test.TestClass.float64Field as getField;
        let o = newObject();
        getField(o) == 123.456f64

    @test
    def testGetField05(): Bool =
        import flix.test.TestClass:__new__() as newObject;
        import get flix.test.TestClass.int8Field as getField;
        let o = newObject();
        getField(o) == 127i8

    @test
    def testGetField06(): Bool =
        import flix.test.TestClass:__new__() as newObject;
        import get flix.test.TestClass.int16Field as getField;
        let o = newObject();
        getField(o) == 127i16

    @test
    def testGetField07(): Bool =
        import flix.test.TestClass:__new__() as newObject;
        import get flix.test.TestClass.int32Field as getField;
        let o = newObject();
        getField(o) == 127i32

    @test
    def testGetField08(): Bool =
        import flix.test.TestClass:__new__() as newObject;
        import get flix.test.TestClass.int64Field as getField;
        let o = newObject();
        getField(o) == 127i64

    @test
    def testGetField09(): Bool =
        import flix.test.TestClass:__new__() as newObject;
        import get flix.test.TestClass.stringField as getField;
        let o = newObject();
        getField(o) == "Hello World"

}