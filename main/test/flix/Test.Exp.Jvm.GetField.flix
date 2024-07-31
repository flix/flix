mod Test.Exp.Jvm.GetField {

    import java.lang.Object

    @test
    def testGetBoolField01(): Bool \ IO =
        import java_new dev.flix.test.TestClass(): ##dev.flix.test.TestClass \ IO as newObject;
        let o = newObject();
        o.java_get_field() == true

    @test
    def testGetCharField01(): Bool \ IO =
        import java_new dev.flix.test.TestClass(): ##dev.flix.test.TestClass \ IO as newObject;
        let o = newObject();
        o.java_get_field() == 'A'

    @test
    def testGetFloat32Field01(): Bool \ IO =
        import java_new dev.flix.test.TestClass(): ##dev.flix.test.TestClass \ IO as newObject;
        let o = newObject();
        o.java_get_field() == 123.456f32

    @test
    def testGetFloat64Field01(): Bool \ IO =
        import java_new dev.flix.test.TestClass(): ##dev.flix.test.TestClass \ IO as newObject;
        let o = newObject();
        o.java_get_field() == 123.456f64

    @test
    def testGetInt8Field01(): Bool \ IO =
        import java_new dev.flix.test.TestClass(): ##dev.flix.test.TestClass \ IO as newObject;
        let o = newObject();
        o.java_get_field() == 127i8

    @test
    def testGetInt16Field01(): Bool \ IO =
        import java_new dev.flix.test.TestClass(): ##dev.flix.test.TestClass \ IO as newObject;
        let o = newObject();
        o.java_get_field() == 127i16

    @test
    def testGetInt32Field01(): Bool \ IO =
        import java_new dev.flix.test.TestClass(): ##dev.flix.test.TestClass \ IO as newObject;
        let o = newObject();
        o.java_get_field() == 127i32

    @test
    def testGetInt64Field01(): Bool \ IO =
        import java_new dev.flix.test.TestClass(): ##dev.flix.test.TestClass \ IO as newObject;
        let o = newObject();
        o.java_get_field() == 127i64

    @test
    def testGetStringField01(): Bool \ IO =
        import java_new dev.flix.test.TestClass(): ##dev.flix.test.TestClass \ IO as newObject;
        let o = newObject();
        o.java_get_field() == "Hello World"

    @test
    def testGetPureField01(): Bool =
        import java_new dev.flix.test.TestClass(): ##dev.flix.test.TestClass \ {} as newObject;
        let o = newObject();
        o.java_get_field() == "Hello World"

    @test
    def testGetObjectField01(): Object \ IO =
        import java_new dev.flix.test.TestClass(): ##dev.flix.test.TestClass \ IO as newObject;
        let o = newObject();
        o.java_get_field()

    @test
    def testGetInheritedField01(): Bool \ IO =
        import java_new dev.flix.test.TestClassWithInheritedMethod(): ##dev.flix.test.TestClassWithInheritedMethod as newObj;
        let obj = newObj();
        o.java_get_field() == 42

}
