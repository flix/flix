mod Test.Exp.Jvm.GetFieldStaticInnerClass {

    import java.lang.Object

    @test
    def testGetBoolField01(): Bool \ IO =
        import java_new dev.flix.test.TestClass$_StaticNestedClass(): ##dev.flix.test.TestClass$_StaticNestedClass \ IO as newObject;
        import java_get_field dev.flix.test.TestClass$_StaticNestedClass.boolField: Bool \ IO as getField;
        let o: ##dev.flix.test.TestClass$_StaticNestedClass = newObject();
        getField(o) == true

    @test
    def testGetCharField01(): Bool \ IO =
        import java_new dev.flix.test.TestClass$_StaticNestedClass(): ##dev.flix.test.TestClass$_StaticNestedClass \ IO as newObject;
        import java_get_field dev.flix.test.TestClass$_StaticNestedClass.charField: Char \ IO as getField;
        let o: ##dev.flix.test.TestClass$_StaticNestedClass = newObject();
        getField(o) == 'A'

    @test
    def testGetFloat32Field01(): Bool \ IO =
        import java_new dev.flix.test.TestClass$_StaticNestedClass(): ##dev.flix.test.TestClass$_StaticNestedClass \ IO as newObject;
        import java_get_field dev.flix.test.TestClass$_StaticNestedClass.float32Field: Float32 \ IO as getField;
        let o: ##dev.flix.test.TestClass$_StaticNestedClass = newObject();
        getField(o) == 123.456f32

    @test
    def testGetFloat64Field01(): Bool \ IO =
        import java_new dev.flix.test.TestClass$_StaticNestedClass(): ##dev.flix.test.TestClass$_StaticNestedClass \ IO as newObject;
        import java_get_field dev.flix.test.TestClass$_StaticNestedClass.float64Field: Float64 \ IO as getField;
        let o: ##dev.flix.test.TestClass$_StaticNestedClass = newObject();
        getField(o) == 123.456f64

    @test
    def testGetInt8Field01(): Bool \ IO =
        import java_new dev.flix.test.TestClass$_StaticNestedClass(): ##dev.flix.test.TestClass$_StaticNestedClass \ IO as newObject;
        import java_get_field dev.flix.test.TestClass$_StaticNestedClass.int8Field: Int8 \ IO as getField;
        let o: ##dev.flix.test.TestClass$_StaticNestedClass = newObject();
        getField(o) == 127i8

    @test
    def testGetInt16Field01(): Bool \ IO =
        import java_new dev.flix.test.TestClass$_StaticNestedClass(): ##dev.flix.test.TestClass$_StaticNestedClass \ IO as newObject;
        import java_get_field dev.flix.test.TestClass$_StaticNestedClass.int16Field: Int16 \ IO as getField;
        let o: ##dev.flix.test.TestClass$_StaticNestedClass = newObject();
        getField(o) == 127i16

    @test
    def testGetInt32Field01(): Bool \ IO =
        import java_new dev.flix.test.TestClass$_StaticNestedClass(): ##dev.flix.test.TestClass$_StaticNestedClass \ IO as newObject;
        import java_get_field dev.flix.test.TestClass$_StaticNestedClass.int32Field: Int32 \ IO as getField;
        let o: ##dev.flix.test.TestClass$_StaticNestedClass = newObject();
        getField(o) == 127i32

    @test
    def testGetInt64Field01(): Bool \ IO =
        import java_new dev.flix.test.TestClass$_StaticNestedClass(): ##dev.flix.test.TestClass$_StaticNestedClass \ IO as newObject;
        import java_get_field dev.flix.test.TestClass$_StaticNestedClass.int64Field: Int64 \ IO as getField;
        let o: ##dev.flix.test.TestClass$_StaticNestedClass = newObject();
        getField(o) == 127i64

    @test
    def testGetStringField01(): Bool \ IO =
        import java_new dev.flix.test.TestClass$_StaticNestedClass(): ##dev.flix.test.TestClass$_StaticNestedClass \ IO as newObject;
        import java_get_field dev.flix.test.TestClass$_StaticNestedClass.stringField: String \ IO as getField;
        let o: ##dev.flix.test.TestClass$_StaticNestedClass = newObject();
        getField(o) == "Hello World"

    @test
    def testGetPureField01(): Bool =
        import java_new dev.flix.test.TestClass$_StaticNestedClass(): ##dev.flix.test.TestClass$_StaticNestedClass \ {} as newObject;
        import java_get_field dev.flix.test.TestClass$_StaticNestedClass.stringField: String \ {} as getField;
        let o: ##dev.flix.test.TestClass$_StaticNestedClass = newObject();
        getField(o) == "Hello World"

    @test
    def testGetObjectField01(): Object \ IO =
        import java_new dev.flix.test.TestClass$_StaticNestedClass(): ##dev.flix.test.TestClass$_StaticNestedClass \ IO as newObject;
        import java_get_field dev.flix.test.TestClass$_StaticNestedClass.stringField: ##java.lang.Object \ IO as getField;
        let o: ##dev.flix.test.TestClass$_StaticNestedClass = newObject();
        getField(o)
}
