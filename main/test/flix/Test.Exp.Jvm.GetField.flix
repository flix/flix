mod Test.Exp.Jvm.GetField {

    import dev.flix.test.TestClass
    import dev.flix.test.TestClassWithInheritedMethod
    import java.lang.{Object => JObject}

    @test
    def testGetBoolField01(): Bool \ IO =
        import java_get_field dev.flix.test.TestClass.boolField: Bool \ IO as getField;
        let o = new TestClass();
        getField(o) == true

    @test
    def testGetCharField01(): Bool \ IO =
        import java_get_field dev.flix.test.TestClass.charField: Char \ IO as getField;
        let o = new TestClass();
        getField(o) == 'A'

    @test
    def testGetFloat32Field01(): Bool \ IO =
        import java_get_field dev.flix.test.TestClass.float32Field: Float32 \ IO as getField;
        let o = new TestClass();
        getField(o) == 123.456f32

    @test
    def testGetFloat64Field01(): Bool \ IO =
        import java_get_field dev.flix.test.TestClass.float64Field: Float64 \ IO as getField;
        let o = new TestClass();
        getField(o) == 123.456f64

    @test
    def testGetInt8Field01(): Bool \ IO =
        import java_get_field dev.flix.test.TestClass.int8Field: Int8 \ IO as getField;
        let o = new TestClass();
        getField(o) == 127i8

    @test
    def testGetInt16Field01(): Bool \ IO =
        import java_get_field dev.flix.test.TestClass.int16Field: Int16 \ IO as getField;
        let o = new TestClass();
        getField(o) == 127i16

    @test
    def testGetInt32Field01(): Bool \ IO =
        import java_get_field dev.flix.test.TestClass.int32Field: Int32 \ IO as getField;
        let o = new TestClass();
        getField(o) == 127i32

    @test
    def testGetInt64Field01(): Bool \ IO =
        import java_get_field dev.flix.test.TestClass.int64Field: Int64 \ IO as getField;
        let o = new TestClass();
        getField(o) == 127i64

    @test
    def testGetStringField01(): Bool \ IO =
        import java_get_field dev.flix.test.TestClass.stringField: String \ IO as getField;
        let o = new TestClass();
        getField(o) == "Hello World"

    @test
    def testGetPureField01(): Bool =
        import java_get_field dev.flix.test.TestClass.stringField: String \ {} as getField;
        let o = unsafe new TestClass();
        getField(o) == "Hello World"

    @test
    def testGetObjectField01(): JObject \ IO =
        import java_get_field dev.flix.test.TestClass.stringField: JObject \ IO as getField;
        let o = unsafe new TestClass();
        getField(o)

    @test
    def testGetInheritedField01(): Bool \ IO =
        import java_get_field dev.flix.test.TestClassWithInheritedMethod.m_x: Int32 \ {} as getField;
        let obj = new TestClassWithInheritedMethod();
        getField(obj) == 42

}
