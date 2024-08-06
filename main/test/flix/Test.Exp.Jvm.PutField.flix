mod Test.Exp.Jvm.PutField {

    import dev.flix.test.TestClass
    import dev.flix.test.TestClassWithInheritedMethod
    @test
    def testPutBoolField01(): Bool \ IO =
        import java_get_field dev.flix.test.TestClass.boolField: Bool \ IO as getField;
        import java_set_field dev.flix.test.TestClass.boolField: Unit \ IO as setField;
        let o = new TestClass();
        o `setField` false;
        getField(o) == false

    @test
    def testPutCharField01(): Bool \ IO =
        import java_get_field dev.flix.test.TestClass.charField: Char \ IO as getField;
        import java_set_field dev.flix.test.TestClass.charField: Unit \ IO as setField;
        let o = new TestClass();
        o `setField` 'X';
        getField(o) == 'X'

    @test
    def testPutFloat32Field01(): Bool \ IO =
        import java_get_field dev.flix.test.TestClass.float32Field: Float32 \ IO as getField;
        import java_set_field dev.flix.test.TestClass.float32Field: Unit \ IO as setField;
        let o = new TestClass();
        o `setField` 21.42f32;
        getField(o) == 21.42f32

    @test
    def testPutFloat64Field01(): Bool \ IO =
        import java_get_field dev.flix.test.TestClass.float64Field: Float64 \ IO as getField;
        import java_set_field dev.flix.test.TestClass.float64Field: Unit \ IO as setField;
        let o = new TestClass();
        o `setField` 21.42f64;
        getField(o) == 21.42f64

    @test
    def testPutInt8Field01(): Bool \ IO =
        import java_get_field dev.flix.test.TestClass.int8Field: Int8 \ IO as getField;
        import java_set_field dev.flix.test.TestClass.int8Field: Unit \ IO as setField;
        let o = new TestClass();
        o `setField` 42i8;
        getField(o) == 42i8

    @test
    def testPutInt16Field01(): Bool \ IO =
        import java_get_field dev.flix.test.TestClass.int16Field: Int16 \ IO as getField;
        import java_set_field dev.flix.test.TestClass.int16Field: Unit \ IO as setField;
        let o = new TestClass();
        setField(o, 42i16);
        getField(o) == 42i16

    @test
    def testPutInt32Field01(): Bool \ IO =
        import java_get_field dev.flix.test.TestClass.int32Field: Int32 \ IO as getField;
        import java_set_field dev.flix.test.TestClass.int32Field: Unit \ IO as setField;
        let o = new TestClass();
        setField(o, 42i32);
        getField(o) == 42i32

    @test
    def testPutInt64Field01(): Bool \ IO =
        import java_get_field dev.flix.test.TestClass.int64Field: Int64 \ IO as getField;
        import java_set_field dev.flix.test.TestClass.int64Field: Unit \ IO as setField;
        let o = new TestClass();
        setField(o, 42i64);
        getField(o) == 42i64

    @test
    def testPutStringField01(): Bool \ IO =
        import java_get_field dev.flix.test.TestClass.stringField: String \ IO as getField;
        import java_set_field dev.flix.test.TestClass.stringField: Unit \ IO as setField;
        let o = new TestClass();
        setField(o, "Goodbye World");
        getField(o) == "Goodbye World"

    @test
    def testPutInheritedField01(): Bool \ IO =
        import java_get_field dev.flix.test.TestClassWithInheritedMethod.m_x: Int32 \ {} as getField;
        import java_set_field dev.flix.test.TestClassWithInheritedMethod.m_x: Unit \ IO as setField;
        let obj = new TestClassWithInheritedMethod();
        setField(obj, 123);
        getField(obj) == 123
}
