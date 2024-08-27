mod Test.Exp.Jvm.GetFieldDoubleNestedClass {

    import dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass
    import java.lang.Object

    @test
    def testGetBoolField01(): Bool \ IO =
        import java_get_field dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.boolField: Bool \ IO as getField;
        let o: TestClass$_StaticNestedClass$DoubleNestedClass = new TestClass$_StaticNestedClass$DoubleNestedClass();
        getField(o) == true

    @test
    def testGetCharField01(): Bool \ IO =
        import java_get_field dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.charField: Char \ IO as getField;
        let o: TestClass$_StaticNestedClass$DoubleNestedClass = new TestClass$_StaticNestedClass$DoubleNestedClass();
        getField(o) == 'A'

    @test
    def testGetFloat32Field01(): Bool \ IO =
        import java_get_field dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.float32Field: Float32 \ IO as getField;
        let o: TestClass$_StaticNestedClass$DoubleNestedClass = new TestClass$_StaticNestedClass$DoubleNestedClass();
        getField(o) == 123.456f32

    @test
    def testGetFloat64Field01(): Bool \ IO =
        import java_get_field dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.float64Field: Float64 \ IO as getField;
        let o: TestClass$_StaticNestedClass$DoubleNestedClass = new TestClass$_StaticNestedClass$DoubleNestedClass();
        getField(o) == 123.456f64

    @test
    def testGetInt8Field01(): Bool \ IO =
        import java_get_field dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.int8Field: Int8 \ IO as getField;
        let o: TestClass$_StaticNestedClass$DoubleNestedClass = new TestClass$_StaticNestedClass$DoubleNestedClass();
        getField(o) == 127i8

    @test
    def testGetInt16Field01(): Bool \ IO =
        import java_get_field dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.int16Field: Int16 \ IO as getField;
        let o: TestClass$_StaticNestedClass$DoubleNestedClass = new TestClass$_StaticNestedClass$DoubleNestedClass();
        getField(o) == 127i16

    @test
    def testGetInt32Field01(): Bool \ IO =
        import java_get_field dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.int32Field: Int32 \ IO as getField;
        let o: TestClass$_StaticNestedClass$DoubleNestedClass = new TestClass$_StaticNestedClass$DoubleNestedClass();
        getField(o) == 127i32

    @test
    def testGetInt64Field01(): Bool \ IO =
        import java_get_field dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.int64Field: Int64 \ IO as getField;
        let o: TestClass$_StaticNestedClass$DoubleNestedClass = new TestClass$_StaticNestedClass$DoubleNestedClass();
        getField(o) == 127i64

    @test
    def testGetStringField01(): Bool \ IO =
        import java_get_field dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.stringField: String \ IO as getField;
        let o: TestClass$_StaticNestedClass$DoubleNestedClass = new TestClass$_StaticNestedClass$DoubleNestedClass();
        getField(o) == "Hello World"

    @test
    def testGetPureField01(): Bool =
        import java_get_field dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.stringField: String \ {} as getField;
        let o: TestClass$_StaticNestedClass$DoubleNestedClass = unsafe new TestClass$_StaticNestedClass$DoubleNestedClass();
        getField(o) == "Hello World"

    @test
    def testGetObjectField01(): Object \ IO =
        import java_get_field dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.stringField: Object \ IO as getField;
        let o: TestClass$_StaticNestedClass$DoubleNestedClass = new TestClass$_StaticNestedClass$DoubleNestedClass();
        getField(o)
}
