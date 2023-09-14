mod Test.Exp.Jvm.GetFieldDoubleNestedClass {

    @Test
    def testGetBoolField01(): Bool \ IO =
        import new dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass(): ##dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass \ IO as newObject;
        import get dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.boolField: Bool \ IO as getField;
        let o: ##dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass = newObject();
        getField(o) == true

    @Test
    def testGetCharField01(): Bool \ IO =
        import new dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass(): ##dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass \ IO as newObject;
        import get dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.charField: Char \ IO as getField;
        let o: ##dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass = newObject();
        getField(o) == 'A'

    @Test
    def testGetFloat32Field01(): Bool \ IO =
        import new dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass(): ##dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass \ IO as newObject;
        import get dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.float32Field: Float32 \ IO as getField;
        let o: ##dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass = newObject();
        getField(o) == 123.456f32

    @Test
    def testGetFloat64Field01(): Bool \ IO =
        import new dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass(): ##dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass \ IO as newObject;
        import get dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.float64Field: Float64 \ IO as getField;
        let o: ##dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass = newObject();
        getField(o) == 123.456f64

    @Test
    def testGetInt8Field01(): Bool \ IO =
        import new dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass(): ##dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass \ IO as newObject;
        import get dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.int8Field: Int8 \ IO as getField;
        let o: ##dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass = newObject();
        getField(o) == 127i8

    @Test
    def testGetInt16Field01(): Bool \ IO =
        import new dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass(): ##dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass \ IO as newObject;
        import get dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.int16Field: Int16 \ IO as getField;
        let o: ##dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass = newObject();
        getField(o) == 127i16

    @Test
    def testGetInt32Field01(): Bool \ IO =
        import new dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass(): ##dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass \ IO as newObject;
        import get dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.int32Field: Int32 \ IO as getField;
        let o: ##dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass = newObject();
        getField(o) == 127i32

    @Test
    def testGetInt64Field01(): Bool \ IO =
        import new dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass(): ##dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass \ IO as newObject;
        import get dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.int64Field: Int64 \ IO as getField;
        let o: ##dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass = newObject();
        getField(o) == 127i64

    @Test
    def testGetStringField01(): Bool \ IO =
        import new dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass(): ##dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass \ IO as newObject;
        import get dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.stringField: String \ IO as getField;
        let o: ##dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass = newObject();
        getField(o) == "Hello World"

    @Test
    def testGetPureField01(): Bool =
        import new dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass(): ##dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass \ {} as newObject;
        import get dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.stringField: String \ {} as getField;
        let o: ##dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass = newObject();
        getField(o) == "Hello World"

    @Test
    def testGetObjectField01(): ##java.lang.Object \ IO =
        import new dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass(): ##dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass \ IO as newObject;
        import get dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass.stringField: ##java.lang.Object \ IO as getField;
        let o: ##dev.flix.test.TestClass$_StaticNestedClass$DoubleNestedClass = newObject();
        getField(o)
}
