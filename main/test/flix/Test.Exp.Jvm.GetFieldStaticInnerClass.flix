mod Test.Exp.Jvm.GetFieldStaticInnerClass {

    @Test
    def testGetBoolField01(): Bool \ IO =
        import new dev.flix.test.TestClass$_StaticNestedClass(): ##dev.flix.test.TestClass$_StaticNestedClass \ IO as newObject;
        import get dev.flix.test.TestClass$_StaticNestedClass.boolField: Bool \ IO as getField;
        let o: ##dev.flix.test.TestClass$_StaticNestedClass = newObject();
        getField(o) == true

    @Test
    def testGetCharField01(): Bool \ IO =
        import new dev.flix.test.TestClass$_StaticNestedClass(): ##dev.flix.test.TestClass$_StaticNestedClass \ IO as newObject;
        import get dev.flix.test.TestClass$_StaticNestedClass.charField: Char \ IO as getField;
        let o: ##dev.flix.test.TestClass$_StaticNestedClass = newObject();
        getField(o) == 'A'

    @Test
    def testGetFloat32Field01(): Bool \ IO =
        import new dev.flix.test.TestClass$_StaticNestedClass(): ##dev.flix.test.TestClass$_StaticNestedClass \ IO as newObject;
        import get dev.flix.test.TestClass$_StaticNestedClass.float32Field: Float32 \ IO as getField;
        let o: ##dev.flix.test.TestClass$_StaticNestedClass = newObject();
        getField(o) == 123.456f32

    @Test
    def testGetFloat64Field01(): Bool \ IO =
        import new dev.flix.test.TestClass$_StaticNestedClass(): ##dev.flix.test.TestClass$_StaticNestedClass \ IO as newObject;
        import get dev.flix.test.TestClass$_StaticNestedClass.float64Field: Float64 \ IO as getField;
        let o: ##dev.flix.test.TestClass$_StaticNestedClass = newObject();
        getField(o) == 123.456f64

    @Test
    def testGetInt8Field01(): Bool \ IO =
        import new dev.flix.test.TestClass$_StaticNestedClass(): ##dev.flix.test.TestClass$_StaticNestedClass \ IO as newObject;
        import get dev.flix.test.TestClass$_StaticNestedClass.int8Field: Int8 \ IO as getField;
        let o: ##dev.flix.test.TestClass$_StaticNestedClass = newObject();
        getField(o) == 127i8

    @Test
    def testGetInt16Field01(): Bool \ IO =
        import new dev.flix.test.TestClass$_StaticNestedClass(): ##dev.flix.test.TestClass$_StaticNestedClass \ IO as newObject;
        import get dev.flix.test.TestClass$_StaticNestedClass.int16Field: Int16 \ IO as getField;
        let o: ##dev.flix.test.TestClass$_StaticNestedClass = newObject();
        getField(o) == 127i16

    @Test
    def testGetInt32Field01(): Bool \ IO =
        import new dev.flix.test.TestClass$_StaticNestedClass(): ##dev.flix.test.TestClass$_StaticNestedClass \ IO as newObject;
        import get dev.flix.test.TestClass$_StaticNestedClass.int32Field: Int32 \ IO as getField;
        let o: ##dev.flix.test.TestClass$_StaticNestedClass = newObject();
        getField(o) == 127i32

    @Test
    def testGetInt64Field01(): Bool \ IO =
        import new dev.flix.test.TestClass$_StaticNestedClass(): ##dev.flix.test.TestClass$_StaticNestedClass \ IO as newObject;
        import get dev.flix.test.TestClass$_StaticNestedClass.int64Field: Int64 \ IO as getField;
        let o: ##dev.flix.test.TestClass$_StaticNestedClass = newObject();
        getField(o) == 127i64

    @Test
    def testGetStringField01(): Bool \ IO =
        import new dev.flix.test.TestClass$_StaticNestedClass(): ##dev.flix.test.TestClass$_StaticNestedClass \ IO as newObject;
        import get dev.flix.test.TestClass$_StaticNestedClass.stringField: String \ IO as getField;
        let o: ##dev.flix.test.TestClass$_StaticNestedClass = newObject();
        getField(o) == "Hello World"

    @Test
    def testGetPureField01(): Bool =
        import new dev.flix.test.TestClass$_StaticNestedClass(): ##dev.flix.test.TestClass$_StaticNestedClass \ {} as newObject;
        import get dev.flix.test.TestClass$_StaticNestedClass.stringField: String \ {} as getField;
        let o: ##dev.flix.test.TestClass$_StaticNestedClass = newObject();
        getField(o) == "Hello World"

    @Test
    def testGetObjectField01(): ##java.lang.Object \ IO =
        import new dev.flix.test.TestClass$_StaticNestedClass(): ##dev.flix.test.TestClass$_StaticNestedClass \ IO as newObject;
        import get dev.flix.test.TestClass$_StaticNestedClass.stringField: ##java.lang.Object \ IO as getField;
        let o: ##dev.flix.test.TestClass$_StaticNestedClass = newObject();
        getField(o)
}
