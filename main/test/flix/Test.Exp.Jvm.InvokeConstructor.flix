mod Test.Exp.Jvm.InvokeConstructor {

    @test
    def testInvokeConstructor01(): ##java.lang.Boolean \ IO =
        import java_new java.lang.Boolean(Bool): ##java.lang.Boolean \ IO as newBoolean;
        newBoolean(true)

    @test
    def testInvokeConstructor02(): ##java.lang.Character \ IO =
        import java_new java.lang.Character(Char): ##java.lang.Character \ IO as newCharacter;
        newCharacter('a')

    @test
    def testInvokeConstructor03(): ##java.lang.Float \ IO =
        import java_new java.lang.Float(Float32): ##java.lang.Float \ IO as newFloat;
        newFloat(123.456f32)

    @test
    def testInvokeConstructor04(): ##java.lang.Double \ IO =
        import java_new java.lang.Double(Float64): ##java.lang.Double \ IO as newDouble;
        newDouble(123.456f64)

    @test
    def testInvokeConstructor05(): ##java.lang.Byte \ IO =
        import java_new java.lang.Byte(Int8): ##java.lang.Byte \ IO as newByte;
        newByte(123i8)

    @test
    def testInvokeConstructor06(): ##java.lang.Short \ IO =
        import java_new java.lang.Short(Int16): ##java.lang.Short \ IO as newShort;
        newShort(123i16)

    @test
    def testInvokeConstructor07(): ##java.lang.Integer \ IO =
        import java_new java.lang.Integer(Int32): ##java.lang.Integer \ IO as newInteger;
        newInteger(123i32)

    @test
    def testInvokeConstructor08(): ##java.lang.Long \ IO =
        import java_new java.lang.Long(Int64): ##java.lang.Long \ IO as newLong;
        newLong(123i64)

    @test
    def testInvokeConstructor09(): ##java.lang.String \ IO =
        import java_new java.lang.String(String): ##java.lang.String \ IO as newString;
        newString("Hello World")

    @test
    def testInvokePureConstructor01(): ##java.lang.String =
        import java_new java.lang.String(String): ##java.lang.String \ {} as newString;
        newString("Hello World")

    @test
    def testInvokeObjectConstructor01(): ##java.lang.Object \ IO =
        import java_new java.lang.String(String): ##java.lang.Object \ IO as newString;
        newString("Hello World")

    @test
    def testInvokeStaticNestedConstructor01(): ##java.util.Locale$Builder \ IO =
        import java_new java.util.Locale$Builder(): ##java.util.Locale$Builder as newBuilder;
        newBuilder()

    @test
    def testInvokeConstructorWithArrayParam(): ##java.net.URLClassLoader \ IO = region rc {
        import java_new java.net.URLClassLoader(Array[##java.net.URL, rc]): ##java.net.URLClassLoader \ IO as newClassLoader;
        newClassLoader(Array#{} @ rc)
    }
}
