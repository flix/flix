mod Test.Exp.Jvm.InvokeConstructor {

    import java.lang.Boolean
    import java.lang.Byte
    import java.lang.Character
    import java.lang.Double
    import java.lang.Float
    import java.lang.Integer
    import java.lang.Long
    import java.lang.Object
    import java.lang.Short
    import java.lang.{String => JString}
    import java.net.URLClassLoader
    import java.util.{Locale$Builder => Builder}

    @test
    def testInvokeConstructor01(): Boolean \ IO =
        import java_new java.lang.Boolean(Bool): Boolean \ IO as newBoolean;
        newBoolean(true)

    @test
    def testInvokeConstructor02(): Character \ IO =
        import java_new java.lang.Character(Char): Character \ IO as newCharacter;
        newCharacter('a')

    @test
    def testInvokeConstructor03(): Float \ IO =
        import java_new java.lang.Float(Float32): Float \ IO as newFloat;
        newFloat(123.456f32)

    @test
    def testInvokeConstructor04(): Double \ IO =
        import java_new java.lang.Double(Float64): Double \ IO as newDouble;
        newDouble(123.456f64)

    @test
    def testInvokeConstructor05(): Byte \ IO =
        import java_new java.lang.Byte(Int8): Byte \ IO as newByte;
        newByte(123i8)

    @test
    def testInvokeConstructor06(): Short \ IO =
        import java_new java.lang.Short(Int16): Short \ IO as newShort;
        newShort(123i16)

    @test
    def testInvokeConstructor07(): Integer \ IO =
        import java_new java.lang.Integer(Int32): Integer \ IO as newInteger;
        newInteger(123i32)

    @test
    def testInvokeConstructor08(): Long \ IO =
        import java_new java.lang.Long(Int64): Long \ IO as newLong;
        newLong(123i64)

    @test
    def testInvokeConstructor09(): JString \ IO =
        import java_new java.lang.String(String): JString \ IO as newString;
        newString("Hello World")

    @test
    def testInvokePureConstructor01(): JString =
        import java_new java.lang.String(String): JString \ {} as newString;
        newString("Hello World")

    @test
    def testInvokeObjectConstructor01(): Object \ IO =
        import java_new java.lang.String(String): Object \ IO as newString;
        newString("Hello World")

    @test
    def testInvokeStaticNestedConstructor01(): Builder \ IO =
        import java_new java.util.Locale$Builder(): Builder as newBuilder;
        newBuilder()

    @test
    def testInvokeConstructorWithArrayParam(): URLClassLoader \ IO = region rc {
        import java_new java.net.URLClassLoader(Array[##java.net.URL, rc]): URLClassLoader \ IO as newClassLoader;
        newClassLoader(Array#{} @ rc)
    }
}
