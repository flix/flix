mod Test.Exp.Jvm.InvokeConstructor {

    @Test
    def testInvokeConstructor01(): ##java.lang.Boolean \ IO =
        import new java.lang.Boolean(Bool): ##java.lang.Boolean \ IO as newBoolean;
        newBoolean(true)

    @Test
    def testInvokeConstructor02(): ##java.lang.Character \ IO =
        import new java.lang.Character(Char): ##java.lang.Character \ IO as newCharacter;
        newCharacter('a')

    @Test
    def testInvokeConstructor03(): ##java.lang.Float \ IO =
        import new java.lang.Float(Float32): ##java.lang.Float \ IO as newFloat;
        newFloat(123.456f32)

    @Test
    def testInvokeConstructor04(): ##java.lang.Double \ IO =
        import new java.lang.Double(Float64): ##java.lang.Double \ IO as newDouble;
        newDouble(123.456f64)

    @Test
    def testInvokeConstructor05(): ##java.lang.Byte \ IO =
        import new java.lang.Byte(Int8): ##java.lang.Byte \ IO as newByte;
        newByte(123i8)

    @Test
    def testInvokeConstructor06(): ##java.lang.Short \ IO =
        import new java.lang.Short(Int16): ##java.lang.Short \ IO as newShort;
        newShort(123i16)

    @Test
    def testInvokeConstructor07(): ##java.lang.Integer \ IO =
        import new java.lang.Integer(Int32): ##java.lang.Integer \ IO as newInteger;
        newInteger(123i32)

    @Test
    def testInvokeConstructor08(): ##java.lang.Long \ IO =
        import new java.lang.Long(Int64): ##java.lang.Long \ IO as newLong;
        newLong(123i64)

    @Test
    def testInvokeConstructor09(): ##java.lang.String \ IO =
        import new java.lang.String(String): ##java.lang.String \ IO as newString;
        newString("Hello World")

    @Test
    def testInvokePureConstructor01(): ##java.lang.String =
        import new java.lang.String(String): ##java.lang.String \ {} as newString;
        newString("Hello World")

    @Test
    def testInvokeObjectConstructor01(): ##java.lang.Object \ IO =
        import new java.lang.String(String): ##java.lang.Object \ IO as newString;
        newString("Hello World")

    @Test
    def testInvokeStaticNestedConstructor01(): ##java.util.Locale$Builder \ IO =
        import new java.util.Locale$Builder(): ##java.util.Locale$Builder as newBuilder;
        newBuilder()

    @Test
    def testInvokeConstructorWithArrayParam(): ##java.net.URLClassLoader \ IO = region rc {
        import new java.net.URLClassLoader(Array[##java.net.URL, rc]): ##java.net.URLClassLoader \ IO as newClassLoader;
        newClassLoader(Array#{} @ rc)
    }
}
