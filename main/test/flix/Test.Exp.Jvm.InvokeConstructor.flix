namespace Test/Exp/Jvm/InvokeConstructor {

    @test
    def testInvokeConstructor01(): ##java.lang.Boolean & Impure =
        import new java.lang.Boolean(Bool): ##java.lang.Boolean & Impure as newBoolean;
        newBoolean(true)

    @test
    def testInvokeConstructor02(): ##java.lang.Character & Impure =
        import new java.lang.Character(Char): ##java.lang.Character & Impure as newCharacter;
        newCharacter('a')

    @test
    def testInvokeConstructor03(): ##java.lang.Float & Impure =
        import new java.lang.Float(Float32): ##java.lang.Float & Impure as newFloat;
        newFloat(123.456f32)

    @test
    def testInvokeConstructor04(): ##java.lang.Double & Impure =
        import new java.lang.Double(Float64): ##java.lang.Double & Impure as newDouble;
        newDouble(123.456f64)

    @test
    def testInvokeConstructor05(): ##java.lang.Byte & Impure =
        import new java.lang.Byte(Int8): ##java.lang.Byte & Impure as newByte;
        newByte(123i8)

    @test
    def testInvokeConstructor06(): ##java.lang.Short & Impure =
        import new java.lang.Short(Int16): ##java.lang.Short & Impure as newShort;
        newShort(123i16)

    @test
    def testInvokeConstructor07(): ##java.lang.Integer & Impure =
        import new java.lang.Integer(Int32): ##java.lang.Integer & Impure as newInteger;
        newInteger(123i32)

    @test
    def testInvokeConstructor08(): ##java.lang.Long & Impure =
        import new java.lang.Long(Int64): ##java.lang.Long & Impure as newLong;
        newLong(123i64)

    @test
    def testInvokeConstructor09(): ##java.lang.String & Impure =
        import new java.lang.String(String): ##java.lang.String & Impure as newString;
        newString("Hello World")

    @test
    def testInvokePureConstructor01(): ##java.lang.String =
        import new java.lang.String(String): _ & Pure as newString;
        newString("Hello World")

    @test
    def testInvokeObjectConstructor01(): ##java.lang.Object & Impure =
        import new java.lang.String(String): ##java.lang.Object & Impure as newString;
        newString("Hello World")
}
