namespace Test/Expression/Jvm/InvokeConstructor {

    @test
    def testInvokeConstructor01(): ##java.lang.Boolean =
        import new java.lang.Boolean(Bool) as newBoolean;
        newBoolean(true)

    @test
    def testInvokeConstructor02(): ##java.lang.Character =
        import new java.lang.Character(Char) as newCharacter;
        newCharacter('a')

    @test
    def testInvokeConstructor03(): ##java.lang.Float =
        import new java.lang.Float(Float32) as newFloat;
        newFloat(123.456f32)

    @test
    def testInvokeConstructor04(): ##java.lang.Double =
        import new java.lang.Double(Float64) as newDouble;
        newDouble(123.456f64)

    @test
    def testInvokeConstructor05(): ##java.lang.Byte =
        import new java.lang.Byte(Int8) as newByte;
        newByte(123i8)

    @test
    def testInvokeConstructor06(): ##java.lang.Short =
        import new java.lang.Short(Int16) as newShort;
        newShort(123i16)

    @test
    def testInvokeConstructor07(): ##java.lang.Integer =
        import new java.lang.Integer(Int32) as newInteger;
        newInteger(123i32)

    @test
    def testInvokeConstructor08(): ##java.lang.Long =
        import new java.lang.Long(Int64) as newLong;
        newLong(123i64)

    @test
    def testInvokeConstructor09(): ##java.lang.String =
        import new java.lang.String(String) as newString;
        newString("Hello World")

}
