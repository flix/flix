namespace Test/Expression/Jvm/InvokeConstructor {

    @test
    def testInvokeConstructor01(): ##java.lang.Boolean =
        import java.lang.Boolean:__new__(Bool) as newBoolean;
        newBoolean(true)

    @test
    def testInvokeConstructor02(): ##java.lang.Byte =
        import java.lang.Byte:__new__(Int8) as newByte;
        newByte(123i8)

    @test
    def testInvokeConstructor03(): ##java.lang.Character =
        import java.lang.Character:__new__(Char) as newCharacter;
        newCharacter('a')

    @test
    def testInvokeConstructor04(): ##java.lang.Double =
        import java.lang.Double:__new__(Float64) as newDouble;
        newDouble(123.456f64)

    @test
    def testInvokeConstructor05(): ##java.lang.Float =
        import java.lang.Float:__new__(Int8) as newFloat;
        newFloat(123.456f32)

    @test
    def testInvokeConstructor06(): ##java.lang.Integer =
        import java.lang.Integer:__new__(Int32) as newInteger;
        newInteger(123i32)

    @test
    def testInvokeConstructor07(): ##java.lang.Long =
        import java.lang.Long:__new__(Int64) as newLong;
        newLong(123i64)

}
