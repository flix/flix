mod Test.Exp.CheckedTypeCast {

    import java.io.Serializable
    import java.lang.Integer
    import java.lang.Object
    import java.lang.{StringBuilder => JStringBuilder}

    @test
    def testCheckedTypeCast01(): Object = checked_cast(null)

    @test
    def testCheckedTypeCast02(): String = checked_cast(null)

    @test
    def testCheckedTypeCast03(): Integer = checked_cast(null)

    @test
    def testCheckedTypeCast04(): Serializable = checked_cast(null)

    @test
    def testCheckedTypeCast05(): Array[String, Static] = checked_cast(null)

    @test

    def testCheckedTypeCast06(): Serializable \ IO = {
        checked_cast(new JStringBuilder())
    }

    @test
    def testCheckedTypeCast07(): Object \ IO = {
        checked_cast(new JStringBuilder())
    }

    @test
    def testCheckedTypeCast08(): Object = checked_cast("ABC")

    @test
    def testCheckedTypeCast09(): Object = region rc {
        checked_cast(Array#{} @ rc)
    }

    @test
    def testCheckedTypeCast10(): Object = checked_cast(Box.box(42i8))

    @test
    def testCheckedTypeCast11(): Object = checked_cast(Box.box(42i16))

    @test
    def testCheckedTypeCast12(): Object = checked_cast(Box.box(42i32))

    @test
    def testCheckedTypeCast13(): Object = checked_cast(Box.box(42i64))

    @test
    def testCheckedTypeCast14(): Object = checked_cast(Box.box(42.0f32))

    @test
    def testCheckedTypeCast15(): Object = checked_cast(Box.box(42.0f64))

    @test
    def testCheckedTypeCast16(): Object = checked_cast(Box.box('c'))

    @test
    def testCheckedTypeCast17(): Object = checked_cast(Box.box(true))

    @test
    def testCheckedTypeCast18(): Object = upcast(42)

    type alias Num = Int32

    def upcast(x: Num): Object = checked_cast(Box.box(x))

}
