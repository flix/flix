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

}
