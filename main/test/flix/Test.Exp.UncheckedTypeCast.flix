mod Test.Exp.UncheckedTypeCast {

    import java.lang.Integer
    import java.lang.Object
    import java.io.Serializable
    import java.lang.StringBuilder
    import java.lang.Error

    @test
    def testUncheckedTypeCast01(): Object = unchecked_cast(null as Object)

    @test
    def testUncheckedTypeCast02(): String = unchecked_cast(null as String)

    @test
    def testUncheckedTypeCast03(): Integer = unchecked_cast(null as Integer)

    @test
    def testUncheckedTypeCast04(): Serializable = unchecked_cast(null as Serializable)

    @test
    def testUncheckedTypeCast05(): Serializable = region rc {
        let _ = rc;
        let sb = unchecked_cast ((new StringBuilder(): _ \ IO) as _ \ rc);
        unchecked_cast(sb as Serializable)
    }

    @test
    def testUncheckedTypeCast06(): Object = region rc {
        let _ = rc;
        let sb = unchecked_cast ((new StringBuilder(): _ \ IO) as _ \ rc);
        unchecked_cast(sb as Object)
    }

    def castToString(x: a): String =
        unchecked_cast(x as String)

    @test
    def testUncheckedTypeCast07(): Bool =
        // Test that cast from primitive types to object types doesn't cause compile time crash.
        if (-3 < 10) true else castToString(42) == ""

}
