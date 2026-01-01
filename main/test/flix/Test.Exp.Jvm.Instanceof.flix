mod Test.Exp.Jvm.Instanceof {

    import java.lang.CharSequence
    import java.lang.Enum
    import java.lang.Long
    import java.lang.Number
    import java.lang.Object
    import java.lang.{String => JString}
    import java.lang.StringBuilder
    import java.math.BigInteger
    import java.nio.file.OpenOption
    import java.nio.file.StandardOpenOption

    @test
    def testInstanceof01(): Bool = "Hello World" instanceof JString

    @test
    def testInstanceof02(): Bool = "Hello World" instanceof CharSequence

    @test
    def testInstanceof03(): Bool = "Hello World" instanceof Object

    @test
    def testInstanceof04(): Bool = 100ii instanceof BigInteger

    @test
    def testInstanceof05(): Bool = 100ii instanceof Number

    @test
    def testInstanceof06(): Bool = 100ii instanceof Object

    @test
    def testInstanceof07(): Bool =
        StandardOpenOption.DELETE_ON_CLOSE instanceof StandardOpenOption

    @test
    def testInstanceof08(): Bool =
        StandardOpenOption.DELETE_ON_CLOSE instanceof OpenOption

    @test
    def testInstanceof09(): Bool =
        StandardOpenOption.DELETE_ON_CLOSE instanceof Enum

    @test
    def testInstanceof10(): Bool =
        StandardOpenOption.DELETE_ON_CLOSE instanceof Object

    @test
    def testInstanceof11(): Bool = (100ii instanceof Long) == false

    @test
    def testInstanceof12(): Bool = ("Hello World" instanceof StringBuilder) == false

    @test
    def testInstanceof13(): Bool \ IO = {
        let arr = Array#{"hi"} @ Static;
        Array.get(0, arr) instanceof JString
    }

    @test
    def testInstanceOf14(): Bool  = (123 instanceof Number) == false

    @test
    def testInstanceOf15(): Bool = (true instanceof Object) == false
}
