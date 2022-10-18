namespace Test/Java/Function {
    import java.util.function.IntFunction
    import java.lang.Object

    @test
    def testIntFunction(): Bool = takesIntFunction((_: Int32) -> newObject())

    def takesIntFunction(_f: IntFunction): Bool = true

    def newObject(): Object \ IO = {
        import new java.lang.Object(): Object \ IO as mk;
        mk()
    }
}
