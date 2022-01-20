namespace Test/Def/Scoped {

    pub def oneScopedParam(scoped x: Int32): Int32 = x - 1

    pub def twoScopedParams(scoped x: Int32, scoped y: Int32): Int32 = x + y

    @test
    def empty(): Bool = true
}
