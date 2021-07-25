namespace Test/Def/Scoped {

    pub def oneScopedParam(scoped x: Int): Int = x - 1

    pub def twoScopedParams(scoped x: Int, scoped y: Int): Int = x + y

    @test
    def empty(): Bool = true
}