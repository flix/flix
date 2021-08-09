namespace Test/Def/Scoped {

    // this fails because Sub.sub is not scoped
//    pub def oneScopedParam(scoped x: Int): Int = x - 1

    // this fails because Add.add is not scoped
//    pub def twoScopedParams(scoped x: Int, scoped y: Int): Int = x + y

    @test
    def empty(): Bool = true
}