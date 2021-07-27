namespace Test/Def/Scoped {

    pub def binaryLosesScopedness(scoped x: Int): Int = x + 1

    @test
    def empty(): Bool = true
}