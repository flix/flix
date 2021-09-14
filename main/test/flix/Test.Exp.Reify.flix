namespace Test/Exp/Reify {

    @test
    def reify01(): Bool = hof(_ -> 123 as & Pure)

    @test
    def reify02(): Bool = not hof(_ -> 123 as & Impure)

    def hof(_f: a -> b & ef): Bool = reify ef

}
