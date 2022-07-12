namespace Test/Exp/Reify {

    @test
    def reify01(): Bool = hof(_ -> 123)

    @test
    def reify02(): Bool = not hof(_ -> 123 as & Impure)

    @test
    def reify03(): Bool = hof((x -> x) >> (y -> y))

    @test
    def reify04(): Bool = not hof((x -> x) >> (y -> y as & Impure))

    @test
    def reify05(): Bool = not hof((x -> x as & Impure) >> (y -> y))

    @test
    def reify06(): Bool = not hof((x -> x as & Impure) >> (y -> y as & Impure))

    def hof(_f: a -> b & ef): Bool = reify ef

    def >>(f: a -> b & ef1, g: b -> c & ef2): a -> c \ { ef1, ef2 } = x -> g(f(x))

}
