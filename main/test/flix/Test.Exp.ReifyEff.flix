namespace Test/Exp/ReifyEff {

    @test
    def reifyEff01(): Bool = hof(_ -> 123)

    @test
    def reify02(): Bool = not hof(_ -> 123 as & Impure)

    @test
    def reify03(): Bool = not hof(_ -> 123 as \ E)

    @test
    def reify04(): Bool = hof((x -> x) >> (y -> y))

    @test
    def reify05(): Bool = not hof((x -> x) >> (y -> y as & Impure))

    @test
    def reify06(): Bool = not hof((x -> x as & Impure) >> (y -> y))

    @test
    def reify07(): Bool = not hof((x -> x as & Impure) >> (y -> y as & Impure))

    @test
    def reify08(): Bool = not hof((x -> x) >> (y -> y as \ E))

    @test
    def reify09(): Bool = not hof((x -> x as \ E) >> (y -> y))

    @test
    def reify10(): Bool = not hof((x -> x as \ E) >> (y -> y as \ E))

    @test
    def reify11(): Bool = not hof((x -> x as \ E) >> (y -> y as & Impure))

    @test
    def reify12(): Bool = not hof((x -> x as & Impure) >> (y -> y as \ E))

    def hof(f: a -> b & ef): Bool = reifyEff(f) {
        case Pure(_) => true
        case _ => false
    }

    def >>(f: a -> b & ef1, g: b -> c & ef2): a -> c \ { ef1, ef2 } = x -> g(f(x))

    eff E
}
