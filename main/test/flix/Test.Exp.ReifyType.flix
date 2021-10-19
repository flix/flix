namespace Test/Exp/ReifyType {

    @test
    def reify01(): Bool = reifyType Bool == ReifiedBool

    @test
    def reify02(): Bool = reflect(true) == ReifiedBool

    @test
    def reify03(): Bool = reifyType Char== ReifiedBool

    @test
    def reify04(): Bool = reflect('a')== ReifiedBool

    // Look into `Prelude.flix` for the full types available in `ReifiedType`.

    def reflect(x: a): ReifiedType = reifyType a

}
