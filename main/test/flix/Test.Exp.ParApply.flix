namespace Test/Exp/ParApply {

    @test
    def parApply01(): Bool =
        let f = _ -> _ -> _ -> true;
        par f(1, 2, "s")

}
