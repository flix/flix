namespace Test/Exp/ParApply {

    @test
    def parApply01(): Bool =
        let f = _ -> _ -> _ -> true;
        par f(1, 2, "s")

    @test
    def parApply02(): Bool =
        let r = { f = _ -> _ -> _ -> true };
        r.f(2, 1, "s")

}
