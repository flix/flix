mod Test.Exp.UncheckedMaskingCast {

    @test
    def testMask01(): Bool = masked_cast(true)

    @test
    def testMask02(): Bool = masked_cast(Ref.get(Ref.fresh(Static, true)))

    @test
    def testMask03(): Bool = region rc {
        let r = Ref.fresh(rc, false);
        masked_cast(Ref.put(true, r));
        Ref.get(r)
    }

}
