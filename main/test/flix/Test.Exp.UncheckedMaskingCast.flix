mod Test.Exp.UncheckedMaskingCast {

    @test
    def testMask01(): Bool = masked_cast(true)

    @test
    def testMask02(): Bool = masked_cast(deref (ref true @ Static))

    @test
    def testMask03(): Bool = region rc {
        let r = ref false @ rc;
        masked_cast(r := true);
        deref r
    }

}
