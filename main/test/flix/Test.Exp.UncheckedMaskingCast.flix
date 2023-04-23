mod Test.Exp.UncheckedMaskingCast {

    @test
    def testMask01(): Bool = masked_cast(true)

    @test
    def testMask02(): Bool = masked_cast(deref (ref true @ Static))

    @test
    def testMask03(): Bool = region rh {
        let r = ref false @ rh;
        masked_cast(r := true);
        deref r
    }

}
