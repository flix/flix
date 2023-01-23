namespace Test/Exp/Mask {

    @test
    def testMask01(): Bool = $MASK$(true)

    @test
    def testMask02(): Bool = $MASK$(deref (ref true))

    @test
    def testMask03(): Bool = region rh {
        let r = ref false @ rh;
        $MASK$(r := true);
        deref r
    }

}
