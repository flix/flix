namespace Test/Exp/Mask {

    @test
    def testMask01(): Bool = $MASK$(true)

    @test
    def testMask02(): Bool = $MASK$(deref (ref true))

    @test
    def testMask03(): Bool \ IO = {
        let r = ref false;
        $MASK$(r := true);
        deref r
    }

}
