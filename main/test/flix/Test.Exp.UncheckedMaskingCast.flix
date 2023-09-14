mod Test.Exp.UncheckedMaskingCast {

    @Test
    def testMask01(): Bool = masked_cast(true)

    @Test
    def testMask02(): Bool = masked_cast(deref (ref true @ Static))

    @Test
    def testMask03(): Bool = region rc {
        let r = ref false @ rc;
        masked_cast(r := true);
        deref r
    }

}
