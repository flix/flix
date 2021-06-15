namespace Test/Exp/ScopedReference {

    @test
    def testScopedRef01(): Unit = // Pure
        let region r;
        let _u = ref () @ r;
        ()

    @test
    def testScopedRef02(): Unit = // Pure
        let region r;
        let _u = ref true @ r;
        ()

    @test
    def testScopedRef03(): Unit = // Pure
        let region r;
        let _u = ref 'a' @ r;
        ()


    @test
    def testScopedDeref01(): Bool = // Pure
        let region r;
        let x = ref () @ r;
        deref x == ()

    @test
    def testScopedDeref02(): Bool = // Pure
        let region r;
        let x = ref true @ r;
        deref x == true

    @test
    def testScopedDeref03(): Bool = // Pure
        let region r;
        let x = ref 'a' @ r;
        deref x == 'a'

}
