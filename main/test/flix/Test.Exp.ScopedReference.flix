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


    @test
    def testScopedAssign01(): Bool = // Pure
        let region r;
        let x = ref () @ r;
        x := ();
        deref x == ()

    @test
    def testScopedAssign02(): Bool = // Pure
        let region r;
        let x = ref true @ r;
        x := false;
        deref x == false

    @test
    def testScopedAssign03(): Bool = // Pure
        let region r;
        let x = ref 'a' @ r;
        x := 'b';
        deref x == 'b'


    @test
    def testMultipleRegions01(): Bool = // Pure
        let region r1;
        let region r2;
        let x = ref 1 @ r1;
        let y = ref 2 @ r2;
        (deref x + deref y) == 3


    @test
    def testCapturedRegions01(): Bool = // Pure
        let region r1;
        let region r2;
        let x = ref 1 @ r1;
        let y = ref 2 @ r2;
        let f = () -> x := 7;
        let g = () -> y := 8;
        f();
        g();
        (deref x + deref y) == 15

}
