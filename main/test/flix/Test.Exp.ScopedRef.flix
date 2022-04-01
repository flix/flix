namespace Test/Exp/ScopedRef {

    @test
    def testScopedDeref01(): Bool = // Pure
        region r {
            let x = ref () @ r;
            deref x == ()
        }

    @test
    def testScopedDeref02(): Bool = // Pure
        region r {
            let x = ref true @ r;
            deref x == true
        }

    @test
    def testScopedDeref03(): Bool = // Pure
        region r {
            let x = ref 'a' @ r;
            deref x == 'a'
        }

    @test
    def testScopedAssign01(): Bool = // Pure
        region r {
            let x = ref () @ r;
            x := ();
            deref x == ()
        }

    @test
    def testScopedAssign02(): Bool = // Pure
        region r {
            let x = ref true @ r;
            x := false;
            deref x == false
        }

    @test
    def testScopedAssign03(): Bool = // Pure
        region r {
            let x = ref 'a' @ r;
            x := 'b';
            deref x == 'b'
        }


}
