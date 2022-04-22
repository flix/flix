namespace Test/Exp/Ref/Precedence {

    @test
    def testRefPrecedence01(): Bool = region r {
        let l = ref ref 123 @ r @ r;
        let r = deref deref l;
        r == 123
    }

    @test
    def testRefPrecedence02(): Bool = region r {
        let l = ref ref ref 123 @ r @ r @ r;
        let r = deref deref deref l;
        r == 123
    }

    @test
    def testRefPrecedence03(): Bool = region r {
        let x = ref 123 @ r;
        let y = ref 123 @ r;
        let r = deref x + deref y;
        r == (123 + 123)
    }
}
