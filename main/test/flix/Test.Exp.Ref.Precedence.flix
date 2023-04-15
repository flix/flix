mod Test.Exp.Ref.Precedence {

    @test
    def testRefPrecedence01(): Bool = region reg {
        let l = ref ref 123 @ reg @ reg;
        let r = deref deref l;
        r == 123
    }

    @test
    def testRefPrecedence02(): Bool = region reg {
        let l = ref ref ref 123 @ reg @ reg @ reg;
        let r = deref deref deref l;
        r == 123
    }

    @test
    def testRefPrecedence03(): Bool = region reg {
        let x = ref 123 @ reg;
        let y = ref 123 @ reg;
        let r = deref x + deref y;
        r == (123 + 123)
    }
}
