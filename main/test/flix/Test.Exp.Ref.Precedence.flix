mod Test.Exp.Ref.Precedence {

    @test
    def testRefPrecedence01(): Bool = region reg {
        let l = Ref.fresh(reg, Ref.fresh(reg, 123));
        let r = deref deref l;
        r == 123
    }

    @test
    def testRefPrecedence02(): Bool = region reg {
        let l = Ref.fresh(reg, Ref.fresh(reg, Ref.fresh(reg, 123)));
        let r = deref deref deref l;
        r == 123
    }

    @test
    def testRefPrecedence03(): Bool = region reg {
        let x = Ref.fresh(reg, 123);
        let y = Ref.fresh(reg, 123);
        let r = deref x + deref y;
        r == (123 + 123)
    }
}
