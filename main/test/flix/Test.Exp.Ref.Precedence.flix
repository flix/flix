mod Test.Exp.Ref.Precedence {

    @test
    def testRefPrecedence01(): Bool = region reg {
        let l = Ref.fresh(reg, Ref.fresh(reg, 123));
        let r = Ref.get(Ref.get(l));
        r == 123
    }

    @test
    def testRefPrecedence02(): Bool = region reg {
        let l = Ref.fresh(reg, Ref.fresh(reg, Ref.fresh(reg, 123)));
        let r = Ref.get(Ref.get(Ref.get(l)));
        r == 123
    }

    @test
    def testRefPrecedence03(): Bool = region reg {
        let x = Ref.fresh(reg, 123);
        let y = Ref.fresh(reg, 123);
        let r = Ref.get(x) + Ref.get(y);
        r == (123 + 123)
    }
}
