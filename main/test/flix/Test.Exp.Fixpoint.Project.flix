namespace Test/Exp/Fixpoint/Project {

    @test
    def testProject01(): Bool =
        let f = #{ A(1). B(1). C(1). };
        let b1 = (project A f) |= A(1).;
        let b2 = (project A f) |= B(1).;
        let b3 = (project A f) |= C(1).;
        b1 and not b2 and not b3

    @test
    def testProject02(): Bool =
        let f = #{ A(1). B(1). C(1). };
        let b1 = (project B f) |= A(1).;
        let b2 = (project B f) |= B(1).;
        let b3 = (project B f) |= C(1).;
        not b1 and b2 and not b3

    @test
    def testProject03(): Bool =
        let f = #{ A(1). B(1). C(1). };
        let b1 = (project C f) |= A(1).;
        let b2 = (project C f) |= B(1).;
        let b3 = (project C f) |= C(1).;
        not b1 and not b2 and b3

    @test
    def testProjectCompose01(): Bool =
        let f1 = #{ A(1). B(1). C(1). };
        let f2 = project A f1;
        let f3 = f2 <+> #{ B("x"). C("y"). };
        f3 |= #{ A(1). B("x"). C("y"). }

    @test
    def testProjectCompose02(): Bool =
        let f1 = #{ A(1). B(1). C(1). };
        let f2 = project B f1;
        let f3 = f2 <+> #{ A("x"). C("y"). };
        f3 |= #{ B(1). A("x"). C("y"). }

    @test
    def testProjectCompose03(): Bool =
        let f1 = #{ A(1). B(1). C(1). };
        let f2 = project C f1;
        let f3 = f2 <+> #{ A("x"). B("y"). };
        f3 |= #{ C(1). A("x"). B("y"). }

}
