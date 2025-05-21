mod Test.Exp.ExtMatch {

    @test
    def testExtMatch01Int32(): Bool = {
        ematch (xvar A(1)) {
            case A(x) => x == 1
            case A(_) => false
        }
    }

    @test
    def testExtMatch02Int32(): Bool = {
        ematch xvar A(1) {
            case A(x) => x == 1
            case A(_) => false
        }
    }

}
