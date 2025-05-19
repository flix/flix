mod Test.Exp.ExtMatch {

    @test
    def testExtMatch01Int32(): Bool = {
        ematch (XVar A(1)) {
            case A(x) => x == 1
        }
    }

}
