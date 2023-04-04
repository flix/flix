mod Test.Exp.Apply.Named {
    def named(p1: {param1 = Bool}): Bool = p1.param1

    def namedNamed(p1: {param1 = Bool}, _p2: {param2 = Bool}): Bool = p1.param1

    def namedUnnamed(p1: {param1 = Bool}, _p2: Bool): Bool = p1.param1

    def unnamedNamed(p1: Bool, _p2: {param2 = Bool}): Bool = p1

    @test
    def testNamedParam01(): Bool = named(param1 = true)

    @test
    def testNamedParam02(): Bool = namedNamed(param1 = true, param2 = true)

    @test
    def testNamedParam03(): Bool = namedUnnamed(param1 = true, true)

    @test
    def testNamedParam04(): Bool = unnamedNamed(true, param2 = true)

    @test
    def testNamedParam05(): Bool =
        let namedLambda = p1 -> p1.param1;
        namedLambda(param1 = true)

    @test
    def testNamedParam06(): Bool =
        let namedNamedLambda = (p1, _p2) -> p1.param1;
        namedNamedLambda(param1 = true, param2 = true)

    @test
    def testNamedParam07(): Bool =
        let namedUnnamedLambda = (p1, _p2) -> p1.param1;
        namedUnnamedLambda(param1 = true, true)

    @test
    def testNamedParam08(): Bool =
        let unnamedNamedLambda = (p1, _p2) -> p1;
        unnamedNamedLambda(true, param2 = true)
}
