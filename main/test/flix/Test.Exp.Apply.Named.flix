mod Test.Exp.Apply.Named {

    use Assert.assertEq

    @Test
    def testNamedParam01(): Unit \ Assert =
        assertEq(expected = true, named(param1 = true))

    @Test
    def testNamedParam02(): Unit \ Assert =
        assertEq(expected = true, namedNamed(param1 = true, param2 = true))

    @Test
    def testNamedParam03(): Unit \ Assert =
        assertEq(expected = true, namedUnnamed(param1 = true, true))

    @Test
    def testNamedParam04(): Unit \ Assert =
        assertEq(expected = true, unnamedNamed(true, param2 = true))

    @Test
    def testNamedParam05(): Unit \ Assert =
        let namedLambda = p1 -> p1#param1;
        assertEq(expected = true, namedLambda(param1 = true))

    @Test
    def testNamedParam06(): Unit \ Assert =
        let namedNamedLambda = (p1, _p2) -> p1#param1;
        assertEq(expected = true, namedNamedLambda(param1 = true, param2 = true))

    @Test
    def testNamedParam07(): Unit \ Assert =
        let namedUnnamedLambda = (p1, _p2) -> p1#param1;
        assertEq(expected = true, namedUnnamedLambda(param1 = true, true))

    @Test
    def testNamedParam08(): Unit \ Assert =
        let unnamedNamedLambda = (p1, _p2) -> p1;
        assertEq(expected = true, unnamedNamedLambda(true, param2 = true))

    def named(p1: {param1 = Bool}): Bool = p1#param1

    def namedNamed(p1: {param1 = Bool}, _p2: {param2 = Bool}): Bool = p1#param1

    def namedUnnamed(p1: {param1 = Bool}, _p2: Bool): Bool = p1#param1

    def unnamedNamed(p1: Bool, _p2: {param2 = Bool}): Bool = p1

}
