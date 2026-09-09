mod Test.Eff.Poly.MultiInstantiation {

    use Assert.assertEq

    eff AskTell[t] {
        def askTell(x: t): t
    }

    def askTwice(x: t): t \ AskTell[t] =
        AskTell.askTell(AskTell.askTell(x))

    def runAskTell(response: t, f: Unit -> t \ AskTell[t]): t =
        run f() with handler AskTell {
            def askTell(_x, k) = k(response)
        }

    @Test
    def testPolymorphicFunctionInt32(): Unit \ Assert =
        let result = run askTwice(40) with handler AskTell {
            def askTell(x, k) = k(x + 1)
        };
        assertEq(expected = 42, result)

    @Test
    def testPolymorphicFunctionString(): Unit \ Assert =
        let result = run askTwice("a") with handler AskTell {
            def askTell(x, k) = k("${x}b")
        };
        assertEq(expected = "abb", result)

    @Test
    def testPolymorphicHandlerInt32(): Unit \ Assert =
        let result = runAskTell(42, () -> AskTell.askTell(0));
        assertEq(expected = 42, result)

    @Test
    def testPolymorphicHandlerString(): Unit \ Assert =
        let result = runAskTell("ok", () -> AskTell.askTell("ignored"));
        assertEq(expected = "ok", result)

}
