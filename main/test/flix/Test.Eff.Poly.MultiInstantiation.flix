mod Test.Eff.Poly.MultiInstantiation {

    use Assert.assertEq

    eff AskTell[t] {
        def askTell(x: t): t
    }

    eff EqAskTell[t] {
        def askTell(x: t): t with Eq[t]
    }

    def askTwice(x: t): t \ AskTell[t] =
        AskTell.askTell(AskTell.askTell(x))

    def eqAskTwice(x: t): t \ EqAskTell[t] with Eq[t] =
        EqAskTell.askTell(EqAskTell.askTell(x))

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
    def testEqBoundInt32(): Unit \ Assert =
        let result = run eqAskTwice(40) with handler EqAskTell {
            def askTell(x, k) = if (x == 40 or x == 41) k(x + 1) else k(x)
        };
        assertEq(expected = 42, result)

    @Test
    def testEqBoundString(): Unit \ Assert =
        let result = run eqAskTwice("a") with handler EqAskTell {
            def askTell(x, k) = if (x == "a" or x == "ab") k("${x}b") else k(x)
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
