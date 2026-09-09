mod Test.Eff.Poly.Unit {

    use Assert.assertEq

    eff AskTell[t] {
        def askTell(x: t): t
    }

    eff State[s] {
        def get(): s
        def put(x: s): Unit
    }

    eff Result[e, a] {
        def fail(error: e): a
    }

    def fail(): Unit \ Result[String, Unit] =
        Result.fail("boom")

    @Test
    def testAskTellZeroShot(): Unit \ Assert =
        let result = run AskTell.askTell(()) with handler AskTell {
            def askTell(x, _k) = if (x == ()) () else ()
        };
        assertEq(expected = (), result)

    @Test
    def testAskTellOneShot(): Unit \ Assert =
        let result = run AskTell.askTell(()) with handler AskTell {
            def askTell(x, k) = if (x == ()) k(()) else k(())
        };
        assertEq(expected = (), result)

    @Test
    def testAskTellMultiShot(): Unit \ Assert =
        let result = run AskTell.askTell(()) with handler AskTell {
            def askTell(x, k) =
                if (x == () and k(()) == () and k(()) == ()) () else unreachable!()
        };
        assertEq(expected = (), result)

    @Test
    def testStateZeroShot(): Unit \ Assert =
        let result = run {
            State.put(());
            State.get()
        } with handler State {
            def get(k) = k(())
            def put(x, _k) = if (x == ()) () else ()
        };
        assertEq(expected = (), result)

    @Test
    def testStateOneShot(): Unit \ Assert =
        let result = run {
            State.put(());
            State.get()
        } with handler State {
            def get(k) = k(())
            def put(x, k) = if (x == ()) k(()) else ()
        };
        assertEq(expected = (), result)

    @Test
    def testStateMultiShot(): Unit \ Assert =
        let result = run {
            State.put(());
            State.get()
        } with handler State {
            def get(k) = k(())
            def put(x, k) =
                if (x == () and k(()) == () and k(()) == ()) () else unreachable!()
        };
        assertEq(expected = (), result)

    @Test
    def testResultZeroShot(): Unit \ Assert =
        let result = run fail() with handler Result {
            def fail(error, _k) = if (error == "boom") () else ()
        };
        assertEq(expected = (), result)

    @Test
    def testResultOneShot(): Unit \ Assert =
        let result = run fail() with handler Result {
            def fail(error, k) = if (error == "boom") k(()) else k(())
        };
        assertEq(expected = (), result)

    @Test
    def testResultMultiShot(): Unit \ Assert =
        let result = run fail() with handler Result {
            def fail(error, k) =
                if (error == "boom" and k(()) == () and k(()) == ()) () else unreachable!()
        };
        assertEq(expected = (), result)

}
