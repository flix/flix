mod Test.Eff.Poly.String {

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

    def fail(): String \ Result[Int32, String] =
        Result.fail(42)

    @Test
    def testAskTellZeroShot(): Unit \ Assert =
        let result = run AskTell.askTell("hello") with handler AskTell {
            def askTell(x, _k) = if (x == "hello") "world" else "unexpected"
        };
        assertEq(expected = "world", result)

    @Test
    def testAskTellOneShot(): Unit \ Assert =
        let result = run AskTell.askTell("hello") with handler AskTell {
            def askTell(x, k) = if (x == "hello") k("world") else k("unexpected")
        };
        assertEq(expected = "world", result)

    @Test
    def testAskTellMultiShot(): Unit \ Assert =
        let result = run AskTell.askTell("hello") with handler AskTell {
            def askTell(x, k) =
                if (x == "hello") {
                    discard k("world");
                    k("done")
                } else k("unexpected")
        };
        assertEq(expected = "done", result)

    @Test
    def testStateZeroShot(): Unit \ Assert =
        let result = run {
            State.put("hello");
            State.get()
        } with handler State {
            def get(k) = k("world")
            def put(x, _k) = if (x == "hello") "world" else "unexpected"
        };
        assertEq(expected = "world", result)

    @Test
    def testStateOneShot(): Unit \ Assert =
        let result = run {
            State.put("hello");
            State.get()
        } with handler State {
            def get(k) = k("world")
            def put(x, k) = if (x == "hello") k(()) else "unexpected"
        };
        assertEq(expected = "world", result)

    @Test
    def testStateMultiShot(): Unit \ Assert =
        let result = run {
            State.put("hello");
            State.get()
        } with handler State {
            def get(k) = k("world")
            def put(x, k) =
                if (x == "hello") {
                    discard k(());
                    k(())
                } else "unexpected"
        };
        assertEq(expected = "world", result)

    @Test
    def testResultZeroShot(): Unit \ Assert =
        let result = run fail() with handler Result {
            def fail(error, _k) = if (error == 42) "world" else "unexpected"
        };
        assertEq(expected = "world", result)

    @Test
    def testResultOneShot(): Unit \ Assert =
        let result = run fail() with handler Result {
            def fail(error, k) = if (error == 42) k("world") else k("unexpected")
        };
        assertEq(expected = "world", result)

    @Test
    def testResultMultiShot(): Unit \ Assert =
        let result = run fail() with handler Result {
            def fail(error, k) =
                if (error == 42) {
                    discard k("world");
                    k("done")
                } else k("unexpected")
        };
        assertEq(expected = "done", result)

}

