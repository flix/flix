mod Test.Eff.Poly.Int64 {

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

    def fail(): Int64 \ Result[String, Int64] =
        Result.fail("boom")

    @Test
    def testAskTellZeroShot(): Unit \ Assert =
        let result = run AskTell.askTell(42i64) with handler AskTell {
            def askTell(x, _k) = if (x == 42i64) 43i64 else -1i64
        };
        assertEq(expected = 43i64, result)

    @Test
    def testAskTellOneShot(): Unit \ Assert =
        let result = run AskTell.askTell(42i64) with handler AskTell {
            def askTell(x, k) = if (x == 42i64) k(43i64) else k(-1i64)
        };
        assertEq(expected = 43i64, result)

    @Test
    def testAskTellMultiShot(): Unit \ Assert =
        let result = run AskTell.askTell(42i64) with handler AskTell {
            def askTell(x, k) =
                if (x == 42i64) {
                    discard k(43i64);
                    k(44i64)
                } else k(-1i64)
        };
        assertEq(expected = 44i64, result)

    @Test
    def testStateZeroShot(): Unit \ Assert =
        let result = run {
            State.put(42i64);
            State.get()
        } with handler State {
            def get(k) = k(43i64)
            def put(x, _k) = if (x == 42i64) 43i64 else -1i64
        };
        assertEq(expected = 43i64, result)

    @Test
    def testStateOneShot(): Unit \ Assert =
        let result = run {
            State.put(42i64);
            State.get()
        } with handler State {
            def get(k) = k(43i64)
            def put(x, k) = if (x == 42i64) k(()) else -1i64
        };
        assertEq(expected = 43i64, result)

    @Test
    def testStateMultiShot(): Unit \ Assert =
        let result = run {
            State.put(42i64);
            State.get()
        } with handler State {
            def get(k) = k(43i64)
            def put(x, k) =
                if (x == 42i64) {
                    discard k(());
                    k(())
                } else -1i64
        };
        assertEq(expected = 43i64, result)

    @Test
    def testResultZeroShot(): Unit \ Assert =
        let result = run fail() with handler Result {
            def fail(error, _k) = if (error == "boom") 43i64 else -1i64
        };
        assertEq(expected = 43i64, result)

    @Test
    def testResultOneShot(): Unit \ Assert =
        let result = run fail() with handler Result {
            def fail(error, k) = if (error == "boom") k(43i64) else k(-1i64)
        };
        assertEq(expected = 43i64, result)

    @Test
    def testResultMultiShot(): Unit \ Assert =
        let result = run fail() with handler Result {
            def fail(error, k) =
                if (error == "boom") {
                    discard k(43i64);
                    k(44i64)
                } else k(-1i64)
        };
        assertEq(expected = 44i64, result)

}

