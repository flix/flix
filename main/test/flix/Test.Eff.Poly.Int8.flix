mod Test.Eff.Poly.Int8 {

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

    def fail(): Int8 \ Result[String, Int8] =
        Result.fail("boom")

    @Test
    def testAskTellZeroShot(): Unit \ Assert =
        let result = run AskTell.askTell(42i8) with handler AskTell {
            def askTell(x, _k) = if (x == 42i8) 43i8 else -1i8
        };
        assertEq(expected = 43i8, result)

    @Test
    def testAskTellOneShot(): Unit \ Assert =
        let result = run AskTell.askTell(42i8) with handler AskTell {
            def askTell(x, k) = if (x == 42i8) k(43i8) else k(-1i8)
        };
        assertEq(expected = 43i8, result)

    @Test
    def testAskTellMultiShot(): Unit \ Assert =
        let result = run AskTell.askTell(42i8) with handler AskTell {
            def askTell(x, k) =
                if (x == 42i8) {
                    discard k(43i8);
                    k(44i8)
                } else k(-1i8)
        };
        assertEq(expected = 44i8, result)

    @Test
    def testStateZeroShot(): Unit \ Assert =
        let result = run {
            State.put(42i8);
            State.get()
        } with handler State {
            def get(k) = k(43i8)
            def put(x, _k) = if (x == 42i8) 43i8 else -1i8
        };
        assertEq(expected = 43i8, result)

    @Test
    def testStateOneShot(): Unit \ Assert =
        let result = run {
            State.put(42i8);
            State.get()
        } with handler State {
            def get(k) = k(43i8)
            def put(x, k) = if (x == 42i8) k(()) else -1i8
        };
        assertEq(expected = 43i8, result)

    @Test
    def testStateMultiShot(): Unit \ Assert =
        let result = run {
            State.put(42i8);
            State.get()
        } with handler State {
            def get(k) = k(43i8)
            def put(x, k) =
                if (x == 42i8) {
                    discard k(());
                    k(())
                } else -1i8
        };
        assertEq(expected = 43i8, result)

    @Test
    def testResultZeroShot(): Unit \ Assert =
        let result = run fail() with handler Result {
            def fail(error, _k) = if (error == "boom") 43i8 else -1i8
        };
        assertEq(expected = 43i8, result)

    @Test
    def testResultOneShot(): Unit \ Assert =
        let result = run fail() with handler Result {
            def fail(error, k) = if (error == "boom") k(43i8) else k(-1i8)
        };
        assertEq(expected = 43i8, result)

    @Test
    def testResultMultiShot(): Unit \ Assert =
        let result = run fail() with handler Result {
            def fail(error, k) =
                if (error == "boom") {
                    discard k(43i8);
                    k(44i8)
                } else k(-1i8)
        };
        assertEq(expected = 44i8, result)

}

