mod Test.Eff.Poly.Bool {

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

    def fail(): Bool \ Result[String, Bool] =
        Result.fail("boom")

    @Test
    def testAskTellZeroShot(): Unit \ Assert =
        let result = run AskTell.askTell(true) with handler AskTell {
            def askTell(x, _k) = if (x == true) false else false
        };
        assertEq(expected = false, result)

    @Test
    def testAskTellOneShot(): Unit \ Assert =
        let result = run AskTell.askTell(true) with handler AskTell {
            def askTell(x, k) = if (x == true) k(false) else k(false)
        };
        assertEq(expected = false, result)

    @Test
    def testAskTellMultiShot(): Unit \ Assert =
        let result = run AskTell.askTell(true) with handler AskTell {
            def askTell(x, k) =
                if (x == true) {
                    discard k(false);
                    k(true)
                } else k(false)
        };
        assertEq(expected = true, result)

    @Test
    def testStateZeroShot(): Unit \ Assert =
        let result = run {
            State.put(true);
            State.get()
        } with handler State {
            def get(k) = k(false)
            def put(x, _k) = if (x == true) false else false
        };
        assertEq(expected = false, result)

    @Test
    def testStateOneShot(): Unit \ Assert =
        let result = run {
            State.put(true);
            State.get()
        } with handler State {
            def get(k) = k(false)
            def put(x, k) = if (x == true) k(()) else false
        };
        assertEq(expected = false, result)

    @Test
    def testStateMultiShot(): Unit \ Assert =
        let result = run {
            State.put(true);
            State.get()
        } with handler State {
            def get(k) = k(false)
            def put(x, k) =
                if (x == true) {
                    discard k(());
                    k(())
                } else false
        };
        assertEq(expected = false, result)

    @Test
    def testResultZeroShot(): Unit \ Assert =
        let result = run fail() with handler Result {
            def fail(error, _k) = if (error == "boom") false else false
        };
        assertEq(expected = false, result)

    @Test
    def testResultOneShot(): Unit \ Assert =
        let result = run fail() with handler Result {
            def fail(error, k) = if (error == "boom") k(false) else k(false)
        };
        assertEq(expected = false, result)

    @Test
    def testResultMultiShot(): Unit \ Assert =
        let result = run fail() with handler Result {
            def fail(error, k) =
                if (error == "boom") {
                    discard k(false);
                    k(true)
                } else k(false)
        };
        assertEq(expected = true, result)

}

