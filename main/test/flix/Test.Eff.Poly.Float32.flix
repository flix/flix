mod Test.Eff.Poly.Float32 {

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

    def fail(): Float32 \ Result[String, Float32] =
        Result.fail("boom")

    @Test
    def testAskTellZeroShot(): Unit \ Assert =
        let result = run AskTell.askTell(42.0f32) with handler AskTell {
            def askTell(x, _k) = if (x == 42.0f32) 43.0f32 else -1.0f32
        };
        assertEq(expected = 43.0f32, result)

    @Test
    def testAskTellOneShot(): Unit \ Assert =
        let result = run AskTell.askTell(42.0f32) with handler AskTell {
            def askTell(x, k) = if (x == 42.0f32) k(43.0f32) else k(-1.0f32)
        };
        assertEq(expected = 43.0f32, result)

    @Test
    def testAskTellMultiShot(): Unit \ Assert =
        let result = run AskTell.askTell(42.0f32) with handler AskTell {
            def askTell(x, k) =
                if (x == 42.0f32) {
                    discard k(43.0f32);
                    k(44.0f32)
                } else k(-1.0f32)
        };
        assertEq(expected = 44.0f32, result)

    @Test
    def testStateZeroShot(): Unit \ Assert =
        let result = run {
            State.put(42.0f32);
            State.get()
        } with handler State {
            def get(k) = k(43.0f32)
            def put(x, _k) = if (x == 42.0f32) 43.0f32 else -1.0f32
        };
        assertEq(expected = 43.0f32, result)

    @Test
    def testStateOneShot(): Unit \ Assert =
        let result = run {
            State.put(42.0f32);
            State.get()
        } with handler State {
            def get(k) = k(43.0f32)
            def put(x, k) = if (x == 42.0f32) k(()) else -1.0f32
        };
        assertEq(expected = 43.0f32, result)

    @Test
    def testStateMultiShot(): Unit \ Assert =
        let result = run {
            State.put(42.0f32);
            State.get()
        } with handler State {
            def get(k) = k(43.0f32)
            def put(x, k) =
                if (x == 42.0f32) {
                    discard k(());
                    k(())
                } else -1.0f32
        };
        assertEq(expected = 43.0f32, result)

    @Test
    def testResultZeroShot(): Unit \ Assert =
        let result = run fail() with handler Result {
            def fail(error, _k) = if (error == "boom") 43.0f32 else -1.0f32
        };
        assertEq(expected = 43.0f32, result)

    @Test
    def testResultOneShot(): Unit \ Assert =
        let result = run fail() with handler Result {
            def fail(error, k) = if (error == "boom") k(43.0f32) else k(-1.0f32)
        };
        assertEq(expected = 43.0f32, result)

    @Test
    def testResultMultiShot(): Unit \ Assert =
        let result = run fail() with handler Result {
            def fail(error, k) =
                if (error == "boom") {
                    discard k(43.0f32);
                    k(44.0f32)
                } else k(-1.0f32)
        };
        assertEq(expected = 44.0f32, result)

}

