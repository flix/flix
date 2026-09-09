mod Test.Eff.Poly.Enum {

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

    def fail(): Option[Int32] \ Result[String, Option[Int32]] =
        Result.fail("boom")

    @Test
    def testAskTellZeroShot(): Unit \ Assert =
        let result = run AskTell.askTell(Some(42)) with handler AskTell {
            def askTell(x, _k) = if (x == Some(42)) Some(43) else None
        };
        assertEq(expected = Some(43), result)

    @Test
    def testAskTellOneShot(): Unit \ Assert =
        let result = run AskTell.askTell(Some(42)) with handler AskTell {
            def askTell(x, k) = if (x == Some(42)) k(Some(43)) else k(None)
        };
        assertEq(expected = Some(43), result)

    @Test
    def testAskTellMultiShot(): Unit \ Assert =
        let result = run AskTell.askTell(Some(42)) with handler AskTell {
            def askTell(x, k) =
                if (x == Some(42)) {
                    discard k(Some(43));
                    k(Some(44))
                } else k(None)
        };
        assertEq(expected = Some(44), result)

    @Test
    def testStateZeroShot(): Unit \ Assert =
        let result = run {
            State.put(Some(42));
            State.get()
        } with handler State {
            def get(k) = k(Some(43))
            def put(x, _k) = if (x == Some(42)) Some(43) else None
        };
        assertEq(expected = Some(43), result)

    @Test
    def testStateOneShot(): Unit \ Assert =
        let result = run {
            State.put(Some(42));
            State.get()
        } with handler State {
            def get(k) = k(Some(43))
            def put(x, k) = if (x == Some(42)) k(()) else None
        };
        assertEq(expected = Some(43), result)

    @Test
    def testStateMultiShot(): Unit \ Assert =
        let result = run {
            State.put(Some(42));
            State.get()
        } with handler State {
            def get(k) = k(Some(43))
            def put(x, k) =
                if (x == Some(42)) {
                    discard k(());
                    k(())
                } else None
        };
        assertEq(expected = Some(43), result)

    @Test
    def testResultZeroShot(): Unit \ Assert =
        let result = run fail() with handler Result {
            def fail(error, _k) = if (error == "boom") Some(43) else None
        };
        assertEq(expected = Some(43), result)

    @Test
    def testResultOneShot(): Unit \ Assert =
        let result = run fail() with handler Result {
            def fail(error, k) = if (error == "boom") k(Some(43)) else k(None)
        };
        assertEq(expected = Some(43), result)

    @Test
    def testResultMultiShot(): Unit \ Assert =
        let result = run fail() with handler Result {
            def fail(error, k) =
                if (error == "boom") {
                    discard k(Some(43));
                    k(Some(44))
                } else k(None)
        };
        assertEq(expected = Some(44), result)

}

