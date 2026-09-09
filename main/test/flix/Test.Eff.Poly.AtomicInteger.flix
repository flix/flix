mod Test.Eff.Poly.AtomicInteger {

    use Assert.assertEq

    import java.util.concurrent.atomic.AtomicInteger

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

    def newAtomicInteger(n: Int32): AtomicInteger =
        unsafe IO { new AtomicInteger(n) }

    def toInt(n: AtomicInteger): Int32 =
        unsafe IO { n.intValue() }

    def fail(): AtomicInteger \ Result[String, AtomicInteger] =
        Result.fail("boom")

    @Test
    def testAskTellZeroShot(): Unit \ Assert =
        let result = run AskTell.askTell(newAtomicInteger(42)) with handler AskTell {
            def askTell(x, _k) = if (toInt(x) == 42) newAtomicInteger(43) else newAtomicInteger(-1)
        };
        assertEq(expected = 43, toInt(result))

    @Test
    def testAskTellOneShot(): Unit \ Assert =
        let result = run AskTell.askTell(newAtomicInteger(42)) with handler AskTell {
            def askTell(x, k) = if (toInt(x) == 42) k(newAtomicInteger(43)) else k(newAtomicInteger(-1))
        };
        assertEq(expected = 43, toInt(result))

    @Test
    def testAskTellMultiShot(): Unit \ Assert =
        let result = run AskTell.askTell(newAtomicInteger(42)) with handler AskTell {
            def askTell(x, k) =
                if (toInt(x) == 42) {
                    discard k(newAtomicInteger(43));
                    k(newAtomicInteger(44))
                } else k(newAtomicInteger(-1))
        };
        assertEq(expected = 44, toInt(result))

    @Test
    def testStateZeroShot(): Unit \ Assert =
        let result = run {
            State.put(newAtomicInteger(42));
            State.get()
        } with handler State {
            def get(k) = k(newAtomicInteger(43))
            def put(x, _k) = if (toInt(x) == 42) newAtomicInteger(43) else newAtomicInteger(-1)
        };
        assertEq(expected = 43, toInt(result))

    @Test
    def testStateOneShot(): Unit \ Assert =
        let result = run {
            State.put(newAtomicInteger(42));
            State.get()
        } with handler State {
            def get(k) = k(newAtomicInteger(43))
            def put(x, k) = if (toInt(x) == 42) k(()) else newAtomicInteger(-1)
        };
        assertEq(expected = 43, toInt(result))

    @Test
    def testStateMultiShot(): Unit \ Assert =
        let result = run {
            State.put(newAtomicInteger(42));
            State.get()
        } with handler State {
            def get(k) = k(newAtomicInteger(43))
            def put(x, k) =
                if (toInt(x) == 42) {
                    discard k(());
                    k(())
                } else newAtomicInteger(-1)
        };
        assertEq(expected = 43, toInt(result))

    @Test
    def testResultZeroShot(): Unit \ Assert =
        let result = run fail() with handler Result {
            def fail(error, _k) = if (error == "boom") newAtomicInteger(43) else newAtomicInteger(-1)
        };
        assertEq(expected = 43, toInt(result))

    @Test
    def testResultOneShot(): Unit \ Assert =
        let result = run fail() with handler Result {
            def fail(error, k) = if (error == "boom") k(newAtomicInteger(43)) else k(newAtomicInteger(-1))
        };
        assertEq(expected = 43, toInt(result))

    @Test
    def testResultMultiShot(): Unit \ Assert =
        let result = run fail() with handler Result {
            def fail(error, k) =
                if (error == "boom") {
                    discard k(newAtomicInteger(43));
                    k(newAtomicInteger(44))
                } else k(newAtomicInteger(-1))
        };
        assertEq(expected = 44, toInt(result))

}
