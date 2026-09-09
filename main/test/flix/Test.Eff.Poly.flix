mod Test.Eff.Poly {

    use Assert.assertEq

    import java.util.concurrent.atomic.AtomicInteger

    eff State[s] {
        def get(): s
        def put(x: s): Unit
    }

    eff AskTell[t] {
        def askTell(x: t): t
    }

    def newAtomicInteger(n: Int32): AtomicInteger =
        unsafe IO { new AtomicInteger(n) }

    def toInt(n: AtomicInteger): Int32 =
        unsafe IO { n.intValue() }

    @Test
    def testStateUnit(): Unit \ Assert =
        let result = run {
            State.put(());
            State.get()
        } with handler State {
            def get(k) = k(())
            def put(_x, k) = k(())
        };
        assertEq(expected = (), result)

    @Test
    def testStateBool(): Unit \ Assert =
        let result = run {
            State.put(true);
            State.get()
        } with handler State {
            def get(k) = k(false)
            def put(x, k) = if (x) k(()) else true
        };
        assertEq(expected = false, result)

    @Test
    def testStateChar(): Unit \ Assert =
        let result = run {
            State.put('a');
            State.get()
        } with handler State {
            def get(k) = k('b')
            def put(x, k) = if (x == 'a') k(()) else '?'
        };
        assertEq(expected = 'b', result)

    @Test
    def testStateFloat32(): Unit \ Assert =
        let result = run {
            State.put(42.0f32);
            State.get()
        } with handler State {
            def get(k) = k(123.0f32)
            def put(x, k) = if (x == 42.0f32) k(()) else -1.0f32
        };
        assertEq(expected = 123.0f32, result)

    @Test
    def testStateFloat64(): Unit \ Assert =
        let result = run {
            State.put(42.0f64);
            State.get()
        } with handler State {
            def get(k) = k(123.0f64)
            def put(x, k) = if (x == 42.0f64) k(()) else -1.0f64
        };
        assertEq(expected = 123.0f64, result)

    @Test
    def testStateInt8(): Unit \ Assert =
        let result = run {
            State.put(42i8);
            State.get()
        } with handler State {
            def get(k) = k(123i8)
            def put(x, k) = if (x == 42i8) k(()) else -1i8
        };
        assertEq(expected = 123i8, result)

    @Test
    def testStateInt16(): Unit \ Assert =
        let result = run {
            State.put(42i16);
            State.get()
        } with handler State {
            def get(k) = k(123i16)
            def put(x, k) = if (x == 42i16) k(()) else -1i16
        };
        assertEq(expected = 123i16, result)

    @Test
    def testStateInt32(): Unit \ Assert =
        let result = run {
            State.put(42);
            State.get()
        } with handler State {
            def get(k) = k(123)
            def put(x, k) = if (x == 42) k(()) else -1
        };
        assertEq(expected = 123, result)

    @Test
    def testStateInt64(): Unit \ Assert =
        let result = run {
            State.put(42i64);
            State.get()
        } with handler State {
            def get(k) = k(123i64)
            def put(x, k) = if (x == 42i64) k(()) else -1i64
        };
        assertEq(expected = 123i64, result)

    @Test
    def testStateString(): Unit \ Assert =
        let result = run {
            State.put("hello");
            State.get()
        } with handler State {
            def get(k) = k("world")
            def put(x, k) = if (x == "hello") k(()) else "unexpected"
        };
        assertEq(expected = "world", result)

    @Test
    def testStateEnum(): Unit \ Assert =
        let result = run {
            State.put(Some(42));
            State.get()
        } with handler State {
            def get(k) = k(Some(123))
            def put(x, k) = if (x == Some(42)) k(()) else None
        };
        assertEq(expected = Some(123), result)

    @Test
    def testStateNative(): Unit \ Assert =
        let result = run {
            State.put(newAtomicInteger(42));
            State.get()
        } with handler State {
            def get(k) = k(newAtomicInteger(123))
            def put(x, k) = if (toInt(x) == 42) k(()) else newAtomicInteger(-1)
        };
        assertEq(expected = 123, toInt(result))

    @Test
    def testAskTellUnit(): Unit \ Assert =
        let result = run AskTell.askTell(()) with handler AskTell {
            def askTell(x, k) = k(x)
        };
        assertEq(expected = (), result)

    @Test
    def testAskTellBool(): Unit \ Assert =
        let result = run AskTell.askTell(true) with handler AskTell {
            def askTell(x, k) = k(not x)
        };
        assertEq(expected = false, result)

    @Test
    def testAskTellChar(): Unit \ Assert =
        let result = run AskTell.askTell('a') with handler AskTell {
            def askTell(x, k) = if (x == 'a') k('b') else k('?')
        };
        assertEq(expected = 'b', result)

    @Test
    def testAskTellFloat32(): Unit \ Assert =
        let result = run AskTell.askTell(42.0f32) with handler AskTell {
            def askTell(x, k) = k(x + 1.0f32)
        };
        assertEq(expected = 43.0f32, result)

    @Test
    def testAskTellFloat64(): Unit \ Assert =
        let result = run AskTell.askTell(42.0f64) with handler AskTell {
            def askTell(x, k) = k(x + 1.0f64)
        };
        assertEq(expected = 43.0f64, result)

    @Test
    def testAskTellInt8(): Unit \ Assert =
        let result = run AskTell.askTell(42i8) with handler AskTell {
            def askTell(x, k) = k(x + 1i8)
        };
        assertEq(expected = 43i8, result)

    @Test
    def testAskTellInt16(): Unit \ Assert =
        let result = run AskTell.askTell(42i16) with handler AskTell {
            def askTell(x, k) = k(x + 1i16)
        };
        assertEq(expected = 43i16, result)

    @Test
    def testAskTellInt32(): Unit \ Assert =
        let result = run AskTell.askTell(42) with handler AskTell {
            def askTell(x, k) = k(x + 1)
        };
        assertEq(expected = 43, result)

    @Test
    def testAskTellInt64(): Unit \ Assert =
        let result = run AskTell.askTell(42i64) with handler AskTell {
            def askTell(x, k) = k(x + 1i64)
        };
        assertEq(expected = 43i64, result)

    @Test
    def testAskTellString(): Unit \ Assert =
        let result = run AskTell.askTell("hello") with handler AskTell {
            def askTell(x, k) = if (x == "hello") k("world") else k("unexpected")
        };
        assertEq(expected = "world", result)

    @Test
    def testAskTellEnum(): Unit \ Assert =
        let result = run AskTell.askTell(Some(42)) with handler AskTell {
            def askTell(x, k) = if (x == Some(42)) k(Some(123)) else k(None)
        };
        assertEq(expected = Some(123), result)

    @Test
    def testAskTellNative(): Unit \ Assert =
        let result = run AskTell.askTell(newAtomicInteger(42)) with handler AskTell {
            def askTell(x, k) = k(newAtomicInteger(toInt(x) + 1))
        };
        assertEq(expected = 43, toInt(result))

    eff Emit[t] {
        def emit(x: t): Unit
    }

    @CompileTest
    def emitInt(): Unit \ Emit[Int32] = Emit.emit(42)

    @CompileTest
    def emitString(): Unit \ Emit[String] = Emit.emit("hello")

    eff F[t] {
        def f(x: t): Unit
    }

    eff G[t] {
        def g(x: t): Unit
    }

    @CompileTest
    def useBoth(): Unit \ F[Int32] + G[String] = {
        F.f(42);
        G.g("hello")
    }

    eff Pair[a, b] {
        def put(x: a, y: b): Unit
    }

    @CompileTest
    def usePair(): Unit \ Pair[Int32, String] = Pair.put(42, "hello")

}
