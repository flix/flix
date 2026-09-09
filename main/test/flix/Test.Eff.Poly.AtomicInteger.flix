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

    def askTwice(x: t): t \ AskTell[t] =
        AskTell.askTell(AskTell.askTell(x))

    def replace(next: s): (s, s) \ State[s] = {
        let previous = State.get();
        State.put(next);
        let current = State.get();
        (previous, current)
    }

    def failWith(error: e): a \ Result[e, a] =
        Result.fail(error)

    def runState(initial: s, f: Unit -> a \ State[s]): a =
        region r {
            let cell = Ref.fresh(r, initial);
            run f() with handler State {
                def get(k) = k(Ref.get(cell))
                def put(value, k) = {
                    Ref.put(value, cell);
                    k(())
                }
            }
        }

    def runResult(value: a, f: Unit -> b \ Result[e, a]): b =
        run f() with handler Result {
            def fail(_error, k) = k(value)
        }

    def useBoth(x: a, y: b): (a, b) \ AskTell[a] + State[b] = {
        let x1 = AskTell.askTell(x);
        State.put(y);
        let y1 = State.get();
        (x1, y1)
    }

    @Test
    def testAskTellPolymorphic(): Unit \ Assert =
        let result = run askTwice(newAtomicInteger(42)) with handler AskTell {
            def askTell(x, k) =
                if (toInt(x) == 42) k(newAtomicInteger(43))
                else if (toInt(x) == 43) k(newAtomicInteger(44))
                else k(newAtomicInteger(-1))
        };
        assertEq(expected = 44, toInt(result))

    @Test
    def testStatePolymorphic(): Unit \ Assert =
        let result = runState(newAtomicInteger(42), () -> replace(newAtomicInteger(43)));
        let (previous, current) = result;
        assertEq(expected = (42, 43), (toInt(previous), toInt(current)))

    @Test
    def testResultPolymorphic(): Unit \ Assert =
        let result = runResult(newAtomicInteger(43), () -> failWith("boom"));
        assertEq(expected = 43, toInt(result))

    @Test
    def testMultipleEffects(): Unit \ Assert =
        let result = run useBoth(newAtomicInteger(42), "before") with handler AskTell {
            def askTell(x, k) = if (toInt(x) == 42) k(newAtomicInteger(43)) else unreachable!()
        } with handler State {
            def get(k) = k("after")
            def put(x, k) = if (x == "before") k(()) else unreachable!()
        };
        let (x, state) = result;
        assertEq(expected = (43, "after"), (toInt(x), state))

}
