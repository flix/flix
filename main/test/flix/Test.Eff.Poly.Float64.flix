mod Test.Eff.Poly.Float64 {

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

    def fail(): Float64 \ Result[String, Float64] =
        Result.fail("boom")

    @Test
    def testAskTellZeroShot(): Unit \ Assert =
        let result = run AskTell.askTell(42.0f64) with handler AskTell {
            def askTell(x, _k) = if (x == 42.0f64) 43.0f64 else -1.0f64
        };
        assertEq(expected = 43.0f64, result)

    @Test
    def testAskTellOneShot(): Unit \ Assert =
        let result = run AskTell.askTell(42.0f64) with handler AskTell {
            def askTell(x, k) = if (x == 42.0f64) k(43.0f64) else k(-1.0f64)
        };
        assertEq(expected = 43.0f64, result)

    @Test
    def testAskTellMultiShot(): Unit \ Assert =
        let result = run AskTell.askTell(42.0f64) with handler AskTell {
            def askTell(x, k) =
                if (x == 42.0f64) {
                    discard k(43.0f64);
                    k(44.0f64)
                } else k(-1.0f64)
        };
        assertEq(expected = 44.0f64, result)

    @Test
    def testStateZeroShot(): Unit \ Assert =
        let result = run {
            State.put(42.0f64);
            State.get()
        } with handler State {
            def get(k) = k(43.0f64)
            def put(x, _k) = if (x == 42.0f64) 43.0f64 else -1.0f64
        };
        assertEq(expected = 43.0f64, result)

    @Test
    def testStateOneShot(): Unit \ Assert =
        let result = run {
            State.put(42.0f64);
            State.get()
        } with handler State {
            def get(k) = k(43.0f64)
            def put(x, k) = if (x == 42.0f64) k(()) else -1.0f64
        };
        assertEq(expected = 43.0f64, result)

    @Test
    def testStateMultiShot(): Unit \ Assert =
        let result = run {
            State.put(42.0f64);
            State.get()
        } with handler State {
            def get(k) = k(43.0f64)
            def put(x, k) =
                if (x == 42.0f64) {
                    discard k(());
                    k(())
                } else -1.0f64
        };
        assertEq(expected = 43.0f64, result)

    @Test
    def testResultZeroShot(): Unit \ Assert =
        let result = run fail() with handler Result {
            def fail(error, _k) = if (error == "boom") 43.0f64 else -1.0f64
        };
        assertEq(expected = 43.0f64, result)

    @Test
    def testResultOneShot(): Unit \ Assert =
        let result = run fail() with handler Result {
            def fail(error, k) = if (error == "boom") k(43.0f64) else k(-1.0f64)
        };
        assertEq(expected = 43.0f64, result)

    @Test
    def testResultMultiShot(): Unit \ Assert =
        let result = run fail() with handler Result {
            def fail(error, k) =
                if (error == "boom") {
                    discard k(43.0f64);
                    k(44.0f64)
                } else k(-1.0f64)
        };
        assertEq(expected = 44.0f64, result)

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
        let result = run askTwice(42.0f64) with handler AskTell {
            def askTell(x, k) =
                if (x == 42.0f64) k(43.0f64)
                else if (x == 43.0f64) k(44.0f64)
                else k(-1.0f64)
        };
        assertEq(expected = 44.0f64, result)

    @Test
    def testStatePolymorphic(): Unit \ Assert =
        let result = runState(42.0f64, () -> replace(43.0f64));
        assertEq(expected = (42.0f64, 43.0f64), result)

    @Test
    def testResultPolymorphic(): Unit \ Assert =
        let result = runResult(43.0f64, () -> failWith("boom"));
        assertEq(expected = 43.0f64, result)

    @Test
    def testMultipleEffects(): Unit \ Assert =
        let result = run useBoth(42.0f64, "before") with handler AskTell {
            def askTell(x, k) = if (x == 42.0f64) k(43.0f64) else unreachable!()
        } with handler State {
            def get(k) = k("after")
            def put(x, k) = if (x == "before") k(()) else unreachable!()
        };
        assertEq(expected = (43.0f64, "after"), result)

}
