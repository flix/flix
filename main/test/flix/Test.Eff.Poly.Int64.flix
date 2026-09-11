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
        let result = run askTwice(42i64) with handler AskTell {
            def askTell(x, k) =
                if (x == 42i64) k(43i64)
                else if (x == 43i64) k(44i64)
                else k(-1i64)
        };
        assertEq(expected = 44i64, result)

    @Test
    def testStatePolymorphic(): Unit \ Assert =
        let result = runState(42i64, () -> replace(43i64));
        assertEq(expected = (42i64, 43i64), result)

    @Test
    def testResultPolymorphic(): Unit \ Assert =
        let result = runResult(43i64, () -> failWith("boom"));
        assertEq(expected = 43i64, result)

    @Test
    def testMultipleEffects(): Unit \ Assert =
        let result = run useBoth(42i64, "before") with handler AskTell {
            def askTell(x, k) = if (x == 42i64) k(43i64) else unreachable!()
        } with handler State {
            def get(k) = k("after")
            def put(x, k) = if (x == "before") k(()) else unreachable!()
        };
        assertEq(expected = (43i64, "after"), result)

}
