mod Test.Eff.Poly.String {

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

    def fail(): String \ Result[Int32, String] =
        Result.fail(42)

    @Test
    def testAskTellZeroShot(): Unit \ Assert =
        let result = run AskTell.askTell("hello") with handler AskTell {
            def askTell(x, _k) = if (x == "hello") "world" else "unexpected"
        };
        assertEq(expected = "world", result)

    @Test
    def testAskTellOneShot(): Unit \ Assert =
        let result = run AskTell.askTell("hello") with handler AskTell {
            def askTell(x, k) = if (x == "hello") k("world") else k("unexpected")
        };
        assertEq(expected = "world", result)

    @Test
    def testAskTellMultiShot(): Unit \ Assert =
        let result = run AskTell.askTell("hello") with handler AskTell {
            def askTell(x, k) =
                if (x == "hello") {
                    discard k("world");
                    k("done")
                } else k("unexpected")
        };
        assertEq(expected = "done", result)

    @Test
    def testStateZeroShot(): Unit \ Assert =
        let result = run {
            State.put("hello");
            State.get()
        } with handler State {
            def get(k) = k("world")
            def put(x, _k) = if (x == "hello") "world" else "unexpected"
        };
        assertEq(expected = "world", result)

    @Test
    def testStateOneShot(): Unit \ Assert =
        let result = run {
            State.put("hello");
            State.get()
        } with handler State {
            def get(k) = k("world")
            def put(x, k) = if (x == "hello") k(()) else "unexpected"
        };
        assertEq(expected = "world", result)

    @Test
    def testStateMultiShot(): Unit \ Assert =
        let result = run {
            State.put("hello");
            State.get()
        } with handler State {
            def get(k) = k("world")
            def put(x, k) =
                if (x == "hello") {
                    discard k(());
                    k(())
                } else "unexpected"
        };
        assertEq(expected = "world", result)

    @Test
    def testResultZeroShot(): Unit \ Assert =
        let result = run fail() with handler Result {
            def fail(error, _k) = if (error == 42) "world" else "unexpected"
        };
        assertEq(expected = "world", result)

    @Test
    def testResultOneShot(): Unit \ Assert =
        let result = run fail() with handler Result {
            def fail(error, k) = if (error == 42) k("world") else k("unexpected")
        };
        assertEq(expected = "world", result)

    @Test
    def testResultMultiShot(): Unit \ Assert =
        let result = run fail() with handler Result {
            def fail(error, k) =
                if (error == 42) {
                    discard k("world");
                    k("done")
                } else k("unexpected")
        };
        assertEq(expected = "done", result)

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
        let result = run askTwice("hello") with handler AskTell {
            def askTell(x, k) =
                if (x == "hello") k("world")
                else if (x == "world") k("done")
                else k("unexpected")
        };
        assertEq(expected = "done", result)

    @Test
    def testStatePolymorphic(): Unit \ Assert =
        let result = runState("hello", () -> replace("world"));
        assertEq(expected = ("hello", "world"), result)

    @Test
    def testResultPolymorphic(): Unit \ Assert =
        let result = runResult("world", () -> failWith(42));
        assertEq(expected = "world", result)

    @Test
    def testMultipleEffects(): Unit \ Assert =
        let result = run useBoth("hello", 42) with handler AskTell {
            def askTell(x, k) = if (x == "hello") k("world") else unreachable!()
        } with handler State {
            def get(k) = k(43)
            def put(x, k) = if (x == 42) k(()) else unreachable!()
        };
        assertEq(expected = ("world", 43), result)

}
