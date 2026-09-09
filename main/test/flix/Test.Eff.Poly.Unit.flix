mod Test.Eff.Poly.Unit {

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

    def fail(): Unit \ Result[String, Unit] =
        Result.fail("boom")

    @Test
    def testAskTellZeroShot(): Unit \ Assert =
        let result = run AskTell.askTell(()) with handler AskTell {
            def askTell(x, _k) = if (x == ()) () else ()
        };
        assertEq(expected = (), result)

    @Test
    def testAskTellOneShot(): Unit \ Assert =
        let result = run AskTell.askTell(()) with handler AskTell {
            def askTell(x, k) = if (x == ()) k(()) else k(())
        };
        assertEq(expected = (), result)

    @Test
    def testAskTellMultiShot(): Unit \ Assert =
        let result = run AskTell.askTell(()) with handler AskTell {
            def askTell(x, k) =
                if (x == () and k(()) == () and k(()) == ()) () else unreachable!()
        };
        assertEq(expected = (), result)

    @Test
    def testStateZeroShot(): Unit \ Assert =
        let result = run {
            State.put(());
            State.get()
        } with handler State {
            def get(k) = k(())
            def put(x, _k) = if (x == ()) () else ()
        };
        assertEq(expected = (), result)

    @Test
    def testStateOneShot(): Unit \ Assert =
        let result = run {
            State.put(());
            State.get()
        } with handler State {
            def get(k) = k(())
            def put(x, k) = if (x == ()) k(()) else ()
        };
        assertEq(expected = (), result)

    @Test
    def testStateMultiShot(): Unit \ Assert =
        let result = run {
            State.put(());
            State.get()
        } with handler State {
            def get(k) = k(())
            def put(x, k) =
                if (x == () and k(()) == () and k(()) == ()) () else unreachable!()
        };
        assertEq(expected = (), result)

    @Test
    def testResultZeroShot(): Unit \ Assert =
        let result = run fail() with handler Result {
            def fail(error, _k) = if (error == "boom") () else ()
        };
        assertEq(expected = (), result)

    @Test
    def testResultOneShot(): Unit \ Assert =
        let result = run fail() with handler Result {
            def fail(error, k) = if (error == "boom") k(()) else k(())
        };
        assertEq(expected = (), result)

    @Test
    def testResultMultiShot(): Unit \ Assert =
        let result = run fail() with handler Result {
            def fail(error, k) =
                if (error == "boom" and k(()) == () and k(()) == ()) () else unreachable!()
        };
        assertEq(expected = (), result)

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
        let result = run askTwice(()) with handler AskTell {
            def askTell(x, k) =
                if (x == ()) k(())
                else if (x == ()) k(())
                else k(())
        };
        assertEq(expected = (), result)

    @Test
    def testStatePolymorphic(): Unit \ Assert =
        let result = runState((), () -> replace(()));
        assertEq(expected = ((), ()), result)

    @Test
    def testResultPolymorphic(): Unit \ Assert =
        let result = runResult((), () -> failWith("boom"));
        assertEq(expected = (), result)

    @Test
    def testMultipleEffects(): Unit \ Assert =
        let result = run useBoth((), "before") with handler AskTell {
            def askTell(x, k) = if (x == ()) k(()) else unreachable!()
        } with handler State {
            def get(k) = k("after")
            def put(x, k) = if (x == "before") k(()) else unreachable!()
        };
        assertEq(expected = ((), "after"), result)

}
