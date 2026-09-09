mod Test.Eff.Poly.Char {

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

    def fail(): Char \ Result[String, Char] =
        Result.fail("boom")

    @Test
    def testAskTellZeroShot(): Unit \ Assert =
        let result = run AskTell.askTell('a') with handler AskTell {
            def askTell(x, _k) = if (x == 'a') 'b' else '?'
        };
        assertEq(expected = 'b', result)

    @Test
    def testAskTellOneShot(): Unit \ Assert =
        let result = run AskTell.askTell('a') with handler AskTell {
            def askTell(x, k) = if (x == 'a') k('b') else k('?')
        };
        assertEq(expected = 'b', result)

    @Test
    def testAskTellMultiShot(): Unit \ Assert =
        let result = run AskTell.askTell('a') with handler AskTell {
            def askTell(x, k) =
                if (x == 'a') {
                    discard k('b');
                    k('c')
                } else k('?')
        };
        assertEq(expected = 'c', result)

    @Test
    def testStateZeroShot(): Unit \ Assert =
        let result = run {
            State.put('a');
            State.get()
        } with handler State {
            def get(k) = k('b')
            def put(x, _k) = if (x == 'a') 'b' else '?'
        };
        assertEq(expected = 'b', result)

    @Test
    def testStateOneShot(): Unit \ Assert =
        let result = run {
            State.put('a');
            State.get()
        } with handler State {
            def get(k) = k('b')
            def put(x, k) = if (x == 'a') k(()) else '?'
        };
        assertEq(expected = 'b', result)

    @Test
    def testStateMultiShot(): Unit \ Assert =
        let result = run {
            State.put('a');
            State.get()
        } with handler State {
            def get(k) = k('b')
            def put(x, k) =
                if (x == 'a') {
                    discard k(());
                    k(())
                } else '?'
        };
        assertEq(expected = 'b', result)

    @Test
    def testResultZeroShot(): Unit \ Assert =
        let result = run fail() with handler Result {
            def fail(error, _k) = if (error == "boom") 'b' else '?'
        };
        assertEq(expected = 'b', result)

    @Test
    def testResultOneShot(): Unit \ Assert =
        let result = run fail() with handler Result {
            def fail(error, k) = if (error == "boom") k('b') else k('?')
        };
        assertEq(expected = 'b', result)

    @Test
    def testResultMultiShot(): Unit \ Assert =
        let result = run fail() with handler Result {
            def fail(error, k) =
                if (error == "boom") {
                    discard k('b');
                    k('c')
                } else k('?')
        };
        assertEq(expected = 'c', result)

}

