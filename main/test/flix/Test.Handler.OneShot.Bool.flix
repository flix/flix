mod Test.Handler.OneShot.Bool {

    eff AskTell {
        def askTell(x: Bool): Bool
    }

    @Test
    def testLinear01(): Bool =
        def f1() = AskTell.askTell(false);
        let result = run {
            f1()
        } with handler AskTell {
            def askTell(x, k) = k(not x)
        };
        Assert.eq(true, result)

    @Test
    def testLinear02(): Bool =
        def f1() = AskTell.askTell(false);
        def f2() = f1();
        let result = run {
            f2()
        } with handler AskTell {
            def askTell(x, k) = k(not x)
        };
        Assert.eq(true, result)

    @Test
    def testLinear03(): Bool =
        def f1() = AskTell.askTell(false);
        def f2() = f1();
        def f3() = f2();
        let result = run {
            f3()
        } with handler AskTell {
            def askTell(x, k) = k(not x)
        };
        Assert.eq(true, result)

    @Test
    def testRecursiveLetRec01(): Bool =
        def f() = AskTell.askTell(false);
        def r(x) = if (x == 0) f() else r(x - 1);
        let result = run {
            r(0)
        } with handler AskTell {
            def askTell(x, k) = k(not x)
        };
        Assert.eq(true, result)

    @Test
    def testRecursiveLetRec02(): Bool =
        def f() = AskTell.askTell(false);
        def r(x) = if (x == 0) f() else r(x - 1);
        let result = run {
            r(1)
        } with handler AskTell {
            def askTell(x, k) = k(not x)
        };
        Assert.eq(true, result)

    @Test
    def testRecursiveLetRec03(): Bool =
        def f1() = AskTell.askTell(false);
        def r(x) = if (x == 0) f1() else r(x - 1);
        let result = run {
            r(10)
        } with handler AskTell {
            def askTell(x, k) = k(not x)
        };
        Assert.eq(true, result)

    @Test
    def testMutualRecursive01(): Bool =
        let result = run {
            mutual1(0)
        } with handler AskTell {
            def askTell(x, k) = k(not x)
        };
        Assert.eq(true, result)

    @Test
    def testMutualRecursive02(): Bool =
        let result = run {
            mutual1(1)
        } with handler AskTell {
            def askTell(x, k) = k(not x)
        };
        Assert.eq(true, result)

    @Test
    def testMutualRecursive03(): Bool =
        let result = run {
            mutual1(2)
        } with handler AskTell {
            def askTell(x, k) = k(not x)
        };
        Assert.eq(true, result)

    @Test
    def testMutualRecursive04(): Bool =
        let result = run {
            mutual1(3)
        } with handler AskTell {
            def askTell(x, k) = k(not x)
        };
        Assert.eq(true, result)

    @Test
    def testMutualRecursive05(): Bool =
        let result = run {
            mutual1(10)
        } with handler AskTell {
            def askTell(x, k) = k(not x)
        };
        Assert.eq(true, result)

    def f(): Bool \ AskTell = AskTell.askTell(false)

    def mutual1(x: Int32): Bool \ AskTell =
        if (x == 0) f() else mutual2(x - 1)

    def mutual2(x: Int32): Bool \ AskTell =
        if (x == 0) f() else mutual1(x - 1)

}
