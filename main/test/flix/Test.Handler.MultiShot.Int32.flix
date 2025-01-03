mod Test.Handler.MultiShot.Int32 {

    eff AskTell {
        def askTell(x: Int32): Int32
    }

    @Test
    def testLinear01(): Bool =
        def f1() = AskTell.askTell(42);
        let result = run {
            f1()
        } with handler AskTell {
            def askTell(x, k) = k(x) + k(x)
        };
        Assert.eq(84, result)

    @Test
    def testLinear02(): Bool =
        def f1() = AskTell.askTell(42);
        def f2() = f1();
        let result = run {
            f2()
        } with handler AskTell {
            def askTell(x, k) = k(x) + k(x)
        };
        Assert.eq(84, result)

    @Test
    def testLinear03(): Bool =
        def f1() = AskTell.askTell(42);
        def f2() = f1();
        def f3() = f2();
        let result = run {
            f3()
        } with handler AskTell {
            def askTell(x, k) = k(x) + k(x)
        };
        Assert.eq(84, result)

    @Test
    def testRecursiveLetRec01(): Bool =
        def f() = AskTell.askTell(42);
        def r(x) = if (x == 0) f() else r(x - 1);
        let result = run {
            r(0)
        } with handler AskTell {
            def askTell(x, k) = k(x) + k(x)
        };
        Assert.eq(84, result)

    @Test
    def testRecursiveLetRec02(): Bool =
        def f() = AskTell.askTell(42);
        def r(x) = if (x == 0) f() else r(x - 1);
        let result = run {
            r(1)
        } with handler AskTell {
            def askTell(x, k) = k(x) + k(x)
        };
        Assert.eq(84, result)

    @Test
    def testRecursiveLetRec03(): Bool =
        def f1() = AskTell.askTell(42);
        def r(x) = if (x == 0) f1() else r(x - 1);
        let result = run {
            r(10)
        } with handler AskTell {
            def askTell(x, k) = k(x) + k(x)
        };
        Assert.eq(84, result)

    @Test
    def testMutualRecursive01(): Bool =
        let result = run {
            mutual1(0)
        } with handler AskTell {
            def askTell(x, k) = k(x) + k(x)
        };
        Assert.eq(84, result)

    @Test
    def testMutualRecursive02(): Bool =
        let result = run {
            mutual1(1)
        } with handler AskTell {
            def askTell(x, k) = k(x) + k(x)
        };
        Assert.eq(84, result)

    @Test
    def testMutualRecursive03(): Bool =
        let result = run {
            mutual1(2)
        } with handler AskTell {
            def askTell(x, k) = k(x) + k(x)
        };
        Assert.eq(84, result)

    @Test
    def testMutualRecursive04(): Bool =
        let result = run {
            mutual1(3)
        } with handler AskTell {
            def askTell(x, k) = k(x) + k(x)
        };
        Assert.eq(84, result)

    @Test
    def testMutualRecursive05(): Bool =
        let result = run {
            mutual1(10)
        } with handler AskTell {
            def askTell(x, k) = k(x) + k(x)
        };
        Assert.eq(84, result)

    def f(): Int32 \ AskTell = AskTell.askTell(42)

    def mutual1(x: Int32): Int32 \ AskTell =
        if (x == 0) f() else mutual2(x - 1)

    def mutual2(x: Int32): Int32 \ AskTell =
        if (x == 0) f() else mutual1(x - 1)

}
