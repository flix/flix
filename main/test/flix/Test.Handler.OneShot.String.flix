mod Test.Handler.OneShot.String {

    eff Ask {
        def ask(x: String): String
    }

    @Test
    def testLinear01(): Bool =
        def f1() = Ask.ask("a");
        let result = run {
            f1()
        } with handler Ask {
            def ask(x, k) = k(next(x))
        };
        Assert.eq("b", result)

    @Test
    def testLinear02(): Bool =
        def f1() = Ask.ask("a");
        def f2() = f1();
        let result = run {
            f2()
        } with handler Ask {
            def ask(x, k) = k(next(x))
        };
        Assert.eq("b", result)

    @Test
    def testLinear03(): Bool =
        def f1() = Ask.ask("a");
        def f2() = f1();
        def f3() = f2();
        let result = run {
            f3()
        } with handler Ask {
            def ask(x, k) = k(next(x))
        };
        Assert.eq("b", result)

    @Test
    def testRecursiveLetRec01(): Bool =
        def f() = Ask.ask("a");
        def r(x) = if (x == 0) f() else r(x - 1);
        let result = run {
            r(0)
        } with handler Ask {
            def ask(x, k) = k(next(x))
        };
        Assert.eq("b", result)

    @Test
    def testRecursiveLetRec02(): Bool =
        def f() = Ask.ask("a");
        def r(x) = if (x == 0) f() else r(x - 1);
        let result = run {
            r(1)
        } with handler Ask {
            def ask(x, k) = k(next(x))
        };
        Assert.eq("b", result)

    @Test
    def testRecursiveLetRec03(): Bool =
        def f1() = Ask.ask("a");
        def r(x) = if (x == 0) f1() else r(x - 1);
        let result = run {
            r(10)
        } with handler Ask {
            def ask(x, k) = k(next(x))
        };
        Assert.eq("b", result)

    @Test
    def testMutualRecursive01(): Bool =
        let result = run {
            mutual1(0)
        } with handler Ask {
            def ask(x, k) = k(next(x))
        };
        Assert.eq("b", result)

    @Test
    def testMutualRecursive02(): Bool =
        let result = run {
            mutual1(1)
        } with handler Ask {
            def ask(x, k) = k(next(x))
        };
        Assert.eq("b", result)

    @Test
    def testMutualRecursive03(): Bool =
        let result = run {
            mutual1(2)
        } with handler Ask {
            def ask(x, k) = k(next(x))
        };
        Assert.eq("b", result)

    @Test
    def testMutualRecursive04(): Bool =
        let result = run {
            mutual1(3)
        } with handler Ask {
            def ask(x, k) = k(next(x))
        };
        Assert.eq("b", result)

    @Test
    def testMutualRecursive05(): Bool =
        let result = run {
            mutual1(10)
        } with handler Ask {
            def ask(x, k) = k(next(x))
        };
        Assert.eq("b", result)

    def f(): String \ Ask = Ask.ask("a")

    def mutual1(x: Int32): String \ Ask =
        if (x == 0) f() else mutual2(x - 1)

    def mutual2(x: Int32): String \ Ask =
        if (x == 0) f() else mutual1(x - 1)

    def next(c: String): String = match c {
        case "a" => "b"
        case "b" => "a"
        case _   => unreachable!()
    }

}
