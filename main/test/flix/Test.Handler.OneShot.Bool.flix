mod Test.Handler.OneShot.Bool {

    eff Ask {
        pub def ask(x: Bool): Bool
    }

    @Test
    def testLinear01(): Bool =
        def f1() = do Ask.ask(false);
        let result = try {
            f1()
        } with Ask {
            def ask(x, k) = k(not x)
        };
        Assert.eq(true, result)

    @Test
    def testLinear02(): Bool =
        def f1() = do Ask.ask(false);
        def f2() = f1();
        let result = try {
            f2()
        } with Ask {
            def ask(x, k) = k(not x)
        };
        Assert.eq(true, result)

    @Test
    def testLinear03(): Bool =
        def f1() = do Ask.ask(false);
        def f2() = f1();
        def f3() = f2();
        let result = try {
            f3()
        } with Ask {
            def ask(x, k) = k(not x)
        };
        Assert.eq(true, result)

    @Test
    def testRecursiveLetRec01(): Bool =
        def f() = do Ask.ask(false);
        def r(x) = if (x == 0) f() else r(x - 1);
        let result = try {
            r(0)
        } with Ask {
            def ask(x, k) = k(not x)
        };
        Assert.eq(true, result)

    @Test
    def testRecursiveLetRec02(): Bool =
        def f() = do Ask.ask(false);
        def r(x) = if (x == 0) f() else r(x - 1);
        let result = try {
            r(1)
        } with Ask {
            def ask(x, k) = k(not x)
        };
        Assert.eq(true, result)

    @Test
    def testRecursiveLetRec03(): Bool =
        def f1() = do Ask.ask(false);
        def r(x) = if (x == 0) f1() else r(x - 1);
        let result = try {
            r(10)
        } with Ask {
            def ask(x, k) = k(not x)
        };
        Assert.eq(true, result)

    @Test
    def testMutualRecursive01(): Bool =
        let result = try {
            mutual1(0)
        } with Ask {
            def ask(x, k) = k(not x)
        };
        Assert.eq(true, result)

    @Test
    def testMutualRecursive02(): Bool =
        let result = try {
            mutual1(1)
        } with Ask {
            def ask(x, k) = k(not x)
        };
        Assert.eq(true, result)

    @Test
    def testMutualRecursive03(): Bool =
        let result = try {
            mutual1(2)
        } with Ask {
            def ask(x, k) = k(not x)
        };
        Assert.eq(true, result)

    @Test
    def testMutualRecursive04(): Bool =
        let result = try {
            mutual1(3)
        } with Ask {
            def ask(x, k) = k(not x)
        };
        Assert.eq(true, result)

    @Test
    def testMutualRecursive05(): Bool =
        let result = try {
            mutual1(10)
        } with Ask {
            def ask(x, k) = k(not x)
        };
        Assert.eq(true, result)

    def f(): Bool \ Ask = do Ask.ask(false)

    def mutual1(x: Int32): Bool \ Ask =
        if (x == 0) f() else mutual2(x - 1)

    def mutual2(x: Int32): Bool \ Ask =
        if (x == 0) f() else mutual1(x - 1)

}
