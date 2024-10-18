mod Test.Handler.MultiShot.Float64 {

    eff AskTell {
        pub def askTell(x: Float64): Float64
    }

    @Test
    def testLinear01(): Bool =
        def f1() = AskTell.askTell(42.0f64);
        let result = try {
            f1()
        } with AskTell {
            def askTell(x, k) = k(x) + k(x)
        };
        Assert.eq(84.0f64, result)

    @Test
    def testLinear02(): Bool =
        def f1() = AskTell.askTell(42.0f64);
        def f2() = f1();
        let result = try {
            f2()
        } with AskTell {
            def askTell(x, k) = k(x) + k(x)
        };
        Assert.eq(84.0f64, result)

    @Test
    def testLinear03(): Bool =
        def f1() = AskTell.askTell(42.0f64);
        def f2() = f1();
        def f3() = f2();
        let result = try {
            f3()
        } with AskTell {
            def askTell(x, k) = k(x) + k(x)
        };
        Assert.eq(84.0f64, result)

    @Test
    def testRecursiveLetRec01(): Bool =
        def f() = AskTell.askTell(42.0f64);
        def r(x) = if (x == 0) f() else r(x - 1);
        let result = try {
            r(0)
        } with AskTell {
            def askTell(x, k) = k(x) + k(x)
        };
        Assert.eq(84.0f64, result)

    @Test
    def testRecursiveLetRec02(): Bool =
        def f() = AskTell.askTell(42.0f64);
        def r(x) = if (x == 0) f() else r(x - 1);
        let result = try {
            r(1)
        } with AskTell {
            def askTell(x, k) = k(x) + k(x)
        };
        Assert.eq(84.0f64, result)

    @Test
    def testRecursiveLetRec03(): Bool =
        def f1() = AskTell.askTell(42.0f64);
        def r(x) = if (x == 0) f1() else r(x - 1);
        let result = try {
            r(10)
        } with AskTell {
            def askTell(x, k) = k(x) + k(x)
        };
        Assert.eq(84.0f64, result)

    @Test
    def testMutualRecursive01(): Bool =
        let result = try {
            mutual1(0)
        } with AskTell {
            def askTell(x, k) = k(x) + k(x)
        };
        Assert.eq(84.0f64, result)

    @Test
    def testMutualRecursive02(): Bool =
        let result = try {
            mutual1(1)
        } with AskTell {
            def askTell(x, k) = k(x) + k(x)
        };
        Assert.eq(84.0f64, result)

    @Test
    def testMutualRecursive03(): Bool =
        let result = try {
            mutual1(2)
        } with AskTell {
            def askTell(x, k) = k(x) + k(x)
        };
        Assert.eq(84.0f64, result)

    @Test
    def testMutualRecursive04(): Bool =
        let result = try {
            mutual1(3)
        } with AskTell {
            def askTell(x, k) = k(x) + k(x)
        };
        Assert.eq(84.0f64, result)

    @Test
    def testMutualRecursive05(): Bool =
        let result = try {
            mutual1(10)
        } with AskTell {
            def askTell(x, k) = k(x) + k(x)
        };
        Assert.eq(84.0f64, result)

    def f(): Float64 \ AskTell = AskTell.askTell(42.0f64)

    def mutual1(x: Int32): Float64 \ AskTell =
        if (x == 0) f() else mutual2(x - 1)

    def mutual2(x: Int32): Float64 \ AskTell =
        if (x == 0) f() else mutual1(x - 1)

}
