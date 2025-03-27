mod Test.Handler.ZeroShot.Float64 {

    eff Exc {
        def raise(): Float64
    }

    @Test
    def testLinear01(): Bool =
        def f1() = Exc.raise();
        let result = run {
            f1()
        } with handler Exc {
            def raise(_k) = 42.0f64
        };
        Assert.eq(42.0f64, result)

    @Test
    def testLinear02(): Bool =
        def f1() = Exc.raise();
        def f2() = { discard f1(); unreachable!() };
        let result = run {
            f2()
        } with handler Exc {
            def raise(_k) = 42.0f64
        };
        Assert.eq(42.0f64, result)

    @Test
    def testLinear03(): Bool =
        def f1() = Exc.raise();
        def f2() = { discard f1(); unreachable!() };
        def f3() = { discard f2(); unreachable!() };
        let result = run {
            f3()
        } with handler Exc {
            def raise(_k) = 42.0f64
        };
        Assert.eq(42.0f64, result)

    @Test
    def testRecursiveLetRec01(): Bool =
        def f() = Exc.raise();
        def r(x) = if (x == 0) f() else { discard r(x - 1); unreachable!() } ;
        let result = run {
            r(0)
        } with handler Exc {
            def raise(_k) = 42.0f64
        };
        Assert.eq(42.0f64, result)

    @Test
    def testRecursiveLetRec02(): Bool =
        def f() = Exc.raise();
        def r(x) = if (x == 0) f() else { discard r(x - 1); unreachable!() } ;
        let result = run {
            r(1)
        } with handler Exc {
            def raise(_k) = 42.0f64
        };
        Assert.eq(42.0f64, result)

    @Test
    def testRecursiveLetRec03(): Bool =
        def f() = Exc.raise();
        def r(x) = if (x == 0) f() else { discard r(x - 1); unreachable!() } ;
        let result = run {
            r(10)
        } with handler Exc {
            def raise(_k) = 42.0f64
        };
        Assert.eq(42.0f64, result)

    @Test
    def testMutualRecursive01(): Bool =
        let result = run {
            mutual1(0)
        } with handler Exc {
            def raise(_k) = 42.0f64
        };
        Assert.eq(42.0f64, result)

    @Test
    def testMutualRecursive02(): Bool =
        let result = run {
            mutual1(1)
        } with handler Exc {
            def raise(_k) = 42.0f64
        };
        Assert.eq(42.0f64, result)

    @Test
    def testMutualRecursive03(): Bool =
        let result = run {
            mutual1(2)
        } with handler Exc {
            def raise(_k) = 42.0f64
        };
        Assert.eq(42.0f64, result)

    @Test
    def testMutualRecursive04(): Bool =
        let result = run {
            mutual1(3)
        } with handler Exc {
            def raise(_k) = 42.0f64
        };
        Assert.eq(42.0f64, result)

    @Test
    def testMutualRecursive05(): Bool =
        let result = run {
            mutual1(10)
        } with handler Exc {
            def raise(_k) = 42.0f64
        };
        Assert.eq(42.0f64, result)

    @Test
    def testMutualRecursive06(): Bool =
        let result = run {
            mutual1(10)
        } with handler Exc {
            def raise(_k) = 40.0f64
        };
        Assert.eq(40.0f64, result)

    def f(): Float64 \ Exc =
         discard Exc.raise();
         unreachable!()

    def mutual1(x: Int32): Float64 \ Exc =
        if (x == 0) f() else { discard mutual2(x - 1); unreachable!() }

    def mutual2(x: Int32): Float64 \ Exc =
        if (x == 0) f() else { discard mutual1(x - 1); unreachable!() }

}
