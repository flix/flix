mod Test.Handler.ZeroShot.Int64 {

    use Assert.{assertEq, assertTrue};

    eff Exc {
        def raise(): Int64
    }

    @Test
    def testLinear01(): Unit \ Assert =
        def f1() = Exc.raise();
        let result = run {
            f1()
        } with handler Exc {
            def raise(_k) = 42i64
        };
        assertEq(expected = 42i64, result)

    @Test
    def testLinear02(): Unit \ Assert =
        def f1() = Exc.raise();
        def f2() = { discard f1(); unreachable!() };
        let result = run {
            f2()
        } with handler Exc {
            def raise(_k) = 42i64
        };
        assertEq(expected = 42i64, result)

    @Test
    def testLinear03(): Unit \ Assert =
        def f1() = Exc.raise();
        def f2() = { discard f1(); unreachable!() };
        def f3() = { discard f2(); unreachable!() };
        let result = run {
            f3()
        } with handler Exc {
            def raise(_k) = 42i64
        };
        assertEq(expected = 42i64, result)

    @Test
    def testRecursiveLetRec01(): Unit \ Assert =
        def f() = Exc.raise();
        def r(x) = if (x == 0) f() else { discard r(x - 1); unreachable!() } ;
        let result = run {
            r(0)
        } with handler Exc {
            def raise(_k) = 42i64
        };
        assertEq(expected = 42i64, result)

    @Test
    def testRecursiveLetRec02(): Unit \ Assert =
        def f() = Exc.raise();
        def r(x) = if (x == 0) f() else { discard r(x - 1); unreachable!() } ;
        let result = run {
            r(1)
        } with handler Exc {
            def raise(_k) = 42i64
        };
        assertEq(expected = 42i64, result)

    @Test
    def testRecursiveLetRec03(): Unit \ Assert =
        def f() = Exc.raise();
        def r(x) = if (x == 0) f() else { discard r(x - 1); unreachable!() } ;
        let result = run {
            r(10)
        } with handler Exc {
            def raise(_k) = 42i64
        };
        assertEq(expected = 42i64, result)

    @Test
    def testMutualRecursive01(): Unit \ Assert =
        let result = run {
            mutual1(0)
        } with handler Exc {
            def raise(_k) = 42i64
        };
        assertEq(expected = 42i64, result)

    @Test
    def testMutualRecursive02(): Unit \ Assert =
        let result = run {
            mutual1(1)
        } with handler Exc {
            def raise(_k) = 42i64
        };
        assertEq(expected = 42i64, result)

    @Test
    def testMutualRecursive03(): Unit \ Assert =
        let result = run {
            mutual1(2)
        } with handler Exc {
            def raise(_k) = 42i64
        };
        assertEq(expected = 42i64, result)

    @Test
    def testMutualRecursive04(): Unit \ Assert =
        let result = run {
            mutual1(3)
        } with handler Exc {
            def raise(_k) = 42i64
        };
        assertEq(expected = 42i64, result)

    @Test
    def testMutualRecursive05(): Unit \ Assert =
        let result = run {
            mutual1(10)
        } with handler Exc {
            def raise(_k) = 42i64
        };
        assertEq(expected = 42i64, result)

    def f(): Int64 \ Exc =
         discard Exc.raise();
         unreachable!()

    def mutual1(x: Int32): Int64 \ Exc =
        if (x == 0) f() else { discard mutual2(x - 1); unreachable!() }

    def mutual2(x: Int32): Int64 \ Exc =
        if (x == 0) f() else { discard mutual1(x - 1); unreachable!() }

}
