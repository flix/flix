mod Test.Handler.ZeroShot.Recursive {

    use Assert.{assertEq, assertTrue};

    eff Exc {
        def raise(): Unit
    }

    @Test
    def testRecursiveTryWith01(): Unit \ Assert =
        def r(x) = run {
            if (x == 0) 0 else { let _ = r(x - 1); Exc.raise(); unreachable!() }
        } with handler Exc {
            def raise(_k) = 42
        };
        assertEq(expected = 0, r(0))

    @Test
    def testRecursiveTryWith02(): Unit \ Assert =
        def r(x) = run {
            if (x == 0) 0 else { let _ = r(x - 1); Exc.raise(); unreachable!() }
        } with handler Exc {
            def raise(_k) = 42
        };
        assertEq(expected = 42, r(1))

    @Test
    def testRecursiveTryWith03(): Unit \ Assert =
        def r(x) = run {
            if (x == 0) 0 else { let _ = r(x - 1); Exc.raise(); unreachable!() }
        } with handler Exc {
            def raise(_k) = 42
        };
        assertEq(expected = 42, r(2))

    @Test
    def testRecursiveTryWith04(): Unit \ Assert =
        def r(x) = run {
            if (x == 0) 0 else { let _ = r(x - 1); Exc.raise(); unreachable!() }
        } with handler Exc {
            def raise(_k) = 42
        };
        assertEq(expected = 42, r(3))

    @Test
    def testRecursiveTryWith05(): Unit \ Assert =
        def r(x) = run {
            if (x == 0) 0 else { let _ = r(x - 1); Exc.raise(); unreachable!() }
        } with handler Exc {
            def raise(_k) = 42
        };
        assertEq(expected = 42, r(10))

}
