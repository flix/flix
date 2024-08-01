mod Test.Handler.ZeroShot.Recursive {

    eff Throw {
        pub def throwEffect(): Unit
    }

    @Test
    def testRecursiveTryWith01(): Bool =
        def r(x) = try {
            if (x == 0) 0 else { let _ = r(x - 1); do Throw.throwEffect(); unreachable!() }
        } with Throw {
            def throwEffect(_k) = 42
        };
        Assert.eq(0, r(0))

    @Test
    def testRecursiveTryWith02(): Bool =
        def r(x) = try {
            if (x == 0) 0 else { let _ = r(x - 1); do Throw.throwEffect(); unreachable!() }
        } with Throw {
            def throwEffect(_k) = 42
        };
        Assert.eq(42, r(1))

    @Test
    def testRecursiveTryWith03(): Bool =
        def r(x) = try {
            if (x == 0) 0 else { let _ = r(x - 1); do Throw.throwEffect(); unreachable!() }
        } with Throw {
            def throwEffect(_k) = 42
        };
        Assert.eq(42, r(2))

    @Test
    def testRecursiveTryWith04(): Bool =
        def r(x) = try {
            if (x == 0) 0 else { let _ = r(x - 1); do Throw.throwEffect(); unreachable!() }
        } with Throw {
            def throwEffect(_k) = 42
        };
        Assert.eq(42, r(3))

    @Test
    def testRecursiveTryWith05(): Bool =
        def r(x) = try {
            if (x == 0) 0 else { let _ = r(x - 1); do Throw.throwEffect(); unreachable!() }
        } with Throw {
            def throwEffect(_k) = 42
        };
        Assert.eq(42, r(10))

}
