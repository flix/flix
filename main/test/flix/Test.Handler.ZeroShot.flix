mod Test.Handler.ZeroShot {

    eff Throw {
        pub def throw(): Unit
    }

    @Test
    def testLinear01(): Unit =
        def f1() = do Throw.throw();
        try {
            f1()
        } with Throw {
            def throw(_k) = ()
        };
        unreachable!()

    @Test
    def testLinear02(): Unit =
        def f1() = do Throw.throw();
        def f2() = { f1(); unreachable!() };
        try {
            f2()
        } with Throw {
            def throw(_k) = ()
        };
        unreachable!()

    @Test
    def testLinear03(): Unit =
        def f1() = do Throw.throw();
        def f2() = { f1(); unreachable!() };
        def f3() = { f2(); unreachable!() };
        try {
            f3()
        } with Throw {
            def throw(_k) = ()
        };
        unreachable!()

    @Test
    def testRecursive01(): Unit =
        def f() = do Throw.throw();
        def r(x) = if (x == 0) f() else { r(x - 1); unreachable!() } ;
        try {
            r(0)
        } with Throw {
            def throw(_k) = ()
        };
        unreachable!()

    @Test
    def testRecursive02(): Unit =
        def f() = do Throw.throw();
        def r(x) = if (x == 0) f() else { r(x - 1); unreachable!() } ;
        try {
            r(1)
        } with Throw {
            def throw(_k) = ()
        };
        unreachable!()

    @Test
    def testRecursive03(): Unit =
        def f() = do Throw.throw();
        def r(x) = if (x == 0) f() else { r(x - 1); unreachable!() } ;
        try {
            r(10)
        } with Throw {
            def throw(_k) = ()
        };
        unreachable!()

}
