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
    def testRecursiveLetRec01(): Unit =
        def f() = do Throw.throw();
        def r(x) = if (x == 0) f() else { r(x - 1); unreachable!() } ;
        try {
            r(0)
        } with Throw {
            def throw(_k) = ()
        };
        unreachable!()

    @Test
    def testRecursiveLetRec02(): Unit =
        def f() = do Throw.throw();
        def r(x) = if (x == 0) f() else { r(x - 1); unreachable!() } ;
        try {
            r(1)
        } with Throw {
            def throw(_k) = ()
        };
        unreachable!()

    @Test
    def testRecursiveLetRec03(): Unit =
        def f() = do Throw.throw();
        def r(x) = if (x == 0) f() else { r(x - 1); unreachable!() } ;
        try {
            r(10)
        } with Throw {
            def throw(_k) = ()
        };
        unreachable!()

    @Test
    def testMutualRecursive01(): Unit =
        try {
            mutual1(0)
        } with Throw {
            def throw(_k) = ()
        };
        unreachable!()

    @Test
    def testMutualRecursive02(): Unit =
        try {
            mutual1(1)
        } with Throw {
            def throw(_k) = ()
        };
        unreachable!()

    @Test
    def testMutualRecursive03(): Unit =
        try {
            mutual1(2)
        } with Throw {
            def throw(_k) = ()
        };
        unreachable!()

    @Test
    def testMutualRecursive04(): Unit =
        try {
            mutual1(3)
        } with Throw {
            def throw(_k) = ()
        };
        unreachable!()

    @Test
    def testMutualRecursive05(): Unit =
        try {
            mutual1(10)
        } with Throw {
            def throw(_k) = ()
        };
        unreachable!()

    def f(): Unit \ Throw =
         do Throw.throw();
         unreachable!()

    def mutual1(x: Int32): Unit \ Throw =
        if (x == 0) f() else { mutual2(x - 1); unreachable!() }

    def mutual2(x: Int32): Unit \ Throw =
        if (x == 0) f() else { mutual1(x - 1); unreachable!() }

}
