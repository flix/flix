mod Test.Handler.ZeroShot.Int8 {

    eff Throw {
        pub def throw(): Int8
    }

    @Test
    def testLinear01(): Bool =
        def f1() = do Throw.throw();
        let result = try {
            f1()
        } with Throw {
            def throw(_k) = 42i8
        };
        Assert.eq(42i8, result)

    @Test
    def testLinear02(): Bool =
        def f1() = do Throw.throw();
        def f2() = { f1(); unreachable!() };
        let result = try {
            f2()
        } with Throw {
            def throw(_k) = 42i8
        };
        Assert.eq(42i8, result)

    @Test
    def testLinear03(): Bool =
        def f1() = do Throw.throw();
        def f2() = { f1(); unreachable!() };
        def f3() = { f2(); unreachable!() };
        let result = try {
            f3()
        } with Throw {
            def throw(_k) = 42i8
        };
        Assert.eq(42i8, result)

    @Test
    def testRecursiveLetRec01(): Bool =
        def f() = do Throw.throw();
        def r(x) = if (x == 0i8) f() else { r(x - 1i8); unreachable!() } ;
        let result = try {
            r(0i8)
        } with Throw {
            def throw(_k) = 42i8
        };
        Assert.eq(42i8, result)

    @Test
    def testRecursiveLetRec02(): Bool =
        def f() = do Throw.throw();
        def r(x) = if (x == 0i8) f() else { r(x - 1i8); unreachable!() } ;
        let result = try {
            r(1i8)
        } with Throw {
            def throw(_k) = 42i8
        };
        Assert.eq(42i8, result)

    @Test
    def testRecursiveLetRec03(): Bool =
        def f() = do Throw.throw();
        def r(x) = if (x == 0i8) f() else { r(x - 1i8); unreachable!() } ;
        let result = try {
            r(10i8)
        } with Throw {
            def throw(_k) = 42i8
        };
        Assert.eq(42i8, result)

    @Test
    def testMutualRecursive01(): Bool =
        let result = try {
            mutual1(0i8)
        } with Throw {
            def throw(_k) = 42i8
        };
        Assert.eq(42i8, result)

    @Test
    def testMutualRecursive02(): Bool =
        let result = try {
            mutual1(1i8)
        } with Throw {
            def throw(_k) = 42i8
        };
        Assert.eq(42i8, result)

    @Test
    def testMutualRecursive03(): Bool =
        let result = try {
            mutual1(2i8)
        } with Throw {
            def throw(_k) = 42i8
        };
        Assert.eq(42i8, result)

    @Test
    def testMutualRecursive04(): Bool =
        let result = try {
            mutual1(3i8)
        } with Throw {
            def throw(_k) = 42i8
        };
        Assert.eq(42i8, result)

    @Test
    def testMutualRecursive05(): Bool =
        let result = try {
            mutual1(10i8)
        } with Throw {
            def throw(_k) = 42i8
        };
        Assert.eq(42i8, result)

    def f(): Int8 \ Throw =
         do Throw.throw();
         unreachable!()

    def mutual1(x: Int8): Int8 \ Throw =
        if (x == 0i8) f() else { mutual2(x - 1i8); unreachable!() }

    def mutual2(x: Int8): Int8 \ Throw =
        if (x == 0i8) f() else { mutual1(x - 1i8); unreachable!() }

}
