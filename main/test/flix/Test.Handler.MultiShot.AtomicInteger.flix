mod Test.Handler.MultiShot.AtomicInteger {

    import java.util.concurrent.atomic.AtomicInteger

    def newAtomicInteger(n: Int32): AtomicInteger =
        unsafe new AtomicInteger(n)

    def toInt(n: AtomicInteger): Int32 =
        unsafe n.intValue()

    eff Ask {
        pub def ask(x: AtomicInteger): AtomicInteger
    }

    @Test
    def testLinear01(): Bool =
        def f1() = do Ask.ask(newAtomicInteger(42));
        let result = try {
            f1()
        } with Ask {
            def ask(x, k) = newAtomicInteger(toInt(k(x)) + toInt(k(x)))
        };
        Assert.eq(84, toInt(result))

    @Test
    def testLinear02(): Bool =
        def f1() = do Ask.ask(newAtomicInteger(42));
        def f2() = f1();
        let result = try {
            f2()
        } with Ask {
            def ask(x, k) = newAtomicInteger(toInt(k(x)) + toInt(k(x)))
        };
        Assert.eq(84, toInt(result))

    @Test
    def testLinear03(): Bool =
        def f1() = do Ask.ask(newAtomicInteger(42));
        def f2() = f1();
        def f3() = f2();
        let result = try {
            f3()
        } with Ask {
            def ask(x, k) = newAtomicInteger(toInt(k(x)) + toInt(k(x)))
        };
        Assert.eq(84, toInt(result))

    @Test
    def testRecursiveLetRec01(): Bool =
        def f() = do Ask.ask(newAtomicInteger(42));
        def r(x) = if (x == 0) f() else r(x - 1);
        let result = try {
            r(0)
        } with Ask {
            def ask(x, k) = newAtomicInteger(toInt(k(x)) + toInt(k(x)))
        };
        Assert.eq(84, toInt(result))

    @Test
    def testRecursiveLetRec02(): Bool =
        def f() = do Ask.ask(newAtomicInteger(42));
        def r(x) = if (x == 0) f() else r(x - 1);
        let result = try {
            r(1)
        } with Ask {
            def ask(x, k) = newAtomicInteger(toInt(k(x)) + toInt(k(x)))
        };
        Assert.eq(84, toInt(result))

    @Test
    def testRecursiveLetRec03(): Bool =
        def f1() = do Ask.ask(newAtomicInteger(42));
        def r(x) = if (x == 0) f1() else r(x - 1);
        let result = try {
            r(10)
        } with Ask {
            def ask(x, k) = newAtomicInteger(toInt(k(x)) + toInt(k(x)))
        };
        Assert.eq(84, toInt(result))

    @Test
    def testMutualRecursive01(): Bool =
        let result = try {
            mutual1(0)
        } with Ask {
            def ask(x, k) = newAtomicInteger(toInt(k(x)) + toInt(k(x)))
        };
        Assert.eq(84, toInt(result))

    @Test
    def testMutualRecursive02(): Bool =
        let result = try {
            mutual1(1)
        } with Ask {
            def ask(x, k) = newAtomicInteger(toInt(k(x)) + toInt(k(x)))
        };
        Assert.eq(84, toInt(result))

    @Test
    def testMutualRecursive03(): Bool =
        let result = try {
            mutual1(2)
        } with Ask {
            def ask(x, k) = newAtomicInteger(toInt(k(x)) + toInt(k(x)))
        };
        Assert.eq(84, toInt(result))

    @Test
    def testMutualRecursive04(): Bool =
        let result = try {
            mutual1(3)
        } with Ask {
            def ask(x, k) = newAtomicInteger(toInt(k(x)) + toInt(k(x)))
        };
        Assert.eq(84, toInt(result))

    @Test
    def testMutualRecursive05(): Bool =
        let result = try {
            mutual1(10)
        } with Ask {
            def ask(x, k) = newAtomicInteger(toInt(k(x)) + toInt(k(x)))
        };
        Assert.eq(84, toInt(result))

    def f(): AtomicInteger \ Ask = do Ask.ask(newAtomicInteger(42))

    def mutual1(x: Int32): AtomicInteger \ Ask =
        if (x == 0) f() else mutual2(x - 1)

    def mutual2(x: Int32): AtomicInteger \ Ask =
        if (x == 0) f() else mutual1(x - 1)

}
