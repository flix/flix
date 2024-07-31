mod Test.Handler.ZeroShot.AtomicInteger {

    import java.util.concurrent.atomic.AtomicInteger

    def newAtomicInteger(n: Int32): AtomicInteger =
        unsafe new AtomicInteger(n)

    def toInt(n: AtomicInteger): Int32 =
        unsafe n.intValue()

    eff Throw {
        pub def throw(): AtomicInteger
    }

    @Test
    def testLinear01(): Bool =
        def f1() = do Throw.throw();
        let result = try {
            f1()
        } with Throw {
            def throw(_k) = newAtomicInteger(42)
        };
        Assert.eq(toInt(newAtomicInteger(42)), toInt(result))

    @Test
    def testLinear02(): Bool =
        def f1() = do Throw.throw();
        def f2() = { f1(); unreachable!() };
        let result = try {
            f2()
        } with Throw {
            def throw(_k) = newAtomicInteger(42)
        };
        Assert.eq(toInt(newAtomicInteger(42)), toInt(result))

    @Test
    def testLinear03(): Bool =
        def f1() = do Throw.throw();
        def f2() = { f1(); unreachable!() };
        def f3() = { f2(); unreachable!() };
        let result = try {
            f3()
        } with Throw {
            def throw(_k) = newAtomicInteger(42)
        };
        Assert.eq(toInt(newAtomicInteger(42)), toInt(result))

    @Test
    def testRecursiveLetRec01(): Bool =
        def f() = do Throw.throw();
        def r(x) = if (x == 0) f() else { r(x - 1); unreachable!() } ;
        let result = try {
            r(0)
        } with Throw {
            def throw(_k) = newAtomicInteger(42)
        };
        Assert.eq(toInt(newAtomicInteger(42)), toInt(result))

    @Test
    def testRecursiveLetRec02(): Bool =
        def f() = do Throw.throw();
        def r(x) = if (x == 0) f() else { r(x - 1); unreachable!() } ;
        let result = try {
            r(1)
        } with Throw {
            def throw(_k) = newAtomicInteger(42)
        };
        Assert.eq(toInt(newAtomicInteger(42)), toInt(result))

    @Test
    def testRecursiveLetRec03(): Bool =
        def f() = do Throw.throw();
        def r(x) = if (x == 0) f() else { r(x - 1); unreachable!() } ;
        let result = try {
            r(10)
        } with Throw {
            def throw(_k) = newAtomicInteger(42)
        };
        Assert.eq(toInt(newAtomicInteger(42)), toInt(result))

    @Test
    def testMutualRecursive01(): Bool =
        let result = try {
            mutual1(0)
        } with Throw {
            def throw(_k) = newAtomicInteger(42)
        };
        Assert.eq(toInt(newAtomicInteger(42)), toInt(result))

    @Test
    def testMutualRecursive02(): Bool =
        let result = try {
            mutual1(1)
        } with Throw {
            def throw(_k) = newAtomicInteger(42)
        };
        Assert.eq(toInt(newAtomicInteger(42)), toInt(result))

    @Test
    def testMutualRecursive03(): Bool =
        let result = try {
            mutual1(2)
        } with Throw {
            def throw(_k) = newAtomicInteger(42)
        };
        Assert.eq(toInt(newAtomicInteger(42)), toInt(result))

    @Test
    def testMutualRecursive04(): Bool =
        let result = try {
            mutual1(3)
        } with Throw {
            def throw(_k) = newAtomicInteger(42)
        };
        Assert.eq(toInt(newAtomicInteger(42)), toInt(result))

    @Test
    def testMutualRecursive05(): Bool =
        let result = try {
            mutual1(10)
        } with Throw {
            def throw(_k) = newAtomicInteger(42)
        };
        Assert.eq(toInt(newAtomicInteger(42)), toInt(result))

    @Test
    def testMutualRecursive06(): Bool =
        let result = try {
            mutual1(10)
        } with Throw {
            def throw(_k) = newAtomicInteger(40)
        };
        Assert.eq(toInt(newAtomicInteger(40)), toInt(result))

    def f(): AtomicInteger \ Throw =
         do Throw.throw();
         unreachable!()

    def mutual1(x: Int32): AtomicInteger \ Throw =
        if (x == 0) f() else { mutual2(x - 1); unreachable!() }

    def mutual2(x: Int32): AtomicInteger \ Throw =
        if (x == 0) f() else { mutual1(x - 1); unreachable!() }

}
