mod Test.Handler.OneShot.AtomicInteger {

    use Assert.{assertEq, assertTrue};

    import java.util.concurrent.atomic.AtomicInteger

    def newAtomicInteger(n: Int32): AtomicInteger =
        unsafe IO { new AtomicInteger(n) }

    def toInt(n: AtomicInteger): Int32 =
        unsafe IO { n.intValue() }

    eff Ask {
        def ask(x: AtomicInteger): AtomicInteger
    }

    @Test
    def testLinear01(): Unit \ Assert =
        def f1() = Ask.ask(newAtomicInteger(42));
        let result = run {
            f1()
        } with handler Ask {
            def ask(x, k) = k(newAtomicInteger(toInt(x) + toInt(x)))
        };
        assertEq(expected = 84, toInt(result))

    @Test
    def testLinear02(): Unit \ Assert =
        def f1() = Ask.ask(newAtomicInteger(42));
        def f2() = f1();
        let result = run {
            f2()
        } with handler Ask {
            def ask(x, k) = k(newAtomicInteger(toInt(x) + toInt(x)))
        };
        assertEq(expected = 84, toInt(result))

    @Test
    def testLinear03(): Unit \ Assert =
        def f1() = Ask.ask(newAtomicInteger(42));
        def f2() = f1();
        def f3() = f2();
        let result = run {
            f3()
        } with handler Ask {
            def ask(x, k) = k(newAtomicInteger(toInt(x) + toInt(x)))
        };
        assertEq(expected = 84, toInt(result))

    @Test
    def testRecursiveLetRec01(): Unit \ Assert =
        def f() = Ask.ask(newAtomicInteger(42));
        def r(x) = if (x == 0) f() else r(x - 1);
        let result = run {
            r(0)
        } with handler Ask {
            def ask(x, k) = k(newAtomicInteger(toInt(x) + toInt(x)))
        };
        assertEq(expected = 84, toInt(result))

    @Test
    def testRecursiveLetRec02(): Unit \ Assert =
        def f() = Ask.ask(newAtomicInteger(42));
        def r(x) = if (x == 0) f() else r(x - 1);
        let result = run {
            r(1)
        } with handler Ask {
            def ask(x, k) = k(newAtomicInteger(toInt(x) + toInt(x)))
        };
        assertEq(expected = 84, toInt(result))

    @Test
    def testRecursiveLetRec03(): Unit \ Assert =
        def f1() = Ask.ask(newAtomicInteger(42));
        def r(x) = if (x == 0) f1() else r(x - 1);
        let result = run {
            r(10)
        } with handler Ask {
            def ask(x, k) = k(newAtomicInteger(toInt(x) + toInt(x)))
        };
        assertEq(expected = 84, toInt(result))

    @Test
    def testMutualRecursive01(): Unit \ Assert =
        let result = run {
            mutual1(0)
        } with handler Ask {
            def ask(x, k) = k(newAtomicInteger(toInt(x) + toInt(x)))
        };
        assertEq(expected = 84, toInt(result))

    @Test
    def testMutualRecursive02(): Unit \ Assert =
        let result = run {
            mutual1(1)
        } with handler Ask {
            def ask(x, k) = k(newAtomicInteger(toInt(x) + toInt(x)))
        };
        assertEq(expected = 84, toInt(result))

    @Test
    def testMutualRecursive03(): Unit \ Assert =
        let result = run {
            mutual1(2)
        } with handler Ask {
            def ask(x, k) = k(newAtomicInteger(toInt(x) + toInt(x)))
        };
        assertEq(expected = 84, toInt(result))

    @Test
    def testMutualRecursive04(): Unit \ Assert =
        let result = run {
            mutual1(3)
        } with handler Ask {
            def ask(x, k) = k(newAtomicInteger(toInt(x) + toInt(x)))
        };
        assertEq(expected = 84, toInt(result))

    @Test
    def testMutualRecursive05(): Unit \ Assert =
        let result = run {
            mutual1(10)
        } with handler Ask {
            def ask(x, k) = k(newAtomicInteger(toInt(x) + toInt(x)))
        };
        assertEq(expected = 84, toInt(result))

    def f(): AtomicInteger \ Ask = Ask.ask(newAtomicInteger(42))

    def mutual1(x: Int32): AtomicInteger \ Ask =
        if (x == 0) f() else mutual2(x - 1)

    def mutual2(x: Int32): AtomicInteger \ Ask =
        if (x == 0) f() else mutual1(x - 1)

}
