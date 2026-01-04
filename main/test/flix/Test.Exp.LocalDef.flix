mod Test.Exp.LocalDef {

    use Assert.assertEq

    ///
    /// Basic Tail Recursion.
    ///
    @Test
    def testLocalDef01(): Unit \ Assert =
        def loop(n, acc) = if (n == 0) acc else loop(n - 1, acc + 1);
        assertEq(expected = 10, loop(10, 0))

    @Test
    def testLocalDef02(): Unit \ Assert =
        def loop(acc, n) = if (n == 0) acc else loop(acc + 1, n - 1);
        assertEq(expected = 10, loop(0, 10))

    ///
    /// Partial Application.
    ///

    @Test
    def testLocalDefPartialApply01(): Unit \ Assert =
        def loop(n, acc) = if (n == 0) acc else loop(n - 1)(acc + 1);
        assertEq(expected = 10, loop(10)(0))

    @Test
    def testLocalDefPartialApply02(): Unit \ Assert =
        def loop(acc, n) = if (n == 0) acc else loop(acc + 1)(n - 1);
        assertEq(expected = 10, loop(0)(10))

    ///
    /// Capturing.
    ///

    @Test
    def testLocalDefCapture01(): Unit \ Assert =
        let a = 21;
        def loop(n) = if (n == 0) a else loop(n - 1);
        assertEq(expected = a, loop(10))

    @Test
    def testLocalDefCapture02(): Unit \ Assert =
        let a = 21;
        let b = 42;
        def loop(n) = if (n == 0) a + b else loop(n - 1);
        assertEq(expected = a + b, loop(00))

    @Test
    def testLocalDefCapture03(): Unit \ Assert =
        let inc = x -> x + 1;
        let dec = x -> x - 1;
        def loop(n) = if (n == 0) inc(0) else loop(dec(n));
        assertEq(expected = 1, loop(10))

    @Test
    def testLocalDefCapture04(): Unit \ Assert =
        def inc(x) = x + 1;
        def dec(x) = x - 1;
        def loop(n) = if (n == 0) inc(0) else loop(dec(n));
        assertEq(expected = 1, loop(10))

    @Test
    def testLocalDefCapture05(): Unit \ Assert =
        let a = 1;
        let b = 2;
        def f(c) = if (c == 2) () -> c + b else if (c == 1) f(b) else f(a);
        assertEq(expected = 4, f(10)())

    @Test
    def testLocalDefCapture06(): Unit \ Assert =
        let a = 1;
        let b = 2;
        def f(c) = if (c == 2) () -> c + b else if (c == 1) f(b) else f(a);
        assertEq(expected = 4, ((q) -> f(q))(10)())

    @Test
    def testLocalDefCapture07(): Unit \ Assert =
        let a = 1;
        let b = 2;
        def f(c) = if (c == 2) () -> c + b else if (c == 1) f(b) else f(a);
        assertEq(expected = 4, (() -> f)()(10)())

    @Test
    def testLocalDefCapture08(): Unit \ Assert =
        let a = 1;
        let b = 2;
        def f(c) = if (c == 2) () -> c + b else if (c == 1) f(b) else f(a);
        assertEq(expected = 4, (() -> f(b))()())

    @Test
    def testLocalDefCapture09(): Unit \ Assert =
        let a = 1;
        let b = 2;
        let g = {
            let d = 3;
            def f(l) = if (l == 4) () -> d + l else f(a + b + 1);
            () -> f
        };
        assertEq(expected = 7, g()(5)())

    @Test
    def testLocalDefCapture10(): Unit \ Assert =
        assertEq(expected = 1, foo(42, _ -> -1)())

    def foo(x: Int32, r: Unit -> Int32): Unit -> Int32 =
        def local() = x;
        if (x == 0) r else foo(x - 1, _ -> local())

    ///
    /// Nested Let Recs.
    ///
    @Test
    def testLocalDefNested01(): Unit \ Assert =
        def outer(n) = {
            def inner(m) = if (m == 0) 0 else inner(m - 1);
            inner(n)
        };
        assertEq(expected = 0, outer(10))

    @Test
    def testLocalDefNested02(): Unit \ Assert =
        def outer(n) = {
            def inner(m) = if (m == 0) n else inner(m - 1);
            inner(n)
        };
        assertEq(expected = 10, outer(10))

    @Test
    def testLocalDefNested03(): Unit \ Assert =
        def outer(n) = {
            def inc(x) = x + 1;
            def dec(x) = x - 1;
            def inner(m) = if (m == 0) inc(m) else inner(dec(m));
            inner(n)
        };
        assertEq(expected = 1, outer(10))

    ///
    /// Primitive Types.
    ///
    @Test
    def testLocalDefUnit01(): Unit \ Assert =
        def loop(n) = if (n == 0) () else loop(n - 1);
        assertEq(expected = (), loop(10))

    @Test
    def testLocalDefBool01(): Unit \ Assert =
        def loop(n) = if (n == 0) true else loop(n - 1);
        assertEq(expected = true, loop(10))

    @Test
    def testLocalDefChar01(): Unit \ Assert =
        def loop(n) = if (n == 0) 'a' else loop(n - 1);
        assertEq(expected = 'a', loop(10))

    @Test
    def testLocalDefFloat3201(): Unit \ Assert =
        def loop(n) = if (n == 0) 123.456f32 else loop(n - 1);
        assertEq(expected = 123.456f32, loop(10))

    @Test
    def testLocalDefFloat6401(): Unit \ Assert =
        def loop(n) = if (n == 0) 123.456f64 else loop(n - 1);
        assertEq(expected = 123.456f64, loop(10))

    @Test
    def testLocalDefBigDecimal01(): Unit \ Assert =
        def loop(n) = if (n == 0) 123.456ff else loop(n - 1);
        assertEq(expected = 123.456ff, loop(10))

    @Test
    def testLocalDefInt801(): Unit \ Assert =
        def loop(n) = if (n == 0) 123i8 else loop(n - 1);
        assertEq(expected = 123i8, loop(10))

    @Test
    def testLocalDefInt1601(): Unit \ Assert =
        def loop(n) = if (n == 0) 123i16 else loop(n - 1);
        assertEq(expected = 123i16, loop(10))

    @Test
    def testLocalDefInt3201(): Unit \ Assert =
        def loop(n) = if (n == 0) 123i32 else loop(n - 1);
        assertEq(expected = 123i32, loop(10))

    @Test
    def testLocalDefInt6401(): Unit \ Assert =
        def loop(n) = if (n == 0) 123i64 else loop(n - 1);
        assertEq(expected = 123i64, loop(10))

    @Test
    def testLocalDefBigInt01(): Unit \ Assert =
        def loop(n) = if (n == 0) 123ii else loop(n - 1);
        assertEq(expected = 123ii, loop(10))

    @Test
    def testLocalDefString01(): Unit \ Assert =
        def loop(n) = if (n == 0) "hello" else loop(n - 1);
        assertEq(expected = "hello", loop(10))

    ///
    /// Symbolic nested definitions
    ///
    @Test
    def testLocalDefSymbol01(): Unit \ Assert =
        def <><(a, b) = a + " <>< " + b;
        assertEq(expected = "left <>< right", "left" <>< "right")

    ///
    /// Nested definitions with type ascription
    ///
    @Test
    def testLocalDefAscribed01(): Unit \ Assert =
        def succ(x): Int32 = x + 1;
        assertEq(expected = 2, succ(1))

    ///
    /// Nested definitions with type ascription
    ///
    @Test
    def testLocalDefAscribed02(): Unit \ Assert =
        def succ(x): Int32 \ {} = x + 1;
        assertEq(expected = 2, succ(1))

    ///
    /// Nested definitions with type ascription
    ///
    @Test
    def testLocalDefAscribed03(): Unit \ Assert = region rc {
        def incr(x): Unit \ rc = Ref.put(Ref.get(x) + 1, x);
        let r = Ref.fresh(rc, 1);
        incr(r);
        assertEq(expected = 2, Ref.get(r))
    }
}
