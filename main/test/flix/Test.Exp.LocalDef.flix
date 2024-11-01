mod Test.Exp.LocalDef {

    ///
    /// Basic Tail Recursion.
    ///
    @test
    def testLocalDef01(): Bool =
        def loop(n, acc) = if (n == 0) acc else loop(n - 1, acc + 1);
        loop(10, 0) == 10

    @test
    def testLocalDef02(): Bool =
        def loop(acc, n) = if (n == 0) acc else loop(acc + 1, n - 1);
        loop(0, 10) == 10

    ///
    /// Partial Application.
    ///

    @test
    def testLocalDefPartialApply01(): Bool =
        def loop(n, acc) = if (n == 0) acc else loop(n - 1)(acc + 1);
        loop(10)(0) == 10

    @test
    def testLocalDefPartialApply02(): Bool =
        def loop(acc, n) = if (n == 0) acc else loop(acc + 1)(n - 1);
        loop(0)(10) == 10

    ///
    /// Capturing.
    ///

    @test
    def testLocalDefCapture01(): Bool =
        let a = 21;
        def loop(n) = if (n == 0) a else loop(n - 1);
        loop(10) == a

    @test
    def testLocalDefCapture02(): Bool =
        let a = 21;
        let b = 42;
        def loop(n) = if (n == 0) a + b else loop(n - 1);
        loop(00) == a + b

    @test
    def testLocalDefCapture03(): Bool =
        let inc = x -> x + 1;
        let dec = x -> x - 1;
        def loop(n) = if (n == 0) inc(0) else loop(dec(n));
        loop(10) == 1

    @test
    def testLocalDefCapture04(): Bool =
        def inc(x) = x + 1;
        def dec(x) = x - 1;
        def loop(n) = if (n == 0) inc(0) else loop(dec(n));
        loop(10) == 1

    @test
    def testLocalDefCapture05(): Bool =
        let a = 1;
        let b = 2;
        def f(c) = if (c == 2) () -> c + b else if (c == 1) f(b) else f(a);
        f(10)() == 4

    @test
    def testLocalDefCapture06(): Bool =
        let a = 1;
        let b = 2;
        def f(c) = if (c == 2) () -> c + b else if (c == 1) f(b) else f(a);
        ((q) -> f(q))(10)() == 4

    @test
    def testLocalDefCapture07(): Bool =
        let a = 1;
        let b = 2;
        def f(c) = if (c == 2) () -> c + b else if (c == 1) f(b) else f(a);
        (() -> f)()(10)() == 4

    @test
    def testLocalDefCapture08(): Bool =
        let a = 1;
        let b = 2;
        def f(c) = if (c == 2) () -> c + b else if (c == 1) f(b) else f(a);
        (() -> f(b))()() == 4

    @test
    def testLocalDefCapture09(): Bool =
        let a = 1;
        let b = 2;
        let g = {
            let d = 3;
            def f(l) = if (l == 4) () -> d + l else f(a + b + 1);
            () -> f
        };
        g()(5)() == 7

    @test
    def testLocalDefCapture10(): Bool =
        foo(42, _ -> -1)() == 1

    def foo(x: Int32, r: Unit -> Int32): Unit -> Int32 =
        def local() = x;
        if (x == 0) r else foo(x - 1, _ -> local())

    ///
    /// Nested Let Recs.
    ///
    @test
    def testLocalDefNested01(): Bool =
        def outer(n) = {
            def inner(m) = if (m == 0) 0 else inner(m - 1);
            inner(n)
        };
        outer(10) == 0

    @test
    def testLocalDefNested02(): Bool =
        def outer(n) = {
            def inner(m) = if (m == 0) n else inner(m - 1);
            inner(n)
        };
        outer(10) == 10

    @test
    def testLocalDefNested03(): Bool =
        def outer(n) = {
            def inc(x) = x + 1;
            def dec(x) = x - 1;
            def inner(m) = if (m == 0) inc(m) else inner(dec(m));
            inner(n)
        };
        outer(10) == 1

    ///
    /// Primitive Types.
    ///
    @test
    def testLocalDefUnit01(): Bool =
        def loop(n) = if (n == 0) () else loop(n - 1);
        loop(10) == ()

    @test
    def testLocalDefBool01(): Bool =
        def loop(n) = if (n == 0) true else loop(n - 1);
        loop(10) == true

    @test
    def testLocalDefChar01(): Bool =
        def loop(n) = if (n == 0) 'a' else loop(n - 1);
        loop(10) == 'a'

    @test
    def testLocalDefFloat3201(): Bool =
        def loop(n) = if (n == 0) 123.456f32 else loop(n - 1);
        loop(10) == 123.456f32

    @test
    def testLocalDefFloat6401(): Bool =
        def loop(n) = if (n == 0) 123.456f64 else loop(n - 1);
        loop(10) == 123.456f64

    @test
    def testLocalDefBigDecimal01(): Bool =
        def loop(n) = if (n == 0) 123.456ff else loop(n - 1);
        loop(10) == 123.456ff

    @test
    def testLocalDefInt801(): Bool =
        def loop(n) = if (n == 0) 123i8 else loop(n - 1);
        loop(10) == 123i8

    @test
    def testLocalDefInt1601(): Bool =
        def loop(n) = if (n == 0) 123i16 else loop(n - 1);
        loop(10) == 123i16

    @test
    def testLocalDefInt3201(): Bool =
        def loop(n) = if (n == 0) 123i32 else loop(n - 1);
        loop(10) == 123i32

    @test
    def testLocalDefInt6401(): Bool =
        def loop(n) = if (n == 0) 123i64 else loop(n - 1);
        loop(10) == 123i64

    @test
    def testLocalDefBigInt01(): Bool =
        def loop(n) = if (n == 0) 123ii else loop(n - 1);
        loop(10) == 123ii

    @test
    def testLocalDefString01(): Bool =
        def loop(n) = if (n == 0) "hello" else loop(n - 1);
        loop(10) == "hello"

    ///
    /// Symbolic nested definitions
    ///
    @test
    def testLocalDefSymbol01(): Bool =
        def <><(a, b) = a + " <>< " + b;
        "left" <>< "right" == "left <>< right"

    ///
    /// Nested definitions with type ascription
    ///
    @test
    def testLocalDefAscribed01(): Bool =
        def succ(x): Int32 = x + 1;
        succ(1) == 2

    ///
    /// Nested definitions with type ascription
    ///
    @test
    def testLocalDefAscribed02(): Bool =
        def succ(x): Int32 \ {} = x + 1;
        succ(1) == 2

    ///
    /// Nested definitions with type ascription
    ///
    @test
    def testLocalDefAscribed03(): Bool = region rc {
        def incr(x): Unit \ rc = Ref.put(Ref.get(x) + 1, x);
        let r = Ref.fresh(rc, 1);
        incr(r);
        Ref.get(r) == 2
    }
}
