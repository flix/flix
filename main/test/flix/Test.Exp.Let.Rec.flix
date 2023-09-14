mod Test.Exp.LetRec {

    ///
    /// Basic Tail Recursion.
    ///
    @Test
    def testLetRec01(): Bool =
        def loop(n, acc) = if (n == 0) acc else loop(n - 1, acc + 1);
        loop(10, 0) == 10

    @Test
    def testLetRec02(): Bool =
        def loop(acc, n) = if (n == 0) acc else loop(acc + 1, n - 1);
        loop(0, 10) == 10

    ///
    /// Partial Application.
    ///

    @Test
    def testLetRecPartialApply01(): Bool =
        def loop(n, acc) = if (n == 0) acc else loop(n - 1)(acc + 1);
        loop(10)(0) == 10

    @Test
    def testLetRecPartialApply02(): Bool =
        def loop(acc, n) = if (n == 0) acc else loop(acc + 1)(n - 1);
        loop(0)(10) == 10

    ///
    /// Capturing.
    ///

    @Test
    def testLetRecCapture01(): Bool =
        let a = 21;
        def loop(n) = if (n == 0) a else loop(n - 1);
        loop(10) == a

    @Test
    def testLetRecCapture02(): Bool =
        let a = 21;
        let b = 42;
        def loop(n) = if (n == 0) a + b else loop(n - 1);
        loop(00) == a + b

    @Test
    def testLetRecCapture03(): Bool =
        let inc = x -> x + 1;
        let dec = x -> x - 1;
        def loop(n) = if (n == 0) inc(0) else loop(dec(n));
        loop(10) == 1

    @Test
    def testLetRecCapture04(): Bool =
        def inc(x) = x + 1;
        def dec(x) = x - 1;
        def loop(n) = if (n == 0) inc(0) else loop(dec(n));
        loop(10) == 1

    ///
    /// Nested Let Recs.
    ///
    @Test
    def testLetRecNested01(): Bool =
        def outer(n) = {
            def inner(m) = if (m == 0) 0 else inner(m - 1);
            inner(n)
        };
        outer(10) == 0

    @Test
    def testLetRecNested02(): Bool =
        def outer(n) = {
            def inner(m) = if (m == 0) n else inner(m - 1);
            inner(n)
        };
        outer(10) == 10

    @Test
    def testLetRecNested03(): Bool =
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
    @Test
    def testLetRecUnit01(): Bool =
        def loop(n) = if (n == 0) () else loop(n - 1);
        loop(10) == ()

    @Test
    def testLetRecBool01(): Bool =
        def loop(n) = if (n == 0) true else loop(n - 1);
        loop(10) == true

    @Test
    def testLetRecChar01(): Bool =
        def loop(n) = if (n == 0) 'a' else loop(n - 1);
        loop(10) == 'a'

    @Test
    def testLetRecFloat3201(): Bool =
        def loop(n) = if (n == 0) 123.456f32 else loop(n - 1);
        loop(10) == 123.456f32

    @Test
    def testLetRecFloat6401(): Bool =
        def loop(n) = if (n == 0) 123.456f64 else loop(n - 1);
        loop(10) == 123.456f64

    @Test
    def testLetRecBigDecimal01(): Bool =
        def loop(n) = if (n == 0) 123.456ff else loop(n - 1);
        loop(10) == 123.456ff

    @Test
    def testLetRecInt801(): Bool =
        def loop(n) = if (n == 0) 123i8 else loop(n - 1);
        loop(10) == 123i8

    @Test
    def testLetRecInt1601(): Bool =
        def loop(n) = if (n == 0) 123i16 else loop(n - 1);
        loop(10) == 123i16

    @Test
    def testLetRecInt3201(): Bool =
        def loop(n) = if (n == 0) 123i32 else loop(n - 1);
        loop(10) == 123i32

    @Test
    def testLetRecInt6401(): Bool =
        def loop(n) = if (n == 0) 123i64 else loop(n - 1);
        loop(10) == 123i64

    @Test
    def testLetRecBigInt01(): Bool =
        def loop(n) = if (n == 0) 123ii else loop(n - 1);
        loop(10) == 123ii

    @Test
    def testLetRecString01(): Bool =
        def loop(n) = if (n == 0) "hello" else loop(n - 1);
        loop(10) == "hello"

    ///
    /// Symbolic nested definitions
    ///
    @Test
    def testLetRecSymbol01(): Bool =
        def <><(a, b) = a + " <>< " + b;
        "left" <>< "right" == "left <>< right"

    ///
    /// Nested definitions with type ascription
    ///
    @Test
    def testLetRecAscribed01(): Bool =
        def succ(x): Int32 = x + 1;
        succ(1) == 2

    ///
    /// Nested definitions with type ascription
    ///
    @Test
    def testLetRecAscribed02(): Bool =
        def succ(x): Int32 \ {} = x + 1;
        succ(1) == 2

    ///
    /// Nested definitions with type ascription
    ///
    @Test
    def testLetRecAscribed03(): Bool = region rc {
        def incr(x): Unit \ rc = x := deref x + 1;
        let r = ref 1 @ rc;
        incr(r);
        deref r == 2
    }
}
