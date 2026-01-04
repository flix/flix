mod Test.Handler.OneShot.Float32 {

    use Assert.{assertEq, assertTrue};

    eff AskTell {
        def askTell(x: Float32): Float32
    }

    @Test
    def testLinear01(): Unit \ Assert =
        def f1() = AskTell.askTell(42.0f32);
        let result = run {
            f1()
        } with handler AskTell {
            def askTell(x, k) = k(x + x)
        };
        assertEq(expected = 84.0f32, result)

    @Test
    def testLinear02(): Unit \ Assert =
        def f1() = AskTell.askTell(42.0f32);
        def f2() = f1();
        let result = run {
            f2()
        } with handler AskTell {
            def askTell(x, k) = k(x + x)
        };
        assertEq(expected = 84.0f32, result)

    @Test
    def testLinear03(): Unit \ Assert =
        def f1() = AskTell.askTell(42.0f32);
        def f2() = f1();
        def f3() = f2();
        let result = run {
            f3()
        } with handler AskTell {
            def askTell(x, k) = k(x + x)
        };
        assertEq(expected = 84.0f32, result)

    @Test
    def testRecursiveLetRec01(): Unit \ Assert =
        def f() = AskTell.askTell(42.0f32);
        def r(x) = if (x == 0) f() else r(x - 1);
        let result = run {
            r(0)
        } with handler AskTell {
            def askTell(x, k) = k(x + x)
        };
        assertEq(expected = 84.0f32, result)

    @Test
    def testRecursiveLetRec02(): Unit \ Assert =
        def f() = AskTell.askTell(42.0f32);
        def r(x) = if (x == 0) f() else r(x - 1);
        let result = run {
            r(1)
        } with handler AskTell {
            def askTell(x, k) = k(x + x)
        };
        assertEq(expected = 84.0f32, result)

    @Test
    def testRecursiveLetRec03(): Unit \ Assert =
        def f1() = AskTell.askTell(42.0f32);
        def r(x) = if (x == 0) f1() else r(x - 1);
        let result = run {
            r(10)
        } with handler AskTell {
            def askTell(x, k) = k(x + x)
        };
        assertEq(expected = 84.0f32, result)

    @Test
    def testMutualRecursive01(): Unit \ Assert =
        let result = run {
            mutual1(0)
        } with handler AskTell {
            def askTell(x, k) = k(x + x)
        };
        assertEq(expected = 84.0f32, result)

    @Test
    def testMutualRecursive02(): Unit \ Assert =
        let result = run {
            mutual1(1)
        } with handler AskTell {
            def askTell(x, k) = k(x + x)
        };
        assertEq(expected = 84.0f32, result)

    @Test
    def testMutualRecursive03(): Unit \ Assert =
        let result = run {
            mutual1(2)
        } with handler AskTell {
            def askTell(x, k) = k(x + x)
        };
        assertEq(expected = 84.0f32, result)

    @Test
    def testMutualRecursive04(): Unit \ Assert =
        let result = run {
            mutual1(3)
        } with handler AskTell {
            def askTell(x, k) = k(x + x)
        };
        assertEq(expected = 84.0f32, result)

    @Test
    def testMutualRecursive05(): Unit \ Assert =
        let result = run {
            mutual1(10)
        } with handler AskTell {
            def askTell(x, k) = k(x + x)
        };
        assertEq(expected = 84.0f32, result)

    def f(): Float32 \ AskTell = AskTell.askTell(42.0f32)

    def mutual1(x: Int32): Float32 \ AskTell =
        if (x == 0) f() else mutual2(x - 1)

    def mutual2(x: Int32): Float32 \ AskTell =
        if (x == 0) f() else mutual1(x - 1)

}
