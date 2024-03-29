mod Test.Handler.OneShot.Char {

    eff AskTell {
        pub def askTell(x: Char): Char
    }

    @Test
    def testLinear01(): Bool =
        def f1() = do AskTell.askTell('a');
        let result = try {
            f1()
        } with AskTell {
            def askTell(x, k) = k(next(x))
        };
        Assert.eq('b', result)

    @Test
    def testLinear02(): Bool =
        def f1() = do AskTell.askTell('a');
        def f2() = f1();
        let result = try {
            f2()
        } with AskTell {
            def askTell(x, k) = k(next(x))
        };
        Assert.eq('b', result)

    @Test
    def testLinear03(): Bool =
        def f1() = do AskTell.askTell('a');
        def f2() = f1();
        def f3() = f2();
        let result = try {
            f3()
        } with AskTell {
            def askTell(x, k) = k(next(x))
        };
        Assert.eq('b', result)

    @Test
    def testRecursiveLetRec01(): Bool =
        def f() = do AskTell.askTell('a');
        def r(x) = if (x == 0) f() else r(x - 1);
        let result = try {
            r(0)
        } with AskTell {
            def askTell(x, k) = k(next(x))
        };
        Assert.eq('b', result)

    @Test
    def testRecursiveLetRec02(): Bool =
        def f() = do AskTell.askTell('a');
        def r(x) = if (x == 0) f() else r(x - 1);
        let result = try {
            r(1)
        } with AskTell {
            def askTell(x, k) = k(next(x))
        };
        Assert.eq('b', result)

    @Test
    def testRecursiveLetRec03(): Bool =
        def f1() = do AskTell.askTell('a');
        def r(x) = if (x == 0) f1() else r(x - 1);
        let result = try {
            r(10)
        } with AskTell {
            def askTell(x, k) = k(next(x))
        };
        Assert.eq('b', result)

    @Test
    def testMutualRecursive01(): Bool =
        let result = try {
            mutual1(0)
        } with AskTell {
            def askTell(x, k) = k(next(x))
        };
        Assert.eq('b', result)

    @Test
    def testMutualRecursive02(): Bool =
        let result = try {
            mutual1(1)
        } with AskTell {
            def askTell(x, k) = k(next(x))
        };
        Assert.eq('b', result)

    @Test
    def testMutualRecursive03(): Bool =
        let result = try {
            mutual1(2)
        } with AskTell {
            def askTell(x, k) = k(next(x))
        };
        Assert.eq('b', result)

    @Test
    def testMutualRecursive04(): Bool =
        let result = try {
            mutual1(3)
        } with AskTell {
            def askTell(x, k) = k(next(x))
        };
        Assert.eq('b', result)

    @Test
    def testMutualRecursive05(): Bool =
        let result = try {
            mutual1(10)
        } with AskTell {
            def askTell(x, k) = k(next(x))
        };
        Assert.eq('b', result)

    def f(): Char \ AskTell = do AskTell.askTell('a')

    def mutual1(x: Int32): Char \ AskTell =
        if (x == 0) f() else mutual2(x - 1)

    def mutual2(x: Int32): Char \ AskTell =
        if (x == 0) f() else mutual1(x - 1)

    def next(c: Char): Char = match c {
        case 'a' => 'b'
        case 'b' => 'a'
        case _   => unreachable!()
    }

}
