mod Test.DefaultHandler.Poly {

    eff Emit[t] {
        def emit(x: t): Unit
    }

    mod Emit {
        @DefaultHandler
        pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Emit[t]) + IO =
            run f() with handler Emit {
                def emit(_x, k) = k()
            }
    }

    @Test
    def testEmitInt32(): Unit \ Emit[Int32] =
        Emit.emit(42)

    @Test
    def testEmitString(): Unit \ Emit[String] =
        Emit.emit("hello")

    eff Echo[t] {
        def echo(x: t): t
    }

    mod Echo {
        @DefaultHandler
        pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Echo[t]) + IO =
            run f() with handler Echo {
                def echo(x, k) = k(x)
            }
    }

    @Test
    def testEchoInt32(): Unit \ Assert + Echo[Int32] =
        Assert.assertEq(expected = 42, Echo.echo(42))

    @Test
    def testEchoString(): Unit \ Assert + Echo[String] =
        Assert.assertEq(expected = "hello", Echo.echo("hello"))

    eff Pair[a, b] {
        def first(x: a, y: b): a
    }

    mod Pair {
        @DefaultHandler
        pub def runWithIO(f: Unit -> r \ ef): r \ (ef - Pair[a, b]) + IO =
            run f() with handler Pair {
                def first(x, _y, k) = k(x)
            }
    }

    @Test
    def testPair(): Unit \ Assert + Pair[Int32, String] =
        Assert.assertEq(expected = 42, Pair.first(42, "hello"))

    mod Api {
        @Export
        pub def echoInt32(x: Int32): Int32 \ Test.DefaultHandler.Poly.Echo[Int32] =
            Test.DefaultHandler.Poly.Echo.echo(x)
    }

}
