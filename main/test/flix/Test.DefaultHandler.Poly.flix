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
    def testEmitUnit(): Unit \ Emit[Unit] =
        Emit.emit(())

    @Test
    def testEmitBool(): Unit \ Emit[Bool] =
        Emit.emit(true)

    @Test
    def testEmitChar(): Unit \ Emit[Char] =
        Emit.emit('x')

    @Test
    def testEmitFloat32(): Unit \ Emit[Float32] =
        Emit.emit(1.5f32)

    @Test
    def testEmitFloat64(): Unit \ Emit[Float64] =
        Emit.emit(1.5)

    @Test
    def testEmitInt8(): Unit \ Emit[Int8] =
        Emit.emit(42i8)

    @Test
    def testEmitInt16(): Unit \ Emit[Int16] =
        Emit.emit(42i16)

    @Test
    def testEmitInt32(): Unit \ Emit[Int32] =
        Emit.emit(42)

    @Test
    def testEmitInt64(): Unit \ Emit[Int64] =
        Emit.emit(42i64)

    @Test
    def testEmitBigInt(): Unit \ Emit[BigInt] =
        Emit.emit(42ii)

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
    def testEchoUnit(): Unit \ Assert + Echo[Unit] =
        Assert.assertEq(expected = (), Echo.echo(()))

    @Test
    def testEchoBool(): Unit \ Assert + Echo[Bool] =
        Assert.assertEq(expected = true, Echo.echo(true))

    @Test
    def testEchoChar(): Unit \ Assert + Echo[Char] =
        Assert.assertEq(expected = 'x', Echo.echo('x'))

    @Test
    def testEchoFloat32(): Unit \ Assert + Echo[Float32] =
        Assert.assertEq(expected = 1.5f32, Echo.echo(1.5f32))

    @Test
    def testEchoFloat64(): Unit \ Assert + Echo[Float64] =
        Assert.assertEq(expected = 1.5, Echo.echo(1.5))

    @Test
    def testEchoInt8(): Unit \ Assert + Echo[Int8] =
        Assert.assertEq(expected = 42i8, Echo.echo(42i8))

    @Test
    def testEchoInt16(): Unit \ Assert + Echo[Int16] =
        Assert.assertEq(expected = 42i16, Echo.echo(42i16))

    @Test
    def testEchoInt32(): Unit \ Assert + Echo[Int32] =
        Assert.assertEq(expected = 42, Echo.echo(42))

    @Test
    def testEchoInt64(): Unit \ Assert + Echo[Int64] =
        Assert.assertEq(expected = 42i64, Echo.echo(42i64))

    @Test
    def testEchoBigInt(): Unit \ Assert + Echo[BigInt] =
        Assert.assertEq(expected = 42ii, Echo.echo(42ii))

    @Test
    def testEchoString(): Unit \ Assert + Echo[String] =
        Assert.assertEq(expected = "hello", Echo.echo("hello"))

    @Test
    def testEchoTuple(): Unit \ Assert + Echo[(String, Int32)] =
        Assert.assertEq(expected = ("hello", 42), Echo.echo(("hello", 42)))

    @Test
    def testMultiplePolymorphicEffects(): Unit \ Assert + Emit[Int32] + Echo[String] = {
        Emit.emit(42);
        Assert.assertEq(expected = "hello", Echo.echo("hello"))
    }

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
    def testPairUnit(): Unit \ Assert + Pair[Unit, Unit] =
        Assert.assertEq(expected = (), Pair.first((), ()))

    @Test
    def testPairBool(): Unit \ Assert + Pair[Bool, Bool] =
        Assert.assertEq(expected = true, Pair.first(true, false))

    @Test
    def testPairChar(): Unit \ Assert + Pair[Char, Char] =
        Assert.assertEq(expected = 'x', Pair.first('x', 'y'))

    @Test
    def testPairFloat32(): Unit \ Assert + Pair[Float32, Float32] =
        Assert.assertEq(expected = 1.5f32, Pair.first(1.5f32, 2.5f32))

    @Test
    def testPairFloat64(): Unit \ Assert + Pair[Float64, Float64] =
        Assert.assertEq(expected = 1.5, Pair.first(1.5, 2.5))

    @Test
    def testPairInt8(): Unit \ Assert + Pair[Int8, Int8] =
        Assert.assertEq(expected = 42i8, Pair.first(42i8, 21i8))

    @Test
    def testPairInt16(): Unit \ Assert + Pair[Int16, Int16] =
        Assert.assertEq(expected = 42i16, Pair.first(42i16, 21i16))

    @Test
    def testPairInt32(): Unit \ Assert + Pair[Int32, Int32] =
        Assert.assertEq(expected = 42, Pair.first(42, 21))

    @Test
    def testPairInt64(): Unit \ Assert + Pair[Int64, Int64] =
        Assert.assertEq(expected = 42i64, Pair.first(42i64, 21i64))

    @Test
    def testPairBigInt(): Unit \ Assert + Pair[BigInt, BigInt] =
        Assert.assertEq(expected = 42ii, Pair.first(42ii, 21ii))

    @Test
    def testPairString(): Unit \ Assert + Pair[String, String] =
        Assert.assertEq(expected = "hello", Pair.first("hello", "goodbye"))

    @Test
    def testPairMixedParameters(): Unit \ Assert + Pair[Int32, String] =
        Assert.assertEq(expected = 42, Pair.first(42, "hello"))

    mod Api {
        @Export
        pub def echoInt32(x: Int32): Int32 \ Test.DefaultHandler.Poly.Echo[Int32] =
            Test.DefaultHandler.Poly.Echo.echo(x)
    }

}
