///
/// Tests calls to trait signatures that resolve to an instance def whose body is a primitive
/// operation. The compiler applies the operation directly when the def merely wraps it.
///
mod Test.Exp.Apply.Primitive {

    use Assert.assertEq

    ///
    /// Returns the current value of the counter `c` and increments it.
    ///
    /// The value returned by the n-th call is `n - 1`, hence the values
    /// returned by `next` reveal the order in which the calls were evaluated.
    ///
    def next(c: Ref[Int32, r]): Int32 \ r =
        let n = Ref.get(c);
        Ref.put(n + 1, c);
        n

    ///
    /// Wrappers of a unary operation.
    ///

    @Test
    def testUnary01(): Unit \ Assert =
        assertEq(expected = -42, Wrap.neg(42))

    @Test
    def testUnary02(): Unit \ Assert =
        assertEq(expected = -1.5, Wrap.neg(1.5))

    @Test
    def testUnary03(): Unit \ Assert =
        assertEq(expected = "-abc", Wrap.neg("abc"))

    @Test
    def testUnary04(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 5);
        assertEq(expected = -5, Wrap.neg(next(c)));
        assertEq(expected = 6, Ref.get(c))
    }

    @Test
    def testUnary05(): Unit \ Assert =
        assertEq(expected = 42, Wrap.neg(Wrap.neg(42)))

    @Test
    def testUnary06(): Unit \ Assert =
        assertEq(expected = false, Logic.neg(true))

    ///
    /// Wrappers of a binary operation.
    ///

    @Test
    def testBinary01(): Unit \ Assert =
        assertEq(expected = 7, Wrap.minus(10, 3))

    @Test
    def testBinary02(): Unit \ Assert =
        assertEq(expected = 7.5, Wrap.minus(10.0, 2.5))

    @Test
    def testBinary03(): Unit \ Assert =
        assertEq(expected = "abc-def", Wrap.minus("abc", "def"))

    @Test
    def testBinary04(): Unit \ Assert =
        assertEq(expected = 5, Wrap.minus(Wrap.minus(10, 3), Wrap.minus(4, 2)))

    @Test
    def testBinary05(): Unit \ Assert =
        assertEq(expected = 13, Wrap.minus(10, Wrap.neg(3)))

    ///
    /// The arguments are evaluated once and from left to right.
    ///

    @Test
    def testEvalOrder01(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        assertEq(expected = -1, Wrap.minus(next(c), next(c)));
        assertEq(expected = 2, Ref.get(c))
    }

    @Test
    def testEvalOrder02(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        assertEq(expected = -15, Wrap.minus(Wrap.minus(next(c), next(c)), Wrap.minus(next(c) * 10, next(c) * 2)));
        assertEq(expected = 4, Ref.get(c))
    }

    @Test
    def testEvalOrder03(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        assertEq(expected = "0-1", Wrap.minus("${next(c)}", "${next(c)}"))
    }

    ///
    /// Defs that are not wrappers: the operands are not the parameters in declaration order.
    ///

    @Test
    def testFlipped01(): Unit \ Assert =
        assertEq(expected = -7, Almost.flipped(10, 3))

    @Test
    def testFlipped02(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        assertEq(expected = 1, Almost.flipped(next(c), next(c)));
        assertEq(expected = 2, Ref.get(c))
    }

    @Test
    def testTwice01(): Unit \ Assert =
        assertEq(expected = 20, Almost.twice(10, 3))

    @Test
    def testTwice02(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 1);
        assertEq(expected = 2, Almost.twice(next(c), next(c)));
        assertEq(expected = 3, Ref.get(c))
    }

    @Test
    def testNegFirst01(): Unit \ Assert =
        assertEq(expected = -10, Almost.negFirst(10, 3))

    @Test
    def testNegFirst02(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 1);
        assertEq(expected = -1, Almost.negFirst(next(c), next(c)));
        assertEq(expected = 3, Ref.get(c))
    }

    @Test
    def testInc01(): Unit \ Assert =
        assertEq(expected = 43, Almost.inc(42))

    ///
    /// A wrapper that is marked `@DontInline`.
    ///

    @Test
    def testDontInline01(): Unit \ Assert =
        assertEq(expected = 7, Almost.kept(10, 3))

    @Test
    def testDontInline02(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        assertEq(expected = -1, Almost.kept(next(c), next(c)))
    }

    ///
    /// Wrappers of `and` and `or`: a call evaluates both arguments.
    ///

    @Test
    def testAnd01(): Unit \ Assert =
        assertEq(expected = (false, false, false, true), (Logic.conj(false, false), Logic.conj(false, true), Logic.conj(true, false), Logic.conj(true, true)))

    @Test
    def testAnd02(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        assertEq(expected = false, Logic.conj(false, next(c) == 0));
        assertEq(expected = 1, Ref.get(c))
    }

    @Test
    def testOr01(): Unit \ Assert =
        assertEq(expected = (false, true, true, true), (Logic.disj(false, false), Logic.disj(false, true), Logic.disj(true, false), Logic.disj(true, true)))

    @Test
    def testOr02(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        assertEq(expected = true, Logic.disj(true, next(c) == 0));
        assertEq(expected = 1, Ref.get(c))
    }

    ///
    /// Signatures used as functions.
    ///

    @Test
    def testClosure01(): Unit \ Assert =
        assertEq(expected = 4, List.foldLeft(Wrap.minus, 10, 1 :: 2 :: 3 :: Nil))

    @Test
    def testClosure02(): Unit \ Assert =
        assertEq(expected = -1 :: -2 :: -3 :: Nil, List.map(Wrap.neg, 1 :: 2 :: 3 :: Nil))

    @Test
    def testClosure03(): Unit \ Assert =
        let f = Wrap.minus(10);
        assertEq(expected = (7, 6), (f(3), f(4)))

    @Test
    def testClosure04(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        let f = Wrap.minus(next(c));
        assertEq(expected = (-1, -2), (f(next(c)), f(next(c))))
    }

    @Test
    def testClosure05(): Unit \ Assert =
        assertEq(expected = 9 :: 8 :: Nil, List.map(Wrap.minus(10), 1 :: 2 :: Nil))

    ///
    /// Signatures used through a trait constraint.
    ///

    @Test
    def testConstraint01(): Unit \ Assert =
        assertEq(expected = 7, minus(10, 3))

    @Test
    def testConstraint02(): Unit \ Assert =
        assertEq(expected = 7.5, minus(10.0, 2.5))

    @Test
    def testConstraint03(): Unit \ Assert =
        assertEq(expected = "abc-def", minus("abc", "def"))

    @Test
    def testConstraint04(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        assertEq(expected = -1, minus(next(c), next(c)))
    }

    @Test
    def testConstraint05(): Unit \ Assert =
        assertEq(expected = (-4, -4.0, "-abc-abc"), (negTwice(2), negTwice(2.0), negTwice("abc")))

    ///
    /// Arguments that perform an effect operation.
    ///

    @Test
    def testHandler01(): Unit \ Assert =
        let r = run {
            Wrap.minus(Ask.ask(), Wrap.neg(Ask.ask()))
        } with handler Ask {
            def ask(_, k) = k(21)
        };
        assertEq(expected = 42, r)

    @Test
    def testHandler02(): Unit \ Assert =
        let r = run {
            Wrap.minus(Ask.ask(), Ask.ask()) :: Nil
        } with handler Ask {
            def ask(_, k) = k(1) ::: k(10)
        };
        assertEq(expected = 0 :: -9 :: 9 :: 0 :: Nil, r)

    @Test
    def testHandler03(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        let r = run {
            Wrap.minus(Ask.ask(), next(c)) + Wrap.minus(next(c), Ask.ask())
        } with handler Ask {
            def ask(_, k) = k(100)
        };
        assertEq(expected = 1, r)
    }

    ///
    /// Tail position.
    ///

    @Test
    def testTail01(): Unit \ Assert =
        assertEq(expected = -100000, countDown(100000, 0))

    ///
    /// Wrappers from the standard library.
    ///

    @Test
    def testLibrary01(): Unit \ Assert =
        assertEq(expected = 6, List.foldLeft(Add.add, 0, 1 :: 2 :: 3 :: Nil))

    @Test
    def testLibrary02(): Unit \ Assert =
        assertEq(expected = -1 :: -2 :: Nil, List.map(Neg.neg, 1 :: 2 :: Nil))

    @Test
    def testLibrary03(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        assertEq(expected = (true, false, true, false), (next(c) < next(c), next(c) > next(c), next(c) <= next(c), next(c) >= next(c)))
    }

    @Test
    def testLibrary04(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        assertEq(expected = (false, true), (next(c) == next(c), next(c) != next(c)))
    }

    @Test
    def testLibrary05(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 1);
        assertEq(expected = (3, 12, -6), (next(c) + next(c), next(c) * next(c), next(c) - next(c) * next(c) + 31))
    }

    @Test
    def testLibrary06(): Unit \ Assert =
        assertEq(expected = (3i8, 3i16, 3i64, 3.0f32, 3.0, 3ii, "12"), (1i8 + 2i8, 1i16 + 2i16, 1i64 + 2i64, 1.0f32 + 2.0f32, 1.0 + 2.0, 1ii + 2ii, "1" + "2"))

    ///
    /// A trait with instances that wrap a primitive operation, and one instance that does not.
    ///
    trait Wrap[a] {
        pub def neg(x: a): a

        pub def minus(x: a, y: a): a
    }

    instance Wrap[Int32] {
        pub def neg(x: Int32): Int32 = %%INT32_NEG%%(x)

        pub def minus(x: Int32, y: Int32): Int32 = %%INT32_SUB%%(x, y)
    }

    instance Wrap[Float64] {
        pub def neg(x: Float64): Float64 = %%FLOAT64_NEG%%(x)

        pub def minus(x: Float64, y: Float64): Float64 = %%FLOAT64_SUB%%(x, y)
    }

    instance Wrap[String] {
        pub def neg(x: String): String = "-${x}"

        pub def minus(x: String, y: String): String = "${x}-${y}"
    }

    ///
    /// A trait with an instance where each def applies a primitive operation, but is not a wrapper
    /// that the compiler may replace by the operation.
    ///
    trait Almost[a] {
        pub def flipped(x: a, y: a): a

        pub def twice(x: a, y: a): a

        pub def negFirst(x: a, y: a): a

        pub def inc(x: a): a

        pub def kept(x: a, y: a): a
    }

    instance Almost[Int32] {
        pub def flipped(x: Int32, y: Int32): Int32 = %%INT32_SUB%%(y, x)

        pub def twice(x: Int32, _y: Int32): Int32 = %%INT32_ADD%%(x, x)

        pub def negFirst(x: Int32, _y: Int32): Int32 = %%INT32_NEG%%(x)

        pub def inc(x: Int32): Int32 = %%INT32_ADD%%(x, 1)

        @DontInline
        pub def kept(x: Int32, y: Int32): Int32 = %%INT32_SUB%%(x, y)
    }

    ///
    /// A trait with an instance that wraps the Boolean operations.
    ///
    trait Logic[a] {
        pub def neg(x: a): a

        pub def conj(x: a, y: a): a

        pub def disj(x: a, y: a): a
    }

    instance Logic[Bool] {
        pub def neg(x: Bool): Bool = not x

        pub def conj(x: Bool, y: Bool): Bool = x and y

        pub def disj(x: Bool, y: Bool): Bool = x or y
    }

    eff Ask {
        def ask(): Int32
    }

    def minus(x: a, y: a): a with Wrap[a] = Wrap.minus(x, y)

    def negTwice(x: a): a with Wrap[a] = Wrap.minus(Wrap.neg(x), x)

    def countDown(i: Int32, acc: Int32): Int32 =
        if (i == 0) acc else countDown(Wrap.minus(i, 1), Wrap.minus(acc, 1))

}
