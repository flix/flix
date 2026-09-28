mod Test.EvalOrder {

    use Assert.assertEq

    import java.lang.{Math => JMath}
    import java.math.BigDecimal
    import java.math.BigInteger

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
    /// Defs.
    ///

    @Test
    def testDef01(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        assertEq(expected = (0, 1, 2), d3(next(c), next(c), next(c)))
    }

    @Test
    def testDef02(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        assertEq(expected = ((0, 1), (2, 3)), pair(d2(next(c), next(c)), d2(next(c), next(c))))
    }

    @Test
    def testDef03(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        assertEq(expected = (0, 1), named(x = next(c), y = next(c)))
    }

    @Test
    def testDef04(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        assertEq(expected = (0, 1, 2), effectful(c, next(c), next(c)))
    }

    ///
    /// Parameter Usage.
    ///

    @Test
    def testParam01(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        assertEq(expected = (1, 0), flip(next(c), next(c)))
    }

    @Test
    def testParam02(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        let r = snd(next(c), next(c));
        assertEq(expected = 1, r);
        assertEq(expected = 2, Ref.get(c))
    }

    @Test
    def testParam03(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        let r = twice(next(c), next(c));
        assertEq(expected = (1, 0, 1, 0), r);
        assertEq(expected = 2, Ref.get(c))
    }

    ///
    /// Lambdas.
    ///

    @Test
    def testLambda01(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        let f = (x, y) -> (x, y);
        assertEq(expected = (0, 1), f(next(c), next(c)))
    }

    @Test
    def testLambda02(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        assertEq(expected = (0, 1), ((x, y) -> (x, y))(next(c), next(c)))
    }

    ///
    /// Closures.
    ///

    @Test
    def testClosure01(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        let k = 10;
        let f = (x, y) -> (x + k, y + k);
        assertEq(expected = (10, 11), f(next(c), next(c)))
    }

    @Test
    def testClosure02(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        assertEq(expected = (0, 1), mk(c)(next(c)))
    }

    @Test
    def testClosure03(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        assertEq(expected = (0, 1), ho(d2, c))
    }

    ///
    /// Local Defs.
    ///

    @Test
    def testLocalDef01(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        def loc(x, y) = (x, y);
        assertEq(expected = (0, 1), loc(next(c), next(c)))
    }

    ///
    /// Currying.
    ///

    @Test
    def testCurried01(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        assertEq(expected = (0, 1, 2), d3(next(c))(next(c))(next(c)))
    }

    @Test
    def testCurried02(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        assertEq(expected = (0, 1, 2), d3(next(c), next(c))(next(c)))
    }

    @Test
    def testCurried03(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        assertEq(expected = (0, 1, 2), mk2(c)(next(c))(next(c)))
    }

    ///
    /// Trait Signatures.
    ///

    @Test
    def testSig01(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        assertEq(expected = (0, 1), Pair.pair(next(c), next(c)))
    }

    @Test
    def testSig02(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        assertEq(expected = (0, 1), Pair.pairDefault(next(c), next(c)))
    }

    @Test
    def testSig03(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        assertEq(expected = (0, 1), constrained(() -> next(c)))
    }

    @Test
    def testSig04(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        assertEq(expected = -1, next(c) - next(c))
    }

    ///
    /// Effect Operations.
    ///

    @Test
    def testOp01(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        let r = run {
            Op.op(next(c), next(c))
        } with handler Op {
            def op(x, y, k) = k((x, y))
        };
        assertEq(expected = (0, 1), r)
    }

    @Test
    def testOp02(): Unit \ Assert = region rc {
        let log = Ref.fresh(rc, Nil);
        run {
            discard d3(Emit.emit(1), Emit.emit(2), Emit.emit(3))
        } with handler Emit {
            def emit(x, k) = {
                Ref.put(x :: Ref.get(log), log);
                k(x)
            }
        };
        assertEq(expected = 1 :: 2 :: 3 :: Nil, List.reverse(Ref.get(log)))
    }

    @Test
    def testOp03(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        let r = run {
            d2(Emit.emit(10), Emit.emit(20))
        } with handler Emit {
            def emit(_, k) = k(next(c))
        };
        assertEq(expected = (0, 1), r)
    }

    ///
    /// Tail Calls.
    ///

    @Test
    def testTailCall01(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        assertEq(expected = (4, 5), selfTail(c, 3, -1, -1))
    }

    @Test
    def testTailCall02(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        assertEq(expected = (0, 1, 2), otherTail(c))
    }

    ///
    /// Java Interop.
    ///

    @Test
    def testJava01(): Unit \ {Assert, IO} = region rc {
        let c = Ref.fresh(rc, 0);
        assertEq(expected = -1, JMath.subtractExact(next(c), next(c)))
    }

    @Test
    def testJava02(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        assertEq(expected = "abcd", "abcdef".substring(next(c), 3 + next(c)))
    }

    @Test
    def testJava03(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        assertEq(expected = "bcdef0", recv(c).substring(next(c)))
    }

    @Test
    def testJava04(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        let d = new BigDecimal(new BigInteger("${10 + next(c)}"), next(c));
        assertEq(expected = "1.0", d.toString())
    }

    ///
    /// Tags.
    ///

    @Test
    def testTag01(): Unit \ Assert = region rc {
        let c = Ref.fresh(rc, 0);
        assertEq(expected = E.Tag(0, 1), E.Tag(next(c), next(c)))
    }

    ///
    /// New Structs.
    ///

    mod S {

        use Assert.assertEq
        use Test.EvalOrder.next

        @Test
        def testStructNew01(): Unit \ Assert = region rc {
            let c = Ref.fresh(rc, 0);
            let s = new S @ rc { x = next(c), y = next(c), z = next(c) };
            assertEq(expected = (0, 1, 2), (s->x, s->y, s->z))
        }

        @Test
        def testStructNew02(): Unit \ Assert = region rc {
            let c = Ref.fresh(rc, 0);
            let s = new S @ rc { z = next(c), y = next(c), x = next(c) };
            assertEq(expected = (2, 1, 0), (s->x, s->y, s->z))
        }

        @Test
        def testStructNew03(): Unit \ Assert = region rc {
            let c = Ref.fresh(rc, 0);
            let s = new S @ rc { y = next(c), z = next(c), x = next(c) };
            assertEq(expected = (2, 0, 1), (s->x, s->y, s->z))
        }

    }

    trait Pair[a] {
        pub def pair(x: a, y: a): (a, a)

        pub def pairDefault(x: a, y: a): (a, a) = (x, y)
    }

    instance Pair[Int32] {
        pub def pair(x: Int32, y: Int32): (Int32, Int32) = (x, y)
    }

    eff Op {
        def op(x: Int32, y: Int32): (Int32, Int32)
    }

    eff Emit {
        def emit(x: Int32): Int32
    }

    enum E with Eq, ToString {
        case Tag(Int32, Int32)
    }

    struct S[r] {
        x: Int32,
        y: Int32,
        z: Int32
    }

    def d2(x: Int32, y: Int32): (Int32, Int32) = (x, y)

    def d3(x: Int32, y: Int32, z: Int32): (Int32, Int32, Int32) = (x, y, z)

    def pair(x: (Int32, Int32), y: (Int32, Int32)): ((Int32, Int32), (Int32, Int32)) = (x, y)

    def named(x: {x = Int32}, y: {y = Int32}): (Int32, Int32) = (x#x, y#y)

    def effectful(c: Ref[Int32, r], x: Int32, y: Int32): (Int32, Int32, Int32) \ r = (x, y, next(c))

    def flip(x: Int32, y: Int32): (Int32, Int32) = (y, x)

    def snd(_x: Int32, y: Int32): Int32 = y

    def twice(x: Int32, y: Int32): (Int32, Int32, Int32, Int32) = (y, x, y, x)

    def mk(c: Ref[Int32, r]): (Int32 -> (Int32, Int32)) \ r =
        let n = next(c);
        x -> (n, x)

    def mk2(c: Ref[Int32, r]): (Int32 -> (Int32 -> (Int32, Int32, Int32))) \ r =
        let n = next(c);
        x -> y -> (n, x, y)

    def ho(f: (Int32, Int32) -> (Int32, Int32), c: Ref[Int32, r]): (Int32, Int32) \ r =
        f(next(c), next(c))

    def constrained(f: Unit -> a \ ef): (a, a) \ ef with Pair[a] =
        Pair.pair(f(), f())

    def selfTail(c: Ref[Int32, r], i: Int32, x: Int32, y: Int32): (Int32, Int32) \ r =
        if (i == 0) (x, y) else selfTail(c, i - 1, next(c), next(c))

    def otherTail(c: Ref[Int32, r]): (Int32, Int32, Int32) \ r =
        d3(next(c), next(c), next(c))

    def recv(c: Ref[Int32, r]): String \ r =
        let n = next(c);
        "abcdef${n}"

}
