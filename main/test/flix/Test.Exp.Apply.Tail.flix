mod Test.Exp.Apply.Tail {

    use Assert.assertEq

    @Test
    def testApplyTail01(): Unit \ Assert =
        assertEq(expected = true, odd(1))

    @Test
    def testApplyTail02(): Unit \ Assert =
        assertEq(expected = true, even(12))

    @Test
    def testApplyTail03(): Unit \ Assert =
        assertEq(expected = true, odd(123))

    @Test
    def testApplyTail04(): Unit \ Assert =
        assertEq(expected = true, even(1234))

    @Test
    def testApplyTail05(): Unit \ Assert =
        assertEq(expected = true, odd(12345))

    @Test
    def testApplyTail06(): Unit \ Assert =
        assertEq(expected = true, even(123456))

    @Test
    def testApplyTail07(): Unit \ Assert =
        assertEq(expected = true, odd(1234567))

    @Test
    def testApplyTail08(): Unit \ Assert =
        assertEq(expected = true, a(42))

    @Test
    def testApplyTail09(): Unit \ Assert =
        assertEq(expected = true, a1(42, z -> z + 1))

    @Test
    def testApplyTail10(): Unit \ Assert =
        assertEq(expected = 123, ping(123, x -> x))

    @Test
    def testApplyTail11(): Unit \ Assert =
        assertEq(expected = 12345, ping(12345, x -> x))

    @Test
    def testApplyTail12(): Unit \ Assert =
        assertEq(expected = 1234567, ping(1234567, x -> x))

    @Test
    def testApplyTail13(): Unit \ Assert = region rc {
        let n = Ref.fresh(rc, 1);
        assertEq(expected = true, stmtRecursion(n))
    }

    @Test
    def testApplyTail14(): Unit \ Assert = region rc {
        let n = Ref.fresh(rc, 12);
        assertEq(expected = true, stmtRecursion(n))
    }

    @Test
    def testApplyTail15(): Unit \ Assert = region rc {
        let n = Ref.fresh(rc, 1234);
        assertEq(expected = true, stmtRecursion(n))
    }

    @Test
    def testApplyTail16(): Unit \ Assert = region rc {
        let n = Ref.fresh(rc, 1234567);
        assertEq(expected = true, stmtRecursion(n))
    }

    def odd(n: Int32): Bool = match n {
        case 0 => false
        case 1 => true
        case x => even(x - 1)
    }

    def even(n: Int32): Bool = match n {
        case 0 => true
        case 1 => false
        case x => odd(x - 1)
    }

    def a(x: Int32): Bool = b(x + 1)

    def b(x: Int32): Bool = c(x - 1)

    def c(x: Int32): Bool = x == 42

    def a1(x: Int32, f: Int32 -> Int32): Bool = b1(f(x), y -> y - 1)

    def b1(x: Int32, f: Int32 -> Int32): Bool = c1(f(x), y -> y == 42)

    def c1(x: Int32, f: Int32 -> Bool): Bool = f(x)

    def ping(n: Int32, c: Int32 -> Int32): Int32 = match n {
        case 0 => c(0)
        case 1 => c(1)
        case x => pong(x - 1, z -> c(z + 1))
    }

    def pong(n: Int32, c: Int32 -> Int32): Int32 = match n {
        case 0 => c(0)
        case 1 => c(1)
        case x => ping(x - 1, z -> c(z + 1))
    }

    def stmtRecursion(n: Ref[Int32, r]): Bool \ r = {
        Ref.put(Ref.get(n) - 1, n);
        if (Ref.get(n) == 0) true else stmtRecursion(n)
    }

}
