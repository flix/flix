mod Test.Handler.Ask.AtomicInteger {

    import java.util.concurrent.atomic.AtomicInteger

    use Assert.assertEq;

    def newAtomicInteger(n: Int32): AtomicInteger =
        unsafe IO { new AtomicInteger(n) }

    def toInt(n: AtomicInteger): Int32 =
        unsafe IO { n.intValue() }

    eff Ask {
        def ask(x: AtomicInteger): Unit
    }

    def generator(x: AtomicInteger): Unit \ Ask =
        Ask.ask(x); generator(newAtomicInteger(toInt(x) + 1))

    def sample(limit: Int32): List[AtomicInteger] =
        run {
            generator(newAtomicInteger(0)); Nil
        } with handler Ask {
            def ask(x, k) = if (toInt(x) == limit) Nil else x :: k()
        }

    def range(a: AtomicInteger, b: AtomicInteger): List[AtomicInteger] =
        List.unfold(i -> if (toInt(i) >= toInt(b)) None else Some((i, newAtomicInteger(toInt(i) + 1))), a)

    @Test
    def testSample01(): Unit \ Assert =
        assertEq(expected = Nil, sample(0) |> List.map(toInt))

    @Test
    def testSample02(): Unit \ Assert =
        assertEq(expected = 0 :: Nil, sample(1) |> List.map(toInt))

    @Test
    def testSample03(): Unit \ Assert =
        assertEq(expected = 0 :: 1 :: Nil, sample(2) |> List.map(toInt))

    @Test
    def testSample04(): Unit \ Assert =
        assertEq(expected = 0 :: 1 :: 2 :: Nil, sample(3) |> List.map(toInt))

    @Test
    def testSample05(): Unit \ Assert =
        assertEq(expected = 0 :: 1 :: 2 :: 3 :: Nil, sample(4) |> List.map(toInt))

    @Test
    def testSample06(): Unit \ Assert =
        assertEq(expected = 0 :: 1 :: 2 :: 3 :: 4 :: Nil, sample(5) |> List.map(toInt))

    @Test
    def testSample07(): Unit \ Assert =
        assertEq(expected = 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil, sample(6) |> List.map(toInt))

    @Test
    def testSample08(): Unit \ Assert =
        assertEq(expected = range(newAtomicInteger(0), newAtomicInteger(10)) |> List.map(toInt), sample(10) |> List.map(toInt))

    @Test
    def testSample09(): Unit \ Assert =
        assertEq(expected = range(newAtomicInteger(0), newAtomicInteger(100)) |> List.map(toInt), sample(100) |> List.map(toInt))

}
