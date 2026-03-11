mod Test.Handler.Ask.Int8 {

    use Assert.assertEq;

    eff Ask {
        def ask(x: Int8): Unit
    }

    def generator(x: Int8): Unit \ Ask =
        Ask.ask(x); generator(x + 1i8)

    def sample(limit: Int8): List[Int8] =
        run {
            generator(0i8); Nil
        } with handler Ask {
            def ask(x, k) = if (x == limit) Nil else x :: k()
        }

    def range(a: Int8, b: Int8): List[Int8] =
        List.unfold(i -> if (i >= b) None else Some((i, i + 1i8)), a)

    @Test
    def testSample01(): Unit \ Assert =
        assertEq(expected = Nil, sample(0i8))

    @Test
    def testSample02(): Unit \ Assert =
        assertEq(expected = 0i8 :: Nil, sample(1i8))

    @Test
    def testSample03(): Unit \ Assert =
        assertEq(expected = 0i8 :: 1i8 :: Nil, sample(2i8))

    @Test
    def testSample04(): Unit \ Assert =
        assertEq(expected = 0i8 :: 1i8 :: 2i8 :: Nil, sample(3i8))

    @Test
    def testSample05(): Unit \ Assert =
        assertEq(expected = 0i8 :: 1i8 :: 2i8 :: 3i8 :: Nil, sample(4i8))

    @Test
    def testSample06(): Unit \ Assert =
        assertEq(expected = 0i8 :: 1i8 :: 2i8 :: 3i8 :: 4i8 :: Nil, sample(5i8))

    @Test
    def testSample07(): Unit \ Assert =
        assertEq(expected = 0i8 :: 1i8 :: 2i8 :: 3i8 :: 4i8 :: 5i8 :: Nil, sample(6i8))

    @Test
    def testSample08(): Unit \ Assert =
        assertEq(expected = range(0i8, 10i8), sample(10i8))

    @Test
    def testSample09(): Unit \ Assert =
        assertEq(expected = range(0i8, 100i8), sample(100i8))

}
