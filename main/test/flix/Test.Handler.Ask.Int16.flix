mod Test.Handler.Ask.Int16 {

    use Assert.assertEq;

    eff Ask {
        def ask(x: Int16): Unit
    }

    def generator(x: Int16): Unit \ Ask =
        Ask.ask(x); generator(x + 1i16)

    def sample(limit: Int16): List[Int16] =
        run {
            generator(0i16); Nil
        } with handler Ask {
            def ask(x, k) = if (x == limit) Nil else x :: k()
        }

    def range(a: Int16, b: Int16): List[Int16] =
        List.unfold(i -> if (i >= b) None else Some((i, i + 1i16)), a)

    @Test
    def testSample01(): Unit \ Assert =
        assertEq(expected = Nil, sample(0i16))

    @Test
    def testSample02(): Unit \ Assert =
        assertEq(expected = 0i16 :: Nil, sample(1i16))

    @Test
    def testSample03(): Unit \ Assert =
        assertEq(expected = 0i16 :: 1i16 :: Nil, sample(2i16))

    @Test
    def testSample04(): Unit \ Assert =
        assertEq(expected = 0i16 :: 1i16 :: 2i16 :: Nil, sample(3i16))

    @Test
    def testSample05(): Unit \ Assert =
        assertEq(expected = 0i16 :: 1i16 :: 2i16 :: 3i16 :: Nil, sample(4i16))

    @Test
    def testSample06(): Unit \ Assert =
        assertEq(expected = 0i16 :: 1i16 :: 2i16 :: 3i16 :: 4i16 :: Nil, sample(5i16))

    @Test
    def testSample07(): Unit \ Assert =
        assertEq(expected = 0i16 :: 1i16 :: 2i16 :: 3i16 :: 4i16 :: 5i16 :: Nil, sample(6i16))

    @Test
    def testSample08(): Unit \ Assert =
        assertEq(expected = range(0i16, 10i16), sample(10i16))

    @Test
    def testSample09(): Unit \ Assert =
        assertEq(expected = range(0i16, 100i16), sample(100i16))

}
