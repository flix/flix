mod Test.Handler.Ask.Float64 {

    use Assert.assertEq;

    eff Ask {
        def ask(x: Float64): Unit
    }

    def generator(x: Float64): Unit \ Ask =
        Ask.ask(x); generator(x + 1.0f64)

    def sample(limit: Float64): List[Float64] =
        run {
            generator(0.0f64); Nil
        } with handler Ask {
            def ask(x, k) = if (x == limit) Nil else x :: k()
        }

    def range(a: Float64, b: Float64): List[Float64] =
        List.unfold(i -> if (i >= b) None else Some((i, i + 1.0f64)), a)

    @Test
    def testSample01(): Unit \ Assert =
        assertEq(expected = Nil, sample(0.0f64))

    @Test
    def testSample02(): Unit \ Assert =
        assertEq(expected = 0.0f64 :: Nil, sample(1.0f64))

    @Test
    def testSample03(): Unit \ Assert =
        assertEq(expected = 0.0f64 :: 1.0f64 :: Nil, sample(2.0f64))

    @Test
    def testSample04(): Unit \ Assert =
        assertEq(expected = 0.0f64 :: 1.0f64 :: 2.0f64 :: Nil, sample(3.0f64))

    @Test
    def testSample05(): Unit \ Assert =
        assertEq(expected = 0.0f64 :: 1.0f64 :: 2.0f64 :: 3.0f64 :: Nil, sample(4.0f64))

    @Test
    def testSample06(): Unit \ Assert =
        assertEq(expected = 0.0f64 :: 1.0f64 :: 2.0f64 :: 3.0f64 :: 4.0f64 :: Nil, sample(5.0f64))

    @Test
    def testSample07(): Unit \ Assert =
        assertEq(expected = 0.0f64 :: 1.0f64 :: 2.0f64 :: 3.0f64 :: 4.0f64 :: 5.0f64 :: Nil, sample(6.0f64))

    @Test
    def testSample08(): Unit \ Assert =
        assertEq(expected = range(0.0f64, 10.0f64), sample(10.0f64))

    @Test
    def testSample09(): Unit \ Assert =
        assertEq(expected = range(0.0f64, 100.0f64), sample(100.0f64))

}
