mod Test.Handler.Ask.Float32 {

    use Assert.assertEq;

    eff Ask {
        def ask(x: Float32): Unit
    }

    def generator(x: Float32): Unit \ Ask =
        Ask.ask(x); generator(x + 1.0f32)

    def sample(limit: Float32): List[Float32] =
        run {
            generator(0.0f32); Nil
        } with handler Ask {
            def ask(x, k) = if (x == limit) Nil else x :: k()
        }

    def range(a: Float32, b: Float32): List[Float32] =
        List.unfold(i -> if (i >= b) None else Some((i, i + 1.0f32)), a)

    @Test
    def testSample01(): Unit \ Assert =
        assertEq(expected = Nil, sample(0.0f32))

    @Test
    def testSample02(): Unit \ Assert =
        assertEq(expected = 0.0f32 :: Nil, sample(1.0f32))

    @Test
    def testSample03(): Unit \ Assert =
        assertEq(expected = 0.0f32 :: 1.0f32 :: Nil, sample(2.0f32))

    @Test
    def testSample04(): Unit \ Assert =
        assertEq(expected = 0.0f32 :: 1.0f32 :: 2.0f32 :: Nil, sample(3.0f32))

    @Test
    def testSample05(): Unit \ Assert =
        assertEq(expected = 0.0f32 :: 1.0f32 :: 2.0f32 :: 3.0f32 :: Nil, sample(4.0f32))

    @Test
    def testSample06(): Unit \ Assert =
        assertEq(expected = 0.0f32 :: 1.0f32 :: 2.0f32 :: 3.0f32 :: 4.0f32 :: Nil, sample(5.0f32))

    @Test
    def testSample07(): Unit \ Assert =
        assertEq(expected = 0.0f32 :: 1.0f32 :: 2.0f32 :: 3.0f32 :: 4.0f32 :: 5.0f32 :: Nil, sample(6.0f32))

    @Test
    def testSample08(): Unit \ Assert =
        assertEq(expected = range(0.0f32, 10.0f32), sample(10.0f32))

    @Test
    def testSample09(): Unit \ Assert =
        assertEq(expected = range(0.0f32, 100.0f32), sample(100.0f32))

}
