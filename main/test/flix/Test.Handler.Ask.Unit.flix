mod Test.Handler.Ask.Unit {

    use Assert.assertEq;

    eff Ask {
        def ask(x: Int32): Unit
    }

    def generator(x: Int32): Unit \ Ask =
        Ask.ask(x); generator(x + 1)

    def sample(limit: Int32): List[Unit] =
        run {
            generator(0); Nil
        } with handler Ask {
            def ask(x, k) = if (x == limit) Nil else () :: k()
        }

    def range(a: Int32, b: Int32): List[Unit] =
        List.unfold(i -> if (i >= b) None else Some(((), i + 1)), a)

    @Test
    def testSample01(): Unit \ Assert =
        assertEq(expected = Nil, sample(0))

    @Test
    def testSample02(): Unit \ Assert =
        assertEq(expected = () :: Nil, sample(1))

    @Test
    def testSample03(): Unit \ Assert =
        assertEq(expected = () :: () :: Nil, sample(2))

    @Test
    def testSample04(): Unit \ Assert =
        assertEq(expected = () :: () :: () :: Nil, sample(3))

    @Test
    def testSample05(): Unit \ Assert =
        assertEq(expected = () :: () :: () :: () :: Nil, sample(4))

    @Test
    def testSample06(): Unit \ Assert =
        assertEq(expected = () :: () :: () :: () :: () :: Nil, sample(5))

    @Test
    def testSample07(): Unit \ Assert =
        assertEq(expected = () :: () :: () :: () :: () :: () :: Nil, sample(6))

    @Test
    def testSample08(): Unit \ Assert =
        assertEq(expected = range(0, 10), sample(10))

    @Test
    def testSample09(): Unit \ Assert =
        assertEq(expected = range(0, 100), sample(100))

}
