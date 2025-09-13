mod Test.Handler.Ask.Enum {

    eff Ask {
        def ask(x: Int32): Unit
    }

    def generator(x: Int32): Unit \ Ask =
        Ask.ask(x); generator(x + 1)

    def sample(limit: Int32): List[Option[Int32]] =
        run {
            generator(0); Nil
        } with handler Ask {
            def ask(x, k) = if (x == limit) Nil else Some(x) :: k()
        }

    def range(a: Int32, b: Int32): List[Option[Int32]] =
        List.unfold(i -> if (i >= b) None else Some((Some(i), i + 1)), a)

    @Test
    def testSample01(): Bool =
        Nil == sample(0)

    @Test
    def testSample02(): Bool =
        Some(0) :: Nil == sample(1)

    @Test
    def testSample03(): Bool =
        Some(0) :: Some(1) :: Nil == sample(2)

    @Test
    def testSample04(): Bool =
        Some(0) :: Some(1) :: Some(2) :: Nil == sample(3)

    @Test
    def testSample05(): Bool =
        Some(0) :: Some(1) :: Some(2) :: Some(3) :: Nil == sample(4)

    @Test
    def testSample06(): Bool =
        Some(0) :: Some(1) :: Some(2) :: Some(3) :: Some(4) :: Nil == sample(5)

    @Test
    def testSample07(): Bool =
        Some(0) :: Some(1) :: Some(2) :: Some(3) :: Some(4) :: Some(5) :: Nil == sample(6)

    @Test
    def testSample08(): Bool =
        range(0, 10) == sample(10)

    @Test
    def testSample09(): Bool =
        range(0, 100) == sample(100)

}
