mod Test.Handler.Gen.Enum {

    eff Gen {
        pub def gen(): Option[Int32]
    }

    def generator(): Unit \ Gen =
        Gen.gen(); generator()

    def sample(limit: Int32): List[Option[Int32]] =
        region rc {
            let counter = Ref.fresh(rc, Some(0));
            try {
                generator(); Nil
            } with Gen {
                def gen(k) =
                    let i = getAndInc(counter);
                    if (i == limit) Nil else Some(i) :: k(Some(i))
            }
        }

    def getAndInc(r: Ref[Option[Int32], r]): Int32 \ r =
        match Ref.get(r) {
            case Some(i) =>
                Ref.put(Some(i + 1), r);
                i
            case None => unreachable!()
        }

    def range(a: Int32, b: Int32): List[Option[Int32]] =
        List.unfold(i -> if (i >= b) None else Some((Some(i), i + 1)), a)

    @Test
    def testSample01(): Bool =
        Assert.eq(Nil, sample(0))

    @Test
    def testSample02(): Bool =
        Assert.eq(Some(0) :: Nil, sample(1))

    @Test
    def testSample03(): Bool =
        Assert.eq(Some(0) :: Some(1) :: Nil, sample(2))

    @Test
    def testSample04(): Bool =
        Assert.eq(Some(0) :: Some(1) :: Some(2) :: Nil, sample(3))

    @Test
    def testSample05(): Bool =
        Assert.eq(Some(0) :: Some(1) :: Some(2) :: Some(3) :: Nil, sample(4))

    @Test
    def testSample06(): Bool =
        Assert.eq(Some(0) :: Some(1) :: Some(2) :: Some(3) :: Some(4) :: Nil, sample(5))

    @Test
    def testSample07(): Bool =
        Assert.eq(Some(0) :: Some(1) :: Some(2) :: Some(3) :: Some(4) :: Some(5) :: Nil, sample(6))

    @Test
    def testSample08(): Bool =
        Assert.eq(range(0, 10), sample(10))

    @Test
    def testSample09(): Bool =
        Assert.eq(range(0, 100), sample(100))

}
