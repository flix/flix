mod Test.Handler.Gen.Unit {

    eff Gen {
        def gen(): Unit
    }

    def generator(): Unit \ Gen =
        Gen.gen(); generator()

    def sample(limit: Int32): List[Unit] =
        region rc {
            let counter = Ref.fresh(rc, 0);
            try {
                generator(); Nil
            } with Gen {
                def gen(k) =
                    let i = getAndInc(counter);
                    if (i == limit) Nil else () :: k()
            }
        }

    def getAndInc(r: Ref[Int32, r]): Int32 \ r =
        let i = Ref.get(r);
        Ref.put(i + 1, r);
        i

    def range(a: Int32, b: Int32): List[Unit] =
        List.unfold(i -> if (i >= b) None else Some(((), i + 1)), a)

    @Test
    def testSample01(): Bool =
        Assert.eq(Nil, sample(0))

    @Test
    def testSample02(): Bool =
        Assert.eq(() :: Nil, sample(1))

    @Test
    def testSample03(): Bool =
        Assert.eq(() :: () :: Nil, sample(2))

    @Test
    def testSample04(): Bool =
        Assert.eq(() :: () :: () :: Nil, sample(3))

    @Test
    def testSample05(): Bool =
        Assert.eq(() :: () :: () :: () :: Nil, sample(4))

    @Test
    def testSample06(): Bool =
        Assert.eq(() :: () :: () :: () :: () :: Nil, sample(5))

    @Test
    def testSample07(): Bool =
        Assert.eq(() :: () :: () :: () :: () :: () :: Nil, sample(6))

    @Test
    def testSample08(): Bool =
        Assert.eq(range(0, 10), sample(10))

    @Test
    def testSample09(): Bool =
        Assert.eq(range(0, 100), sample(100))

}
