mod Test.Handler.Gen.Float32 {

    eff Gen {
        def gen(): Float32
    }

    def generator(): Unit \ Gen =
        Gen.gen(); generator()

    def sample(limit: Float32): List[Float32] =
        region rc {
            let counter = Ref.fresh(rc, 0.0f32);
            try {
                generator(); Nil
            } with Gen {
                def gen(k) =
                    let i = getAndInc(counter);
                    if (i == limit) Nil else i :: k(i)
            }
        }

    def getAndInc(r: Ref[Float32, r]): Float32 \ r =
        let i = Ref.get(r);
        Ref.put(i + 1.0f32, r);
        i

    def range(a: Float32, b: Float32): List[Float32] =
        List.unfold(i -> if (i >= b) None else Some((i, i + 1.0f32)), a)

    @Test
    def testSample01(): Bool =
        Assert.eq(Nil, sample(0.0f32))

    @Test
    def testSample02(): Bool =
        Assert.eq(0.0f32 :: Nil, sample(1.0f32))

    @Test
    def testSample03(): Bool =
        Assert.eq(0.0f32 :: 1.0f32 :: Nil, sample(2.0f32))

    @Test
    def testSample04(): Bool =
        Assert.eq(0.0f32 :: 1.0f32 :: 2.0f32 :: Nil, sample(3.0f32))

    @Test
    def testSample05(): Bool =
        Assert.eq(0.0f32 :: 1.0f32 :: 2.0f32 :: 3.0f32 :: Nil, sample(4.0f32))

    @Test
    def testSample06(): Bool =
        Assert.eq(0.0f32 :: 1.0f32 :: 2.0f32 :: 3.0f32 :: 4.0f32 :: Nil, sample(5.0f32))

    @Test
    def testSample07(): Bool =
        Assert.eq(0.0f32 :: 1.0f32 :: 2.0f32 :: 3.0f32 :: 4.0f32 :: 5.0f32 :: Nil, sample(6.0f32))

    @Test
    def testSample08(): Bool =
        Assert.eq(range(0.0f32, 10.0f32), sample(10.0f32))

    @Test
    def testSample09(): Bool =
        Assert.eq(range(0.0f32, 100.0f32), sample(100.0f32))

}
