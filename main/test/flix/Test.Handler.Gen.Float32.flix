mod Test.Handler.Gen.Float32 {

    use Assert.assertEq;

    eff Gen {
        def gen(): Float32
    }

    def generator(): Unit \ Gen =
        discard Gen.gen(); generator()

    def sample(limit: Float32): List[Float32] =
        region rc {
            let counter = Ref.fresh(rc, 0.0f32);
            run {
                generator(); Nil
            } with handler Gen {
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
