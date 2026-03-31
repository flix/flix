mod Test.Handler.Gen.Int64 {

    use Assert.assertEq;

    eff Gen {
        def gen(): Int64
    }

    def generator(): Unit \ Gen =
        discard Gen.gen(); generator()

    def sample(limit: Int64): List[Int64] =
        region rc {
            let counter = Ref.fresh(rc, 0i64);
            run {
                generator(); Nil
            } with handler Gen {
                def gen(k) =
                    let i = getAndInc(counter);
                    if (i == limit) Nil else i :: k(i)
            }
        }

    def getAndInc(r: Ref[Int64, r]): Int64 \ r =
        let i = Ref.get(r);
        Ref.put(i + 1i64, r);
        i

    def range(a: Int64, b: Int64): List[Int64] =
        List.unfold(i -> if (i >= b) None else Some((i, i + 1i64)), a)

    @Test
    def testSample01(): Unit \ Assert =
        assertEq(expected = Nil, sample(0i64))

    @Test
    def testSample02(): Unit \ Assert =
        assertEq(expected = 0i64 :: Nil, sample(1i64))

    @Test
    def testSample03(): Unit \ Assert =
        assertEq(expected = 0i64 :: 1i64 :: Nil, sample(2i64))

    @Test
    def testSample04(): Unit \ Assert =
        assertEq(expected = 0i64 :: 1i64 :: 2i64 :: Nil, sample(3i64))

    @Test
    def testSample05(): Unit \ Assert =
        assertEq(expected = 0i64 :: 1i64 :: 2i64 :: 3i64 :: Nil, sample(4i64))

    @Test
    def testSample06(): Unit \ Assert =
        assertEq(expected = 0i64 :: 1i64 :: 2i64 :: 3i64 :: 4i64 :: Nil, sample(5i64))

    @Test
    def testSample07(): Unit \ Assert =
        assertEq(expected = 0i64 :: 1i64 :: 2i64 :: 3i64 :: 4i64 :: 5i64 :: Nil, sample(6i64))

    @Test
    def testSample08(): Unit \ Assert =
        assertEq(expected = range(0i64, 10i64), sample(10i64))

    @Test
    def testSample09(): Unit \ Assert =
        assertEq(expected = range(0i64, 100i64), sample(100i64))

}
