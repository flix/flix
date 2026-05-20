mod Test.Handler.Gen.Bool {

    use Assert.assertEq;

    eff Gen {
        def gen(): Bool
    }

    def generator(): Unit \ Gen =
        discard Gen.gen(); generator()

    def sample(limit: Int32): List[Bool] =
        region rc {
            let counter = Ref.fresh(rc, (0, true));
            run {
                generator(); Nil
            } with handler Gen {
                def gen(k) =
                    let (i, b) = getAndInc(counter);
                    if (i == limit) Nil else b :: k(b)
            }
        }

    def getAndInc(r: Ref[(Int32, Bool), r]): (Int32, Bool) \ r =
        let (i, b) = Ref.get(r);
        Ref.put((i + 1, not b), r);
        (i, b)

    def range(a: Int32, b: Int32): List[Bool] =
        List.unfold(match (i, v) -> if (i >= b) None else Some((v, (i + 1, not v))), (a, true))

    @Test
    def testSample01(): Unit \ Assert =
        assertEq(expected = Nil, sample(0))

    @Test
    def testSample02(): Unit \ Assert =
        assertEq(expected = true :: Nil, sample(1))

    @Test
    def testSample03(): Unit \ Assert =
        assertEq(expected = true :: false :: Nil, sample(2))

    @Test
    def testSample04(): Unit \ Assert =
        assertEq(expected = true :: false :: true :: Nil, sample(3))

    @Test
    def testSample05(): Unit \ Assert =
        assertEq(expected = true :: false :: true :: false :: Nil, sample(4))

    @Test
    def testSample06(): Unit \ Assert =
        assertEq(expected = true :: false :: true :: false :: true :: Nil, sample(5))

    @Test
    def testSample07(): Unit \ Assert =
        assertEq(expected = true :: false :: true :: false :: true :: false :: Nil, sample(6))

    @Test
    def testSample08(): Unit \ Assert =
        assertEq(expected = range(0, 10), sample(10))

    @Test
    def testSample09(): Unit \ Assert =
        assertEq(expected = range(0, 100), sample(100))

}
