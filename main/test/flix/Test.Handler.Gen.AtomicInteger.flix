mod Test.Handler.Gen.AtomicInteger {

    type alias AtomicInteger = ##java.util.concurrent.atomic.AtomicInteger

    def newAtomicInteger(n: Int32): AtomicInteger =
        import new java.util.concurrent.atomic.AtomicInteger(Int32): AtomicInteger \ {} as mk;
        mk(n)

    def toInt(n: AtomicInteger): Int32 =
        import java.util.concurrent.atomic.AtomicInteger.intValue(): Int32 \ {} as intValue;
        intValue(n)

    eff Gen {
        pub def gen(): AtomicInteger
    }

    def generator(): Unit \ Gen =
        do Gen.gen(); generator()

    def sample(limit: Int32): List[Int32] =
        region rc {
            let counter = ref newAtomicInteger(0) @ rc;
            try {
                generator(); Nil
            } with Gen {
                def gen(k) =
                    let i = toInt(getAndInc(counter));
                    if (i == limit) Nil else i :: k(newAtomicInteger(i))
            }
        }

    def getAndInc(r: Ref[AtomicInteger, r]): AtomicInteger \ r =
        let i = toInt(deref r);
        r := newAtomicInteger(i + 1);
        newAtomicInteger(i)

    def range(a: AtomicInteger, b: AtomicInteger): List[Int32] =
        List.unfold(i -> if (toInt(i) >= toInt(b)) None else Some((toInt(i), newAtomicInteger(toInt(i) + 1))), a)

    @Test
    def testSample01(): Bool =
        Assert.eq(Nil, sample(0))

    @Test
    def testSample02(): Bool =
        Assert.eq(0 :: Nil, sample(1))

    @Test
    def testSample03(): Bool =
        Assert.eq(0 :: 1 :: Nil, sample(2))

    @Test
    def testSample04(): Bool =
        Assert.eq(0 :: 1 :: 2 :: Nil, sample(3))

    @Test
    def testSample05(): Bool =
        Assert.eq(0 :: 1 :: 2 :: 3 :: Nil, sample(4))

    @Test
    def testSample06(): Bool =
        Assert.eq(0 :: 1 :: 2 :: 3 :: 4 :: Nil, sample(5))

    @Test
    def testSample07(): Bool =
        Assert.eq(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil, sample(6))

    @Test
    def testSample08(): Bool =
        Assert.eq(range(newAtomicInteger(0), newAtomicInteger(10)), sample(10))

    @Test
    def testSample09(): Bool =
        Assert.eq(range(newAtomicInteger(0), newAtomicInteger(100)), sample(100))

}
