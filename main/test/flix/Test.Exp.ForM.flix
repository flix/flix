mod Test.Exp.ForM {

    use Assert.assertEq

    @Test
    def testMonadicFor01(): Unit \ Assert =
        let l =
            forM (x <- 1 :: 2 :: 3 :: Nil)
                yield x + 1;
        assertEq(expected = 2 :: 3 :: 4 :: Nil, l)

    @Test
    def testMonadicFor02(): Unit \ Assert =
        let l =
            forM( x <- 1 :: 2 :: 3 :: Nil  )yield x + 1;
        assertEq(expected = 2 :: 3 :: 4 :: Nil, l)

    @Test
    def testMonadicFor03(): Unit \ Assert =
        let l =
            forM( x <- 1 :: 2 :: 3 :: Nil  )         yield x + 1;
        assertEq(expected = 2 :: 3 :: 4 :: Nil, l)

    @Test
    def testMonadicFor04(): Unit \ Assert =
        let l =
            forM (x <- 1 :: 2 :: 3 :: Nil;
                 y <- 4 :: 5 :: Nil)
                    yield (x, y);
        assertEq(expected = (1, 4) :: (1, 5) :: (2, 4) :: (2, 5) :: (3, 4) :: (3, 5) :: Nil, l)

    @Test
    def testMonadicFor05(): Unit \ Assert =
        let l =
            forM ((x, y) <- (1, 0) :: (2, 1) :: (3, 2) :: Nil;
                 z <- 4 :: 5 :: Nil)
                    yield (x, y, z);
        assertEq(expected = (1, 0, 4) :: (1, 0, 5) :: (2, 1, 4) :: (2, 1, 5) :: (3, 2, 4) :: (3, 2, 5) :: Nil, l)

    @Test
    def testMonadicFor06(): Unit \ Assert =
        let l =
            forM ((x, y) <- (1, 0) :: (2, 1) :: (3, 2) :: Nil;
                 z <- 4 :: 5 :: Nil;
                 _ <- List.range(0, 10))
                    yield (x, y, z);
        let l2 = List.repeat(10, (1, 0, 4) :: (1, 0, 5) :: (2, 1, 4) :: (2, 1, 5) :: (3, 2, 4) :: (3, 2, 5) :: Nil) |> List.flatten |> List.sort;
        assertEq(expected = l2, l)

    @Test
    def testMonadicFor07(): Unit \ Assert =
        let l = // Monad.flatMap(match x -> Monad.flatMap(match y -> Applicative.point((x, y)), 4 :: 5 :: Nil), 1 :: 2 :: 3 :: Nil)
            forM (x <- 1 :: 2 :: 3 :: Nil) yield
                forM (y <- 4 :: 5 :: Nil)
                    yield (x, y);
        assertEq(expected = ((1, 4) :: (1, 5) :: Nil) :: ((2, 4) :: (2, 5) :: Nil) :: ((3, 4) :: (3, 5) :: Nil) :: Nil, l)

    @Test
    def testMonadicFor08(): Unit \ Assert = region rc {
        let l1 = Ref.fresh(rc, Nil);
        let l2 =
            forM (x <- 1 :: 2 :: 3 :: Nil) yield {
                Ref.put(x :: Ref.get(l1), l1);
                x
            };
        assertEq(expected = l2, List.reverse(Ref.get(l1)))
    }

    @Test
    def testMonadicFor09(): Unit \ Assert =
        let l = // Monad.flatMap(match x -> if (x > 1) Applicative.point(x + 1) else MonadZero.empty(), 1 :: 2 :: 3 :: Nil)
            forM (x <- 1 :: 2 :: 3 :: Nil;
                 if x > 1)
                yield x + 1;
        assertEq(expected = 3 :: 4 :: Nil, l)

    @Test
    def testMonadicFor10(): Unit \ Assert =
        let l =
            forM (x <- 1 :: 2 :: 3 :: Nil;
                 if (x > 1))
                yield x + 1;
        assertEq(expected = 3 :: 4 :: Nil, l)

    @Test
    def testMonadicFor11(): Unit \ Assert =
        let l =
            forM (x <- 1 :: 2 :: 3 :: Nil;
                 if x > 1 and x < 3)
                yield x + 1;
        assertEq(expected = 3 :: Nil, l)

    @Test
    def testMonadicFor12(): Unit \ Assert =
        let l = // Monad.flatMap(match x -> if (x > 1) if (x < 3) Applicative.point(x + 1) else MonadZero.empty() else MonadZero.empty(), 1 :: 2 :: 3 :: Nil)
            forM (x <- 1 :: 2 :: 3 :: Nil;
                 if x > 1;
                 if x < 3)
                yield x + 1;
        assertEq(expected = 3 :: Nil, l)

    @Test
    def testMonadicFor13(): Unit \ Assert =
        let l0 =
            forM (x <- 1 :: 2 :: 3 :: Nil;
                 if x > 1;
                 if x < 3)
                yield x + 1;
        let l1 = Monad.flatMap(match x ->
                    if (x > 1 and x < 3)
                        Applicative.point(x + 1)
                    else MonadZero.empty(),
                    1 :: 2 :: 3 :: Nil);
        assertEq(expected = l1, l0)

    @Test
    def testMonadicFor14(): Unit \ Assert =
        let l =
            forM (x <- 1 :: 2 :: 3 :: Nil;
                 if x < 1)
                yield x + 1;
        assertEq(expected = Nil, l)

    @Test
    def testMonadicFor15(): Unit \ Assert =
        let l =
            forM (x <- 1 :: 2 :: 3 :: Nil;
                 if x > 3)
                yield x + 1;
        assertEq(expected = Nil, l)

    @Test
    def testMonadicFor16(): Unit \ Assert =
        let l =
            forM (x <- 1 :: 2 :: 3 :: Nil;
                 if x > 1;
                 if x > 3)
                yield x + 1;
        assertEq(expected = Nil, l)

    @Test
    def testMonadicFor17(): Unit \ Assert =
        let l =
            forM (x <- 1 :: 2 :: 3 :: Nil;
                 if x < 1;
                 if x < 3)
                yield x + 1;
        assertEq(expected = Nil, l)

    @Test
    def testMonadicFor18(): Unit \ Assert =
        let l =
            forM (x <- 1 :: 2 :: 3 :: Nil;
                 if x < 1;
                 if x > 3)
                yield x + 1;
        assertEq(expected = Nil, l)

    @Test
    def testMonadicFor19(): Unit \ Assert =
        let l =
            forM (x <- 1 :: 2 :: 3 :: Nil;
                 y <- 4 :: 5 :: Nil;
                 if y < 5)
                    yield (x, y);
        assertEq(expected = (1, 4) :: (2, 4) :: (3, 4) :: Nil, l)

    @Test
    def testMonadicFor20(): Unit \ Assert =
        let l =
            forM (x <- 1 :: 2 :: 3 :: Nil;
                 y <- 4 :: 5 :: Nil;
                 if y < 4)
                    yield (x, y);
        assertEq(expected = Nil, l)

    @Test
    def testMonadicFor21(): Unit \ Assert =
        let l =
            forM (x <- 1 :: 2 :: 3 :: Nil;
                 if x > 1;
                 y <- 4 :: 5 :: 6 :: Nil;
                 if y < 6)
                    yield (x, y);
        assertEq(expected = (2, 4) :: (2, 5) :: (3, 4) :: (3, 5) :: Nil, l)

}
