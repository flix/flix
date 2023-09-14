mod Test.Exp.MonadicFor {

    @Test
    def testMonadicFor01(): Bool =
        let l =
            forM (x <- 1 :: 2 :: 3 :: Nil)
                yield x + 1;
        l == 2 :: 3 :: 4 :: Nil

    @Test
    def testMonadicFor02(): Bool =
        let l =
            forM( x <- 1 :: 2 :: 3 :: Nil  )yield x + 1;
        l == 2 :: 3 :: 4 :: Nil

    @Test
    def testMonadicFor03(): Bool =
        let l =
            forM( x <- 1 :: 2 :: 3 :: Nil  )         yield x + 1;
        l == 2 :: 3 :: 4 :: Nil

    @Test
    def testMonadicFor04(): Bool =
        let l =
            forM (x <- 1 :: 2 :: 3 :: Nil;
                 y <- 4 :: 5 :: Nil)
                    yield (x, y);
        l == (1, 4) :: (1, 5) :: (2, 4) :: (2, 5) :: (3, 4) :: (3, 5) :: Nil

    @Test
    def testMonadicFor05(): Bool =
        let l =
            forM ((x, y) <- (1, 0) :: (2, 1) :: (3, 2) :: Nil;
                 z <- 4 :: 5 :: Nil)
                    yield (x, y, z);
        l == (1, 0, 4) :: (1, 0, 5) :: (2, 1, 4) :: (2, 1, 5) :: (3, 2, 4) :: (3, 2, 5) :: Nil

    @Test
    def testMonadicFor06(): Bool =
        let l =
            forM ((x, y) <- (1, 0) :: (2, 1) :: (3, 2) :: Nil;
                 z <- 4 :: 5 :: Nil;
                 _ <- List.range(0, 10))
                    yield (x, y, z);
        let l2 = List.repeat(10, (1, 0, 4) :: (1, 0, 5) :: (2, 1, 4) :: (2, 1, 5) :: (3, 2, 4) :: (3, 2, 5) :: Nil) |> List.flatten |> List.sort;
        l == l2

    @Test
    def testMonadicFor07(): Bool =
        let l = // Monad.flatMap(match x -> Monad.flatMap(match y -> Applicative.point((x, y)), 4 :: 5 :: Nil), 1 :: 2 :: 3 :: Nil)
            forM (x <- 1 :: 2 :: 3 :: Nil) yield
                forM (y <- 4 :: 5 :: Nil)
                    yield (x, y);
        l == ((1, 4) :: (1, 5) :: Nil) :: ((2, 4) :: (2, 5) :: Nil) :: ((3, 4) :: (3, 5) :: Nil) :: Nil

    @Test
    def testMonadicFor08(): Bool = region rc {
        let l1 = ref Nil @ rc;
        let l2 =
            forM (x <- 1 :: 2 :: 3 :: Nil) yield {
                l1 := x :: deref l1;
                x
            };
        List.reverse(deref l1) == l2
    }

    @Test
    def testMonadicFor09(): Bool =
        let l = // Monad.flatMap(match x -> if (x > 1) Applicative.point(x + 1) else MonadZero.empty(), 1 :: 2 :: 3 :: Nil)
            forM (x <- 1 :: 2 :: 3 :: Nil;
                 if x > 1)
                yield x + 1;
        l == 3 :: 4 :: Nil

    @Test
    def testMonadicFor10(): Bool =
        let l =
            forM (x <- 1 :: 2 :: 3 :: Nil;
                 if (x > 1))
                yield x + 1;
        l == 3 :: 4 :: Nil

    @Test
    def testMonadicFor11(): Bool =
        let l =
            forM (x <- 1 :: 2 :: 3 :: Nil;
                 if x > 1 and x < 3)
                yield x + 1;
        l == 3 :: Nil

    @Test
    def testMonadicFor12(): Bool =
        let l = // Monad.flatMap(match x -> if (x > 1) if (x < 3) Applicative.point(x + 1) else MonadZero.empty() else MonadZero.empty(), 1 :: 2 :: 3 :: Nil)
            forM (x <- 1 :: 2 :: 3 :: Nil;
                 if x > 1;
                 if x < 3)
                yield x + 1;
        l == 3 :: Nil

    @Test
    def testMonadicFor13(): Bool =
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
        l0 == l1

    @Test
    def testMonadicFor14(): Bool =
        let l =
            forM (x <- 1 :: 2 :: 3 :: Nil;
                 if x < 1)
                yield x + 1;
        l == Nil

    @Test
    def testMonadicFor15(): Bool =
        let l =
            forM (x <- 1 :: 2 :: 3 :: Nil;
                 if x > 3)
                yield x + 1;
        l == Nil

    @Test
    def testMonadicFor16(): Bool =
        let l =
            forM (x <- 1 :: 2 :: 3 :: Nil;
                 if x > 1;
                 if x > 3)
                yield x + 1;
        l == Nil

    @Test
    def testMonadicFor17(): Bool =
        let l =
            forM (x <- 1 :: 2 :: 3 :: Nil;
                 if x < 1;
                 if x < 3)
                yield x + 1;
        l == Nil

    @Test
    def testMonadicFor18(): Bool =
        let l =
            forM (x <- 1 :: 2 :: 3 :: Nil;
                 if x < 1;
                 if x > 3)
                yield x + 1;
        l == Nil

    @Test
    def testMonadicFor19(): Bool =
        let l =
            forM (x <- 1 :: 2 :: 3 :: Nil;
                 y <- 4 :: 5 :: Nil;
                 if y < 5)
                    yield (x, y);
        l == (1, 4) :: (2, 4) :: (3, 4) :: Nil

    @Test
    def testMonadicFor20(): Bool =
        let l =
            forM (x <- 1 :: 2 :: 3 :: Nil;
                 y <- 4 :: 5 :: Nil;
                 if y < 4)
                    yield (x, y);
        l == Nil

    @Test
    def testMonadicFor21(): Bool =
        let l =
            forM (x <- 1 :: 2 :: 3 :: Nil;
                 if x > 1;
                 y <- 4 :: 5 :: 6 :: Nil;
                 if y < 6)
                    yield (x, y);
        l == (2, 4) :: (2, 5) :: (3, 4) :: (3, 5) :: Nil

    @Test
    def testMonadicFor22(): Bool =
        let l0 =
            forM (x <- 1 :: 2 :: 3 :: Nil;
                 if x > 1;
                 y <- 4 :: 5 :: 6 :: Nil;
                 if y < 6)
                    yield (x, y);

        let l1 = Monad.flatMap(match x ->
                    Monad.flatMap(match y ->
                        if (x > 1)
                            if (y < 6)
                                Applicative.point((x, y))
                            else
                                MonadZero.empty()
                        else
                            MonadZero.empty(),
                        4 :: 5 :: 6 :: Nil),
                    1 :: 2 :: 3 :: Nil);
        l0 == l1


    @Test
    def testMonadicFor23(): Bool =
        let l =
            forM (x <- 1 :: 2 :: 3 :: Nil;
                 y <- 4 :: 5 :: 6 :: Nil;
                 if x > 1;
                 if y < 6)
                    yield (x, y);
        // Monad.flatMap(match x ->
        //     Monad.flatMap(match y ->
        //         if (x > 1)
        //             if (y < 6)
        //                 Applicative.point((x, y))
        //             else
        //                 MonadZero.empty()
        //         else
        //             MonadZero.empty(),
        //         4 :: 5 :: 6 :: Nil),
        //    1 :: 2 :: 3 :: Nil)
        l == (2, 4) :: (2, 5) :: (3, 4) :: (3, 5) :: Nil

}
