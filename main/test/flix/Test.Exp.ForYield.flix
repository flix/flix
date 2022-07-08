namespace Test/Exp/ForYield {

    @test
    def testForYield01(): Bool =
        let l =
            for (x <- 1 :: 2 :: 3 :: Nil)
                yield x + 1;
        l == 2 :: 3 :: 4 :: Nil

    @test
    def testForYield02(): Bool =
        let l =
            for( x <- 1 :: 2 :: 3 :: Nil  )yield x + 1;
        l == 2 :: 3 :: 4 :: Nil

    @test
    def testForYield03(): Bool =
        let l =
            for( x <- 1 :: 2 :: 3 :: Nil  )         yield x + 1;
        l == 2 :: 3 :: 4 :: Nil

    @test
    def testForYield04(): Bool =
        let l =
            for (x <- 1 :: 2 :: 3 :: Nil;
                 y <- 4 :: 5 :: Nil)
                    yield (x, y);
        l == (1, 4) :: (1, 5) :: (2, 4) :: (2, 5) :: (3, 4) :: (3, 5) :: Nil

    @test
    def testForYield05(): Bool =
        let l =
            for ((x, y) <- (1, 0) :: (2, 1) :: (3, 2) :: Nil;
                 z <- 4 :: 5 :: Nil)
                    yield (x, y, z);
        l == (1, 0, 4) :: (1, 0, 5) :: (2, 1, 4) :: (2, 1, 5) :: (3, 2, 4) :: (3, 2, 5) :: Nil

    @test
    def testForYield06(): Bool =
        let l =
            for ((x, y) <- (1, 0) :: (2, 1) :: (3, 2) :: Nil;
                 z <- 4 :: 5 :: Nil;
                 _ <- List.range(0, 10))
                    yield (x, y, z);
        let l2 = List.repeat(10, (1, 0, 4) :: (1, 0, 5) :: (2, 1, 4) :: (2, 1, 5) :: (3, 2, 4) :: (3, 2, 5) :: Nil) |> List.flatten |> List.sort;
        l == l2

    @test
    def testForYield07(): Bool =
        let l =
            for (x <- 1 :: 2 :: 3 :: Nil) yield
                for (y <- 4 :: 5 :: Nil)
                    yield (x, y);
        l == ((1, 4) :: (1, 5) :: Nil) :: ((2, 4) :: (2, 5) :: Nil) :: ((3, 4) :: (3, 5) :: Nil) :: Nil

    @test
    def testForYield08(): Bool = region r {
        let l1 = ref Nil @ r;
        let l2 =
            for (x <- 1 :: 2 :: 3 :: Nil) yield {
                l1 := x :: deref l1;
                x
            };
        List.reverse(deref l1) == l2
        }

    @test
    def testForYield09(): Bool =
        let l =
            for (x <- 1 :: 2 :: 3 :: Nil;
                 if x > 1)
                yield x + 1;
        // Functor.map(match x -> x + 1,
        //   FunctorFilter.filter(match x -> x > 1, 1 :: 2 :: 3 :: Nil))
        l == 3 :: 4 :: Nil

    @test
    def testForYield10(): Bool =
        let l =
            for (x <- 1 :: 2 :: 3 :: Nil;
                 if (x > 1))
                yield x + 1;
        l == 3 :: 4 :: Nil

    @test
    def testForYield11(): Bool =
        let l =
            for (x <- 1 :: 2 :: 3 :: Nil;
                 if x > 1 and x < 3)
                yield x + 1;
        l == 3 :: Nil

    @test
    def testForYield12(): Bool =
        let l =
            for (x <- 1 :: 2 :: 3 :: Nil;
                 if x > 1;
                 if x < 3)
                yield x + 1;
        l == 3 :: Nil

}
