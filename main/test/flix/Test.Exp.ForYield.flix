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
            for (x <- 1 :: 2 :: 3 :: Nil) yield
                for (y <- 4 :: 5 :: Nil)
                    yield (x, y);
        l == (1, 4) :: (1, 5) :: (2, 4) :: (2, 5) :: (3, 4) :: (3, 5) :: Nil
}
