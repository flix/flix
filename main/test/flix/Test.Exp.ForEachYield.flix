namespace Test/Exp/ForEachYield {

    @test
    def testForEachYield01(): List[Int32] =
        foreach (x <- 1 :: 2 :: Nil)
            yield x

    @test
    def testForEachYield02(): List[(Int32, Int32)] =
        foreach (x <- 1 :: 2 :: Nil; y <- 1 :: 2 :: 3 :: Nil)
            yield (x, y)

    @test
    def testForEachYield03(): List[(Int32, Int32, String)] =
        foreach ((x, y) <- (1, 2) :: (3, 4) :: Nil; z <- "1" :: "2" :: "3" :: Nil)
            yield (x, y, z)

    @test
    def testForEachYield04(): List[(Int32, String)] =
        foreach ((x, _) <- (1, 2) :: (3, 4) :: Nil; y <- "1" :: "2" :: "3" :: Nil)
            yield (x, y)

    @test
    def testForEachYield05(): List[(Int32, String)] =
        foreach ((x, _) <- (1, 2) :: (3, 4) :: Nil; if x > 2; y <- "1" :: "2" :: "3" :: Nil)
            yield (x, y)

    @test
    def testForEachYield06(): List[(Int32, String)] =
        foreach ((x, _) <- (1, 2) :: (3, 4) :: Nil; y <- "1" :: "2" :: "3" :: Nil; if x > 2)
            yield (x, y)

    @test
    def testForEachYield07(): List[(Int32, String)] =
        foreach ((x, _) <- (1, 2) :: (3, 4) :: Nil; y <- "1" :: "2" :: "3" :: Nil; if y != "1")
            yield (x, y)

    @test
    def testForEachYieldMix01(): Set[(Int32, Int32, String)] =
        foreach ((x, y) <- (1, 2) :: (3, 4) :: Nil; z <- "1" :: "2" :: "3" :: Nil)
            yield (x, y, z)

    @test
    def testForEachYieldMix02(): Chain[(Int32, Int32, String)] =
        foreach ((x, y) <- (1, 2) :: (3, 4) :: Nil; z <- "1" :: "2" :: "3" :: Nil)
            yield (x, y, z)

}
