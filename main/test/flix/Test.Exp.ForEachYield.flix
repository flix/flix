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

    @test
    def testForEachYieldMix03(): List[(Int32, Int32, String)] =
        let c = List.toChain((1, 2) :: (3, 4) :: Nil);
        let s = List.toSet("1" :: "2" :: "3" :: Nil);
        foreach ((x, y) <- c; z <- s)
            yield (x, y, z)

    @test
    def testForEachYieldMix04(): Set[(Int32, Int32, String)] =
        let c = List.toChain((1, 2) :: (3, 4) :: Nil);
        let s = List.toSet("1" :: "2" :: "3" :: Nil);
        foreach ((x, y) <- c; z <- s)
            yield (x, y, z)

    @test
    def testForEachYieldMix05(): Chain[(Int32, Int32, String)] =
        let c = List.toChain((1, 2) :: (3, 4) :: Nil);
        let s = List.toSet("1" :: "2" :: "3" :: Nil);
        foreach ((x, y) <- c; z <- s)
            yield (x, y, z)

    @test
    def testForEachYieldMix06(): List[(Int32, Int32, String)] =
        let c = List.toSet((1, 2) :: (3, 4) :: Nil);
        let s = List.toChain("1" :: "2" :: "3" :: Nil);
        foreach ((x, y) <- c; z <- s)
            yield (x, y, z)

    @test
    def testForEachYieldMix07(): Set[(Int32, Int32, String)] =
        let c = List.toSet((1, 2) :: (3, 4) :: Nil);
        let s = List.toChain("1" :: "2" :: "3" :: Nil);
        foreach ((x, y) <- c; z <- s)
            yield (x, y, z)

    @test
    def testForEachYieldMix08(): Chain[(Int32, Int32, String)] =
        let c = List.toSet((1, 2) :: (3, 4) :: Nil);
        let s = List.toChain("1" :: "2" :: "3" :: Nil);
        foreach ((x, y) <- c; z <- s)
            yield (x, y, z)

    @test
    def testForEachYieldMix09(): List[(Int32, Int32, String)] =
        let c = (1, 2) :: (3, 4) :: Nil;
        let s = List.toChain("1" :: "2" :: "3" :: Nil);
        foreach ((x, y) <- c; z <- s)
            yield (x, y, z)

    @test
    def testForEachYieldMix10(): Set[(Int32, Int32, String)] =
        let c = (1, 2) :: (3, 4) :: Nil;
        let s = List.toChain("1" :: "2" :: "3" :: Nil);
        foreach ((x, y) <- c; z <- s)
            yield (x, y, z)

    @test
    def testForEachYieldMix11(): Chain[(Int32, Int32, String)] =
        let c = (1, 2) :: (3, 4) :: Nil;
        let s = List.toChain("1" :: "2" :: "3" :: Nil);
        foreach ((x, y) <- c; z <- s)
            yield (x, y, z)

    @test
    def testForEachYieldMix12(): List[(Int32, Int32, String)] =
        let c = List.toSet((1, 2) :: (3, 4) :: Nil);
        let s = "1" :: "2" :: "3" :: Nil;
        foreach ((x, y) <- c; z <- s)
            yield (x, y, z)

    @test
    def testForEachYieldMix13(): Set[(Int32, Int32, String)] =
        let c = List.toSet((1, 2) :: (3, 4) :: Nil);
        let s = "1" :: "2" :: "3" :: Nil;
        foreach ((x, y) <- c; z <- s)
            yield (x, y, z)

    @test
    def testForEachYieldMix14(): Chain[(Int32, Int32, String)] =
        let c = List.toSet((1, 2) :: (3, 4) :: Nil);
        let s = "1" :: "2" :: "3" :: Nil;
        foreach ((x, y) <- c; z <- s)
            yield (x, y, z)

    @test
    def testForEachYieldMix15(): List[(Int32, Int32, String)] =
        let c = List.toSet((1, 2) :: (3, 4) :: Nil);
        let s = Map#{"1" => true, "2" => true, "3" => true};
        foreach ((x, y) <- c; z <- s)
            yield (x, y, z)

    @test
    def testForEachYieldMix16(): Set[(Int32, Int32, String)] =
        let c = List.toSet((1, 2) :: (3, 4) :: Nil);
        let s = Map#{"1" => true, "2" => true, "3" => true};
        foreach ((x, y) <- c; z <- s)
            yield (x, y, z)

    @test
    def testForEachYieldMix17(): Chain[(Int32, Int32, String)] =
        let c = List.toSet((1, 2) :: (3, 4) :: Nil);
        let s = Map#{"1" => true, "2" => true, "3" => true};
        foreach ((x, y) <- c; z <- s)
            yield (x, y, z)

}
