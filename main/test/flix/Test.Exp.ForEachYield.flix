mod Test.Exp.ForEachYield {

    @Test
    def testForEachYield01(): List[Int32] =
        foreach (x <- 1 :: 2 :: Nil)
            yield x

    @Test
    def testForEachYieldResult01(): Bool =
        (foreach (x <- 1 :: 2 :: Nil)
            yield x) == 1 :: 2 :: Nil

    @Test
    def testForEachYield02(): List[(Int32, Int32)] =
        foreach (x <- 1 :: 2 :: Nil; y <- 1 :: 2 :: 3 :: Nil)
            yield (x, y)

    @Test
    def testForEachYieldResult02(): Bool =
        (foreach (x <- 1 :: 2 :: Nil; y <- 1 :: 2 :: 3 :: Nil)
            yield (x, y)) == (1, 1) :: (1, 2) :: (1, 3) :: (2, 1) :: (2, 2) :: (2, 3) :: Nil

    @Test
    def testForEachYield03(): List[(Int32, Int32, String)] =
        foreach ((x, y) <- (1, 2) :: (3, 4) :: Nil; z <- "1" :: "2" :: "3" :: Nil)
            yield (x, y, z)

    @Test
    def testForEachYieldResult03(): Bool =
        (foreach ((x, y) <- (1, 2) :: (3, 4) :: Nil; z <- "1" :: "2" :: "3" :: Nil)
            yield (x, y, z)) == (1, 2, "1") :: (1, 2, "2") :: (1, 2, "3") :: (3, 4, "1") :: (3, 4, "2") :: (3, 4, "3") :: Nil

    @Test
    def testForEachYield04(): List[(Int32, String)] =
        foreach ((x, _) <- (1, 2) :: (3, 4) :: Nil; y <- "1" :: "2" :: "3" :: Nil)
            yield (x, y)

    @Test
    def testForEachYieldResult04(): Bool =
        (foreach ((x, _) <- (1, 2) :: (3, 4) :: Nil; y <- "1" :: "2" :: "3" :: Nil)
            yield (x, y)) == (1, "1") :: (1, "2") :: (1, "3") :: (3, "1") :: (3, "2") :: (3, "3") :: Nil

    @Test
    def testForEachYield05(): List[(Int32, String)] =
        foreach ((x, _) <- (1, 2) :: (3, 4) :: Nil; if x > 2; y <- "1" :: "2" :: "3" :: Nil)
            yield (x, y)

    @Test
    def testForEachYieldResult05(): Bool =
        (foreach ((x, _) <- (1, 2) :: (3, 4) :: Nil; if x > 2; y <- "1" :: "2" :: "3" :: Nil)
            yield (x, y)) == (3, "1") :: (3, "2") :: (3, "3") :: Nil

    @Test
    def testForEachYield06(): List[(Int32, String)] =
        foreach ((x, _) <- (1, 2) :: (3, 4) :: Nil; y <- "1" :: "2" :: "3" :: Nil; if x > 2)
            yield (x, y)

    @Test
    def testForEachYieldResult06(): Bool =
        (foreach ((x, _) <- (1, 2) :: (3, 4) :: Nil; y <- "1" :: "2" :: "3" :: Nil; if x > 2)
            yield (x, y)) == (3, "1") :: (3, "2") :: (3, "3") :: Nil

    @Test
    def testForEachYield07(): List[(Int32, String)] =
        foreach ((x, _) <- (1, 2) :: (3, 4) :: Nil; y <- "1" :: "2" :: "3" :: Nil; if y != "1")
            yield (x, y)

    @Test
    def testForEachYieldResult07(): Bool =
        (foreach ((x, _) <- (1, 2) :: (3, 4) :: Nil; y <- "1" :: "2" :: "3" :: Nil; if y != "1")
            yield (x, y)) == (1, "2") :: (1, "3") :: (3, "2") :: (3, "3") :: Nil

    @Test
    def testForEachYield08(): List[(Int32, String, Int32)] =
        foreach ((x, y) <- (-1, 0) :: (1, 2) :: (3, 4) :: Nil; z <- "1" :: "2" :: "3" :: Nil; if (x, y) != (3, 4))
            yield (x, z, y)

    @Test
    def testForEachYieldResult08(): Bool =
        (foreach ((x, y) <- (-1, 0) :: (1, 2) :: (3, 4) :: Nil; z <- "1" :: "2" :: "3" :: Nil; if (x, y) != (3, 4))
            yield (x, z, y)) == (-1, "1", 0) :: (-1, "2", 0) :: (-1, "3", 0) :: (1, "1", 2) :: (1, "2", 2) :: (1, "3", 2) :: Nil

    @Test
    def testForEachYield09(): List[(Int32, String, Int32)] =
        foreach ((x, y) <- (-1, 0) :: (1, 2) :: (3, 4) :: Nil; z <- "1" :: "2" :: "3" :: Nil; if (x, y) != (3, 3))
            yield (x, z, y)

    @Test
    def testForEachYieldResult09(): Bool =
        (foreach ((x, y) <- (-1, 0) :: (1, 2) :: (3, 4) :: Nil; z <- "1" :: "2" :: "3" :: Nil; if (x, y) != (3, 3))
            yield (x, z, y)) == (-1, "1", 0) :: (-1, "2", 0) :: (-1, "3", 0) :: (1, "1", 2) :: (1, "2", 2) :: (1, "3", 2) :: (3, "1", 4) :: (3, "2", 4) :: (3, "3", 4) :: Nil

    @Test
    def testForEachYield10(): List[(Int32, String, Int32)] =
        foreach ((x, y) <- (-1, 0) :: (1, 2) :: (3, 4) :: Nil; if y < 2; z <- "1" :: "2" :: "3" :: Nil)
            yield (x, z, y)

    @Test
    def testForEachYieldResult10(): Bool =
        (foreach ((x, y) <- (-1, 0) :: (1, 2) :: (3, 4) :: Nil; if y < 2; z <- "1" :: "2" :: "3" :: Nil)
            yield (x, z, y)) == (-1, "1", 0) :: (-1, "2", 0) :: (-1, "3", 0) :: Nil

    @Test
    def testForEachYield11(): List[(Int32, String, Int32)] =
        foreach ((x, y) <- (-1, 0) :: (1, 2) :: (3, 4) :: Nil; if y < 0; z <- "1" :: "2" :: "3" :: Nil)
            yield (x, z, y)

    @Test
    def testForEachYieldResult11(): Bool =
        (foreach ((x, y) <- (-1, 0) :: (1, 2) :: (3, 4) :: Nil; if y < 0; z <- "1" :: "2" :: "3" :: Nil)
            yield (x, z, y)) == Nil: List[(Int32, String, Int32)]

    @Test
    def testForEachYield12(): List[(Int32, String, Int32)] =
        foreach ((x, y) <- (-1, 0) :: (1, 2) :: (3, 4) :: Nil; z <- "1" :: "2" :: "3" :: Nil; if (x, y) != (1, 2))
            yield (x, z, y)

    @Test
    def testForEachYieldResult12(): Bool =
        (foreach ((x, y) <- (-1, 0) :: (1, 2) :: (3, 4) :: Nil; z <- "1" :: "2" :: "3" :: Nil; if (x, y) != (1, 2))
            yield (x, z, y)) == (-1, "1", 0) :: (-1, "2", 0) :: (-1, "3", 0) :: (3, "1", 4) :: (3, "2", 4) :: (3, "3", 4) :: Nil

    @Test
    def testForEachYieldMix01(): Set[(Int32, Int32, String)] =
        foreach ((x, y) <- (1, 2) :: (3, 4) :: Nil; z <- "1" :: "2" :: "3" :: Nil)
            yield (x, y, z)

    @Test
    def testForEachYieldMix02(): Chain[(Int32, Int32, String)] =
        foreach ((x, y) <- (1, 2) :: (3, 4) :: Nil; z <- "1" :: "2" :: "3" :: Nil)
            yield (x, y, z)

    @Test
    def testForEachYieldMix03(): List[(Int32, Int32, String)] =
        let a = List.toChain((1, 2) :: (3, 4) :: Nil);
        let b = List.toSet("1" :: "2" :: "3" :: Nil);
        foreach ((x, y) <- a; z <- b)
            yield (x, y, z)

    @Test
    def testForEachYieldMix04(): Set[(Int32, Int32, String)] =
        let a = List.toChain((1, 2) :: (3, 4) :: Nil);
        let b = List.toSet("1" :: "2" :: "3" :: Nil);
        foreach ((x, y) <- a; z <- b)
            yield (x, y, z)

    @Test
    def testForEachYieldMix05(): Chain[(Int32, Int32, String)] =
        let a = List.toChain((1, 2) :: (3, 4) :: Nil);
        let b = List.toSet("1" :: "2" :: "3" :: Nil);
        foreach ((x, y) <- a; z <- b)
            yield (x, y, z)

    @Test
    def testForEachYieldMix06(): List[(Int32, Int32, String)] =
        let a = List.toSet((1, 2) :: (3, 4) :: Nil);
        let b = List.toChain("1" :: "2" :: "3" :: Nil);
        foreach ((x, y) <- a; z <- b)
            yield (x, y, z)

    @Test
    def testForEachYieldMix07(): Set[(Int32, Int32, String)] =
        let a = List.toSet((1, 2) :: (3, 4) :: Nil);
        let b = List.toChain("1" :: "2" :: "3" :: Nil);
        foreach ((x, y) <- a; z <- b)
            yield (x, y, z)

    @Test
    def testForEachYieldMix08(): Chain[(Int32, Int32, String)] =
        let a = List.toSet((1, 2) :: (3, 4) :: Nil);
        let b = List.toChain("1" :: "2" :: "3" :: Nil);
        foreach ((x, y) <- a; z <- b)
            yield (x, y, z)

    @Test
    def testForEachYieldMix09(): List[(Int32, Int32, String)] =
        let a = (1, 2) :: (3, 4) :: Nil;
        let b = List.toChain("1" :: "2" :: "3" :: Nil);
        foreach ((x, y) <- a; z <- b)
            yield (x, y, z)

    @Test
    def testForEachYieldMix10(): Set[(Int32, Int32, String)] =
        let a = (1, 2) :: (3, 4) :: Nil;
        let b = List.toChain("1" :: "2" :: "3" :: Nil);
        foreach ((x, y) <- a; z <- b)
            yield (x, y, z)

    @Test
    def testForEachYieldMix11(): Chain[(Int32, Int32, String)] =
        let a = (1, 2) :: (3, 4) :: Nil;
        let b = List.toChain("1" :: "2" :: "3" :: Nil);
        foreach ((x, y) <- a; z <- b)
            yield (x, y, z)

    @Test
    def testForEachYieldMix12(): List[(Int32, Int32, String)] =
        let a = List.toSet((1, 2) :: (3, 4) :: Nil);
        let b = "1" :: "2" :: "3" :: Nil;
        foreach ((x, y) <- a; z <- b)
            yield (x, y, z)

    @Test
    def testForEachYieldMix13(): Set[(Int32, Int32, String)] =
        let a = List.toSet((1, 2) :: (3, 4) :: Nil);
        let b = "1" :: "2" :: "3" :: Nil;
        foreach ((x, y) <- a; z <- b)
            yield (x, y, z)

    @Test
    def testForEachYieldMix14(): Chain[(Int32, Int32, String)] =
        let a = List.toSet((1, 2) :: (3, 4) :: Nil);
        let b = "1" :: "2" :: "3" :: Nil;
        foreach ((x, y) <- a; z <- b)
            yield (x, y, z)

    @Test
    def testForEachYieldMix15(): List[(Int32, Int32, Bool)] =
        let a = List.toSet((1, 2) :: (3, 4) :: Nil);
        let b = Map#{"1" => true, "2" => true, "3" => true};
        foreach ((x, y) <- a; z <- b)
            yield (x, y, z)

    @Test
    def testForEachYieldMix16(): Set[(Int32, Int32, Bool)] =
        let a = List.toSet((1, 2) :: (3, 4) :: Nil);
        let b = Map#{"1" => true, "2" => true, "3" => true};
        foreach ((x, y) <- a; z <- b)
            yield (x, y, z)

    @Test
    def testForEachYieldMix17(): Chain[(Int32, Int32, Bool)] =
        let a = List.toSet((1, 2) :: (3, 4) :: Nil);
        let b = Map#{"1" => true, "2" => true, "3" => true};
        foreach ((x, y) <- a; z <- b)
            yield (x, y, z)

    // N.B. There is no instance of `Order` on `Chain` so we cannot have `m[Chain[a]] with Collectable[m]`

    @Test
    def testForEachYieldMix18(): List[List[(Int32, String, Bool, Int32)]] =
        let a = List.toSet((1, 2) :: (3, 4) :: Nil);
        let b = Map#{"1" => true, "2" => true, "3" => true};
        let c = Set#{"5", "6", "7"};
        foreach ((x, y) <- a; z <- b)
            yield
                foreach (q <- c)
                    yield (x, q, z, y)

    @Test
    def testForEachYieldMix19(): List[Set[(Int32, String, Bool, Int32)]] =
        let a = List.toSet((1, 2) :: (3, 4) :: Nil);
        let b = Map#{"1" => true, "2" => true, "3" => true};
        let c = Set#{"5", "6", "7"};
        foreach ((x, y) <- a; z <- b)
            yield
                foreach (q <- c)
                    yield (x, q, z, y)

    @Test
    def testForEachYieldMix20(): Set[List[(Int32, String, Bool, Int32)]] =
        let a = List.toSet((1, 2) :: (3, 4) :: Nil);
        let b = Map#{"1" => true, "2" => true, "3" => true};
        let c = Set#{"5", "6", "7"};
        foreach ((x, y) <- a; z <- b)
            yield
                foreach (q <- c)
                    yield (x, q, z, y)

    @Test
    def testForEachYieldMix21(): Set[Set[(Int32, String, Bool, Int32)]] =
        let a = List.toSet((1, 2) :: (3, 4) :: Nil);
        let b = Map#{"1" => true, "2" => true, "3" => true};
        let c = Set#{"5", "6", "7"};
        foreach ((x, y) <- a; z <- b)
            yield
                foreach (q <- c)
                    yield (x, q, z, y)

    @Test
    def testForEachYieldMix22(): Chain[List[(Int32, String, Bool, Int32)]] =
        let a = List.toSet((1, 2) :: (3, 4) :: Nil);
        let b = Map#{"1" => true, "2" => true, "3" => true};
        let c = Set#{"5", "6", "7"};
        foreach ((x, y) <- a; z <- b)
            yield
                foreach (q <- c)
                    yield (x, q, z, y)

    @Test
    def testForEachYieldMix23(): Chain[Set[(Int32, String, Bool, Int32)]] =
        let a = List.toSet((1, 2) :: (3, 4) :: Nil);
        let b = Map#{"1" => true, "2" => true, "3" => true};
        let c = Set#{"5", "6", "7"};
        foreach ((x, y) <- a; z <- b)
            yield
                foreach (q <- c)
                    yield (x, q, z, y)

}
