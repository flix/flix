namespace Test/Exp/Foreach {

    @test
    def testForeach01(): Bool = region r {
        let y = ref 0 @ r;
        foreach (x <- 1 :: Nil) y := x + 1;
        deref y == 2
    }

    @test
    def testForeach02(): Bool = region r {
        let y = ref 0 @ r;
        foreach (x <- 1 :: Nil)
            y := x + 1;
        deref y == 2
    }

    @test
    def testForeach03(): Bool = region r {
        let z = ref 0 @ r;
        foreach ((x, y) <- (1, 2) :: (3, 4) :: Nil) z := deref z + x + y; // ForEach.foreach(match (x, y) -> z := deref z + x + y, (1, 2) :: (3, 4) :: Nil)
        deref z == 10
    }

    @test
    def testForeach04(): Bool = region r {
        let z = ref 0 @ r;
        foreach ((x, y) <- (1, 2) :: (3, 4) :: Nil)
            z := deref z + x + y; // ForEach.foreach(match (x, y) -> z := deref z + x + y, (1, 2) :: (3, 4) :: Nil)
        deref z == 10
    }

    @test
    def testForeach05(): Bool = region r {
        let z = ref 0 @ r;
        let l = (1, 2) :: (3, 4) :: Nil;
        foreach ((x, y) <- l)
            z := deref z + x + y; // ForEach.foreach(match (x, y) -> z := deref z + x + y, l)
        deref z == 10
    }

    @test
    def testForeach06(): Bool = region r {
        let z = ref 0 @ r;
        let l = (1, 2) :: (3, 4) :: Nil;
        foreach ((x, _) <- l)
            z := deref z + x; // ForEach.foreach(match (x, _) -> z := deref z + x, l)
        deref z == 4
    }

}
