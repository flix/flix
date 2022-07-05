namespace Test/Exp/Foreach {

    //
    // Note that some of these tests are poorly on purpose to test parsing is correct.
    //

    @test
    def testForeach01(): Bool = region r {
        let y = ref 0 @ r;
        foreach (x <- 1 :: Nil) y := x + 1; // ForEach.foreach(match x -> y := x + 1, 1 :: Nil)
        deref y == 2
    }

    @test
    def testForeach02(): Bool = region r {
        let y = ref 0 @ r;
        foreach (x <- 1 :: Nil)
            y := x + 1; // ForEach.foreach(match x -> y := x + 1, 1 :: Nil)
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

    @test
    def testForeach07(): Bool = region r {
        let z = ref 0 @ r;
        let q = ref 1 @ r;
        let l = (1, 2) :: (3, 4) :: Nil;
        foreach ((x, y) <- l) {
            q := deref q + x;
            z := deref z + x + y
        };
        deref z + deref q == 15
    }

    @test
    def testForeach08(): Bool = region r {
        let z = ref 0 @ r;
        foreach (x <- 1 :: 2 :: Nil)
            foreach(y <- 3 :: 4 :: Nil)
                z := deref z + x + y;
        deref z == (3 + 1) + (4 + 1) + (3 + 2) + (4 + 2)
    }

    @test
    def testForeach09(): Bool = region r {
        let z = ref 0 @ r;
        foreach (x <- 1 :: 2 :: Nil)
            foreach(y <- 3 :: 4 :: Nil)
                foreach(_ <- List.range(0, 10))
                    z := deref z + (x * y);
        deref z == (3 * 1) * 10 + (4 * 1) * 10 + (3 * 2) * 10 + (4 * 2) * 10
    }

    @test
    def testForeach10(): Bool = region r {
        let z = ref 0 @ r;
        foreach (x <- 1 :: 2 :: Nil) {
            foreach(y <- 3 :: 4 :: Nil)
                foreach(_ <- List.range(0, 10))
                    z := deref z + (x * y)
        };
        deref z == (3 * 1) * 10 + (4 * 1) * 10 + (3 * 2) * 10 + (4 * 2) * 10
    }

    @test
    def testForeach11(): Bool = region r {
        let z = ref 0 @ r;
        foreach (x <- 1 :: 2 :: Nil) {
            foreach(y <- 3 :: 4 :: Nil) {
                foreach(_ <- List.range(0, 10))
                    z := deref z + (x * y)
            }
        };
        deref z == (3 * 1) * 10 + (4 * 1) * 10 + (3 * 2) * 10 + (4 * 2) * 10
    }

    @test
    def testForeach12(): Bool = region r {
        let z = ref 0 @ r;
        foreach (x <- 1 :: 2 :: Nil) {
            foreach(y <- 3 :: 4 :: Nil) {
                foreach(_ <- List.range(0, 10)) {
                    z := deref z + (x * y)
                }
            }
        };
        deref z == (3 * 1) * 10 + (4 * 1) * 10 + (3 * 2) * 10 + (4 * 2) * 10
    }

    @test
    def testForeach13(): Bool = region r {
        let z = ref 0 @ r;
        foreach (x <- 1 :: 2 :: Nil) {
            foreach(y <- 3 :: 4 :: Nil) {
                foreach(_ <- List.range(0, 10)) z := deref z + (x * y)
            }
        };
        deref z == (3 * 1) * 10 + (4 * 1) * 10 + (3 * 2) * 10 + (4 * 2) * 10
    }

    @test
    def testForeach14(): Bool = region r {
        let z = ref 0 @ r;
        foreach (x <- 1 :: 2 :: Nil) {
            foreach(y <- 3 :: 4 :: Nil)
                foreach(_ <- List.range(0, 10)) z := deref z + (x * y)
        };
        deref z == (3 * 1) * 10 + (4 * 1) * 10 + (3 * 2) * 10 + (4 * 2) * 10
    }

    @test
    def testForeach15(): Bool = region r {
        let z = ref 0 @ r;
        foreach (x <- 1 :: 2 :: Nil) foreach(y <- 3 :: 4 :: Nil) foreach(_ <- List.range(0, 10)) z := deref z + (x * y);
        deref z == (3 * 1) * 10 + (4 * 1) * 10 + (3 * 2) * 10 + (4 * 2) * 10
    }

    @test
    def testForeach16(): Bool = region r {
        let z = ref "" @ r;
        foreach (x <- "1" :: "2" :: Nil) foreach(y <- "3" :: "4" :: Nil) z := deref z + "(${x} * ${y}) + ";
        deref z == "(1 * 3) + (1 * 4) + (2 * 3) + (2 * 4) + "
    }

    @test
    def testForeach17(): Bool = region r {
        let y = ref 0 @ r;
        foreach(   x     <-      1 :: Nil   )y := x + 1; // ForEach.foreach(match x -> y := x + 1, 1 :: Nil)
        deref y == 2
    }

    @test
    def testForeach18(): Bool = region r {
        let z = ref 0 @ r;
        foreach (x <- 1 :: 2 :: Nil;y <- 3 :: 4 :: Nil)
                    z := deref z + (x * y);
        deref z == (3 * 1) + (4 * 1) + (3 * 2) + (4 * 2)
    }

    @test
    def testForeach19(): Bool = region r {
        let z = ref 0 @ r;
        foreach(  x <- 1 :: 2 :: Nil  ;   y  <-  3 :: 4 :: Nil )z := deref z + (x * y);
        deref z == (3 * 1) + (4 * 1) + (3 * 2) + (4 * 2)
    }

    @test
    def testForeach20(): Bool = region r {
        let z = ref 0 @ r;
        foreach (x <- 1 :: 2 :: Nil;
                 y <- 3 :: 4 :: Nil)
                    z := deref z + (x * y);
        deref z == (3 * 1) + (4 * 1) + (3 * 2) + (4 * 2)
    }

    @test
    def testForeach21(): Bool = region r {
        let z = ref 0 @ r;
        foreach (x <- 1 :: 2 :: Nil;
                 y <- 3 :: 4 :: Nil;
                 _ <- List.range(0, 10)) {
                    z := deref z + (x * y)
        };
        deref z == (3 * 1) * 10 + (4 * 1) * 10 + (3 * 2) * 10 + (4 * 2) * 10
    }

    @test
    def testForeach22(): Bool = region r {
        let z = ref "" @ r;
        foreach (x <- "1" :: "2" :: Nil; y <- "3" :: "4" :: Nil) z := deref z + "(${x} * ${y}) + ";
        deref z == "(1 * 3) + (1 * 4) + (2 * 3) + (2 * 4) + "
    }

    @test
    def testForeach23(): Bool = region r {
        let y = ref 0 @ r;
        foreach (x <- 1 :: 2 :: 3 :: Nil; if x > 1) y := deref y + x; // ForEach.foreach(match x -> if (x > 1) y := deref y + x else (), 1 :: 2 :: 3 :: Nil)
        deref y == 5
    }

    @test
    def testForeach24(): Bool = region r {
        let y = ref 0 @ r;
        foreach (x <- 1 :: 2 :: 3 :: Nil;
                 if x > 1)
                    y := deref y + x; // ForEach.foreach(match x -> if (x > 1) y := deref y + x else (), 1 :: 2 :: 3 :: Nil)
        deref y == 5
    }

    @test
    def testForeach25(): Bool = region r {
        let y = ref 0 @ r;
        foreach (x <- 1 :: 2 :: 3 :: Nil;
                 if x > 1) {
                    y := deref y + x // ForEach.foreach(match x -> if (x > 1) y := deref y + x else (), 1 :: 2 :: 3 :: Nil)
        };
        deref y == 5
    }

    @test
    def testForeach26(): Bool = region r {
        let y = ref 0 @ r;
        foreach (x <- 1 :: 2 :: 3 :: Nil; if x > 1) {
            y := deref y + x // ForEach.foreach(match x -> if (x > 1) y := deref y + x else (), 1 :: 2 :: 3 :: Nil)
        };
        deref y == 5
    }

    @test
    def testForeach27(): Bool = region r {
        let z = ref 0 @ r;
        foreach (x <- 1 :: 2 :: Nil;y <- 3 :: 4 :: Nil; if x > 1)
                    z := deref z + (x * y);
        deref z == (3 * 2) + (4 * 2)
    }

    @test
    def testForeach28(): Bool = region r {
        let z = ref 0 @ r;
        foreach(  x <- 1 :: 2 :: Nil  ;   y  <-  3 :: 4 :: Nil;  if x > 1 and y > 3)z := deref z + (x * y);
        deref z == (3 * 1) + (4 * 1) + (3 * 2) + (4 * 2)
    }

    @test
    def testForeach29(): Bool = region r {
        let z = ref 0 @ r;
        foreach (x <- 1 :: 2 :: Nil;
                 y <- 3 :: 4 :: Nil; if x > 1 and y > 3)
                    z := deref z + (x * y);
        deref z == (3 * 1) + (4 * 1) + (3 * 2) + (4 * 2)
    }

    @test
    def testForeach30(): Bool = region r {
        let z = ref 0 @ r;
        foreach (x <- 1 :: 2 :: Nil;
                 y <- 3 :: 4 :: Nil;
                 _ <- List.range(0, 10); if x > 1 and y > 4) {
                    z := deref z + (x * y)
        };
        deref z == 0
    }

    @test
    def testForeach31(): Bool = region r {
        let z = ref "" @ r;
        foreach (x <- "1" :: "2" :: Nil; y <- "3" :: "4" :: Nil; if x != "1") z := deref z + "(${x} * ${y}) + ";
        deref z == "(2 * 3) + (2 * 4) + "
    }

    @test
    def testForeach32(): Bool = region r {
        let y = ref 0 @ r;
        foreach (x <- 1 :: 2 :: 3 :: Nil; if x > 1) y := deref y + x; // ForEach.foreach(match x -> if (x > 1) y := deref y + x else (), 1 :: 2 :: 3 :: Nil)
        deref y == 5
    }

    @test
    def testForeach33(): Bool = region r {
        let y = ref 0 @ r;
        foreach (x <- 1 :: 2 :: 3 :: Nil;
                 if x > 1)
                    y := deref y + x; // ForEach.foreach(match x -> if (x > 1) y := deref y + x else (), 1 :: 2 :: 3 :: Nil)
        deref y == 5
    }

    @test
    def testForeach34(): Bool = region r {
        let z = ref 0 @ r;
        foreach (x <- 1 :: 2 :: Nil;y <- 3 :: 4 :: Nil)
                    z := deref z + (x * y);
        deref z == (3 * 1) + (4 * 1) + (3 * 2) + (4 * 2)
    }

    @test
    def testForeach35(): Bool = region r {
        let z = ref 0 @ r;
        foreach(  x <- 1 :: 2 :: Nil  ;   if x > 1 ;   y  <-  3 :: 4 :: Nil;if y > 3)z := deref z + (x * y);
        deref z == (3 * 1) + (4 * 1) + (3 * 2) + (4 * 2)
    }

    @test
    def testForeach36(): Bool = region r {
        let z = ref 0 @ r;
        foreach (x <- 1 :: 2 :: Nil;if x > 1; y <- 3 :: 4 :: Nil; if y > 3) z := deref z + (x * y);
        deref z == (3 * 1) + (4 * 1) + (3 * 2) + (4 * 2)
    }

    @test
    def testForeach37(): Bool = region r {
        let z = ref 0 @ r;
        foreach (x <- 1 :: 2 :: Nil; if x > 1; y <- 3 :: 4 :: Nil; if y > 3) z := deref z + (x * y);
        deref z == (3 * 1) + (4 * 1) + (3 * 2) + (4 * 2)
    }

}
