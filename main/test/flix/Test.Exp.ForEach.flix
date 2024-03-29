mod Test.Exp.Foreach {

    //
    // Note that some of these tests are poorly formatted on purpose to test parsing.
    //

    @test
    def testForeach01(): Bool = region rc {
        let y = ref 0 @ rc;
        foreach (x <- 1 :: Nil) Ref.put(x + 1, y); // Iterator.foreach(match x -> Ref.put(x + 1, y), Iterable.iterator(List.iterator(rc, 1 :: Nil)))
        deref y == 2
    }

    @test
    def testForeach02(): Bool = region rc {
        let y = ref 0 @ rc;
        foreach (x <- 1 :: Nil)
            Ref.put(x + 1, y); // Iterator.foreach(match x -> Ref.put(x + 1, y), Iterable.iterator(List.iterator(rc, 1 :: Nil)))
        deref y == 2
    }

    @test
    def testForeach03(): Bool = region rc {
        let z = ref 0 @ rc;
        foreach ((x, y) <- (1, 2) :: (3, 4) :: Nil) Ref.put(deref z + x + y, z); // Iterator.foreach(match (x, y) -> Ref.put(deref z + x + y, z), Iterable.iterator(List.iterator(rc, (1, 2) :: (3, 4) :: Nil)))
        deref z == 10
    }

    @test
    def testForeach04(): Bool = region rc {
        let z = ref 0 @ rc;
        foreach ((x, y) <- (1, 2) :: (3, 4) :: Nil)
            Ref.put(deref z + x + y, z); // Iterator.foreach(match (x, y) -> Ref.put(deref z + x + y, z), Iterable.iterator(List.iterator(rc, (1, 2) :: (3, 4) :: Nil)))
        deref z == 10
    }

    @test
    def testForeach05(): Bool = region rc {
        let z = ref 0 @ rc;
        let l = (1, 2) :: (3, 4) :: Nil;
        foreach ((x, y) <- l)
            Ref.put(deref z + x + y, z); // Iterator.foreach(match (x, y) -> Ref.put(deref z + x + y, z), Iterable.iterator(List.iterator(rc, l)))
        deref z == 10
    }

    @test
    def testForeach06(): Bool = region rc {
        let z = ref 0 @ rc;
        let l = (1, 2) :: (3, 4) :: Nil;
        foreach ((x, _) <- l)
            Ref.put(deref z + x, z); // Iterator.foreach(match (x, _) -> Ref.put(deref z + x, z), Iterable.iterator(List.iterator(rc, l)))
        deref z == 4
    }

    @test
    def testForeach07(): Bool = region rc {
        let z = ref 0 @ rc;
        let q = ref 1 @ rc;
        let l = (1, 2) :: (3, 4) :: Nil;
        foreach ((x, y) <- l) {
            Ref.put(deref q + x, q);
            Ref.put(deref z + x + y, z)
        };
        deref z + deref q == 15
    }

    @test
    def testForeach08(): Bool = region rc {
        let z = ref 0 @ rc;
        foreach (x <- 1 :: 2 :: Nil)
            foreach(y <- 3 :: 4 :: Nil)
                Ref.put(deref z + x + y, z);
        deref z == (3 + 1) + (4 + 1) + (3 + 2) + (4 + 2)
    }

    @test
    def testForeach09(): Bool = region rc {
        let z = ref 0 @ rc;
        foreach (x <- 1 :: 2 :: Nil)
            foreach(y <- 3 :: 4 :: Nil)
                foreach(_ <- List.range(0, 10))
                    Ref.put(deref z + (x * y), z);
        deref z == (3 * 1) * 10 + (4 * 1) * 10 + (3 * 2) * 10 + (4 * 2) * 10
    }

    @test
    def testForeach10(): Bool = region rc {
        let z = ref 0 @ rc;
        foreach (x <- 1 :: 2 :: Nil) {
            foreach(y <- 3 :: 4 :: Nil)
                foreach(_ <- List.range(0, 10))
                    Ref.put(deref z + (x * y), z)
        };
        deref z == (3 * 1) * 10 + (4 * 1) * 10 + (3 * 2) * 10 + (4 * 2) * 10
    }

    @test
    def testForeach11(): Bool = region rc {
        let z = ref 0 @ rc;
        foreach (x <- 1 :: 2 :: Nil) {
            foreach(y <- 3 :: 4 :: Nil) {
                foreach(_ <- List.range(0, 10))
                    Ref.put(deref z + (x * y), z)
            }
        };
        deref z == (3 * 1) * 10 + (4 * 1) * 10 + (3 * 2) * 10 + (4 * 2) * 10
    }

    @test
    def testForeach12(): Bool = region rc {
        let z = ref 0 @ rc;
        foreach (x <- 1 :: 2 :: Nil) {
            foreach(y <- 3 :: 4 :: Nil) {
                foreach(_ <- List.range(0, 10)) {
                    Ref.put(deref z + (x * y), z)
                }
            }
        };
        deref z == (3 * 1) * 10 + (4 * 1) * 10 + (3 * 2) * 10 + (4 * 2) * 10
    }

    @test
    def testForeach13(): Bool = region rc {
        let z = ref 0 @ rc;
        foreach (x <- 1 :: 2 :: Nil) {
            foreach(y <- 3 :: 4 :: Nil) {
                foreach(_ <- List.range(0, 10)) Ref.put(deref z + (x * y), z)
            }
        };
        deref z == (3 * 1) * 10 + (4 * 1) * 10 + (3 * 2) * 10 + (4 * 2) * 10
    }

    @test
    def testForeach14(): Bool = region rc {
        let z = ref 0 @ rc;
        foreach (x <- 1 :: 2 :: Nil) {
            foreach(y <- 3 :: 4 :: Nil)
                foreach(_ <- List.range(0, 10)) Ref.put(deref z + (x * y), z)
        };
        deref z == (3 * 1) * 10 + (4 * 1) * 10 + (3 * 2) * 10 + (4 * 2) * 10
    }

    @test
    def testForeach15(): Bool = region rc {
        let z = ref 0 @ rc;
        foreach (x <- 1 :: 2 :: Nil) foreach(y <- 3 :: 4 :: Nil) foreach(_ <- List.range(0, 10)) Ref.put(deref z + (x * y), z);
        deref z == (3 * 1) * 10 + (4 * 1) * 10 + (3 * 2) * 10 + (4 * 2) * 10
    }

    @test
    def testForeach16(): Bool = region rc {
        let z = ref "" @ rc;
        foreach (x <- "1" :: "2" :: Nil) foreach(y <- "3" :: "4" :: Nil) Ref.put(deref z + "(${x} * ${y}) + ", z);
        deref z == "(1 * 3) + (1 * 4) + (2 * 3) + (2 * 4) + "
    }

    @test
    def testForeach17(): Bool = region rc {
        let y = ref 0 @ rc;
        foreach(   x     <-      1 :: Nil  )Ref.put(x + 1, y); // Iterator.foreach(match x -> Ref.put(x + 1, y), Iterable.iterator(List.iterator(rc, 1 :: Nil)))
        deref y == 2
    }

    @test
    def testForeach18(): Bool = region rc {
        let z = ref 0 @ rc;
        foreach (x <- 1 :: 2 :: Nil;y <- 3 :: 4 :: Nil)
                    Ref.put(deref z + (x * y), z);
        deref z == (3 * 1) + (4 * 1) + (3 * 2) + (4 * 2)
    }

    @test
    def testForeach19(): Bool = region rc {
        let z = ref 0 @ rc;
        foreach(  x <- 1 :: 2 :: Nil  ;   y  <- 3 :: 4 :: Nil )Ref.put(deref z + (x * y), z);
        deref z == (3 * 1) + (4 * 1) + (3 * 2) + (4 * 2)
    }

    @test
    def testForeach20(): Bool = region rc {
        let z = ref 0 @ rc;
        foreach (x <- 1 :: 2 :: Nil;
                 y <-  3 :: 4 :: Nil)
                    Ref.put(deref z + (x * y), z);
        deref z == (3 * 1) + (4 * 1) + (3 * 2) + (4 * 2)
    }

    @test
    def testForeach21(): Bool = region rc {
        let z = ref 0 @ rc;
        foreach (x <- 1 :: 2 :: Nil;
                 y <- 3 :: 4 :: Nil;
                 _ <- List.range(0, 10)) {
                    Ref.put(deref z + (x * y), z)
        };
        deref z == (3 * 1) * 10 + (4 * 1) * 10 + (3 * 2) * 10 + (4 * 2) * 10
    }

    @test
    def testForeach22(): Bool = region rc {
        let z = ref "" @ rc;
        foreach (x <- "1" :: "2" :: Nil; y <- "3" :: "4" :: Nil) Ref.put(deref z + "(${x} * ${y}) + ", z);
        deref z == "(1 * 3) + (1 * 4) + (2 * 3) + (2 * 4) + "
    }

    @test
    def testForeach23(): Bool = region rc {
        let y = ref 0 @ rc;
        foreach (x <- 1 :: 2 :: 3 :: Nil; if x > 1) Ref.put(deref y + x, y); // Iterator.foreach(match x -> if (x > 1) Ref.put(deref y + x, y) else (), Iterable.iterator(List.iterator(rc, 1 :: 2 :: 3 :: Nil)))
        deref y == 5
    }

    @test
    def testForeach24(): Bool = region rc {
        let y = ref 0 @ rc;
        foreach (x <- 1 :: 2 :: 3 :: Nil;
                 if x > 1)
                    Ref.put(deref y + x, y); // Iterator.foreach(match x -> if (x > 1) Ref.put(deref y + x, y) else (), Iterable.iterator(List.iterator(rc, 1 :: 2 :: 3 :: Nil)))
        deref y == 5
    }

    @test
    def testForeach25(): Bool = region rc {
        let y = ref 0 @ rc;
        foreach (x <- 1 :: 2 :: 3 :: Nil;
                 if x > 1) {
                    Ref.put(deref y + x, y) // Iterator.foreach(match x -> if (x > 1) Ref.put(deref y + x, y) else (), Iterable.iterator(List.iterator(rc, 1 :: 2 :: 3 :: Nil))))
        };
        deref y == 5
    }

    @test
    def testForeach26(): Bool = region rc {
        let y = ref 0 @ rc;
        foreach (x <- 1 :: 2 :: 3 :: Nil; if x > 1) {
            Ref.put(deref y + x, y) // Iterator.foreach(match x -> if (x > 1) Ref.put(deref y + x, y) else (), Iterable.iterator(List.iterator(rc, 1 :: 2 :: 3 :: Nil)))
        };
        deref y == 5
    }

    @test
    def testForeach27(): Bool = region rc {
        let z = ref 0 @ rc;
        foreach (x <- 1 :: 2 :: Nil;y <- 3 :: 4 :: Nil; if x > 1)
                    Ref.put(deref z + (x * y), z);
        deref z == (3 * 2) + (4 * 2)
    }

    @test
    def testForeach28(): Bool = region rc {
        let z = ref 0 @ rc;
        foreach(  x <- 1 :: 2 :: Nil  ;   y  <- 3 :: 4 :: Nil;  if x > 1 and y > 3)Ref.put(deref z + (x * y), z);
        deref z == 4 * 2
    }

    @test
    def testForeach29(): Bool = region rc {
        let z = ref 0 @ rc;
        foreach (x <- 1 :: 2 :: Nil;
                 y <- 3 :: 4 :: Nil; if x > 1 and y > 3)
                    Ref.put(deref z + (x * y), z);
        deref z == 4 * 2
    }

    @test
    def testForeach30(): Bool = region rc {
        let z = ref 0 @ rc;
        foreach (x <- 1 :: 2 :: Nil;
                 y <- 3 :: 4 :: Nil;
                 _ <- List.range(0, 10); if x > 1 and y > 4) {
                    Ref.put(deref z + (x * y), z)
        };
        deref z == 0
    }

    @test
    def testForeach31(): Bool = region rc {
        let z = ref "" @ rc;
        foreach (x <- "1" :: "2" :: Nil; y <- "3" :: "4" :: Nil; if x != "1") Ref.put(deref z + "(${x} * ${y}) + ", z);
        deref z == "(2 * 3) + (2 * 4) + "
    }

    @test
    def testForeach32(): Bool = region rc {
        let y = ref 0 @ rc;
        foreach (x <- 1 :: 2 :: 3 :: Nil; if x > 1) Ref.put(deref y + x, y); // Iterator.foreach(match x -> if (x > 1) Ref.put(deref y + x, y) else (), Iterable.iterator(List.iterator(rc, 1 :: 2 :: 3 :: Nil)))
        deref y == 5
    }

    @test
    def testForeach33(): Bool = region rc {
        let y = ref 0 @ rc;
        foreach (x <- 1 :: 2 :: 3 :: Nil;
                 if x > 1)
                    Ref.put(deref y + x, y); // Iterator.foreach(match x -> if (x > 1) Ref.put(deref y + x, y) else (), Iterable.iterator(List.iterator(rc, 1 :: 2 :: 3 :: Nil)))
        deref y == 5
    }

    @test
    def testForeach35(): Bool = region rc {
        let z = ref 0 @ rc;
        foreach(  x <-  1 :: 2 :: Nil  ;   if x > 1 ;   y  <-   3 :: 4 :: Nil;if y > 3)Ref.put(deref z + (x * y), z);
        // Iterator.foreach(match x -> if (x > 1) Iterator.foreach(match y -> if (y > 3) Ref.put(deref z + (x * y), z) else (), Iterable.iterator(List.iterator(rc, 3 :: 4 :: Nil))) else (), Iterable.iterator(List.iterator(rc, 1 :: 2 :: Nil)))
        deref z == 4 * 2
    }

    @test
    def testForeach36(): Bool = region rc {
        let z = ref 0 @ rc;
        foreach (x <- 1 :: 2 :: Nil; if x > 1; y <- 3 :: 4 :: Nil; if y > 3) Ref.put(deref z + (x * y), z);
        deref z == 4 * 2
    }

    @test
    def testForeach37(): Bool = region rc {
        let z = ref 0 @ rc;
        foreach (x <- 1 :: 2 :: Nil; if x > 1; y <- 3 :: 4 :: Nil; if y > 3) Ref.put(deref z + (x * y), z);
        deref z == 4 * 2
    }

    @test
    def testForeach38(): Bool = region rc {
        let z = ref 0 @ rc;
        foreach (x <- 1 :: 2 :: Nil; if x > 1; y <- 3 :: 4 :: Nil) Ref.put(deref z + (x * y), z);
        deref z == (3 * 2) + (4 * 2)
    }

    @test
    def testForeach39(): Bool = region rc {
        let z = ref 0 @ rc;
        foreach (x <- 1 :: 2 :: Nil; y <- 3 :: 4 :: Nil; if y > 3; if x > 1) Ref.put(deref z + (x * y), z);
        deref z == 4 * 2
    }

    @test
    def testForeach40(): Bool = region rc {
        let y = ref 0 @ rc;
        foreach (x <- List.range(1, 2)) Ref.put(x + 1, y); // Iterator.foreach(match x -> Ref.put(x + 1, y), Iterable.iterator(MutList.range(rc, 1, 2)))
        deref y == 2
    }

    @test
    def testForeach41(): Bool = region rc {
        let l = MutList.empty(rc);
        foreach (i <- List.range(0, 10)) {
            MutList.push!(i, l)
        };
        let y = ref 0 @ rc;
        foreach (x <- MutList.toList(l))
            Ref.put(deref y + x, y); // Iterator.foreach(match x -> Ref.put(x + 1, y), Iterable.iterator(l))
        deref y == (9 + 1) * 9 / 2
    }

    @test
    def testForeach42(): Bool = region rc {
        let l = MutList.empty(rc);
        foreach (i <- List.range(0, 10)) {
            MutList.push!(i, l)
        };
        let y = ref 0 @ rc;
        foreach (x <- MutList.toList(l);
                 if x < 5)
            Ref.put(deref y + x, y); // Iterator.foreach(match x -> if (x < 5) Ref.put(x + 1, y) else (), Iterable.iterator(l))
        deref y == (4 + 1) * 4 / 2
    }

    @test
    def testForeach43(): Bool = region rc {
        let z = ref 0 @ rc;
        foreach ((x, y) <- (1, 2) :: (3, 4) :: Nil) Ref.put(deref z + x + y, z);
        deref z == 10
    }

}
