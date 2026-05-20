mod Test.Exp.Foreach {

    use Assert.assertEq

    @Test
    def testForeach01(): Unit \ Assert = region rc {
        let y = Ref.fresh(rc, 0);
        foreach (x <- 1 :: Nil) Ref.put(x + 1, y); // ForEach.forEach(match x -> Ref.put(x + 1, y), 1 :: Nil)
        assertEq(expected = 2, Ref.get(y))
    }

    @Test
    def testForeach02(): Unit \ Assert = region rc {
        let y = Ref.fresh(rc, 0);
        foreach (x <- 1 :: Nil)
            Ref.put(x + 1, y); // ForEach.forEach(match x -> Ref.put(x + 1, y), 1 :: Nil)
        assertEq(expected = 2, Ref.get(y))
    }

    @Test
    def testForeach03(): Unit \ Assert = region rc {
        let z = Ref.fresh(rc, 0);
        foreach ((x, y) <- (1, 2) :: (3, 4) :: Nil) Ref.put(Ref.get(z) + x + y, z); // ForEach.forEach(match (x, y) -> Ref.put(Ref.get(z) + x + y, z), (1, 2) :: (3, 4) :: Nil)
        assertEq(expected = 10, Ref.get(z))
    }

    @Test
    def testForeach04(): Unit \ Assert = region rc {
        let z = Ref.fresh(rc, 0);
        foreach ((x, y) <- (1, 2) :: (3, 4) :: Nil)
            Ref.put(Ref.get(z) + x + y, z); // ForEach.forEach(match (x, y) -> Ref.put(Ref.get(z) + x + y, z), (1, 2) :: (3, 4) :: Nil)
        assertEq(expected = 10, Ref.get(z))
    }

    @Test
    def testForeach05(): Unit \ Assert = region rc {
        let z = Ref.fresh(rc, 0);
        let l = (1, 2) :: (3, 4) :: Nil;
        foreach ((x, y) <- l)
            Ref.put(Ref.get(z) + x + y, z); // ForEach.forEach(match (x, y) -> Ref.put(Ref.get(z) + x + y, z), l)
        assertEq(expected = 10, Ref.get(z))
    }

    @Test
    def testForeach06(): Unit \ Assert = region rc {
        let z = Ref.fresh(rc, 0);
        let l = (1, 2) :: (3, 4) :: Nil;
        foreach ((x, _) <- l)
            Ref.put(Ref.get(z) + x, z); // ForEach.forEach(match (x, _) -> Ref.put(Ref.get(z) + x, z), l)
        assertEq(expected = 4, Ref.get(z))
    }

    @Test
    def testForeach07(): Unit \ Assert = region rc {
        let z = Ref.fresh(rc, 0);
        let q = Ref.fresh(rc, 1);
        let l = (1, 2) :: (3, 4) :: Nil;
        foreach ((x, y) <- l) {
            Ref.put(Ref.get(q) + x, q);
            Ref.put(Ref.get(z) + x + y, z)
        };
        assertEq(expected = 15, Ref.get(z) + Ref.get(q))
    }

    @Test
    def testForeach08(): Unit \ Assert = region rc {
        let z = Ref.fresh(rc, 0);
        foreach (x <- 1 :: 2 :: Nil)
            foreach(y <- 3 :: 4 :: Nil)
                Ref.put(Ref.get(z) + x + y, z);
        assertEq(expected = (3 + 1) + (4 + 1) + (3 + 2) + (4 + 2), Ref.get(z))
    }

    @Test
    def testForeach09(): Unit \ Assert = region rc {
        let z = Ref.fresh(rc, 0);
        foreach (x <- 1 :: 2 :: Nil)
            foreach(y <- 3 :: 4 :: Nil)
                foreach(_ <- List.range(0, 10))
                    Ref.put(Ref.get(z) + (x * y), z);
        assertEq(expected = (3 * 1) * 10 + (4 * 1) * 10 + (3 * 2) * 10 + (4 * 2) * 10, Ref.get(z))
    }

    @Test
    def testForeach10(): Unit \ Assert = region rc {
        let z = Ref.fresh(rc, 0);
        foreach (x <- 1 :: 2 :: Nil) {
            foreach(y <- 3 :: 4 :: Nil)
                foreach(_ <- List.range(0, 10))
                    Ref.put(Ref.get(z) + (x * y), z)
        };
        assertEq(expected = (3 * 1) * 10 + (4 * 1) * 10 + (3 * 2) * 10 + (4 * 2) * 10, Ref.get(z))
    }

    @Test
    def testForeach11(): Unit \ Assert = region rc {
        let z = Ref.fresh(rc, 0);
        foreach (x <- 1 :: 2 :: Nil) {
            foreach(y <- 3 :: 4 :: Nil) {
                foreach(_ <- List.range(0, 10))
                    Ref.put(Ref.get(z) + (x * y), z)
            }
        };
        assertEq(expected = (3 * 1) * 10 + (4 * 1) * 10 + (3 * 2) * 10 + (4 * 2) * 10, Ref.get(z))
    }

    @Test
    def testForeach12(): Unit \ Assert = region rc {
        let z = Ref.fresh(rc, 0);
        foreach (x <- 1 :: 2 :: Nil) {
            foreach(y <- 3 :: 4 :: Nil) {
                foreach(_ <- List.range(0, 10)) {
                    Ref.put(Ref.get(z) + (x * y), z)
                }
            }
        };
        assertEq(expected = (3 * 1) * 10 + (4 * 1) * 10 + (3 * 2) * 10 + (4 * 2) * 10, Ref.get(z))
    }

    @Test
    def testForeach13(): Unit \ Assert = region rc {
        let z = Ref.fresh(rc, 0);
        foreach (x <- 1 :: 2 :: Nil) {
            foreach(y <- 3 :: 4 :: Nil) {
                foreach(_ <- List.range(0, 10)) Ref.put(Ref.get(z) + (x * y), z)
            }
        };
        assertEq(expected = (3 * 1) * 10 + (4 * 1) * 10 + (3 * 2) * 10 + (4 * 2) * 10, Ref.get(z))
    }

    @Test
    def testForeach14(): Unit \ Assert = region rc {
        let z = Ref.fresh(rc, 0);
        foreach (x <- 1 :: 2 :: Nil) {
            foreach(y <- 3 :: 4 :: Nil)
                foreach(_ <- List.range(0, 10)) Ref.put(Ref.get(z) + (x * y), z)
        };
        assertEq(expected = (3 * 1) * 10 + (4 * 1) * 10 + (3 * 2) * 10 + (4 * 2) * 10, Ref.get(z))
    }

    @Test
    def testForeach15(): Unit \ Assert = region rc {
        let z = Ref.fresh(rc, 0);
        foreach (x <- 1 :: 2 :: Nil) foreach(y <- 3 :: 4 :: Nil) foreach(_ <- List.range(0, 10)) Ref.put(Ref.get(z) + (x * y), z);
        assertEq(expected = (3 * 1) * 10 + (4 * 1) * 10 + (3 * 2) * 10 + (4 * 2) * 10, Ref.get(z))
    }

    @Test
    def testForeach16(): Unit \ Assert = region rc {
        let z = Ref.fresh(rc, "");
        foreach (x <- "1" :: "2" :: Nil) foreach(y <- "3" :: "4" :: Nil) Ref.put(Ref.get(z) + "(${x} * ${y}) + ", z);
        assertEq(expected = "(1 * 3) + (1 * 4) + (2 * 3) + (2 * 4) + ", Ref.get(z))
    }

    @Test
    def testForeach17(): Unit \ Assert = region rc {
        let y = Ref.fresh(rc, 0);
        foreach(   x     <-      1 :: Nil  )Ref.put(x + 1, y); // ForEach.forEach(match x -> Ref.put(x + 1, y), 1 :: Nil)
        assertEq(expected = 2, Ref.get(y))
    }

    @Test
    def testForeach18(): Unit \ Assert = region rc {
        let z = Ref.fresh(rc, 0);
        foreach (x <- 1 :: 2 :: Nil;y <- 3 :: 4 :: Nil)
                    Ref.put(Ref.get(z) + (x * y), z);
        assertEq(expected = (3 * 1) + (4 * 1) + (3 * 2) + (4 * 2), Ref.get(z))
    }

    @Test
    def testForeach19(): Unit \ Assert = region rc {
        let z = Ref.fresh(rc, 0);
        foreach(  x <- 1 :: 2 :: Nil  ;   y  <- 3 :: 4 :: Nil )Ref.put(Ref.get(z) + (x * y), z);
        assertEq(expected = (3 * 1) + (4 * 1) + (3 * 2) + (4 * 2), Ref.get(z))
    }

    @Test
    def testForeach20(): Unit \ Assert = region rc {
        let z = Ref.fresh(rc, 0);
        foreach (x <- 1 :: 2 :: Nil;
                 y <-  3 :: 4 :: Nil)
                    Ref.put(Ref.get(z) + (x * y), z);
        assertEq(expected = (3 * 1) + (4 * 1) + (3 * 2) + (4 * 2), Ref.get(z))
    }

    @Test
    def testForeach21(): Unit \ Assert = region rc {
        let z = Ref.fresh(rc, 0);
        foreach (x <- 1 :: 2 :: Nil;
                 y <- 3 :: 4 :: Nil;
                 _ <- List.range(0, 10)) {
                    Ref.put(Ref.get(z) + (x * y), z)
        };
        assertEq(expected = (3 * 1) * 10 + (4 * 1) * 10 + (3 * 2) * 10 + (4 * 2) * 10, Ref.get(z))
    }

    @Test
    def testForeach22(): Unit \ Assert = region rc {
        let z = Ref.fresh(rc, "");
        foreach (x <- "1" :: "2" :: Nil; y <- "3" :: "4" :: Nil) Ref.put(Ref.get(z) + "(${x} * ${y}) + ", z);
        assertEq(expected = "(1 * 3) + (1 * 4) + (2 * 3) + (2 * 4) + ", Ref.get(z))
    }

    @Test
    def testForeach23(): Unit \ Assert = region rc {
        let y = Ref.fresh(rc, 0);
        foreach (x <- 1 :: 2 :: 3 :: Nil; if x > 1) Ref.put(Ref.get(y) + x, y); // ForEach.forEach(match x -> if (x > 1) Ref.put(Ref.get(y) + x, y) else (), 1 :: 2 :: 3 :: Nil)
        assertEq(expected = 5, Ref.get(y))
    }

    @Test
    def testForeach24(): Unit \ Assert = region rc {
        let y = Ref.fresh(rc, 0);
        foreach (x <- 1 :: 2 :: 3 :: Nil;
                 if x > 1)
                    Ref.put(Ref.get(y) + x, y); // ForEach.forEach(match x -> if (x > 1) Ref.put(Ref.get(y) + x, y) else (), 1 :: 2 :: 3 :: Nil)
        assertEq(expected = 5, Ref.get(y))
    }

    @Test
    def testForeach25(): Unit \ Assert = region rc {
        let y = Ref.fresh(rc, 0);
        foreach (x <- 1 :: 2 :: 3 :: Nil;
                 if x > 1) {
                    Ref.put(Ref.get(y) + x, y) // ForEach.forEach(match x -> if (x > 1) Ref.put(Ref.get(y) + x, y) else (), 1 :: 2 :: 3 :: Nil))
        };
        assertEq(expected = 5, Ref.get(y))
    }

    @Test
    def testForeach26(): Unit \ Assert = region rc {
        let y = Ref.fresh(rc, 0);
        foreach (x <- 1 :: 2 :: 3 :: Nil; if x > 1) {
            Ref.put(Ref.get(y) + x, y) // ForEach.forEach(match x -> if (x > 1) Ref.put(Ref.get(y) + x, y) else (), 1 :: 2 :: 3 :: Nil)
        };
        assertEq(expected = 5, Ref.get(y))
    }

    @Test
    def testForeach27(): Unit \ Assert = region rc {
        let z = Ref.fresh(rc, 0);
        foreach (x <- 1 :: 2 :: Nil;y <- 3 :: 4 :: Nil; if x > 1)
                    Ref.put(Ref.get(z) + (x * y), z);
        assertEq(expected = (3 * 2) + (4 * 2), Ref.get(z))
    }

    @Test
    def testForeach28(): Unit \ Assert = region rc {
        let z = Ref.fresh(rc, 0);
        foreach(  x <- 1 :: 2 :: Nil  ;   y  <- 3 :: 4 :: Nil;  if x > 1 and y > 3)Ref.put(Ref.get(z) + (x * y), z);
        assertEq(expected = 4 * 2, Ref.get(z))
    }

    @Test
    def testForeach29(): Unit \ Assert = region rc {
        let z = Ref.fresh(rc, 0);
        foreach (x <- 1 :: 2 :: Nil;
                 y <- 3 :: 4 :: Nil; if x > 1 and y > 3)
                    Ref.put(Ref.get(z) + (x * y), z);
        assertEq(expected = 4 * 2, Ref.get(z))
    }

    @Test
    def testForeach30(): Unit \ Assert = region rc {
        let z = Ref.fresh(rc, 0);
        foreach (x <- 1 :: 2 :: Nil;
                 y <- 3 :: 4 :: Nil;
                 _ <- List.range(0, 10); if x > 1 and y > 4) {
                    Ref.put(Ref.get(z) + (x * y), z)
        };
        assertEq(expected = 0, Ref.get(z))
    }

    @Test
    def testForeach31(): Unit \ Assert = region rc {
        let z = Ref.fresh(rc, "");
        foreach (x <- "1" :: "2" :: Nil; y <- "3" :: "4" :: Nil; if x != "1") Ref.put(Ref.get(z) + "(${x} * ${y}) + ", z);
        assertEq(expected = "(2 * 3) + (2 * 4) + ", Ref.get(z))
    }

    @Test
    def testForeach32(): Unit \ Assert = region rc {
        let y = Ref.fresh(rc, 0);
        foreach (x <- 1 :: 2 :: 3 :: Nil; if x > 1) Ref.put(Ref.get(y) + x, y); // ForEach.forEach(match x -> if (x > 1) Ref.put(Ref.get(y) + x, y) else (), 1 :: 2 :: 3 :: Nil)
        assertEq(expected = 5, Ref.get(y))
    }

    @Test
    def testForeach33(): Unit \ Assert = region rc {
        let y = Ref.fresh(rc, 0);
        foreach (x <- 1 :: 2 :: 3 :: Nil;
                 if x > 1)
                    Ref.put(Ref.get(y) + x, y); // ForEach.forEach(match x -> if (x > 1) Ref.put(Ref.get(y) + x, y) else (), 1 :: 2 :: 3 :: Nil)
        assertEq(expected = 5, Ref.get(y))
    }

    @Test
    def testForeach34(): Unit \ Assert = region rc {
        let z = Ref.fresh(rc, 0);
        foreach(  x <-  1 :: 2 :: Nil  ;   if x > 1 ;   y  <-   3 :: 4 :: Nil;if y > 3)Ref.put(Ref.get(z) + (x * y), z);
        // ForEach.forEach(match x -> if (x > 1) Iterator.foreach(match y -> if (y > 3) Ref.put(Ref.get(z) + (x * y), z) else (), Iterable.iterator(List.iterator(rc, 3 :: 4 :: Nil))) else (), 1 :: 2 :: Nil)
        assertEq(expected = 4 * 2, Ref.get(z))
    }

    @Test
    def testForeach35(): Unit \ Assert = region rc {
        let z = Ref.fresh(rc, 0);
        foreach (x <- 1 :: 2 :: Nil; if x > 1; y <- 3 :: 4 :: Nil; if y > 3) Ref.put(Ref.get(z) + (x * y), z);
        assertEq(expected = 4 * 2, Ref.get(z))
    }

    @Test
    def testForeach36(): Unit \ Assert = region rc {
        let z = Ref.fresh(rc, 0);
        foreach (x <- 1 :: 2 :: Nil; if x > 1; y <- 3 :: 4 :: Nil; if y > 3) Ref.put(Ref.get(z) + (x * y), z);
        assertEq(expected = 4 * 2, Ref.get(z))
    }

    @Test
    def testForeach37(): Unit \ Assert = region rc {
        let z = Ref.fresh(rc, 0);
        foreach (x <- 1 :: 2 :: Nil; if x > 1; y <- 3 :: 4 :: Nil) Ref.put(Ref.get(z) + (x * y), z);
        assertEq(expected = (3 * 2) + (4 * 2), Ref.get(z))
    }

    @Test
    def testForeach38(): Unit \ Assert = region rc {
        let z = Ref.fresh(rc, 0);
        foreach (x <- 1 :: 2 :: Nil; y <- 3 :: 4 :: Nil; if y > 3; if x > 1) Ref.put(Ref.get(z) + (x * y), z);
        assertEq(expected = 4 * 2, Ref.get(z))
    }

    @Test
    def testForeach39(): Unit \ Assert = region rc {
        let y = Ref.fresh(rc, 0);
        foreach (x <- List.range(1, 2)) Ref.put(x + 1, y); // ForEach.forEach(match x -> Ref.put(x + 1, y), 1, 2)
        assertEq(expected = 2, Ref.get(y))
    }

    @Test
    def testForeach40(): Unit \ Assert = region rc {
        let l = (MutList.empty(rc): MutList[Int32, rc]);
        foreach (i <- List.range(0, 10)) {
            MutList.push(i, l)
        };
        let y = Ref.fresh(rc, 0);
        foreach (x <- MutList.toList(l))
            Ref.put(Ref.get(y) + x, y); // ForEach.forEach(match x -> Ref.put(x + 1, y), l)
        assertEq(expected = (9 + 1) * 9 / 2, Ref.get(y))
    }

    @Test
    def testForeach41(): Unit \ Assert = region rc {
        let l = (MutList.empty(rc): MutList[Int32, rc]);
        foreach (i <- List.range(0, 10)) {
            MutList.push(i, l)
        };
        let y = Ref.fresh(rc, 0);
        foreach (x <- MutList.toList(l);
                 if x < 5)
            Ref.put(Ref.get(y) + x, y); // ForEach.forEach(match x -> if (x < 5) Ref.put(x + 1, y) else (), l)
        assertEq(expected = (4 + 1) * 4 / 2, Ref.get(y))
    }

    @Test
    def testForeach42(): Unit \ Assert = region rc {
        let z = Ref.fresh(rc, 0);
        foreach ((x, y) <- (1, 2) :: (3, 4) :: Nil) Ref.put(Ref.get(z) + x + y, z);
        assertEq(expected = 10, Ref.get(z))
    }

}
