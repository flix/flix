namespace Test/Exp/Foreach {

    //
    // Note that some of these tests are poorly formatted on purpose to test parsing.
    //

    @test
    def testForEach01(): Bool = region r {
        let y = ref 0 @ r;
        foreach (x <- List.iterator(r, 1 :: Nil)) y := x + 1; // Iterator.foreach(match x -> y := x + 1, Iterable.iterator(List.iterator(r, 1 :: Nil)))
        deref y == 2
    }

    @test
    def testForEach02(): Bool = region r {
        let y = ref 0 @ r;
        foreach (x <- List.iterator(r, 1 :: Nil))
            y := x + 1; // Iterator.foreach(match x -> y := x + 1, Iterable.iterator(List.iterator(r, 1 :: Nil)))
        deref y == 2
    }

    @test
    def testForEach03(): Bool = region r {
        let z = ref 0 @ r;
        foreach ((x, y) <- List.iterator(r, (1, 2) :: (3, 4) :: Nil)) z := deref z + x + y; // Iterator.foreach(match (x, y) -> z := deref z + x + y, Iterable.iterator(List.iterator(r, (1, 2) :: (3, 4) :: Nil)))
        deref z == 10
    }

    @test
    def testForEach04(): Bool = region r {
        let z = ref 0 @ r;
        foreach ((x, y) <- List.iterator(r, (1, 2) :: (3, 4) :: Nil))
            z := deref z + x + y; // Iterator.foreach(match (x, y) -> z := deref z + x + y, Iterable.iterator(List.iterator(r, (1, 2) :: (3, 4) :: Nil)))
        deref z == 10
    }

    @test
    def testForEach05(): Bool = region r {
        let z = ref 0 @ r;
        let l = (1, 2) :: (3, 4) :: Nil;
        foreach ((x, y) <- List.iterator(r, l))
            z := deref z + x + y; // Iterator.foreach(match (x, y) -> z := deref z + x + y, Iterable.iterator(List.iterator(r, l)))
        deref z == 10
    }

    @test
    def testForEach06(): Bool = region r {
        let z = ref 0 @ r;
        let l = (1, 2) :: (3, 4) :: Nil;
        foreach ((x, _) <- List.iterator(r, l))
            z := deref z + x; // Iterator.foreach(match (x, _) -> z := deref z + x, Iterable.iterator(List.iterator(r, l)))
        deref z == 4
    }

    @test
    def testForEach07(): Bool = region r {
        let z = ref 0 @ r;
        let q = ref 1 @ r;
        let l = (1, 2) :: (3, 4) :: Nil;
        foreach ((x, y) <- List.iterator(r, l)) {
            q := deref q + x;
            z := deref z + x + y
        };
        deref z + deref q == 15
    }

    @test
    def testForEach08(): Bool = region r {
        let z = ref 0 @ r;
        foreach (x <- List.iterator(r, 1 :: 2 :: Nil))
            foreach(y <- List.iterator(r, 3 :: 4 :: Nil))
                z := deref z + x + y;
        deref z == (3 + 1) + (4 + 1) + (3 + 2) + (4 + 2)
    }

    @test
    def testForEach09(): Bool = region r {
        let z = ref 0 @ r;
        foreach (x <- List.iterator(r, 1 :: 2 :: Nil))
            foreach(y <- List.iterator(r, 3 :: 4 :: Nil))
                foreach(_ <- List.iterator(r, List.range(0, 10)))
                    z := deref z + (x * y);
        deref z == (3 * 1) * 10 + (4 * 1) * 10 + (3 * 2) * 10 + (4 * 2) * 10
    }

    @test
    def testForEach10(): Bool = region r {
        let z = ref 0 @ r;
        foreach (x <- List.iterator(r, 1 :: 2 :: Nil)) {
            foreach(y <- List.iterator(r, 3 :: 4 :: Nil))
                foreach(_ <- List.iterator(r, List.range(0, 10)))
                    z := deref z + (x * y)
        };
        deref z == (3 * 1) * 10 + (4 * 1) * 10 + (3 * 2) * 10 + (4 * 2) * 10
    }

    @test
    def testForEach11(): Bool = region r {
        let z = ref 0 @ r;
        foreach (x <- List.iterator(r, 1 :: 2 :: Nil)) {
            foreach(y <- List.iterator(r, 3 :: 4 :: Nil)) {
                foreach(_ <- List.iterator(r, List.range(0, 10)))
                    z := deref z + (x * y)
            }
        };
        deref z == (3 * 1) * 10 + (4 * 1) * 10 + (3 * 2) * 10 + (4 * 2) * 10
    }

    @test
    def testForEach12(): Bool = region r {
        let z = ref 0 @ r;
        foreach (x <- List.iterator(r, 1 :: 2 :: Nil)) {
            foreach(y <- List.iterator(r, 3 :: 4 :: Nil)) {
                foreach(_ <- List.iterator(r, List.range(0, 10))) {
                    z := deref z + (x * y)
                }
            }
        };
        deref z == (3 * 1) * 10 + (4 * 1) * 10 + (3 * 2) * 10 + (4 * 2) * 10
    }

    @test
    def testForEach13(): Bool = region r {
        let z = ref 0 @ r;
        foreach (x <- List.iterator(r, 1 :: 2 :: Nil)) {
            foreach(y <- List.iterator(r, 3 :: 4 :: Nil)) {
                foreach(_ <- List.iterator(r, List.range(0, 10))) z := deref z + (x * y)
            }
        };
        deref z == (3 * 1) * 10 + (4 * 1) * 10 + (3 * 2) * 10 + (4 * 2) * 10
    }

    @test
    def testForEach14(): Bool = region r {
        let z = ref 0 @ r;
        foreach (x <- List.iterator(r, 1 :: 2 :: Nil)) {
            foreach(y <- List.iterator(r, 3 :: 4 :: Nil))
                foreach(_ <- List.iterator(r, List.range(0, 10))) z := deref z + (x * y)
        };
        deref z == (3 * 1) * 10 + (4 * 1) * 10 + (3 * 2) * 10 + (4 * 2) * 10
    }

    @test
    def testForEach15(): Bool = region r {
        let z = ref 0 @ r;
        foreach (x <- List.iterator(r, 1 :: 2 :: Nil)) foreach(y <- List.iterator(r, 3 :: 4 :: Nil)) foreach(_ <- List.iterator(r, List.range(0, 10))) z := deref z + (x * y);
        deref z == (3 * 1) * 10 + (4 * 1) * 10 + (3 * 2) * 10 + (4 * 2) * 10
    }

    @test
    def testForEach16(): Bool = region r {
        let z = ref "" @ r;
        foreach (x <- List.iterator(r, "1" :: "2" :: Nil)) foreach(y <- List.iterator(r, "3" :: "4" :: Nil)) z := deref z + "(${x} * ${y}) + ";
        deref z == "(1 * 3) + (1 * 4) + (2 * 3) + (2 * 4) + "
    }

    @test
    def testForEach17(): Bool = region r {
        let y = ref 0 @ r;
        foreach(   x     <- List.iterator(r,      1 :: Nil)   )y := x + 1; // Iterator.foreach(match x -> y := x + 1, Iterable.iterator(List.iterator(r, 1 :: Nil)))
        deref y == 2
    }

    @test
    def testForEach18(): Bool = region r {
        let z = ref 0 @ r;
        foreach (x <- List.iterator(r, 1 :: 2 :: Nil);y <- List.iterator(r, 3 :: 4 :: Nil))
                    z := deref z + (x * y);
        deref z == (3 * 1) + (4 * 1) + (3 * 2) + (4 * 2)
    }

    @test
    def testForEach19(): Bool = region r {
        let z = ref 0 @ r;
        foreach(  x <- List.iterator(r, 1 :: 2 :: Nil)  ;   y  <- List.iterator(r,  3 :: 4 :: Nil) )z := deref z + (x * y);
        deref z == (3 * 1) + (4 * 1) + (3 * 2) + (4 * 2)
    }

    @test
    def testForEach20(): Bool = region r {
        let z = ref 0 @ r;
        foreach (x <- List.iterator(r, 1 :: 2 :: Nil);
                 y <- List.iterator(r, 3 :: 4 :: Nil))
                    z := deref z + (x * y);
        deref z == (3 * 1) + (4 * 1) + (3 * 2) + (4 * 2)
    }

    @test
    def testForEach21(): Bool = region r {
        let z = ref 0 @ r;
        foreach (x <- List.iterator(r, 1 :: 2 :: Nil);
                 y <- List.iterator(r, 3 :: 4 :: Nil);
                 _ <- List.iterator(r, List.range(0, 10))) {
                    z := deref z + (x * y)
        };
        deref z == (3 * 1) * 10 + (4 * 1) * 10 + (3 * 2) * 10 + (4 * 2) * 10
    }

    @test
    def testForEach22(): Bool = region r {
        let z = ref "" @ r;
        foreach (x <- List.iterator(r, "1" :: "2" :: Nil); y <- List.iterator(r, "3" :: "4" :: Nil)) z := deref z + "(${x} * ${y}) + ";
        deref z == "(1 * 3) + (1 * 4) + (2 * 3) + (2 * 4) + "
    }

    @test
    def testForEach23(): Bool = region r {
        let y = ref 0 @ r;
        foreach (x <- List.iterator(r, 1 :: 2 :: 3 :: Nil); if x > 1) y := deref y + x; // Iterator.foreach(match x -> if (x > 1) y := deref y + x else (), Iterable.iterator(List.iterator(r, 1 :: 2 :: 3 :: Nil)))
        deref y == 5
    }

    @test
    def testForEach24(): Bool = region r {
        let y = ref 0 @ r;
        foreach (x <- List.iterator(r, 1 :: 2 :: 3 :: Nil);
                 if x > 1)
                    y := deref y + x; // Iterator.foreach(match x -> if (x > 1) y := deref y + x else (), Iterable.iterator(List.iterator(r, 1 :: 2 :: 3 :: Nil)))
        deref y == 5
    }

    @test
    def testForEach25(): Bool = region r {
        let y = ref 0 @ r;
        foreach (x <- List.iterator(r, 1 :: 2 :: 3 :: Nil);
                 if x > 1) {
                    y := deref y + x // Iterator.foreach(match x -> if (x > 1) y := deref y + x else (), Iterable.iterator(List.iterator(r, 1 :: 2 :: 3 :: Nil))))
        };
        deref y == 5
    }

    @test
    def testForEach26(): Bool = region r {
        let y = ref 0 @ r;
        foreach (x <- List.iterator(r, 1 :: 2 :: 3 :: Nil); if x > 1) {
            y := deref y + x // Iterator.foreach(match x -> if (x > 1) y := deref y + x else (), Iterable.iterator(List.iterator(r, 1 :: 2 :: 3 :: Nil)))
        };
        deref y == 5
    }

    @test
    def testForEach27(): Bool = region r {
        let z = ref 0 @ r;
        foreach (x <- List.iterator(r, 1 :: 2 :: Nil);y <- List.iterator(r, 3 :: 4 :: Nil); if x > 1)
                    z := deref z + (x * y);
        deref z == (3 * 2) + (4 * 2)
    }

    @test
    def testForEach28(): Bool = region r {
        let z = ref 0 @ r;
        foreach(  x <- List.iterator(r, 1 :: 2 :: Nil)  ;   y  <- List.iterator(r,  3 :: 4 :: Nil);  if x > 1 and y > 3)z := deref z + (x * y);
        deref z == 4 * 2
    }

    @test
    def testForEach29(): Bool = region r {
        let z = ref 0 @ r;
        foreach (x <- List.iterator(r, 1 :: 2 :: Nil);
                 y <- List.iterator(r, 3 :: 4 :: Nil); if x > 1 and y > 3)
                    z := deref z + (x * y);
        deref z == 4 * 2
    }

    @test
    def testForEach30(): Bool = region r {
        let z = ref 0 @ r;
        foreach (x <- List.iterator(r, 1 :: 2 :: Nil);
                 y <- List.iterator(r, 3 :: 4 :: Nil);
                 _ <- List.iterator(r, List.range(0, 10)); if x > 1 and y > 4) {
                    z := deref z + (x * y)
        };
        deref z == 0
    }

    @test
    def testForEach31(): Bool = region r {
        let z = ref "" @ r;
        foreach (x <- List.iterator(r, "1" :: "2" :: Nil); y <- List.iterator(r, "3" :: "4" :: Nil); if x != "1") z := deref z + "(${x} * ${y}) + ";
        deref z == "(2 * 3) + (2 * 4) + "
    }

    @test
    def testForEach32(): Bool = region r {
        let y = ref 0 @ r;
        foreach (x <- List.iterator(r, 1 :: 2 :: 3 :: Nil); if x > 1) y := deref y + x; // Iterator.foreach(match x -> if (x > 1) y := deref y + x else (), Iterable.iterator(List.iterator(r, 1 :: 2 :: 3 :: Nil)))
        deref y == 5
    }

    @test
    def testForEach33(): Bool = region r {
        let y = ref 0 @ r;
        foreach (x <- List.iterator(r, 1 :: 2 :: 3 :: Nil);
                 if x > 1)
                    y := deref y + x; // Iterator.foreach(match x -> if (x > 1) y := deref y + x else (), Iterable.iterator(List.iterator(r, 1 :: 2 :: 3 :: Nil)))
        deref y == 5
    }

    @test
    def testForEach35(): Bool = region r {
        let z = ref 0 @ r;
        foreach(  x <- List.iterator(r, 1 :: 2 :: Nil)  ;   if x > 1 ;   y  <- List.iterator(r,  3 :: 4 :: Nil);if y > 3)z := deref z + (x * y);
        // Iterator.foreach(match x -> if (x > 1) Iterator.foreach(match y -> if (y > 3) z := deref z + (x * y) else (), Iterable.iterator(List.iterator(r, 3 :: 4 :: Nil))) else (), Iterable.iterator(List.iterator(r, 1 :: 2 :: Nil)))
        deref z == 4 * 2
    }

    @test
    def testForEach36(): Bool = region r {
        let z = ref 0 @ r;
        foreach (x <- List.iterator(r, 1 :: 2 :: Nil);if x > 1; y <- List.iterator(r, 3 :: 4 :: Nil); if y > 3) z := deref z + (x * y);
        deref z == 4 * 2
    }

    @test
    def testForEach37(): Bool = region r {
        let z = ref 0 @ r;
        foreach (x <- List.iterator(r, 1 :: 2 :: Nil); if x > 1; y <- List.iterator(r, 3 :: 4 :: Nil); if y > 3) z := deref z + (x * y);
        deref z == 4 * 2
    }

    @test
    def testForEach38(): Bool = region r {
        let z = ref 0 @ r;
        foreach (x <- List.iterator(r, 1 :: 2 :: Nil); if x > 1; y <- List.iterator(r, 3 :: 4 :: Nil)) z := deref z + (x * y);
        deref z == (3 * 2) + (4 * 2)
    }

    @test
    def testForEach39(): Bool = region r {
        let z = ref 0 @ r;
        foreach (x <- List.iterator(r, 1 :: 2 :: Nil); y <- List.iterator(r, 3 :: 4 :: Nil); if y > 3; if x > 1) z := deref z + (x * y);
        deref z == 4 * 2
    }

    @test
    def testForEach40(): Bool = region r {
        let y = ref 0 @ r;
        foreach (x <- MutList.range(r, 1, 2)) y := x + 1; // Iterator.foreach(match x -> y := x + 1, Iterable.iterator(MutList.range(r, 1, 2)))
        deref y == 2
    }

    @test
    def testForEach41(): Bool = region r {
        let l = new MutList(r);
        foreach (i <- Iterator.range(r, 0, 10)) {
            MutList.push!(i, l)
        };
        let y = ref 0 @ r;
        foreach (x <- l)
            y := deref y + x; // Iterator.foreach(match x -> y := x + 1, Iterable.iterator(l))
        deref y == (9 + 1) * 9 / 2
    }

    @test
    def testForEach42(): Bool = region r {
        let l = new MutList(r);
        foreach (i <- Iterator.range(r, 0, 10)) {
            MutList.push!(i, l)
        };
        let y = ref 0 @ r;
        foreach (x <- l;
                 if x < 5)
            y := deref y + x; // Iterator.foreach(match x -> if (x < 5) y := x + 1 else (), Iterable.iterator(l))
        deref y == (4 + 1) * 4 / 2
    }

    @test
    def testForEach43(): Bool = region r {
        let z = ref 0 @ r;
        foreach ((x, y) <- List.toMutDeque(r, (1, 2) :: (3, 4) :: Nil)) z := deref z + x + y;
        deref z == 10
    }

}
