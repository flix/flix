mod Test.Eff.SubEffecting {

    use Assert.assertEq

    ///
    /// Sub-effecting lets a lambda or local def have a larger effect than its body.
    ///
    /// The tests are grouped as follows:
    ///
    ///   - Lambdas and local defs whose body is pure are accepted where an effectful function is expected.
    ///   - A region may absorb the effect that its enclosing def declares.
    ///   - Many lambdas inside one region.
    ///   - A lambda bound outside a region and used inside it.
    ///   - `foreach` loops, which desugar to lambdas, inside one region.
    ///

    def requiresPure(f: Int32 -> Int32): Int32 = f(1)

    def requiresPure2(f: (Int32, Int32) -> Int32): Int32 = f(1, 2)

    def requiresIO(f: Int32 -> Int32 \ IO): Int32 \ IO = f(1)

    def requiresIO2(f: (Int32, Int32) -> Int32 \ IO): Int32 \ IO = f(1, 2)

    /////////////////////////////////////////////////////////////////////////////
    // Lambdas and local defs                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testLambda01(): Unit \ Assert + IO =
        assertEq(expected = 2, requiresIO(x -> x + 1))

    @Test
    def testLambda02(): Unit \ Assert + IO =
        assertEq(expected = 3, requiresIO2((x, y) -> x + y))

    @Test
    def testLambda03(): Unit \ Assert + IO =
        let f = x -> x + 1;
        assertEq(expected = 2, requiresIO(f))

    @Test
    def testLambda04(): Unit \ Assert + IO =
        let f = (x, y) -> x + y;
        assertEq(expected = 3, requiresIO2(f))

    @Test
    def testLambda05(): Unit \ Assert + IO =
        let f = x -> x;
        assertEq(expected = 1, requiresIO(f))

    @Test
    def testLocalDef01(): Unit \ Assert + IO =
        def inc(x) = x + 1;
        assertEq(expected = 2, requiresIO(inc))

    @Test
    def testLocalDef02(): Unit \ Assert + IO =
        def add(x, y) = x + y;
        assertEq(expected = 3, requiresIO2(add))

    @Test
    def testLocalDef03(): Unit \ Assert + IO =
        def inc(x) = x + 1;
        assertEq(expected = 2, requiresIO(x -> inc(x)))

    /////////////////////////////////////////////////////////////////////////////
    // A region absorbs the effect of its enclosing def                        //
    /////////////////////////////////////////////////////////////////////////////

    def regionAbsorbsIO01(): Int32 \ IO = region rc {
        let r = Ref.fresh(rc, 0);
        List.forEach(x -> Ref.put(x, r), 1 :: 2 :: Nil);
        Ref.get(r)
    }

    def regionAbsorbsIO02(): Int32 \ IO = region rc {
        let r = Ref.fresh(rc, 0);
        foreach (x <- 1 :: 2 :: Nil) Ref.put(x, r);
        Ref.get(r)
    }

    @Test
    def testRegionAbsorbsIO01(): Unit \ Assert + IO =
        assertEq(expected = 2, regionAbsorbsIO01())

    @Test
    def testRegionAbsorbsIO02(): Unit \ Assert + IO =
        assertEq(expected = 2, regionAbsorbsIO02())

    /////////////////////////////////////////////////////////////////////////////
    // Many lambdas inside one region                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testManyLambdasInRegion04(): Unit \ Assert = region rc {
        let l = 1 :: 2 :: 3 :: Nil;
        let r = Ref.fresh(rc, 0);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        assertEq(expected = 24, Ref.get(r))
    }

    @Test
    def testManyLambdasInRegion08(): Unit \ Assert = region rc {
        let l = 1 :: 2 :: 3 :: Nil;
        let r = Ref.fresh(rc, 0);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        assertEq(expected = 48, Ref.get(r))
    }

    @Test
    def testManyLambdasInRegion11(): Unit \ Assert = region rc {
        let l = 1 :: 2 :: 3 :: Nil;
        let r = Ref.fresh(rc, 0);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        assertEq(expected = 66, Ref.get(r))
    }

    @Test
    def testManyLambdasInRegion12(): Unit \ Assert = region rc {
        let l = 1 :: 2 :: 3 :: Nil;
        let r = Ref.fresh(rc, 0);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        assertEq(expected = 72, Ref.get(r))
    }

    @Test
    def testManyLambdasInRegion16(): Unit \ Assert = region rc {
        let l = 1 :: 2 :: 3 :: Nil;
        let r = Ref.fresh(rc, 0);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        assertEq(expected = 96, Ref.get(r))
    }

    @Test
    def testManyLambdasInRegion20(): Unit \ Assert = region rc {
        let l = 1 :: 2 :: 3 :: Nil;
        let r = Ref.fresh(rc, 0);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        assertEq(expected = 120, Ref.get(r))
    }

    /////////////////////////////////////////////////////////////////////////////
    // A lambda bound outside a region and used inside it                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testOuterLambdaInRegion01(): Unit \ Assert =
        // The body is an application, so the effect of the lambda is solved before the region is.
        let f = (x: Int32) -> Int32.abs(x);
        region rc {
            let r = Ref.fresh(rc, 0);
            Ref.put(requiresPure(f), r);
            assertEq(expected = 1, Ref.get(r))
        }

    @Test
    def testOuterLambdaInRegion02(): Unit \ Assert =
        // The ascription fixes the effect of the lambda.
        let f: Int32 -> Int32 = x -> x;
        region rc {
            let r = Ref.fresh(rc, 0);
            Ref.put(requiresPure(f), r);
            assertEq(expected = 1, Ref.get(r))
        }

    @Test
    def testOuterLambdaInRegion03(): Unit \ Assert =
        // The body is syntactically pure and nothing outside the region constrains the effect of the lambda.
        let f = (x: Int32) -> x;
        region rc {
            let r = Ref.fresh(rc, 0);
            Ref.put(requiresPure(f), r);
            assertEq(expected = 1, Ref.get(r))
        }

    @Test
    def testOuterLambdaInRegion04(): Unit \ Assert =
        // The outer layer of a curried lambda has a lambda as its body, which is syntactically pure.
        let f = (x: Int32, y: Int32) -> x + y;
        region rc {
            let r = Ref.fresh(rc, 0);
            Ref.put(requiresPure2(f), r);
            assertEq(expected = 3, Ref.get(r))
        }

    @Test
    def testOuterLambdaInRegion05(): Unit \ Assert =
        // The use of the outer lambda is itself inside a lambda inside the region.
        let f = (x: Int32, y: Int32) -> x + y;
        region rc {
            let r = Ref.fresh(rc, 0);
            List.forEach(_ -> Ref.put(Ref.get(r) + requiresPure2(f), r), 1 :: 2 :: Nil);
            assertEq(expected = 6, Ref.get(r))
        }

    @Test
    def testOuterLambdaInRegion06(): Unit \ Assert =
        // The outer lambda is called inside the region, so its effect is part of the effect of the region.
        let w = (y: Int32) -> y + 1;
        region rc {
            let r = Ref.fresh(rc, 0);
            List.forEach(x -> Ref.put(Ref.get(r) + x, r), List.range(0, w(2)));
            assertEq(expected = 3, Ref.get(r))
        }

    @Test
    def testOuterLambdaInRegion07(): Unit \ Assert =
        // The outer lambda captures a variable and is used inside a lambda inside a region inside another lambda.
        let height = 2;
        let src = (x: Int32, y: Int32) -> if (y < 0 or y >= height or x < 0) 0 else x + y;
        let rows = List.range(0, height) |> List.map(y ->
            region rc {
                let r = Ref.fresh(rc, 0);
                List.forEach(x -> Ref.put(Ref.get(r) + requiresPure2(src) + x + y, r), 0 :: 1 :: Nil);
                Ref.get(r)
            });
        assertEq(expected = 1 :: 3 :: Nil, rows)

    /////////////////////////////////////////////////////////////////////////////
    // Foreach loops inside one region                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testForeachInRegion01(): Unit \ Assert = region rc {
        let l = 1 :: 2 :: 3 :: Nil;
        let k = 4 :: 5 :: 6 :: Nil;
        let r = Ref.fresh(rc, 0);
        foreach (x <- l) Ref.put(Ref.get(r) + x, r);
        foreach (x <- l) Ref.put(Ref.get(r) + x, r);
        foreach (a <- l) {
            Ref.put(Ref.get(r) + a, r);
            foreach (b <- k) Ref.put(Ref.get(r) + b, r)
        };
        foreach (a <- l; b <- k) Ref.put(Ref.get(r) + a + b, r);
        foreach (a <- l; if a > 1; b <- k; if b < 6) Ref.put(Ref.get(r) + a + b, r);
        foreach (a <- l; b <- k; if a > 1 and b < 6) Ref.put(Ref.get(r) + a + b, r);
        assertEq(expected = 6 + 6 + (6 + 45) + 63 + 28 + 28, Ref.get(r))
    }

    @Test
    def testForeachInRegion02(): Unit \ Assert = region rc {
        let l = 1 :: 2 :: 3 :: Nil;
        let k = 4 :: 5 :: 6 :: Nil;
        let r = Ref.fresh(rc, 0);
        foreach (x <- l) Ref.put(Ref.get(r) + x, r);
        foreach (x <- l) Ref.put(Ref.get(r) + x, r);
        List.iterator(rc, l) |> Iterator.forEach(match x -> Ref.put(Ref.get(r) + x, r));
        foreach (a <- l) {
            Ref.put(Ref.get(r) + a, r);
            foreach (b <- k) Ref.put(Ref.get(r) + b, r)
        };
        List.iterator(rc, l) |> Iterator.forEach(match a -> {
            Ref.put(Ref.get(r) + a, r);
            List.iterator(rc, k) |> Iterator.forEach(match b -> Ref.put(Ref.get(r) + b, r))
        });
        foreach (a <- l; b <- k) Ref.put(Ref.get(r) + a + b, r);
        List.iterator(rc, l) |> Iterator.forEach(match a ->
            List.iterator(rc, k) |> Iterator.forEach(match b -> Ref.put(Ref.get(r) + a + b, r)));
        foreach (a <- l; if a > 1; b <- k; if b < 6) Ref.put(Ref.get(r) + a + b, r);
        foreach (a <- l; b <- k; if a > 1 and b < 6) Ref.put(Ref.get(r) + a + b, r);
        List.iterator(rc, l) |> Iterator.forEach(match a ->
            List.iterator(rc, k) |> Iterator.forEach(match b ->
                if (a > 1 and b < 6) Ref.put(Ref.get(r) + a + b, r) else ()));
        assertEq(expected = 6 + 6 + 6 + (6 + 45) + (6 + 45) + 63 + 63 + 28 + 28 + 28, Ref.get(r))
    }

}
