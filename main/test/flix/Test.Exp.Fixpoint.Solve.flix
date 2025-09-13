mod Test.Exp.Fixpoint.Solve {

    use Assert.{assertEq, assertTrue};

    @test
    def testFixpointSolveUnit(): Unit \ Assert =
        let p = #{
            Edge((), ()). Edge((), ()). Edge((), ()). Edge((), ()).
            Edge(y, x) :- Edge(x, y).
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        let m = query p select (x, y) from Path(x, y);
        assertTrue(Vector.memberOf(((), ()), m))

    @test
    def testFixpointSolveBool(): Unit \ Assert =
        let p = #{
            Edge(true, false). Edge(false, true). Edge(true, false). Edge(false, true).
            Edge(y, x) :- Edge(x, y).
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        let m = query p select (x, y) from Path(x, y);
        assertTrue(Vector.memberOf((false, true), m))

    @test
    def testFixpointSolveChar(): Unit \ Assert =
        let p = #{
            Edge('a', 'b'). Edge('b', 'c'). Edge('c', 'd'). Edge('d', 'e').
            Edge(y, x) :- Edge(x, y).
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        let m = query p select (x, y) from Path(x, y);
        assertTrue(Vector.memberOf(('d', 'b'), m))

    @test
    def testFixpointSolveInt8(): Unit \ Assert =
        let p = #{
            Edge(1i8, 2i8). Edge(2i8, 3i8). Edge(3i8, 4i8). Edge(4i8, 5i8).
            Edge(y, x) :- Edge(x, y).
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        let m = query p select (x, y) from Path(x, y);
        assertTrue(Vector.memberOf((4i8, 3i8), m))

    @test
    def testFixpointSolveInt16(): Unit \ Assert =
        let p = #{
            Edge(1i16, 2i16). Edge(2i16, 3i16). Edge(3i16, 4i16). Edge(4i16, 5i16).
            Edge(y, x) :- Edge(x, y).
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        let m = query p select (x, y) from Path(x, y);
        assertTrue(Vector.memberOf((4i16, 3i16), m))

    @test
    def testFixpointSolveInt32(): Unit \ Assert =
        let p = #{
            Edge(1i32, 2i32). Edge(2i32, 3i32). Edge(3i32, 4i32). Edge(4i32, 5i32).
            Edge(y, x) :- Edge(x, y).
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        let m = query p select (x, y) from Path(x, y);
        assertTrue(Vector.memberOf((4i32, 3i32), m))

    @test
    def testFixpointSolveInt64(): Unit \ Assert =
        let p = #{
            Edge(1i64, 2i64). Edge(2i64, 3i64). Edge(3i64, 4i64). Edge(4i64, 5i64).
            Edge(y, x) :- Edge(x, y).
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        let m = query p select (x, y) from Path(x, y);
        assertTrue(Vector.memberOf((4i64, 3i64), m))

    @test
    def testFixpointSolveBigInt(): Unit \ Assert =
        let p = #{
            Edge(1ii, 2ii). Edge(2ii, 3ii). Edge(3ii, 4ii). Edge(4ii, 5ii).
            Edge(y, x) :- Edge(x, y).
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        let m = query p select (x, y) from Path(x, y);
        assertTrue(Vector.memberOf((4ii, 3ii), m))

    @test
    def testFixpointSolveString(): Unit \ Assert =
        let p = #{
            Edge("a", "b"). Edge("b", "c"). Edge("c", "d"). Edge("d", "e").
            Edge(y, x) :- Edge(x, y).
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        let m = query p select (x, y) from Path(x, y);
        assertTrue(Vector.memberOf(("d", "b"), m))

    @test
    def testFixpointOption(): Unit \ Assert =
        let p = #{
            Edge(Some("a"), Some("b")). Edge(Some("b"), Some("c")). Edge(Some("c"), Some("d")). Edge(Some("d"), Some("e")).
            Edge(None, Some("a")).
            Edge(Some("a"), None).
            Edge(y, x) :- Edge(x, y).
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        let m = query p select (x, y) from Path(x, y);
        assertTrue(Vector.memberOf((Some("d"), Some("b")), m))

    @test
    def testFixpointSolveResult(): Unit \ Assert =
        let p = #{
            Edge(Ok("a"), Ok("b")). Edge(Ok("b"), Ok("c")). Edge(Ok("c"), Ok("d")). Edge(Ok("d"), Ok("e")).
            Edge(Ok("a"), Err("a")).
            Edge(Err("a"), Ok("a")).
            Edge(y, x) :- Edge(x, y).
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        let m = query p select (x, y) from Path(x, y);
        assertTrue(Vector.memberOf((Ok("d"), Ok("b")), m))

    @test
    def testFixpointSolvePoly01(): Unit \ Assert =
        let p = closure() <+> #{
            Edge(1i32, 2i32). Edge(2i32, 3i32). Edge(3i32, 4i32). Edge(4i32, 5i32).
        };
        let m = query p select (x, y) from Path(x, y);
        assertTrue(Vector.memberOf((4i32, 2i32), m))

    @test
    def testFixpointSolvePoly02(): Unit \ Assert =
        let p = closure() <+> #{
            Edge(1i64, 2i64). Edge(2i64, 3i64). Edge(3i64, 4i64). Edge(4i64, 5i64).
        };
        let m = query p select (x, y) from Path(x, y);
        assertTrue(Vector.memberOf((4i64, 2i64), m))

    @test
    def testFixpointSolvePoly03(): Unit \ Assert =
        let p = closure() <+> #{
            Edge("a", "b"). Edge("b", "c"). Edge("c", "d"). Edge("d", "e").
        };
        let m = query p select (x, y) from Path(x, y);
        assertTrue(Vector.memberOf(("d", "b"), m))

    def closure(): #{Edge(a, a), Path(a, a) | r} with Order[a] = #{
        Edge(y, x) :- Edge(x, y).
        Path(x, y) :- Edge(x, y).
        Path(x, z) :- Path(x, y), Edge(y, z).
    }

    @test
    def testFixpointSolveArity1(): Unit \ Assert =
        let p = #{
            One("one").
            Last(x) :- One(x).
        };
        let m = query p select x from Last(x);
        assertEq(expected = Vector#{"one"}, m)

    @test
    def testFixpointSolveArity5(): Unit \ Assert =
        let p = #{
            Five("one", "two", "three", "four", "five").
            Last(x) :- Five(_, _, _,   _, x).
        };
        let m = query p select x from Last(x);
        assertEq(expected = Vector#{"five"}, m)

    @test
    def testFixpointSolveArity10(): Unit \ Assert =
        let p = #{
            Ten("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten").
            Last(x) :- Ten(_, _, _,   _, _, _,   _, _, _,  x).
        };
        let m = query p select x from Last(x);
        assertEq(expected = Vector#{"ten"}, m)


    @test
    def testFixpointSolveArity15(): Unit \ Assert =
        let p = #{
            Fifteen("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen").
            Last(x) :- Fifteen(_, _, _,   _, _, _,   _, _, _,   _, _, _,    _, _, x).
        };
        let m = query p select x from Last(x);
        assertEq(expected = Vector#{"fifteen"}, m)


    /////////////////////////////////////////////////////////////////////////////
    // Test that `solve` does not mutate the original program being solved     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testFixpointSolveKeyword01(): Unit \ Assert = {
        let pr1 = solve #{
            ASolve(42).
        };
        let pr2 = #{
            ASolve(-42).
        };
        let combined = solve pr1, pr2;
        let mm1 = query pr1 select x from ASolve(x);
        let mm2 = query combined select x from ASolve(x);
        assertEq(expected = Vector#{42}, mm1);
        assertEq(expected = Vector#{-42, 42}, mm2)
    }

    @Test
    def testFixpointSolveKeyword02(): Unit \ Assert = {
        let pr1 = solve #{
            ASolve(42).
        };
        let pr2 = solve #{
            ASolve(-42).
        };
        let combined = solve pr1, pr2;
        let mm1 = query pr1 select x from ASolve(x);
        let mm2 = query combined select x from ASolve(x);
        let mm3 = query pr2 select x from ASolve(x);
        assertEq(expected = Vector#{42}, mm1);
        assertEq(expected = Vector#{-42, 42}, mm2);
        assertEq(expected = Vector#{-42}, mm3)
    }

    @Test
    def testFixpointSolveKeyword03(): Unit \ Assert = {
        let pr1 = solve #{
            BSolve(1).
            BSolve(2).
            ASolve(x) :- BSolve(x).
        };
        let pr2 = #{
            ASolve(-x) :- BSolve(x).
        };
        let combined = solve pr1, pr2;
        let mm1 = query pr1 select x from ASolve(x);
        let mm2 = query combined select x from ASolve(x);
        assertEq(expected = Vector#{1, 2}, mm1);
        assertEq(expected = Vector#{-2, -1, 1, 2}, mm2)
    }

    @Test
    def testFixpointSolveKeyword04(): Unit \ Assert = {
        let pr1 = #{
            ASolve(1).
            ASolve(x) :- BSolve(x).
        };
        let pr2 = #{
            BSolve(2).
            BSolve(x) :- ASolve(x).
        };
        let combined = solve pr1, pr2;
        let pr1Solved = solve pr1;
        let pr2Solved = solve pr2;
        let mm11 = query pr1Solved select x from ASolve(x);
        let mm12 = query pr1Solved select x from BSolve(x);
        let mm21 = query pr2Solved select x from ASolve(x);
        let mm22 = query pr2Solved select x from BSolve(x);
        let mm31 = query combined select x from ASolve(x);
        let mm32 = query combined select x from BSolve(x);
        assertEq(expected = Vector#{1}, mm11);
        assertEq(expected = Vector.empty(), mm12);
        assertEq(expected = Vector.empty(), mm21);
        assertEq(expected = Vector#{2}, mm22);
        assertEq(expected = Vector#{1, 2}, mm31);
        assertEq(expected = Vector#{1, 2}, mm32)
    }

}
