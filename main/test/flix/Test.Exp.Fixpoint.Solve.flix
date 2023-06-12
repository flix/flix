mod Test.Exp.Fixpoint.Solve {

    @test
    def testFixpointSolveUnit(): Bool =
        let p = #{
            Edge((), ()). Edge((), ()). Edge((), ()). Edge((), ()).
            Edge(y, x) :- Edge(x, y).
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        let m = query p select (x, y) from Path(x, y);
        Vector.memberOf(((), ()), m)

    @test
    def testFixpointSolveBool(): Bool =
        let p = #{
            Edge(true, false). Edge(false, true). Edge(true, false). Edge(false, true).
            Edge(y, x) :- Edge(x, y).
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        let m = query p select (x, y) from Path(x, y);
        Vector.memberOf((false, true), m)

    @test
    def testFixpointSolveChar(): Bool =
        let p = #{
            Edge('a', 'b'). Edge('b', 'c'). Edge('c', 'd'). Edge('d', 'e').
            Edge(y, x) :- Edge(x, y).
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        let m = query p select (x, y) from Path(x, y);
        Vector.memberOf(('d', 'b'), m)

    @test
    def testFixpointSolveInt8(): Bool =
        let p = #{
            Edge(1i8, 2i8). Edge(2i8, 3i8). Edge(3i8, 4i8). Edge(4i8, 5i8).
            Edge(y, x) :- Edge(x, y).
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        let m = query p select (x, y) from Path(x, y);
        Vector.memberOf((4i8, 3i8), m)

    @test
    def testFixpointSolveInt16(): Bool =
        let p = #{
            Edge(1i16, 2i16). Edge(2i16, 3i16). Edge(3i16, 4i16). Edge(4i16, 5i16).
            Edge(y, x) :- Edge(x, y).
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        let m = query p select (x, y) from Path(x, y);
        Vector.memberOf((4i16, 3i16), m)

    @test
    def testFixpointSolveInt32(): Bool =
        let p = #{
            Edge(1i32, 2i32). Edge(2i32, 3i32). Edge(3i32, 4i32). Edge(4i32, 5i32).
            Edge(y, x) :- Edge(x, y).
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        let m = query p select (x, y) from Path(x, y);
        Vector.memberOf((4i32, 3i32), m)

    @test
    def testFixpointSolveInt64(): Bool =
        let p = #{
            Edge(1i64, 2i64). Edge(2i64, 3i64). Edge(3i64, 4i64). Edge(4i64, 5i64).
            Edge(y, x) :- Edge(x, y).
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        let m = query p select (x, y) from Path(x, y);
        Vector.memberOf((4i64, 3i64), m)

    @test
    def testFixpointSolveBigInt(): Bool =
        let p = #{
            Edge(1ii, 2ii). Edge(2ii, 3ii). Edge(3ii, 4ii). Edge(4ii, 5ii).
            Edge(y, x) :- Edge(x, y).
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        let m = query p select (x, y) from Path(x, y);
        Vector.memberOf((4ii, 3ii), m)

    @test
    def testFixpointSolveString(): Bool =
        let p = #{
            Edge("a", "b"). Edge("b", "c"). Edge("c", "d"). Edge("d", "e").
            Edge(y, x) :- Edge(x, y).
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        let m = query p select (x, y) from Path(x, y);
        Vector.memberOf(("d", "b"), m)

    @test
    def testFixpointOption(): Bool =
        let p = #{
            Edge(Some("a"), Some("b")). Edge(Some("b"), Some("c")). Edge(Some("c"), Some("d")). Edge(Some("d"), Some("e")).
            Edge(None, Some("a")).
            Edge(Some("a"), None).
            Edge(y, x) :- Edge(x, y).
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        let m = query p select (x, y) from Path(x, y);
        Vector.memberOf((Some("d"), Some("b")), m)

    @test
    def testFixpointSolveResult(): Bool =
        let p = #{
            Edge(Ok("a"), Ok("b")). Edge(Ok("b"), Ok("c")). Edge(Ok("c"), Ok("d")). Edge(Ok("d"), Ok("e")).
            Edge(Ok("a"), Err("a")).
            Edge(Err("a"), Ok("a")).
            Edge(y, x) :- Edge(x, y).
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        let m = query p select (x, y) from Path(x, y);
        Vector.memberOf((Ok("d"), Ok("b")), m)

    @test
    def testFixpointSolvePoly01(): Bool =
        let p = closure() <+> #{
            Edge(1i32, 2i32). Edge(2i32, 3i32). Edge(3i32, 4i32). Edge(4i32, 5i32).
        };
        let m = query p select (x, y) from Path(x, y);
        Vector.memberOf((4i32, 2i32), m)

    @test
    def testFixpointSolvePoly02(): Bool =
        let p = closure() <+> #{
            Edge(1i64, 2i64). Edge(2i64, 3i64). Edge(3i64, 4i64). Edge(4i64, 5i64).
        };
        let m = query p select (x, y) from Path(x, y);
        Vector.memberOf((4i64, 2i64), m)

    @test
    def testFixpointSolvePoly03(): Bool =
        let p = closure() <+> #{
            Edge("a", "b"). Edge("b", "c"). Edge("c", "d"). Edge("d", "e").
        };
        let m = query p select (x, y) from Path(x, y);
        Vector.memberOf(("d", "b"), m)

    def closure(): #{Edge(a, a), Path(a, a) | r} with Order[a] = #{
        Edge(y, x) :- Edge(x, y).
        Path(x, y) :- Edge(x, y).
        Path(x, z) :- Path(x, y), Edge(y, z).
    }

    @test
    def testFixpointSolveArity1(): Bool =
        let p = #{
            One("one").
            Last(x) :- One(x).
        };
        let m = query p select x from Last(x);
        m == Vector#{"one"}

    @test
    def testFixpointSolveArity5(): Bool =
        let p = #{
            Five("one", "two", "three", "four", "five").
            Last(x) :- Five(_, _, _,   _, x).
        };
        let m = query p select x from Last(x);
        m == Vector#{"five"}

    @test
    def testFixpointSolveArity10(): Bool =
        let p = #{
            Ten("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten").
            Last(x) :- Ten(_, _, _,   _, _, _,   _, _, _,  x).
        };
        let m = query p select x from Last(x);
        m == Vector#{"ten"}


    @test
    def testFixpointSolveArity15(): Bool =
        let p = #{
            Fifteen("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen").
            Last(x) :- Fifteen(_, _, _,   _, _, _,   _, _, _,   _, _, _,    _, _, x).
        };
        let m = query p select x from Last(x);
        m == Vector#{"fifteen"}
}
