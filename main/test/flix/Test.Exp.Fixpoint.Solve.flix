namespace Test/Exp/Fixpoint/Solve {

    @test
    def testFixpointSolveUnit(): Bool =
        let m = solve #{
            Edge((), ()). Edge((), ()). Edge((), ()). Edge((), ()).
            Edge(y, x) :- Edge(x, y).
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        m |= Path((), ()).

    @test
    def testFixpointSolveBool(): Bool =
        let m = solve #{
            Edge(true, false). Edge(false, true). Edge(true, false). Edge(false, true).
            Edge(y, x) :- Edge(x, y).
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        m |= Path(false, true).

    @test
    def testFixpointSolveChar(): Bool =
        let m = solve #{
            Edge('a', 'b'). Edge('b', 'c'). Edge('c', 'd'). Edge('d', 'e').
            Edge(y, x) :- Edge(x, y).
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        m |= Path('d', 'b').

    @test
    def testFixpointSolveInt8(): Bool =
        let m = solve #{
            Edge(1i8, 2i8). Edge(2i8, 3i8). Edge(3i8, 4i8). Edge(4i8, 5i8).
            Edge(y, x) :- Edge(x, y).
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        m |= Path(4i8, 3i8).

    @test
    def testFixpointSolveInt16(): Bool =
        let m = solve #{
            Edge(1i16, 2i16). Edge(2i16, 3i16). Edge(3i16, 4i16). Edge(4i16, 5i16).
            Edge(y, x) :- Edge(x, y).
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        m |= Path(4i16, 3i16).

    @test
    def testFixpointSolveInt32(): Bool =
        let m = solve #{
            Edge(1i32, 2i32). Edge(2i32, 3i32). Edge(3i32, 4i32). Edge(4i32, 5i32).
            Edge(y, x) :- Edge(x, y).
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        m |= Path(4i32, 3i32).

    @test
    def testFixpointSolveInt64(): Bool =
        let m = solve #{
            Edge(1i64, 2i64). Edge(2i64, 3i64). Edge(3i64, 4i64). Edge(4i64, 5i64).
            Edge(y, x) :- Edge(x, y).
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        m |= Path(4i64, 3i64).

    @test
    def testFixpointSolveBigInt(): Bool =
        let m = solve #{
            Edge(1ii, 2ii). Edge(2ii, 3ii). Edge(3ii, 4ii). Edge(4ii, 5ii).
            Edge(y, x) :- Edge(x, y).
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        m |= Path(4ii, 3ii).

    @test
    def testFixpointSolveString(): Bool =
        let m = solve #{
            Edge("a", "b"). Edge("b", "c"). Edge("c", "d"). Edge("d", "e").
            Edge(y, x) :- Edge(x, y).
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        m |= Path("d", "b").

    @test
    def testFixpointOption(): Bool =
        let m = solve #{
            Edge(Some("a"), Some("b")). Edge(Some("b"), Some("c")). Edge(Some("c"), Some("d")). Edge(Some("d"), Some("e")).
            Edge(None, Some("a")).
            Edge(Some("a"), None).
            Edge(y, x) :- Edge(x, y).
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        m |= Path(Some("d"), Some("b")).

    @test
    def testFixpointSolveResult(): Bool =
        let m = solve #{
            Edge(Ok("a"), Ok("b")). Edge(Ok("b"), Ok("c")). Edge(Ok("c"), Ok("d")). Edge(Ok("d"), Ok("e")).
            Edge(Ok("a"), Err("a")).
            Edge(Err("a"), Ok("a")).
            Edge(y, x) :- Edge(x, y).
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        m |= Path(Ok("d"), Ok("b")).

    @test
    def testFixpointSolvePoly01(): Bool =
        let m = solve closure() <+> #{
            Edge(1i32, 2i32). Edge(2i32, 3i32). Edge(3i32, 4i32). Edge(4i32, 5i32).
        };
        m |= Path(4i32, 2i32).

    @test
    def testFixpointSolvePoly02(): Bool =
        let m = solve closure() <+> #{
            Edge(1i64, 2i64). Edge(2i64, 3i64). Edge(3i64, 4i64). Edge(4i64, 5i64).
        };
        m |= Path(4i64, 2i64).

    @test
    def testFixpointSolvePoly03(): Bool =
        let m = solve closure() <+> #{
            Edge("a", "b"). Edge("b", "c"). Edge("c", "d"). Edge("d", "e").
        };
        m |= Path("d", "b").

    def closure(): #{Edge(a, a), Path(a, a)} = #{
        Edge(y, x) :- Edge(x, y).
        Path(x, y) :- Edge(x, y).
        Path(x, z) :- Path(x, y), Edge(y, z).
    }

    enum Option[t] {
        case None,
        case Some(t)
    }

    enum Result[t, e] {
        case Ok(t),
        case Err(e)
    }

    instance ToString[Option[a]] with [a : ToString] {
        def toString(o: Option[a]): String = ???
    }

    instance ToString[Result[t, e]] with [t : ToString, e : ToString] {
        def toString(r: Result[t, e]): String = ???
    }

}
