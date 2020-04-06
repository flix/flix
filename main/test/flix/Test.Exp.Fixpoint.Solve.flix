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
    def testFixpointSolveInt32(): Bool =
        let m = solve #{
            Edge(1i32, 2i32). Edge(2i32, 3i32). Edge(3i32, 4i32). Edge(4i32, 5i32).
            Edge(y, x) :- Edge(x, y).
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        m |= Path(4i32, 3i32).




}
