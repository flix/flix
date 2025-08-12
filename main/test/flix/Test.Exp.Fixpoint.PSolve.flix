mod Test.Exp.Fixpoint.PQuery {

    ///////////////////////////////////////////////////////////////////////////
    // Basic sanity checks that PSolve computes provenance.
    ///////////////////////////////////////////////////////////////////////////

    @Test
    def testPSolve01(): Bool =
        let db = #{
            A(1). A(2). A(3).
            B(1). B(2). B(3).
            R(x) :- A(x), B(x), B(x), A(x).
        };
        let pm = psolve db;
        let expect = Vector#{"A(1)", "B(1)", "B(1)", "A(1)"};
        let result = pquery pm select R(1) with {A, B};
        let actual = result |> Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
            case B(x) => "B(${x})"
        });
        Assert.eq(expect, actual)

    @Test
    def testPSolve02(): Bool =
        let db = #{
            A(1). A(2). A(3).
            B(1). B(2). B(3).
            R(x) :- A(x), B(x), B(x), A(x).
        };
        let pm = psolve db;
        let expect = Vector#{"A(2)", "B(2)", "B(2)", "A(2)"};
        let result = pquery pm select R(2) with {A, B};
        let actual = result |> Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
            case B(x) => "B(${x})"
        });
        Assert.eq(expect, actual)

    @Test
    def testPSolve03(): Bool =
        let db = #{
            A(1). A(2). A(3).
            B(1). B(2). B(3).
            R(x) :- A(x), B(x), B(x), A(x).
        };
        let pm = psolve db;
        let expect = Vector#{"A(3)", "B(3)", "B(3)", "A(3)"};
        let result = pquery pm select R(3) with {A, B};
        let actual = result |> Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
            case B(x) => "B(${x})"
        });
        Assert.eq(expect, actual)

    ///////////////////////////////////////////////////////////////////////////
    // `psolve` can be used with multiple calls to `pquery`.
    ///////////////////////////////////////////////////////////////////////////

    @Test
    def testPSolveMultiple01(): Bool = {
        let db = #{
            Edge(0, 0). Edge(1, 1). Edge(2, 2).
        };
        let pr = #{
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
            Cycle(x)   :- Path(x, x).
        };
        let pm = psolve db, pr;
        def qry(at) = {
            let result = pquery pm select Cycle(at) with {Edge};
            result |> Vector.map(v -> ematch v { case Edge(x, _) => x })
        };
        Vector#{0} `Assert.eq` qry(0) and
        Vector#{1} `Assert.eq` qry(1) and
        Vector#{2} `Assert.eq` qry(2)
    }

    @Test
    def testPSolveMultiple02(): Bool = {
        let db = #{
            Edge(0, 1). Edge(1, 2). Edge(2, 3). Edge(3, 0). Edge(1, 4). Edge(4, 5). Edge(5, 6). Edge(6, 4).
        };
        let pr = #{
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
            Cycle(x)   :- Path(x, x).
        };
        let pm = psolve db, pr;
        def qry(at) = {
            let result = pquery pm select Cycle(at) with {Edge};
            result |> Vector.map(v -> ematch v { case Edge(x, _) => x })
        };
        Vector#{0, 1, 2, 3} `Assert.eq` qry(0) and
        Vector#{1, 2, 3, 0} `Assert.eq` qry(1) and
        Vector#{2, 3, 0, 1} `Assert.eq` qry(2) and
        Vector#{3, 0, 1, 2} `Assert.eq` qry(3)
    }

    @Test
    def testPSolveMultiple03(): Bool = {
        let db = #{
            Edge(0, 1). Edge(1, 0). Edge(2, 3). Edge(3, 2).
        };
        let pr = #{
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
            Cycle(x)   :- Path(x, x).
        };
        let pm = psolve db, pr;
        def qry(at) = {
            let result = pquery pm select Cycle(at) with {Edge};
            result |> Vector.map(v -> ematch v { case Edge(x, _) => x })
        };
        Vector#{0, 1} `Assert.eq` qry(0) and
        Vector#{1, 0} `Assert.eq` qry(1) and
        Vector#{2, 3} `Assert.eq` qry(2) and
        Vector#{3, 2} `Assert.eq` qry(3)
    }

    @Test
    def testPSolveMultiple04(): Bool = {
        let db = #{
            Edge(0, 1). Edge(1, 2). Edge(2, 0). Edge(3, 4). Edge(4, 5). Edge(5, 3).
        };
        let pr = #{
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
            Cycle(x)   :- Path(x, x).
        };
        let pm = psolve db, pr;
        def qry(at) = {
            let result = pquery pm select Cycle(at) with {Edge};
            result |> Vector.map(v -> ematch v { case Edge(x, _) => x })
        };
        Vector#{0, 1, 2} `Assert.eq` qry(0) and
        Vector#{1, 2, 0} `Assert.eq` qry(1) and
        Vector#{2, 0, 1} `Assert.eq` qry(2) and
        Vector#{3, 4, 5} `Assert.eq` qry(3) and
        Vector#{4, 5, 3} `Assert.eq` qry(4) and
        Vector#{5, 3, 4} `Assert.eq` qry(5)
    }

    @Test
    def testPSolveMultiple05(): Bool = {
        let db = #{
            Edge(0, 1). Edge(1, 2). Edge(2, 3). Edge(3, 0). Edge(4, 5). Edge(5, 6). Edge(6, 7). Edge(7, 4).
        };
        let pr = #{
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
            Cycle(x)   :- Path(x, x).
        };
        let pm = psolve db, pr;
        def qry(at) = {
            let result = pquery pm select Cycle(at) with {Edge};
            result |> Vector.map(v -> ematch v { case Edge(x, _) => x })
        };
        Vector#{0, 1, 2, 3} `Assert.eq` qry(0) and
        Vector#{1, 2, 3, 0} `Assert.eq` qry(1) and
        Vector#{2, 3, 0, 1} `Assert.eq` qry(2) and
        Vector#{3, 0, 1, 2} `Assert.eq` qry(3) and
        Vector#{4, 5, 6, 7} `Assert.eq` qry(4) and
        Vector#{5, 6, 7, 4} `Assert.eq` qry(5) and
        Vector#{6, 7, 4, 5} `Assert.eq` qry(6) and
        Vector#{7, 4, 5, 6} `Assert.eq` qry(7)
    }

}
