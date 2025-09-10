mod Test.Exp.Fixpoint.PQuery {

    use Assert.{assertEq, assertTrue};

    ///////////////////////////////////////////////////////////////////////////
    // `psolve` should support computation of provenance with `pquery`.
    ///////////////////////////////////////////////////////////////////////////

    @Test
    def testPSolve01(): Unit \ Assert =
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
        assertEq(expected = expect, actual)

    @Test
    def testPSolve02(): Unit \ Assert =
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
        assertEq(expected = expect, actual)

    @Test
    def testPSolve03(): Unit \ Assert =
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
        assertEq(expected = expect, actual)

    ///////////////////////////////////////////////////////////////////////////
    // `psolve` should support multiple calls to `pquery`.
    ///////////////////////////////////////////////////////////////////////////

    @Test
    def testPSolveMultiple01(): Unit \ Assert = {
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
        assertEq(expected = Vector#{0}, qry(0));
        assertEq(expected = Vector#{1}, qry(1));
        assertEq(expected = Vector#{2}, qry(2))
    }

    @Test
    def testPSolveMultiple02(): Unit \ Assert = {
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
        assertEq(expected = Vector#{0, 1, 2, 3}, qry(0));
        assertEq(expected = Vector#{1, 2, 3, 0}, qry(1));
        assertEq(expected = Vector#{2, 3, 0, 1}, qry(2));
        assertEq(expected = Vector#{3, 0, 1, 2}, qry(3))
    }

    @Test
    def testPSolveMultiple03(): Unit \ Assert = {
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
        assertEq(expected = Vector#{0, 1}, qry(0));
        assertEq(expected = Vector#{1, 0}, qry(1));
        assertEq(expected = Vector#{2, 3}, qry(2));
        assertEq(expected = Vector#{3, 2}, qry(3))
    }

    @Test
    def testPSolveMultiple04(): Unit \ Assert = {
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
        assertEq(expected = Vector#{0, 1, 2}, qry(0));
        assertEq(expected = Vector#{1, 2, 0}, qry(1));
        assertEq(expected = Vector#{2, 0, 1}, qry(2));
        assertEq(expected = Vector#{3, 4, 5}, qry(3));
        assertEq(expected = Vector#{4, 5, 3}, qry(4));
        assertEq(expected = Vector#{5, 3, 4}, qry(5))
    }

    @Test
    def testPSolveMultiple05(): Unit \ Assert = {
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
        assertEq(expected = Vector#{0, 1, 2, 3}, qry(0));
        assertEq(expected = Vector#{1, 2, 3, 0}, qry(1));
        assertEq(expected = Vector#{2, 3, 0, 1}, qry(2));
        assertEq(expected = Vector#{3, 0, 1, 2}, qry(3));
        assertEq(expected = Vector#{4, 5, 6, 7}, qry(4));
        assertEq(expected = Vector#{5, 6, 7, 4}, qry(5));
        assertEq(expected = Vector#{6, 7, 4, 5}, qry(6));
        assertEq(expected = Vector#{7, 4, 5, 6}, qry(7))
    }


    ///////////////////////////////////////////////////////////////////////////
    // `psolve` should be idempotent.
    ///////////////////////////////////////////////////////////////////////////

    @Test
    def testPSolveIdempotent01(): Unit \ Assert =
        let db = #{
            Edge(1, 2). Edge(2, 3). Edge(3, 1). Edge(1, 4).
        };
        let pr = #{
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
            Cycle(x)   :- Path(x, x).
        };
        let pm = psolve (psolve db, pr);
        let pp = pquery pm select Cycle(2) with {Edge};
        assertEq(expected = Vector#{(2, 3), (3, 1), (1, 2)}, Vector.map(v -> ematch v { case Edge(x, y) => (x, y) }, pp))

    @Test
    def testPSolveIdempotent02(): Unit \ Assert =
        let db = #{
            Edge(1, 2). Edge(2, 3). Edge(3, 1). Edge(1, 4).
        };
        let pr = #{
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
            Cycle(x)   :- Path(x, x).
        };
        let pm = psolve (psolve (psolve db, pr));
        let pp = pquery pm select Cycle(2) with {Edge};
        assertEq(expected = Vector#{(2, 3), (3, 1), (1, 2)}, Vector.map(v -> ematch v { case Edge(x, y) => (x, y) }, pp))


    ///////////////////////////////////////////////////////////////////////////
    // `psolve` should return an immutable value.
    ///////////////////////////////////////////////////////////////////////////

    @Test
    def testPSolveImmutable01(): Unit \ Assert =
        let db1 = #{
            Edge(1, 2). Edge(2, 3). Edge(3, 1). Edge(1, 4).
        };
        let db2 = #{ Edge(5, 5). };
        let db3 = #{ Edge(7, 7). };
        let pr = #{
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
            Cycle(x) :- Path(x, x).
        };
        let pm = psolve db1, pr;

        // pm should retain edb facts.
        assertTrue(Vector.nonEmpty(query pm select () from Edge(1, 2)));
        assertTrue(Vector.nonEmpty(query pm select () from Edge(2, 3)));

        // pm should should allow the derivation of new facts.
        assertTrue(Vector.nonEmpty(query db2, pm, pr select () from Cycle(5)));
        assertTrue(Vector.nonEmpty(query db3, pm, pr select () from Cycle(7)));

        // pm should *NOT* contain the newly derived facts.
        assertTrue(Vector.isEmpty(query pm select () from Cycle(5)));
        assertTrue(Vector.isEmpty(query pm select () from Cycle(7)))

}
