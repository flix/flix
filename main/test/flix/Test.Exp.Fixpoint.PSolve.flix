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

}
