mod Test.Exp.Fixpoint.PQuery {

    ///////////////////////////////////////////////////////////////////////////
    // Provenance at Depth 1
    ///////////////////////////////////////////////////////////////////////////

    @Test
    def testPQueryUnit(): Bool =
        let pr = #{
            A(()).
        };
        let pp = pquery pr select A(()) with {A};
        Vector.map(v -> ematch v { case A(x) => x }, pp) `Assert.eq` Vector#{()}

    @Test
    def testPQueryBool(): Bool =
        let pr = #{
            A(true).
        };
        let pp = pquery pr select A(true) with {A};
        Vector.map(v -> ematch v { case A(x) => x }, pp) `Assert.eq` Vector#{true}

    @Test
    def testPQueryInt8(): Bool =
        let pr = #{
            A(42i8).
        };
        let pp = pquery pr select A(42i8) with {A};
        Vector.map(v -> ematch v { case A(x) => x }, pp) `Assert.eq` Vector#{42i8}

    @Test
    def testPQueryInt16(): Bool =
        let pr = #{
            A(1000i16).
        };
        let pp = pquery pr select A(1000i16) with {A};
        Vector.map(v -> ematch v { case A(x) => x }, pp) `Assert.eq` Vector#{1000i16}

    @Test
    def testPQueryInt32(): Bool =
        let pr = #{
            A(1).
        };
        let pp = pquery pr select A(1) with {A};
        Vector.map(v -> ematch v { case A(x) => x }, pp) `Assert.eq` Vector#{1}

    @Test
    def testPQueryInt64(): Bool =
        let pr = #{
            A(1000000000i64).
        };
        let pp = pquery pr select A(1000000000i64) with {A};
        Vector.map(v -> ematch v { case A(x) => x }, pp) `Assert.eq` Vector#{1000000000i64}

    @Test
    def testPQueryFloat32(): Bool =
        let pr = #{
            A(3.14f32).
        };
        let pp = pquery pr select A(3.14f32) with {A};
        Vector.map(v -> ematch v { case A(x) => x }, pp) `Assert.eq` Vector#{3.14f32}

    @Test
    def testPQueryFloat64(): Bool =
        let pr = #{
            A(3.141592653589793f64).
        };
        let pp = pquery pr select A(3.141592653589793f64) with {A};
        Vector.map(v -> ematch v { case A(x) => x }, pp) `Assert.eq` Vector#{3.141592653589793f64}

    @Test
    def testPQueryString(): Bool =
        let pr = #{
            A("hello").
        };
        let pp = pquery pr select A("hello") with {A};
        Vector.map(v -> ematch v { case A(x) => x }, pp) `Assert.eq` Vector#{"hello"}

    @Test
    def testPQueryInt32Pair(): Bool =
        let pr = #{
            A((1, 2)).
        };
        let pp = pquery pr select A((1, 2)) with {A};
        Vector.map(v -> ematch v { case A(x) => x }, pp) `Assert.eq` Vector#{(1, 2)}

    ///////////////////////////////////////////////////////////////////////////
    // Provenance at Depth 2
    ///////////////////////////////////////////////////////////////////////////

    @Test
    def testPQueryDepth2Unit(): Bool =
        let pr = #{
            A(()).
            B(x) :- A(x).
            R(x) :- B(x), B(x).
        };
        let pp = pquery pr select R(()) with {A};
        Vector.map(v -> ematch v { case A(x) => x }, pp) `Assert.eq` Vector#{(), ()}

    @Test
    def testPQueryDepth2Bool(): Bool =
        let pr = #{
            A(true).
            B(x) :- A(x).
            R(x) :- B(x), B(x).
        };
        let pp = pquery pr select R(true) with {A};
        Vector.map(v -> ematch v { case A(x) => x }, pp) `Assert.eq` Vector#{true, true}

    @Test
    def testPQueryDepth2Int8(): Bool =
        let pr = #{
            A(42i8).
            B(x) :- A(x).
            R(x) :- B(x), B(x).
        };
        let pp = pquery pr select R(42i8) with {A};
        Vector.map(v -> ematch v { case A(x) => x }, pp) `Assert.eq` Vector#{42i8, 42i8}

    @Test
    def testPQueryDepth2Int16(): Bool =
        let pr = #{
            A(1000i16).
            B(x) :- A(x).
            R(x) :- B(x), B(x).
        };
        let pp = pquery pr select R(1000i16) with {A};
        Vector.map(v -> ematch v { case A(x) => x }, pp) `Assert.eq` Vector#{1000i16, 1000i16}

    @Test
    def testPQueryDepth2Int32(): Bool =
        let pr = #{
            A(1).
            B(x) :- A(x).
            R(x) :- B(x), B(x).
        };
        let pp = pquery pr select R(1) with {A};
        Vector.map(v -> ematch v { case A(x) => x }, pp) `Assert.eq` Vector#{1, 1}

    @Test
    def testPQueryDepth2Int64(): Bool =
        let pr = #{
            A(1000000000i64).
            B(x) :- A(x).
            R(x) :- B(x), B(x).
        };
        let pp = pquery pr select R(1000000000i64) with {A};
        Vector.map(v -> ematch v { case A(x) => x }, pp) `Assert.eq` Vector#{1000000000i64, 1000000000i64}

    @Test
    def testPQueryDepth2Float32(): Bool =
        let pr = #{
            A(3.14f32).
            B(x) :- A(x).
            R(x) :- B(x), B(x).
        };
        let pp = pquery pr select R(3.14f32) with {A};
        Vector.map(v -> ematch v { case A(x) => x }, pp) `Assert.eq` Vector#{3.14f32, 3.14f32}

    @Test
    def testPQueryDepth2Float64(): Bool =
        let pr = #{
            A(3.141592653589793f64).
            B(x) :- A(x).
            R(x) :- B(x), B(x).
        };
        let pp = pquery pr select R(3.141592653589793f64) with {A};
        Vector.map(v -> ematch v { case A(x) => x }, pp) `Assert.eq` Vector#{3.141592653589793f64, 3.141592653589793f64}

    @Test
    def testPQueryDepth2String(): Bool =
        let pr = #{
            A("hello").
            B(x) :- A(x).
            R(x) :- B(x), B(x).
        };
        let pp = pquery pr select R("hello") with {A};
        Vector.map(v -> ematch v { case A(x) => x }, pp) `Assert.eq` Vector#{"hello", "hello"}

    @Test
    def testPQueryDepth2Int32Pair(): Bool =
        let pr = #{
            A((1, 2)).
            B(x) :- A(x).
            R(x) :- B(x), B(x).
        };
        let pp = pquery pr select R((1, 2)) with {A};
        Vector.map(v -> ematch v { case A(x) => x }, pp) `Assert.eq` Vector#{(1, 2), (1, 2)}

    ///////////////////////////////////////////////////////////////////////////
    // Provenance for Cycles
    ///////////////////////////////////////////////////////////////////////////

    @Test
    def testPQueryWithCycle01(): Bool =
        let db = #{
            Edge(1, 2). Edge(2, 1).
        };
        let pr = #{
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
            Cycle(x)   :- Path(x, x).
        };
        let pp = pquery db, pr select Cycle(1) with {Edge};
        Vector.map(v -> ematch v { case Edge(x, y) => (x, y) }, pp) `Assert.eq` Vector#{(1, 2), (2, 1)}

    @Test
    def testPQueryWithCycle02(): Bool =
        let db = #{
            Edge(1, 2). Edge(2, 3). Edge(3, 1). Edge(1, 4).
        };
        let pr = #{
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
            Cycle(x)   :- Path(x, x).
        };
        let pp = pquery db, pr select Cycle(2) with {Edge};
        Vector.map(v -> ematch v { case Edge(x, y) => (x, y) }, pp) `Assert.eq` Vector#{(2, 3), (3, 1), (1, 2)}

    @Test
    def testPQueryWithCycle03(): Bool =
        let db = #{
            Edge(1, 2). Edge(2, 3). Edge(3, 4). Edge(4, 5). Edge(5, 2). Edge(1, 6).
        };
        let pr = #{
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
            Cycle(x)   :- Path(x, x).
        };
        let pp = pquery db, pr select Cycle(3) with {Edge};
        Vector.map(v -> ematch v { case Edge(x, y) => (x, y) }, pp) `Assert.eq` Vector#{(3, 4), (4, 5), (5, 2), (2, 3)}

    @Test
    def testPQueryWithCycle04(): Bool =
        let db = #{
            Edge(1, 2). Edge(2, 3). Edge(3, 1). Edge(3, 4). Edge(4, 5). Edge(5, 3).
        };
        let pr = #{
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
            Cycle(x)   :- Path(x, x).
        };
        let pp = pquery db, pr select Cycle(1) with {Edge};
        Vector.map(v -> ematch v { case Edge(x, y) => (x, y) }, pp) `Assert.eq` Vector#{(1, 2), (2, 3), (3, 1)}

    @Test
    def testPQueryWithCycle05(): Bool =
        let db = #{
            Edge(1, 2). Edge(2, 3). Edge(3, 4). Edge(4, 1). Edge(2, 5). Edge(5, 6). Edge(6, 7). Edge(7, 5).
        };
        let pr = #{
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
            Cycle(x)   :- Path(x, x).
        };
        let pp = pquery db, pr select Cycle(2) with {Edge};
        Vector.map(v -> ematch v { case Edge(x, y) => (x, y) }, pp) `Assert.eq` Vector#{(2, 3), (3, 4), (4, 1), (1, 2)}

    ///////////////////////////////////////////////////////////////////////////
    // Provenance for Alternating Paths
    ///////////////////////////////////////////////////////////////////////////

    @Test
    def testPQueryWithAlternatingPath01(): Bool =
        let db = #{
            A(1).
            B(1).
        };
        let pr = #{
            R(x) :- A(x), B(x).
        };
        let pp = pquery db, pr select R(1) with {A, B};
        Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
            case B(x) => "B(${x})"
        }, pp) `Assert.eq` Vector#{"A(1)", "B(1)"}

    @Test
    def testPQueryWithAlternatingPath02(): Bool =
        let db = #{
            A(1).
            B(1).
        };
        let pr = #{
            R(x) :- A(x), B(x), A(x), B(x).
        };
        let pp = pquery db, pr select R(1) with {A, B};
        Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
            case B(x) => "B(${x})"
        }, pp) `Assert.eq` Vector#{"A(1)", "B(1)", "A(1)", "B(1)"}

    @Test
    def testPQueryWithAlternatingPath03(): Bool =
        let db = #{
            A(1).
            B(1).
        };
        let pr = #{
            R(x) :- A(x), B(x), A(x), B(x), A(x), B(x).
        };
        let pp = pquery db, pr select R(1) with {A, B};
        Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
            case B(x) => "B(${x})"
        }, pp) `Assert.eq` Vector#{"A(1)", "B(1)", "A(1)", "B(1)", "A(1)", "B(1)"}

    @Test
    def testPQueryWithAlternatingPath04(): Bool =
        let db = #{
            A(1).
            B(1).
        };
        let pr = #{
            R(x) :- A(x), B(x), A(x), B(x), A(x), B(x), A(x), B(x).
        };
        let pp = pquery db, pr select R(1) with {A, B};
        Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
            case B(x) => "B(${x})"
        }, pp) `Assert.eq` Vector#{"A(1)", "B(1)", "A(1)", "B(1)", "A(1)", "B(1)", "A(1)", "B(1)"}

    @Test
    def testPQueryWithAlternatingPath05(): Bool =
        let db = #{
            A(1).
            B(1).
        };
        let pr = #{
            R(x) :- A(x), B(x), A(x), B(x), A(x), B(x), A(x), B(x), A(x), B(x).
        };
        let pp = pquery db, pr select R(1) with {A, B};
        Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
            case B(x) => "B(${x})"
        }, pp) `Assert.eq` Vector#{"A(1)", "B(1)", "A(1)", "B(1)", "A(1)", "B(1)", "A(1)", "B(1)", "A(1)", "B(1)"}

    ///////////////////////////////////////////////////////////////////////////
    // Provenance for a subset of a path
    ///////////////////////////////////////////////////////////////////////////

    @Test
    def testPQueryWithSequence01(): Bool =
        let db = #{
            A(1). A(2). A(3).
            B(1). B(2). B(3).
            C(1). C(2). C(3).
            D(1). D(2). D(3).
        };
        let pr = #{
            R(x) :- A(x), C(x), B(x), D(x), A(x), B(x), C(x), D(x), B(x).
        };
        let pp = pquery db, pr select R(2) with {A, C};
        Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
            case C(x) => "C(${x})"
        }, pp) `Assert.eq` Vector#{"A(2)", "C(2)", "A(2)", "C(2)"}

    @Test
    def testPQueryWithSequence02(): Bool =
        let db = #{
            A(1). A(2). A(3).
            B(1). B(2). B(3).
            C(1). C(2). C(3).
            D(1). D(2). D(3).
        };
        let pr = #{
            R(x) :- D(x), B(x), A(x), C(x), D(x), C(x), A(x).
        };
        let pp = pquery db, pr select R(1) with {B, D};
        Vector.map(v -> ematch v {
            case B(x) => "B(${x})"
            case D(x) => "D(${x})"
        }, pp) `Assert.eq` Vector#{"D(1)", "B(1)", "D(1)"}

    @Test
    def testPQueryWithSequence03(): Bool =
        let db = #{
            A(1). A(2). A(3).
            B(1). B(2). B(3).
            C(1). C(2). C(3).
            D(1). D(2). D(3).
        };
        let pr = #{
            R(x) :- C(x), A(x), D(x), B(x), B(x), C(x), D(x), A(x), C(x), B(x).
        };
        let pp = pquery db, pr select R(3) with {C, D};
        Vector.map(v -> ematch v {
            case C(x) => "C(${x})"
            case D(x) => "D(${x})"
        }, pp) `Assert.eq` Vector#{"C(3)", "D(3)", "C(3)", "D(3)", "C(3)"}

    @Test
    def testPQueryWithSequence04(): Bool =
        let db = #{
            A(1). A(2). A(3).
            B(1). B(2). B(3).
            C(1). C(2). C(3).
            D(1). D(2). D(3).
        };
        let pr = #{
            R(x) :- B(x), D(x), A(x), C(x), B(x), A(x).
        };
        let pp = pquery db, pr select R(1) with {A};
        Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
        }, pp) `Assert.eq` Vector#{"A(1)", "A(1)"}

    @Test
    def testPQueryWithSequence05(): Bool =
        let db = #{
            A(1). A(2). A(3).
            B(1). B(2). B(3).
            C(1). C(2). C(3).
            D(1). D(2). D(3).
        };
        let pr = #{
            R(x) :- A(x), A(x), B(x), D(x), C(x), D(x), C(x), B(x), A(x), D(x), B(x), C(x).
        };
        let pp = pquery db, pr select R(2) with {B, C, D};
        Vector.map(v -> ematch v {
            case B(x) => "B(${x})"
            case C(x) => "C(${x})"
            case D(x) => "D(${x})"
        }, pp) `Assert.eq` Vector#{"B(2)", "D(2)", "C(2)", "D(2)", "C(2)", "B(2)", "D(2)", "B(2)", "C(2)"}

    ///////////////////////////////////////////////////////////////////////////
    // Provenance for a subset of a tree
    ///////////////////////////////////////////////////////////////////////////

    @Test
    def testPQueryWithTree01(): Bool =
        let db = #{
            A(1). A(2). A(3).
            B(1). B(2). B(3).
            C(1). C(2). C(3).
        };
        let pr = #{
            Q(x) :- A(x), B(x), C(x).
            R(0) :- Q(1), Q(2), Q(3).
        };
        let pp = pquery db, pr select R(0) with {A, B, C};
        Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
            case B(x) => "B(${x})"
            case C(x) => "C(${x})"
        }, pp) `Assert.eq` Vector#{"A(1)", "B(1)", "C(1)", "A(2)", "B(2)", "C(2)", "A(3)", "B(3)", "C(3)"}

    @Test
    def testPQueryWithRandomTree02(): Bool =
        let db = #{
            A(1). A(2). A(3).
            B(1). B(2). B(3).
            C(1). C(2). C(3).
        };
        let pr = #{
            Q(x) :- A(x), B(x), C(x).
            R(0) :- Q(1), Q(2), Q(3).
        };
        let pp = pquery db, pr select R(0) with {A, B};
        Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
            case B(x) => "B(${x})"
        }, pp) `Assert.eq` Vector#{"A(1)", "B(1)", "A(2)", "B(2)", "A(3)", "B(3)"}

    @Test
    def testPQueryWithRandomTree03(): Bool =
        let db = #{
            A(1). A(2). A(3).
            B(1). B(2). B(3).
            C(1). C(2). C(3).
        };
        let pr = #{
            Q(x) :- A(x), B(x), C(x).
            R(0) :- Q(1), Q(2), Q(3).
        };
        let pp = pquery db, pr select R(0) with {A, C};
        Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
            case C(x) => "C(${x})"
        }, pp) `Assert.eq` Vector#{"A(1)", "C(1)", "A(2)", "C(2)", "A(3)", "C(3)"}

    @Test
    def testPQueryWithRandomTree04(): Bool =
        let db = #{
            A(1). A(2). A(3).
            B(1). B(2). B(3).
            C(1). C(2). C(3).
        };
        let pr = #{
            Q(x) :- A(x), B(x), C(x).
            R(0) :- Q(1), Q(2), Q(3).
        };
        let pp = pquery db, pr select R(0) with {B, C};
        Vector.map(v -> ematch v {
            case B(x) => "B(${x})"
            case C(x) => "C(${x})"
        }, pp) `Assert.eq` Vector#{"B(1)", "C(1)", "B(2)", "C(2)", "B(3)", "C(3)"}

    @Test
    def testPQueryWithRandomTree05(): Bool =
        let db = #{
            A(1). A(2). A(3).
            B(1). B(2). B(3).
            C(1). C(2). C(3).
        };
        let pr = #{
            Q(x) :- A(x), B(x), C(x).
            R(0) :- Q(1), Q(2), Q(3).
        };
        let pp = pquery db, pr select R(0) with {A};
        Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
        }, pp) `Assert.eq` Vector#{"A(1)", "A(2)", "A(3)"}

    @Test
    def testPQueryWithRandomTree06(): Bool =
        let db = #{
            A(1). A(2). A(3).
            B(1). B(2). B(3).
            C(1). C(2). C(3).
        };
        let pr = #{
            Q(x) :- A(x), B(x), C(x).
            R(0) :- Q(1), Q(2), Q(3).
        };
        let pp = pquery db, pr select R(0) with {B};
        Vector.map(v -> ematch v {
            case B(x) => "B(${x})"
        }, pp) `Assert.eq` Vector#{"B(1)", "B(2)", "B(3)"}

    @Test
    def testPQueryWithRandomTree07(): Bool =
        let db = #{
            A(1). A(2). A(3).
            B(1). B(2). B(3).
            C(1). C(2). C(3).
        };
        let pr = #{
            Q(x) :- A(x), B(x), C(x).
            R(0) :- Q(1), Q(2), Q(3).
        };
        let pp = pquery db, pr select R(0) with {C};
        Vector.map(v -> ematch v {
            case C(x) => "C(${x})"
        }, pp) `Assert.eq` Vector#{"C(1)", "C(2)", "C(3)"}

}
