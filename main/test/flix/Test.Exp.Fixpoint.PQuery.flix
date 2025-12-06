mod Test.Exp.Fixpoint.PQuery {

    use Assert.assertEq;

    ///////////////////////////////////////////////////////////////////////////
    // Provenance at depth 1
    ///////////////////////////////////////////////////////////////////////////

    @Test
    def testPQueryUnit(): Unit \ Assert =
        let pr = #{
            A(()).
        };
        let pp = pquery pr select A(()) with {A};
        let expected = Vector#{()};
        let actual = Vector.map(v -> ematch v { case A(x) => x }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryBool(): Unit \ Assert =
        let pr = #{
            A(true).
        };
        let pp = pquery pr select A(true) with {A};
        let expected = Vector#{true};
        let actual = Vector.map(v -> ematch v { case A(x) => x }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryInt8(): Unit \ Assert =
        let pr = #{
            A(42i8).
        };
        let pp = pquery pr select A(42i8) with {A};
        let expected = Vector#{42i8};
        let actual = Vector.map(v -> ematch v { case A(x) => x }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryInt16(): Unit \ Assert =
        let pr = #{
            A(1000i16).
        };
        let pp = pquery pr select A(1000i16) with {A};
        let expected = Vector#{1000i16};
        let actual = Vector.map(v -> ematch v { case A(x) => x }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryInt32(): Unit \ Assert =
        let pr = #{
            A(1).
        };
        let pp = pquery pr select A(1) with {A};
        let expected = Vector#{1};
        let actual = Vector.map(v -> ematch v { case A(x) => x }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryInt64(): Unit \ Assert =
        let pr = #{
            A(1000000000i64).
        };
        let pp = pquery pr select A(1000000000i64) with {A};
        let expected = Vector#{1000000000i64};
        let actual = Vector.map(v -> ematch v { case A(x) => x }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryFloat32(): Unit \ Assert =
        let pr = #{
            A(3.14f32).
        };
        let pp = pquery pr select A(3.14f32) with {A};
        let expected = Vector#{3.14f32};
        let actual = Vector.map(v -> ematch v { case A(x) => x }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryFloat64(): Unit \ Assert =
        let pr = #{
            A(3.141592653589793f64).
        };
        let pp = pquery pr select A(3.141592653589793f64) with {A};
        let expected = Vector#{3.141592653589793f64};
        let actual = Vector.map(v -> ematch v { case A(x) => x }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryString(): Unit \ Assert =
        let pr = #{
            A("hello").
        };
        let pp = pquery pr select A("hello") with {A};
        let expected = Vector#{"hello"};
        let actual = Vector.map(v -> ematch v { case A(x) => x }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryInt32Pair(): Unit \ Assert =
        let pr = #{
            A((1, 2)).
        };
        let pp = pquery pr select A((1, 2)) with {A};
        let expected = Vector#{(1, 2)};
        let actual = Vector.map(v -> ematch v { case A(x) => x }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryNullary(): Unit \ Assert =
        let pr = #{
            A.
        };
        let pp = pquery pr select A with {A};
        let expected = Vector#{42};
        let actual = Vector.map(v -> ematch v { case A => 42 }, pp);
        assertEq(expected = expected, actual)

    ///////////////////////////////////////////////////////////////////////////
    // Provenance at depth 2
    ///////////////////////////////////////////////////////////////////////////

    @Test
    def testPQueryDepth2Unit(): Unit \ Assert =
        let pr = #{
            A(()).
            B(x) :- A(x).
            R(x) :- B(x), B(x).
        };
        let pp = pquery pr select R(()) with {A};
        let expected = Vector#{(), ()};
        let actual = Vector.map(v -> ematch v { case A(x) => x }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryDepth2Bool(): Unit \ Assert =
        let pr = #{
            A(true).
            B(x) :- A(x).
            R(x) :- B(x), B(x).
        };
        let pp = pquery pr select R(true) with {A};
        let expected = Vector#{true, true};
        let actual = Vector.map(v -> ematch v { case A(x) => x }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryDepth2Int8(): Unit \ Assert =
        let pr = #{
            A(42i8).
            B(x) :- A(x).
            R(x) :- B(x), B(x).
        };
        let pp = pquery pr select R(42i8) with {A};
        let expected = Vector#{42i8, 42i8};
        let actual = Vector.map(v -> ematch v { case A(x) => x }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryDepth2Int16(): Unit \ Assert =
        let pr = #{
            A(1000i16).
            B(x) :- A(x).
            R(x) :- B(x), B(x).
        };
        let pp = pquery pr select R(1000i16) with {A};
        let expected = Vector#{1000i16, 1000i16};
        let actual = Vector.map(v -> ematch v { case A(x) => x }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryDepth2Int32(): Unit \ Assert =
        let pr = #{
            A(1).
            B(x) :- A(x).
            R(x) :- B(x), B(x).
        };
        let pp = pquery pr select R(1) with {A};
        let expected = Vector#{1, 1};
        let actual = Vector.map(v -> ematch v { case A(x) => x }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryDepth2Int64(): Unit \ Assert =
        let pr = #{
            A(1000000000i64).
            B(x) :- A(x).
            R(x) :- B(x), B(x).
        };
        let pp = pquery pr select R(1000000000i64) with {A};
        let expected = Vector#{1000000000i64, 1000000000i64};
        let actual = Vector.map(v -> ematch v { case A(x) => x }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryDepth2Float32(): Unit \ Assert =
        let pr = #{
            A(3.14f32).
            B(x) :- A(x).
            R(x) :- B(x), B(x).
        };
        let pp = pquery pr select R(3.14f32) with {A};
        let expected = Vector#{3.14f32, 3.14f32};
        let actual = Vector.map(v -> ematch v { case A(x) => x }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryDepth2Float64(): Unit \ Assert =
        let pr = #{
            A(3.141592653589793f64).
            B(x) :- A(x).
            R(x) :- B(x), B(x).
        };
        let pp = pquery pr select R(3.141592653589793f64) with {A};
        let expected = Vector#{3.141592653589793f64, 3.141592653589793f64};
        let actual = Vector.map(v -> ematch v { case A(x) => x }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryDepth2String(): Unit \ Assert =
        let pr = #{
            A("hello").
            B(x) :- A(x).
            R(x) :- B(x), B(x).
        };
        let pp = pquery pr select R("hello") with {A};
        let expected = Vector#{"hello", "hello"};
        let actual = Vector.map(v -> ematch v { case A(x) => x }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryDepth2Int32Pair(): Unit \ Assert =
        let pr = #{
            A((1, 2)).
            B(x) :- A(x).
            R(x) :- B(x), B(x).
        };
        let pp = pquery pr select R((1, 2)) with {A};
        let expected = Vector#{(1, 2), (1, 2)};
        let actual = Vector.map(v -> ematch v { case A(x) => x }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryDepth2Nullary01(): Unit \ Assert =
        let pr = #{
            A.
            B :- A.
            R :- B, B.
        };
        let pp = pquery pr select R with {A};
        let expected = Vector#{42, 42};
        let actual = Vector.map(v -> ematch v { case A => 42 }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryDepth2Nullary02(): Unit \ Assert =
        // We use negative dependencies because it forces the use of `memberOf`.
        // Positive dependencies would simply translate to foreach loop.
        let pr = #{
            A.
            B :- not A.
            C :- not B.
        };

        let pp = pquery pr select C with {A, B, C};
        let expected = Vector#{'C'};
        let actual = Vector.map(v -> ematch v {
            case A => 'A'
            case B => 'B'
            case C => 'C'
        }, pp);
        assertEq(expected = expected, actual)

    ///////////////////////////////////////////////////////////////////////////
    // Provenance for cycles
    ///////////////////////////////////////////////////////////////////////////

    @Test
    def testPQueryWithCycle01(): Unit \ Assert =
        let db = #{
            Edge(1, 2). Edge(2, 1).
        };
        let pr = #{
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
            Cycle(x)   :- Path(x, x).
        };
        let pp = pquery db, pr select Cycle(1) with {Edge};
        let expected = Vector#{(1, 2), (2, 1)};
        let actual = Vector.map(v -> ematch v { case Edge(x, y) => (x, y) }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryWithCycle02(): Unit \ Assert =
        let db = #{
            Edge(1, 2). Edge(2, 3). Edge(3, 1). Edge(1, 4).
        };
        let pr = #{
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
            Cycle(x)   :- Path(x, x).
        };
        let pp = pquery db, pr select Cycle(2) with {Edge};
        let expected = Vector#{(2, 3), (3, 1), (1, 2)};
        let actual = Vector.map(v -> ematch v { case Edge(x, y) => (x, y) }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryWithCycle03(): Unit \ Assert =
        let db = #{
            Edge(1, 2). Edge(2, 3). Edge(3, 4). Edge(4, 5). Edge(5, 2). Edge(1, 6).
        };
        let pr = #{
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
            Cycle(x)   :- Path(x, x).
        };
        let pp = pquery db, pr select Cycle(3) with {Edge};
        let expected = Vector#{(3, 4), (4, 5), (5, 2), (2, 3)};
        let actual = Vector.map(v -> ematch v { case Edge(x, y) => (x, y) }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryWithCycle04(): Unit \ Assert =
        let db = #{
            Edge(1, 2). Edge(2, 3). Edge(3, 1). Edge(3, 4). Edge(4, 5). Edge(5, 3).
        };
        let pr = #{
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
            Cycle(x)   :- Path(x, x).
        };
        let pp = pquery db, pr select Cycle(1) with {Edge};
        let expected = Vector#{(1, 2), (2, 3), (3, 1)};
        let actual = Vector.map(v -> ematch v { case Edge(x, y) => (x, y) }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryWithCycle05(): Unit \ Assert =
        let db = #{
            Edge(1, 2). Edge(2, 3). Edge(3, 4). Edge(4, 1). Edge(2, 5). Edge(5, 6). Edge(6, 7). Edge(7, 5).
        };
        let pr = #{
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
            Cycle(x)   :- Path(x, x).
        };
        let pp = pquery db, pr select Cycle(2) with {Edge};
        let expected = Vector#{(2, 3), (3, 4), (4, 1), (1, 2)};
        let actual = Vector.map(v -> ematch v { case Edge(x, y) => (x, y) }, pp);
        assertEq(expected = expected, actual)

    ///////////////////////////////////////////////////////////////////////////
    // Provenance for alternating paths
    ///////////////////////////////////////////////////////////////////////////

    @Test
    def testPQueryWithAlternatingPath01(): Unit \ Assert =
        let db = #{
            A(1).
            B(1).
        };
        let pr = #{
            R(x) :- A(x), B(x).
        };
        let pp = pquery db, pr select R(1) with {A, B};
        let expected = Vector#{"A(1)", "B(1)"};
        let actual = Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
            case B(x) => "B(${x})"
        }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryWithAlternatingPath02(): Unit \ Assert =
        let db = #{
            A(1).
            B(1).
        };
        let pr = #{
            R(x) :- A(x), B(x), A(x), B(x).
        };
        let pp = pquery db, pr select R(1) with {A, B};
        let expected = Vector#{"A(1)", "B(1)", "A(1)", "B(1)"};
        let actual = Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
            case B(x) => "B(${x})"
        }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryWithAlternatingPath03(): Unit \ Assert =
        let db = #{
            A(1).
            B(1).
        };
        let pr = #{
            R(x) :- A(x), B(x), A(x), B(x), A(x), B(x).
        };
        let pp = pquery db, pr select R(1) with {A, B};
        let expected = Vector#{"A(1)", "B(1)", "A(1)", "B(1)", "A(1)", "B(1)"};
        let actual = Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
            case B(x) => "B(${x})"
        }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryWithAlternatingPath04(): Unit \ Assert =
        let db = #{
            A(1).
            B(1).
        };
        let pr = #{
            R(x) :- A(x), B(x), A(x), B(x), A(x), B(x), A(x), B(x).
        };
        let pp = pquery db, pr select R(1) with {A, B};
        let expected = Vector#{"A(1)", "B(1)", "A(1)", "B(1)", "A(1)", "B(1)", "A(1)", "B(1)"};
        let actual = Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
            case B(x) => "B(${x})"
        }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryWithAlternatingPath05(): Unit \ Assert =
        let db = #{
            A(1).
            B(1).
        };
        let pr = #{
            R(x) :- A(x), B(x), A(x), B(x), A(x), B(x), A(x), B(x), A(x), B(x).
        };
        let pp = pquery db, pr select R(1) with {A, B};
        let expected = Vector#{"A(1)", "B(1)", "A(1)", "B(1)", "A(1)", "B(1)", "A(1)", "B(1)", "A(1)", "B(1)"};
        let actual = Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
            case B(x) => "B(${x})"
        }, pp);
        assertEq(expected = expected, actual)

    ///////////////////////////////////////////////////////////////////////////
    // Provenance for a subset of a path
    ///////////////////////////////////////////////////////////////////////////

    @Test
    def testPQueryWithSequence01(): Unit \ Assert =
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
        let expected = Vector#{"A(2)", "C(2)", "A(2)", "C(2)"};
        let actual = Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
            case C(x) => "C(${x})"
        }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryWithSequence02(): Unit \ Assert =
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
        let expected = Vector#{"D(1)", "B(1)", "D(1)"};
        let actual = Vector.map(v -> ematch v {
            case B(x) => "B(${x})"
            case D(x) => "D(${x})"
        }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryWithSequence03(): Unit \ Assert =
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
        let expected = Vector#{"C(3)", "D(3)", "C(3)", "D(3)", "C(3)"};
        let actual = Vector.map(v -> ematch v {
            case C(x) => "C(${x})"
            case D(x) => "D(${x})"
        }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryWithSequence04(): Unit \ Assert =
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
        let expected = Vector#{"A(1)", "A(1)"};
        let actual = Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
        }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryWithSequence05(): Unit \ Assert =
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
        let expected = Vector#{"B(2)", "D(2)", "C(2)", "D(2)", "C(2)", "B(2)", "D(2)", "B(2)", "C(2)"};
        let actual = Vector.map(v -> ematch v {
            case B(x) => "B(${x})"
            case C(x) => "C(${x})"
            case D(x) => "D(${x})"
        }, pp);
        assertEq(expected = expected, actual)

    ///////////////////////////////////////////////////////////////////////////
    // Provenance for a subset of a tree
    ///////////////////////////////////////////////////////////////////////////

    @Test
    def testPQueryWithTree01(): Unit \ Assert =
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
        let expected = Vector#{"A(1)", "B(1)", "C(1)", "A(2)", "B(2)", "C(2)", "A(3)", "B(3)", "C(3)"};
        let actual = Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
            case B(x) => "B(${x})"
            case C(x) => "C(${x})"
        }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryWithRandomTree02(): Unit \ Assert =
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
        let expected = Vector#{"A(1)", "B(1)", "A(2)", "B(2)", "A(3)", "B(3)"};
        let actual = Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
            case B(x) => "B(${x})"
        }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryWithRandomTree03(): Unit \ Assert =
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
        let expected = Vector#{"A(1)", "C(1)", "A(2)", "C(2)", "A(3)", "C(3)"};
        let actual = Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
            case C(x) => "C(${x})"
        }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryWithRandomTree04(): Unit \ Assert =
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
        let expected = Vector#{"B(1)", "C(1)", "B(2)", "C(2)", "B(3)", "C(3)"};
        let actual = Vector.map(v -> ematch v {
            case B(x) => "B(${x})"
            case C(x) => "C(${x})"
        }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryWithRandomTree05(): Unit \ Assert =
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
        let expected = Vector#{"A(1)", "A(2)", "A(3)"};
        let actual = Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
        }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryWithRandomTree06(): Unit \ Assert =
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
        let expected = Vector#{"B(1)", "B(2)", "B(3)"};
        let actual = Vector.map(v -> ematch v {
            case B(x) => "B(${x})"
        }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryWithRandomTree07(): Unit \ Assert =
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
        let expected = Vector#{"C(1)", "C(2)", "C(3)"};
        let actual = Vector.map(v -> ematch v {
            case C(x) => "C(${x})"
        }, pp);
        assertEq(expected = expected, actual)

    ///////////////////////////////////////////////////////////////////////////
    // Provenance computation on a path
    ///////////////////////////////////////////////////////////////////////////

    @Test
    def testPQueryWithPathCost01(): Unit \ Assert =
        let db = #{
            Edge("a", "b", 3).
            Edge("b", "c", 1).
            Edge("c", "d", 2).
        };
        let pr = #{
            Path(x, y) :- Edge(x, y, _).
            Path(x, z) :- Path(x, y), Edge(y, z, _).
        };
        let pp = pquery db, pr select Path("a", "d") with {Edge};
        let expected = 6;
        let actual = Vector.sumWith(v -> ematch v {
            case Edge(_, _, c) => c
        }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryWithPathCost02(): Unit \ Assert =
        let db = #{
            Edge("a", "b", 2).
            Edge("b", "c", 4).
            Edge("c", "d", 3).
            Edge("d", "e", 1).
            Edge("a", "x", 5).
            Edge("b", "y", 6).
            Edge("c", "z", 2).
            Edge("e", "w", 7).
        };
        let pr = #{
            Path(x, y) :- Edge(x, y, _).
            Path(x, z) :- Path(x, y), Edge(y, z, _).
        };
        let pp = pquery db, pr select Path("a", "e") with {Edge};
        let expected = 10;
        let actual = Vector.sumWith(v -> ematch v {
            case Edge(_, _, c) => c
        }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryWithPathCost03(): Unit \ Assert =
        let db = #{
            Edge("a", "b", 3).
            Edge("b", "c", 2).
            Edge("c", "d", 4).
            Edge("d", "f", 1).
            Edge("f", "g", 2).
            Edge("g", "e", 3).
            Edge("a", "x", 5).
            Edge("x", "y", 2).
            Edge("y", "x", 1).
            Edge("b", "z", 6).
            Edge("z", "w", 3).
            Edge("w", "z", 2).
            Edge("c", "p", 4).
            Edge("p", "q", 1).
            Edge("q", "r", 2).
            Edge("r", "p", 3).
            Edge("d", "m", 2).
            Edge("f", "n", 7).
            Edge("g", "s", 1).
            Edge("e", "t", 4).
        };
        let pr = #{
            Path(x, y) :- Edge(x, y, _).
            Path(x, z) :- Path(x, y), Edge(y, z, _).
        };
        let pp = pquery db, pr select Path("a", "e") with {Edge};
        let expected = 15;
        let actual = Vector.sumWith(v -> ematch v {
            case Edge(_, _, c) => c
        }, pp);
        assertEq(expected = expected, actual)

    ///////////////////////////////////////////////////////////////////////////
    // Provenance with functions and filters.
    ///////////////////////////////////////////////////////////////////////////

    @Test
    def testPQueryWithAppAndFilter01(): Unit \ Assert =
        let db = #{
            A(1). A(3). A(5).
            B(2). B(7). B(11).
        };
        let pr = #{
            R(x * y) :- A(x), B(y), if (x <= y).
        };
        let pp = pquery db, pr select R(21) with {A, B};
        let expected = Vector#{"A(3)", "B(7)"};
        let actual = Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
            case B(x) => "B(${x})"
        }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryWithAppAndFilter02(): Unit \ Assert =
        let db = #{
            A(2). A(4). A(6).
            B(3). B(8). B(12).
        };
        let pr = #{
            R(x * y) :- A(x), B(y), if (x <= y).
        };
        let pp = pquery db, pr select R(32) with {A, B};
        let expected = Vector#{"A(4)", "B(8)"};
        let actual = Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
            case B(x) => "B(${x})"
        }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryWithAppAndFilter03(): Unit \ Assert =
        let db = #{
            A(2). A(4). A(6).
            B(3). B(7). B(12).
            C(5). C(8). C(15).
        };
        let pr = #{
            R(x + y + z) :- A(x), B(y), C(z), if (x <= y), if (y <= z).
        };
        let pp = pquery db, pr select R(19) with {A, B, C};
        let expected = Vector#{"A(4)", "B(7)", "C(8)"};
        let actual = Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
            case B(x) => "B(${x})"
            case C(x) => "C(${x})"
        }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryWithAppAndFilter04(): Unit \ Assert =
        let db = #{
            A(1). A(3). A(8).
            B(4). B(9). B(14).
            C(6). C(10). C(18).
        };
        let pr = #{
            R(x * y - z) :- A(x), B(y), C(z), if (x < y), if (z > x).
        };
        let pp = pquery db, pr select R(21) with {A, B, C};
        let expected = Vector#{"A(3)", "B(9)", "C(6)"};
        let actual = Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
            case B(x) => "B(${x})"
            case C(x) => "C(${x})"
        }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryWithAppAndFilter05(): Unit \ Assert =
        let db = #{
            A(2). A(5). A(9).
            B(3). B(8). B(13).
            C(4). C(11). C(20).
        };
        let pr = #{
            R(x * y + z) :- A(x), B(y), C(z), if (x <= y), if (z >= y), if (x + z > 10).
        };
        let pp = pquery db, pr select R(51) with {A, B, C};
        let expected = Vector#{"A(5)", "B(8)", "C(11)"};
        let actual = Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
            case B(x) => "B(${x})"
            case C(x) => "C(${x})"
        }, pp);
        assertEq(expected = expected, actual)

    /////////////////////////////////////////////////////////////////////////////
    /// Pquery should have an open row.
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testPQueryOpenRow01(): Unit \ Assert =
        let pr = #{
            A(1, 2).
        };
        let pp = pquery pr select A(1, 2) with {A};
        let expected = Vector#{3};
        let actual = Vector.map(v -> ematch v {
            case A(x, y) => x + y
            case B(x)    => x
        }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryOpenRow02(): Unit \ Assert =
        let pr = #{
            A(1, 2).
            B(4).
        };
        let pp = pquery pr select B(4) with {B};
        let expected = Vector#{4};
        let actual = Vector.map(v -> ematch v {
            case A(x, y) => x + y
            case B(x)    => x
        }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryOpenRow03(): Unit \ Assert =
        let pr = #{
            A(1, 2).
            B(4).
        };
        let pp = pquery pr select A(1, 2) with {A};
        let expected = Vector#{3};
        let actual = Vector.map(v -> ematch v {
            case A(x, y)    => x + y
            case B(x)       => x
            case C(x, y, z) => x + y + z
        }, pp);
        assertEq(expected = expected, actual)

    /////////////////////////////////////////////////////////////////////////////
    /// Term types are ordered correctly.
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testPQueryTermTypes01(): Unit \ Assert =
        let pr = #{
            A(1i32, 'c').
            B('c').
            R(x, y) :- A(x, y), B(y).
        };
        let pp = pquery pr select R(1i32, 'c') with {A};
        let res = Vector.map(v -> ematch v {
            case A(x, y) => (x, y)
        }, pp);
        let expected = Vector#{(1i32, 'c')};
        assertEq(expected = expected, res)

    @Test
    def testPQueryTermTypes02(): Unit \ Assert =
        let pr = #{
            A(1i32, 'c', 2.4f64).
            B('c').
            R(x, y) :- A(x, y, z), B(y).
        };
        let pp = pquery pr select R(1i32, 'c') with {A};
        let res = Vector.map(v -> ematch v {
            case A(x, y, z) => (x, y, z)
        }, pp);
        let expected = Vector#{(1i32, 'c', 2.4f64)};
        assertEq(expected = expected, res)

    @Test
    def testPQueryTermTypes03(): Unit \ Assert =
        let pr = #{
            A(1i32, 2i64, 3.4f32, 5.6f64, 6i8, 7i16, "Hello", 'c', true).
        };
        let pp = pquery pr select A(1i32, 2i64, 3.4f32, 5.6f64, 6i8, 7i16, "Hello", 'c', true) with {A};
        let res = Vector.map(v -> ematch v {
            case A(x, y, z, w, u, t, s, r, q) => (x, y, z, w, u, t, s, r, q)
        }, pp);
        let expected = Vector#{(1i32, 2i64, 3.4f32, 5.6f64, 6i8, 7i16, "Hello", 'c', true)};
        assertEq(expected = expected, res)

    /////////////////////////////////////////////////////////////////////////////
    /// `pquery` should track provenance through rho abstraction.
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testPQueryWithRhoAbstraction01(): Unit \ Assert =
        let db = #{
            A(1). A(2). A(3).
        };
        let pr = #(A, R) -> #{
            Tmp(x) :- A(x).
            R(x) :- Tmp(x).
        };
        let pp = pquery db, pr select R(2) with { A };
        let result = Vector.map(v -> ematch v { case A(x) => "A(${x})" }, pp);
        let expected = Vector#{"A(2)"};
        assertEq(expected = expected, result)

    @Test
    def testPQueryWithRhoAbstraction02(): Unit \ Assert =
        let db = #{
            A(1). A(2). A(3).
            B(1). B(2). B(3).
        };
        let pr = #(A, B, R) -> #{
            Tmp(x) :- A(x), B(x).
            R(x) :- Tmp(x).
        };
        let pp = pquery db, pr select R(2) with { A, B };
        let result = Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
            case B(x) => "B(${x})"
        }, pp);
        let expected = Vector#{"A(2)", "B(2)"};
        assertEq(expected = expected, result)

    /////////////////////////////////////////////////////////////////////////////
    /// `pquery` should support functional predicates.
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testPQueryWithFunctionalPredicate01(): Unit \ Assert =
        def f(n) = Vector.range(n, 2 * n);
        let db = #{
            A(1). A(2). A(3).
        };
        let pr = #{
            R(y) :- A(x), let y = f(x).
        };
        let pp = pquery db, pr select R(4) with {A};
        let expected = Vector#{"A(3)"};
        let actual = Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
        }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryWithFunctionalPredicate02(): Unit \ Assert =
        def f(n) = Vector.range(n, 2 * n);
        let db = #{
            A(1). A(2). A(3).
            B(1). B(2). B(3).
        };
        let pr = #{
            R(y) :- A(x), B(x), let y = f(x).
        };
        let pp = pquery db, pr select R(4) with {A, B};
        let expected = Vector#{"A(3)", "B(3)"};
        let actual = Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
            case B(x) => "B(${x})"
        }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryWithFunctionalPredicate03(): Unit \ Assert =
        def f(n) = Vector.range(n, 2 * n);
        let db = #{
            A(1). A(2). A(3).
            B(1). B(2). B(3).
        };
        let pr = #{
            R(z1, z2) :- A(x), B(y), let z1 = f(x), let z2 = f(y).
        };
        let pp = pquery db, pr select R(4, 4) with {A, B};
        let expected = Vector#{"A(3)", "B(3)"};
        let actual = Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
            case B(x) => "B(${x})"
        }, pp);
        assertEq(expected = expected, actual)


    @Test
    def testPQueryWithFunctionalPredicate04(): Unit \ Assert = {
        let db = #{
            A(1). A(3). A(5).
        };
        let pr = #{
            R(x * y) :- A(x), let y = Vector#{2, 7, 11}, if (x <= y).
        };
        let pp = pquery db, pr select R(21) with {A};
        let expected = Vector#{"A(3)"};
        let actual = Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
        }, pp);
        assertEq(expected = expected, actual)
    }

    @Test
    def testPQueryWithFunctionalPredicate05(): Unit \ Assert = {
        let db = #{
            A(1). A(3). A(5).
        };
        let pr = #{
            R(x + y) :- A(x), let y = Vector#{x}.
        };
        let pp = pquery db, pr select R(2) with {A};
        let expected = Vector#{"A(1)"};
        let actual = Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
        }, pp);
        assertEq(expected = expected, actual)
    }

    @Test
    def testPQueryWithFunctionalPredicate06(): Unit \ Assert = {
        let db = #{
            A(1). A(3). A(5).
        };
        let pr = #{
            R(x + y) :- A(x), let y = Vector#{x}.
        };
        let pp = pquery db, pr select R(6) with {A};
        let expected = Vector#{"A(3)"};
        let actual = Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
        }, pp);
        assertEq(expected = expected, actual)
    }

    @Test
    def testPQueryWithFunctionalPredicate07(): Unit \ Assert = {
        let db = #{
            A(1). A(3). A(5).
        };
        let pr = #{
            R(x + y) :- A(x), let y = Vector#{x}.
        };
        let pp = pquery db, pr select R(10) with {A};
        let expected = Vector#{"A(5)"};
        let actual = Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
        }, pp);
        assertEq(expected = expected, actual)
    }

    @Test
    def testPQueryWithFunctionalPredicate08(): Unit \ Assert = {
        let db = #{
            A(1). A(3). A(5).
        };
        let pr = #{
            R(10) :- A(x), let y = Vector#{3}, if (y + x > 7).
        };
        let pp = pquery db, pr select R(10) with {A};
        let expected = Vector#{"A(5)"};
        let actual = Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
        }, pp);
        assertEq(expected = expected, actual)
    }

    @Test
    def testPQueryWithFunctionalPredicate09(): Unit \ Assert = {
        let db = #{
            A(1). A(3). A(5).
        };
        let pr = #{
            R(10) :- A(x), let y = Vector#{3}, if (y + x < 5).
        };
        let pp = pquery db, pr select R(10) with {A};
        let expected = Vector#{"A(1)"};
        let actual = Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
        }, pp);
        assertEq(expected = expected, actual)
    }

    @Test
    def testPQueryWithFunctionalPredicate10(): Unit \ Assert = {
        let db = #{
            A(1). A(3). A(5).
        };
        let pr = #{
            R(10) :- A(x), let y = Vector#{3}, if (y + x > 5 and x + y < 7).
        };
        let pp = pquery db, pr select R(10) with {A};
        let expected = Vector#{"A(3)"};
        let actual = Vector.map(v -> ematch v {
            case A(x) => "A(${x})"
        }, pp);
        assertEq(expected = expected, actual)
    }

    /////////////////////////////////////////////////////////////////////////////
    /// Provenance should compute the shortest path.
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testPQueryShortestPath01(): Unit \ Assert = {
        let db = #{
            A(1). A(2). A(3). A(4). A(5).
        };
        let pr = #{
            B(1) :- not A(-1).
            B(x + 1) :- B(x), A(x).
            C(-1) :- not B(-2).
            C(x) :- B(x).
            C(x) :- A(x), C(-1).
        };
        let pp = pquery db, pr select C(5) with {A, B, C};
        let expected = Vector#{5, 5, -1};
        let actual = Vector.map(v -> ematch v {
            case A(x) => x
            case B(x) => x
            case C(x) => x
        }, pp);
        assertEq(expected = expected, actual)
    }

    @Test
    def testPQueryShortestPath02(): Unit \ Assert = {
        let db = #{
            A(1). A(2). A(3). A(4). A(5).
        };
        let pr = #{
            B(1) :- not A(-1).
            B(x + 1) :- B(x), A(x).
            C(-1) :- not B(-2).
            C(x) :- B(x).
            C(x) :- A(x), C(-1).
        };
        let pp = pquery db, pr select C(4) with {A, B, C};
        let expected = Vector#{4, 4, -1};
        let actual = Vector.map(v -> ematch v {
            case A(x) => x
            case B(x) => x
            case C(x) => x
        }, pp);
        assertEq(expected = expected, actual)
    }

    /////////////////////////////////////////////////////////////////////////////
    /// `pquery` should work with negative dependencies.
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testPQueryNegative01(): Unit \ Assert =
        let pr = #{
            B(1) :- not A(1).
        };

        let pp = pquery pr select B(1) with {B};
        let expected = Vector#{0};
        let actual = Vector.map(v -> ematch v {
            case B(_) => 0
        }, pp);
        assertEq(expected = expected, actual)

    @Test
    def testPQueryNegative02(): Unit \ Assert =
        let pr = #{
            A(1).
            B(1) :- not A(1).
            C(1) :- not B(1).
        };

        let pp = pquery pr select C(1) with {A, B, C};
        let expected = Vector#{1};
        let actual = Vector.map(v -> ematch v {
            case A(_) => -1
            case B(_) => 0
            case C(_) => 1
        }, pp);
        assertEq(expected = expected, actual)

}
