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
        Vector.map(v -> ematch v { case A(x) => x }, pp) == Vector#{()}

    @Test
    def testPQueryBool(): Bool =
        let pr = #{
            A(true).
        };
        let pp = pquery pr select A(true) with {A};
        Vector.map(v -> ematch v { case A(x) => x }, pp) == Vector#{true}

    @Test
    def testPQueryInt8(): Bool =
        let pr = #{
            A(42i8).
        };
        let pp = pquery pr select A(42i8) with {A};
        Vector.map(v -> ematch v { case A(x) => x }, pp) == Vector#{42i8}

    @Test
    def testPQueryInt16(): Bool =
        let pr = #{
            A(1000i16).
        };
        let pp = pquery pr select A(1000i16) with {A};
        Vector.map(v -> ematch v { case A(x) => x }, pp) == Vector#{1000i16}

    @Test
    def testPQueryInt32(): Bool =
        let pr = #{
            A(1).
        };
        let pp = pquery pr select A(1) with {A};
        Vector.map(v -> ematch v { case A(x) => x }, pp) == Vector#{1}

    @Test
    def testPQueryInt64(): Bool =
        let pr = #{
            A(1000000000i64).
        };
        let pp = pquery pr select A(1000000000i64) with {A};
        Vector.map(v -> ematch v { case A(x) => x }, pp) == Vector#{1000000000i64}

    @Test
    def testPQueryFloat32(): Bool =
        let pr = #{
            A(3.14f32).
        };
        let pp = pquery pr select A(3.14f32) with {A};
        Vector.map(v -> ematch v { case A(x) => x }, pp) == Vector#{3.14f32}

    @Test
    def testPQueryFloat64(): Bool =
        let pr = #{
            A(3.141592653589793f64).
        };
        let pp = pquery pr select A(3.141592653589793f64) with {A};
        Vector.map(v -> ematch v { case A(x) => x }, pp) == Vector#{3.141592653589793f64}

    @Test
    def testPQueryString(): Bool =
        let pr = #{
            A("hello").
        };
        let pp = pquery pr select A("hello") with {A};
        Vector.map(v -> ematch v { case A(x) => x }, pp) == Vector#{"hello"}

    @Test
    def testPQueryInt32Pair(): Bool =
        let pr = #{
            A((1, 2)).
        };
        let pp = pquery pr select A((1, 2)) with {A};
        Vector.map(v -> ematch v { case A(x) => x }, pp) == Vector#{(1, 2)}

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

}
