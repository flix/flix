mod Test.Exp.Fixpoint.PQuery {

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

    /////////////////////////////////////////////////////////////////////////////
    // Test that `pquery` result type has open row                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testPQueryOpenRow01(): Bool =
        let pr = #{
            A(1, 2).
        };
        let pp = pquery pr select A(1, 2) with {A};
        Vector.map(v -> ematch v {
            case A(x, y) => x + y
            case B(x) => x
        }, pp) == Vector#{3}

    @Test
    def testPQueryOpenRow02(): Bool =
        let pr = #{
            A(1, 2).
            B(4).
        };
        let pp = pquery pr select B(4) with {B};
        Vector.map(v -> ematch v {
            case A(x, y) => x + y
            case B(x) => x
        }, pp) == Vector#{4}

    @Test
    def testPQueryOpenRow03(): Bool =
        let pr = #{
            A(1, 2).
            B(4).
        };
        let pp = pquery pr select A(1, 2) with {A};
        Vector.map(v -> ematch v {
            case A(x, y) => x + y
            case B(x) => x
            case C(x, y, z) => x + y + z
        }, pp) == Vector#{3}

}
