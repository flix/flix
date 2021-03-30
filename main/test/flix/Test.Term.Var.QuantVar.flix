namespace Test/Term/Var/QuantVar {

    @test
    def testQuantVar01(): Bool =
        let p = #{
            A(1).
            R(x) :- A(x).
        };
        (solve p) |= #{ R(1). }

    @test
    def testQuantVar02(): Bool =
        let p = #{
            A(1, 2).
            R(x, y) :- A(x, y).
        };
        (solve p) |= #{ R(1, 2). }

    @test
    def testQuantVar03(): Bool =
        let p = #{
            A(1, 2, 3).
            R(x, y, z) :- A(x, y, z).
        };
        (solve p) |= #{ R(1, 2, 3). }

    @test
    def testQuantVar04(): Bool =
        let p = #{
            A(1, 2, 3, 4).
            R(x, y, z, w) :- A(x, y, z, w).
        };
        (solve p) |= #{ R(1, 2, 3, 4). }

    @test
    def testQuantVarOverlap01(): Bool =
        let p = #{
            A(1, 2, 3, 4).
            A(4, 3, 2, 1).
            A(1, 1, 1, 1).
            A(2, 2, 2, 2).
            R(x, x, z, w) :- A(x, x, z, w).
        };
        let m = solve p;
        (m |= #{ R(1, 1, 1, 1). }) and not (m |= #{ R(1, 2, 3, 4). })

    @test
    def testQuantVarOverlap02(): Bool =
        let p = #{
            A(1, 2, 3, 4).
            A(4, 3, 2, 1).
            A(1, 1, 1, 1).
            A(2, 2, 2, 2).
            R(x, y, y, w) :- A(x, y, y, w).
        };
        let m = solve p;
        (m |= #{ R(1, 1, 1, 1). }) and not (m |= #{ R(1, 2, 3, 4). })

    @test
    def testQuantVarOverlap03(): Bool =
        let p = #{
            A(1, 2, 3, 4).
            A(4, 3, 2, 1).
            A(1, 1, 1, 1).
            A(2, 2, 2, 2).
            R(x, y, z, z) :- A(x, y, z, z).
        };
        let m = solve p;
        (m |= #{ R(1, 1, 1, 1). }) and not (m |= #{ R(1, 2, 3, 4). })

    @test
    def testQuantVarOverlap04(): Bool =
        let p = #{
            A(1, 2, 3, 4).
            A(4, 3, 2, 1).
            A(1, 1, 1, 1).
            A(2, 2, 2, 2).
            R(x, y, z, x) :- A(x, y, z, x).
        };
        let m = solve p;
        (m |= #{ R(1, 1, 1, 1). }) and not (m |= #{ R(1, 2, 3, 4). })

    @test
    def testQuantVarOverlap05(): Bool =
        let p = #{
            A(1, 2, 3, 4).
            A(4, 3, 2, 1).
            A(1, 1, 1, 1).
            A(2, 2, 2, 2).
            R(x, x, w, x) :- A(x, x, w, x).
        };
        let m = solve p;
        (m |= #{ R(1, 1, 1, 1). }) and not (m |= #{ R(1, 2, 3, 4). })

    @test
    def testQuantVarOverlap06(): Bool =
        let p = #{
            A(1, 2, 3, 4).
            A(4, 3, 2, 1).
            A(1, 1, 1, 1).
            A(2, 2, 2, 2).
            R(x, x, x, x) :- A(x, x, x, x).
        };
        let m = solve p;
        (m |= #{ R(1, 1, 1, 1). }) and not (m |= #{ R(1, 2, 3, 4). })

    @test
    def testQuantVarMultiOverlap01(): Bool =
        let p = #{
            A(1, 3).
            A(1, 1).
            A(2, 2).
            B(1, 3).
            B(1, 1).
            B(2, 2).
            R(x, y) :- A(x, x), B(y, y).
        };
        let m = solve p;
        (m |= #{ R(1, 1). }) and not (m |= #{ R(1, 3). })

    @test
    def testQuantVarMultiOverlap02(): Bool =
        let p = #{
            A(1, 3).
            A(1, 1).
            A(2, 2).
            B(1, 3).
            B(1, 1).
            B(2, 2).
            R(x, x) :- A(x, x), B(x, x).
        };
        let m = solve p;
        (m |= #{ R(1, 1). }) and not (m |= #{ R(1, 3). })

}
