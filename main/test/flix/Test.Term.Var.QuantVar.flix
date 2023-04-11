mod Test.Term.Var.QuantVar {

    @test
    def testQuantVar01(): Bool =
        let p = #{
            A(1).
            R(x) :- A(x).
        };
        let r = query p select x from R(x);
        List.memberOf(1, r)


    @test
    def testQuantVar02(): Bool =
        let p = #{
            A(1, 2).
            R(x, y) :- A(x, y).
        };
        let r = query p select (x, y) from R(x, y);
        List.memberOf((1, 2), r)


    @test
    def testQuantVar03(): Bool =
        let p = #{
            A(1, 2, 3).
            R(x, y, z) :- A(x, y, z).
        };
        let r = query p select (x, y, z) from R(x, y, z);
        List.memberOf((1, 2, 3), r)


    @test
    def testQuantVar04(): Bool =
        let p = #{
            A(1, 2, 3, 4).
            R(x, y, z, w) :- A(x, y, z, w).
        };
        let r = query p select (x, y, z, w) from R(x, y, z, w);
        List.memberOf((1, 2, 3, 4), r)

    @test
    def testQuantVarOverlap01(): Bool =
        let p = #{
            A(1, 2, 3, 4).
            A(4, 3, 2, 1).
            A(1, 1, 1, 1).
            A(2, 2, 2, 2).
            R(x, x, z, w) :- A(x, x, z, w).
        };
        let r = query p select (x, y, z, w) from R(x, y, z, w);
        List.memberOf((1, 1, 1, 1), r) and not List.memberOf((1, 2, 3, 4), r)

    @test
    def testQuantVarOverlap02(): Bool =
        let p = #{
            A(1, 2, 3, 4).
            A(4, 3, 2, 1).
            A(1, 1, 1, 1).
            A(2, 2, 2, 2).
            R(x, y, y, w) :- A(x, y, y, w).
        };
        let r = query p select (x, y, z, w) from R(x, y, z, w);
        List.memberOf((1, 1, 1, 1), r) and not List.memberOf((1, 2, 3, 4), r)

    @test
    def testQuantVarOverlap03(): Bool =
        let p = #{
            A(1, 2, 3, 4).
            A(4, 3, 2, 1).
            A(1, 1, 1, 1).
            A(2, 2, 2, 2).
            R(x, y, z, z) :- A(x, y, z, z).
        };
        let r = query p select (x, y, z, w) from R(x, y, z, w);
        List.memberOf((1, 1, 1, 1), r) and not List.memberOf((1, 2, 3, 4), r)

    @test
    def testQuantVarOverlap04(): Bool =
        let p = #{
            A(1, 2, 3, 4).
            A(4, 3, 2, 1).
            A(1, 1, 1, 1).
            A(2, 2, 2, 2).
            R(x, y, z, x) :- A(x, y, z, x).
        };
        let r = query p select (x, y, z, w) from R(x, y, z, w);
        List.memberOf((1, 1, 1, 1), r) and not List.memberOf((1, 2, 3, 4), r)

    @test
    def testQuantVarOverlap05(): Bool =
        let p = #{
            A(1, 2, 3, 4).
            A(4, 3, 2, 1).
            A(1, 1, 1, 1).
            A(2, 2, 2, 2).
            R(x, x, w, x) :- A(x, x, w, x).
        };
        let r = query p select (x, y, z, w) from R(x, y, z, w);
        List.memberOf((1, 1, 1, 1), r) and not List.memberOf((1, 2, 3, 4), r)

    @test
    def testQuantVarOverlap06(): Bool =
        let p = #{
            A(1, 2, 3, 4).
            A(4, 3, 2, 1).
            A(1, 1, 1, 1).
            A(2, 2, 2, 2).
            R(x, x, x, x) :- A(x, x, x, x).
        };
        let r = query p select (x, y, z, w) from R(x, y, z, w);
        List.memberOf((1, 1, 1, 1), r) and not List.memberOf((1, 2, 3, 4), r)

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
        let r = query p select (x, y) from R(x, y);
        List.memberOf((1, 1), r) and not List.memberOf((1, 3), r)

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
        let r = query p select (x, y) from R(x, y);
        List.memberOf((1, 1), r) and not List.memberOf((1, 3), r)

}
