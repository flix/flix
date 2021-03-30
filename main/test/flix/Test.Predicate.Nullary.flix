namespace Test/Predicate/Nullary {

    @test
    def testNullaryPredicate01(): Bool =
        (solve #{ A(). }) |= #{ A(). }

    @test
    def testNullaryPredicate02(): Bool =
        (solve #{ A(). B(). C(). }) |= #{ A(). }

    @test
    def testNullaryPredicate03(): Bool =
        (solve #{ A(). A(). A(). }) |= #{ A(). }

    @test
    def testNullaryPredicate04(): Bool =
        (solve #{ A(). }) |= #{ A(). A(). A(). }

    @test
    def testNullaryPredicate05(): Bool =
        (solve #{ A(). A(). A(). }) |= #{ A(). A(). A(). }

    @test
    def testNullaryPredicate06(): Bool =
        let p = #{
            R() :- A(), B().
        };
        let m = solve p;
        not (m |= #{ R(). })

    @test
    def testNullaryPredicate07(): Bool =
        let p = #{
            A().
            R() :- A(), B().
        };
        let m = solve p;
        not (m |= #{ R(). })

    @test
    def testNullaryPredicate08(): Bool =
        let p = #{
            A().
            B().
            R() :- A(), B().
        };
        let m = solve p;
        m |= #{ R(). }

    @test
    def testTruePredicate01(): Bool =
        (True().) |= (True().)

    @test
    def testTruePredicate02(): Bool =
        (True().) |= #{ True(). True(). }

    @test
    def testTruePredicate03(): Bool =
        (True().) |= #{ True(). True(). True(). }

    @test
    def testTruePredicate04(): Bool =
        #{ True(). True(). } |= (True().)

    @test
    def testTruePredicate05(): Bool =
        #{ True(). True(). True(). } |= (True().)

    @test
    def testTruePredicate06(): Bool =
        (solve True().) |= (True().)

    @test
    def testTruePredicate07(): Bool =
        (solve (solve True().)) |= (True().)

    @test
    def testTruePredicate08(): Bool =
        (solve (solve (solve True().))) |= (True().)

}
