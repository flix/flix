mod Test.Predicate.Nullary {

    @test
    def testNullaryPredicate01(): Bool =
        let p = #{
            A(). B(). C().
        };
        let m = query p select 42 from A();
        Vector.memberOf(42, m)

    @test
    def testNullaryPredicate02(): Bool =
        let p = #{
            A(). B(). C().
        };
        let m = query p select 42 from B();
        Vector.memberOf(42, m)

    @test
    def testNullaryPredicate03(): Bool =
        let p = #{
            A(). B(). C().
        };
        let m = query p select 42 from C();
        Vector.memberOf(42, m)

    @test
    def testNullaryPredicate04(): Bool =
        let p = #{
            A(). B(). C().
            R() :- A().
        };
        let m = query p select 42 from R();
        Vector.memberOf(42, m)

    @test
    def testNullaryPredicate05(): Bool =
        let p = #{
            A(). B(). C().
            R() :- B().
        };
        let m = query p select 42 from R();
        Vector.memberOf(42, m)

    @test
    def testNullaryPredicate06(): Bool =
        let p = #{
            A(). B(). C().
            R() :- C().
        };
        let m = query p select 42 from R();
        Vector.memberOf(42, m)

    @test
    def testNullaryPredicate07(): Bool =
        let p = #{
            A(). B(). C().
            R() :- A(), B(), C().
        };
        let m = query p select 42 from R();
        Vector.memberOf(42, m)

}
