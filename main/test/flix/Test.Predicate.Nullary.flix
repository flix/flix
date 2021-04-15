namespace Test/Predicate/Nullary {

    @test
    def testNullaryPredicate01(): Bool & Impure =
        let p = #{
            A(). B(). C().
        };
        let m = query p select 42 from A();
        Array.memberOf(42, m)

    @test
    def testNullaryPredicate02(): Bool & Impure =
        let p = #{
            A(). B(). C().
        };
        let m = query p select 42 from B();
        Array.memberOf(42, m)

    @test
    def testNullaryPredicate03(): Bool & Impure =
        let p = #{
            A(). B(). C().
        };
        let m = query p select 42 from C();
        Array.memberOf(42, m)

    @test
    def testNullaryPredicate04(): Bool & Impure =
        let p = #{
            A(). B(). C().
            R() :- A().
        };
        let m = query p select 42 from R();
        Array.memberOf(42, m)

    @test
    def testNullaryPredicate05(): Bool & Impure =
        let p = #{
            A(). B(). C().
            R() :- B().
        };
        let m = query p select 42 from R();
        Array.memberOf(42, m)

    @test
    def testNullaryPredicate06(): Bool & Impure =
        let p = #{
            A(). B(). C().
            R() :- C().
        };
        let m = query p select 42 from R();
        Array.memberOf(42, m)

    @test
    def testNullaryPredicate07(): Bool & Impure =
        let p = #{
            A(). B(). C().
            R() :- A(), B(), C().
        };
        let m = query p select 42 from R();
        Array.memberOf(42, m)

}
